module Amount_m
    use Conversion_factors_m, only: AVOGADROS_NUMBER
    use erloff, only: ErrorList_t, Fatal, Module_, Procedure_
    use iso_varying_string, only: &
            VARYING_STRING, &
            assignment(=), &
            operator(==), &
            operator(//), &
            len, &
            split, &
            var_str
    use Miscellaneous_m, only: &
            operator(.safeEq.), &
            equalWithinAbsolute_ => equalWithinAbsolute, &
            equalWithinRelative_ => equalWithinRelative, &
            parseCloseBrace, &
            parseOpenBrace, &
            parseSI, &
            parseSpace, &
            wrapInLatexQuantity, &
            wrapInLatexUnit, &
            PARSE_ERROR, &
            UNKNOWN_UNIT
    use parff, only: &
            ParsedRational_t, &
            ParseResult_t, &
            ParserOutput_t, &
            State_t, &
            dropThen, &
            parseChar, &
            parseRational, &
            parseString, &
            parseWith, &
            thenDrop
    use strff, only: join, toString

    implicit none
    private

    type, public :: Amount_t
        double precision :: mols
    contains
        private
        procedure :: toUnits
        generic, public :: operator(.in.) => toUnits
        procedure, pass(amount) :: doubleTimesAmount
        procedure, pass(amount) :: integerTimesAmount
        procedure, pass(amount) :: amountTimesDouble
        procedure, pass(amount) :: amountTimesInteger
        generic, public :: operator(*) => &
                doubleTimesAmount, &
                integerTimesAmount, &
                amountTimesDouble, &
                amountTimesInteger
        procedure :: amountDividedByDouble
        procedure :: amountDividedByInteger
        procedure, pass(numerator) :: amountDividedByAmount
        generic, public :: operator(/) => &
                amountDividedByDouble, &
                amountDividedByInteger, &
                amountDividedByAmount
        procedure :: amountPlusAmount
        generic, public :: operator(+) => amountPlusAmount
        procedure :: amountMinusAmount
        generic, public :: operator(-) => amountMinusAmount
        procedure :: greaterThan
        generic, public :: operator(>) => greaterThan
        procedure :: lessThan
        generic, public :: operator(<) => lessThan
        procedure :: greaterThanOrEqual
        generic, public :: operator(>=) => greaterThanOrEqual
        procedure :: lessThanOrEqual
        generic, public :: operator(<=) => lessThanOrEqual
        procedure :: equal_
        generic, public :: operator(==) => equal_
        procedure :: equalWithinAbsolute
        procedure :: equalWithinRelative
        generic, public :: equal => &
                equal_, equalWithinAbsolute, equalWithinRelative
        procedure :: notEqual
        generic, public :: operator(/=) => notEqual
        procedure :: toStringFullPrecision
        procedure :: toStringWithPrecision
        generic, public :: toString => &
                toStringFullPrecision, toStringWithPrecision
        procedure :: toStringInFullPrecision
        procedure :: toStringInWithPrecision
        generic, public :: toStringIn => &
                toStringInFullPrecision, toStringInWithPrecision
    end type Amount_t

    type, abstract, public :: AmountUnit_t
        double precision :: conversion_factor
    contains
        procedure(justUnitToString), deferred :: unitToString
        procedure(unitWithValueToString), deferred :: valueToString
        generic :: toString => unitToString, valueToString
        procedure(parseAsI), deferred :: parseAs
    end type AmountUnit_t

    type, extends(AmountUnit_t), public :: AmountSimpleUnit_t
        character(len=20) :: symbol
    contains
        procedure :: unitToString => simpleUnitToString
        procedure :: valueToString => simpleValueToString
        procedure :: parseAs => simpleParseAs
    end type AmountSimpleUnit_t

    type, extends(AmountUnit_t), public :: AmountGnuplotUnit_t
        character(len=50) :: symbol
    contains
        procedure :: unitToString => gnuplotUnitToString
        procedure :: valueToString => gnuplotValueToString
        procedure :: parseAs => gnuplotParseAs
    end type AmountGnuplotUnit_t

    type, extends(AmountUnit_t), public :: AmountLatexUnit_t
        character(len=100) :: symbol
    contains
        procedure :: unitToString => latexUnitToString
        procedure :: valueToString => latexValueToString
        procedure :: parseAs => latexParseAs
    end type AmountLatexUnit_t

    abstract interface
        elemental function justUnitToString(self) result(string)
            import AmountUnit_t, VARYING_STRING
            class(AmountUnit_t), intent(in) :: self
            type(VARYING_STRING) :: string
        end function justUnitToString

        pure function unitWithValueToString(self, value_) result(string)
            import AmountUnit_t, VARYING_STRING
            class(AmountUnit_t), intent(in) :: self
            type(VARYING_STRING), intent(in) :: value_
            type(VARYING_STRING) :: string
        end function unitWithValueToString

        pure subroutine parseAsI(self, string, errors, amount)
            import ErrorList_t, Amount_t, AmountUnit_t, VARYING_STRING
            class(AmountUnit_t), intent(in) :: self
            type(VARYING_STRING), intent(in) :: string
            type(ErrorList_t), intent(out) :: errors
            type(Amount_t), intent(out) :: amount
        end subroutine parseAsI
    end interface

    interface operator(.unit.)
        module procedure fromUnits
    end interface operator(.unit.)

    interface fromString
        module procedure fromStringBasicC
        module procedure fromStringBasicS
        module procedure fromStringWithUnitsC
        module procedure fromStringWithUnitsS
        module procedure simpleUnitFromStringC
        module procedure simpleUnitFromStringS
        module procedure simpleUnitFromStringWithUnitsC
        module procedure simpleUnitFromStringWithUnitsS
    end interface fromString

    interface sum
        module procedure sumAmount
    end interface sum

    type(AmountSimpleUnit_t), parameter, public :: MOLS = &
            AmountSimpleUnit_t( &
                    conversion_factor = 1.0d0, &
                    symbol = "mol")
    type(AmountGnuplotUnit_t), parameter, public :: MOLS_GNUPLOT = &
            AmountGnuplotUnit_t( &
                    conversion_factor = 1.0d0, &
                    symbol = "mol")
    type(AmountLatexUnit_t), parameter, public :: MOLS_LATEX = &
            AmountLatexUnit_t( &
                    conversion_factor = 1.0d0, &
                    symbol = "\mole")
    type(AmountSimpleUnit_t), parameter, public :: PARTICLES = &
            AmountSimpleUnit_t( &
                    conversion_factor = AVOGADROS_NUMBER, &
                    symbol = "particles")
    type(AmountGnuplotUnit_t), parameter, public :: PARTICLES_GNUPLOT = &
            AmountGnuplotUnit_t( &
                    conversion_factor = AVOGADROS_NUMBER, &
                    symbol = "particles")
    type(AmountLatexUnit_t), parameter, public :: PARTICLES_LATEX = &
            AmountLatexUnit_t( &
                    conversion_factor = AVOGADROS_NUMBER, &
                    symbol = ".particles")

    type(AmountSimpleUnit_t), public :: DEFAULT_OUTPUT_UNITS = MOLS

    type(AmountSimpleUnit_t), parameter, public :: PROVIDED_UNITS(*) = &
            [MOLS, PARTICLES]
    type(AmountGnuplotUnit_t), parameter, public :: PROVIDED_GNUPLOT_UNITS(*) = &
            [MOLS_GNUPLOT, PARTICLES_GNUPLOT]
    type(AmountLatexUnit_t), parameter, public :: PROVIDED_LATEX_UNITS(*) = &
            [MOLS_LATEX, PARTICLES_LATEX]

    public :: operator(.unit.), fromString, selectUnit, sum
contains
    pure subroutine fromStringBasicC(string, errors, amount)
        character(len=*), intent(in) :: string
        type(ErrorList_t), intent(out) :: errors
        type(Amount_t), intent(out) :: amount

        type(ErrorList_t) :: errors_

        call fromString( &
                var_str(string), PROVIDED_UNITS, errors_, amount)
        call errors%appendErrors( &
                errors_, &
                Module_("Amount_m"), &
                Procedure_("fromStringBasicC"))
    end subroutine fromStringBasicC

    pure subroutine fromStringBasicS(string, errors, amount)
        type(VARYING_STRING), intent(in) :: string
        type(ErrorList_t), intent(out) :: errors
        type(Amount_t), intent(out) :: amount

        type(ErrorList_t) :: errors_

        call fromString( &
                string, PROVIDED_UNITS, errors_, amount)
        call errors%appendErrors( &
                errors_, &
                Module_("Amount_m"), &
                Procedure_("fromStringBasicS"))
    end subroutine fromStringBasicS

    pure subroutine fromStringWithUnitsC(string, units, errors, amount)
        character(len=*), intent(in) :: string
        class(AmountUnit_t), intent(in) :: units(:)
        type(ErrorList_t), intent(out) :: errors
        type(Amount_t), intent(out) :: amount

        type(ErrorList_t) :: errors_

        call fromString( &
                var_str(string), units, errors_, amount)
        call errors%appendErrors( &
                errors_, &
                Module_("Amount_m"), &
                Procedure_("fromStringWithUnitsC"))
    end subroutine fromStringWithUnitsC

    pure subroutine fromStringWithUnitsS(string, units, errors, amount)
        type(VARYING_STRING), intent(in) :: string
        class(AmountUnit_t), intent(in) :: units(:)
        type(ErrorList_t), intent(out) :: errors
        type(Amount_t), intent(out) :: amount

        type(ErrorList_t) :: all_errors(size(units))
        integer :: i

        do i = 1, size(units)
            call units(i)%parseAs(string, all_errors(i), amount)
            if (.not. all_errors(i)%hasAny()) return
        end do
        do i = 1, size(units)
            call errors%appendErrors( &
                    all_errors(i), &
                    Module_("Amount_m"), &
                    Procedure_("fromStringWithUnitsS"))
        end do
    end subroutine fromStringWithUnitsS

    elemental function fromUnits(value_, units) result(amount)
        double precision, intent(in) :: value_
        class(AmountUnit_t), intent(in) :: units
        type(Amount_t) :: amount

        amount%mols = value_ / units%conversion_factor
    end function fromUnits

    elemental function toUnits(self, units) result(amount)
        class(Amount_t), intent(in) :: self
        class(AmountUnit_t), intent(in) :: units
        double precision :: amount

        amount = self%mols * units%conversion_factor
    end function toUnits

    elemental function doubleTimesAmount( &
            multiplier, amount) result(new_amount)
        double precision, intent(in) :: multiplier
        class(Amount_t), intent(in) :: amount
        type(Amount_t) :: new_amount

        new_amount%mols = &
                multiplier * amount%mols
    end function doubleTimesAmount

    elemental function integerTimesAmount( &
            multiplier, amount) result(new_amount)
        integer, intent(in) :: multiplier
        class(Amount_t), intent(in) :: amount
        type(Amount_t) :: new_amount

        new_amount%mols = &
                dble(multiplier) * amount%mols
    end function integerTimesAmount

    elemental function amountTimesDouble( &
            amount, multiplier) result(new_amount)
        class(Amount_t), intent(in) :: amount
        double precision, intent(in) :: multiplier
        type(Amount_t) :: new_amount

        new_amount%mols = &
                amount%mols * multiplier
    end function amountTimesDouble

    elemental function amountTimesInteger( &
            amount, multiplier) result(new_amount)
        class(Amount_t), intent(in) :: amount
        integer, intent(in) :: multiplier
        type(Amount_t) :: new_amount

        new_amount%mols = &
                amount%mols * dble(multiplier)
    end function amountTimesInteger

    elemental function amountDividedByDouble( &
            amount, divisor) result(new_amount)
        class(Amount_t), intent(in) :: amount
        double precision, intent(in) :: divisor
        type(Amount_t) :: new_amount

        new_amount%mols = &
                amount%mols / divisor
    end function amountDividedByDouble

    elemental function amountDividedByInteger( &
            amount, divisor) result(new_amount)
        class(Amount_t), intent(in) :: amount
        integer, intent(in) :: divisor
        type(Amount_t) :: new_amount

        new_amount%mols = &
                amount%mols / dble(divisor)
    end function amountDividedByInteger

    elemental function amountDividedByAmount( &
            numerator, denomenator) result(ratio)
        class(Amount_t), intent(in) :: numerator
        class(Amount_t), intent(in) :: denomenator
        double precision :: ratio

        ratio = numerator%mols / denomenator%mols
    end function amountDividedByAmount

    elemental function amountPlusAmount( &
            amount1, amount2) result(new_amount)
        class(Amount_t), intent(in) :: amount1
        class(Amount_t), intent(in) :: amount2
        type(Amount_t) :: new_amount

        new_amount%mols = &
                amount1%mols + amount2%mols
    end function amountPlusAmount

    elemental function amountMinusAmount( &
            amount1, amount2) result(new_amount)
        class(Amount_t), intent(in) :: amount1
        class(Amount_t), intent(in) :: amount2
        type(Amount_t) :: new_amount

        new_amount%mols = &
                amount1%mols - amount2%mols
    end function amountMinusAmount

    pure function sumAmount(amounts)
        type(Amount_t), intent(in) :: amounts(:)
        type(Amount_t) :: sumAmount

        sumAmount%mols = sum(amounts%mols)
    end function sumAmount

    elemental function greaterThan(lhs, rhs)
        class(Amount_t), intent(in) :: lhs
        class(Amount_t), intent(in) :: rhs
        logical :: greaterThan

        greaterThan = lhs%mols > rhs%mols
    end function greaterThan

    elemental function lessThan(lhs,rhs)
        class(Amount_t), intent(in) :: lhs
        class(Amount_t), intent(in) :: rhs
        logical :: lessThan

        lessThan = lhs%mols < rhs%mols
    end function lessThan

    elemental function greaterThanOrEqual(lhs, rhs)
        class(Amount_t), intent(in) :: lhs
        class(Amount_t), intent(in) :: rhs
        logical :: greaterThanOrEqual

        greaterThanOrEqual = lhs%mols >= rhs%mols
    end function greaterThanOrEqual

    elemental function lessThanOrEqual(lhs, rhs)
        class(Amount_t), intent(in) :: lhs
        class(Amount_t), intent(in) :: rhs
        logical :: lessThanOrEqual

        lessThanOrEqual = lhs%mols <= rhs%mols
    end function lessThanOrEqual

    elemental function equal_(lhs,rhs)
        class(Amount_t), intent(in) :: lhs
        class(Amount_t), intent(in) :: rhs
        logical :: equal_

        equal_ = lhs%mols .safeEq. rhs%mols
    end function equal_

    elemental function equalWithinAbsolute(lhs, rhs, within)
        class(Amount_t), intent(in) :: lhs
        class(Amount_t), intent(in) :: rhs
        class(Amount_t), intent(in) :: within
        logical :: equalWithinAbsolute

        equalWithinAbsolute = equalWithinAbsolute_( &
                lhs%mols, rhs%mols, within%mols)
    end function equalWithinAbsolute

    elemental function equalWithinRelative(lhs, rhs, within)
        class(Amount_t), intent(in) :: lhs
        class(Amount_t), intent(in) :: rhs
        double precision, intent(in) :: within
        logical :: equalWithinRelative

        equalWithinRelative = equalWithinRelative_( &
                lhs%mols, rhs%mols, within)
    end function equalWithinRelative

    elemental function notEqual(lhs, rhs)
        class(Amount_t), intent(in) :: lhs
        class(Amount_t), intent(in) :: rhs
        logical :: notEqual

        notEqual = .not. lhs == rhs
    end function notEqual

    elemental function toStringFullPrecision(self) result(string)
        class(Amount_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = self%toStringIn(DEFAULT_OUTPUT_UNITS)
    end function toStringFullPrecision

    elemental function toStringWithPrecision(self, significant_digits) result(string)
        class(Amount_t), intent(in) :: self
        integer, intent(in) :: significant_digits
        type(VARYING_STRING) :: string

        string = self%toStringIn(DEFAULT_OUTPUT_UNITS, significant_digits)
    end function toStringWithPrecision

    elemental function toStringInFullPrecision(self, units) result(string)
        class(Amount_t), intent(in) :: self
        class(AmountUnit_t), intent(in) :: units
        type(VARYING_STRING) :: string

        string = units%toString(toString(self.in.units))
    end function toStringInFullPrecision

    elemental function toStringInWithPrecision( &
            self, units, significant_digits) result(string)
        class(Amount_t), intent(in) :: self
        class(AmountUnit_t), intent(in) :: units
        integer, intent(in) :: significant_digits
        type(VARYING_STRING) :: string

        string = units%toString(toString(self.in.units, significant_digits))
    end function toStringInWithPrecision

    elemental function simpleUnitToString(self) result(string)
        class(AmountSimpleUnit_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = trim(self%symbol)
    end function simpleUnitToString

    pure function simpleValueToString(self, value_) result(string)
        class(AmountSimpleUnit_t), intent(in) :: self
        type(VARYING_STRING), intent(in) :: value_
        type(VARYING_STRING) :: string

        string = value_ // " " // self%toString()
    end function simpleValueToString

    pure subroutine simpleParseAs(self, string, errors, amount)
        class(AmountSimpleUnit_t), intent(in) :: self
        type(VARYING_STRING), intent(in) :: string
        type(ErrorList_t), intent(out) :: errors
        type(Amount_t), intent(out) :: amount

        type(ParseResult_t) :: parse_result

        parse_result = parseWith(theParser, string)
        if (parse_result%ok) then
            select type (the_number => parse_result%parsed)
            type is (ParsedRational_t)
                amount = the_number%value_.unit.self
            end select
        else
            call errors%appendError(Fatal( &
                    PARSE_ERROR, &
                    Module_("Amount_m"), &
                    Procedure_("simpleParseAs"), &
                    parse_result%message))
        end if
    contains
        pure function theParser(state_) result(result_)
            type(State_t), intent(in) :: state_
            type(ParserOutput_t) :: result_

            result_ = thenDrop( &
                    thenDrop(parseRational, parseSpace, state_), &
                    parseUnit)
        end function theParser

        pure function parseUnit(state_) result(result_)
            type(State_t), intent(in) :: state_
            type(ParserOutput_t) :: result_

            result_ = parseString(trim(self%symbol), state_)
        end function parseUnit
    end subroutine simpleParseAs

    pure subroutine gnuplotParseAs(self, string, errors, amount)
        class(AmountGnuplotUnit_t), intent(in) :: self
        type(VARYING_STRING), intent(in) :: string
        type(ErrorList_t), intent(out) :: errors
        type(Amount_t), intent(out) :: amount

        type(ParseResult_t) :: parse_result

        parse_result = parseWith(theParser, string)
        if (parse_result%ok) then
            select type (the_number => parse_result%parsed)
            type is (ParsedRational_t)
                amount = the_number%value_.unit.self
            end select
        else
            call errors%appendError(Fatal( &
                    PARSE_ERROR, &
                    Module_("Amount_m"), &
                    Procedure_("gnuplotParseAs"), &
                    parse_result%message))
        end if
    contains
        pure function theParser(state_) result(result_)
            type(State_t), intent(in) :: state_
            type(ParserOutput_t) :: result_

            result_ = thenDrop( &
                    thenDrop(parseRational, parseSpace, state_), &
                    parseUnit)
        end function theParser

        pure function parseUnit(state_) result(result_)
            type(State_t), intent(in) :: state_
            type(ParserOutput_t) :: result_

            result_ = parseString(trim(self%symbol), state_)
        end function parseUnit
    end subroutine gnuplotParseAs

    pure subroutine latexParseAs(self, string, errors, amount)
        class(AmountLatexUnit_t), intent(in) :: self
        type(VARYING_STRING), intent(in) :: string
        type(ErrorList_t), intent(out) :: errors
        type(Amount_t), intent(out) :: amount

        type(ParseResult_t) :: parse_result

        parse_result = parseWith(theParser, string)
        if (parse_result%ok) then
            select type (the_number => parse_result%parsed)
            type is (ParsedRational_t)
                amount = the_number%value_.unit.self
            end select
        else
            call errors%appendError(Fatal( &
                    PARSE_ERROR, &
                    Module_("Amount_m"), &
                    Procedure_("latexParseAs"), &
                    parse_result%message))
        end if
    contains
        pure function theParser(state_) result(result_)
            type(State_t), intent(in) :: state_
            type(ParserOutput_t) :: result_

            result_ = thenDrop( &
                    thenDrop( &
                            thenDrop( &
                                    thenDrop( &
                                            dropThen( &
                                                    dropThen(parseSI, parseOpenBrace, state_), &
                                                    parseRational), &
                                            parseCloseBrace), &
                                    parseOpenBrace), &
                            parseUnit), &
                    parseCloseBrace)
        end function theParser

        pure function parseUnit(state_) result(result_)
            type(State_t), intent(in) :: state_
            type(ParserOutput_t) :: result_

            result_ = parseString(trim(self%symbol), state_)
        end function parseUnit
    end subroutine latexParseAs

    elemental function gnuplotUnitToString(self) result(string)
        class(AmountGnuplotUnit_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = trim(self%symbol)
    end function gnuplotUnitToString

    pure function gnuplotValueToString(self, value_) result(string)
        class(AmountGnuplotUnit_t), intent(in) :: self
        type(VARYING_STRING), intent(in) :: value_
        type(VARYING_STRING) :: string

        string = value_ // " " // self%toString()
    end function gnuplotValueToString

    elemental function latexUnitToString(self) result(string)
        class(AmountLatexUnit_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = wrapInLatexUnit(trim(self%symbol))
    end function latexUnitToString

    pure function latexValueToString(self, value_) result(string)
        class(AmountLatexUnit_t), intent(in) :: self
        type(VARYING_STRING), intent(in) :: value_
        type(VARYING_STRING) :: string

        string = wrapInLatexQuantity(value_, trim(self%symbol))
    end function latexValueToString

    pure subroutine simpleUnitFromStringC(string, errors, unit)
        character(len=*), intent(in) :: string
        type(ErrorList_t), intent(out) :: errors
        type(AmountSimpleUnit_t), intent(out) :: unit

        type(ErrorList_t) :: errors_

        call fromString(var_str(string), PROVIDED_UNITS, errors_, unit)
        call errors%appendErrors( &
                errors_, &
                Module_("Amount_m"), &
                Procedure_("simpleUnitFromStringC"))
    end subroutine simpleUnitFromStringC

    pure subroutine simpleUnitFromStringS(string, errors, unit)
        type(VARYING_STRING), intent(in) :: string
        type(ErrorList_t), intent(out) :: errors
        type(AmountSimpleUnit_t), intent(out) :: unit

        type(ErrorList_t) :: errors_

        call fromString(string, PROVIDED_UNITS, errors_, unit)
        call errors%appendErrors( &
                errors_, &
                Module_("Amount_m"), &
                Procedure_("simpleUnitFromStringS"))
    end subroutine simpleUnitFromStringS

    pure subroutine simpleUnitFromStringWithUnitsC(string, units, errors, unit)
        character(len=*), intent(in) :: string
        type(AmountSimpleUnit_t), intent(in) :: units(:)
        type(ErrorList_t), intent(out) :: errors
        type(AmountSimpleUnit_t), intent(out) :: unit

        type(ErrorList_t) :: errors_

        call fromString(var_str(string), units, errors_, unit)
        call errors%appendErrors( &
                errors_, &
                Module_("Amount_m"), &
                Procedure_("simpleUnitFromStringWithUnitsC"))
    end subroutine simpleUnitFromStringWithUnitsC

    pure subroutine simpleUnitFromStringWithUnitsS(string, units, errors, unit)
        type(VARYING_STRING), intent(in) :: string
        type(AmountSimpleUnit_t), intent(in) :: units(:)
        type(ErrorList_t), intent(out) :: errors
        type(AmountSimpleUnit_t), intent(out) :: unit

        type(ErrorList_t) :: errors_
        integer :: which_unit

        call selectUnit(string, units, errors, which_unit)
        if (errors_%hasAny()) then
            call errors%appendErrors( &
                    errors_, &
                    Module_("Amount_m"), &
                    Procedure_("simpleUnitFromStringWithUnitsS"))
        else
            unit = units(which_unit)
        end if
    end subroutine simpleUnitFromStringWithUnitsS

    pure subroutine selectUnit(string, units, errors, index)
        type(VARYING_STRING), intent(in) :: string
        class(AmountUnit_t), intent(in) :: units(:)
        type(ErrorList_t), intent(out) :: errors
        integer, intent(out) :: index

        integer :: i
        type(VARYING_STRING) :: unit_strings(size(units))

        do i = 1, size(units)
            if (string == units(i)%toString()) then
                index = i
                return
            end if
        end do
        do i = 1, size(units)
            unit_strings(i) = units(i)%toString()
        end do
        call errors%appendError(Fatal( &
                UNKNOWN_UNIT, &
                Module_("Amount_m"), &
                Procedure_("selectUnit"), &
                '"' // string // '", known units: [' // join(unit_strings, ', ') // ']'))
    end subroutine selectUnit
end module Amount_m
