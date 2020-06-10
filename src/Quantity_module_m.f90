module Quantity_module_m
    use erloff, only: ErrorList_t, Fatal, Module_, Procedure_
    use iso_varying_string, only: &
            VARYING_STRING, &
            assignment(=), &
            operator(==), &
            operator(//), &
            len, &
            split, &
            var_str
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
    use quaff_Utilities_m, only: &
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
    use strff, only: join, toString

    implicit none
    private

    type, public :: QuantityCamel_t
        double precision :: units_lower
    contains
        private
        procedure :: toUnits
        generic, public :: operator(.in.) => toUnits
        procedure, pass(quantity_lower) :: doubleTimesQuantityCamel
        procedure, pass(quantity_lower) :: integerTimesQuantityCamel
        procedure, pass(quantity_lower) :: quantitySnakeTimesDouble
        procedure, pass(quantity_lower) :: quantitySnakeTimesInteger
        generic, public :: operator(*) => &
                doubleTimesQuantityCamel, &
                integerTimesQuantityCamel, &
                quantitySnakeTimesDouble, &
                quantitySnakeTimesInteger
        procedure :: quantitySnakeDividedByDouble
        procedure :: quantitySnakeDividedByInteger
        procedure, pass(numerator) :: quantitySnakeDividedByQuantityCamel
        generic, public :: operator(/) => &
                quantitySnakeDividedByDouble, &
                quantitySnakeDividedByInteger, &
                quantitySnakeDividedByQuantityCamel
        procedure :: quantitySnakePlusQuantityCamel
        generic, public :: operator(+) => quantitySnakePlusQuantityCamel
        procedure :: quantitySnakeMinusQuantityCamel
        generic, public :: operator(-) => quantitySnakeMinusQuantityCamel
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
    end type QuantityCamel_t

    type, abstract, public :: QuantityCamelUnit_t
        double precision :: conversion_factor
    contains
        procedure(justUnitToString), deferred :: unitToString
        procedure(unitWithValueToString), deferred :: valueToString
        generic :: toString => unitToString, valueToString
        procedure(parseAsI), deferred :: parseAs
    end type QuantityCamelUnit_t

    type, extends(QuantityCamelUnit_t), public :: QuantityCamelSimpleUnit_t
        character(len=20) :: symbol
    contains
        procedure :: unitToString => simpleUnitToString
        procedure :: valueToString => simpleValueToString
        procedure :: parseAs => simpleParseAs
    end type QuantityCamelSimpleUnit_t

    type, extends(QuantityCamelUnit_t), public :: QuantityCamelGnuplotUnit_t
        character(len=50) :: symbol
    contains
        procedure :: unitToString => gnuplotUnitToString
        procedure :: valueToString => gnuplotValueToString
        procedure :: parseAs => gnuplotParseAs
    end type QuantityCamelGnuplotUnit_t

    type, extends(QuantityCamelUnit_t), public :: QuantityCamelLatexUnit_t
        character(len=100) :: symbol
    contains
        procedure :: unitToString => latexUnitToString
        procedure :: valueToString => latexValueToString
        procedure :: parseAs => latexParseAs
    end type QuantityCamelLatexUnit_t

    abstract interface
        elemental function justUnitToString(self) result(string)
            import QuantityCamelUnit_t, VARYING_STRING
            class(QuantityCamelUnit_t), intent(in) :: self
            type(VARYING_STRING) :: string
        end function justUnitToString

        pure function unitWithValueToString(self, value_) result(string)
            import QuantityCamelUnit_t, VARYING_STRING
            class(QuantityCamelUnit_t), intent(in) :: self
            type(VARYING_STRING), intent(in) :: value_
            type(VARYING_STRING) :: string
        end function unitWithValueToString

        pure subroutine parseAsI(self, string, errors, quantity_lower)
            import ErrorList_t, QuantityCamel_t, QuantityCamelUnit_t, VARYING_STRING
            class(QuantityCamelUnit_t), intent(in) :: self
            type(VARYING_STRING), intent(in) :: string
            type(ErrorList_t), intent(out) :: errors
            type(QuantityCamel_t), intent(out) :: quantity_lower
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
        module procedure sumQuantityCamel
    end interface sum

    type(QuantityCamelSimpleUnit_t), parameter, public :: UNITS_CAPITAL = &
            QuantityCamelSimpleUnit_t( &
                    conversion_factor = 1.0d0, &
                    symbol = "unit_sym")
    type(QuantityCamelGnuplotUnit_t), parameter, public :: UNITS_CAPITAL_GNUPLOT = &
            QuantityCamelGnuplotUnit_t( &
                    conversion_factor = 1.0d0, &
                    symbol = "gnu_sym")
    type(QuantityCamelLatexUnit_t), parameter, public :: UNITS_CAPITAL_LATEX = &
            QuantityCamelLatexUnit_t( &
                    conversion_factor = 1.0d0, &
                    symbol = "ltx_sym")
    type(QuantityCamelSimpleUnit_t), parameter, public :: UNITS_CAPITAL2 = &
            QuantityCamelSimpleUnit_t( &
                    conversion_factor = 1.0d0, &
                    symbol = "unit_sym")
    type(QuantityCamelGnuplotUnit_t), parameter, public :: UNITS_CAPITAL2_GNUPLOT = &
            QuantityCamelGnuplotUnit_t( &
                    conversion_factor = 1.0d0, &
                    symbol = "gnu_sym")
    type(QuantityCamelLatexUnit_t), parameter, public :: UNITS_CAPITAL2_LATEX = &
            QuantityCamelLatexUnit_t( &
                    conversion_factor = 1.0d0, &
                    symbol = "ltx_sym")

    type(QuantityCamelSimpleUnit_t), public :: DEFAULT_OUTPUT_UNITS = UNITS_CAPITAL

    type(QuantityCamelSimpleUnit_t), parameter, public :: PROVIDED_UNITS(*) = &
            [UNITS_CAPITAL, UNITS_CAPITAL2]
    type(QuantityCamelGnuplotUnit_t), parameter, public :: PROVIDED_GNUPLOT_UNITS(*) = &
            [UNITS_CAPITAL_GNUPLOT, UNITS_CAPITAL2_GNUPLOT]
    type(QuantityCamelLatexUnit_t), parameter, public :: PROVIDED_LATEX_UNITS(*) = &
            [UNITS_CAPITAL_LATEX, UNITS_CAPITAL2_LATEX]

    public :: operator(.unit.), fromString, selectUnit, sum
contains
    pure subroutine fromStringBasicC(string, errors, quantity_lower)
        character(len=*), intent(in) :: string
        type(ErrorList_t), intent(out) :: errors
        type(QuantityCamel_t), intent(out) :: quantity_lower

        type(ErrorList_t) :: errors_

        call fromString( &
                var_str(string), PROVIDED_UNITS, errors_, quantity_lower)
        call errors%appendErrors( &
                errors_, &
                Module_("Quantity_module_m"), &
                Procedure_("fromStringBasicC"))
    end subroutine fromStringBasicC

    pure subroutine fromStringBasicS(string, errors, quantity_lower)
        type(VARYING_STRING), intent(in) :: string
        type(ErrorList_t), intent(out) :: errors
        type(QuantityCamel_t), intent(out) :: quantity_lower

        type(ErrorList_t) :: errors_

        call fromString( &
                string, PROVIDED_UNITS, errors_, quantity_lower)
        call errors%appendErrors( &
                errors_, &
                Module_("Quantity_module_m"), &
                Procedure_("fromStringBasicS"))
    end subroutine fromStringBasicS

    pure subroutine fromStringWithUnitsC(string, units, errors, quantity_lower)
        character(len=*), intent(in) :: string
        class(QuantityCamelUnit_t), intent(in) :: units(:)
        type(ErrorList_t), intent(out) :: errors
        type(QuantityCamel_t), intent(out) :: quantity_lower

        type(ErrorList_t) :: errors_

        call fromString( &
                var_str(string), units, errors_, quantity_lower)
        call errors%appendErrors( &
                errors_, &
                Module_("Quantity_module_m"), &
                Procedure_("fromStringWithUnitsC"))
    end subroutine fromStringWithUnitsC

    pure subroutine fromStringWithUnitsS(string, units, errors, quantity_lower)
        type(VARYING_STRING), intent(in) :: string
        class(QuantityCamelUnit_t), intent(in) :: units(:)
        type(ErrorList_t), intent(out) :: errors
        type(QuantityCamel_t), intent(out) :: quantity_lower

        type(ErrorList_t) :: all_errors(size(units))
        integer :: i

        do i = 1, size(units)
            call units(i)%parseAs(string, all_errors(i), quantity_lower)
            if (.not. all_errors(i)%hasAny()) return
        end do
        do i = 1, size(units)
            call errors%appendErrors( &
                    all_errors(i), &
                    Module_("Quantity_module_m"), &
                    Procedure_("fromStringWithUnitsS"))
        end do
    end subroutine fromStringWithUnitsS

    elemental function fromUnits(value_, units) result(quantity_lower)
        double precision, intent(in) :: value_
        class(QuantityCamelUnit_t), intent(in) :: units
        type(QuantityCamel_t) :: quantity_lower

        quantity_lower%units_lower = value_ / units%conversion_factor
    end function fromUnits

    elemental function toUnits(self, units) result(quantity_lower)
        class(QuantityCamel_t), intent(in) :: self
        class(QuantityCamelUnit_t), intent(in) :: units
        double precision :: quantity_lower

        quantity_lower = self%units_lower * units%conversion_factor
    end function toUnits

    elemental function doubleTimesQuantityCamel( &
            multiplier, quantity_lower) result(new_quantity_lower)
        double precision, intent(in) :: multiplier
        class(QuantityCamel_t), intent(in) :: quantity_lower
        type(QuantityCamel_t) :: new_quantity_lower

        new_quantity_lower%units_lower = &
                multiplier * quantity_lower%units_lower
    end function doubleTimesQuantityCamel

    elemental function integerTimesQuantityCamel( &
            multiplier, quantity_lower) result(new_quantity_lower)
        integer, intent(in) :: multiplier
        class(QuantityCamel_t), intent(in) :: quantity_lower
        type(QuantityCamel_t) :: new_quantity_lower

        new_quantity_lower%units_lower = &
                dble(multiplier) * quantity_lower%units_lower
    end function integerTimesQuantityCamel

    elemental function quantitySnakeTimesDouble( &
            quantity_lower, multiplier) result(new_quantity_lower)
        class(QuantityCamel_t), intent(in) :: quantity_lower
        double precision, intent(in) :: multiplier
        type(QuantityCamel_t) :: new_quantity_lower

        new_quantity_lower%units_lower = &
                quantity_lower%units_lower * multiplier
    end function quantitySnakeTimesDouble

    elemental function quantitySnakeTimesInteger( &
            quantity_lower, multiplier) result(new_quantity_lower)
        class(QuantityCamel_t), intent(in) :: quantity_lower
        integer, intent(in) :: multiplier
        type(QuantityCamel_t) :: new_quantity_lower

        new_quantity_lower%units_lower = &
                quantity_lower%units_lower * dble(multiplier)
    end function quantitySnakeTimesInteger

    elemental function quantitySnakeDividedByDouble( &
            quantity_lower, divisor) result(new_quantity_lower)
        class(QuantityCamel_t), intent(in) :: quantity_lower
        double precision, intent(in) :: divisor
        type(QuantityCamel_t) :: new_quantity_lower

        new_quantity_lower%units_lower = &
                quantity_lower%units_lower / divisor
    end function quantitySnakeDividedByDouble

    elemental function quantitySnakeDividedByInteger( &
            quantity_lower, divisor) result(new_quantity_lower)
        class(QuantityCamel_t), intent(in) :: quantity_lower
        integer, intent(in) :: divisor
        type(QuantityCamel_t) :: new_quantity_lower

        new_quantity_lower%units_lower = &
                quantity_lower%units_lower / dble(divisor)
    end function quantitySnakeDividedByInteger

    elemental function quantitySnakeDividedByQuantityCamel( &
            numerator, denomenator) result(ratio)
        class(QuantityCamel_t), intent(in) :: numerator
        class(QuantityCamel_t), intent(in) :: denomenator
        double precision :: ratio

        ratio = numerator%units_lower / denomenator%units_lower
    end function quantitySnakeDividedByQuantityCamel

    elemental function quantitySnakePlusQuantityCamel( &
            quantity_lower1, quantity_lower2) result(new_quantity_lower)
        class(QuantityCamel_t), intent(in) :: quantity_lower1
        class(QuantityCamel_t), intent(in) :: quantity_lower2
        type(QuantityCamel_t) :: new_quantity_lower

        new_quantity_lower%units_lower = &
                quantity_lower1%units_lower + quantity_lower2%units_lower
    end function quantitySnakePlusQuantityCamel

    elemental function quantitySnakeMinusQuantityCamel( &
            quantity_lower1, quantity_lower2) result(new_quantity_lower)
        class(QuantityCamel_t), intent(in) :: quantity_lower1
        class(QuantityCamel_t), intent(in) :: quantity_lower2
        type(QuantityCamel_t) :: new_quantity_lower

        new_quantity_lower%units_lower = &
                quantity_lower1%units_lower - quantity_lower2%units_lower
    end function quantitySnakeMinusQuantityCamel

    pure function sumQuantityCamel(quantity_lowers)
        type(QuantityCamel_t), intent(in) :: quantity_lowers(:)
        type(QuantityCamel_t) :: sumQuantityCamel

        sumQuantityCamel%units_lower = sum(quantity_lowers%units_lower)
    end function sumQuantityCamel

    elemental function greaterThan(lhs, rhs)
        class(QuantityCamel_t), intent(in) :: lhs
        class(QuantityCamel_t), intent(in) :: rhs
        logical :: greaterThan

        greaterThan = lhs%units_lower > rhs%units_lower
    end function greaterThan

    elemental function lessThan(lhs,rhs)
        class(QuantityCamel_t), intent(in) :: lhs
        class(QuantityCamel_t), intent(in) :: rhs
        logical :: lessThan

        lessThan = lhs%units_lower < rhs%units_lower
    end function lessThan

    elemental function greaterThanOrEqual(lhs, rhs)
        class(QuantityCamel_t), intent(in) :: lhs
        class(QuantityCamel_t), intent(in) :: rhs
        logical :: greaterThanOrEqual

        greaterThanOrEqual = lhs%units_lower >= rhs%units_lower
    end function greaterThanOrEqual

    elemental function lessThanOrEqual(lhs, rhs)
        class(QuantityCamel_t), intent(in) :: lhs
        class(QuantityCamel_t), intent(in) :: rhs
        logical :: lessThanOrEqual

        lessThanOrEqual = lhs%units_lower <= rhs%units_lower
    end function lessThanOrEqual

    elemental function equal_(lhs,rhs)
        class(QuantityCamel_t), intent(in) :: lhs
        class(QuantityCamel_t), intent(in) :: rhs
        logical :: equal_

        equal_ = lhs%units_lower .safeEq. rhs%units_lower
    end function equal_

    elemental function equalWithinAbsolute(lhs, rhs, within)
        class(QuantityCamel_t), intent(in) :: lhs
        class(QuantityCamel_t), intent(in) :: rhs
        class(QuantityCamel_t), intent(in) :: within
        logical :: equalWithinAbsolute

        equalWithinAbsolute = equalWithinAbsolute_( &
                lhs%units_lower, rhs%units_lower, within%units_lower)
    end function equalWithinAbsolute

    elemental function equalWithinRelative(lhs, rhs, within)
        class(QuantityCamel_t), intent(in) :: lhs
        class(QuantityCamel_t), intent(in) :: rhs
        double precision, intent(in) :: within
        logical :: equalWithinRelative

        equalWithinRelative = equalWithinRelative_( &
                lhs%units_lower, rhs%units_lower, within)
    end function equalWithinRelative

    elemental function notEqual(lhs, rhs)
        class(QuantityCamel_t), intent(in) :: lhs
        class(QuantityCamel_t), intent(in) :: rhs
        logical :: notEqual

        notEqual = .not. lhs == rhs
    end function notEqual

    elemental function toStringFullPrecision(self) result(string)
        class(QuantityCamel_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = self%toStringIn(DEFAULT_OUTPUT_UNITS)
    end function toStringFullPrecision

    elemental function toStringWithPrecision(self, significant_digits) result(string)
        class(QuantityCamel_t), intent(in) :: self
        integer, intent(in) :: significant_digits
        type(VARYING_STRING) :: string

        string = self%toStringIn(DEFAULT_OUTPUT_UNITS, significant_digits)
    end function toStringWithPrecision

    elemental function toStringInFullPrecision(self, units) result(string)
        class(QuantityCamel_t), intent(in) :: self
        class(QuantityCamelUnit_t), intent(in) :: units
        type(VARYING_STRING) :: string

        string = units%toString(toString(self.in.units))
    end function toStringInFullPrecision

    elemental function toStringInWithPrecision( &
            self, units, significant_digits) result(string)
        class(QuantityCamel_t), intent(in) :: self
        class(QuantityCamelUnit_t), intent(in) :: units
        integer, intent(in) :: significant_digits
        type(VARYING_STRING) :: string

        string = units%toString(toString(self.in.units, significant_digits))
    end function toStringInWithPrecision

    elemental function simpleUnitToString(self) result(string)
        class(QuantityCamelSimpleUnit_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = trim(self%symbol)
    end function simpleUnitToString

    pure function simpleValueToString(self, value_) result(string)
        class(QuantityCamelSimpleUnit_t), intent(in) :: self
        type(VARYING_STRING), intent(in) :: value_
        type(VARYING_STRING) :: string

        string = value_ // " " // self%toString()
    end function simpleValueToString

    pure subroutine simpleParseAs(self, string, errors, quantity_lower)
        class(QuantityCamelSimpleUnit_t), intent(in) :: self
        type(VARYING_STRING), intent(in) :: string
        type(ErrorList_t), intent(out) :: errors
        type(QuantityCamel_t), intent(out) :: quantity_lower

        type(ParseResult_t) :: parse_result

        parse_result = parseWith(theParser, string)
        if (parse_result%ok) then
            select type (the_number => parse_result%parsed)
            type is (ParsedRational_t)
                quantity_lower = the_number%value_.unit.self
            end select
        else
            call errors%appendError(Fatal( &
                    PARSE_ERROR, &
                    Module_("Quantity_module_m"), &
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

    pure subroutine gnuplotParseAs(self, string, errors, quantity_lower)
        class(QuantityCamelGnuplotUnit_t), intent(in) :: self
        type(VARYING_STRING), intent(in) :: string
        type(ErrorList_t), intent(out) :: errors
        type(QuantityCamel_t), intent(out) :: quantity_lower

        type(ParseResult_t) :: parse_result

        parse_result = parseWith(theParser, string)
        if (parse_result%ok) then
            select type (the_number => parse_result%parsed)
            type is (ParsedRational_t)
                quantity_lower = the_number%value_.unit.self
            end select
        else
            call errors%appendError(Fatal( &
                    PARSE_ERROR, &
                    Module_("Quantity_module_m"), &
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

    pure subroutine latexParseAs(self, string, errors, quantity_lower)
        class(QuantityCamelLatexUnit_t), intent(in) :: self
        type(VARYING_STRING), intent(in) :: string
        type(ErrorList_t), intent(out) :: errors
        type(QuantityCamel_t), intent(out) :: quantity_lower

        type(ParseResult_t) :: parse_result

        parse_result = parseWith(theParser, string)
        if (parse_result%ok) then
            select type (the_number => parse_result%parsed)
            type is (ParsedRational_t)
                quantity_lower = the_number%value_.unit.self
            end select
        else
            call errors%appendError(Fatal( &
                    PARSE_ERROR, &
                    Module_("Quantity_module_m"), &
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
        class(QuantityCamelGnuplotUnit_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = trim(self%symbol)
    end function gnuplotUnitToString

    pure function gnuplotValueToString(self, value_) result(string)
        class(QuantityCamelGnuplotUnit_t), intent(in) :: self
        type(VARYING_STRING), intent(in) :: value_
        type(VARYING_STRING) :: string

        string = value_ // " " // self%toString()
    end function gnuplotValueToString

    elemental function latexUnitToString(self) result(string)
        class(QuantityCamelLatexUnit_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = wrapInLatexUnit(trim(self%symbol))
    end function latexUnitToString

    pure function latexValueToString(self, value_) result(string)
        class(QuantityCamelLatexUnit_t), intent(in) :: self
        type(VARYING_STRING), intent(in) :: value_
        type(VARYING_STRING) :: string

        string = wrapInLatexQuantity(value_, trim(self%symbol))
    end function latexValueToString

    pure subroutine simpleUnitFromStringC(string, errors, unit)
        character(len=*), intent(in) :: string
        type(ErrorList_t), intent(out) :: errors
        type(QuantityCamelSimpleUnit_t), intent(out) :: unit

        type(ErrorList_t) :: errors_

        call fromString(var_str(string), PROVIDED_UNITS, errors_, unit)
        call errors%appendErrors( &
                errors_, &
                Module_("Quantity_module_m"), &
                Procedure_("simpleUnitFromStringC"))
    end subroutine simpleUnitFromStringC

    pure subroutine simpleUnitFromStringS(string, errors, unit)
        type(VARYING_STRING), intent(in) :: string
        type(ErrorList_t), intent(out) :: errors
        type(QuantityCamelSimpleUnit_t), intent(out) :: unit

        type(ErrorList_t) :: errors_

        call fromString(string, PROVIDED_UNITS, errors_, unit)
        call errors%appendErrors( &
                errors_, &
                Module_("Quantity_module_m"), &
                Procedure_("simpleUnitFromStringS"))
    end subroutine simpleUnitFromStringS

    pure subroutine simpleUnitFromStringWithUnitsC(string, units, errors, unit)
        character(len=*), intent(in) :: string
        type(QuantityCamelSimpleUnit_t), intent(in) :: units(:)
        type(ErrorList_t), intent(out) :: errors
        type(QuantityCamelSimpleUnit_t), intent(out) :: unit

        type(ErrorList_t) :: errors_

        call fromString(var_str(string), units, errors_, unit)
        call errors%appendErrors( &
                errors_, &
                Module_("Quantity_module_m"), &
                Procedure_("simpleUnitFromStringWithUnitsC"))
    end subroutine simpleUnitFromStringWithUnitsC

    pure subroutine simpleUnitFromStringWithUnitsS(string, units, errors, unit)
        type(VARYING_STRING), intent(in) :: string
        type(QuantityCamelSimpleUnit_t), intent(in) :: units(:)
        type(ErrorList_t), intent(out) :: errors
        type(QuantityCamelSimpleUnit_t), intent(out) :: unit

        type(ErrorList_t) :: errors_
        integer :: which_unit

        call selectUnit(string, units, errors, which_unit)
        if (errors_%hasAny()) then
            call errors%appendErrors( &
                    errors_, &
                    Module_("Quantity_module_m"), &
                    Procedure_("simpleUnitFromStringWithUnitsS"))
        else
            unit = units(which_unit)
        end if
    end subroutine simpleUnitFromStringWithUnitsS

    pure subroutine selectUnit(string, units, errors, index)
        type(VARYING_STRING), intent(in) :: string
        class(QuantityCamelUnit_t), intent(in) :: units(:)
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
                Module_("Quantity_module_m"), &
                Procedure_("selectUnit"), &
                '"' // string // '", known units: [' // join(unit_strings, ', ') // ']'))
    end subroutine selectUnit
end module Quantity_module_m
