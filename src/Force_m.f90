module Force_m
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
    use quaff_Conversion_factors_m, only: &
            DYNES_PER_NEWTON, &
            KILOPONDS_PER_NEWTON, &
            MILLINEWTONS_PER_NEWTON, &
            POUNDS_PER_NEWTON
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

    type, public :: Force_t
        double precision :: newtons
    contains
        private
        procedure :: toUnits
        generic, public :: operator(.in.) => toUnits
        procedure, pass(force) :: doubleTimesForce
        procedure, pass(force) :: integerTimesForce
        procedure, pass(force) :: forceTimesDouble
        procedure, pass(force) :: forceTimesInteger
        generic, public :: operator(*) => &
                doubleTimesForce, &
                integerTimesForce, &
                forceTimesDouble, &
                forceTimesInteger
        procedure :: forceDividedByDouble
        procedure :: forceDividedByInteger
        procedure, pass(numerator) :: forceDividedByForce
        generic, public :: operator(/) => &
                forceDividedByDouble, &
                forceDividedByInteger, &
                forceDividedByForce
        procedure :: forcePlusForce
        generic, public :: operator(+) => forcePlusForce
        procedure :: forceMinusForce
        generic, public :: operator(-) => forceMinusForce
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
    end type Force_t

    type, abstract, public :: ForceUnit_t
        double precision :: conversion_factor
    contains
        procedure(justUnitToString), deferred :: unitToString
        procedure(unitWithValueToString), deferred :: valueToString
        generic :: toString => unitToString, valueToString
        procedure(parseAsI), deferred :: parseAs
    end type ForceUnit_t

    type, extends(ForceUnit_t), public :: ForceSimpleUnit_t
        character(len=20) :: symbol
    contains
        procedure :: unitToString => simpleUnitToString
        procedure :: valueToString => simpleValueToString
        procedure :: parseAs => simpleParseAs
    end type ForceSimpleUnit_t

    type, extends(ForceUnit_t), public :: ForceGnuplotUnit_t
        character(len=50) :: symbol
    contains
        procedure :: unitToString => gnuplotUnitToString
        procedure :: valueToString => gnuplotValueToString
        procedure :: parseAs => gnuplotParseAs
    end type ForceGnuplotUnit_t

    type, extends(ForceUnit_t), public :: ForceLatexUnit_t
        character(len=100) :: symbol
    contains
        procedure :: unitToString => latexUnitToString
        procedure :: valueToString => latexValueToString
        procedure :: parseAs => latexParseAs
    end type ForceLatexUnit_t

    abstract interface
        elemental function justUnitToString(self) result(string)
            import ForceUnit_t, VARYING_STRING
            class(ForceUnit_t), intent(in) :: self
            type(VARYING_STRING) :: string
        end function justUnitToString

        pure function unitWithValueToString(self, value_) result(string)
            import ForceUnit_t, VARYING_STRING
            class(ForceUnit_t), intent(in) :: self
            type(VARYING_STRING), intent(in) :: value_
            type(VARYING_STRING) :: string
        end function unitWithValueToString

        pure subroutine parseAsI(self, string, errors, force)
            import ErrorList_t, Force_t, ForceUnit_t, VARYING_STRING
            class(ForceUnit_t), intent(in) :: self
            type(VARYING_STRING), intent(in) :: string
            type(ErrorList_t), intent(out) :: errors
            type(Force_t), intent(out) :: force
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
        module procedure sumForce
    end interface sum

    type(ForceSimpleUnit_t), parameter, public :: DYNES = &
            ForceSimpleUnit_t( &
                    conversion_factor = DYNES_PER_NEWTON, &
                    symbol = "dyn")
    type(ForceGnuplotUnit_t), parameter, public :: DYNES_GNUPLOT = &
            ForceGnuplotUnit_t( &
                    conversion_factor = DYNES_PER_NEWTON, &
                    symbol = "dyn")
    type(ForceSimpleUnit_t), parameter, public :: KILOPONDS = &
            ForceSimpleUnit_t( &
                    conversion_factor = KILOPONDS_PER_NEWTON, &
                    symbol = "kp")
    type(ForceGnuplotUnit_t), parameter, public :: KILOPONDS_GNUPLOT = &
            ForceGnuplotUnit_t( &
                    conversion_factor = KILOPONDS_PER_NEWTON, &
                    symbol = "kp")
    type(ForceSimpleUnit_t), parameter, public :: MILLINEWTONS = &
            ForceSimpleUnit_t( &
                    conversion_factor = MILLINEWTONS_PER_NEWTON, &
                    symbol = "mN")
    type(ForceGnuplotUnit_t), parameter, public :: MILLINEWTONS_GNUPLOT = &
            ForceGnuplotUnit_t( &
                    conversion_factor = MILLINEWTONS_PER_NEWTON, &
                    symbol = "mN")
    type(ForceLatexUnit_t), parameter, public :: MILLINEWTONS_LATEX = &
            ForceLatexUnit_t( &
                    conversion_factor = MILLINEWTONS_PER_NEWTON, &
                    symbol = "\milli\newton")
    type(ForceSimpleUnit_t), parameter, public :: NEWTONS = &
            ForceSimpleUnit_t( &
                    conversion_factor = 1.0d0, &
                    symbol = "N")
    type(ForceGnuplotUnit_t), parameter, public :: NEWTONS_GNUPLOT = &
            ForceGnuplotUnit_t( &
                    conversion_factor = 1.0d0, &
                    symbol = "N")
    type(ForceLatexUnit_t), parameter, public :: NEWTONS_LATEX = &
            ForceLatexUnit_t( &
                    conversion_factor = 1.0d0, &
                    symbol = "\newton")
    type(ForceSimpleUnit_t), parameter, public :: POUNDS_FORCE = &
            ForceSimpleUnit_t( &
                    conversion_factor = POUNDS_PER_NEWTON, &
                    symbol = "lbf")
    type(ForceGnuplotUnit_t), parameter, public :: POUNDS_FORCE_GNUPLOT = &
            ForceGnuplotUnit_t( &
                    conversion_factor = POUNDS_PER_NEWTON, &
                    symbol = "lbf")

    type(ForceSimpleUnit_t), public :: DEFAULT_OUTPUT_UNITS = NEWTONS

    type(ForceSimpleUnit_t), parameter, public :: PROVIDED_UNITS(*) = &
            [DYNES, KILOPONDS, MILLINEWTONS, NEWTONS, POUNDS_FORCE]
    type(ForceGnuplotUnit_t), parameter, public :: PROVIDED_GNUPLOT_UNITS(*) = &
            [DYNES_GNUPLOT, &
            KILOPONDS_GNUPLOT, &
            MILLINEWTONS_GNUPLOT, &
            NEWTONS_GNUPLOT, &
            POUNDS_FORCE_GNUPLOT]
    type(ForceLatexUnit_t), parameter, public :: PROVIDED_LATEX_UNITS(*) = &
            [MILLINEWTONS_LATEX, NEWTONS_LATEX]

    public :: operator(.unit.), fromString, selectUnit, sum
contains
    pure subroutine fromStringBasicC(string, errors, force)
        character(len=*), intent(in) :: string
        type(ErrorList_t), intent(out) :: errors
        type(Force_t), intent(out) :: force

        type(ErrorList_t) :: errors_

        call fromString( &
                var_str(string), PROVIDED_UNITS, errors_, force)
        call errors%appendErrors( &
                errors_, &
                Module_("Force_m"), &
                Procedure_("fromStringBasicC"))
    end subroutine fromStringBasicC

    pure subroutine fromStringBasicS(string, errors, force)
        type(VARYING_STRING), intent(in) :: string
        type(ErrorList_t), intent(out) :: errors
        type(Force_t), intent(out) :: force

        type(ErrorList_t) :: errors_

        call fromString( &
                string, PROVIDED_UNITS, errors_, force)
        call errors%appendErrors( &
                errors_, &
                Module_("Force_m"), &
                Procedure_("fromStringBasicS"))
    end subroutine fromStringBasicS

    pure subroutine fromStringWithUnitsC(string, units, errors, force)
        character(len=*), intent(in) :: string
        class(ForceUnit_t), intent(in) :: units(:)
        type(ErrorList_t), intent(out) :: errors
        type(Force_t), intent(out) :: force

        type(ErrorList_t) :: errors_

        call fromString( &
                var_str(string), units, errors_, force)
        call errors%appendErrors( &
                errors_, &
                Module_("Force_m"), &
                Procedure_("fromStringWithUnitsC"))
    end subroutine fromStringWithUnitsC

    pure subroutine fromStringWithUnitsS(string, units, errors, force)
        type(VARYING_STRING), intent(in) :: string
        class(ForceUnit_t), intent(in) :: units(:)
        type(ErrorList_t), intent(out) :: errors
        type(Force_t), intent(out) :: force

        type(ErrorList_t) :: all_errors(size(units))
        integer :: i

        do i = 1, size(units)
            call units(i)%parseAs(string, all_errors(i), force)
            if (.not. all_errors(i)%hasAny()) return
        end do
        do i = 1, size(units)
            call errors%appendErrors( &
                    all_errors(i), &
                    Module_("Force_m"), &
                    Procedure_("fromStringWithUnitsS"))
        end do
    end subroutine fromStringWithUnitsS

    elemental function fromUnits(value_, units) result(force)
        double precision, intent(in) :: value_
        class(ForceUnit_t), intent(in) :: units
        type(Force_t) :: force

        force%newtons = value_ / units%conversion_factor
    end function fromUnits

    elemental function toUnits(self, units) result(force)
        class(Force_t), intent(in) :: self
        class(ForceUnit_t), intent(in) :: units
        double precision :: force

        force = self%newtons * units%conversion_factor
    end function toUnits

    elemental function doubleTimesForce( &
            multiplier, force) result(new_force)
        double precision, intent(in) :: multiplier
        class(Force_t), intent(in) :: force
        type(Force_t) :: new_force

        new_force%newtons = &
                multiplier * force%newtons
    end function doubleTimesForce

    elemental function integerTimesForce( &
            multiplier, force) result(new_force)
        integer, intent(in) :: multiplier
        class(Force_t), intent(in) :: force
        type(Force_t) :: new_force

        new_force%newtons = &
                dble(multiplier) * force%newtons
    end function integerTimesForce

    elemental function forceTimesDouble( &
            force, multiplier) result(new_force)
        class(Force_t), intent(in) :: force
        double precision, intent(in) :: multiplier
        type(Force_t) :: new_force

        new_force%newtons = &
                force%newtons * multiplier
    end function forceTimesDouble

    elemental function forceTimesInteger( &
            force, multiplier) result(new_force)
        class(Force_t), intent(in) :: force
        integer, intent(in) :: multiplier
        type(Force_t) :: new_force

        new_force%newtons = &
                force%newtons * dble(multiplier)
    end function forceTimesInteger

    elemental function forceDividedByDouble( &
            force, divisor) result(new_force)
        class(Force_t), intent(in) :: force
        double precision, intent(in) :: divisor
        type(Force_t) :: new_force

        new_force%newtons = &
                force%newtons / divisor
    end function forceDividedByDouble

    elemental function forceDividedByInteger( &
            force, divisor) result(new_force)
        class(Force_t), intent(in) :: force
        integer, intent(in) :: divisor
        type(Force_t) :: new_force

        new_force%newtons = &
                force%newtons / dble(divisor)
    end function forceDividedByInteger

    elemental function forceDividedByForce( &
            numerator, denomenator) result(ratio)
        class(Force_t), intent(in) :: numerator
        class(Force_t), intent(in) :: denomenator
        double precision :: ratio

        ratio = numerator%newtons / denomenator%newtons
    end function forceDividedByForce

    elemental function forcePlusForce( &
            force1, force2) result(new_force)
        class(Force_t), intent(in) :: force1
        class(Force_t), intent(in) :: force2
        type(Force_t) :: new_force

        new_force%newtons = &
                force1%newtons + force2%newtons
    end function forcePlusForce

    elemental function forceMinusForce( &
            force1, force2) result(new_force)
        class(Force_t), intent(in) :: force1
        class(Force_t), intent(in) :: force2
        type(Force_t) :: new_force

        new_force%newtons = &
                force1%newtons - force2%newtons
    end function forceMinusForce

    pure function sumForce(forces)
        type(Force_t), intent(in) :: forces(:)
        type(Force_t) :: sumForce

        sumForce%newtons = sum(forces%newtons)
    end function sumForce

    elemental function greaterThan(lhs, rhs)
        class(Force_t), intent(in) :: lhs
        class(Force_t), intent(in) :: rhs
        logical :: greaterThan

        greaterThan = lhs%newtons > rhs%newtons
    end function greaterThan

    elemental function lessThan(lhs,rhs)
        class(Force_t), intent(in) :: lhs
        class(Force_t), intent(in) :: rhs
        logical :: lessThan

        lessThan = lhs%newtons < rhs%newtons
    end function lessThan

    elemental function greaterThanOrEqual(lhs, rhs)
        class(Force_t), intent(in) :: lhs
        class(Force_t), intent(in) :: rhs
        logical :: greaterThanOrEqual

        greaterThanOrEqual = lhs%newtons >= rhs%newtons
    end function greaterThanOrEqual

    elemental function lessThanOrEqual(lhs, rhs)
        class(Force_t), intent(in) :: lhs
        class(Force_t), intent(in) :: rhs
        logical :: lessThanOrEqual

        lessThanOrEqual = lhs%newtons <= rhs%newtons
    end function lessThanOrEqual

    elemental function equal_(lhs,rhs)
        class(Force_t), intent(in) :: lhs
        class(Force_t), intent(in) :: rhs
        logical :: equal_

        equal_ = lhs%newtons .safeEq. rhs%newtons
    end function equal_

    elemental function equalWithinAbsolute(lhs, rhs, within)
        class(Force_t), intent(in) :: lhs
        class(Force_t), intent(in) :: rhs
        class(Force_t), intent(in) :: within
        logical :: equalWithinAbsolute

        equalWithinAbsolute = equalWithinAbsolute_( &
                lhs%newtons, rhs%newtons, within%newtons)
    end function equalWithinAbsolute

    elemental function equalWithinRelative(lhs, rhs, within)
        class(Force_t), intent(in) :: lhs
        class(Force_t), intent(in) :: rhs
        double precision, intent(in) :: within
        logical :: equalWithinRelative

        equalWithinRelative = equalWithinRelative_( &
                lhs%newtons, rhs%newtons, within)
    end function equalWithinRelative

    elemental function notEqual(lhs, rhs)
        class(Force_t), intent(in) :: lhs
        class(Force_t), intent(in) :: rhs
        logical :: notEqual

        notEqual = .not. lhs == rhs
    end function notEqual

    elemental function toStringFullPrecision(self) result(string)
        class(Force_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = self%toStringIn(DEFAULT_OUTPUT_UNITS)
    end function toStringFullPrecision

    elemental function toStringWithPrecision(self, significant_digits) result(string)
        class(Force_t), intent(in) :: self
        integer, intent(in) :: significant_digits
        type(VARYING_STRING) :: string

        string = self%toStringIn(DEFAULT_OUTPUT_UNITS, significant_digits)
    end function toStringWithPrecision

    elemental function toStringInFullPrecision(self, units) result(string)
        class(Force_t), intent(in) :: self
        class(ForceUnit_t), intent(in) :: units
        type(VARYING_STRING) :: string

        string = units%toString(toString(self.in.units))
    end function toStringInFullPrecision

    elemental function toStringInWithPrecision( &
            self, units, significant_digits) result(string)
        class(Force_t), intent(in) :: self
        class(ForceUnit_t), intent(in) :: units
        integer, intent(in) :: significant_digits
        type(VARYING_STRING) :: string

        string = units%toString(toString(self.in.units, significant_digits))
    end function toStringInWithPrecision

    elemental function simpleUnitToString(self) result(string)
        class(ForceSimpleUnit_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = trim(self%symbol)
    end function simpleUnitToString

    pure function simpleValueToString(self, value_) result(string)
        class(ForceSimpleUnit_t), intent(in) :: self
        type(VARYING_STRING), intent(in) :: value_
        type(VARYING_STRING) :: string

        string = value_ // " " // self%toString()
    end function simpleValueToString

    pure subroutine simpleParseAs(self, string, errors, force)
        class(ForceSimpleUnit_t), intent(in) :: self
        type(VARYING_STRING), intent(in) :: string
        type(ErrorList_t), intent(out) :: errors
        type(Force_t), intent(out) :: force

        type(ParseResult_t) :: parse_result

        parse_result = parseWith(theParser, string)
        if (parse_result%ok) then
            select type (the_number => parse_result%parsed)
            type is (ParsedRational_t)
                force = the_number%value_.unit.self
            end select
        else
            call errors%appendError(Fatal( &
                    PARSE_ERROR, &
                    Module_("Force_m"), &
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

    pure subroutine gnuplotParseAs(self, string, errors, force)
        class(ForceGnuplotUnit_t), intent(in) :: self
        type(VARYING_STRING), intent(in) :: string
        type(ErrorList_t), intent(out) :: errors
        type(Force_t), intent(out) :: force

        type(ParseResult_t) :: parse_result

        parse_result = parseWith(theParser, string)
        if (parse_result%ok) then
            select type (the_number => parse_result%parsed)
            type is (ParsedRational_t)
                force = the_number%value_.unit.self
            end select
        else
            call errors%appendError(Fatal( &
                    PARSE_ERROR, &
                    Module_("Force_m"), &
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

    pure subroutine latexParseAs(self, string, errors, force)
        class(ForceLatexUnit_t), intent(in) :: self
        type(VARYING_STRING), intent(in) :: string
        type(ErrorList_t), intent(out) :: errors
        type(Force_t), intent(out) :: force

        type(ParseResult_t) :: parse_result

        parse_result = parseWith(theParser, string)
        if (parse_result%ok) then
            select type (the_number => parse_result%parsed)
            type is (ParsedRational_t)
                force = the_number%value_.unit.self
            end select
        else
            call errors%appendError(Fatal( &
                    PARSE_ERROR, &
                    Module_("Force_m"), &
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
        class(ForceGnuplotUnit_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = trim(self%symbol)
    end function gnuplotUnitToString

    pure function gnuplotValueToString(self, value_) result(string)
        class(ForceGnuplotUnit_t), intent(in) :: self
        type(VARYING_STRING), intent(in) :: value_
        type(VARYING_STRING) :: string

        string = value_ // " " // self%toString()
    end function gnuplotValueToString

    elemental function latexUnitToString(self) result(string)
        class(ForceLatexUnit_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = wrapInLatexUnit(trim(self%symbol))
    end function latexUnitToString

    pure function latexValueToString(self, value_) result(string)
        class(ForceLatexUnit_t), intent(in) :: self
        type(VARYING_STRING), intent(in) :: value_
        type(VARYING_STRING) :: string

        string = wrapInLatexQuantity(value_, trim(self%symbol))
    end function latexValueToString

    pure subroutine simpleUnitFromStringC(string, errors, unit)
        character(len=*), intent(in) :: string
        type(ErrorList_t), intent(out) :: errors
        type(ForceSimpleUnit_t), intent(out) :: unit

        type(ErrorList_t) :: errors_

        call fromString(var_str(string), PROVIDED_UNITS, errors_, unit)
        call errors%appendErrors( &
                errors_, &
                Module_("Force_m"), &
                Procedure_("simpleUnitFromStringC"))
    end subroutine simpleUnitFromStringC

    pure subroutine simpleUnitFromStringS(string, errors, unit)
        type(VARYING_STRING), intent(in) :: string
        type(ErrorList_t), intent(out) :: errors
        type(ForceSimpleUnit_t), intent(out) :: unit

        type(ErrorList_t) :: errors_

        call fromString(string, PROVIDED_UNITS, errors_, unit)
        call errors%appendErrors( &
                errors_, &
                Module_("Force_m"), &
                Procedure_("simpleUnitFromStringS"))
    end subroutine simpleUnitFromStringS

    pure subroutine simpleUnitFromStringWithUnitsC(string, units, errors, unit)
        character(len=*), intent(in) :: string
        type(ForceSimpleUnit_t), intent(in) :: units(:)
        type(ErrorList_t), intent(out) :: errors
        type(ForceSimpleUnit_t), intent(out) :: unit

        type(ErrorList_t) :: errors_

        call fromString(var_str(string), units, errors_, unit)
        call errors%appendErrors( &
                errors_, &
                Module_("Force_m"), &
                Procedure_("simpleUnitFromStringWithUnitsC"))
    end subroutine simpleUnitFromStringWithUnitsC

    pure subroutine simpleUnitFromStringWithUnitsS(string, units, errors, unit)
        type(VARYING_STRING), intent(in) :: string
        type(ForceSimpleUnit_t), intent(in) :: units(:)
        type(ErrorList_t), intent(out) :: errors
        type(ForceSimpleUnit_t), intent(out) :: unit

        type(ErrorList_t) :: errors_
        integer :: which_unit

        call selectUnit(string, units, errors, which_unit)
        if (errors_%hasAny()) then
            call errors%appendErrors( &
                    errors_, &
                    Module_("Force_m"), &
                    Procedure_("simpleUnitFromStringWithUnitsS"))
        else
            unit = units(which_unit)
        end if
    end subroutine simpleUnitFromStringWithUnitsS

    pure subroutine selectUnit(string, units, errors, index)
        type(VARYING_STRING), intent(in) :: string
        class(ForceUnit_t), intent(in) :: units(:)
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
                Module_("Force_m"), &
                Procedure_("selectUnit"), &
                '"' // string // '", known units: [' // join(unit_strings, ', ') // ']'))
    end subroutine selectUnit
end module Force_m
