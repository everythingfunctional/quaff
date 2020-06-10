module Speed_m
    use Conversion_factors_m, only: &
            CENTIMETERS_PER_SECOND_PER_METERS_PER_SECOND, &
            FEET_PER_SECOND_PER_METERS_PER_SECOND
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

    type, public :: Speed_t
        double precision :: meters_per_second
    contains
        private
        procedure :: toUnits
        generic, public :: operator(.in.) => toUnits
        procedure, pass(speed) :: doubleTimesSpeed
        procedure, pass(speed) :: integerTimesSpeed
        procedure, pass(speed) :: speedTimesDouble
        procedure, pass(speed) :: speedTimesInteger
        generic, public :: operator(*) => &
                doubleTimesSpeed, &
                integerTimesSpeed, &
                speedTimesDouble, &
                speedTimesInteger
        procedure :: speedDividedByDouble
        procedure :: speedDividedByInteger
        procedure, pass(numerator) :: speedDividedBySpeed
        generic, public :: operator(/) => &
                speedDividedByDouble, &
                speedDividedByInteger, &
                speedDividedBySpeed
        procedure :: speedPlusSpeed
        generic, public :: operator(+) => speedPlusSpeed
        procedure :: speedMinusSpeed
        generic, public :: operator(-) => speedMinusSpeed
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
    end type Speed_t

    type, abstract, public :: SpeedUnit_t
        double precision :: conversion_factor
    contains
        procedure(justUnitToString), deferred :: unitToString
        procedure(unitWithValueToString), deferred :: valueToString
        generic :: toString => unitToString, valueToString
        procedure(parseAsI), deferred :: parseAs
    end type SpeedUnit_t

    type, extends(SpeedUnit_t), public :: SpeedSimpleUnit_t
        character(len=20) :: symbol
    contains
        procedure :: unitToString => simpleUnitToString
        procedure :: valueToString => simpleValueToString
        procedure :: parseAs => simpleParseAs
    end type SpeedSimpleUnit_t

    type, extends(SpeedUnit_t), public :: SpeedGnuplotUnit_t
        character(len=50) :: symbol
    contains
        procedure :: unitToString => gnuplotUnitToString
        procedure :: valueToString => gnuplotValueToString
        procedure :: parseAs => gnuplotParseAs
    end type SpeedGnuplotUnit_t

    type, extends(SpeedUnit_t), public :: SpeedLatexUnit_t
        character(len=100) :: symbol
    contains
        procedure :: unitToString => latexUnitToString
        procedure :: valueToString => latexValueToString
        procedure :: parseAs => latexParseAs
    end type SpeedLatexUnit_t

    abstract interface
        elemental function justUnitToString(self) result(string)
            import SpeedUnit_t, VARYING_STRING
            class(SpeedUnit_t), intent(in) :: self
            type(VARYING_STRING) :: string
        end function justUnitToString

        pure function unitWithValueToString(self, value_) result(string)
            import SpeedUnit_t, VARYING_STRING
            class(SpeedUnit_t), intent(in) :: self
            type(VARYING_STRING), intent(in) :: value_
            type(VARYING_STRING) :: string
        end function unitWithValueToString

        pure subroutine parseAsI(self, string, errors, speed)
            import ErrorList_t, Speed_t, SpeedUnit_t, VARYING_STRING
            class(SpeedUnit_t), intent(in) :: self
            type(VARYING_STRING), intent(in) :: string
            type(ErrorList_t), intent(out) :: errors
            type(Speed_t), intent(out) :: speed
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
        module procedure sumSpeed
    end interface sum

    type(SpeedSimpleUnit_t), parameter, public :: CENTIMETERS_PER_SECOND = &
            SpeedSimpleUnit_t( &
                    conversion_factor = CENTIMETERS_PER_SECOND_PER_METERS_PER_SECOND, &
                    symbol = "cm/s")
    type(SpeedGnuplotUnit_t), parameter, public :: CENTIMETERS_PER_SECOND_GNUPLOT = &
            SpeedGnuplotUnit_t( &
                    conversion_factor = CENTIMETERS_PER_SECOND_PER_METERS_PER_SECOND, &
                    symbol = "cm/s")
    type(SpeedLatexUnit_t), parameter, public :: CENTIMETERS_PER_SECOND_LATEX = &
            SpeedLatexUnit_t( &
                    conversion_factor = CENTIMETERS_PER_SECOND_PER_METERS_PER_SECOND, &
                    symbol = "\centi\meter\per\second")
    type(SpeedSimpleUnit_t), parameter, public :: FEET_PER_SECOND = &
            SpeedSimpleUnit_t( &
                    conversion_factor = FEET_PER_SECOND_PER_METERS_PER_SECOND, &
                    symbol = "ft/s")
    type(SpeedGnuplotUnit_t), parameter, public :: FEET_PER_SECOND_GNUPLOT = &
            SpeedGnuplotUnit_t( &
                    conversion_factor = FEET_PER_SECOND_PER_METERS_PER_SECOND, &
                    symbol = "ft/s")
    type(SpeedSimpleUnit_t), parameter, public :: METERS_PER_SECOND = &
            SpeedSimpleUnit_t( &
                    conversion_factor = 1.0d0, &
                    symbol = "m/s")
    type(SpeedGnuplotUnit_t), parameter, public :: METERS_PER_SECOND_GNUPLOT = &
            SpeedGnuplotUnit_t( &
                    conversion_factor = 1.0d0, &
                    symbol = "m/s")
    type(SpeedLatexUnit_t), parameter, public :: METERS_PER_SECOND_LATEX = &
            SpeedLatexUnit_t( &
                    conversion_factor = 1.0d0, &
                    symbol = "\meter\per\second")

    type(SpeedSimpleUnit_t), public :: DEFAULT_OUTPUT_UNITS = METERS_PER_SECOND

    type(SpeedSimpleUnit_t), parameter, public :: PROVIDED_UNITS(*) = &
            [CENTIMETERS_PER_SECOND, FEET_PER_SECOND, METERS_PER_SECOND]
    type(SpeedGnuplotUnit_t), parameter, public :: PROVIDED_GNUPLOT_UNITS(*) = &
            [CENTIMETERS_PER_SECOND_GNUPLOT, &
            FEET_PER_SECOND_GNUPLOT, &
            METERS_PER_SECOND_GNUPLOT]
    type(SpeedLatexUnit_t), parameter, public :: PROVIDED_LATEX_UNITS(*) = &
            [CENTIMETERS_PER_SECOND_LATEX, METERS_PER_SECOND_LATEX]

    public :: operator(.unit.), fromString, selectUnit, sum
contains
    pure subroutine fromStringBasicC(string, errors, speed)
        character(len=*), intent(in) :: string
        type(ErrorList_t), intent(out) :: errors
        type(Speed_t), intent(out) :: speed

        type(ErrorList_t) :: errors_

        call fromString( &
                var_str(string), PROVIDED_UNITS, errors_, speed)
        call errors%appendErrors( &
                errors_, &
                Module_("Speed_m"), &
                Procedure_("fromStringBasicC"))
    end subroutine fromStringBasicC

    pure subroutine fromStringBasicS(string, errors, speed)
        type(VARYING_STRING), intent(in) :: string
        type(ErrorList_t), intent(out) :: errors
        type(Speed_t), intent(out) :: speed

        type(ErrorList_t) :: errors_

        call fromString( &
                string, PROVIDED_UNITS, errors_, speed)
        call errors%appendErrors( &
                errors_, &
                Module_("Speed_m"), &
                Procedure_("fromStringBasicS"))
    end subroutine fromStringBasicS

    pure subroutine fromStringWithUnitsC(string, units, errors, speed)
        character(len=*), intent(in) :: string
        class(SpeedUnit_t), intent(in) :: units(:)
        type(ErrorList_t), intent(out) :: errors
        type(Speed_t), intent(out) :: speed

        type(ErrorList_t) :: errors_

        call fromString( &
                var_str(string), units, errors_, speed)
        call errors%appendErrors( &
                errors_, &
                Module_("Speed_m"), &
                Procedure_("fromStringWithUnitsC"))
    end subroutine fromStringWithUnitsC

    pure subroutine fromStringWithUnitsS(string, units, errors, speed)
        type(VARYING_STRING), intent(in) :: string
        class(SpeedUnit_t), intent(in) :: units(:)
        type(ErrorList_t), intent(out) :: errors
        type(Speed_t), intent(out) :: speed

        type(ErrorList_t) :: all_errors(size(units))
        integer :: i

        do i = 1, size(units)
            call units(i)%parseAs(string, all_errors(i), speed)
            if (.not. all_errors(i)%hasAny()) return
        end do
        do i = 1, size(units)
            call errors%appendErrors( &
                    all_errors(i), &
                    Module_("Speed_m"), &
                    Procedure_("fromStringWithUnitsS"))
        end do
    end subroutine fromStringWithUnitsS

    elemental function fromUnits(value_, units) result(speed)
        double precision, intent(in) :: value_
        class(SpeedUnit_t), intent(in) :: units
        type(Speed_t) :: speed

        speed%meters_per_second = value_ / units%conversion_factor
    end function fromUnits

    elemental function toUnits(self, units) result(speed)
        class(Speed_t), intent(in) :: self
        class(SpeedUnit_t), intent(in) :: units
        double precision :: speed

        speed = self%meters_per_second * units%conversion_factor
    end function toUnits

    elemental function doubleTimesSpeed( &
            multiplier, speed) result(new_speed)
        double precision, intent(in) :: multiplier
        class(Speed_t), intent(in) :: speed
        type(Speed_t) :: new_speed

        new_speed%meters_per_second = &
                multiplier * speed%meters_per_second
    end function doubleTimesSpeed

    elemental function integerTimesSpeed( &
            multiplier, speed) result(new_speed)
        integer, intent(in) :: multiplier
        class(Speed_t), intent(in) :: speed
        type(Speed_t) :: new_speed

        new_speed%meters_per_second = &
                dble(multiplier) * speed%meters_per_second
    end function integerTimesSpeed

    elemental function speedTimesDouble( &
            speed, multiplier) result(new_speed)
        class(Speed_t), intent(in) :: speed
        double precision, intent(in) :: multiplier
        type(Speed_t) :: new_speed

        new_speed%meters_per_second = &
                speed%meters_per_second * multiplier
    end function speedTimesDouble

    elemental function speedTimesInteger( &
            speed, multiplier) result(new_speed)
        class(Speed_t), intent(in) :: speed
        integer, intent(in) :: multiplier
        type(Speed_t) :: new_speed

        new_speed%meters_per_second = &
                speed%meters_per_second * dble(multiplier)
    end function speedTimesInteger

    elemental function speedDividedByDouble( &
            speed, divisor) result(new_speed)
        class(Speed_t), intent(in) :: speed
        double precision, intent(in) :: divisor
        type(Speed_t) :: new_speed

        new_speed%meters_per_second = &
                speed%meters_per_second / divisor
    end function speedDividedByDouble

    elemental function speedDividedByInteger( &
            speed, divisor) result(new_speed)
        class(Speed_t), intent(in) :: speed
        integer, intent(in) :: divisor
        type(Speed_t) :: new_speed

        new_speed%meters_per_second = &
                speed%meters_per_second / dble(divisor)
    end function speedDividedByInteger

    elemental function speedDividedBySpeed( &
            numerator, denomenator) result(ratio)
        class(Speed_t), intent(in) :: numerator
        class(Speed_t), intent(in) :: denomenator
        double precision :: ratio

        ratio = numerator%meters_per_second / denomenator%meters_per_second
    end function speedDividedBySpeed

    elemental function speedPlusSpeed( &
            speed1, speed2) result(new_speed)
        class(Speed_t), intent(in) :: speed1
        class(Speed_t), intent(in) :: speed2
        type(Speed_t) :: new_speed

        new_speed%meters_per_second = &
                speed1%meters_per_second + speed2%meters_per_second
    end function speedPlusSpeed

    elemental function speedMinusSpeed( &
            speed1, speed2) result(new_speed)
        class(Speed_t), intent(in) :: speed1
        class(Speed_t), intent(in) :: speed2
        type(Speed_t) :: new_speed

        new_speed%meters_per_second = &
                speed1%meters_per_second - speed2%meters_per_second
    end function speedMinusSpeed

    pure function sumSpeed(speeds)
        type(Speed_t), intent(in) :: speeds(:)
        type(Speed_t) :: sumSpeed

        sumSpeed%meters_per_second = sum(speeds%meters_per_second)
    end function sumSpeed

    elemental function greaterThan(lhs, rhs)
        class(Speed_t), intent(in) :: lhs
        class(Speed_t), intent(in) :: rhs
        logical :: greaterThan

        greaterThan = lhs%meters_per_second > rhs%meters_per_second
    end function greaterThan

    elemental function lessThan(lhs,rhs)
        class(Speed_t), intent(in) :: lhs
        class(Speed_t), intent(in) :: rhs
        logical :: lessThan

        lessThan = lhs%meters_per_second < rhs%meters_per_second
    end function lessThan

    elemental function greaterThanOrEqual(lhs, rhs)
        class(Speed_t), intent(in) :: lhs
        class(Speed_t), intent(in) :: rhs
        logical :: greaterThanOrEqual

        greaterThanOrEqual = lhs%meters_per_second >= rhs%meters_per_second
    end function greaterThanOrEqual

    elemental function lessThanOrEqual(lhs, rhs)
        class(Speed_t), intent(in) :: lhs
        class(Speed_t), intent(in) :: rhs
        logical :: lessThanOrEqual

        lessThanOrEqual = lhs%meters_per_second <= rhs%meters_per_second
    end function lessThanOrEqual

    elemental function equal_(lhs,rhs)
        class(Speed_t), intent(in) :: lhs
        class(Speed_t), intent(in) :: rhs
        logical :: equal_

        equal_ = lhs%meters_per_second .safeEq. rhs%meters_per_second
    end function equal_

    elemental function equalWithinAbsolute(lhs, rhs, within)
        class(Speed_t), intent(in) :: lhs
        class(Speed_t), intent(in) :: rhs
        class(Speed_t), intent(in) :: within
        logical :: equalWithinAbsolute

        equalWithinAbsolute = equalWithinAbsolute_( &
                lhs%meters_per_second, rhs%meters_per_second, within%meters_per_second)
    end function equalWithinAbsolute

    elemental function equalWithinRelative(lhs, rhs, within)
        class(Speed_t), intent(in) :: lhs
        class(Speed_t), intent(in) :: rhs
        double precision, intent(in) :: within
        logical :: equalWithinRelative

        equalWithinRelative = equalWithinRelative_( &
                lhs%meters_per_second, rhs%meters_per_second, within)
    end function equalWithinRelative

    elemental function notEqual(lhs, rhs)
        class(Speed_t), intent(in) :: lhs
        class(Speed_t), intent(in) :: rhs
        logical :: notEqual

        notEqual = .not. lhs == rhs
    end function notEqual

    elemental function toStringFullPrecision(self) result(string)
        class(Speed_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = self%toStringIn(DEFAULT_OUTPUT_UNITS)
    end function toStringFullPrecision

    elemental function toStringWithPrecision(self, significant_digits) result(string)
        class(Speed_t), intent(in) :: self
        integer, intent(in) :: significant_digits
        type(VARYING_STRING) :: string

        string = self%toStringIn(DEFAULT_OUTPUT_UNITS, significant_digits)
    end function toStringWithPrecision

    elemental function toStringInFullPrecision(self, units) result(string)
        class(Speed_t), intent(in) :: self
        class(SpeedUnit_t), intent(in) :: units
        type(VARYING_STRING) :: string

        string = units%toString(toString(self.in.units))
    end function toStringInFullPrecision

    elemental function toStringInWithPrecision( &
            self, units, significant_digits) result(string)
        class(Speed_t), intent(in) :: self
        class(SpeedUnit_t), intent(in) :: units
        integer, intent(in) :: significant_digits
        type(VARYING_STRING) :: string

        string = units%toString(toString(self.in.units, significant_digits))
    end function toStringInWithPrecision

    elemental function simpleUnitToString(self) result(string)
        class(SpeedSimpleUnit_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = trim(self%symbol)
    end function simpleUnitToString

    pure function simpleValueToString(self, value_) result(string)
        class(SpeedSimpleUnit_t), intent(in) :: self
        type(VARYING_STRING), intent(in) :: value_
        type(VARYING_STRING) :: string

        string = value_ // " " // self%toString()
    end function simpleValueToString

    pure subroutine simpleParseAs(self, string, errors, speed)
        class(SpeedSimpleUnit_t), intent(in) :: self
        type(VARYING_STRING), intent(in) :: string
        type(ErrorList_t), intent(out) :: errors
        type(Speed_t), intent(out) :: speed

        type(ParseResult_t) :: parse_result

        parse_result = parseWith(theParser, string)
        if (parse_result%ok) then
            select type (the_number => parse_result%parsed)
            type is (ParsedRational_t)
                speed = the_number%value_.unit.self
            end select
        else
            call errors%appendError(Fatal( &
                    PARSE_ERROR, &
                    Module_("Speed_m"), &
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

    pure subroutine gnuplotParseAs(self, string, errors, speed)
        class(SpeedGnuplotUnit_t), intent(in) :: self
        type(VARYING_STRING), intent(in) :: string
        type(ErrorList_t), intent(out) :: errors
        type(Speed_t), intent(out) :: speed

        type(ParseResult_t) :: parse_result

        parse_result = parseWith(theParser, string)
        if (parse_result%ok) then
            select type (the_number => parse_result%parsed)
            type is (ParsedRational_t)
                speed = the_number%value_.unit.self
            end select
        else
            call errors%appendError(Fatal( &
                    PARSE_ERROR, &
                    Module_("Speed_m"), &
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

    pure subroutine latexParseAs(self, string, errors, speed)
        class(SpeedLatexUnit_t), intent(in) :: self
        type(VARYING_STRING), intent(in) :: string
        type(ErrorList_t), intent(out) :: errors
        type(Speed_t), intent(out) :: speed

        type(ParseResult_t) :: parse_result

        parse_result = parseWith(theParser, string)
        if (parse_result%ok) then
            select type (the_number => parse_result%parsed)
            type is (ParsedRational_t)
                speed = the_number%value_.unit.self
            end select
        else
            call errors%appendError(Fatal( &
                    PARSE_ERROR, &
                    Module_("Speed_m"), &
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
        class(SpeedGnuplotUnit_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = trim(self%symbol)
    end function gnuplotUnitToString

    pure function gnuplotValueToString(self, value_) result(string)
        class(SpeedGnuplotUnit_t), intent(in) :: self
        type(VARYING_STRING), intent(in) :: value_
        type(VARYING_STRING) :: string

        string = value_ // " " // self%toString()
    end function gnuplotValueToString

    elemental function latexUnitToString(self) result(string)
        class(SpeedLatexUnit_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = wrapInLatexUnit(trim(self%symbol))
    end function latexUnitToString

    pure function latexValueToString(self, value_) result(string)
        class(SpeedLatexUnit_t), intent(in) :: self
        type(VARYING_STRING), intent(in) :: value_
        type(VARYING_STRING) :: string

        string = wrapInLatexQuantity(value_, trim(self%symbol))
    end function latexValueToString

    pure subroutine simpleUnitFromStringC(string, errors, unit)
        character(len=*), intent(in) :: string
        type(ErrorList_t), intent(out) :: errors
        type(SpeedSimpleUnit_t), intent(out) :: unit

        type(ErrorList_t) :: errors_

        call fromString(var_str(string), PROVIDED_UNITS, errors_, unit)
        call errors%appendErrors( &
                errors_, &
                Module_("Speed_m"), &
                Procedure_("simpleUnitFromStringC"))
    end subroutine simpleUnitFromStringC

    pure subroutine simpleUnitFromStringS(string, errors, unit)
        type(VARYING_STRING), intent(in) :: string
        type(ErrorList_t), intent(out) :: errors
        type(SpeedSimpleUnit_t), intent(out) :: unit

        type(ErrorList_t) :: errors_

        call fromString(string, PROVIDED_UNITS, errors_, unit)
        call errors%appendErrors( &
                errors_, &
                Module_("Speed_m"), &
                Procedure_("simpleUnitFromStringS"))
    end subroutine simpleUnitFromStringS

    pure subroutine simpleUnitFromStringWithUnitsC(string, units, errors, unit)
        character(len=*), intent(in) :: string
        type(SpeedSimpleUnit_t), intent(in) :: units(:)
        type(ErrorList_t), intent(out) :: errors
        type(SpeedSimpleUnit_t), intent(out) :: unit

        type(ErrorList_t) :: errors_

        call fromString(var_str(string), units, errors_, unit)
        call errors%appendErrors( &
                errors_, &
                Module_("Speed_m"), &
                Procedure_("simpleUnitFromStringWithUnitsC"))
    end subroutine simpleUnitFromStringWithUnitsC

    pure subroutine simpleUnitFromStringWithUnitsS(string, units, errors, unit)
        type(VARYING_STRING), intent(in) :: string
        type(SpeedSimpleUnit_t), intent(in) :: units(:)
        type(ErrorList_t), intent(out) :: errors
        type(SpeedSimpleUnit_t), intent(out) :: unit

        type(ErrorList_t) :: errors_
        integer :: which_unit

        call selectUnit(string, units, errors, which_unit)
        if (errors_%hasAny()) then
            call errors%appendErrors( &
                    errors_, &
                    Module_("Speed_m"), &
                    Procedure_("simpleUnitFromStringWithUnitsS"))
        else
            unit = units(which_unit)
        end if
    end subroutine simpleUnitFromStringWithUnitsS

    pure subroutine selectUnit(string, units, errors, index)
        type(VARYING_STRING), intent(in) :: string
        class(SpeedUnit_t), intent(in) :: units(:)
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
                Module_("Speed_m"), &
                Procedure_("selectUnit"), &
                '"' // string // '", known units: [' // join(unit_strings, ', ') // ']'))
    end subroutine selectUnit
end module Speed_m
