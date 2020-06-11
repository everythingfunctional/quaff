module Acceleration_m
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
            CENTIMETERS_PER_SQUARE_SECOND_PER_METERS_PER_SQUARE_SECOND, &
            FEET_PER_SQUARE_SECOND_PER_METERS_PER_SQUARE_SECOND
    use quaff_Utilities_m, only: &
            operator(.safeEq.), &
            equalWithinAbsolute_ => equalWithinAbsolute, &
            equalWithinRelative_ => equalWithinRelative, &
            parseSpace, &
            PARSE_ERROR, &
            UNKNOWN_UNIT
    use strff, only: join, toString

    implicit none
    private

    type, public :: Acceleration_t
        double precision :: meters_per_square_second
    contains
        private
        procedure :: toUnits
        generic, public :: operator(.in.) => toUnits
        procedure, pass(acceleration) :: doubleTimesAcceleration
        procedure, pass(acceleration) :: integerTimesAcceleration
        procedure, pass(acceleration) :: accelerationTimesDouble
        procedure, pass(acceleration) :: accelerationTimesInteger
        generic, public :: operator(*) => &
                doubleTimesAcceleration, &
                integerTimesAcceleration, &
                accelerationTimesDouble, &
                accelerationTimesInteger
        procedure :: accelerationDividedByDouble
        procedure :: accelerationDividedByInteger
        procedure, pass(numerator) :: accelerationDividedByAcceleration
        generic, public :: operator(/) => &
                accelerationDividedByDouble, &
                accelerationDividedByInteger, &
                accelerationDividedByAcceleration
        procedure :: accelerationPlusAcceleration
        generic, public :: operator(+) => accelerationPlusAcceleration
        procedure :: accelerationMinusAcceleration
        generic, public :: operator(-) => accelerationMinusAcceleration
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
    end type Acceleration_t

    type, abstract, public :: AccelerationUnit_t
        double precision :: conversion_factor
    contains
        procedure(justUnitToString), deferred :: unitToString
        procedure(unitWithValueToString), deferred :: valueToString
        generic :: toString => unitToString, valueToString
        procedure(parseAsI), deferred :: parseAs
    end type AccelerationUnit_t

    type, extends(AccelerationUnit_t), public :: AccelerationSimpleUnit_t
        character(len=20) :: symbol
    contains
        procedure :: unitToString => simpleUnitToString
        procedure :: valueToString => simpleValueToString
        procedure :: parseAs => simpleParseAs
    end type AccelerationSimpleUnit_t

    type, extends(AccelerationUnit_t), public :: AccelerationGnuplotUnit_t
        character(len=50) :: symbol
    contains
        procedure :: unitToString => gnuplotUnitToString
        procedure :: valueToString => gnuplotValueToString
        procedure :: parseAs => gnuplotParseAs
    end type AccelerationGnuplotUnit_t

    abstract interface
        elemental function justUnitToString(self) result(string)
            import AccelerationUnit_t, VARYING_STRING
            class(AccelerationUnit_t), intent(in) :: self
            type(VARYING_STRING) :: string
        end function justUnitToString

        pure function unitWithValueToString(self, value_) result(string)
            import AccelerationUnit_t, VARYING_STRING
            class(AccelerationUnit_t), intent(in) :: self
            type(VARYING_STRING), intent(in) :: value_
            type(VARYING_STRING) :: string
        end function unitWithValueToString

        pure subroutine parseAsI(self, string, errors, acceleration)
            import ErrorList_t, Acceleration_t, AccelerationUnit_t, VARYING_STRING
            class(AccelerationUnit_t), intent(in) :: self
            type(VARYING_STRING), intent(in) :: string
            type(ErrorList_t), intent(out) :: errors
            type(Acceleration_t), intent(out) :: acceleration
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
        module procedure sumAcceleration
    end interface sum

    type(AccelerationSimpleUnit_t), parameter, public :: CENTIMETERS_PER_SQUARE_SECOND = &
            AccelerationSimpleUnit_t( &
                    conversion_factor = CENTIMETERS_PER_SQUARE_SECOND_PER_METERS_PER_SQUARE_SECOND, &
                    symbol = "cm/s^2")
    type(AccelerationGnuplotUnit_t), parameter, public :: CENTIMETERS_PER_SQUARE_SECOND_GNUPLOT = &
            AccelerationGnuplotUnit_t( &
                    conversion_factor = CENTIMETERS_PER_SQUARE_SECOND_PER_METERS_PER_SQUARE_SECOND, &
                    symbol = "cm/s^2")
    type(AccelerationSimpleUnit_t), parameter, public :: FEET_PER_SQUARE_SECOND = &
            AccelerationSimpleUnit_t( &
                    conversion_factor = FEET_PER_SQUARE_SECOND_PER_METERS_PER_SQUARE_SECOND, &
                    symbol = "ft/s^2")
    type(AccelerationGnuplotUnit_t), parameter, public :: FEET_PER_SQUARE_SECOND_GNUPLOT = &
            AccelerationGnuplotUnit_t( &
                    conversion_factor = FEET_PER_SQUARE_SECOND_PER_METERS_PER_SQUARE_SECOND, &
                    symbol = "ft/s^2")
    type(AccelerationSimpleUnit_t), parameter, public :: METERS_PER_SQUARE_SECOND = &
            AccelerationSimpleUnit_t( &
                    conversion_factor = 1.0d0, &
                    symbol = "m/s^2")
    type(AccelerationGnuplotUnit_t), parameter, public :: METERS_PER_SQUARE_SECOND_GNUPLOT = &
            AccelerationGnuplotUnit_t( &
                    conversion_factor = 1.0d0, &
                    symbol = "m/s^2")

    type(AccelerationSimpleUnit_t), public :: DEFAULT_OUTPUT_UNITS = METERS_PER_SQUARE_SECOND

    type(AccelerationSimpleUnit_t), parameter, public :: PROVIDED_UNITS(*) = &
            [CENTIMETERS_PER_SQUARE_SECOND, &
            FEET_PER_SQUARE_SECOND, &
            METERS_PER_SQUARE_SECOND]
    type(AccelerationGnuplotUnit_t), parameter, public :: PROVIDED_GNUPLOT_UNITS(*) = &
            [CENTIMETERS_PER_SQUARE_SECOND_GNUPLOT, &
            FEET_PER_SQUARE_SECOND_GNUPLOT, &
            METERS_PER_SQUARE_SECOND_GNUPLOT]

    public :: operator(.unit.), fromString, selectUnit, sum
contains
    pure subroutine fromStringBasicC(string, errors, acceleration)
        character(len=*), intent(in) :: string
        type(ErrorList_t), intent(out) :: errors
        type(Acceleration_t), intent(out) :: acceleration

        type(ErrorList_t) :: errors_

        call fromString( &
                var_str(string), PROVIDED_UNITS, errors_, acceleration)
        call errors%appendErrors( &
                errors_, &
                Module_("Acceleration_m"), &
                Procedure_("fromStringBasicC"))
    end subroutine fromStringBasicC

    pure subroutine fromStringBasicS(string, errors, acceleration)
        type(VARYING_STRING), intent(in) :: string
        type(ErrorList_t), intent(out) :: errors
        type(Acceleration_t), intent(out) :: acceleration

        type(ErrorList_t) :: errors_

        call fromString( &
                string, PROVIDED_UNITS, errors_, acceleration)
        call errors%appendErrors( &
                errors_, &
                Module_("Acceleration_m"), &
                Procedure_("fromStringBasicS"))
    end subroutine fromStringBasicS

    pure subroutine fromStringWithUnitsC(string, units, errors, acceleration)
        character(len=*), intent(in) :: string
        class(AccelerationUnit_t), intent(in) :: units(:)
        type(ErrorList_t), intent(out) :: errors
        type(Acceleration_t), intent(out) :: acceleration

        type(ErrorList_t) :: errors_

        call fromString( &
                var_str(string), units, errors_, acceleration)
        call errors%appendErrors( &
                errors_, &
                Module_("Acceleration_m"), &
                Procedure_("fromStringWithUnitsC"))
    end subroutine fromStringWithUnitsC

    pure subroutine fromStringWithUnitsS(string, units, errors, acceleration)
        type(VARYING_STRING), intent(in) :: string
        class(AccelerationUnit_t), intent(in) :: units(:)
        type(ErrorList_t), intent(out) :: errors
        type(Acceleration_t), intent(out) :: acceleration

        type(ErrorList_t) :: all_errors(size(units))
        integer :: i

        do i = 1, size(units)
            call units(i)%parseAs(string, all_errors(i), acceleration)
            if (.not. all_errors(i)%hasAny()) return
        end do
        do i = 1, size(units)
            call errors%appendErrors( &
                    all_errors(i), &
                    Module_("Acceleration_m"), &
                    Procedure_("fromStringWithUnitsS"))
        end do
    end subroutine fromStringWithUnitsS

    elemental function fromUnits(value_, units) result(acceleration)
        double precision, intent(in) :: value_
        class(AccelerationUnit_t), intent(in) :: units
        type(Acceleration_t) :: acceleration

        acceleration%meters_per_square_second = value_ / units%conversion_factor
    end function fromUnits

    elemental function toUnits(self, units) result(acceleration)
        class(Acceleration_t), intent(in) :: self
        class(AccelerationUnit_t), intent(in) :: units
        double precision :: acceleration

        acceleration = self%meters_per_square_second * units%conversion_factor
    end function toUnits

    elemental function doubleTimesAcceleration( &
            multiplier, acceleration) result(new_acceleration)
        double precision, intent(in) :: multiplier
        class(Acceleration_t), intent(in) :: acceleration
        type(Acceleration_t) :: new_acceleration

        new_acceleration%meters_per_square_second = &
                multiplier * acceleration%meters_per_square_second
    end function doubleTimesAcceleration

    elemental function integerTimesAcceleration( &
            multiplier, acceleration) result(new_acceleration)
        integer, intent(in) :: multiplier
        class(Acceleration_t), intent(in) :: acceleration
        type(Acceleration_t) :: new_acceleration

        new_acceleration%meters_per_square_second = &
                dble(multiplier) * acceleration%meters_per_square_second
    end function integerTimesAcceleration

    elemental function accelerationTimesDouble( &
            acceleration, multiplier) result(new_acceleration)
        class(Acceleration_t), intent(in) :: acceleration
        double precision, intent(in) :: multiplier
        type(Acceleration_t) :: new_acceleration

        new_acceleration%meters_per_square_second = &
                acceleration%meters_per_square_second * multiplier
    end function accelerationTimesDouble

    elemental function accelerationTimesInteger( &
            acceleration, multiplier) result(new_acceleration)
        class(Acceleration_t), intent(in) :: acceleration
        integer, intent(in) :: multiplier
        type(Acceleration_t) :: new_acceleration

        new_acceleration%meters_per_square_second = &
                acceleration%meters_per_square_second * dble(multiplier)
    end function accelerationTimesInteger

    elemental function accelerationDividedByDouble( &
            acceleration, divisor) result(new_acceleration)
        class(Acceleration_t), intent(in) :: acceleration
        double precision, intent(in) :: divisor
        type(Acceleration_t) :: new_acceleration

        new_acceleration%meters_per_square_second = &
                acceleration%meters_per_square_second / divisor
    end function accelerationDividedByDouble

    elemental function accelerationDividedByInteger( &
            acceleration, divisor) result(new_acceleration)
        class(Acceleration_t), intent(in) :: acceleration
        integer, intent(in) :: divisor
        type(Acceleration_t) :: new_acceleration

        new_acceleration%meters_per_square_second = &
                acceleration%meters_per_square_second / dble(divisor)
    end function accelerationDividedByInteger

    elemental function accelerationDividedByAcceleration( &
            numerator, denomenator) result(ratio)
        class(Acceleration_t), intent(in) :: numerator
        class(Acceleration_t), intent(in) :: denomenator
        double precision :: ratio

        ratio = numerator%meters_per_square_second / denomenator%meters_per_square_second
    end function accelerationDividedByAcceleration

    elemental function accelerationPlusAcceleration( &
            acceleration1, acceleration2) result(new_acceleration)
        class(Acceleration_t), intent(in) :: acceleration1
        class(Acceleration_t), intent(in) :: acceleration2
        type(Acceleration_t) :: new_acceleration

        new_acceleration%meters_per_square_second = &
                acceleration1%meters_per_square_second + acceleration2%meters_per_square_second
    end function accelerationPlusAcceleration

    elemental function accelerationMinusAcceleration( &
            acceleration1, acceleration2) result(new_acceleration)
        class(Acceleration_t), intent(in) :: acceleration1
        class(Acceleration_t), intent(in) :: acceleration2
        type(Acceleration_t) :: new_acceleration

        new_acceleration%meters_per_square_second = &
                acceleration1%meters_per_square_second - acceleration2%meters_per_square_second
    end function accelerationMinusAcceleration

    pure function sumAcceleration(accelerations)
        type(Acceleration_t), intent(in) :: accelerations(:)
        type(Acceleration_t) :: sumAcceleration

        sumAcceleration%meters_per_square_second = sum(accelerations%meters_per_square_second)
    end function sumAcceleration

    elemental function greaterThan(lhs, rhs)
        class(Acceleration_t), intent(in) :: lhs
        class(Acceleration_t), intent(in) :: rhs
        logical :: greaterThan

        greaterThan = lhs%meters_per_square_second > rhs%meters_per_square_second
    end function greaterThan

    elemental function lessThan(lhs,rhs)
        class(Acceleration_t), intent(in) :: lhs
        class(Acceleration_t), intent(in) :: rhs
        logical :: lessThan

        lessThan = lhs%meters_per_square_second < rhs%meters_per_square_second
    end function lessThan

    elemental function greaterThanOrEqual(lhs, rhs)
        class(Acceleration_t), intent(in) :: lhs
        class(Acceleration_t), intent(in) :: rhs
        logical :: greaterThanOrEqual

        greaterThanOrEqual = lhs%meters_per_square_second >= rhs%meters_per_square_second
    end function greaterThanOrEqual

    elemental function lessThanOrEqual(lhs, rhs)
        class(Acceleration_t), intent(in) :: lhs
        class(Acceleration_t), intent(in) :: rhs
        logical :: lessThanOrEqual

        lessThanOrEqual = lhs%meters_per_square_second <= rhs%meters_per_square_second
    end function lessThanOrEqual

    elemental function equal_(lhs,rhs)
        class(Acceleration_t), intent(in) :: lhs
        class(Acceleration_t), intent(in) :: rhs
        logical :: equal_

        equal_ = lhs%meters_per_square_second .safeEq. rhs%meters_per_square_second
    end function equal_

    elemental function equalWithinAbsolute(lhs, rhs, within)
        class(Acceleration_t), intent(in) :: lhs
        class(Acceleration_t), intent(in) :: rhs
        class(Acceleration_t), intent(in) :: within
        logical :: equalWithinAbsolute

        equalWithinAbsolute = equalWithinAbsolute_( &
                lhs%meters_per_square_second, rhs%meters_per_square_second, within%meters_per_square_second)
    end function equalWithinAbsolute

    elemental function equalWithinRelative(lhs, rhs, within)
        class(Acceleration_t), intent(in) :: lhs
        class(Acceleration_t), intent(in) :: rhs
        double precision, intent(in) :: within
        logical :: equalWithinRelative

        equalWithinRelative = equalWithinRelative_( &
                lhs%meters_per_square_second, rhs%meters_per_square_second, within)
    end function equalWithinRelative

    elemental function notEqual(lhs, rhs)
        class(Acceleration_t), intent(in) :: lhs
        class(Acceleration_t), intent(in) :: rhs
        logical :: notEqual

        notEqual = .not. lhs == rhs
    end function notEqual

    elemental function toStringFullPrecision(self) result(string)
        class(Acceleration_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = self%toStringIn(DEFAULT_OUTPUT_UNITS)
    end function toStringFullPrecision

    elemental function toStringWithPrecision(self, significant_digits) result(string)
        class(Acceleration_t), intent(in) :: self
        integer, intent(in) :: significant_digits
        type(VARYING_STRING) :: string

        string = self%toStringIn(DEFAULT_OUTPUT_UNITS, significant_digits)
    end function toStringWithPrecision

    elemental function toStringInFullPrecision(self, units) result(string)
        class(Acceleration_t), intent(in) :: self
        class(AccelerationUnit_t), intent(in) :: units
        type(VARYING_STRING) :: string

        string = units%toString(toString(self.in.units))
    end function toStringInFullPrecision

    elemental function toStringInWithPrecision( &
            self, units, significant_digits) result(string)
        class(Acceleration_t), intent(in) :: self
        class(AccelerationUnit_t), intent(in) :: units
        integer, intent(in) :: significant_digits
        type(VARYING_STRING) :: string

        string = units%toString(toString(self.in.units, significant_digits))
    end function toStringInWithPrecision

    elemental function simpleUnitToString(self) result(string)
        class(AccelerationSimpleUnit_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = trim(self%symbol)
    end function simpleUnitToString

    pure function simpleValueToString(self, value_) result(string)
        class(AccelerationSimpleUnit_t), intent(in) :: self
        type(VARYING_STRING), intent(in) :: value_
        type(VARYING_STRING) :: string

        string = value_ // " " // self%toString()
    end function simpleValueToString

    pure subroutine simpleParseAs(self, string, errors, acceleration)
        class(AccelerationSimpleUnit_t), intent(in) :: self
        type(VARYING_STRING), intent(in) :: string
        type(ErrorList_t), intent(out) :: errors
        type(Acceleration_t), intent(out) :: acceleration

        type(ParseResult_t) :: parse_result

        parse_result = parseWith(theParser, string)
        if (parse_result%ok) then
            select type (the_number => parse_result%parsed)
            type is (ParsedRational_t)
                acceleration = the_number%value_.unit.self
            end select
        else
            call errors%appendError(Fatal( &
                    PARSE_ERROR, &
                    Module_("Acceleration_m"), &
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

    pure subroutine gnuplotParseAs(self, string, errors, acceleration)
        class(AccelerationGnuplotUnit_t), intent(in) :: self
        type(VARYING_STRING), intent(in) :: string
        type(ErrorList_t), intent(out) :: errors
        type(Acceleration_t), intent(out) :: acceleration

        type(ParseResult_t) :: parse_result

        parse_result = parseWith(theParser, string)
        if (parse_result%ok) then
            select type (the_number => parse_result%parsed)
            type is (ParsedRational_t)
                acceleration = the_number%value_.unit.self
            end select
        else
            call errors%appendError(Fatal( &
                    PARSE_ERROR, &
                    Module_("Acceleration_m"), &
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

    elemental function gnuplotUnitToString(self) result(string)
        class(AccelerationGnuplotUnit_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = trim(self%symbol)
    end function gnuplotUnitToString

    pure function gnuplotValueToString(self, value_) result(string)
        class(AccelerationGnuplotUnit_t), intent(in) :: self
        type(VARYING_STRING), intent(in) :: value_
        type(VARYING_STRING) :: string

        string = value_ // " " // self%toString()
    end function gnuplotValueToString

    pure subroutine simpleUnitFromStringC(string, errors, unit)
        character(len=*), intent(in) :: string
        type(ErrorList_t), intent(out) :: errors
        type(AccelerationSimpleUnit_t), intent(out) :: unit

        type(ErrorList_t) :: errors_

        call fromString(var_str(string), PROVIDED_UNITS, errors_, unit)
        call errors%appendErrors( &
                errors_, &
                Module_("Acceleration_m"), &
                Procedure_("simpleUnitFromStringC"))
    end subroutine simpleUnitFromStringC

    pure subroutine simpleUnitFromStringS(string, errors, unit)
        type(VARYING_STRING), intent(in) :: string
        type(ErrorList_t), intent(out) :: errors
        type(AccelerationSimpleUnit_t), intent(out) :: unit

        type(ErrorList_t) :: errors_

        call fromString(string, PROVIDED_UNITS, errors_, unit)
        call errors%appendErrors( &
                errors_, &
                Module_("Acceleration_m"), &
                Procedure_("simpleUnitFromStringS"))
    end subroutine simpleUnitFromStringS

    pure subroutine simpleUnitFromStringWithUnitsC(string, units, errors, unit)
        character(len=*), intent(in) :: string
        type(AccelerationSimpleUnit_t), intent(in) :: units(:)
        type(ErrorList_t), intent(out) :: errors
        type(AccelerationSimpleUnit_t), intent(out) :: unit

        type(ErrorList_t) :: errors_

        call fromString(var_str(string), units, errors_, unit)
        call errors%appendErrors( &
                errors_, &
                Module_("Acceleration_m"), &
                Procedure_("simpleUnitFromStringWithUnitsC"))
    end subroutine simpleUnitFromStringWithUnitsC

    pure subroutine simpleUnitFromStringWithUnitsS(string, units, errors, unit)
        type(VARYING_STRING), intent(in) :: string
        type(AccelerationSimpleUnit_t), intent(in) :: units(:)
        type(ErrorList_t), intent(out) :: errors
        type(AccelerationSimpleUnit_t), intent(out) :: unit

        type(ErrorList_t) :: errors_
        integer :: which_unit

        call selectUnit(string, units, errors, which_unit)
        if (errors_%hasAny()) then
            call errors%appendErrors( &
                    errors_, &
                    Module_("Acceleration_m"), &
                    Procedure_("simpleUnitFromStringWithUnitsS"))
        else
            unit = units(which_unit)
        end if
    end subroutine simpleUnitFromStringWithUnitsS

    pure subroutine selectUnit(string, units, errors, index)
        type(VARYING_STRING), intent(in) :: string
        class(AccelerationUnit_t), intent(in) :: units(:)
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
                Module_("Acceleration_m"), &
                Procedure_("selectUnit"), &
                '"' // string // '", known units: [' // join(unit_strings, ', ') // ']'))
    end subroutine selectUnit
end module Acceleration_m
