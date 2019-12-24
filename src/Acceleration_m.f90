module Acceleration_m
    use Conversion_factors_m, only: &
            CENTIMETERS_PER_SQUARE_SECOND_PER_METERS_PER_SQUARE_SECOND, &
            FEET_PER_SQUARE_SECOND_PER_METERS_PER_SQUARE_SECOND
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
            wrapInLatexQuantity, &
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
        procedure :: toGnuplotStringFullPrecision
        procedure :: toGnuplotStringWithPrecision
        generic, public :: toGnuplotString => &
                toGnuplotStringFullPrecision, toGnuplotStringWithPrecision
        procedure :: toGnuplotStringInFullPrecision
        procedure :: toGnuplotStringInWithPrecision
        generic, public :: toGnuplotStringIn => &
                toGnuplotStringInFullPrecision, toGnuplotStringInWithPrecision
        procedure :: toLatexStringFullPrecision
        procedure :: toLatexStringWithPrecision
        generic, public :: toLatexString => &
                toLatexStringFullPrecision, toLatexStringWithPrecision
        procedure :: toLatexStringInFullPrecision
        procedure :: toLatexStringInWithPrecision
        generic, public :: toLatexStringIn => &
                toLatexStringInFullPrecision, toLatexStringInWithPrecision
    end type Acceleration_t

    type, public :: AccelerationUnit_t
        double precision :: conversion_factor
        character(len=10) :: symbol
        character(len=50) :: gnuplot_symbol
        character(len=100) :: latex_symbol
    contains
        procedure :: toString => unitToString
        procedure :: toGnuplotString => unitToGnuplotString
        procedure :: toLatexString => unitToLatexString
    end type AccelerationUnit_t

    interface operator(.unit.)
        module procedure fromUnits
    end interface operator(.unit.)

    interface fromString
        module procedure fromStringBasicC
        module procedure fromStringBasicS
        module procedure fromStringWithUnitsC
        module procedure fromStringWithUnitsS
        module procedure unitFromStringBasicC
        module procedure unitFromStringBasicS
        module procedure unitFromStringWithUnitsC
        module procedure unitFromStringWithUnitsS
    end interface fromString

    interface sum
        module procedure sumAcceleration
    end interface sum

    type(AccelerationUnit_t), parameter, public :: CENTIMETERS_PER_SQUARE_SECOND = &
            AccelerationUnit_t( &
                    conversion_factor = CENTIMETERS_PER_SQUARE_SECOND_PER_METERS_PER_SQUARE_SECOND, &
                    symbol = "cm/s^2", &
                    gnuplot_symbol = "cm/s^2", &
                    latex_symbol = "\centi\meter\per\square\second")
    type(AccelerationUnit_t), parameter, public :: FEET_PER_SQUARE_SECOND = &
            AccelerationUnit_t( &
                    conversion_factor = FEET_PER_SQUARE_SECOND_PER_METERS_PER_SQUARE_SECOND, &
                    symbol = "ft/s^2", &
                    gnuplot_symbol = "ft/s^2", &
                    latex_symbol = "\foot\per\square\second")
    type(AccelerationUnit_t), parameter, public :: METERS_PER_SQUARE_SECOND = &
            AccelerationUnit_t( &
                    conversion_factor = 1.0d0, &
                    symbol = "m/s^2", &
                    gnuplot_symbol = "m/s^2", &
                    latex_symbol = "\meter\per\square\second")

    type(AccelerationUnit_t), public :: DEFAULT_OUTPUT_UNITS = METERS_PER_SQUARE_SECOND

    type(AccelerationUnit_t), parameter, public :: PROVIDED_UNITS(*) = &
            [CENTIMETERS_PER_SQUARE_SECOND, &
            FEET_PER_SQUARE_SECOND, &
            METERS_PER_SQUARE_SECOND]

    public :: operator(.unit.), fromString, sum
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
        type(AccelerationUnit_t), intent(in) :: units(:)
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
        type(AccelerationUnit_t), intent(in) :: units(:)
        type(ErrorList_t), intent(out) :: errors
        type(Acceleration_t), intent(out) :: acceleration

        double precision :: number
        character(len=100) :: number_chars
        type(VARYING_STRING) :: number_string
        integer :: status
        type(VARYING_STRING) :: symbol
        type(AccelerationUnit_t) :: unit
        type(ErrorList_t) :: unit_errors

        acceleration%meters_per_square_second = 0.0d0
        symbol = string
        call split(symbol, number_string, " ")
        if (len(symbol) == 0) then
            call errors%appendError(Fatal( &
                    PARSE_ERROR, &
                    Module_("Acceleration_m"), &
                    Procedure_("fromStringWithUnitsS"), &
                    'No unit symbol found in string "' // string // '"'))
            return
        end if
        number_chars = number_string
        read(number_chars, *, iostat=status) number
        if (status /= 0) then
            call errors%appendError(Fatal( &
                    PARSE_ERROR, &
                    Module_("Acceleration_m"), &
                    Procedure_("fromStringWithUnitsS"), &
                    'Error parsing number from string "' // number_string // '"'))
        end if
        call fromString(symbol, units, unit_errors, unit)
        acceleration = number.unit.unit
        call errors%appendErrors( &
                unit_errors, &
                Module_("Acceleration_m"), &
                Procedure_("fromStringWithUnitsS"))
    end subroutine fromStringWithUnitsS

    elemental function fromUnits(value_, units) result(acceleration)
        double precision, intent(in) :: value_
        type(AccelerationUnit_t), intent(in) :: units
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

        string = toString(self.in.units) // " " // units%toString()
    end function toStringInFullPrecision

    elemental function toStringInWithPrecision( &
            self, units, significant_digits) result(string)
        class(Acceleration_t), intent(in) :: self
        class(AccelerationUnit_t), intent(in) :: units
        integer, intent(in) :: significant_digits
        type(VARYING_STRING) :: string

        string = &
                toString(self.in.units, significant_digits) &
                // " " // units%toString()
    end function toStringInWithPrecision

    elemental function toGnuplotStringFullPrecision(self) result(string)
        class(Acceleration_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = self%toGnuplotStringIn(DEFAULT_OUTPUT_UNITS)
    end function toGnuplotStringFullPrecision

    elemental function toGnuplotStringWithPrecision( &
            self, significant_digits) result(string)
        class(Acceleration_t), intent(in) :: self
        integer, intent(in) :: significant_digits
        type(VARYING_STRING) :: string

        string = self%toGnuplotStringIn( &
                DEFAULT_OUTPUT_UNITS, significant_digits)
    end function toGnuplotStringWithPrecision

    elemental function toGnuplotStringInFullPrecision(self, units) result(string)
        class(Acceleration_t), intent(in) :: self
        class(AccelerationUnit_t), intent(in) :: units
        type(VARYING_STRING) :: string

        string = toString(self.in.units) // " " // units%toGnuplotString()
    end function toGnuplotStringInFullPrecision

    elemental function toGnuplotStringInWithPrecision( &
            self, units, significant_digits) result(string)
        class(Acceleration_t), intent(in) :: self
        class(AccelerationUnit_t), intent(in) :: units
        integer, intent(in) :: significant_digits
        type(VARYING_STRING) :: string

        string = &
                toString(self.in.units, significant_digits) &
                // " " // units%toGnuplotString()
    end function toGnuplotStringInWithPrecision

    elemental function toLatexStringFullPrecision(self) result(string)
        class(Acceleration_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = self%toLatexStringIn(DEFAULT_OUTPUT_UNITS)
    end function toLatexStringFullPrecision

    elemental function toLatexStringWithPrecision(self, significant_digits) result(string)
        class(Acceleration_t), intent(in) :: self
        integer, intent(in) :: significant_digits
        type(VARYING_STRING) :: string

        string = self%toLatexStringIn(DEFAULT_OUTPUT_UNITS, significant_digits)
    end function toLatexStringWithPrecision

    elemental function toLatexStringInFullPrecision(self, units) result(string)
        class(Acceleration_t), intent(in) :: self
        class(AccelerationUnit_t), intent(in) :: units
        type(VARYING_STRING) :: string

        string = wrapInLatexQuantity( &
                toString(self.in.units), units%toLatexString())
    end function toLatexStringInFullPrecision

    elemental function toLatexStringInWithPrecision( &
            self, units, significant_digits) result(string)
        class(Acceleration_t), intent(in) :: self
        class(AccelerationUnit_t), intent(in) :: units
        integer, intent(in) :: significant_digits
        type(VARYING_STRING) :: string

        string = wrapInLatexQuantity( &
                toString(self.in.units, significant_digits), &
                units%toLatexString())
    end function toLatexStringInWithPrecision

    pure subroutine unitFromStringBasicC(string, errors, unit)
        character(len=*), intent(in) :: string
        type(ErrorList_t), intent(out) :: errors
        type(AccelerationUnit_t), intent(out) :: unit

        type(ErrorList_t) :: errors_

        call fromString( &
                var_str(string), PROVIDED_UNITS, errors_, unit)
        call errors%appendErrors( &
                errors_, &
                Module_("Acceleration_m"), &
                Procedure_("unitFromStringBasicC"))
    end subroutine unitFromStringBasicC

    pure subroutine unitFromStringBasicS(string, errors, unit)
        type(VARYING_STRING), intent(in) :: string
        type(ErrorList_t), intent(out) :: errors
        type(AccelerationUnit_t), intent(out) :: unit

        type(ErrorList_t) :: errors_

        call fromString( &
                string, PROVIDED_UNITS, errors_, unit)
        call errors%appendErrors( &
                errors_, &
                Module_("Acceleration_m"), &
                Procedure_("unitFromStringBasicS"))
    end subroutine unitFromStringBasicS

    pure subroutine unitFromStringWithUnitsC(string, units, errors, unit)
        character(len=*), intent(in) :: string
        type(AccelerationUnit_t), intent(in) :: units(:)
        type(ErrorList_t), intent(out) :: errors
        type(AccelerationUnit_t), intent(out) :: unit

        type(ErrorList_t) :: errors_

        call fromString(var_str(string), units, errors_, unit)
        call errors%appendErrors( &
                errors_, &
                Module_("Acceleration_m"), &
                Procedure_("unitFromStringWithUnitsC"))
    end subroutine unitFromStringWithUnitsC

    pure subroutine unitFromStringWithUnitsS(string, units, errors, unit)
        type(VARYING_STRING), intent(in) :: string
        type(AccelerationUnit_t), intent(in) :: units(:)
        type(ErrorList_t), intent(out) :: errors
        type(AccelerationUnit_t), intent(out) :: unit

        integer :: i
        type(VARYING_STRING) :: unit_strings(size(units))

        do i = 1, size(units)
            if (string == units(i)%symbol) then
                unit = units(i)
                exit
            end if
        end do
        if (i > size(units)) then
            do i = 1, size(units)
                unit_strings(i) = units(i)%toString()
            end do
            call errors%appendError(Fatal( &
                    UNKNOWN_UNIT, &
                    Module_("Acceleration_m"), &
                    Procedure_("unitFromStringWithUnitsS"), &
                    '"' // string // '", known units: [' // join(unit_strings, ', ') // ']' ))
        end if
    end subroutine unitFromStringWithUnitsS

    elemental function unitToString(self) result(string)
        class(AccelerationUnit_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = trim(self%symbol)
    end function unitToString

    elemental function unitToGnuplotString(self) result(string)
        class(AccelerationUnit_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = trim(self%gnuplot_symbol)
    end function unitToGnuplotString

    elemental function unitToLatexString(self) result(string)
        class(AccelerationUnit_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = trim(self%latex_symbol)
    end function unitToLatexString
end module Acceleration_m
