module Speed_m
    use Conversion_factors_m, only: CENTIMETERS_PER_SECOND_PER_METERS_PER_SECOND
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
    end type Speed_t

    type, public :: SpeedUnit_t
        double precision :: conversion_factor
        character(len=10) :: symbol
        character(len=50) :: gnuplot_symbol
        character(len=100) :: latex_symbol
    contains
        procedure :: toString => unitToString
        procedure :: toGnuplotString => unitToGnuplotString
        procedure :: toLatexString => unitToLatexString
    end type SpeedUnit_t

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

    type(SpeedUnit_t), parameter, public :: CENTIMETERS_PER_SECOND = &
            SpeedUnit_t( &
                    conversion_factor = CENTIMETERS_PER_SECOND_PER_METERS_PER_SECOND, &
                    symbol = "cm/s", &
                    gnuplot_symbol = "cm/s", &
                    latex_symbol = "\centi\meter\per\second")
    type(SpeedUnit_t), parameter, public :: METERS_PER_SECOND = &
            SpeedUnit_t( &
                    conversion_factor = 1.0d0, &
                    symbol = "m/s", &
                    gnuplot_symbol = "m/s", &
                    latex_symbol = "\meter\per\second")

    type(SpeedUnit_t), public :: DEFAULT_OUTPUT_UNITS = METERS_PER_SECOND

    type(SpeedUnit_t), parameter, public :: PROVIDED_UNITS(*) = &
            [CENTIMETERS_PER_SECOND, METERS_PER_SECOND]

    public :: operator(.unit.), fromString
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
        type(SpeedUnit_t), intent(in) :: units(:)
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
        type(SpeedUnit_t), intent(in) :: units(:)
        type(ErrorList_t), intent(out) :: errors
        type(Speed_t), intent(out) :: speed

        double precision :: number
        character(len=100) :: number_chars
        type(VARYING_STRING) :: number_string
        integer :: status
        type(VARYING_STRING) :: symbol
        type(SpeedUnit_t) :: unit
        type(ErrorList_t) :: unit_errors

        speed%meters_per_second = 0.0d0
        symbol = string
        call split(symbol, number_string, " ")
        if (len(symbol) == 0) then
            call errors%appendError(Fatal( &
                    PARSE_ERROR, &
                    Module_("Speed_m"), &
                    Procedure_("fromStringWithUnitsS"), &
                    'No unit symbol found in string "' // string // '"'))
            return
        end if
        number_chars = number_string
        read(number_chars, *, iostat=status) number
        if (status /= 0) then
            call errors%appendError(Fatal( &
                    PARSE_ERROR, &
                    Module_("Speed_m"), &
                    Procedure_("fromStringWithUnitsS"), &
                    'Error parsing number from string "' // number_string // '"'))
        end if
        call fromString(symbol, units, unit_errors, unit)
        speed = number.unit.unit
        call errors%appendErrors( &
                unit_errors, &
                Module_("Speed_m"), &
                Procedure_("fromStringWithUnitsS"))
    end subroutine fromStringWithUnitsS

    elemental function fromUnits(value_, units) result(speed)
        double precision, intent(in) :: value_
        type(SpeedUnit_t), intent(in) :: units
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

        string = toString(self.in.units) // " " // units%toString()
    end function toStringInFullPrecision

    elemental function toStringInWithPrecision( &
            self, units, significant_digits) result(string)
        class(Speed_t), intent(in) :: self
        class(SpeedUnit_t), intent(in) :: units
        integer, intent(in) :: significant_digits
        type(VARYING_STRING) :: string

        string = &
                toString(self.in.units, significant_digits) &
                // " " // units%toString()
    end function toStringInWithPrecision

    elemental function toGnuplotStringFullPrecision(self) result(string)
        class(Speed_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = self%toGnuplotStringIn(DEFAULT_OUTPUT_UNITS)
    end function toGnuplotStringFullPrecision

    elemental function toGnuplotStringWithPrecision( &
            self, significant_digits) result(string)
        class(Speed_t), intent(in) :: self
        integer, intent(in) :: significant_digits
        type(VARYING_STRING) :: string

        string = self%toGnuplotStringIn( &
                DEFAULT_OUTPUT_UNITS, significant_digits)
    end function toGnuplotStringWithPrecision

    elemental function toGnuplotStringInFullPrecision(self, units) result(string)
        class(Speed_t), intent(in) :: self
        class(SpeedUnit_t), intent(in) :: units
        type(VARYING_STRING) :: string

        string = toString(self.in.units) // " " // units%toGnuplotString()
    end function toGnuplotStringInFullPrecision

    elemental function toGnuplotStringInWithPrecision( &
            self, units, significant_digits) result(string)
        class(Speed_t), intent(in) :: self
        class(SpeedUnit_t), intent(in) :: units
        integer, intent(in) :: significant_digits
        type(VARYING_STRING) :: string

        string = &
                toString(self.in.units, significant_digits) &
                // " " // units%toGnuplotString()
    end function toGnuplotStringInWithPrecision

    elemental function toLatexStringFullPrecision(self) result(string)
        class(Speed_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = self%toLatexStringIn(DEFAULT_OUTPUT_UNITS)
    end function toLatexStringFullPrecision

    elemental function toLatexStringWithPrecision(self, significant_digits) result(string)
        class(Speed_t), intent(in) :: self
        integer, intent(in) :: significant_digits
        type(VARYING_STRING) :: string

        string = self%toLatexStringIn(DEFAULT_OUTPUT_UNITS, significant_digits)
    end function toLatexStringWithPrecision

    elemental function toLatexStringInFullPrecision(self, units) result(string)
        class(Speed_t), intent(in) :: self
        class(SpeedUnit_t), intent(in) :: units
        type(VARYING_STRING) :: string

        string = wrapInLatexQuantity( &
                toString(self.in.units), units%toLatexString())
    end function toLatexStringInFullPrecision

    elemental function toLatexStringInWithPrecision( &
            self, units, significant_digits) result(string)
        class(Speed_t), intent(in) :: self
        class(SpeedUnit_t), intent(in) :: units
        integer, intent(in) :: significant_digits
        type(VARYING_STRING) :: string

        string = wrapInLatexQuantity( &
                toString(self.in.units, significant_digits), &
                units%toLatexString())
    end function toLatexStringInWithPrecision

    pure subroutine unitFromStringBasicC(string, errors, unit)
        character(len=*), intent(in) :: string
        type(ErrorList_t), intent(out) :: errors
        type(SpeedUnit_t), intent(out) :: unit

        type(ErrorList_t) :: errors_

        call fromString( &
                var_str(string), PROVIDED_UNITS, errors_, unit)
        call errors%appendErrors( &
                errors_, &
                Module_("Speed_m"), &
                Procedure_("unitFromStringBasicC"))
    end subroutine unitFromStringBasicC

    pure subroutine unitFromStringBasicS(string, errors, unit)
        type(VARYING_STRING), intent(in) :: string
        type(ErrorList_t), intent(out) :: errors
        type(SpeedUnit_t), intent(out) :: unit

        type(ErrorList_t) :: errors_

        call fromString( &
                string, PROVIDED_UNITS, errors_, unit)
        call errors%appendErrors( &
                errors_, &
                Module_("Speed_m"), &
                Procedure_("unitFromStringBasicS"))
    end subroutine unitFromStringBasicS

    pure subroutine unitFromStringWithUnitsC(string, units, errors, unit)
        character(len=*), intent(in) :: string
        type(SpeedUnit_t), intent(in) :: units(:)
        type(ErrorList_t), intent(out) :: errors
        type(SpeedUnit_t), intent(out) :: unit

        type(ErrorList_t) :: errors_

        call fromString(var_str(string), units, errors_, unit)
        call errors%appendErrors( &
                errors_, &
                Module_("Speed_m"), &
                Procedure_("unitFromStringWithUnitsC"))
    end subroutine unitFromStringWithUnitsC

    pure subroutine unitFromStringWithUnitsS(string, units, errors, unit)
        type(VARYING_STRING), intent(in) :: string
        type(SpeedUnit_t), intent(in) :: units(:)
        type(ErrorList_t), intent(out) :: errors
        type(SpeedUnit_t), intent(out) :: unit

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
                    Module_("Speed_m"), &
                    Procedure_("unitFromStringWithUnitsS"), &
                    '"' // string // '", known units: [' // join(unit_strings, ', ') // ']' ))
        end if
    end subroutine unitFromStringWithUnitsS

    elemental function unitToString(self) result(string)
        class(SpeedUnit_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = trim(self%symbol)
    end function unitToString

    elemental function unitToGnuplotString(self) result(string)
        class(SpeedUnit_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = trim(self%gnuplot_symbol)
    end function unitToGnuplotString

    elemental function unitToLatexString(self) result(string)
        class(SpeedUnit_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = trim(self%latex_symbol)
    end function unitToLatexString
end module Speed_m