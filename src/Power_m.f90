module Power_m
    use Conversion_factors_m, only: &
            BTU_PER_HOUR_PER_WATT, &
            CALORIES_PER_SECOND_PER_WATT, &
            MEGABTU_PER_HOUR_PER_WATT, &
            MEGAWATTS_PER_WATT
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

    type, public :: Power_t
        double precision :: watts
    contains
        private
        procedure :: toUnits
        generic, public :: operator(.in.) => toUnits
        procedure, pass(power) :: doubleTimesPower
        procedure, pass(power) :: integerTimesPower
        procedure, pass(power) :: powerTimesDouble
        procedure, pass(power) :: powerTimesInteger
        generic, public :: operator(*) => &
                doubleTimesPower, &
                integerTimesPower, &
                powerTimesDouble, &
                powerTimesInteger
        procedure :: powerDividedByDouble
        procedure :: powerDividedByInteger
        procedure, pass(numerator) :: powerDividedByPower
        generic, public :: operator(/) => &
                powerDividedByDouble, &
                powerDividedByInteger, &
                powerDividedByPower
        procedure :: powerPlusPower
        generic, public :: operator(+) => powerPlusPower
        procedure :: powerMinusPower
        generic, public :: operator(-) => powerMinusPower
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
    end type Power_t

    type, public :: PowerUnit_t
        double precision :: conversion_factor
        character(len=10) :: symbol
        character(len=50) :: gnuplot_symbol
        character(len=100) :: latex_symbol
    contains
        procedure :: toString => unitToString
        procedure :: toGnuplotString => unitToGnuplotString
        procedure :: toLatexString => unitToLatexString
    end type PowerUnit_t

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
        module procedure sumPower
    end interface sum

    type(PowerUnit_t), parameter, public :: BTU_PER_HOUR = &
            PowerUnit_t( &
                    conversion_factor = BTU_PER_HOUR_PER_WATT, &
                    symbol = "BTU/hr", &
                    gnuplot_symbol = "BTU/hr", &
                    latex_symbol = "\btu\per\hour")
    type(PowerUnit_t), parameter, public :: CALORIES_PER_SECOND = &
            PowerUnit_t( &
                    conversion_factor = CALORIES_PER_SECOND_PER_WATT, &
                    symbol = "cal/s", &
                    gnuplot_symbol = "cal/s", &
                    latex_symbol = "\calorie\per\second")
    type(PowerUnit_t), parameter, public :: MEGABTU_PER_HOUR = &
            PowerUnit_t( &
                    conversion_factor = MEGABTU_PER_HOUR_PER_WATT, &
                    symbol = "MBTU/hr", &
                    gnuplot_symbol = "MBTU/hr", &
                    latex_symbol = "\mega\btu\per\hour")
    type(PowerUnit_t), parameter, public :: MEGAWATTS = &
            PowerUnit_t( &
                    conversion_factor = MEGAWATTS_PER_WATT, &
                    symbol = "MW", &
                    gnuplot_symbol = "MW", &
                    latex_symbol = "\mega\watt")
    type(PowerUnit_t), parameter, public :: WATTS = &
            PowerUnit_t( &
                    conversion_factor = 1.0d0, &
                    symbol = "W", &
                    gnuplot_symbol = "W", &
                    latex_symbol = "\watt")

    type(PowerUnit_t), public :: DEFAULT_OUTPUT_UNITS = WATTS

    type(PowerUnit_t), parameter, public :: PROVIDED_UNITS(*) = &
            [BTU_PER_HOUR, &
            CALORIES_PER_SECOND, &
            MEGABTU_PER_HOUR, &
            MEGAWATTS, &
            WATTS]

    public :: operator(.unit.), fromString, sum
contains
    pure subroutine fromStringBasicC(string, errors, power)
        character(len=*), intent(in) :: string
        type(ErrorList_t), intent(out) :: errors
        type(Power_t), intent(out) :: power

        type(ErrorList_t) :: errors_

        call fromString( &
                var_str(string), PROVIDED_UNITS, errors_, power)
        call errors%appendErrors( &
                errors_, &
                Module_("Power_m"), &
                Procedure_("fromStringBasicC"))
    end subroutine fromStringBasicC

    pure subroutine fromStringBasicS(string, errors, power)
        type(VARYING_STRING), intent(in) :: string
        type(ErrorList_t), intent(out) :: errors
        type(Power_t), intent(out) :: power

        type(ErrorList_t) :: errors_

        call fromString( &
                string, PROVIDED_UNITS, errors_, power)
        call errors%appendErrors( &
                errors_, &
                Module_("Power_m"), &
                Procedure_("fromStringBasicS"))
    end subroutine fromStringBasicS

    pure subroutine fromStringWithUnitsC(string, units, errors, power)
        character(len=*), intent(in) :: string
        type(PowerUnit_t), intent(in) :: units(:)
        type(ErrorList_t), intent(out) :: errors
        type(Power_t), intent(out) :: power

        type(ErrorList_t) :: errors_

        call fromString( &
                var_str(string), units, errors_, power)
        call errors%appendErrors( &
                errors_, &
                Module_("Power_m"), &
                Procedure_("fromStringWithUnitsC"))
    end subroutine fromStringWithUnitsC

    pure subroutine fromStringWithUnitsS(string, units, errors, power)
        type(VARYING_STRING), intent(in) :: string
        type(PowerUnit_t), intent(in) :: units(:)
        type(ErrorList_t), intent(out) :: errors
        type(Power_t), intent(out) :: power

        double precision :: number
        character(len=100) :: number_chars
        type(VARYING_STRING) :: number_string
        integer :: status
        type(VARYING_STRING) :: symbol
        type(PowerUnit_t) :: unit
        type(ErrorList_t) :: unit_errors

        power%watts = 0.0d0
        symbol = string
        call split(symbol, number_string, " ")
        if (len(symbol) == 0) then
            call errors%appendError(Fatal( &
                    PARSE_ERROR, &
                    Module_("Power_m"), &
                    Procedure_("fromStringWithUnitsS"), &
                    'No unit symbol found in string "' // string // '"'))
            return
        end if
        number_chars = number_string
        read(number_chars, *, iostat=status) number
        if (status /= 0) then
            call errors%appendError(Fatal( &
                    PARSE_ERROR, &
                    Module_("Power_m"), &
                    Procedure_("fromStringWithUnitsS"), &
                    'Error parsing number from string "' // number_string // '"'))
        end if
        call fromString(symbol, units, unit_errors, unit)
        power = number.unit.unit
        call errors%appendErrors( &
                unit_errors, &
                Module_("Power_m"), &
                Procedure_("fromStringWithUnitsS"))
    end subroutine fromStringWithUnitsS

    elemental function fromUnits(value_, units) result(power)
        double precision, intent(in) :: value_
        type(PowerUnit_t), intent(in) :: units
        type(Power_t) :: power

        power%watts = value_ / units%conversion_factor
    end function fromUnits

    elemental function toUnits(self, units) result(power)
        class(Power_t), intent(in) :: self
        class(PowerUnit_t), intent(in) :: units
        double precision :: power

        power = self%watts * units%conversion_factor
    end function toUnits

    elemental function doubleTimesPower( &
            multiplier, power) result(new_power)
        double precision, intent(in) :: multiplier
        class(Power_t), intent(in) :: power
        type(Power_t) :: new_power

        new_power%watts = &
                multiplier * power%watts
    end function doubleTimesPower

    elemental function integerTimesPower( &
            multiplier, power) result(new_power)
        integer, intent(in) :: multiplier
        class(Power_t), intent(in) :: power
        type(Power_t) :: new_power

        new_power%watts = &
                dble(multiplier) * power%watts
    end function integerTimesPower

    elemental function powerTimesDouble( &
            power, multiplier) result(new_power)
        class(Power_t), intent(in) :: power
        double precision, intent(in) :: multiplier
        type(Power_t) :: new_power

        new_power%watts = &
                power%watts * multiplier
    end function powerTimesDouble

    elemental function powerTimesInteger( &
            power, multiplier) result(new_power)
        class(Power_t), intent(in) :: power
        integer, intent(in) :: multiplier
        type(Power_t) :: new_power

        new_power%watts = &
                power%watts * dble(multiplier)
    end function powerTimesInteger

    elemental function powerDividedByDouble( &
            power, divisor) result(new_power)
        class(Power_t), intent(in) :: power
        double precision, intent(in) :: divisor
        type(Power_t) :: new_power

        new_power%watts = &
                power%watts / divisor
    end function powerDividedByDouble

    elemental function powerDividedByInteger( &
            power, divisor) result(new_power)
        class(Power_t), intent(in) :: power
        integer, intent(in) :: divisor
        type(Power_t) :: new_power

        new_power%watts = &
                power%watts / dble(divisor)
    end function powerDividedByInteger

    elemental function powerDividedByPower( &
            numerator, denomenator) result(ratio)
        class(Power_t), intent(in) :: numerator
        class(Power_t), intent(in) :: denomenator
        double precision :: ratio

        ratio = numerator%watts / denomenator%watts
    end function powerDividedByPower

    elemental function powerPlusPower( &
            power1, power2) result(new_power)
        class(Power_t), intent(in) :: power1
        class(Power_t), intent(in) :: power2
        type(Power_t) :: new_power

        new_power%watts = &
                power1%watts + power2%watts
    end function powerPlusPower

    elemental function powerMinusPower( &
            power1, power2) result(new_power)
        class(Power_t), intent(in) :: power1
        class(Power_t), intent(in) :: power2
        type(Power_t) :: new_power

        new_power%watts = &
                power1%watts - power2%watts
    end function powerMinusPower

    pure function sumPower(powers)
        type(Power_t), intent(in) :: powers(:)
        type(Power_t) :: sumPower

        sumPower%watts = sum(powers%watts)
    end function sumPower

    elemental function greaterThan(lhs, rhs)
        class(Power_t), intent(in) :: lhs
        class(Power_t), intent(in) :: rhs
        logical :: greaterThan

        greaterThan = lhs%watts > rhs%watts
    end function greaterThan

    elemental function lessThan(lhs,rhs)
        class(Power_t), intent(in) :: lhs
        class(Power_t), intent(in) :: rhs
        logical :: lessThan

        lessThan = lhs%watts < rhs%watts
    end function lessThan

    elemental function greaterThanOrEqual(lhs, rhs)
        class(Power_t), intent(in) :: lhs
        class(Power_t), intent(in) :: rhs
        logical :: greaterThanOrEqual

        greaterThanOrEqual = lhs%watts >= rhs%watts
    end function greaterThanOrEqual

    elemental function lessThanOrEqual(lhs, rhs)
        class(Power_t), intent(in) :: lhs
        class(Power_t), intent(in) :: rhs
        logical :: lessThanOrEqual

        lessThanOrEqual = lhs%watts <= rhs%watts
    end function lessThanOrEqual

    elemental function equal_(lhs,rhs)
        class(Power_t), intent(in) :: lhs
        class(Power_t), intent(in) :: rhs
        logical :: equal_

        equal_ = lhs%watts .safeEq. rhs%watts
    end function equal_

    elemental function equalWithinAbsolute(lhs, rhs, within)
        class(Power_t), intent(in) :: lhs
        class(Power_t), intent(in) :: rhs
        class(Power_t), intent(in) :: within
        logical :: equalWithinAbsolute

        equalWithinAbsolute = equalWithinAbsolute_( &
                lhs%watts, rhs%watts, within%watts)
    end function equalWithinAbsolute

    elemental function equalWithinRelative(lhs, rhs, within)
        class(Power_t), intent(in) :: lhs
        class(Power_t), intent(in) :: rhs
        double precision, intent(in) :: within
        logical :: equalWithinRelative

        equalWithinRelative = equalWithinRelative_( &
                lhs%watts, rhs%watts, within)
    end function equalWithinRelative

    elemental function notEqual(lhs, rhs)
        class(Power_t), intent(in) :: lhs
        class(Power_t), intent(in) :: rhs
        logical :: notEqual

        notEqual = .not. lhs == rhs
    end function notEqual

    elemental function toStringFullPrecision(self) result(string)
        class(Power_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = self%toStringIn(DEFAULT_OUTPUT_UNITS)
    end function toStringFullPrecision

    elemental function toStringWithPrecision(self, significant_digits) result(string)
        class(Power_t), intent(in) :: self
        integer, intent(in) :: significant_digits
        type(VARYING_STRING) :: string

        string = self%toStringIn(DEFAULT_OUTPUT_UNITS, significant_digits)
    end function toStringWithPrecision

    elemental function toStringInFullPrecision(self, units) result(string)
        class(Power_t), intent(in) :: self
        class(PowerUnit_t), intent(in) :: units
        type(VARYING_STRING) :: string

        string = toString(self.in.units) // " " // units%toString()
    end function toStringInFullPrecision

    elemental function toStringInWithPrecision( &
            self, units, significant_digits) result(string)
        class(Power_t), intent(in) :: self
        class(PowerUnit_t), intent(in) :: units
        integer, intent(in) :: significant_digits
        type(VARYING_STRING) :: string

        string = &
                toString(self.in.units, significant_digits) &
                // " " // units%toString()
    end function toStringInWithPrecision

    elemental function toGnuplotStringFullPrecision(self) result(string)
        class(Power_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = self%toGnuplotStringIn(DEFAULT_OUTPUT_UNITS)
    end function toGnuplotStringFullPrecision

    elemental function toGnuplotStringWithPrecision( &
            self, significant_digits) result(string)
        class(Power_t), intent(in) :: self
        integer, intent(in) :: significant_digits
        type(VARYING_STRING) :: string

        string = self%toGnuplotStringIn( &
                DEFAULT_OUTPUT_UNITS, significant_digits)
    end function toGnuplotStringWithPrecision

    elemental function toGnuplotStringInFullPrecision(self, units) result(string)
        class(Power_t), intent(in) :: self
        class(PowerUnit_t), intent(in) :: units
        type(VARYING_STRING) :: string

        string = toString(self.in.units) // " " // units%toGnuplotString()
    end function toGnuplotStringInFullPrecision

    elemental function toGnuplotStringInWithPrecision( &
            self, units, significant_digits) result(string)
        class(Power_t), intent(in) :: self
        class(PowerUnit_t), intent(in) :: units
        integer, intent(in) :: significant_digits
        type(VARYING_STRING) :: string

        string = &
                toString(self.in.units, significant_digits) &
                // " " // units%toGnuplotString()
    end function toGnuplotStringInWithPrecision

    elemental function toLatexStringFullPrecision(self) result(string)
        class(Power_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = self%toLatexStringIn(DEFAULT_OUTPUT_UNITS)
    end function toLatexStringFullPrecision

    elemental function toLatexStringWithPrecision(self, significant_digits) result(string)
        class(Power_t), intent(in) :: self
        integer, intent(in) :: significant_digits
        type(VARYING_STRING) :: string

        string = self%toLatexStringIn(DEFAULT_OUTPUT_UNITS, significant_digits)
    end function toLatexStringWithPrecision

    elemental function toLatexStringInFullPrecision(self, units) result(string)
        class(Power_t), intent(in) :: self
        class(PowerUnit_t), intent(in) :: units
        type(VARYING_STRING) :: string

        string = wrapInLatexQuantity( &
                toString(self.in.units), units%toLatexString())
    end function toLatexStringInFullPrecision

    elemental function toLatexStringInWithPrecision( &
            self, units, significant_digits) result(string)
        class(Power_t), intent(in) :: self
        class(PowerUnit_t), intent(in) :: units
        integer, intent(in) :: significant_digits
        type(VARYING_STRING) :: string

        string = wrapInLatexQuantity( &
                toString(self.in.units, significant_digits), &
                units%toLatexString())
    end function toLatexStringInWithPrecision

    pure subroutine unitFromStringBasicC(string, errors, unit)
        character(len=*), intent(in) :: string
        type(ErrorList_t), intent(out) :: errors
        type(PowerUnit_t), intent(out) :: unit

        type(ErrorList_t) :: errors_

        call fromString( &
                var_str(string), PROVIDED_UNITS, errors_, unit)
        call errors%appendErrors( &
                errors_, &
                Module_("Power_m"), &
                Procedure_("unitFromStringBasicC"))
    end subroutine unitFromStringBasicC

    pure subroutine unitFromStringBasicS(string, errors, unit)
        type(VARYING_STRING), intent(in) :: string
        type(ErrorList_t), intent(out) :: errors
        type(PowerUnit_t), intent(out) :: unit

        type(ErrorList_t) :: errors_

        call fromString( &
                string, PROVIDED_UNITS, errors_, unit)
        call errors%appendErrors( &
                errors_, &
                Module_("Power_m"), &
                Procedure_("unitFromStringBasicS"))
    end subroutine unitFromStringBasicS

    pure subroutine unitFromStringWithUnitsC(string, units, errors, unit)
        character(len=*), intent(in) :: string
        type(PowerUnit_t), intent(in) :: units(:)
        type(ErrorList_t), intent(out) :: errors
        type(PowerUnit_t), intent(out) :: unit

        type(ErrorList_t) :: errors_

        call fromString(var_str(string), units, errors_, unit)
        call errors%appendErrors( &
                errors_, &
                Module_("Power_m"), &
                Procedure_("unitFromStringWithUnitsC"))
    end subroutine unitFromStringWithUnitsC

    pure subroutine unitFromStringWithUnitsS(string, units, errors, unit)
        type(VARYING_STRING), intent(in) :: string
        type(PowerUnit_t), intent(in) :: units(:)
        type(ErrorList_t), intent(out) :: errors
        type(PowerUnit_t), intent(out) :: unit

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
                    Module_("Power_m"), &
                    Procedure_("unitFromStringWithUnitsS"), &
                    '"' // string // '", known units: [' // join(unit_strings, ', ') // ']' ))
        end if
    end subroutine unitFromStringWithUnitsS

    elemental function unitToString(self) result(string)
        class(PowerUnit_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = trim(self%symbol)
    end function unitToString

    elemental function unitToGnuplotString(self) result(string)
        class(PowerUnit_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = trim(self%gnuplot_symbol)
    end function unitToGnuplotString

    elemental function unitToLatexString(self) result(string)
        class(PowerUnit_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = trim(self%latex_symbol)
    end function unitToLatexString
end module Power_m
