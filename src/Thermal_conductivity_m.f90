module Thermal_conductivity_m
    use Conversion_factors_m, only: &
            CAL_PER_SEC_CM_K_PER_WATTS_PER_METER_KELVIN, &
            WATTS_PER_CENTIMETER_KELVIN_PER_WATTS_PER_METER_KELVIN
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

    type, public :: ThermalConductivity_t
        double precision :: watts_per_meter_kelvin
    contains
        private
        procedure :: toUnits
        generic, public :: operator(.in.) => toUnits
        procedure, pass(thermal_conductivity) :: doubleTimesThermalConductivity
        procedure, pass(thermal_conductivity) :: integerTimesThermalConductivity
        procedure, pass(thermal_conductivity) :: thermalConductivityTimesDouble
        procedure, pass(thermal_conductivity) :: thermalConductivityTimesInteger
        generic, public :: operator(*) => &
                doubleTimesThermalConductivity, &
                integerTimesThermalConductivity, &
                thermalConductivityTimesDouble, &
                thermalConductivityTimesInteger
        procedure :: thermalConductivityDividedByDouble
        procedure :: thermalConductivityDividedByInteger
        procedure, pass(numerator) :: thermalConductivityDividedByThermalConductivity
        generic, public :: operator(/) => &
                thermalConductivityDividedByDouble, &
                thermalConductivityDividedByInteger, &
                thermalConductivityDividedByThermalConductivity
        procedure :: thermalConductivityPlusThermalConductivity
        generic, public :: operator(+) => thermalConductivityPlusThermalConductivity
        procedure :: thermalConductivityMinusThermalConductivity
        generic, public :: operator(-) => thermalConductivityMinusThermalConductivity
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
    end type ThermalConductivity_t

    type, public :: ThermalConductivityUnit_t
        double precision :: conversion_factor
        character(len=20) :: symbol
        character(len=50) :: gnuplot_symbol
        character(len=100) :: latex_symbol
    contains
        procedure :: toString => unitToString
        procedure :: toGnuplotString => unitToGnuplotString
        procedure :: toLatexString => unitToLatexString
    end type ThermalConductivityUnit_t

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

    type(ThermalConductivityUnit_t), parameter, public :: CALORIES_PER_SECOND_CENTIMETER_KELVIN = &
            ThermalConductivityUnit_t( &
                    conversion_factor = CAL_PER_SEC_CM_K_PER_WATTS_PER_METER_KELVIN, &
                    symbol = "cal/(s cm K)", &
                    gnuplot_symbol = "cal/(s cm K)", &
                    latex_symbol = "\calorie\per\second\per\centi\meter\per\kelvin")
    type(ThermalConductivityUnit_t), parameter, public :: WATTS_PER_CENTIMETER_KELVIN = &
            ThermalConductivityUnit_t( &
                    conversion_factor = WATTS_PER_CENTIMETER_KELVIN_PER_WATTS_PER_METER_KELVIN, &
                    symbol = "W/(cm K)", &
                    gnuplot_symbol = "W/(cm K)", &
                    latex_symbol = "\watt\per\centi\meter\per\kelvin")
    type(ThermalConductivityUnit_t), parameter, public :: WATTS_PER_METER_KELVIN = &
            ThermalConductivityUnit_t( &
                    conversion_factor = 1.0d0, &
                    symbol = "W/(m K)", &
                    gnuplot_symbol = "W/(m K)", &
                    latex_symbol = "\watt\per\meter\per\kelvin")

    type(ThermalConductivityUnit_t), public :: DEFAULT_OUTPUT_UNITS = WATTS_PER_METER_KELVIN

    type(ThermalConductivityUnit_t), parameter, public :: PROVIDED_UNITS(*) = &
            [CALORIES_PER_SECOND_CENTIMETER_KELVIN, &
            WATTS_PER_CENTIMETER_KELVIN, &
            WATTS_PER_METER_KELVIN]

    public :: operator(.unit.), fromString
contains
    pure subroutine fromStringBasicC(string, errors, thermal_conductivity)
        character(len=*), intent(in) :: string
        type(ErrorList_t), intent(out) :: errors
        type(ThermalConductivity_t), intent(out) :: thermal_conductivity

        type(ErrorList_t) :: errors_

        call fromString( &
                var_str(string), PROVIDED_UNITS, errors_, thermal_conductivity)
        call errors%appendErrors( &
                errors_, &
                Module_("Thermal_conductivity_m"), &
                Procedure_("fromStringBasicC"))
    end subroutine fromStringBasicC

    pure subroutine fromStringBasicS(string, errors, thermal_conductivity)
        type(VARYING_STRING), intent(in) :: string
        type(ErrorList_t), intent(out) :: errors
        type(ThermalConductivity_t), intent(out) :: thermal_conductivity

        type(ErrorList_t) :: errors_

        call fromString( &
                string, PROVIDED_UNITS, errors_, thermal_conductivity)
        call errors%appendErrors( &
                errors_, &
                Module_("Thermal_conductivity_m"), &
                Procedure_("fromStringBasicS"))
    end subroutine fromStringBasicS

    pure subroutine fromStringWithUnitsC(string, units, errors, thermal_conductivity)
        character(len=*), intent(in) :: string
        type(ThermalConductivityUnit_t), intent(in) :: units(:)
        type(ErrorList_t), intent(out) :: errors
        type(ThermalConductivity_t), intent(out) :: thermal_conductivity

        type(ErrorList_t) :: errors_

        call fromString( &
                var_str(string), units, errors_, thermal_conductivity)
        call errors%appendErrors( &
                errors_, &
                Module_("Thermal_conductivity_m"), &
                Procedure_("fromStringWithUnitsC"))
    end subroutine fromStringWithUnitsC

    pure subroutine fromStringWithUnitsS(string, units, errors, thermal_conductivity)
        type(VARYING_STRING), intent(in) :: string
        type(ThermalConductivityUnit_t), intent(in) :: units(:)
        type(ErrorList_t), intent(out) :: errors
        type(ThermalConductivity_t), intent(out) :: thermal_conductivity

        double precision :: number
        character(len=100) :: number_chars
        type(VARYING_STRING) :: number_string
        integer :: status
        type(VARYING_STRING) :: symbol
        type(ThermalConductivityUnit_t) :: unit
        type(ErrorList_t) :: unit_errors

        thermal_conductivity%watts_per_meter_kelvin = 0.0d0
        symbol = string
        call split(symbol, number_string, " ")
        if (len(symbol) == 0) then
            call errors%appendError(Fatal( &
                    PARSE_ERROR, &
                    Module_("Thermal_conductivity_m"), &
                    Procedure_("fromStringWithUnitsS"), &
                    'No unit symbol found in string "' // string // '"'))
            return
        end if
        number_chars = number_string
        read(number_chars, *, iostat=status) number
        if (status /= 0) then
            call errors%appendError(Fatal( &
                    PARSE_ERROR, &
                    Module_("Thermal_conductivity_m"), &
                    Procedure_("fromStringWithUnitsS"), &
                    'Error parsing number from string "' // number_string // '"'))
        end if
        call fromString(symbol, units, unit_errors, unit)
        thermal_conductivity = number.unit.unit
        call errors%appendErrors( &
                unit_errors, &
                Module_("Thermal_conductivity_m"), &
                Procedure_("fromStringWithUnitsS"))
    end subroutine fromStringWithUnitsS

    elemental function fromUnits(value_, units) result(thermal_conductivity)
        double precision, intent(in) :: value_
        type(ThermalConductivityUnit_t), intent(in) :: units
        type(ThermalConductivity_t) :: thermal_conductivity

        thermal_conductivity%watts_per_meter_kelvin = value_ / units%conversion_factor
    end function fromUnits

    elemental function toUnits(self, units) result(thermal_conductivity)
        class(ThermalConductivity_t), intent(in) :: self
        class(ThermalConductivityUnit_t), intent(in) :: units
        double precision :: thermal_conductivity

        thermal_conductivity = self%watts_per_meter_kelvin * units%conversion_factor
    end function toUnits

    elemental function doubleTimesThermalConductivity( &
            multiplier, thermal_conductivity) result(new_thermal_conductivity)
        double precision, intent(in) :: multiplier
        class(ThermalConductivity_t), intent(in) :: thermal_conductivity
        type(ThermalConductivity_t) :: new_thermal_conductivity

        new_thermal_conductivity%watts_per_meter_kelvin = &
                multiplier * thermal_conductivity%watts_per_meter_kelvin
    end function doubleTimesThermalConductivity

    elemental function integerTimesThermalConductivity( &
            multiplier, thermal_conductivity) result(new_thermal_conductivity)
        integer, intent(in) :: multiplier
        class(ThermalConductivity_t), intent(in) :: thermal_conductivity
        type(ThermalConductivity_t) :: new_thermal_conductivity

        new_thermal_conductivity%watts_per_meter_kelvin = &
                dble(multiplier) * thermal_conductivity%watts_per_meter_kelvin
    end function integerTimesThermalConductivity

    elemental function thermalConductivityTimesDouble( &
            thermal_conductivity, multiplier) result(new_thermal_conductivity)
        class(ThermalConductivity_t), intent(in) :: thermal_conductivity
        double precision, intent(in) :: multiplier
        type(ThermalConductivity_t) :: new_thermal_conductivity

        new_thermal_conductivity%watts_per_meter_kelvin = &
                thermal_conductivity%watts_per_meter_kelvin * multiplier
    end function thermalConductivityTimesDouble

    elemental function thermalConductivityTimesInteger( &
            thermal_conductivity, multiplier) result(new_thermal_conductivity)
        class(ThermalConductivity_t), intent(in) :: thermal_conductivity
        integer, intent(in) :: multiplier
        type(ThermalConductivity_t) :: new_thermal_conductivity

        new_thermal_conductivity%watts_per_meter_kelvin = &
                thermal_conductivity%watts_per_meter_kelvin * dble(multiplier)
    end function thermalConductivityTimesInteger

    elemental function thermalConductivityDividedByDouble( &
            thermal_conductivity, divisor) result(new_thermal_conductivity)
        class(ThermalConductivity_t), intent(in) :: thermal_conductivity
        double precision, intent(in) :: divisor
        type(ThermalConductivity_t) :: new_thermal_conductivity

        new_thermal_conductivity%watts_per_meter_kelvin = &
                thermal_conductivity%watts_per_meter_kelvin / divisor
    end function thermalConductivityDividedByDouble

    elemental function thermalConductivityDividedByInteger( &
            thermal_conductivity, divisor) result(new_thermal_conductivity)
        class(ThermalConductivity_t), intent(in) :: thermal_conductivity
        integer, intent(in) :: divisor
        type(ThermalConductivity_t) :: new_thermal_conductivity

        new_thermal_conductivity%watts_per_meter_kelvin = &
                thermal_conductivity%watts_per_meter_kelvin / dble(divisor)
    end function thermalConductivityDividedByInteger

    elemental function thermalConductivityDividedByThermalConductivity( &
            numerator, denomenator) result(ratio)
        class(ThermalConductivity_t), intent(in) :: numerator
        class(ThermalConductivity_t), intent(in) :: denomenator
        double precision :: ratio

        ratio = numerator%watts_per_meter_kelvin / denomenator%watts_per_meter_kelvin
    end function thermalConductivityDividedByThermalConductivity

    elemental function thermalConductivityPlusThermalConductivity( &
            thermal_conductivity1, thermal_conductivity2) result(new_thermal_conductivity)
        class(ThermalConductivity_t), intent(in) :: thermal_conductivity1
        class(ThermalConductivity_t), intent(in) :: thermal_conductivity2
        type(ThermalConductivity_t) :: new_thermal_conductivity

        new_thermal_conductivity%watts_per_meter_kelvin = &
                thermal_conductivity1%watts_per_meter_kelvin + thermal_conductivity2%watts_per_meter_kelvin
    end function thermalConductivityPlusThermalConductivity

    elemental function thermalConductivityMinusThermalConductivity( &
            thermal_conductivity1, thermal_conductivity2) result(new_thermal_conductivity)
        class(ThermalConductivity_t), intent(in) :: thermal_conductivity1
        class(ThermalConductivity_t), intent(in) :: thermal_conductivity2
        type(ThermalConductivity_t) :: new_thermal_conductivity

        new_thermal_conductivity%watts_per_meter_kelvin = &
                thermal_conductivity1%watts_per_meter_kelvin - thermal_conductivity2%watts_per_meter_kelvin
    end function thermalConductivityMinusThermalConductivity

    elemental function greaterThan(lhs, rhs)
        class(ThermalConductivity_t), intent(in) :: lhs
        class(ThermalConductivity_t), intent(in) :: rhs
        logical :: greaterThan

        greaterThan = lhs%watts_per_meter_kelvin > rhs%watts_per_meter_kelvin
    end function greaterThan

    elemental function lessThan(lhs,rhs)
        class(ThermalConductivity_t), intent(in) :: lhs
        class(ThermalConductivity_t), intent(in) :: rhs
        logical :: lessThan

        lessThan = lhs%watts_per_meter_kelvin < rhs%watts_per_meter_kelvin
    end function lessThan

    elemental function greaterThanOrEqual(lhs, rhs)
        class(ThermalConductivity_t), intent(in) :: lhs
        class(ThermalConductivity_t), intent(in) :: rhs
        logical :: greaterThanOrEqual

        greaterThanOrEqual = lhs%watts_per_meter_kelvin >= rhs%watts_per_meter_kelvin
    end function greaterThanOrEqual

    elemental function lessThanOrEqual(lhs, rhs)
        class(ThermalConductivity_t), intent(in) :: lhs
        class(ThermalConductivity_t), intent(in) :: rhs
        logical :: lessThanOrEqual

        lessThanOrEqual = lhs%watts_per_meter_kelvin <= rhs%watts_per_meter_kelvin
    end function lessThanOrEqual

    elemental function equal_(lhs,rhs)
        class(ThermalConductivity_t), intent(in) :: lhs
        class(ThermalConductivity_t), intent(in) :: rhs
        logical :: equal_

        equal_ = lhs%watts_per_meter_kelvin .safeEq. rhs%watts_per_meter_kelvin
    end function equal_

    elemental function equalWithinAbsolute(lhs, rhs, within)
        class(ThermalConductivity_t), intent(in) :: lhs
        class(ThermalConductivity_t), intent(in) :: rhs
        class(ThermalConductivity_t), intent(in) :: within
        logical :: equalWithinAbsolute

        equalWithinAbsolute = equalWithinAbsolute_( &
                lhs%watts_per_meter_kelvin, rhs%watts_per_meter_kelvin, within%watts_per_meter_kelvin)
    end function equalWithinAbsolute

    elemental function equalWithinRelative(lhs, rhs, within)
        class(ThermalConductivity_t), intent(in) :: lhs
        class(ThermalConductivity_t), intent(in) :: rhs
        double precision, intent(in) :: within
        logical :: equalWithinRelative

        equalWithinRelative = equalWithinRelative_( &
                lhs%watts_per_meter_kelvin, rhs%watts_per_meter_kelvin, within)
    end function equalWithinRelative

    elemental function notEqual(lhs, rhs)
        class(ThermalConductivity_t), intent(in) :: lhs
        class(ThermalConductivity_t), intent(in) :: rhs
        logical :: notEqual

        notEqual = .not. lhs == rhs
    end function notEqual

    elemental function toStringFullPrecision(self) result(string)
        class(ThermalConductivity_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = self%toStringIn(DEFAULT_OUTPUT_UNITS)
    end function toStringFullPrecision

    elemental function toStringWithPrecision(self, significant_digits) result(string)
        class(ThermalConductivity_t), intent(in) :: self
        integer, intent(in) :: significant_digits
        type(VARYING_STRING) :: string

        string = self%toStringIn(DEFAULT_OUTPUT_UNITS, significant_digits)
    end function toStringWithPrecision

    elemental function toStringInFullPrecision(self, units) result(string)
        class(ThermalConductivity_t), intent(in) :: self
        class(ThermalConductivityUnit_t), intent(in) :: units
        type(VARYING_STRING) :: string

        string = toString(self.in.units) // " " // units%toString()
    end function toStringInFullPrecision

    elemental function toStringInWithPrecision( &
            self, units, significant_digits) result(string)
        class(ThermalConductivity_t), intent(in) :: self
        class(ThermalConductivityUnit_t), intent(in) :: units
        integer, intent(in) :: significant_digits
        type(VARYING_STRING) :: string

        string = &
                toString(self.in.units, significant_digits) &
                // " " // units%toString()
    end function toStringInWithPrecision

    elemental function toGnuplotStringFullPrecision(self) result(string)
        class(ThermalConductivity_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = self%toGnuplotStringIn(DEFAULT_OUTPUT_UNITS)
    end function toGnuplotStringFullPrecision

    elemental function toGnuplotStringWithPrecision( &
            self, significant_digits) result(string)
        class(ThermalConductivity_t), intent(in) :: self
        integer, intent(in) :: significant_digits
        type(VARYING_STRING) :: string

        string = self%toGnuplotStringIn( &
                DEFAULT_OUTPUT_UNITS, significant_digits)
    end function toGnuplotStringWithPrecision

    elemental function toGnuplotStringInFullPrecision(self, units) result(string)
        class(ThermalConductivity_t), intent(in) :: self
        class(ThermalConductivityUnit_t), intent(in) :: units
        type(VARYING_STRING) :: string

        string = toString(self.in.units) // " " // units%toGnuplotString()
    end function toGnuplotStringInFullPrecision

    elemental function toGnuplotStringInWithPrecision( &
            self, units, significant_digits) result(string)
        class(ThermalConductivity_t), intent(in) :: self
        class(ThermalConductivityUnit_t), intent(in) :: units
        integer, intent(in) :: significant_digits
        type(VARYING_STRING) :: string

        string = &
                toString(self.in.units, significant_digits) &
                // " " // units%toGnuplotString()
    end function toGnuplotStringInWithPrecision

    elemental function toLatexStringFullPrecision(self) result(string)
        class(ThermalConductivity_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = self%toLatexStringIn(DEFAULT_OUTPUT_UNITS)
    end function toLatexStringFullPrecision

    elemental function toLatexStringWithPrecision(self, significant_digits) result(string)
        class(ThermalConductivity_t), intent(in) :: self
        integer, intent(in) :: significant_digits
        type(VARYING_STRING) :: string

        string = self%toLatexStringIn(DEFAULT_OUTPUT_UNITS, significant_digits)
    end function toLatexStringWithPrecision

    elemental function toLatexStringInFullPrecision(self, units) result(string)
        class(ThermalConductivity_t), intent(in) :: self
        class(ThermalConductivityUnit_t), intent(in) :: units
        type(VARYING_STRING) :: string

        string = wrapInLatexQuantity( &
                toString(self.in.units), units%toLatexString())
    end function toLatexStringInFullPrecision

    elemental function toLatexStringInWithPrecision( &
            self, units, significant_digits) result(string)
        class(ThermalConductivity_t), intent(in) :: self
        class(ThermalConductivityUnit_t), intent(in) :: units
        integer, intent(in) :: significant_digits
        type(VARYING_STRING) :: string

        string = wrapInLatexQuantity( &
                toString(self.in.units, significant_digits), &
                units%toLatexString())
    end function toLatexStringInWithPrecision

    pure subroutine unitFromStringBasicC(string, errors, unit)
        character(len=*), intent(in) :: string
        type(ErrorList_t), intent(out) :: errors
        type(ThermalConductivityUnit_t), intent(out) :: unit

        type(ErrorList_t) :: errors_

        call fromString( &
                var_str(string), PROVIDED_UNITS, errors_, unit)
        call errors%appendErrors( &
                errors_, &
                Module_("Thermal_conductivity_m"), &
                Procedure_("unitFromStringBasicC"))
    end subroutine unitFromStringBasicC

    pure subroutine unitFromStringBasicS(string, errors, unit)
        type(VARYING_STRING), intent(in) :: string
        type(ErrorList_t), intent(out) :: errors
        type(ThermalConductivityUnit_t), intent(out) :: unit

        type(ErrorList_t) :: errors_

        call fromString( &
                string, PROVIDED_UNITS, errors_, unit)
        call errors%appendErrors( &
                errors_, &
                Module_("Thermal_conductivity_m"), &
                Procedure_("unitFromStringBasicS"))
    end subroutine unitFromStringBasicS

    pure subroutine unitFromStringWithUnitsC(string, units, errors, unit)
        character(len=*), intent(in) :: string
        type(ThermalConductivityUnit_t), intent(in) :: units(:)
        type(ErrorList_t), intent(out) :: errors
        type(ThermalConductivityUnit_t), intent(out) :: unit

        type(ErrorList_t) :: errors_

        call fromString(var_str(string), units, errors_, unit)
        call errors%appendErrors( &
                errors_, &
                Module_("Thermal_conductivity_m"), &
                Procedure_("unitFromStringWithUnitsC"))
    end subroutine unitFromStringWithUnitsC

    pure subroutine unitFromStringWithUnitsS(string, units, errors, unit)
        type(VARYING_STRING), intent(in) :: string
        type(ThermalConductivityUnit_t), intent(in) :: units(:)
        type(ErrorList_t), intent(out) :: errors
        type(ThermalConductivityUnit_t), intent(out) :: unit

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
                    Module_("Thermal_conductivity_m"), &
                    Procedure_("unitFromStringWithUnitsS"), &
                    '"' // string // '", known units: [' // join(unit_strings, ', ') // ']' ))
        end if
    end subroutine unitFromStringWithUnitsS

    elemental function unitToString(self) result(string)
        class(ThermalConductivityUnit_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = trim(self%symbol)
    end function unitToString

    elemental function unitToGnuplotString(self) result(string)
        class(ThermalConductivityUnit_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = trim(self%gnuplot_symbol)
    end function unitToGnuplotString

    elemental function unitToLatexString(self) result(string)
        class(ThermalConductivityUnit_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = trim(self%latex_symbol)
    end function unitToLatexString
end module Thermal_conductivity_m
