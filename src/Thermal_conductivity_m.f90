module Thermal_conductivity_m
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
            CAL_PER_SEC_CM_K_PER_WATTS_PER_METER_KELVIN, &
            WATTS_PER_CENTIMETER_KELVIN_PER_WATTS_PER_METER_KELVIN
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
    end type ThermalConductivity_t

    type, abstract, public :: ThermalConductivityUnit_t
        double precision :: conversion_factor
    contains
        procedure(justUnitToString), deferred :: unitToString
        procedure(unitWithValueToString), deferred :: valueToString
        generic :: toString => unitToString, valueToString
        procedure(parseAsI), deferred :: parseAs
    end type ThermalConductivityUnit_t

    type, extends(ThermalConductivityUnit_t), public :: ThermalConductivitySimpleUnit_t
        character(len=20) :: symbol
    contains
        procedure :: unitToString => simpleUnitToString
        procedure :: valueToString => simpleValueToString
        procedure :: parseAs => simpleParseAs
    end type ThermalConductivitySimpleUnit_t

    type, extends(ThermalConductivityUnit_t), public :: ThermalConductivityGnuplotUnit_t
        character(len=50) :: symbol
    contains
        procedure :: unitToString => gnuplotUnitToString
        procedure :: valueToString => gnuplotValueToString
        procedure :: parseAs => gnuplotParseAs
    end type ThermalConductivityGnuplotUnit_t

    abstract interface
        elemental function justUnitToString(self) result(string)
            import ThermalConductivityUnit_t, VARYING_STRING
            class(ThermalConductivityUnit_t), intent(in) :: self
            type(VARYING_STRING) :: string
        end function justUnitToString

        pure function unitWithValueToString(self, value_) result(string)
            import ThermalConductivityUnit_t, VARYING_STRING
            class(ThermalConductivityUnit_t), intent(in) :: self
            type(VARYING_STRING), intent(in) :: value_
            type(VARYING_STRING) :: string
        end function unitWithValueToString

        pure subroutine parseAsI(self, string, errors, thermal_conductivity)
            import ErrorList_t, ThermalConductivity_t, ThermalConductivityUnit_t, VARYING_STRING
            class(ThermalConductivityUnit_t), intent(in) :: self
            type(VARYING_STRING), intent(in) :: string
            type(ErrorList_t), intent(out) :: errors
            type(ThermalConductivity_t), intent(out) :: thermal_conductivity
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
        module procedure sumThermalConductivity
    end interface sum

    type(ThermalConductivitySimpleUnit_t), parameter, public :: CALORIES_PER_SECOND_CENTIMETER_KELVIN = &
            ThermalConductivitySimpleUnit_t( &
                    conversion_factor = CAL_PER_SEC_CM_K_PER_WATTS_PER_METER_KELVIN, &
                    symbol = "cal/(s cm K)")
    type(ThermalConductivityGnuplotUnit_t), parameter, public :: CALORIES_PER_SECOND_CENTIMETER_KELVIN_GNUPLOT = &
            ThermalConductivityGnuplotUnit_t( &
                    conversion_factor = CAL_PER_SEC_CM_K_PER_WATTS_PER_METER_KELVIN, &
                    symbol = "cal/(s cm K)")
    type(ThermalConductivitySimpleUnit_t), parameter, public :: WATTS_PER_CENTIMETER_KELVIN = &
            ThermalConductivitySimpleUnit_t( &
                    conversion_factor = WATTS_PER_CENTIMETER_KELVIN_PER_WATTS_PER_METER_KELVIN, &
                    symbol = "W/(cm K)")
    type(ThermalConductivityGnuplotUnit_t), parameter, public :: WATTS_PER_CENTIMETER_KELVIN_GNUPLOT = &
            ThermalConductivityGnuplotUnit_t( &
                    conversion_factor = WATTS_PER_CENTIMETER_KELVIN_PER_WATTS_PER_METER_KELVIN, &
                    symbol = "W/(cm K)")
    type(ThermalConductivitySimpleUnit_t), parameter, public :: WATTS_PER_METER_KELVIN = &
            ThermalConductivitySimpleUnit_t( &
                    conversion_factor = 1.0d0, &
                    symbol = "W/(m K)")
    type(ThermalConductivityGnuplotUnit_t), parameter, public :: WATTS_PER_METER_KELVIN_GNUPLOT = &
            ThermalConductivityGnuplotUnit_t( &
                    conversion_factor = 1.0d0, &
                    symbol = "W/(m K)")

    type(ThermalConductivitySimpleUnit_t), public :: DEFAULT_OUTPUT_UNITS = WATTS_PER_METER_KELVIN

    type(ThermalConductivitySimpleUnit_t), parameter, public :: PROVIDED_UNITS(*) = &
            [CALORIES_PER_SECOND_CENTIMETER_KELVIN, &
            WATTS_PER_CENTIMETER_KELVIN, &
            WATTS_PER_METER_KELVIN]
    type(ThermalConductivityGnuplotUnit_t), parameter, public :: PROVIDED_GNUPLOT_UNITS(*) = &
            [CALORIES_PER_SECOND_CENTIMETER_KELVIN_GNUPLOT, &
            WATTS_PER_CENTIMETER_KELVIN_GNUPLOT, &
            WATTS_PER_METER_KELVIN_GNUPLOT]

    public :: operator(.unit.), fromString, selectUnit, sum
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
        class(ThermalConductivityUnit_t), intent(in) :: units(:)
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
        class(ThermalConductivityUnit_t), intent(in) :: units(:)
        type(ErrorList_t), intent(out) :: errors
        type(ThermalConductivity_t), intent(out) :: thermal_conductivity

        type(ErrorList_t) :: all_errors(size(units))
        integer :: i

        do i = 1, size(units)
            call units(i)%parseAs(string, all_errors(i), thermal_conductivity)
            if (.not. all_errors(i)%hasAny()) return
        end do
        do i = 1, size(units)
            call errors%appendErrors( &
                    all_errors(i), &
                    Module_("Thermal_conductivity_m"), &
                    Procedure_("fromStringWithUnitsS"))
        end do
    end subroutine fromStringWithUnitsS

    elemental function fromUnits(value_, units) result(thermal_conductivity)
        double precision, intent(in) :: value_
        class(ThermalConductivityUnit_t), intent(in) :: units
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

    pure function sumThermalConductivity(thermal_conductivitys)
        type(ThermalConductivity_t), intent(in) :: thermal_conductivitys(:)
        type(ThermalConductivity_t) :: sumThermalConductivity

        sumThermalConductivity%watts_per_meter_kelvin = sum(thermal_conductivitys%watts_per_meter_kelvin)
    end function sumThermalConductivity

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

        string = units%toString(toString(self.in.units))
    end function toStringInFullPrecision

    elemental function toStringInWithPrecision( &
            self, units, significant_digits) result(string)
        class(ThermalConductivity_t), intent(in) :: self
        class(ThermalConductivityUnit_t), intent(in) :: units
        integer, intent(in) :: significant_digits
        type(VARYING_STRING) :: string

        string = units%toString(toString(self.in.units, significant_digits))
    end function toStringInWithPrecision

    elemental function simpleUnitToString(self) result(string)
        class(ThermalConductivitySimpleUnit_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = trim(self%symbol)
    end function simpleUnitToString

    pure function simpleValueToString(self, value_) result(string)
        class(ThermalConductivitySimpleUnit_t), intent(in) :: self
        type(VARYING_STRING), intent(in) :: value_
        type(VARYING_STRING) :: string

        string = value_ // " " // self%toString()
    end function simpleValueToString

    pure subroutine simpleParseAs(self, string, errors, thermal_conductivity)
        class(ThermalConductivitySimpleUnit_t), intent(in) :: self
        type(VARYING_STRING), intent(in) :: string
        type(ErrorList_t), intent(out) :: errors
        type(ThermalConductivity_t), intent(out) :: thermal_conductivity

        type(ParseResult_t) :: parse_result

        parse_result = parseWith(theParser, string)
        if (parse_result%ok) then
            select type (the_number => parse_result%parsed)
            type is (ParsedRational_t)
                thermal_conductivity = the_number%value_.unit.self
            end select
        else
            call errors%appendError(Fatal( &
                    PARSE_ERROR, &
                    Module_("Thermal_conductivity_m"), &
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

    pure subroutine gnuplotParseAs(self, string, errors, thermal_conductivity)
        class(ThermalConductivityGnuplotUnit_t), intent(in) :: self
        type(VARYING_STRING), intent(in) :: string
        type(ErrorList_t), intent(out) :: errors
        type(ThermalConductivity_t), intent(out) :: thermal_conductivity

        type(ParseResult_t) :: parse_result

        parse_result = parseWith(theParser, string)
        if (parse_result%ok) then
            select type (the_number => parse_result%parsed)
            type is (ParsedRational_t)
                thermal_conductivity = the_number%value_.unit.self
            end select
        else
            call errors%appendError(Fatal( &
                    PARSE_ERROR, &
                    Module_("Thermal_conductivity_m"), &
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
        class(ThermalConductivityGnuplotUnit_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = trim(self%symbol)
    end function gnuplotUnitToString

    pure function gnuplotValueToString(self, value_) result(string)
        class(ThermalConductivityGnuplotUnit_t), intent(in) :: self
        type(VARYING_STRING), intent(in) :: value_
        type(VARYING_STRING) :: string

        string = value_ // " " // self%toString()
    end function gnuplotValueToString

    pure subroutine simpleUnitFromStringC(string, errors, unit)
        character(len=*), intent(in) :: string
        type(ErrorList_t), intent(out) :: errors
        type(ThermalConductivitySimpleUnit_t), intent(out) :: unit

        type(ErrorList_t) :: errors_

        call fromString(var_str(string), PROVIDED_UNITS, errors_, unit)
        call errors%appendErrors( &
                errors_, &
                Module_("Thermal_conductivity_m"), &
                Procedure_("simpleUnitFromStringC"))
    end subroutine simpleUnitFromStringC

    pure subroutine simpleUnitFromStringS(string, errors, unit)
        type(VARYING_STRING), intent(in) :: string
        type(ErrorList_t), intent(out) :: errors
        type(ThermalConductivitySimpleUnit_t), intent(out) :: unit

        type(ErrorList_t) :: errors_

        call fromString(string, PROVIDED_UNITS, errors_, unit)
        call errors%appendErrors( &
                errors_, &
                Module_("Thermal_conductivity_m"), &
                Procedure_("simpleUnitFromStringS"))
    end subroutine simpleUnitFromStringS

    pure subroutine simpleUnitFromStringWithUnitsC(string, units, errors, unit)
        character(len=*), intent(in) :: string
        type(ThermalConductivitySimpleUnit_t), intent(in) :: units(:)
        type(ErrorList_t), intent(out) :: errors
        type(ThermalConductivitySimpleUnit_t), intent(out) :: unit

        type(ErrorList_t) :: errors_

        call fromString(var_str(string), units, errors_, unit)
        call errors%appendErrors( &
                errors_, &
                Module_("Thermal_conductivity_m"), &
                Procedure_("simpleUnitFromStringWithUnitsC"))
    end subroutine simpleUnitFromStringWithUnitsC

    pure subroutine simpleUnitFromStringWithUnitsS(string, units, errors, unit)
        type(VARYING_STRING), intent(in) :: string
        type(ThermalConductivitySimpleUnit_t), intent(in) :: units(:)
        type(ErrorList_t), intent(out) :: errors
        type(ThermalConductivitySimpleUnit_t), intent(out) :: unit

        type(ErrorList_t) :: errors_
        integer :: which_unit

        call selectUnit(string, units, errors, which_unit)
        if (errors_%hasAny()) then
            call errors%appendErrors( &
                    errors_, &
                    Module_("Thermal_conductivity_m"), &
                    Procedure_("simpleUnitFromStringWithUnitsS"))
        else
            unit = units(which_unit)
        end if
    end subroutine simpleUnitFromStringWithUnitsS

    pure subroutine selectUnit(string, units, errors, index)
        type(VARYING_STRING), intent(in) :: string
        class(ThermalConductivityUnit_t), intent(in) :: units(:)
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
                Module_("Thermal_conductivity_m"), &
                Procedure_("selectUnit"), &
                '"' // string // '", known units: [' // join(unit_strings, ', ') // ']'))
    end subroutine selectUnit
end module Thermal_conductivity_m
