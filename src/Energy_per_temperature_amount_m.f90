module Energy_per_temperature_amount_m
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
            KILOJOULES_PER_KELVIN_MOL_PER_JOULES_PER_KELVIN_MOL
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

    type, public :: EnergyPerTemperatureAmount_t
        double precision :: joules_per_kelvin_mol
    contains
        private
        procedure :: toUnits
        generic, public :: operator(.in.) => toUnits
        procedure, pass(energy_per_temperature_amount) :: doubleTimesEnergyPerTemperatureAmount
        procedure, pass(energy_per_temperature_amount) :: integerTimesEnergyPerTemperatureAmount
        procedure, pass(energy_per_temperature_amount) :: energyPerTemperatureAmountTimesDouble
        procedure, pass(energy_per_temperature_amount) :: energyPerTemperatureAmountTimesInteger
        generic, public :: operator(*) => &
                doubleTimesEnergyPerTemperatureAmount, &
                integerTimesEnergyPerTemperatureAmount, &
                energyPerTemperatureAmountTimesDouble, &
                energyPerTemperatureAmountTimesInteger
        procedure :: energyPerTemperatureAmountDividedByDouble
        procedure :: energyPerTemperatureAmountDividedByInteger
        procedure, pass(numerator) :: energyPerTemperatureAmountDividedByEnergyPerTemperatureAmount
        generic, public :: operator(/) => &
                energyPerTemperatureAmountDividedByDouble, &
                energyPerTemperatureAmountDividedByInteger, &
                energyPerTemperatureAmountDividedByEnergyPerTemperatureAmount
        procedure :: energyPerTemperatureAmountPlusEnergyPerTemperatureAmount
        generic, public :: operator(+) => energyPerTemperatureAmountPlusEnergyPerTemperatureAmount
        procedure :: energyPerTemperatureAmountMinusEnergyPerTemperatureAmount
        generic, public :: operator(-) => energyPerTemperatureAmountMinusEnergyPerTemperatureAmount
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
    end type EnergyPerTemperatureAmount_t

    type, abstract, public :: EnergyPerTemperatureAmountUnit_t
        double precision :: conversion_factor
    contains
        procedure(justUnitToString), deferred :: unitToString
        procedure(unitWithValueToString), deferred :: valueToString
        generic :: toString => unitToString, valueToString
        procedure(parseAsI), deferred :: parseAs
    end type EnergyPerTemperatureAmountUnit_t

    type, extends(EnergyPerTemperatureAmountUnit_t), public :: EnergyPerTemperatureAmountSimpleUnit_t
        character(len=20) :: symbol
    contains
        procedure :: unitToString => simpleUnitToString
        procedure :: valueToString => simpleValueToString
        procedure :: parseAs => simpleParseAs
    end type EnergyPerTemperatureAmountSimpleUnit_t

    abstract interface
        elemental function justUnitToString(self) result(string)
            import EnergyPerTemperatureAmountUnit_t, VARYING_STRING
            class(EnergyPerTemperatureAmountUnit_t), intent(in) :: self
            type(VARYING_STRING) :: string
        end function justUnitToString

        pure function unitWithValueToString(self, value_) result(string)
            import EnergyPerTemperatureAmountUnit_t, VARYING_STRING
            class(EnergyPerTemperatureAmountUnit_t), intent(in) :: self
            type(VARYING_STRING), intent(in) :: value_
            type(VARYING_STRING) :: string
        end function unitWithValueToString

        pure subroutine parseAsI(self, string, errors, energy_per_temperature_amount)
            import ErrorList_t, EnergyPerTemperatureAmount_t, EnergyPerTemperatureAmountUnit_t, VARYING_STRING
            class(EnergyPerTemperatureAmountUnit_t), intent(in) :: self
            type(VARYING_STRING), intent(in) :: string
            type(ErrorList_t), intent(out) :: errors
            type(EnergyPerTemperatureAmount_t), intent(out) :: energy_per_temperature_amount
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
        module procedure sumEnergyPerTemperatureAmount
    end interface sum

    type(EnergyPerTemperatureAmountSimpleUnit_t), parameter, public :: JOULES_PER_KELVIN_MOL = &
            EnergyPerTemperatureAmountSimpleUnit_t( &
                    conversion_factor = 1.0d0, &
                    symbol = "J/(K mol)")
    type(EnergyPerTemperatureAmountSimpleUnit_t), parameter, public :: KILOJOULES_PER_KELVIN_MOL = &
            EnergyPerTemperatureAmountSimpleUnit_t( &
                    conversion_factor = KILOJOULES_PER_KELVIN_MOL_PER_JOULES_PER_KELVIN_MOL, &
                    symbol = "kJ/(K mol)")

    type(EnergyPerTemperatureAmountSimpleUnit_t), public :: DEFAULT_OUTPUT_UNITS = JOULES_PER_KELVIN_MOL

    type(EnergyPerTemperatureAmountSimpleUnit_t), parameter, public :: PROVIDED_UNITS(*) = &
            [JOULES_PER_KELVIN_MOL, KILOJOULES_PER_KELVIN_MOL]

    public :: operator(.unit.), fromString, selectUnit, sum
contains
    pure subroutine fromStringBasicC(string, errors, energy_per_temperature_amount)
        character(len=*), intent(in) :: string
        type(ErrorList_t), intent(out) :: errors
        type(EnergyPerTemperatureAmount_t), intent(out) :: energy_per_temperature_amount

        type(ErrorList_t) :: errors_

        call fromString( &
                var_str(string), PROVIDED_UNITS, errors_, energy_per_temperature_amount)
        call errors%appendErrors( &
                errors_, &
                Module_("Energy_per_temperature_amount_m"), &
                Procedure_("fromStringBasicC"))
    end subroutine fromStringBasicC

    pure subroutine fromStringBasicS(string, errors, energy_per_temperature_amount)
        type(VARYING_STRING), intent(in) :: string
        type(ErrorList_t), intent(out) :: errors
        type(EnergyPerTemperatureAmount_t), intent(out) :: energy_per_temperature_amount

        type(ErrorList_t) :: errors_

        call fromString( &
                string, PROVIDED_UNITS, errors_, energy_per_temperature_amount)
        call errors%appendErrors( &
                errors_, &
                Module_("Energy_per_temperature_amount_m"), &
                Procedure_("fromStringBasicS"))
    end subroutine fromStringBasicS

    pure subroutine fromStringWithUnitsC(string, units, errors, energy_per_temperature_amount)
        character(len=*), intent(in) :: string
        class(EnergyPerTemperatureAmountUnit_t), intent(in) :: units(:)
        type(ErrorList_t), intent(out) :: errors
        type(EnergyPerTemperatureAmount_t), intent(out) :: energy_per_temperature_amount

        type(ErrorList_t) :: errors_

        call fromString( &
                var_str(string), units, errors_, energy_per_temperature_amount)
        call errors%appendErrors( &
                errors_, &
                Module_("Energy_per_temperature_amount_m"), &
                Procedure_("fromStringWithUnitsC"))
    end subroutine fromStringWithUnitsC

    pure subroutine fromStringWithUnitsS(string, units, errors, energy_per_temperature_amount)
        type(VARYING_STRING), intent(in) :: string
        class(EnergyPerTemperatureAmountUnit_t), intent(in) :: units(:)
        type(ErrorList_t), intent(out) :: errors
        type(EnergyPerTemperatureAmount_t), intent(out) :: energy_per_temperature_amount

        type(ErrorList_t) :: all_errors(size(units))
        integer :: i

        do i = 1, size(units)
            call units(i)%parseAs(string, all_errors(i), energy_per_temperature_amount)
            if (.not. all_errors(i)%hasAny()) return
        end do
        do i = 1, size(units)
            call errors%appendErrors( &
                    all_errors(i), &
                    Module_("Energy_per_temperature_amount_m"), &
                    Procedure_("fromStringWithUnitsS"))
        end do
    end subroutine fromStringWithUnitsS

    elemental function fromUnits(value_, units) result(energy_per_temperature_amount)
        double precision, intent(in) :: value_
        class(EnergyPerTemperatureAmountUnit_t), intent(in) :: units
        type(EnergyPerTemperatureAmount_t) :: energy_per_temperature_amount

        energy_per_temperature_amount%joules_per_kelvin_mol = value_ / units%conversion_factor
    end function fromUnits

    elemental function toUnits(self, units) result(energy_per_temperature_amount)
        class(EnergyPerTemperatureAmount_t), intent(in) :: self
        class(EnergyPerTemperatureAmountUnit_t), intent(in) :: units
        double precision :: energy_per_temperature_amount

        energy_per_temperature_amount = self%joules_per_kelvin_mol * units%conversion_factor
    end function toUnits

    elemental function doubleTimesEnergyPerTemperatureAmount( &
            multiplier, energy_per_temperature_amount) result(new_energy_per_temperature_amount)
        double precision, intent(in) :: multiplier
        class(EnergyPerTemperatureAmount_t), intent(in) :: energy_per_temperature_amount
        type(EnergyPerTemperatureAmount_t) :: new_energy_per_temperature_amount

        new_energy_per_temperature_amount%joules_per_kelvin_mol = &
                multiplier * energy_per_temperature_amount%joules_per_kelvin_mol
    end function doubleTimesEnergyPerTemperatureAmount

    elemental function integerTimesEnergyPerTemperatureAmount( &
            multiplier, energy_per_temperature_amount) result(new_energy_per_temperature_amount)
        integer, intent(in) :: multiplier
        class(EnergyPerTemperatureAmount_t), intent(in) :: energy_per_temperature_amount
        type(EnergyPerTemperatureAmount_t) :: new_energy_per_temperature_amount

        new_energy_per_temperature_amount%joules_per_kelvin_mol = &
                dble(multiplier) * energy_per_temperature_amount%joules_per_kelvin_mol
    end function integerTimesEnergyPerTemperatureAmount

    elemental function energyPerTemperatureAmountTimesDouble( &
            energy_per_temperature_amount, multiplier) result(new_energy_per_temperature_amount)
        class(EnergyPerTemperatureAmount_t), intent(in) :: energy_per_temperature_amount
        double precision, intent(in) :: multiplier
        type(EnergyPerTemperatureAmount_t) :: new_energy_per_temperature_amount

        new_energy_per_temperature_amount%joules_per_kelvin_mol = &
                energy_per_temperature_amount%joules_per_kelvin_mol * multiplier
    end function energyPerTemperatureAmountTimesDouble

    elemental function energyPerTemperatureAmountTimesInteger( &
            energy_per_temperature_amount, multiplier) result(new_energy_per_temperature_amount)
        class(EnergyPerTemperatureAmount_t), intent(in) :: energy_per_temperature_amount
        integer, intent(in) :: multiplier
        type(EnergyPerTemperatureAmount_t) :: new_energy_per_temperature_amount

        new_energy_per_temperature_amount%joules_per_kelvin_mol = &
                energy_per_temperature_amount%joules_per_kelvin_mol * dble(multiplier)
    end function energyPerTemperatureAmountTimesInteger

    elemental function energyPerTemperatureAmountDividedByDouble( &
            energy_per_temperature_amount, divisor) result(new_energy_per_temperature_amount)
        class(EnergyPerTemperatureAmount_t), intent(in) :: energy_per_temperature_amount
        double precision, intent(in) :: divisor
        type(EnergyPerTemperatureAmount_t) :: new_energy_per_temperature_amount

        new_energy_per_temperature_amount%joules_per_kelvin_mol = &
                energy_per_temperature_amount%joules_per_kelvin_mol / divisor
    end function energyPerTemperatureAmountDividedByDouble

    elemental function energyPerTemperatureAmountDividedByInteger( &
            energy_per_temperature_amount, divisor) result(new_energy_per_temperature_amount)
        class(EnergyPerTemperatureAmount_t), intent(in) :: energy_per_temperature_amount
        integer, intent(in) :: divisor
        type(EnergyPerTemperatureAmount_t) :: new_energy_per_temperature_amount

        new_energy_per_temperature_amount%joules_per_kelvin_mol = &
                energy_per_temperature_amount%joules_per_kelvin_mol / dble(divisor)
    end function energyPerTemperatureAmountDividedByInteger

    elemental function energyPerTemperatureAmountDividedByEnergyPerTemperatureAmount( &
            numerator, denomenator) result(ratio)
        class(EnergyPerTemperatureAmount_t), intent(in) :: numerator
        class(EnergyPerTemperatureAmount_t), intent(in) :: denomenator
        double precision :: ratio

        ratio = numerator%joules_per_kelvin_mol / denomenator%joules_per_kelvin_mol
    end function energyPerTemperatureAmountDividedByEnergyPerTemperatureAmount

    elemental function energyPerTemperatureAmountPlusEnergyPerTemperatureAmount( &
            energy_per_temperature_amount1, energy_per_temperature_amount2) result(new_energy_per_temperature_amount)
        class(EnergyPerTemperatureAmount_t), intent(in) :: energy_per_temperature_amount1
        class(EnergyPerTemperatureAmount_t), intent(in) :: energy_per_temperature_amount2
        type(EnergyPerTemperatureAmount_t) :: new_energy_per_temperature_amount

        new_energy_per_temperature_amount%joules_per_kelvin_mol = &
                energy_per_temperature_amount1%joules_per_kelvin_mol + energy_per_temperature_amount2%joules_per_kelvin_mol
    end function energyPerTemperatureAmountPlusEnergyPerTemperatureAmount

    elemental function energyPerTemperatureAmountMinusEnergyPerTemperatureAmount( &
            energy_per_temperature_amount1, energy_per_temperature_amount2) result(new_energy_per_temperature_amount)
        class(EnergyPerTemperatureAmount_t), intent(in) :: energy_per_temperature_amount1
        class(EnergyPerTemperatureAmount_t), intent(in) :: energy_per_temperature_amount2
        type(EnergyPerTemperatureAmount_t) :: new_energy_per_temperature_amount

        new_energy_per_temperature_amount%joules_per_kelvin_mol = &
                energy_per_temperature_amount1%joules_per_kelvin_mol - energy_per_temperature_amount2%joules_per_kelvin_mol
    end function energyPerTemperatureAmountMinusEnergyPerTemperatureAmount

    pure function sumEnergyPerTemperatureAmount(energy_per_temperature_amounts)
        type(EnergyPerTemperatureAmount_t), intent(in) :: energy_per_temperature_amounts(:)
        type(EnergyPerTemperatureAmount_t) :: sumEnergyPerTemperatureAmount

        sumEnergyPerTemperatureAmount%joules_per_kelvin_mol = sum(energy_per_temperature_amounts%joules_per_kelvin_mol)
    end function sumEnergyPerTemperatureAmount

    elemental function greaterThan(lhs, rhs)
        class(EnergyPerTemperatureAmount_t), intent(in) :: lhs
        class(EnergyPerTemperatureAmount_t), intent(in) :: rhs
        logical :: greaterThan

        greaterThan = lhs%joules_per_kelvin_mol > rhs%joules_per_kelvin_mol
    end function greaterThan

    elemental function lessThan(lhs,rhs)
        class(EnergyPerTemperatureAmount_t), intent(in) :: lhs
        class(EnergyPerTemperatureAmount_t), intent(in) :: rhs
        logical :: lessThan

        lessThan = lhs%joules_per_kelvin_mol < rhs%joules_per_kelvin_mol
    end function lessThan

    elemental function greaterThanOrEqual(lhs, rhs)
        class(EnergyPerTemperatureAmount_t), intent(in) :: lhs
        class(EnergyPerTemperatureAmount_t), intent(in) :: rhs
        logical :: greaterThanOrEqual

        greaterThanOrEqual = lhs%joules_per_kelvin_mol >= rhs%joules_per_kelvin_mol
    end function greaterThanOrEqual

    elemental function lessThanOrEqual(lhs, rhs)
        class(EnergyPerTemperatureAmount_t), intent(in) :: lhs
        class(EnergyPerTemperatureAmount_t), intent(in) :: rhs
        logical :: lessThanOrEqual

        lessThanOrEqual = lhs%joules_per_kelvin_mol <= rhs%joules_per_kelvin_mol
    end function lessThanOrEqual

    elemental function equal_(lhs,rhs)
        class(EnergyPerTemperatureAmount_t), intent(in) :: lhs
        class(EnergyPerTemperatureAmount_t), intent(in) :: rhs
        logical :: equal_

        equal_ = lhs%joules_per_kelvin_mol .safeEq. rhs%joules_per_kelvin_mol
    end function equal_

    elemental function equalWithinAbsolute(lhs, rhs, within)
        class(EnergyPerTemperatureAmount_t), intent(in) :: lhs
        class(EnergyPerTemperatureAmount_t), intent(in) :: rhs
        class(EnergyPerTemperatureAmount_t), intent(in) :: within
        logical :: equalWithinAbsolute

        equalWithinAbsolute = equalWithinAbsolute_( &
                lhs%joules_per_kelvin_mol, rhs%joules_per_kelvin_mol, within%joules_per_kelvin_mol)
    end function equalWithinAbsolute

    elemental function equalWithinRelative(lhs, rhs, within)
        class(EnergyPerTemperatureAmount_t), intent(in) :: lhs
        class(EnergyPerTemperatureAmount_t), intent(in) :: rhs
        double precision, intent(in) :: within
        logical :: equalWithinRelative

        equalWithinRelative = equalWithinRelative_( &
                lhs%joules_per_kelvin_mol, rhs%joules_per_kelvin_mol, within)
    end function equalWithinRelative

    elemental function notEqual(lhs, rhs)
        class(EnergyPerTemperatureAmount_t), intent(in) :: lhs
        class(EnergyPerTemperatureAmount_t), intent(in) :: rhs
        logical :: notEqual

        notEqual = .not. lhs == rhs
    end function notEqual

    elemental function toStringFullPrecision(self) result(string)
        class(EnergyPerTemperatureAmount_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = self%toStringIn(DEFAULT_OUTPUT_UNITS)
    end function toStringFullPrecision

    elemental function toStringWithPrecision(self, significant_digits) result(string)
        class(EnergyPerTemperatureAmount_t), intent(in) :: self
        integer, intent(in) :: significant_digits
        type(VARYING_STRING) :: string

        string = self%toStringIn(DEFAULT_OUTPUT_UNITS, significant_digits)
    end function toStringWithPrecision

    elemental function toStringInFullPrecision(self, units) result(string)
        class(EnergyPerTemperatureAmount_t), intent(in) :: self
        class(EnergyPerTemperatureAmountUnit_t), intent(in) :: units
        type(VARYING_STRING) :: string

        string = units%toString(toString(self.in.units))
    end function toStringInFullPrecision

    elemental function toStringInWithPrecision( &
            self, units, significant_digits) result(string)
        class(EnergyPerTemperatureAmount_t), intent(in) :: self
        class(EnergyPerTemperatureAmountUnit_t), intent(in) :: units
        integer, intent(in) :: significant_digits
        type(VARYING_STRING) :: string

        string = units%toString(toString(self.in.units, significant_digits))
    end function toStringInWithPrecision

    elemental function simpleUnitToString(self) result(string)
        class(EnergyPerTemperatureAmountSimpleUnit_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = trim(self%symbol)
    end function simpleUnitToString

    pure function simpleValueToString(self, value_) result(string)
        class(EnergyPerTemperatureAmountSimpleUnit_t), intent(in) :: self
        type(VARYING_STRING), intent(in) :: value_
        type(VARYING_STRING) :: string

        string = value_ // " " // self%toString()
    end function simpleValueToString

    pure subroutine simpleParseAs(self, string, errors, energy_per_temperature_amount)
        class(EnergyPerTemperatureAmountSimpleUnit_t), intent(in) :: self
        type(VARYING_STRING), intent(in) :: string
        type(ErrorList_t), intent(out) :: errors
        type(EnergyPerTemperatureAmount_t), intent(out) :: energy_per_temperature_amount

        type(ParseResult_t) :: parse_result

        parse_result = parseWith(theParser, string)
        if (parse_result%ok) then
            select type (the_number => parse_result%parsed)
            type is (ParsedRational_t)
                energy_per_temperature_amount = the_number%value_.unit.self
            end select
        else
            call errors%appendError(Fatal( &
                    PARSE_ERROR, &
                    Module_("Energy_per_temperature_amount_m"), &
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

    pure subroutine simpleUnitFromStringC(string, errors, unit)
        character(len=*), intent(in) :: string
        type(ErrorList_t), intent(out) :: errors
        type(EnergyPerTemperatureAmountSimpleUnit_t), intent(out) :: unit

        type(ErrorList_t) :: errors_

        call fromString(var_str(string), PROVIDED_UNITS, errors_, unit)
        call errors%appendErrors( &
                errors_, &
                Module_("Energy_per_temperature_amount_m"), &
                Procedure_("simpleUnitFromStringC"))
    end subroutine simpleUnitFromStringC

    pure subroutine simpleUnitFromStringS(string, errors, unit)
        type(VARYING_STRING), intent(in) :: string
        type(ErrorList_t), intent(out) :: errors
        type(EnergyPerTemperatureAmountSimpleUnit_t), intent(out) :: unit

        type(ErrorList_t) :: errors_

        call fromString(string, PROVIDED_UNITS, errors_, unit)
        call errors%appendErrors( &
                errors_, &
                Module_("Energy_per_temperature_amount_m"), &
                Procedure_("simpleUnitFromStringS"))
    end subroutine simpleUnitFromStringS

    pure subroutine simpleUnitFromStringWithUnitsC(string, units, errors, unit)
        character(len=*), intent(in) :: string
        type(EnergyPerTemperatureAmountSimpleUnit_t), intent(in) :: units(:)
        type(ErrorList_t), intent(out) :: errors
        type(EnergyPerTemperatureAmountSimpleUnit_t), intent(out) :: unit

        type(ErrorList_t) :: errors_

        call fromString(var_str(string), units, errors_, unit)
        call errors%appendErrors( &
                errors_, &
                Module_("Energy_per_temperature_amount_m"), &
                Procedure_("simpleUnitFromStringWithUnitsC"))
    end subroutine simpleUnitFromStringWithUnitsC

    pure subroutine simpleUnitFromStringWithUnitsS(string, units, errors, unit)
        type(VARYING_STRING), intent(in) :: string
        type(EnergyPerTemperatureAmountSimpleUnit_t), intent(in) :: units(:)
        type(ErrorList_t), intent(out) :: errors
        type(EnergyPerTemperatureAmountSimpleUnit_t), intent(out) :: unit

        type(ErrorList_t) :: errors_
        integer :: which_unit

        call selectUnit(string, units, errors, which_unit)
        if (errors_%hasAny()) then
            call errors%appendErrors( &
                    errors_, &
                    Module_("Energy_per_temperature_amount_m"), &
                    Procedure_("simpleUnitFromStringWithUnitsS"))
        else
            unit = units(which_unit)
        end if
    end subroutine simpleUnitFromStringWithUnitsS

    pure subroutine selectUnit(string, units, errors, index)
        type(VARYING_STRING), intent(in) :: string
        class(EnergyPerTemperatureAmountUnit_t), intent(in) :: units(:)
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
                Module_("Energy_per_temperature_amount_m"), &
                Procedure_("selectUnit"), &
                '"' // string // '", known units: [' // join(unit_strings, ', ') // ']'))
    end subroutine selectUnit
end module Energy_per_temperature_amount_m
