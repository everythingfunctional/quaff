module Energy_per_amount_m
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
            KILOJOULES_PER_MOL_PER_JOULES_PER_MOL
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

    type, public :: EnergyPerAmount_t
        double precision :: joules_per_mol
    contains
        private
        procedure :: toUnits
        generic, public :: operator(.in.) => toUnits
        procedure, pass(energy_per_amount) :: doubleTimesEnergyPerAmount
        procedure, pass(energy_per_amount) :: integerTimesEnergyPerAmount
        procedure, pass(energy_per_amount) :: energyPerAmountTimesDouble
        procedure, pass(energy_per_amount) :: energyPerAmountTimesInteger
        generic, public :: operator(*) => &
                doubleTimesEnergyPerAmount, &
                integerTimesEnergyPerAmount, &
                energyPerAmountTimesDouble, &
                energyPerAmountTimesInteger
        procedure :: energyPerAmountDividedByDouble
        procedure :: energyPerAmountDividedByInteger
        procedure, pass(numerator) :: energyPerAmountDividedByEnergyPerAmount
        generic, public :: operator(/) => &
                energyPerAmountDividedByDouble, &
                energyPerAmountDividedByInteger, &
                energyPerAmountDividedByEnergyPerAmount
        procedure :: energyPerAmountPlusEnergyPerAmount
        generic, public :: operator(+) => energyPerAmountPlusEnergyPerAmount
        procedure :: energyPerAmountMinusEnergyPerAmount
        generic, public :: operator(-) => energyPerAmountMinusEnergyPerAmount
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
    end type EnergyPerAmount_t

    type, abstract, public :: EnergyPerAmountUnit_t
        double precision :: conversion_factor
    contains
        procedure(justUnitToString), deferred :: unitToString
        procedure(unitWithValueToString), deferred :: valueToString
        generic :: toString => unitToString, valueToString
        procedure(parseAsI), deferred :: parseAs
    end type EnergyPerAmountUnit_t

    type, extends(EnergyPerAmountUnit_t), public :: EnergyPerAmountSimpleUnit_t
        character(len=20) :: symbol
    contains
        procedure :: unitToString => simpleUnitToString
        procedure :: valueToString => simpleValueToString
        procedure :: parseAs => simpleParseAs
    end type EnergyPerAmountSimpleUnit_t

    abstract interface
        elemental function justUnitToString(self) result(string)
            import EnergyPerAmountUnit_t, VARYING_STRING
            class(EnergyPerAmountUnit_t), intent(in) :: self
            type(VARYING_STRING) :: string
        end function justUnitToString

        pure function unitWithValueToString(self, value_) result(string)
            import EnergyPerAmountUnit_t, VARYING_STRING
            class(EnergyPerAmountUnit_t), intent(in) :: self
            type(VARYING_STRING), intent(in) :: value_
            type(VARYING_STRING) :: string
        end function unitWithValueToString

        pure subroutine parseAsI(self, string, errors, energy_per_amount)
            import ErrorList_t, EnergyPerAmount_t, EnergyPerAmountUnit_t, VARYING_STRING
            class(EnergyPerAmountUnit_t), intent(in) :: self
            type(VARYING_STRING), intent(in) :: string
            type(ErrorList_t), intent(out) :: errors
            type(EnergyPerAmount_t), intent(out) :: energy_per_amount
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
        module procedure sumEnergyPerAmount
    end interface sum

    type(EnergyPerAmountSimpleUnit_t), parameter, public :: JOULES_PER_MOL = &
            EnergyPerAmountSimpleUnit_t( &
                    conversion_factor = 1.0d0, &
                    symbol = "J/mol")
    type(EnergyPerAmountSimpleUnit_t), parameter, public :: KILOJOULES_PER_MOL = &
            EnergyPerAmountSimpleUnit_t( &
                    conversion_factor = KILOJOULES_PER_MOL_PER_JOULES_PER_MOL, &
                    symbol = "kJ/mol")

    type(EnergyPerAmountSimpleUnit_t), public :: DEFAULT_OUTPUT_UNITS = JOULES_PER_MOL

    type(EnergyPerAmountSimpleUnit_t), parameter, public :: PROVIDED_UNITS(*) = &
            [JOULES_PER_MOL, KILOJOULES_PER_MOL]

    public :: operator(.unit.), fromString, selectUnit, sum
contains
    pure subroutine fromStringBasicC(string, errors, energy_per_amount)
        character(len=*), intent(in) :: string
        type(ErrorList_t), intent(out) :: errors
        type(EnergyPerAmount_t), intent(out) :: energy_per_amount

        type(ErrorList_t) :: errors_

        call fromString( &
                var_str(string), PROVIDED_UNITS, errors_, energy_per_amount)
        call errors%appendErrors( &
                errors_, &
                Module_("Energy_per_amount_m"), &
                Procedure_("fromStringBasicC"))
    end subroutine fromStringBasicC

    pure subroutine fromStringBasicS(string, errors, energy_per_amount)
        type(VARYING_STRING), intent(in) :: string
        type(ErrorList_t), intent(out) :: errors
        type(EnergyPerAmount_t), intent(out) :: energy_per_amount

        type(ErrorList_t) :: errors_

        call fromString( &
                string, PROVIDED_UNITS, errors_, energy_per_amount)
        call errors%appendErrors( &
                errors_, &
                Module_("Energy_per_amount_m"), &
                Procedure_("fromStringBasicS"))
    end subroutine fromStringBasicS

    pure subroutine fromStringWithUnitsC(string, units, errors, energy_per_amount)
        character(len=*), intent(in) :: string
        class(EnergyPerAmountUnit_t), intent(in) :: units(:)
        type(ErrorList_t), intent(out) :: errors
        type(EnergyPerAmount_t), intent(out) :: energy_per_amount

        type(ErrorList_t) :: errors_

        call fromString( &
                var_str(string), units, errors_, energy_per_amount)
        call errors%appendErrors( &
                errors_, &
                Module_("Energy_per_amount_m"), &
                Procedure_("fromStringWithUnitsC"))
    end subroutine fromStringWithUnitsC

    pure subroutine fromStringWithUnitsS(string, units, errors, energy_per_amount)
        type(VARYING_STRING), intent(in) :: string
        class(EnergyPerAmountUnit_t), intent(in) :: units(:)
        type(ErrorList_t), intent(out) :: errors
        type(EnergyPerAmount_t), intent(out) :: energy_per_amount

        type(ErrorList_t) :: all_errors(size(units))
        integer :: i

        do i = 1, size(units)
            call units(i)%parseAs(string, all_errors(i), energy_per_amount)
            if (.not. all_errors(i)%hasAny()) return
        end do
        do i = 1, size(units)
            call errors%appendErrors( &
                    all_errors(i), &
                    Module_("Energy_per_amount_m"), &
                    Procedure_("fromStringWithUnitsS"))
        end do
    end subroutine fromStringWithUnitsS

    elemental function fromUnits(value_, units) result(energy_per_amount)
        double precision, intent(in) :: value_
        class(EnergyPerAmountUnit_t), intent(in) :: units
        type(EnergyPerAmount_t) :: energy_per_amount

        energy_per_amount%joules_per_mol = value_ / units%conversion_factor
    end function fromUnits

    elemental function toUnits(self, units) result(energy_per_amount)
        class(EnergyPerAmount_t), intent(in) :: self
        class(EnergyPerAmountUnit_t), intent(in) :: units
        double precision :: energy_per_amount

        energy_per_amount = self%joules_per_mol * units%conversion_factor
    end function toUnits

    elemental function doubleTimesEnergyPerAmount( &
            multiplier, energy_per_amount) result(new_energy_per_amount)
        double precision, intent(in) :: multiplier
        class(EnergyPerAmount_t), intent(in) :: energy_per_amount
        type(EnergyPerAmount_t) :: new_energy_per_amount

        new_energy_per_amount%joules_per_mol = &
                multiplier * energy_per_amount%joules_per_mol
    end function doubleTimesEnergyPerAmount

    elemental function integerTimesEnergyPerAmount( &
            multiplier, energy_per_amount) result(new_energy_per_amount)
        integer, intent(in) :: multiplier
        class(EnergyPerAmount_t), intent(in) :: energy_per_amount
        type(EnergyPerAmount_t) :: new_energy_per_amount

        new_energy_per_amount%joules_per_mol = &
                dble(multiplier) * energy_per_amount%joules_per_mol
    end function integerTimesEnergyPerAmount

    elemental function energyPerAmountTimesDouble( &
            energy_per_amount, multiplier) result(new_energy_per_amount)
        class(EnergyPerAmount_t), intent(in) :: energy_per_amount
        double precision, intent(in) :: multiplier
        type(EnergyPerAmount_t) :: new_energy_per_amount

        new_energy_per_amount%joules_per_mol = &
                energy_per_amount%joules_per_mol * multiplier
    end function energyPerAmountTimesDouble

    elemental function energyPerAmountTimesInteger( &
            energy_per_amount, multiplier) result(new_energy_per_amount)
        class(EnergyPerAmount_t), intent(in) :: energy_per_amount
        integer, intent(in) :: multiplier
        type(EnergyPerAmount_t) :: new_energy_per_amount

        new_energy_per_amount%joules_per_mol = &
                energy_per_amount%joules_per_mol * dble(multiplier)
    end function energyPerAmountTimesInteger

    elemental function energyPerAmountDividedByDouble( &
            energy_per_amount, divisor) result(new_energy_per_amount)
        class(EnergyPerAmount_t), intent(in) :: energy_per_amount
        double precision, intent(in) :: divisor
        type(EnergyPerAmount_t) :: new_energy_per_amount

        new_energy_per_amount%joules_per_mol = &
                energy_per_amount%joules_per_mol / divisor
    end function energyPerAmountDividedByDouble

    elemental function energyPerAmountDividedByInteger( &
            energy_per_amount, divisor) result(new_energy_per_amount)
        class(EnergyPerAmount_t), intent(in) :: energy_per_amount
        integer, intent(in) :: divisor
        type(EnergyPerAmount_t) :: new_energy_per_amount

        new_energy_per_amount%joules_per_mol = &
                energy_per_amount%joules_per_mol / dble(divisor)
    end function energyPerAmountDividedByInteger

    elemental function energyPerAmountDividedByEnergyPerAmount( &
            numerator, denomenator) result(ratio)
        class(EnergyPerAmount_t), intent(in) :: numerator
        class(EnergyPerAmount_t), intent(in) :: denomenator
        double precision :: ratio

        ratio = numerator%joules_per_mol / denomenator%joules_per_mol
    end function energyPerAmountDividedByEnergyPerAmount

    elemental function energyPerAmountPlusEnergyPerAmount( &
            energy_per_amount1, energy_per_amount2) result(new_energy_per_amount)
        class(EnergyPerAmount_t), intent(in) :: energy_per_amount1
        class(EnergyPerAmount_t), intent(in) :: energy_per_amount2
        type(EnergyPerAmount_t) :: new_energy_per_amount

        new_energy_per_amount%joules_per_mol = &
                energy_per_amount1%joules_per_mol + energy_per_amount2%joules_per_mol
    end function energyPerAmountPlusEnergyPerAmount

    elemental function energyPerAmountMinusEnergyPerAmount( &
            energy_per_amount1, energy_per_amount2) result(new_energy_per_amount)
        class(EnergyPerAmount_t), intent(in) :: energy_per_amount1
        class(EnergyPerAmount_t), intent(in) :: energy_per_amount2
        type(EnergyPerAmount_t) :: new_energy_per_amount

        new_energy_per_amount%joules_per_mol = &
                energy_per_amount1%joules_per_mol - energy_per_amount2%joules_per_mol
    end function energyPerAmountMinusEnergyPerAmount

    pure function sumEnergyPerAmount(energy_per_amounts)
        type(EnergyPerAmount_t), intent(in) :: energy_per_amounts(:)
        type(EnergyPerAmount_t) :: sumEnergyPerAmount

        sumEnergyPerAmount%joules_per_mol = sum(energy_per_amounts%joules_per_mol)
    end function sumEnergyPerAmount

    elemental function greaterThan(lhs, rhs)
        class(EnergyPerAmount_t), intent(in) :: lhs
        class(EnergyPerAmount_t), intent(in) :: rhs
        logical :: greaterThan

        greaterThan = lhs%joules_per_mol > rhs%joules_per_mol
    end function greaterThan

    elemental function lessThan(lhs,rhs)
        class(EnergyPerAmount_t), intent(in) :: lhs
        class(EnergyPerAmount_t), intent(in) :: rhs
        logical :: lessThan

        lessThan = lhs%joules_per_mol < rhs%joules_per_mol
    end function lessThan

    elemental function greaterThanOrEqual(lhs, rhs)
        class(EnergyPerAmount_t), intent(in) :: lhs
        class(EnergyPerAmount_t), intent(in) :: rhs
        logical :: greaterThanOrEqual

        greaterThanOrEqual = lhs%joules_per_mol >= rhs%joules_per_mol
    end function greaterThanOrEqual

    elemental function lessThanOrEqual(lhs, rhs)
        class(EnergyPerAmount_t), intent(in) :: lhs
        class(EnergyPerAmount_t), intent(in) :: rhs
        logical :: lessThanOrEqual

        lessThanOrEqual = lhs%joules_per_mol <= rhs%joules_per_mol
    end function lessThanOrEqual

    elemental function equal_(lhs,rhs)
        class(EnergyPerAmount_t), intent(in) :: lhs
        class(EnergyPerAmount_t), intent(in) :: rhs
        logical :: equal_

        equal_ = lhs%joules_per_mol .safeEq. rhs%joules_per_mol
    end function equal_

    elemental function equalWithinAbsolute(lhs, rhs, within)
        class(EnergyPerAmount_t), intent(in) :: lhs
        class(EnergyPerAmount_t), intent(in) :: rhs
        class(EnergyPerAmount_t), intent(in) :: within
        logical :: equalWithinAbsolute

        equalWithinAbsolute = equalWithinAbsolute_( &
                lhs%joules_per_mol, rhs%joules_per_mol, within%joules_per_mol)
    end function equalWithinAbsolute

    elemental function equalWithinRelative(lhs, rhs, within)
        class(EnergyPerAmount_t), intent(in) :: lhs
        class(EnergyPerAmount_t), intent(in) :: rhs
        double precision, intent(in) :: within
        logical :: equalWithinRelative

        equalWithinRelative = equalWithinRelative_( &
                lhs%joules_per_mol, rhs%joules_per_mol, within)
    end function equalWithinRelative

    elemental function notEqual(lhs, rhs)
        class(EnergyPerAmount_t), intent(in) :: lhs
        class(EnergyPerAmount_t), intent(in) :: rhs
        logical :: notEqual

        notEqual = .not. lhs == rhs
    end function notEqual

    elemental function toStringFullPrecision(self) result(string)
        class(EnergyPerAmount_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = self%toStringIn(DEFAULT_OUTPUT_UNITS)
    end function toStringFullPrecision

    elemental function toStringWithPrecision(self, significant_digits) result(string)
        class(EnergyPerAmount_t), intent(in) :: self
        integer, intent(in) :: significant_digits
        type(VARYING_STRING) :: string

        string = self%toStringIn(DEFAULT_OUTPUT_UNITS, significant_digits)
    end function toStringWithPrecision

    elemental function toStringInFullPrecision(self, units) result(string)
        class(EnergyPerAmount_t), intent(in) :: self
        class(EnergyPerAmountUnit_t), intent(in) :: units
        type(VARYING_STRING) :: string

        string = units%toString(toString(self.in.units))
    end function toStringInFullPrecision

    elemental function toStringInWithPrecision( &
            self, units, significant_digits) result(string)
        class(EnergyPerAmount_t), intent(in) :: self
        class(EnergyPerAmountUnit_t), intent(in) :: units
        integer, intent(in) :: significant_digits
        type(VARYING_STRING) :: string

        string = units%toString(toString(self.in.units, significant_digits))
    end function toStringInWithPrecision

    elemental function simpleUnitToString(self) result(string)
        class(EnergyPerAmountSimpleUnit_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = trim(self%symbol)
    end function simpleUnitToString

    pure function simpleValueToString(self, value_) result(string)
        class(EnergyPerAmountSimpleUnit_t), intent(in) :: self
        type(VARYING_STRING), intent(in) :: value_
        type(VARYING_STRING) :: string

        string = value_ // " " // self%toString()
    end function simpleValueToString

    pure subroutine simpleParseAs(self, string, errors, energy_per_amount)
        class(EnergyPerAmountSimpleUnit_t), intent(in) :: self
        type(VARYING_STRING), intent(in) :: string
        type(ErrorList_t), intent(out) :: errors
        type(EnergyPerAmount_t), intent(out) :: energy_per_amount

        type(ParseResult_t) :: parse_result

        parse_result = parseWith(theParser, string)
        if (parse_result%ok) then
            select type (the_number => parse_result%parsed)
            type is (ParsedRational_t)
                energy_per_amount = the_number%value_.unit.self
            end select
        else
            call errors%appendError(Fatal( &
                    PARSE_ERROR, &
                    Module_("Energy_per_amount_m"), &
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
        type(EnergyPerAmountSimpleUnit_t), intent(out) :: unit

        type(ErrorList_t) :: errors_

        call fromString(var_str(string), PROVIDED_UNITS, errors_, unit)
        call errors%appendErrors( &
                errors_, &
                Module_("Energy_per_amount_m"), &
                Procedure_("simpleUnitFromStringC"))
    end subroutine simpleUnitFromStringC

    pure subroutine simpleUnitFromStringS(string, errors, unit)
        type(VARYING_STRING), intent(in) :: string
        type(ErrorList_t), intent(out) :: errors
        type(EnergyPerAmountSimpleUnit_t), intent(out) :: unit

        type(ErrorList_t) :: errors_

        call fromString(string, PROVIDED_UNITS, errors_, unit)
        call errors%appendErrors( &
                errors_, &
                Module_("Energy_per_amount_m"), &
                Procedure_("simpleUnitFromStringS"))
    end subroutine simpleUnitFromStringS

    pure subroutine simpleUnitFromStringWithUnitsC(string, units, errors, unit)
        character(len=*), intent(in) :: string
        type(EnergyPerAmountSimpleUnit_t), intent(in) :: units(:)
        type(ErrorList_t), intent(out) :: errors
        type(EnergyPerAmountSimpleUnit_t), intent(out) :: unit

        type(ErrorList_t) :: errors_

        call fromString(var_str(string), units, errors_, unit)
        call errors%appendErrors( &
                errors_, &
                Module_("Energy_per_amount_m"), &
                Procedure_("simpleUnitFromStringWithUnitsC"))
    end subroutine simpleUnitFromStringWithUnitsC

    pure subroutine simpleUnitFromStringWithUnitsS(string, units, errors, unit)
        type(VARYING_STRING), intent(in) :: string
        type(EnergyPerAmountSimpleUnit_t), intent(in) :: units(:)
        type(ErrorList_t), intent(out) :: errors
        type(EnergyPerAmountSimpleUnit_t), intent(out) :: unit

        type(ErrorList_t) :: errors_
        integer :: which_unit

        call selectUnit(string, units, errors, which_unit)
        if (errors_%hasAny()) then
            call errors%appendErrors( &
                    errors_, &
                    Module_("Energy_per_amount_m"), &
                    Procedure_("simpleUnitFromStringWithUnitsS"))
        else
            unit = units(which_unit)
        end if
    end subroutine simpleUnitFromStringWithUnitsS

    pure subroutine selectUnit(string, units, errors, index)
        type(VARYING_STRING), intent(in) :: string
        class(EnergyPerAmountUnit_t), intent(in) :: units(:)
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
                Module_("Energy_per_amount_m"), &
                Procedure_("selectUnit"), &
                '"' // string // '", known units: [' // join(unit_strings, ', ') // ']'))
    end subroutine selectUnit
end module Energy_per_amount_m
