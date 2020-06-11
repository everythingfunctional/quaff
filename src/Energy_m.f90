module Energy_m
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
            BTU_PER_JOULE, &
            CALORIES_PER_JOULE, &
            KILOJOULES_PER_JOULE, &
            MEGABTU_PER_JOULE, &
            MEGAWATT_DAYS_PER_JOULE
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

    type, public :: Energy_t
        double precision :: joules
    contains
        private
        procedure :: toUnits
        generic, public :: operator(.in.) => toUnits
        procedure, pass(energy) :: doubleTimesEnergy
        procedure, pass(energy) :: integerTimesEnergy
        procedure, pass(energy) :: energyTimesDouble
        procedure, pass(energy) :: energyTimesInteger
        generic, public :: operator(*) => &
                doubleTimesEnergy, &
                integerTimesEnergy, &
                energyTimesDouble, &
                energyTimesInteger
        procedure :: energyDividedByDouble
        procedure :: energyDividedByInteger
        procedure, pass(numerator) :: energyDividedByEnergy
        generic, public :: operator(/) => &
                energyDividedByDouble, &
                energyDividedByInteger, &
                energyDividedByEnergy
        procedure :: energyPlusEnergy
        generic, public :: operator(+) => energyPlusEnergy
        procedure :: energyMinusEnergy
        generic, public :: operator(-) => energyMinusEnergy
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
    end type Energy_t

    type, abstract, public :: EnergyUnit_t
        double precision :: conversion_factor
    contains
        procedure(justUnitToString), deferred :: unitToString
        procedure(unitWithValueToString), deferred :: valueToString
        generic :: toString => unitToString, valueToString
        procedure(parseAsI), deferred :: parseAs
    end type EnergyUnit_t

    type, extends(EnergyUnit_t), public :: EnergySimpleUnit_t
        character(len=20) :: symbol
    contains
        procedure :: unitToString => simpleUnitToString
        procedure :: valueToString => simpleValueToString
        procedure :: parseAs => simpleParseAs
    end type EnergySimpleUnit_t

    abstract interface
        elemental function justUnitToString(self) result(string)
            import EnergyUnit_t, VARYING_STRING
            class(EnergyUnit_t), intent(in) :: self
            type(VARYING_STRING) :: string
        end function justUnitToString

        pure function unitWithValueToString(self, value_) result(string)
            import EnergyUnit_t, VARYING_STRING
            class(EnergyUnit_t), intent(in) :: self
            type(VARYING_STRING), intent(in) :: value_
            type(VARYING_STRING) :: string
        end function unitWithValueToString

        pure subroutine parseAsI(self, string, errors, energy)
            import ErrorList_t, Energy_t, EnergyUnit_t, VARYING_STRING
            class(EnergyUnit_t), intent(in) :: self
            type(VARYING_STRING), intent(in) :: string
            type(ErrorList_t), intent(out) :: errors
            type(Energy_t), intent(out) :: energy
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
        module procedure sumEnergy
    end interface sum

    type(EnergySimpleUnit_t), parameter, public :: BTU = &
            EnergySimpleUnit_t( &
                    conversion_factor = BTU_PER_JOULE, &
                    symbol = "BTU")
    type(EnergySimpleUnit_t), parameter, public :: CALORIES = &
            EnergySimpleUnit_t( &
                    conversion_factor = CALORIES_PER_JOULE, &
                    symbol = "cal")
    type(EnergySimpleUnit_t), parameter, public :: JOULES = &
            EnergySimpleUnit_t( &
                    conversion_factor = 1.0d0, &
                    symbol = "J")
    type(EnergySimpleUnit_t), parameter, public :: KILOJOULES = &
            EnergySimpleUnit_t( &
                    conversion_factor = KILOJOULES_PER_JOULE, &
                    symbol = "kJ")
    type(EnergySimpleUnit_t), parameter, public :: MEGABTU = &
            EnergySimpleUnit_t( &
                    conversion_factor = MEGABTU_PER_JOULE, &
                    symbol = "MBTU")
    type(EnergySimpleUnit_t), parameter, public :: MEGAWATT_DAYS = &
            EnergySimpleUnit_t( &
                    conversion_factor = MEGAWATT_DAYS_PER_JOULE, &
                    symbol = "MW d")

    type(EnergySimpleUnit_t), public :: DEFAULT_OUTPUT_UNITS = JOULES

    type(EnergySimpleUnit_t), parameter, public :: PROVIDED_UNITS(*) = &
            [BTU, CALORIES, JOULES, KILOJOULES, MEGABTU, MEGAWATT_DAYS]

    public :: operator(.unit.), fromString, selectUnit, sum
contains
    pure subroutine fromStringBasicC(string, errors, energy)
        character(len=*), intent(in) :: string
        type(ErrorList_t), intent(out) :: errors
        type(Energy_t), intent(out) :: energy

        type(ErrorList_t) :: errors_

        call fromString( &
                var_str(string), PROVIDED_UNITS, errors_, energy)
        call errors%appendErrors( &
                errors_, &
                Module_("Energy_m"), &
                Procedure_("fromStringBasicC"))
    end subroutine fromStringBasicC

    pure subroutine fromStringBasicS(string, errors, energy)
        type(VARYING_STRING), intent(in) :: string
        type(ErrorList_t), intent(out) :: errors
        type(Energy_t), intent(out) :: energy

        type(ErrorList_t) :: errors_

        call fromString( &
                string, PROVIDED_UNITS, errors_, energy)
        call errors%appendErrors( &
                errors_, &
                Module_("Energy_m"), &
                Procedure_("fromStringBasicS"))
    end subroutine fromStringBasicS

    pure subroutine fromStringWithUnitsC(string, units, errors, energy)
        character(len=*), intent(in) :: string
        class(EnergyUnit_t), intent(in) :: units(:)
        type(ErrorList_t), intent(out) :: errors
        type(Energy_t), intent(out) :: energy

        type(ErrorList_t) :: errors_

        call fromString( &
                var_str(string), units, errors_, energy)
        call errors%appendErrors( &
                errors_, &
                Module_("Energy_m"), &
                Procedure_("fromStringWithUnitsC"))
    end subroutine fromStringWithUnitsC

    pure subroutine fromStringWithUnitsS(string, units, errors, energy)
        type(VARYING_STRING), intent(in) :: string
        class(EnergyUnit_t), intent(in) :: units(:)
        type(ErrorList_t), intent(out) :: errors
        type(Energy_t), intent(out) :: energy

        type(ErrorList_t) :: all_errors(size(units))
        integer :: i

        do i = 1, size(units)
            call units(i)%parseAs(string, all_errors(i), energy)
            if (.not. all_errors(i)%hasAny()) return
        end do
        do i = 1, size(units)
            call errors%appendErrors( &
                    all_errors(i), &
                    Module_("Energy_m"), &
                    Procedure_("fromStringWithUnitsS"))
        end do
    end subroutine fromStringWithUnitsS

    elemental function fromUnits(value_, units) result(energy)
        double precision, intent(in) :: value_
        class(EnergyUnit_t), intent(in) :: units
        type(Energy_t) :: energy

        energy%joules = value_ / units%conversion_factor
    end function fromUnits

    elemental function toUnits(self, units) result(energy)
        class(Energy_t), intent(in) :: self
        class(EnergyUnit_t), intent(in) :: units
        double precision :: energy

        energy = self%joules * units%conversion_factor
    end function toUnits

    elemental function doubleTimesEnergy( &
            multiplier, energy) result(new_energy)
        double precision, intent(in) :: multiplier
        class(Energy_t), intent(in) :: energy
        type(Energy_t) :: new_energy

        new_energy%joules = &
                multiplier * energy%joules
    end function doubleTimesEnergy

    elemental function integerTimesEnergy( &
            multiplier, energy) result(new_energy)
        integer, intent(in) :: multiplier
        class(Energy_t), intent(in) :: energy
        type(Energy_t) :: new_energy

        new_energy%joules = &
                dble(multiplier) * energy%joules
    end function integerTimesEnergy

    elemental function energyTimesDouble( &
            energy, multiplier) result(new_energy)
        class(Energy_t), intent(in) :: energy
        double precision, intent(in) :: multiplier
        type(Energy_t) :: new_energy

        new_energy%joules = &
                energy%joules * multiplier
    end function energyTimesDouble

    elemental function energyTimesInteger( &
            energy, multiplier) result(new_energy)
        class(Energy_t), intent(in) :: energy
        integer, intent(in) :: multiplier
        type(Energy_t) :: new_energy

        new_energy%joules = &
                energy%joules * dble(multiplier)
    end function energyTimesInteger

    elemental function energyDividedByDouble( &
            energy, divisor) result(new_energy)
        class(Energy_t), intent(in) :: energy
        double precision, intent(in) :: divisor
        type(Energy_t) :: new_energy

        new_energy%joules = &
                energy%joules / divisor
    end function energyDividedByDouble

    elemental function energyDividedByInteger( &
            energy, divisor) result(new_energy)
        class(Energy_t), intent(in) :: energy
        integer, intent(in) :: divisor
        type(Energy_t) :: new_energy

        new_energy%joules = &
                energy%joules / dble(divisor)
    end function energyDividedByInteger

    elemental function energyDividedByEnergy( &
            numerator, denomenator) result(ratio)
        class(Energy_t), intent(in) :: numerator
        class(Energy_t), intent(in) :: denomenator
        double precision :: ratio

        ratio = numerator%joules / denomenator%joules
    end function energyDividedByEnergy

    elemental function energyPlusEnergy( &
            energy1, energy2) result(new_energy)
        class(Energy_t), intent(in) :: energy1
        class(Energy_t), intent(in) :: energy2
        type(Energy_t) :: new_energy

        new_energy%joules = &
                energy1%joules + energy2%joules
    end function energyPlusEnergy

    elemental function energyMinusEnergy( &
            energy1, energy2) result(new_energy)
        class(Energy_t), intent(in) :: energy1
        class(Energy_t), intent(in) :: energy2
        type(Energy_t) :: new_energy

        new_energy%joules = &
                energy1%joules - energy2%joules
    end function energyMinusEnergy

    pure function sumEnergy(energys)
        type(Energy_t), intent(in) :: energys(:)
        type(Energy_t) :: sumEnergy

        sumEnergy%joules = sum(energys%joules)
    end function sumEnergy

    elemental function greaterThan(lhs, rhs)
        class(Energy_t), intent(in) :: lhs
        class(Energy_t), intent(in) :: rhs
        logical :: greaterThan

        greaterThan = lhs%joules > rhs%joules
    end function greaterThan

    elemental function lessThan(lhs,rhs)
        class(Energy_t), intent(in) :: lhs
        class(Energy_t), intent(in) :: rhs
        logical :: lessThan

        lessThan = lhs%joules < rhs%joules
    end function lessThan

    elemental function greaterThanOrEqual(lhs, rhs)
        class(Energy_t), intent(in) :: lhs
        class(Energy_t), intent(in) :: rhs
        logical :: greaterThanOrEqual

        greaterThanOrEqual = lhs%joules >= rhs%joules
    end function greaterThanOrEqual

    elemental function lessThanOrEqual(lhs, rhs)
        class(Energy_t), intent(in) :: lhs
        class(Energy_t), intent(in) :: rhs
        logical :: lessThanOrEqual

        lessThanOrEqual = lhs%joules <= rhs%joules
    end function lessThanOrEqual

    elemental function equal_(lhs,rhs)
        class(Energy_t), intent(in) :: lhs
        class(Energy_t), intent(in) :: rhs
        logical :: equal_

        equal_ = lhs%joules .safeEq. rhs%joules
    end function equal_

    elemental function equalWithinAbsolute(lhs, rhs, within)
        class(Energy_t), intent(in) :: lhs
        class(Energy_t), intent(in) :: rhs
        class(Energy_t), intent(in) :: within
        logical :: equalWithinAbsolute

        equalWithinAbsolute = equalWithinAbsolute_( &
                lhs%joules, rhs%joules, within%joules)
    end function equalWithinAbsolute

    elemental function equalWithinRelative(lhs, rhs, within)
        class(Energy_t), intent(in) :: lhs
        class(Energy_t), intent(in) :: rhs
        double precision, intent(in) :: within
        logical :: equalWithinRelative

        equalWithinRelative = equalWithinRelative_( &
                lhs%joules, rhs%joules, within)
    end function equalWithinRelative

    elemental function notEqual(lhs, rhs)
        class(Energy_t), intent(in) :: lhs
        class(Energy_t), intent(in) :: rhs
        logical :: notEqual

        notEqual = .not. lhs == rhs
    end function notEqual

    elemental function toStringFullPrecision(self) result(string)
        class(Energy_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = self%toStringIn(DEFAULT_OUTPUT_UNITS)
    end function toStringFullPrecision

    elemental function toStringWithPrecision(self, significant_digits) result(string)
        class(Energy_t), intent(in) :: self
        integer, intent(in) :: significant_digits
        type(VARYING_STRING) :: string

        string = self%toStringIn(DEFAULT_OUTPUT_UNITS, significant_digits)
    end function toStringWithPrecision

    elemental function toStringInFullPrecision(self, units) result(string)
        class(Energy_t), intent(in) :: self
        class(EnergyUnit_t), intent(in) :: units
        type(VARYING_STRING) :: string

        string = units%toString(toString(self.in.units))
    end function toStringInFullPrecision

    elemental function toStringInWithPrecision( &
            self, units, significant_digits) result(string)
        class(Energy_t), intent(in) :: self
        class(EnergyUnit_t), intent(in) :: units
        integer, intent(in) :: significant_digits
        type(VARYING_STRING) :: string

        string = units%toString(toString(self.in.units, significant_digits))
    end function toStringInWithPrecision

    elemental function simpleUnitToString(self) result(string)
        class(EnergySimpleUnit_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = trim(self%symbol)
    end function simpleUnitToString

    pure function simpleValueToString(self, value_) result(string)
        class(EnergySimpleUnit_t), intent(in) :: self
        type(VARYING_STRING), intent(in) :: value_
        type(VARYING_STRING) :: string

        string = value_ // " " // self%toString()
    end function simpleValueToString

    pure subroutine simpleParseAs(self, string, errors, energy)
        class(EnergySimpleUnit_t), intent(in) :: self
        type(VARYING_STRING), intent(in) :: string
        type(ErrorList_t), intent(out) :: errors
        type(Energy_t), intent(out) :: energy

        type(ParseResult_t) :: parse_result

        parse_result = parseWith(theParser, string)
        if (parse_result%ok) then
            select type (the_number => parse_result%parsed)
            type is (ParsedRational_t)
                energy = the_number%value_.unit.self
            end select
        else
            call errors%appendError(Fatal( &
                    PARSE_ERROR, &
                    Module_("Energy_m"), &
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
        type(EnergySimpleUnit_t), intent(out) :: unit

        type(ErrorList_t) :: errors_

        call fromString(var_str(string), PROVIDED_UNITS, errors_, unit)
        call errors%appendErrors( &
                errors_, &
                Module_("Energy_m"), &
                Procedure_("simpleUnitFromStringC"))
    end subroutine simpleUnitFromStringC

    pure subroutine simpleUnitFromStringS(string, errors, unit)
        type(VARYING_STRING), intent(in) :: string
        type(ErrorList_t), intent(out) :: errors
        type(EnergySimpleUnit_t), intent(out) :: unit

        type(ErrorList_t) :: errors_

        call fromString(string, PROVIDED_UNITS, errors_, unit)
        call errors%appendErrors( &
                errors_, &
                Module_("Energy_m"), &
                Procedure_("simpleUnitFromStringS"))
    end subroutine simpleUnitFromStringS

    pure subroutine simpleUnitFromStringWithUnitsC(string, units, errors, unit)
        character(len=*), intent(in) :: string
        type(EnergySimpleUnit_t), intent(in) :: units(:)
        type(ErrorList_t), intent(out) :: errors
        type(EnergySimpleUnit_t), intent(out) :: unit

        type(ErrorList_t) :: errors_

        call fromString(var_str(string), units, errors_, unit)
        call errors%appendErrors( &
                errors_, &
                Module_("Energy_m"), &
                Procedure_("simpleUnitFromStringWithUnitsC"))
    end subroutine simpleUnitFromStringWithUnitsC

    pure subroutine simpleUnitFromStringWithUnitsS(string, units, errors, unit)
        type(VARYING_STRING), intent(in) :: string
        type(EnergySimpleUnit_t), intent(in) :: units(:)
        type(ErrorList_t), intent(out) :: errors
        type(EnergySimpleUnit_t), intent(out) :: unit

        type(ErrorList_t) :: errors_
        integer :: which_unit

        call selectUnit(string, units, errors, which_unit)
        if (errors_%hasAny()) then
            call errors%appendErrors( &
                    errors_, &
                    Module_("Energy_m"), &
                    Procedure_("simpleUnitFromStringWithUnitsS"))
        else
            unit = units(which_unit)
        end if
    end subroutine simpleUnitFromStringWithUnitsS

    pure subroutine selectUnit(string, units, errors, index)
        type(VARYING_STRING), intent(in) :: string
        class(EnergyUnit_t), intent(in) :: units(:)
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
                Module_("Energy_m"), &
                Procedure_("selectUnit"), &
                '"' // string // '", known units: [' // join(unit_strings, ', ') // ']'))
    end subroutine selectUnit
end module Energy_m
