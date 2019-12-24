module Energy_m
    use Conversion_factors_m, only: &
            BTU_PER_JOULE, &
            CALORIES_PER_JOULE, &
            KILOJOULES_PER_JOULE, &
            MEGABTU_PER_JOULE, &
            MEGAWATT_DAYS_PER_JOULE
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
    end type Energy_t

    type, public :: EnergyUnit_t
        double precision :: conversion_factor
        character(len=10) :: symbol
        character(len=50) :: gnuplot_symbol
        character(len=100) :: latex_symbol
    contains
        procedure :: toString => unitToString
        procedure :: toGnuplotString => unitToGnuplotString
        procedure :: toLatexString => unitToLatexString
    end type EnergyUnit_t

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
        module procedure sumEnergy
    end interface sum

    type(EnergyUnit_t), parameter, public :: BTU = &
            EnergyUnit_t( &
                    conversion_factor = BTU_PER_JOULE, &
                    symbol = "BTU", &
                    gnuplot_symbol = "BTU", &
                    latex_symbol = "\btu")
    type(EnergyUnit_t), parameter, public :: CALORIES = &
            EnergyUnit_t( &
                    conversion_factor = CALORIES_PER_JOULE, &
                    symbol = "cal", &
                    gnuplot_symbol = "cal", &
                    latex_symbol = "\calorie")
    type(EnergyUnit_t), parameter, public :: JOULES = &
            EnergyUnit_t( &
                    conversion_factor = 1.0d0, &
                    symbol = "J", &
                    gnuplot_symbol = "J", &
                    latex_symbol = "\joule")
    type(EnergyUnit_t), parameter, public :: KILOJOULES = &
            EnergyUnit_t( &
                    conversion_factor = KILOJOULES_PER_JOULE, &
                    symbol = "kJ", &
                    gnuplot_symbol = "kJ", &
                    latex_symbol = "\kilo\joule")
    type(EnergyUnit_t), parameter, public :: MEGABTU = &
            EnergyUnit_t( &
                    conversion_factor = MEGABTU_PER_JOULE, &
                    symbol = "MBTU", &
                    gnuplot_symbol = "MBTU", &
                    latex_symbol = "\mega\btu")
    type(EnergyUnit_t), parameter, public :: MEGAWATT_DAYS = &
            EnergyUnit_t( &
                    conversion_factor = MEGAWATT_DAYS_PER_JOULE, &
                    symbol = "MW d", &
                    gnuplot_symbol = "MW d", &
                    latex_symbol = "\mega\watt\day")

    type(EnergyUnit_t), public :: DEFAULT_OUTPUT_UNITS = JOULES

    type(EnergyUnit_t), parameter, public :: PROVIDED_UNITS(*) = &
            [BTU, CALORIES, JOULES, KILOJOULES, MEGABTU, MEGAWATT_DAYS]

    public :: operator(.unit.), fromString, sum
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
        type(EnergyUnit_t), intent(in) :: units(:)
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
        type(EnergyUnit_t), intent(in) :: units(:)
        type(ErrorList_t), intent(out) :: errors
        type(Energy_t), intent(out) :: energy

        double precision :: number
        character(len=100) :: number_chars
        type(VARYING_STRING) :: number_string
        integer :: status
        type(VARYING_STRING) :: symbol
        type(EnergyUnit_t) :: unit
        type(ErrorList_t) :: unit_errors

        energy%joules = 0.0d0
        symbol = string
        call split(symbol, number_string, " ")
        if (len(symbol) == 0) then
            call errors%appendError(Fatal( &
                    PARSE_ERROR, &
                    Module_("Energy_m"), &
                    Procedure_("fromStringWithUnitsS"), &
                    'No unit symbol found in string "' // string // '"'))
            return
        end if
        number_chars = number_string
        read(number_chars, *, iostat=status) number
        if (status /= 0) then
            call errors%appendError(Fatal( &
                    PARSE_ERROR, &
                    Module_("Energy_m"), &
                    Procedure_("fromStringWithUnitsS"), &
                    'Error parsing number from string "' // number_string // '"'))
        end if
        call fromString(symbol, units, unit_errors, unit)
        energy = number.unit.unit
        call errors%appendErrors( &
                unit_errors, &
                Module_("Energy_m"), &
                Procedure_("fromStringWithUnitsS"))
    end subroutine fromStringWithUnitsS

    elemental function fromUnits(value_, units) result(energy)
        double precision, intent(in) :: value_
        type(EnergyUnit_t), intent(in) :: units
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

        string = toString(self.in.units) // " " // units%toString()
    end function toStringInFullPrecision

    elemental function toStringInWithPrecision( &
            self, units, significant_digits) result(string)
        class(Energy_t), intent(in) :: self
        class(EnergyUnit_t), intent(in) :: units
        integer, intent(in) :: significant_digits
        type(VARYING_STRING) :: string

        string = &
                toString(self.in.units, significant_digits) &
                // " " // units%toString()
    end function toStringInWithPrecision

    elemental function toGnuplotStringFullPrecision(self) result(string)
        class(Energy_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = self%toGnuplotStringIn(DEFAULT_OUTPUT_UNITS)
    end function toGnuplotStringFullPrecision

    elemental function toGnuplotStringWithPrecision( &
            self, significant_digits) result(string)
        class(Energy_t), intent(in) :: self
        integer, intent(in) :: significant_digits
        type(VARYING_STRING) :: string

        string = self%toGnuplotStringIn( &
                DEFAULT_OUTPUT_UNITS, significant_digits)
    end function toGnuplotStringWithPrecision

    elemental function toGnuplotStringInFullPrecision(self, units) result(string)
        class(Energy_t), intent(in) :: self
        class(EnergyUnit_t), intent(in) :: units
        type(VARYING_STRING) :: string

        string = toString(self.in.units) // " " // units%toGnuplotString()
    end function toGnuplotStringInFullPrecision

    elemental function toGnuplotStringInWithPrecision( &
            self, units, significant_digits) result(string)
        class(Energy_t), intent(in) :: self
        class(EnergyUnit_t), intent(in) :: units
        integer, intent(in) :: significant_digits
        type(VARYING_STRING) :: string

        string = &
                toString(self.in.units, significant_digits) &
                // " " // units%toGnuplotString()
    end function toGnuplotStringInWithPrecision

    elemental function toLatexStringFullPrecision(self) result(string)
        class(Energy_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = self%toLatexStringIn(DEFAULT_OUTPUT_UNITS)
    end function toLatexStringFullPrecision

    elemental function toLatexStringWithPrecision(self, significant_digits) result(string)
        class(Energy_t), intent(in) :: self
        integer, intent(in) :: significant_digits
        type(VARYING_STRING) :: string

        string = self%toLatexStringIn(DEFAULT_OUTPUT_UNITS, significant_digits)
    end function toLatexStringWithPrecision

    elemental function toLatexStringInFullPrecision(self, units) result(string)
        class(Energy_t), intent(in) :: self
        class(EnergyUnit_t), intent(in) :: units
        type(VARYING_STRING) :: string

        string = wrapInLatexQuantity( &
                toString(self.in.units), units%toLatexString())
    end function toLatexStringInFullPrecision

    elemental function toLatexStringInWithPrecision( &
            self, units, significant_digits) result(string)
        class(Energy_t), intent(in) :: self
        class(EnergyUnit_t), intent(in) :: units
        integer, intent(in) :: significant_digits
        type(VARYING_STRING) :: string

        string = wrapInLatexQuantity( &
                toString(self.in.units, significant_digits), &
                units%toLatexString())
    end function toLatexStringInWithPrecision

    pure subroutine unitFromStringBasicC(string, errors, unit)
        character(len=*), intent(in) :: string
        type(ErrorList_t), intent(out) :: errors
        type(EnergyUnit_t), intent(out) :: unit

        type(ErrorList_t) :: errors_

        call fromString( &
                var_str(string), PROVIDED_UNITS, errors_, unit)
        call errors%appendErrors( &
                errors_, &
                Module_("Energy_m"), &
                Procedure_("unitFromStringBasicC"))
    end subroutine unitFromStringBasicC

    pure subroutine unitFromStringBasicS(string, errors, unit)
        type(VARYING_STRING), intent(in) :: string
        type(ErrorList_t), intent(out) :: errors
        type(EnergyUnit_t), intent(out) :: unit

        type(ErrorList_t) :: errors_

        call fromString( &
                string, PROVIDED_UNITS, errors_, unit)
        call errors%appendErrors( &
                errors_, &
                Module_("Energy_m"), &
                Procedure_("unitFromStringBasicS"))
    end subroutine unitFromStringBasicS

    pure subroutine unitFromStringWithUnitsC(string, units, errors, unit)
        character(len=*), intent(in) :: string
        type(EnergyUnit_t), intent(in) :: units(:)
        type(ErrorList_t), intent(out) :: errors
        type(EnergyUnit_t), intent(out) :: unit

        type(ErrorList_t) :: errors_

        call fromString(var_str(string), units, errors_, unit)
        call errors%appendErrors( &
                errors_, &
                Module_("Energy_m"), &
                Procedure_("unitFromStringWithUnitsC"))
    end subroutine unitFromStringWithUnitsC

    pure subroutine unitFromStringWithUnitsS(string, units, errors, unit)
        type(VARYING_STRING), intent(in) :: string
        type(EnergyUnit_t), intent(in) :: units(:)
        type(ErrorList_t), intent(out) :: errors
        type(EnergyUnit_t), intent(out) :: unit

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
                    Module_("Energy_m"), &
                    Procedure_("unitFromStringWithUnitsS"), &
                    '"' // string // '", known units: [' // join(unit_strings, ', ') // ']' ))
        end if
    end subroutine unitFromStringWithUnitsS

    elemental function unitToString(self) result(string)
        class(EnergyUnit_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = trim(self%symbol)
    end function unitToString

    elemental function unitToGnuplotString(self) result(string)
        class(EnergyUnit_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = trim(self%gnuplot_symbol)
    end function unitToGnuplotString

    elemental function unitToLatexString(self) result(string)
        class(EnergyUnit_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = trim(self%latex_symbol)
    end function unitToLatexString
end module Energy_m
