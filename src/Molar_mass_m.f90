module Molar_mass_m
    use Conversion_factors_m, only: GRAMS_PER_MOL_PER_KILOGRAMS_PER_MOL
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

    type, public :: MolarMass_t
        double precision :: kilograms_per_mol
    contains
        private
        procedure :: toUnits
        generic, public :: operator(.in.) => toUnits
        procedure, pass(molar_mass) :: doubleTimesMolarMass
        procedure, pass(molar_mass) :: integerTimesMolarMass
        procedure, pass(molar_mass) :: molarMassTimesDouble
        procedure, pass(molar_mass) :: molarMassTimesInteger
        generic, public :: operator(*) => &
                doubleTimesMolarMass, &
                integerTimesMolarMass, &
                molarMassTimesDouble, &
                molarMassTimesInteger
        procedure :: molarMassDividedByDouble
        procedure :: molarMassDividedByInteger
        procedure, pass(numerator) :: molarMassDividedByMolarMass
        generic, public :: operator(/) => &
                molarMassDividedByDouble, &
                molarMassDividedByInteger, &
                molarMassDividedByMolarMass
        procedure :: molarMassPlusMolarMass
        generic, public :: operator(+) => molarMassPlusMolarMass
        procedure :: molarMassMinusMolarMass
        generic, public :: operator(-) => molarMassMinusMolarMass
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
    end type MolarMass_t

    type, public :: MolarMassUnit_t
        double precision :: conversion_factor
        character(len=20) :: symbol
        character(len=50) :: gnuplot_symbol
        character(len=100) :: latex_symbol
    contains
        procedure :: toString => unitToString
        procedure :: toGnuplotString => unitToGnuplotString
        procedure :: toLatexString => unitToLatexString
    end type MolarMassUnit_t

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
        module procedure sumMolarMass
    end interface sum

    type(MolarMassUnit_t), parameter, public :: GRAMS_PER_MOL = &
            MolarMassUnit_t( &
                    conversion_factor = GRAMS_PER_MOL_PER_KILOGRAMS_PER_MOL, &
                    symbol = "g/mol", &
                    gnuplot_symbol = "g/mol", &
                    latex_symbol = "\gram\per\mole")
    type(MolarMassUnit_t), parameter, public :: KILOGRAMS_PER_MOL = &
            MolarMassUnit_t( &
                    conversion_factor = 1.0d0, &
                    symbol = "kg/mol", &
                    gnuplot_symbol = "kg/mol", &
                    latex_symbol = "\kilo\gram\per\mole")

    type(MolarMassUnit_t), public :: DEFAULT_OUTPUT_UNITS = KILOGRAMS_PER_MOL

    type(MolarMassUnit_t), parameter, public :: PROVIDED_UNITS(*) = &
            [GRAMS_PER_MOL, KILOGRAMS_PER_MOL]

    public :: operator(.unit.), fromString, sum
contains
    pure subroutine fromStringBasicC(string, errors, molar_mass)
        character(len=*), intent(in) :: string
        type(ErrorList_t), intent(out) :: errors
        type(MolarMass_t), intent(out) :: molar_mass

        type(ErrorList_t) :: errors_

        call fromString( &
                var_str(string), PROVIDED_UNITS, errors_, molar_mass)
        call errors%appendErrors( &
                errors_, &
                Module_("Molar_mass_m"), &
                Procedure_("fromStringBasicC"))
    end subroutine fromStringBasicC

    pure subroutine fromStringBasicS(string, errors, molar_mass)
        type(VARYING_STRING), intent(in) :: string
        type(ErrorList_t), intent(out) :: errors
        type(MolarMass_t), intent(out) :: molar_mass

        type(ErrorList_t) :: errors_

        call fromString( &
                string, PROVIDED_UNITS, errors_, molar_mass)
        call errors%appendErrors( &
                errors_, &
                Module_("Molar_mass_m"), &
                Procedure_("fromStringBasicS"))
    end subroutine fromStringBasicS

    pure subroutine fromStringWithUnitsC(string, units, errors, molar_mass)
        character(len=*), intent(in) :: string
        type(MolarMassUnit_t), intent(in) :: units(:)
        type(ErrorList_t), intent(out) :: errors
        type(MolarMass_t), intent(out) :: molar_mass

        type(ErrorList_t) :: errors_

        call fromString( &
                var_str(string), units, errors_, molar_mass)
        call errors%appendErrors( &
                errors_, &
                Module_("Molar_mass_m"), &
                Procedure_("fromStringWithUnitsC"))
    end subroutine fromStringWithUnitsC

    pure subroutine fromStringWithUnitsS(string, units, errors, molar_mass)
        type(VARYING_STRING), intent(in) :: string
        type(MolarMassUnit_t), intent(in) :: units(:)
        type(ErrorList_t), intent(out) :: errors
        type(MolarMass_t), intent(out) :: molar_mass

        double precision :: number
        character(len=100) :: number_chars
        type(VARYING_STRING) :: number_string
        integer :: status
        type(VARYING_STRING) :: symbol
        type(MolarMassUnit_t) :: unit
        type(ErrorList_t) :: unit_errors

        molar_mass%kilograms_per_mol = 0.0d0
        symbol = string
        call split(symbol, number_string, " ")
        if (len(symbol) == 0) then
            call errors%appendError(Fatal( &
                    PARSE_ERROR, &
                    Module_("Molar_mass_m"), &
                    Procedure_("fromStringWithUnitsS"), &
                    'No unit symbol found in string "' // string // '"'))
            return
        end if
        number_chars = number_string
        read(number_chars, *, iostat=status) number
        if (status /= 0) then
            call errors%appendError(Fatal( &
                    PARSE_ERROR, &
                    Module_("Molar_mass_m"), &
                    Procedure_("fromStringWithUnitsS"), &
                    'Error parsing number from string "' // number_string // '"'))
        end if
        call fromString(symbol, units, unit_errors, unit)
        molar_mass = number.unit.unit
        call errors%appendErrors( &
                unit_errors, &
                Module_("Molar_mass_m"), &
                Procedure_("fromStringWithUnitsS"))
    end subroutine fromStringWithUnitsS

    elemental function fromUnits(value_, units) result(molar_mass)
        double precision, intent(in) :: value_
        type(MolarMassUnit_t), intent(in) :: units
        type(MolarMass_t) :: molar_mass

        molar_mass%kilograms_per_mol = value_ / units%conversion_factor
    end function fromUnits

    elemental function toUnits(self, units) result(molar_mass)
        class(MolarMass_t), intent(in) :: self
        class(MolarMassUnit_t), intent(in) :: units
        double precision :: molar_mass

        molar_mass = self%kilograms_per_mol * units%conversion_factor
    end function toUnits

    elemental function doubleTimesMolarMass( &
            multiplier, molar_mass) result(new_molar_mass)
        double precision, intent(in) :: multiplier
        class(MolarMass_t), intent(in) :: molar_mass
        type(MolarMass_t) :: new_molar_mass

        new_molar_mass%kilograms_per_mol = &
                multiplier * molar_mass%kilograms_per_mol
    end function doubleTimesMolarMass

    elemental function integerTimesMolarMass( &
            multiplier, molar_mass) result(new_molar_mass)
        integer, intent(in) :: multiplier
        class(MolarMass_t), intent(in) :: molar_mass
        type(MolarMass_t) :: new_molar_mass

        new_molar_mass%kilograms_per_mol = &
                dble(multiplier) * molar_mass%kilograms_per_mol
    end function integerTimesMolarMass

    elemental function molarMassTimesDouble( &
            molar_mass, multiplier) result(new_molar_mass)
        class(MolarMass_t), intent(in) :: molar_mass
        double precision, intent(in) :: multiplier
        type(MolarMass_t) :: new_molar_mass

        new_molar_mass%kilograms_per_mol = &
                molar_mass%kilograms_per_mol * multiplier
    end function molarMassTimesDouble

    elemental function molarMassTimesInteger( &
            molar_mass, multiplier) result(new_molar_mass)
        class(MolarMass_t), intent(in) :: molar_mass
        integer, intent(in) :: multiplier
        type(MolarMass_t) :: new_molar_mass

        new_molar_mass%kilograms_per_mol = &
                molar_mass%kilograms_per_mol * dble(multiplier)
    end function molarMassTimesInteger

    elemental function molarMassDividedByDouble( &
            molar_mass, divisor) result(new_molar_mass)
        class(MolarMass_t), intent(in) :: molar_mass
        double precision, intent(in) :: divisor
        type(MolarMass_t) :: new_molar_mass

        new_molar_mass%kilograms_per_mol = &
                molar_mass%kilograms_per_mol / divisor
    end function molarMassDividedByDouble

    elemental function molarMassDividedByInteger( &
            molar_mass, divisor) result(new_molar_mass)
        class(MolarMass_t), intent(in) :: molar_mass
        integer, intent(in) :: divisor
        type(MolarMass_t) :: new_molar_mass

        new_molar_mass%kilograms_per_mol = &
                molar_mass%kilograms_per_mol / dble(divisor)
    end function molarMassDividedByInteger

    elemental function molarMassDividedByMolarMass( &
            numerator, denomenator) result(ratio)
        class(MolarMass_t), intent(in) :: numerator
        class(MolarMass_t), intent(in) :: denomenator
        double precision :: ratio

        ratio = numerator%kilograms_per_mol / denomenator%kilograms_per_mol
    end function molarMassDividedByMolarMass

    elemental function molarMassPlusMolarMass( &
            molar_mass1, molar_mass2) result(new_molar_mass)
        class(MolarMass_t), intent(in) :: molar_mass1
        class(MolarMass_t), intent(in) :: molar_mass2
        type(MolarMass_t) :: new_molar_mass

        new_molar_mass%kilograms_per_mol = &
                molar_mass1%kilograms_per_mol + molar_mass2%kilograms_per_mol
    end function molarMassPlusMolarMass

    elemental function molarMassMinusMolarMass( &
            molar_mass1, molar_mass2) result(new_molar_mass)
        class(MolarMass_t), intent(in) :: molar_mass1
        class(MolarMass_t), intent(in) :: molar_mass2
        type(MolarMass_t) :: new_molar_mass

        new_molar_mass%kilograms_per_mol = &
                molar_mass1%kilograms_per_mol - molar_mass2%kilograms_per_mol
    end function molarMassMinusMolarMass

    pure function sumMolarMass(molar_masss)
        type(MolarMass_t), intent(in) :: molar_masss(:)
        type(MolarMass_t) :: sumMolarMass

        sumMolarMass%kilograms_per_mol = sum(molar_masss%kilograms_per_mol)
    end function sumMolarMass

    elemental function greaterThan(lhs, rhs)
        class(MolarMass_t), intent(in) :: lhs
        class(MolarMass_t), intent(in) :: rhs
        logical :: greaterThan

        greaterThan = lhs%kilograms_per_mol > rhs%kilograms_per_mol
    end function greaterThan

    elemental function lessThan(lhs,rhs)
        class(MolarMass_t), intent(in) :: lhs
        class(MolarMass_t), intent(in) :: rhs
        logical :: lessThan

        lessThan = lhs%kilograms_per_mol < rhs%kilograms_per_mol
    end function lessThan

    elemental function greaterThanOrEqual(lhs, rhs)
        class(MolarMass_t), intent(in) :: lhs
        class(MolarMass_t), intent(in) :: rhs
        logical :: greaterThanOrEqual

        greaterThanOrEqual = lhs%kilograms_per_mol >= rhs%kilograms_per_mol
    end function greaterThanOrEqual

    elemental function lessThanOrEqual(lhs, rhs)
        class(MolarMass_t), intent(in) :: lhs
        class(MolarMass_t), intent(in) :: rhs
        logical :: lessThanOrEqual

        lessThanOrEqual = lhs%kilograms_per_mol <= rhs%kilograms_per_mol
    end function lessThanOrEqual

    elemental function equal_(lhs,rhs)
        class(MolarMass_t), intent(in) :: lhs
        class(MolarMass_t), intent(in) :: rhs
        logical :: equal_

        equal_ = lhs%kilograms_per_mol .safeEq. rhs%kilograms_per_mol
    end function equal_

    elemental function equalWithinAbsolute(lhs, rhs, within)
        class(MolarMass_t), intent(in) :: lhs
        class(MolarMass_t), intent(in) :: rhs
        class(MolarMass_t), intent(in) :: within
        logical :: equalWithinAbsolute

        equalWithinAbsolute = equalWithinAbsolute_( &
                lhs%kilograms_per_mol, rhs%kilograms_per_mol, within%kilograms_per_mol)
    end function equalWithinAbsolute

    elemental function equalWithinRelative(lhs, rhs, within)
        class(MolarMass_t), intent(in) :: lhs
        class(MolarMass_t), intent(in) :: rhs
        double precision, intent(in) :: within
        logical :: equalWithinRelative

        equalWithinRelative = equalWithinRelative_( &
                lhs%kilograms_per_mol, rhs%kilograms_per_mol, within)
    end function equalWithinRelative

    elemental function notEqual(lhs, rhs)
        class(MolarMass_t), intent(in) :: lhs
        class(MolarMass_t), intent(in) :: rhs
        logical :: notEqual

        notEqual = .not. lhs == rhs
    end function notEqual

    elemental function toStringFullPrecision(self) result(string)
        class(MolarMass_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = self%toStringIn(DEFAULT_OUTPUT_UNITS)
    end function toStringFullPrecision

    elemental function toStringWithPrecision(self, significant_digits) result(string)
        class(MolarMass_t), intent(in) :: self
        integer, intent(in) :: significant_digits
        type(VARYING_STRING) :: string

        string = self%toStringIn(DEFAULT_OUTPUT_UNITS, significant_digits)
    end function toStringWithPrecision

    elemental function toStringInFullPrecision(self, units) result(string)
        class(MolarMass_t), intent(in) :: self
        class(MolarMassUnit_t), intent(in) :: units
        type(VARYING_STRING) :: string

        string = toString(self.in.units) // " " // units%toString()
    end function toStringInFullPrecision

    elemental function toStringInWithPrecision( &
            self, units, significant_digits) result(string)
        class(MolarMass_t), intent(in) :: self
        class(MolarMassUnit_t), intent(in) :: units
        integer, intent(in) :: significant_digits
        type(VARYING_STRING) :: string

        string = &
                toString(self.in.units, significant_digits) &
                // " " // units%toString()
    end function toStringInWithPrecision

    elemental function toGnuplotStringFullPrecision(self) result(string)
        class(MolarMass_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = self%toGnuplotStringIn(DEFAULT_OUTPUT_UNITS)
    end function toGnuplotStringFullPrecision

    elemental function toGnuplotStringWithPrecision( &
            self, significant_digits) result(string)
        class(MolarMass_t), intent(in) :: self
        integer, intent(in) :: significant_digits
        type(VARYING_STRING) :: string

        string = self%toGnuplotStringIn( &
                DEFAULT_OUTPUT_UNITS, significant_digits)
    end function toGnuplotStringWithPrecision

    elemental function toGnuplotStringInFullPrecision(self, units) result(string)
        class(MolarMass_t), intent(in) :: self
        class(MolarMassUnit_t), intent(in) :: units
        type(VARYING_STRING) :: string

        string = toString(self.in.units) // " " // units%toGnuplotString()
    end function toGnuplotStringInFullPrecision

    elemental function toGnuplotStringInWithPrecision( &
            self, units, significant_digits) result(string)
        class(MolarMass_t), intent(in) :: self
        class(MolarMassUnit_t), intent(in) :: units
        integer, intent(in) :: significant_digits
        type(VARYING_STRING) :: string

        string = &
                toString(self.in.units, significant_digits) &
                // " " // units%toGnuplotString()
    end function toGnuplotStringInWithPrecision

    elemental function toLatexStringFullPrecision(self) result(string)
        class(MolarMass_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = self%toLatexStringIn(DEFAULT_OUTPUT_UNITS)
    end function toLatexStringFullPrecision

    elemental function toLatexStringWithPrecision(self, significant_digits) result(string)
        class(MolarMass_t), intent(in) :: self
        integer, intent(in) :: significant_digits
        type(VARYING_STRING) :: string

        string = self%toLatexStringIn(DEFAULT_OUTPUT_UNITS, significant_digits)
    end function toLatexStringWithPrecision

    elemental function toLatexStringInFullPrecision(self, units) result(string)
        class(MolarMass_t), intent(in) :: self
        class(MolarMassUnit_t), intent(in) :: units
        type(VARYING_STRING) :: string

        string = wrapInLatexQuantity( &
                toString(self.in.units), units%toLatexString())
    end function toLatexStringInFullPrecision

    elemental function toLatexStringInWithPrecision( &
            self, units, significant_digits) result(string)
        class(MolarMass_t), intent(in) :: self
        class(MolarMassUnit_t), intent(in) :: units
        integer, intent(in) :: significant_digits
        type(VARYING_STRING) :: string

        string = wrapInLatexQuantity( &
                toString(self.in.units, significant_digits), &
                units%toLatexString())
    end function toLatexStringInWithPrecision

    pure subroutine unitFromStringBasicC(string, errors, unit)
        character(len=*), intent(in) :: string
        type(ErrorList_t), intent(out) :: errors
        type(MolarMassUnit_t), intent(out) :: unit

        type(ErrorList_t) :: errors_

        call fromString( &
                var_str(string), PROVIDED_UNITS, errors_, unit)
        call errors%appendErrors( &
                errors_, &
                Module_("Molar_mass_m"), &
                Procedure_("unitFromStringBasicC"))
    end subroutine unitFromStringBasicC

    pure subroutine unitFromStringBasicS(string, errors, unit)
        type(VARYING_STRING), intent(in) :: string
        type(ErrorList_t), intent(out) :: errors
        type(MolarMassUnit_t), intent(out) :: unit

        type(ErrorList_t) :: errors_

        call fromString( &
                string, PROVIDED_UNITS, errors_, unit)
        call errors%appendErrors( &
                errors_, &
                Module_("Molar_mass_m"), &
                Procedure_("unitFromStringBasicS"))
    end subroutine unitFromStringBasicS

    pure subroutine unitFromStringWithUnitsC(string, units, errors, unit)
        character(len=*), intent(in) :: string
        type(MolarMassUnit_t), intent(in) :: units(:)
        type(ErrorList_t), intent(out) :: errors
        type(MolarMassUnit_t), intent(out) :: unit

        type(ErrorList_t) :: errors_

        call fromString(var_str(string), units, errors_, unit)
        call errors%appendErrors( &
                errors_, &
                Module_("Molar_mass_m"), &
                Procedure_("unitFromStringWithUnitsC"))
    end subroutine unitFromStringWithUnitsC

    pure subroutine unitFromStringWithUnitsS(string, units, errors, unit)
        type(VARYING_STRING), intent(in) :: string
        type(MolarMassUnit_t), intent(in) :: units(:)
        type(ErrorList_t), intent(out) :: errors
        type(MolarMassUnit_t), intent(out) :: unit

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
                    Module_("Molar_mass_m"), &
                    Procedure_("unitFromStringWithUnitsS"), &
                    '"' // string // '", known units: [' // join(unit_strings, ', ') // ']' ))
        end if
    end subroutine unitFromStringWithUnitsS

    elemental function unitToString(self) result(string)
        class(MolarMassUnit_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = trim(self%symbol)
    end function unitToString

    elemental function unitToGnuplotString(self) result(string)
        class(MolarMassUnit_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = trim(self%gnuplot_symbol)
    end function unitToGnuplotString

    elemental function unitToLatexString(self) result(string)
        class(MolarMassUnit_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = trim(self%latex_symbol)
    end function unitToLatexString
end module Molar_mass_m
