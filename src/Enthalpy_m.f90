module Enthalpy_m
    use Conversion_factors_m, only: KILOJOULES_PER_KILOGRAM_PER_JOULES_PER_KILOGRAM
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

    type, public :: Enthalpy_t
        double precision :: joules_per_kilogram
    contains
        private
        procedure :: toUnits
        generic, public :: operator(.in.) => toUnits
        procedure, pass(enthalpy) :: doubleTimesEnthalpy
        procedure, pass(enthalpy) :: integerTimesEnthalpy
        procedure, pass(enthalpy) :: enthalpyTimesDouble
        procedure, pass(enthalpy) :: enthalpyTimesInteger
        generic, public :: operator(*) => &
                doubleTimesEnthalpy, &
                integerTimesEnthalpy, &
                enthalpyTimesDouble, &
                enthalpyTimesInteger
        procedure :: enthalpyDividedByDouble
        procedure :: enthalpyDividedByInteger
        procedure, pass(numerator) :: enthalpyDividedByEnthalpy
        generic, public :: operator(/) => &
                enthalpyDividedByDouble, &
                enthalpyDividedByInteger, &
                enthalpyDividedByEnthalpy
        procedure :: enthalpyPlusEnthalpy
        generic, public :: operator(+) => enthalpyPlusEnthalpy
        procedure :: enthalpyMinusEnthalpy
        generic, public :: operator(-) => enthalpyMinusEnthalpy
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
    end type Enthalpy_t

    type, public :: EnthalpyUnit_t
        double precision :: conversion_factor
        character(len=10) :: symbol
        character(len=50) :: gnuplot_symbol
        character(len=100) :: latex_symbol
    contains
        procedure :: toString => unitToString
        procedure :: toGnuplotString => unitToGnuplotString
        procedure :: toLatexString => unitToLatexString
    end type EnthalpyUnit_t

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
        module procedure sumEnthalpy
    end interface sum

    type(EnthalpyUnit_t), parameter, public :: KILOJOULES_PER_KILOGRAM = &
            EnthalpyUnit_t( &
                    conversion_factor = KILOJOULES_PER_KILOGRAM_PER_JOULES_PER_KILOGRAM, &
                    symbol = "kJ/kg", &
                    gnuplot_symbol = "kJ/kg", &
                    latex_symbol = "\kilo\joule\per\kilo\gram")
    type(EnthalpyUnit_t), parameter, public :: JOULES_PER_KILOGRAM = &
            EnthalpyUnit_t( &
                    conversion_factor = 1.0d0, &
                    symbol = "J/kg", &
                    gnuplot_symbol = "J/kg", &
                    latex_symbol = "\joule\per\kilo\gram")

    type(EnthalpyUnit_t), public :: DEFAULT_OUTPUT_UNITS = JOULES_PER_KILOGRAM

    type(EnthalpyUnit_t), parameter, public :: PROVIDED_UNITS(*) = &
            [KILOJOULES_PER_KILOGRAM, JOULES_PER_KILOGRAM]

    public :: operator(.unit.), fromString, sum
contains
    pure subroutine fromStringBasicC(string, errors, enthalpy)
        character(len=*), intent(in) :: string
        type(ErrorList_t), intent(out) :: errors
        type(Enthalpy_t), intent(out) :: enthalpy

        type(ErrorList_t) :: errors_

        call fromString( &
                var_str(string), PROVIDED_UNITS, errors_, enthalpy)
        call errors%appendErrors( &
                errors_, &
                Module_("Enthalpy_m"), &
                Procedure_("fromStringBasicC"))
    end subroutine fromStringBasicC

    pure subroutine fromStringBasicS(string, errors, enthalpy)
        type(VARYING_STRING), intent(in) :: string
        type(ErrorList_t), intent(out) :: errors
        type(Enthalpy_t), intent(out) :: enthalpy

        type(ErrorList_t) :: errors_

        call fromString( &
                string, PROVIDED_UNITS, errors_, enthalpy)
        call errors%appendErrors( &
                errors_, &
                Module_("Enthalpy_m"), &
                Procedure_("fromStringBasicS"))
    end subroutine fromStringBasicS

    pure subroutine fromStringWithUnitsC(string, units, errors, enthalpy)
        character(len=*), intent(in) :: string
        type(EnthalpyUnit_t), intent(in) :: units(:)
        type(ErrorList_t), intent(out) :: errors
        type(Enthalpy_t), intent(out) :: enthalpy

        type(ErrorList_t) :: errors_

        call fromString( &
                var_str(string), units, errors_, enthalpy)
        call errors%appendErrors( &
                errors_, &
                Module_("Enthalpy_m"), &
                Procedure_("fromStringWithUnitsC"))
    end subroutine fromStringWithUnitsC

    pure subroutine fromStringWithUnitsS(string, units, errors, enthalpy)
        type(VARYING_STRING), intent(in) :: string
        type(EnthalpyUnit_t), intent(in) :: units(:)
        type(ErrorList_t), intent(out) :: errors
        type(Enthalpy_t), intent(out) :: enthalpy

        double precision :: number
        character(len=100) :: number_chars
        type(VARYING_STRING) :: number_string
        integer :: status
        type(VARYING_STRING) :: symbol
        type(EnthalpyUnit_t) :: unit
        type(ErrorList_t) :: unit_errors

        enthalpy%joules_per_kilogram = 0.0d0
        symbol = string
        call split(symbol, number_string, " ")
        if (len(symbol) == 0) then
            call errors%appendError(Fatal( &
                    PARSE_ERROR, &
                    Module_("Enthalpy_m"), &
                    Procedure_("fromStringWithUnitsS"), &
                    'No unit symbol found in string "' // string // '"'))
            return
        end if
        number_chars = number_string
        read(number_chars, *, iostat=status) number
        if (status /= 0) then
            call errors%appendError(Fatal( &
                    PARSE_ERROR, &
                    Module_("Enthalpy_m"), &
                    Procedure_("fromStringWithUnitsS"), &
                    'Error parsing number from string "' // number_string // '"'))
        end if
        call fromString(symbol, units, unit_errors, unit)
        enthalpy = number.unit.unit
        call errors%appendErrors( &
                unit_errors, &
                Module_("Enthalpy_m"), &
                Procedure_("fromStringWithUnitsS"))
    end subroutine fromStringWithUnitsS

    elemental function fromUnits(value_, units) result(enthalpy)
        double precision, intent(in) :: value_
        type(EnthalpyUnit_t), intent(in) :: units
        type(Enthalpy_t) :: enthalpy

        enthalpy%joules_per_kilogram = value_ / units%conversion_factor
    end function fromUnits

    elemental function toUnits(self, units) result(enthalpy)
        class(Enthalpy_t), intent(in) :: self
        class(EnthalpyUnit_t), intent(in) :: units
        double precision :: enthalpy

        enthalpy = self%joules_per_kilogram * units%conversion_factor
    end function toUnits

    elemental function doubleTimesEnthalpy( &
            multiplier, enthalpy) result(new_enthalpy)
        double precision, intent(in) :: multiplier
        class(Enthalpy_t), intent(in) :: enthalpy
        type(Enthalpy_t) :: new_enthalpy

        new_enthalpy%joules_per_kilogram = &
                multiplier * enthalpy%joules_per_kilogram
    end function doubleTimesEnthalpy

    elemental function integerTimesEnthalpy( &
            multiplier, enthalpy) result(new_enthalpy)
        integer, intent(in) :: multiplier
        class(Enthalpy_t), intent(in) :: enthalpy
        type(Enthalpy_t) :: new_enthalpy

        new_enthalpy%joules_per_kilogram = &
                dble(multiplier) * enthalpy%joules_per_kilogram
    end function integerTimesEnthalpy

    elemental function enthalpyTimesDouble( &
            enthalpy, multiplier) result(new_enthalpy)
        class(Enthalpy_t), intent(in) :: enthalpy
        double precision, intent(in) :: multiplier
        type(Enthalpy_t) :: new_enthalpy

        new_enthalpy%joules_per_kilogram = &
                enthalpy%joules_per_kilogram * multiplier
    end function enthalpyTimesDouble

    elemental function enthalpyTimesInteger( &
            enthalpy, multiplier) result(new_enthalpy)
        class(Enthalpy_t), intent(in) :: enthalpy
        integer, intent(in) :: multiplier
        type(Enthalpy_t) :: new_enthalpy

        new_enthalpy%joules_per_kilogram = &
                enthalpy%joules_per_kilogram * dble(multiplier)
    end function enthalpyTimesInteger

    elemental function enthalpyDividedByDouble( &
            enthalpy, divisor) result(new_enthalpy)
        class(Enthalpy_t), intent(in) :: enthalpy
        double precision, intent(in) :: divisor
        type(Enthalpy_t) :: new_enthalpy

        new_enthalpy%joules_per_kilogram = &
                enthalpy%joules_per_kilogram / divisor
    end function enthalpyDividedByDouble

    elemental function enthalpyDividedByInteger( &
            enthalpy, divisor) result(new_enthalpy)
        class(Enthalpy_t), intent(in) :: enthalpy
        integer, intent(in) :: divisor
        type(Enthalpy_t) :: new_enthalpy

        new_enthalpy%joules_per_kilogram = &
                enthalpy%joules_per_kilogram / dble(divisor)
    end function enthalpyDividedByInteger

    elemental function enthalpyDividedByEnthalpy( &
            numerator, denomenator) result(ratio)
        class(Enthalpy_t), intent(in) :: numerator
        class(Enthalpy_t), intent(in) :: denomenator
        double precision :: ratio

        ratio = numerator%joules_per_kilogram / denomenator%joules_per_kilogram
    end function enthalpyDividedByEnthalpy

    elemental function enthalpyPlusEnthalpy( &
            enthalpy1, enthalpy2) result(new_enthalpy)
        class(Enthalpy_t), intent(in) :: enthalpy1
        class(Enthalpy_t), intent(in) :: enthalpy2
        type(Enthalpy_t) :: new_enthalpy

        new_enthalpy%joules_per_kilogram = &
                enthalpy1%joules_per_kilogram + enthalpy2%joules_per_kilogram
    end function enthalpyPlusEnthalpy

    elemental function enthalpyMinusEnthalpy( &
            enthalpy1, enthalpy2) result(new_enthalpy)
        class(Enthalpy_t), intent(in) :: enthalpy1
        class(Enthalpy_t), intent(in) :: enthalpy2
        type(Enthalpy_t) :: new_enthalpy

        new_enthalpy%joules_per_kilogram = &
                enthalpy1%joules_per_kilogram - enthalpy2%joules_per_kilogram
    end function enthalpyMinusEnthalpy

    pure function sumEnthalpy(enthalpys)
        type(Enthalpy_t), intent(in) :: enthalpys(:)
        type(Enthalpy_t) :: sumEnthalpy

        sumEnthalpy%joules_per_kilogram = sum(enthalpys%joules_per_kilogram)
    end function sumEnthalpy

    elemental function greaterThan(lhs, rhs)
        class(Enthalpy_t), intent(in) :: lhs
        class(Enthalpy_t), intent(in) :: rhs
        logical :: greaterThan

        greaterThan = lhs%joules_per_kilogram > rhs%joules_per_kilogram
    end function greaterThan

    elemental function lessThan(lhs,rhs)
        class(Enthalpy_t), intent(in) :: lhs
        class(Enthalpy_t), intent(in) :: rhs
        logical :: lessThan

        lessThan = lhs%joules_per_kilogram < rhs%joules_per_kilogram
    end function lessThan

    elemental function greaterThanOrEqual(lhs, rhs)
        class(Enthalpy_t), intent(in) :: lhs
        class(Enthalpy_t), intent(in) :: rhs
        logical :: greaterThanOrEqual

        greaterThanOrEqual = lhs%joules_per_kilogram >= rhs%joules_per_kilogram
    end function greaterThanOrEqual

    elemental function lessThanOrEqual(lhs, rhs)
        class(Enthalpy_t), intent(in) :: lhs
        class(Enthalpy_t), intent(in) :: rhs
        logical :: lessThanOrEqual

        lessThanOrEqual = lhs%joules_per_kilogram <= rhs%joules_per_kilogram
    end function lessThanOrEqual

    elemental function equal_(lhs,rhs)
        class(Enthalpy_t), intent(in) :: lhs
        class(Enthalpy_t), intent(in) :: rhs
        logical :: equal_

        equal_ = lhs%joules_per_kilogram .safeEq. rhs%joules_per_kilogram
    end function equal_

    elemental function equalWithinAbsolute(lhs, rhs, within)
        class(Enthalpy_t), intent(in) :: lhs
        class(Enthalpy_t), intent(in) :: rhs
        class(Enthalpy_t), intent(in) :: within
        logical :: equalWithinAbsolute

        equalWithinAbsolute = equalWithinAbsolute_( &
                lhs%joules_per_kilogram, rhs%joules_per_kilogram, within%joules_per_kilogram)
    end function equalWithinAbsolute

    elemental function equalWithinRelative(lhs, rhs, within)
        class(Enthalpy_t), intent(in) :: lhs
        class(Enthalpy_t), intent(in) :: rhs
        double precision, intent(in) :: within
        logical :: equalWithinRelative

        equalWithinRelative = equalWithinRelative_( &
                lhs%joules_per_kilogram, rhs%joules_per_kilogram, within)
    end function equalWithinRelative

    elemental function notEqual(lhs, rhs)
        class(Enthalpy_t), intent(in) :: lhs
        class(Enthalpy_t), intent(in) :: rhs
        logical :: notEqual

        notEqual = .not. lhs == rhs
    end function notEqual

    elemental function toStringFullPrecision(self) result(string)
        class(Enthalpy_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = self%toStringIn(DEFAULT_OUTPUT_UNITS)
    end function toStringFullPrecision

    elemental function toStringWithPrecision(self, significant_digits) result(string)
        class(Enthalpy_t), intent(in) :: self
        integer, intent(in) :: significant_digits
        type(VARYING_STRING) :: string

        string = self%toStringIn(DEFAULT_OUTPUT_UNITS, significant_digits)
    end function toStringWithPrecision

    elemental function toStringInFullPrecision(self, units) result(string)
        class(Enthalpy_t), intent(in) :: self
        class(EnthalpyUnit_t), intent(in) :: units
        type(VARYING_STRING) :: string

        string = toString(self.in.units) // " " // units%toString()
    end function toStringInFullPrecision

    elemental function toStringInWithPrecision( &
            self, units, significant_digits) result(string)
        class(Enthalpy_t), intent(in) :: self
        class(EnthalpyUnit_t), intent(in) :: units
        integer, intent(in) :: significant_digits
        type(VARYING_STRING) :: string

        string = &
                toString(self.in.units, significant_digits) &
                // " " // units%toString()
    end function toStringInWithPrecision

    elemental function toGnuplotStringFullPrecision(self) result(string)
        class(Enthalpy_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = self%toGnuplotStringIn(DEFAULT_OUTPUT_UNITS)
    end function toGnuplotStringFullPrecision

    elemental function toGnuplotStringWithPrecision( &
            self, significant_digits) result(string)
        class(Enthalpy_t), intent(in) :: self
        integer, intent(in) :: significant_digits
        type(VARYING_STRING) :: string

        string = self%toGnuplotStringIn( &
                DEFAULT_OUTPUT_UNITS, significant_digits)
    end function toGnuplotStringWithPrecision

    elemental function toGnuplotStringInFullPrecision(self, units) result(string)
        class(Enthalpy_t), intent(in) :: self
        class(EnthalpyUnit_t), intent(in) :: units
        type(VARYING_STRING) :: string

        string = toString(self.in.units) // " " // units%toGnuplotString()
    end function toGnuplotStringInFullPrecision

    elemental function toGnuplotStringInWithPrecision( &
            self, units, significant_digits) result(string)
        class(Enthalpy_t), intent(in) :: self
        class(EnthalpyUnit_t), intent(in) :: units
        integer, intent(in) :: significant_digits
        type(VARYING_STRING) :: string

        string = &
                toString(self.in.units, significant_digits) &
                // " " // units%toGnuplotString()
    end function toGnuplotStringInWithPrecision

    elemental function toLatexStringFullPrecision(self) result(string)
        class(Enthalpy_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = self%toLatexStringIn(DEFAULT_OUTPUT_UNITS)
    end function toLatexStringFullPrecision

    elemental function toLatexStringWithPrecision(self, significant_digits) result(string)
        class(Enthalpy_t), intent(in) :: self
        integer, intent(in) :: significant_digits
        type(VARYING_STRING) :: string

        string = self%toLatexStringIn(DEFAULT_OUTPUT_UNITS, significant_digits)
    end function toLatexStringWithPrecision

    elemental function toLatexStringInFullPrecision(self, units) result(string)
        class(Enthalpy_t), intent(in) :: self
        class(EnthalpyUnit_t), intent(in) :: units
        type(VARYING_STRING) :: string

        string = wrapInLatexQuantity( &
                toString(self.in.units), units%toLatexString())
    end function toLatexStringInFullPrecision

    elemental function toLatexStringInWithPrecision( &
            self, units, significant_digits) result(string)
        class(Enthalpy_t), intent(in) :: self
        class(EnthalpyUnit_t), intent(in) :: units
        integer, intent(in) :: significant_digits
        type(VARYING_STRING) :: string

        string = wrapInLatexQuantity( &
                toString(self.in.units, significant_digits), &
                units%toLatexString())
    end function toLatexStringInWithPrecision

    pure subroutine unitFromStringBasicC(string, errors, unit)
        character(len=*), intent(in) :: string
        type(ErrorList_t), intent(out) :: errors
        type(EnthalpyUnit_t), intent(out) :: unit

        type(ErrorList_t) :: errors_

        call fromString( &
                var_str(string), PROVIDED_UNITS, errors_, unit)
        call errors%appendErrors( &
                errors_, &
                Module_("Enthalpy_m"), &
                Procedure_("unitFromStringBasicC"))
    end subroutine unitFromStringBasicC

    pure subroutine unitFromStringBasicS(string, errors, unit)
        type(VARYING_STRING), intent(in) :: string
        type(ErrorList_t), intent(out) :: errors
        type(EnthalpyUnit_t), intent(out) :: unit

        type(ErrorList_t) :: errors_

        call fromString( &
                string, PROVIDED_UNITS, errors_, unit)
        call errors%appendErrors( &
                errors_, &
                Module_("Enthalpy_m"), &
                Procedure_("unitFromStringBasicS"))
    end subroutine unitFromStringBasicS

    pure subroutine unitFromStringWithUnitsC(string, units, errors, unit)
        character(len=*), intent(in) :: string
        type(EnthalpyUnit_t), intent(in) :: units(:)
        type(ErrorList_t), intent(out) :: errors
        type(EnthalpyUnit_t), intent(out) :: unit

        type(ErrorList_t) :: errors_

        call fromString(var_str(string), units, errors_, unit)
        call errors%appendErrors( &
                errors_, &
                Module_("Enthalpy_m"), &
                Procedure_("unitFromStringWithUnitsC"))
    end subroutine unitFromStringWithUnitsC

    pure subroutine unitFromStringWithUnitsS(string, units, errors, unit)
        type(VARYING_STRING), intent(in) :: string
        type(EnthalpyUnit_t), intent(in) :: units(:)
        type(ErrorList_t), intent(out) :: errors
        type(EnthalpyUnit_t), intent(out) :: unit

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
                    Module_("Enthalpy_m"), &
                    Procedure_("unitFromStringWithUnitsS"), &
                    '"' // string // '", known units: [' // join(unit_strings, ', ') // ']' ))
        end if
    end subroutine unitFromStringWithUnitsS

    elemental function unitToString(self) result(string)
        class(EnthalpyUnit_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = trim(self%symbol)
    end function unitToString

    elemental function unitToGnuplotString(self) result(string)
        class(EnthalpyUnit_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = trim(self%gnuplot_symbol)
    end function unitToGnuplotString

    elemental function unitToLatexString(self) result(string)
        class(EnthalpyUnit_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = trim(self%latex_symbol)
    end function unitToLatexString
end module Enthalpy_m
