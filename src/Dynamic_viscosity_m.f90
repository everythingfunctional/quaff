module Dynamic_viscosity_m
    use Conversion_factors_m, only: MEGAPASCAL_SECONDS_PER_PASCAL_SECOND
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

    type, public :: DynamicViscosity_t
        double precision :: pascal_seconds
    contains
        private
        procedure :: toUnits
        generic, public :: operator(.in.) => toUnits
        procedure, pass(dynamic_viscosity) :: doubleTimesDynamicViscosity
        procedure, pass(dynamic_viscosity) :: integerTimesDynamicViscosity
        procedure, pass(dynamic_viscosity) :: dynamicViscosityTimesDouble
        procedure, pass(dynamic_viscosity) :: dynamicViscosityTimesInteger
        generic, public :: operator(*) => &
                doubleTimesDynamicViscosity, &
                integerTimesDynamicViscosity, &
                dynamicViscosityTimesDouble, &
                dynamicViscosityTimesInteger
        procedure :: dynamicViscosityDividedByDouble
        procedure :: dynamicViscosityDividedByInteger
        procedure, pass(numerator) :: dynamicViscosityDividedByDynamicViscosity
        generic, public :: operator(/) => &
                dynamicViscosityDividedByDouble, &
                dynamicViscosityDividedByInteger, &
                dynamicViscosityDividedByDynamicViscosity
        procedure :: dynamicViscosityPlusDynamicViscosity
        generic, public :: operator(+) => dynamicViscosityPlusDynamicViscosity
        procedure :: dynamicViscosityMinusDynamicViscosity
        generic, public :: operator(-) => dynamicViscosityMinusDynamicViscosity
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
    end type DynamicViscosity_t

    type, public :: DynamicViscosityUnit_t
        double precision :: conversion_factor
        character(len=10) :: symbol
        character(len=50) :: gnuplot_symbol
        character(len=100) :: latex_symbol
    contains
        procedure :: toString => unitToString
        procedure :: toGnuplotString => unitToGnuplotString
        procedure :: toLatexString => unitToLatexString
    end type DynamicViscosityUnit_t

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
        module procedure sumDynamicViscosity
    end interface sum

    type(DynamicViscosityUnit_t), parameter, public :: MEGAPASCAL_SECONDS = &
            DynamicViscosityUnit_t( &
                    conversion_factor = MEGAPASCAL_SECONDS_PER_PASCAL_SECOND, &
                    symbol = "MPa s", &
                    gnuplot_symbol = "MPa s", &
                    latex_symbol = "\mega\pascal\second")
    type(DynamicViscosityUnit_t), parameter, public :: PASCAL_SECONDS = &
            DynamicViscosityUnit_t( &
                    conversion_factor = 1.0d0, &
                    symbol = "Pa s", &
                    gnuplot_symbol = "Pa s", &
                    latex_symbol = "\pascal\second")

    type(DynamicViscosityUnit_t), public :: DEFAULT_OUTPUT_UNITS = PASCAL_SECONDS

    type(DynamicViscosityUnit_t), parameter, public :: PROVIDED_UNITS(*) = &
            [MEGAPASCAL_SECONDS, PASCAL_SECONDS]

    public :: operator(.unit.), fromString, sum
contains
    pure subroutine fromStringBasicC(string, errors, dynamic_viscosity)
        character(len=*), intent(in) :: string
        type(ErrorList_t), intent(out) :: errors
        type(DynamicViscosity_t), intent(out) :: dynamic_viscosity

        type(ErrorList_t) :: errors_

        call fromString( &
                var_str(string), PROVIDED_UNITS, errors_, dynamic_viscosity)
        call errors%appendErrors( &
                errors_, &
                Module_("Dynamic_viscosity_m"), &
                Procedure_("fromStringBasicC"))
    end subroutine fromStringBasicC

    pure subroutine fromStringBasicS(string, errors, dynamic_viscosity)
        type(VARYING_STRING), intent(in) :: string
        type(ErrorList_t), intent(out) :: errors
        type(DynamicViscosity_t), intent(out) :: dynamic_viscosity

        type(ErrorList_t) :: errors_

        call fromString( &
                string, PROVIDED_UNITS, errors_, dynamic_viscosity)
        call errors%appendErrors( &
                errors_, &
                Module_("Dynamic_viscosity_m"), &
                Procedure_("fromStringBasicS"))
    end subroutine fromStringBasicS

    pure subroutine fromStringWithUnitsC(string, units, errors, dynamic_viscosity)
        character(len=*), intent(in) :: string
        type(DynamicViscosityUnit_t), intent(in) :: units(:)
        type(ErrorList_t), intent(out) :: errors
        type(DynamicViscosity_t), intent(out) :: dynamic_viscosity

        type(ErrorList_t) :: errors_

        call fromString( &
                var_str(string), units, errors_, dynamic_viscosity)
        call errors%appendErrors( &
                errors_, &
                Module_("Dynamic_viscosity_m"), &
                Procedure_("fromStringWithUnitsC"))
    end subroutine fromStringWithUnitsC

    pure subroutine fromStringWithUnitsS(string, units, errors, dynamic_viscosity)
        type(VARYING_STRING), intent(in) :: string
        type(DynamicViscosityUnit_t), intent(in) :: units(:)
        type(ErrorList_t), intent(out) :: errors
        type(DynamicViscosity_t), intent(out) :: dynamic_viscosity

        double precision :: number
        character(len=100) :: number_chars
        type(VARYING_STRING) :: number_string
        integer :: status
        type(VARYING_STRING) :: symbol
        type(DynamicViscosityUnit_t) :: unit
        type(ErrorList_t) :: unit_errors

        dynamic_viscosity%pascal_seconds = 0.0d0
        symbol = string
        call split(symbol, number_string, " ")
        if (len(symbol) == 0) then
            call errors%appendError(Fatal( &
                    PARSE_ERROR, &
                    Module_("Dynamic_viscosity_m"), &
                    Procedure_("fromStringWithUnitsS"), &
                    'No unit symbol found in string "' // string // '"'))
            return
        end if
        number_chars = number_string
        read(number_chars, *, iostat=status) number
        if (status /= 0) then
            call errors%appendError(Fatal( &
                    PARSE_ERROR, &
                    Module_("Dynamic_viscosity_m"), &
                    Procedure_("fromStringWithUnitsS"), &
                    'Error parsing number from string "' // number_string // '"'))
        end if
        call fromString(symbol, units, unit_errors, unit)
        dynamic_viscosity = number.unit.unit
        call errors%appendErrors( &
                unit_errors, &
                Module_("Dynamic_viscosity_m"), &
                Procedure_("fromStringWithUnitsS"))
    end subroutine fromStringWithUnitsS

    elemental function fromUnits(value_, units) result(dynamic_viscosity)
        double precision, intent(in) :: value_
        type(DynamicViscosityUnit_t), intent(in) :: units
        type(DynamicViscosity_t) :: dynamic_viscosity

        dynamic_viscosity%pascal_seconds = value_ / units%conversion_factor
    end function fromUnits

    elemental function toUnits(self, units) result(dynamic_viscosity)
        class(DynamicViscosity_t), intent(in) :: self
        class(DynamicViscosityUnit_t), intent(in) :: units
        double precision :: dynamic_viscosity

        dynamic_viscosity = self%pascal_seconds * units%conversion_factor
    end function toUnits

    elemental function doubleTimesDynamicViscosity( &
            multiplier, dynamic_viscosity) result(new_dynamic_viscosity)
        double precision, intent(in) :: multiplier
        class(DynamicViscosity_t), intent(in) :: dynamic_viscosity
        type(DynamicViscosity_t) :: new_dynamic_viscosity

        new_dynamic_viscosity%pascal_seconds = &
                multiplier * dynamic_viscosity%pascal_seconds
    end function doubleTimesDynamicViscosity

    elemental function integerTimesDynamicViscosity( &
            multiplier, dynamic_viscosity) result(new_dynamic_viscosity)
        integer, intent(in) :: multiplier
        class(DynamicViscosity_t), intent(in) :: dynamic_viscosity
        type(DynamicViscosity_t) :: new_dynamic_viscosity

        new_dynamic_viscosity%pascal_seconds = &
                dble(multiplier) * dynamic_viscosity%pascal_seconds
    end function integerTimesDynamicViscosity

    elemental function dynamicViscosityTimesDouble( &
            dynamic_viscosity, multiplier) result(new_dynamic_viscosity)
        class(DynamicViscosity_t), intent(in) :: dynamic_viscosity
        double precision, intent(in) :: multiplier
        type(DynamicViscosity_t) :: new_dynamic_viscosity

        new_dynamic_viscosity%pascal_seconds = &
                dynamic_viscosity%pascal_seconds * multiplier
    end function dynamicViscosityTimesDouble

    elemental function dynamicViscosityTimesInteger( &
            dynamic_viscosity, multiplier) result(new_dynamic_viscosity)
        class(DynamicViscosity_t), intent(in) :: dynamic_viscosity
        integer, intent(in) :: multiplier
        type(DynamicViscosity_t) :: new_dynamic_viscosity

        new_dynamic_viscosity%pascal_seconds = &
                dynamic_viscosity%pascal_seconds * dble(multiplier)
    end function dynamicViscosityTimesInteger

    elemental function dynamicViscosityDividedByDouble( &
            dynamic_viscosity, divisor) result(new_dynamic_viscosity)
        class(DynamicViscosity_t), intent(in) :: dynamic_viscosity
        double precision, intent(in) :: divisor
        type(DynamicViscosity_t) :: new_dynamic_viscosity

        new_dynamic_viscosity%pascal_seconds = &
                dynamic_viscosity%pascal_seconds / divisor
    end function dynamicViscosityDividedByDouble

    elemental function dynamicViscosityDividedByInteger( &
            dynamic_viscosity, divisor) result(new_dynamic_viscosity)
        class(DynamicViscosity_t), intent(in) :: dynamic_viscosity
        integer, intent(in) :: divisor
        type(DynamicViscosity_t) :: new_dynamic_viscosity

        new_dynamic_viscosity%pascal_seconds = &
                dynamic_viscosity%pascal_seconds / dble(divisor)
    end function dynamicViscosityDividedByInteger

    elemental function dynamicViscosityDividedByDynamicViscosity( &
            numerator, denomenator) result(ratio)
        class(DynamicViscosity_t), intent(in) :: numerator
        class(DynamicViscosity_t), intent(in) :: denomenator
        double precision :: ratio

        ratio = numerator%pascal_seconds / denomenator%pascal_seconds
    end function dynamicViscosityDividedByDynamicViscosity

    elemental function dynamicViscosityPlusDynamicViscosity( &
            dynamic_viscosity1, dynamic_viscosity2) result(new_dynamic_viscosity)
        class(DynamicViscosity_t), intent(in) :: dynamic_viscosity1
        class(DynamicViscosity_t), intent(in) :: dynamic_viscosity2
        type(DynamicViscosity_t) :: new_dynamic_viscosity

        new_dynamic_viscosity%pascal_seconds = &
                dynamic_viscosity1%pascal_seconds + dynamic_viscosity2%pascal_seconds
    end function dynamicViscosityPlusDynamicViscosity

    elemental function dynamicViscosityMinusDynamicViscosity( &
            dynamic_viscosity1, dynamic_viscosity2) result(new_dynamic_viscosity)
        class(DynamicViscosity_t), intent(in) :: dynamic_viscosity1
        class(DynamicViscosity_t), intent(in) :: dynamic_viscosity2
        type(DynamicViscosity_t) :: new_dynamic_viscosity

        new_dynamic_viscosity%pascal_seconds = &
                dynamic_viscosity1%pascal_seconds - dynamic_viscosity2%pascal_seconds
    end function dynamicViscosityMinusDynamicViscosity

    pure function sumDynamicViscosity(dynamic_viscositys)
        type(DynamicViscosity_t), intent(in) :: dynamic_viscositys(:)
        type(DynamicViscosity_t) :: sumDynamicViscosity

        sumDynamicViscosity%pascal_seconds = sum(dynamic_viscositys%pascal_seconds)
    end function sumDynamicViscosity

    elemental function greaterThan(lhs, rhs)
        class(DynamicViscosity_t), intent(in) :: lhs
        class(DynamicViscosity_t), intent(in) :: rhs
        logical :: greaterThan

        greaterThan = lhs%pascal_seconds > rhs%pascal_seconds
    end function greaterThan

    elemental function lessThan(lhs,rhs)
        class(DynamicViscosity_t), intent(in) :: lhs
        class(DynamicViscosity_t), intent(in) :: rhs
        logical :: lessThan

        lessThan = lhs%pascal_seconds < rhs%pascal_seconds
    end function lessThan

    elemental function greaterThanOrEqual(lhs, rhs)
        class(DynamicViscosity_t), intent(in) :: lhs
        class(DynamicViscosity_t), intent(in) :: rhs
        logical :: greaterThanOrEqual

        greaterThanOrEqual = lhs%pascal_seconds >= rhs%pascal_seconds
    end function greaterThanOrEqual

    elemental function lessThanOrEqual(lhs, rhs)
        class(DynamicViscosity_t), intent(in) :: lhs
        class(DynamicViscosity_t), intent(in) :: rhs
        logical :: lessThanOrEqual

        lessThanOrEqual = lhs%pascal_seconds <= rhs%pascal_seconds
    end function lessThanOrEqual

    elemental function equal_(lhs,rhs)
        class(DynamicViscosity_t), intent(in) :: lhs
        class(DynamicViscosity_t), intent(in) :: rhs
        logical :: equal_

        equal_ = lhs%pascal_seconds .safeEq. rhs%pascal_seconds
    end function equal_

    elemental function equalWithinAbsolute(lhs, rhs, within)
        class(DynamicViscosity_t), intent(in) :: lhs
        class(DynamicViscosity_t), intent(in) :: rhs
        class(DynamicViscosity_t), intent(in) :: within
        logical :: equalWithinAbsolute

        equalWithinAbsolute = equalWithinAbsolute_( &
                lhs%pascal_seconds, rhs%pascal_seconds, within%pascal_seconds)
    end function equalWithinAbsolute

    elemental function equalWithinRelative(lhs, rhs, within)
        class(DynamicViscosity_t), intent(in) :: lhs
        class(DynamicViscosity_t), intent(in) :: rhs
        double precision, intent(in) :: within
        logical :: equalWithinRelative

        equalWithinRelative = equalWithinRelative_( &
                lhs%pascal_seconds, rhs%pascal_seconds, within)
    end function equalWithinRelative

    elemental function notEqual(lhs, rhs)
        class(DynamicViscosity_t), intent(in) :: lhs
        class(DynamicViscosity_t), intent(in) :: rhs
        logical :: notEqual

        notEqual = .not. lhs == rhs
    end function notEqual

    elemental function toStringFullPrecision(self) result(string)
        class(DynamicViscosity_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = self%toStringIn(DEFAULT_OUTPUT_UNITS)
    end function toStringFullPrecision

    elemental function toStringWithPrecision(self, significant_digits) result(string)
        class(DynamicViscosity_t), intent(in) :: self
        integer, intent(in) :: significant_digits
        type(VARYING_STRING) :: string

        string = self%toStringIn(DEFAULT_OUTPUT_UNITS, significant_digits)
    end function toStringWithPrecision

    elemental function toStringInFullPrecision(self, units) result(string)
        class(DynamicViscosity_t), intent(in) :: self
        class(DynamicViscosityUnit_t), intent(in) :: units
        type(VARYING_STRING) :: string

        string = toString(self.in.units) // " " // units%toString()
    end function toStringInFullPrecision

    elemental function toStringInWithPrecision( &
            self, units, significant_digits) result(string)
        class(DynamicViscosity_t), intent(in) :: self
        class(DynamicViscosityUnit_t), intent(in) :: units
        integer, intent(in) :: significant_digits
        type(VARYING_STRING) :: string

        string = &
                toString(self.in.units, significant_digits) &
                // " " // units%toString()
    end function toStringInWithPrecision

    elemental function toGnuplotStringFullPrecision(self) result(string)
        class(DynamicViscosity_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = self%toGnuplotStringIn(DEFAULT_OUTPUT_UNITS)
    end function toGnuplotStringFullPrecision

    elemental function toGnuplotStringWithPrecision( &
            self, significant_digits) result(string)
        class(DynamicViscosity_t), intent(in) :: self
        integer, intent(in) :: significant_digits
        type(VARYING_STRING) :: string

        string = self%toGnuplotStringIn( &
                DEFAULT_OUTPUT_UNITS, significant_digits)
    end function toGnuplotStringWithPrecision

    elemental function toGnuplotStringInFullPrecision(self, units) result(string)
        class(DynamicViscosity_t), intent(in) :: self
        class(DynamicViscosityUnit_t), intent(in) :: units
        type(VARYING_STRING) :: string

        string = toString(self.in.units) // " " // units%toGnuplotString()
    end function toGnuplotStringInFullPrecision

    elemental function toGnuplotStringInWithPrecision( &
            self, units, significant_digits) result(string)
        class(DynamicViscosity_t), intent(in) :: self
        class(DynamicViscosityUnit_t), intent(in) :: units
        integer, intent(in) :: significant_digits
        type(VARYING_STRING) :: string

        string = &
                toString(self.in.units, significant_digits) &
                // " " // units%toGnuplotString()
    end function toGnuplotStringInWithPrecision

    elemental function toLatexStringFullPrecision(self) result(string)
        class(DynamicViscosity_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = self%toLatexStringIn(DEFAULT_OUTPUT_UNITS)
    end function toLatexStringFullPrecision

    elemental function toLatexStringWithPrecision(self, significant_digits) result(string)
        class(DynamicViscosity_t), intent(in) :: self
        integer, intent(in) :: significant_digits
        type(VARYING_STRING) :: string

        string = self%toLatexStringIn(DEFAULT_OUTPUT_UNITS, significant_digits)
    end function toLatexStringWithPrecision

    elemental function toLatexStringInFullPrecision(self, units) result(string)
        class(DynamicViscosity_t), intent(in) :: self
        class(DynamicViscosityUnit_t), intent(in) :: units
        type(VARYING_STRING) :: string

        string = wrapInLatexQuantity( &
                toString(self.in.units), units%toLatexString())
    end function toLatexStringInFullPrecision

    elemental function toLatexStringInWithPrecision( &
            self, units, significant_digits) result(string)
        class(DynamicViscosity_t), intent(in) :: self
        class(DynamicViscosityUnit_t), intent(in) :: units
        integer, intent(in) :: significant_digits
        type(VARYING_STRING) :: string

        string = wrapInLatexQuantity( &
                toString(self.in.units, significant_digits), &
                units%toLatexString())
    end function toLatexStringInWithPrecision

    pure subroutine unitFromStringBasicC(string, errors, unit)
        character(len=*), intent(in) :: string
        type(ErrorList_t), intent(out) :: errors
        type(DynamicViscosityUnit_t), intent(out) :: unit

        type(ErrorList_t) :: errors_

        call fromString( &
                var_str(string), PROVIDED_UNITS, errors_, unit)
        call errors%appendErrors( &
                errors_, &
                Module_("Dynamic_viscosity_m"), &
                Procedure_("unitFromStringBasicC"))
    end subroutine unitFromStringBasicC

    pure subroutine unitFromStringBasicS(string, errors, unit)
        type(VARYING_STRING), intent(in) :: string
        type(ErrorList_t), intent(out) :: errors
        type(DynamicViscosityUnit_t), intent(out) :: unit

        type(ErrorList_t) :: errors_

        call fromString( &
                string, PROVIDED_UNITS, errors_, unit)
        call errors%appendErrors( &
                errors_, &
                Module_("Dynamic_viscosity_m"), &
                Procedure_("unitFromStringBasicS"))
    end subroutine unitFromStringBasicS

    pure subroutine unitFromStringWithUnitsC(string, units, errors, unit)
        character(len=*), intent(in) :: string
        type(DynamicViscosityUnit_t), intent(in) :: units(:)
        type(ErrorList_t), intent(out) :: errors
        type(DynamicViscosityUnit_t), intent(out) :: unit

        type(ErrorList_t) :: errors_

        call fromString(var_str(string), units, errors_, unit)
        call errors%appendErrors( &
                errors_, &
                Module_("Dynamic_viscosity_m"), &
                Procedure_("unitFromStringWithUnitsC"))
    end subroutine unitFromStringWithUnitsC

    pure subroutine unitFromStringWithUnitsS(string, units, errors, unit)
        type(VARYING_STRING), intent(in) :: string
        type(DynamicViscosityUnit_t), intent(in) :: units(:)
        type(ErrorList_t), intent(out) :: errors
        type(DynamicViscosityUnit_t), intent(out) :: unit

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
                    Module_("Dynamic_viscosity_m"), &
                    Procedure_("unitFromStringWithUnitsS"), &
                    '"' // string // '", known units: [' // join(unit_strings, ', ') // ']' ))
        end if
    end subroutine unitFromStringWithUnitsS

    elemental function unitToString(self) result(string)
        class(DynamicViscosityUnit_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = trim(self%symbol)
    end function unitToString

    elemental function unitToGnuplotString(self) result(string)
        class(DynamicViscosityUnit_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = trim(self%gnuplot_symbol)
    end function unitToGnuplotString

    elemental function unitToLatexString(self) result(string)
        class(DynamicViscosityUnit_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = trim(self%latex_symbol)
    end function unitToLatexString
end module Dynamic_viscosity_m
