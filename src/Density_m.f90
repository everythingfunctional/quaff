module Density_m
    use Conversion_factors_m, only: &
            GRAMS_PER_CUBIC_METER_PER_KILOGRAMS_PER_CUBIC_METER

    implicit none
    private

    type, public :: Density_t
        private
        double precision :: kilograms_per_cubic_meter
    contains
        private
        procedure :: toUnits
        generic, public :: operator(.in.) => toUnits
        procedure, pass(density) :: doubleTimesDensity
        procedure, pass(density) :: integerTimesDensity
        procedure, pass(density) :: densityTimesDouble
        procedure, pass(density) :: densityTimesInteger
        generic, public :: operator(*) => &
                doubleTimesDensity, &
                integerTimesDensity, &
                densityTimesDouble, &
                densityTimesInteger
        procedure :: densityDividedByDouble
        procedure :: densityDividedByInteger
        procedure, pass(numerator) :: densityDividedByDensity
        generic, public :: operator(/) => &
                densityDividedByDouble, &
                densityDividedByInteger, &
                densityDividedByDensity
        procedure :: densityPlusDensity
        generic, public :: operator(+) => densityPlusDensity
        procedure :: densityMinusDensity
        generic, public :: operator(-) => densityMinusDensity
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
    end type Density_t

    type, public :: DensityUnit_t
        double precision :: conversion_factor
        character(len=10) :: symbol
        character(len=50) :: gnuplot_symbol
        character(len=100) :: latex_symbol
    contains
        procedure :: toString => unitToString
        procedure :: toGnuplotString => unitToGnuplotString
        procedure :: toLatexString => unitToLatexString
    end type DensityUnit_t

    interface operator(.unit.)
        module procedure fromUnits
    end interface operator(.unit.)

    interface densityFromString
        module procedure fromStringBasicC
        module procedure fromStringBasicS
        module procedure fromStringWithUnitsC
        module procedure fromStringWithUnitsS
    end interface densityFromString

    interface densityUnitFromString
        module procedure unitFromStringBasicC
        module procedure unitFromStringBasicS
        module procedure unitFromStringWithUnitsC
        module procedure unitFromStringWithUnitsS
    end interface densityUnitFromString

    type(DensityUnit_t), parameter, public :: GRAMS_PER_CUBIC_METER = &
            DensityUnit_t( &
                    conversion_factor = 1.0d0, &
                    symbol = "g/m^3", &
                    gnuplot_symbol = "g/m^3", &
                    latex_symbol = "\gram\per\cubic\meter")
    type(DensityUnit_t), parameter, public :: KILOGRAMS_PER_CUBIC_METER = &
            DensityUnit_t( &
                    conversion_factor = 1.0d0, &
                    symbol = "kg/m^3", &
                    gnuplot_symbol = "kg/m^3", &
                    latex_symbol = "\kilo\gram\per\cubic\meter")

    type(DensityUnit_t), public :: DEFAULT_OUTPUT_UNITS = KILOGRAMS_PER_CUBIC_METER

    type(DensityUnit_t), parameter, public :: PROVIDED_UNITS(*) = &
            [GRAMS_PER_CUBIC_METER, KILOGRAMS_PER_CUBIC_METER]

    public :: &
            operator(.unit.), &
            densityFromString, &
            densityUnitFromString
contains
    function fromStringBasicC(string, errors) result(density)
        use erloff, only: ErrorList_t, Module_, Procedure_
        use iso_varying_string, only: var_str

        character(len=*), intent(in) :: string
        type(ErrorList_t), intent(out) :: errors
        type(Density_t) :: density

        type(ErrorList_t) :: errors_

        density = densityFromString( &
                var_str(string), PROVIDED_UNITS, errors_)
        call errors%appendErrors( &
                errors_, &
                Module_("Density_m"), &
                Procedure_("fromStringBasicC"))
    end function fromStringBasicC

    function fromStringBasicS(string, errors) result(density)
        use erloff, only: ErrorList_t, Module_, Procedure_
        use iso_varying_string, only: VARYING_STRING

        type(VARYING_STRING), intent(in) :: string
        type(ErrorList_t), intent(out) :: errors
        type(Density_t) :: density

        type(ErrorList_t) :: errors_

        density = densityFromString( &
                string, PROVIDED_UNITS, errors_)
        call errors%appendErrors( &
                errors_, &
                Module_("Density_m"), &
                Procedure_("fromStringBasicS"))
    end function fromStringBasicS

    function fromStringWithUnitsC(string, units, errors) result(density)
        use erloff, only: ErrorList_t, Module_, Procedure_
        use iso_varying_string, only: var_str

        character(len=*), intent(in) :: string
        type(DensityUnit_t), intent(in) :: units(:)
        type(ErrorList_t), intent(out) :: errors
        type(Density_t) :: density

        type(ErrorList_t) :: errors_

        density = densityFromString( &
                var_str(string), units, errors_)
        call errors%appendErrors( &
                errors_, &
                Module_("Density_m"), &
                Procedure_("fromStringWithUnitsC"))
    end function fromStringWithUnitsC

    function fromStringWithUnitsS(string, units, errors) result(density)
        use erloff, only: ErrorList_t, Fatal, Module_, Procedure_
        use iso_varying_string, only: &
                VARYING_STRING, &
                assignment(=), &
                operator(//), &
                operator(==), &
                len, &
                split
        use Miscellaneous_m, only: PARSE_ERROR
        use strff, only: join

        type(VARYING_STRING), intent(in) :: string
        type(DensityUnit_t), intent(in) :: units(:)
        type(ErrorList_t), intent(out) :: errors
        type(Density_t) :: density

        double precision :: number
        character(len=100) :: number_chars
        type(VARYING_STRING) :: number_string
        integer :: status
        type(VARYING_STRING) :: symbol
        type(DensityUnit_t) :: unit
        type(ErrorList_t) :: unit_errors

        density%kilograms_per_cubic_meter = 0.0d0
        symbol = string
        call split(symbol, number_string, " ")
        if (len(symbol) == 0) then
            call errors%appendError(Fatal( &
                    PARSE_ERROR, &
                    Module_("Density_m"), &
                    Procedure_("fromStringWithUnitsS"), &
                    'No unit symbol found in string "' // string // '"'))
            return
        end if
        number_chars = number_string
        read(number_chars, *, iostat=status) number
        if (status /= 0) then
            call errors%appendError(Fatal( &
                    PARSE_ERROR, &
                    Module_("Density_m"), &
                    Procedure_("fromStringWithUnitsS"), &
                    'Error parsing number from string "' // number_string // '"'))
        end if
        unit = densityUnitFromString(symbol, units, unit_errors)
        density = number.unit.unit
        call errors%appendErrors( &
                unit_errors, &
                Module_("Density_m"), &
                Procedure_("fromStringWithUnitsS"))
    end function fromStringWithUnitsS

    elemental function fromUnits(value_, units) result(density)
        double precision, intent(in) :: value_
        type(DensityUnit_t), intent(in) :: units
        type(Density_t) :: density

        density%kilograms_per_cubic_meter = value_ / units%conversion_factor
    end function fromUnits

    elemental function toUnits(self, units) result(density)
        class(Density_t), intent(in) :: self
        class(DensityUnit_t), intent(in) :: units
        double precision :: density

        density = self%kilograms_per_cubic_meter * units%conversion_factor
    end function toUnits

    elemental function doubleTimesDensity( &
            multiplier, density) result(new_density)
        double precision, intent(in) :: multiplier
        class(Density_t), intent(in) :: density
        type(Density_t) :: new_density

        new_density%kilograms_per_cubic_meter = &
                multiplier * density%kilograms_per_cubic_meter
    end function doubleTimesDensity

    elemental function integerTimesDensity( &
            multiplier, density) result(new_density)
        integer, intent(in) :: multiplier
        class(Density_t), intent(in) :: density
        type(Density_t) :: new_density

        new_density%kilograms_per_cubic_meter = &
                dble(multiplier) * density%kilograms_per_cubic_meter
    end function integerTimesDensity

    elemental function densityTimesDouble( &
            density, multiplier) result(new_density)
        class(Density_t), intent(in) :: density
        double precision, intent(in) :: multiplier
        type(Density_t) :: new_density

        new_density%kilograms_per_cubic_meter = &
                density%kilograms_per_cubic_meter * multiplier
    end function densityTimesDouble

    elemental function densityTimesInteger( &
            density, multiplier) result(new_density)
        class(Density_t), intent(in) :: density
        integer, intent(in) :: multiplier
        type(Density_t) :: new_density

        new_density%kilograms_per_cubic_meter = &
                density%kilograms_per_cubic_meter * dble(multiplier)
    end function densityTimesInteger

    elemental function densityDividedByDouble( &
            density, divisor) result(new_density)
        class(Density_t), intent(in) :: density
        double precision, intent(in) :: divisor
        type(Density_t) :: new_density

        new_density%kilograms_per_cubic_meter = &
                density%kilograms_per_cubic_meter / divisor
    end function densityDividedByDouble

    elemental function densityDividedByInteger( &
            density, divisor) result(new_density)
        class(Density_t), intent(in) :: density
        integer, intent(in) :: divisor
        type(Density_t) :: new_density

        new_density%kilograms_per_cubic_meter = &
                density%kilograms_per_cubic_meter / dble(divisor)
    end function densityDividedByInteger

    elemental function densityDividedByDensity( &
            numerator, denomenator) result(ratio)
        class(Density_t), intent(in) :: numerator
        class(Density_t), intent(in) :: denomenator
        double precision :: ratio

        ratio = numerator%kilograms_per_cubic_meter / denomenator%kilograms_per_cubic_meter
    end function densityDividedByDensity

    elemental function densityPlusDensity( &
            density1, density2) result(new_density)
        class(Density_t), intent(in) :: density1
        class(Density_t), intent(in) :: density2
        type(Density_t) :: new_density

        new_density%kilograms_per_cubic_meter = &
                density1%kilograms_per_cubic_meter + density2%kilograms_per_cubic_meter
    end function densityPlusDensity

    elemental function densityMinusDensity( &
            density1, density2) result(new_density)
        class(Density_t), intent(in) :: density1
        class(Density_t), intent(in) :: density2
        type(Density_t) :: new_density

        new_density%kilograms_per_cubic_meter = &
                density1%kilograms_per_cubic_meter - density2%kilograms_per_cubic_meter
    end function densityMinusDensity

    elemental function greaterThan(lhs, rhs)
        class(Density_t), intent(in) :: lhs
        class(Density_t), intent(in) :: rhs
        logical :: greaterThan

        greaterThan = lhs%kilograms_per_cubic_meter > rhs%kilograms_per_cubic_meter
    end function greaterThan

    elemental function lessThan(lhs,rhs)
        class(Density_t), intent(in) :: lhs
        class(Density_t), intent(in) :: rhs
        logical :: lessThan

        lessThan = lhs%kilograms_per_cubic_meter < rhs%kilograms_per_cubic_meter
    end function lessThan

    elemental function greaterThanOrEqual(lhs, rhs)
        class(Density_t), intent(in) :: lhs
        class(Density_t), intent(in) :: rhs
        logical :: greaterThanOrEqual

        greaterThanOrEqual = lhs%kilograms_per_cubic_meter >= rhs%kilograms_per_cubic_meter
    end function greaterThanOrEqual

    elemental function lessThanOrEqual(lhs, rhs)
        class(Density_t), intent(in) :: lhs
        class(Density_t), intent(in) :: rhs
        logical :: lessThanOrEqual

        lessThanOrEqual = lhs%kilograms_per_cubic_meter <= rhs%kilograms_per_cubic_meter
    end function lessThanOrEqual

    elemental function equal_(lhs,rhs)
        use Miscellaneous_m, only: operator(.safeEq.)

        class(Density_t), intent(in) :: lhs
        class(Density_t), intent(in) :: rhs
        logical :: equal_

        equal_ = lhs%kilograms_per_cubic_meter .safeEq. rhs%kilograms_per_cubic_meter
    end function equal_

    function equalWithinAbsolute(lhs, rhs, within)
        use Miscellaneous_m, only: equalWithinAbsolute_ => equalWithinAbsolute

        class(Density_t), intent(in) :: lhs
        class(Density_t), intent(in) :: rhs
        class(Density_t), intent(in) :: within
        logical :: equalWithinAbsolute

        equalWithinAbsolute = equalWithinAbsolute_( &
                lhs%kilograms_per_cubic_meter, rhs%kilograms_per_cubic_meter, within%kilograms_per_cubic_meter)
    end function equalWithinAbsolute

    function equalWithinRelative(lhs, rhs, within)
        use Miscellaneous_m, only: equalWithinRelative_ => equalWithinRelative

        class(Density_t), intent(in) :: lhs
        class(Density_t), intent(in) :: rhs
        double precision, intent(in) :: within
        logical :: equalWithinRelative

        equalWithinRelative = equalWithinRelative_( &
                lhs%kilograms_per_cubic_meter, rhs%kilograms_per_cubic_meter, within)
    end function equalWithinRelative

    elemental function notEqual(lhs, rhs)
        class(Density_t), intent(in) :: lhs
        class(Density_t), intent(in) :: rhs
        logical :: notEqual

        notEqual = .not. lhs == rhs
    end function notEqual

    function toStringFullPrecision(self) result(string)
        use iso_varying_string, only: VARYING_STRING

        class(Density_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = self%toStringIn(DEFAULT_OUTPUT_UNITS)
    end function toStringFullPrecision

    function toStringWithPrecision(self, significant_digits) result(string)
        use iso_varying_string, only: VARYING_STRING

        class(Density_t), intent(in) :: self
        integer, intent(in) :: significant_digits
        type(VARYING_STRING) :: string

        string = self%toStringIn(DEFAULT_OUTPUT_UNITS, significant_digits)
    end function toStringWithPrecision

    function toStringInFullPrecision(self, units) result(string)
        use iso_varying_string, only: VARYING_STRING, operator(//)
        use strff, only: toString

        class(Density_t), intent(in) :: self
        class(DensityUnit_t), intent(in) :: units
        type(VARYING_STRING) :: string

        string = toString(self.in.units) // " " // units%toString()
    end function toStringInFullPrecision

    function toStringInWithPrecision( &
            self, units, significant_digits) result(string)
        use iso_varying_string, only: VARYING_STRING, operator(//)
        use strff, only: toString

        class(Density_t), intent(in) :: self
        class(DensityUnit_t), intent(in) :: units
        integer, intent(in) :: significant_digits
        type(VARYING_STRING) :: string

        string = &
                toString(self.in.units, significant_digits) &
                // " " // units%toString()
    end function toStringInWithPrecision

    function toGnuplotStringFullPrecision(self) result(string)
        use iso_varying_string, only: VARYING_STRING

        class(Density_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = self%toGnuplotStringIn(DEFAULT_OUTPUT_UNITS)
    end function toGnuplotStringFullPrecision

    function toGnuplotStringWithPrecision( &
            self, significant_digits) result(string)
        use iso_varying_string, only: VARYING_STRING

        class(Density_t), intent(in) :: self
        integer, intent(in) :: significant_digits
        type(VARYING_STRING) :: string

        string = self%toGnuplotStringIn( &
                DEFAULT_OUTPUT_UNITS, significant_digits)
    end function toGnuplotStringWithPrecision

    function toGnuplotStringInFullPrecision(self, units) result(string)
        use iso_varying_string, only: VARYING_STRING, operator(//)
        use strff, only: toString

        class(Density_t), intent(in) :: self
        class(DensityUnit_t), intent(in) :: units
        type(VARYING_STRING) :: string

        string = toString(self.in.units) // " " // units%toGnuplotString()
    end function toGnuplotStringInFullPrecision

    function toGnuplotStringInWithPrecision( &
            self, units, significant_digits) result(string)
        use iso_varying_string, only: VARYING_STRING, operator(//)
        use strff, only: toString

        class(Density_t), intent(in) :: self
        class(DensityUnit_t), intent(in) :: units
        integer, intent(in) :: significant_digits
        type(VARYING_STRING) :: string

        string = &
                toString(self.in.units, significant_digits) &
                // " " // units%toGnuplotString()
    end function toGnuplotStringInWithPrecision

    function toLatexStringFullPrecision(self) result(string)
        use iso_varying_string, only: VARYING_STRING

        class(Density_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = self%toLatexStringIn(DEFAULT_OUTPUT_UNITS)
    end function toLatexStringFullPrecision

    function toLatexStringWithPrecision(self, significant_digits) result(string)
        use iso_varying_string, only: VARYING_STRING

        class(Density_t), intent(in) :: self
        integer, intent(in) :: significant_digits
        type(VARYING_STRING) :: string

        string = self%toLatexStringIn(DEFAULT_OUTPUT_UNITS, significant_digits)
    end function toLatexStringWithPrecision

    function toLatexStringInFullPrecision(self, units) result(string)
        use iso_varying_string, only: VARYING_STRING, operator(//)
        use Miscellaneous_m, only: wrapInLatexQuantity
        use strff, only: toString

        class(Density_t), intent(in) :: self
        class(DensityUnit_t), intent(in) :: units
        type(VARYING_STRING) :: string

        string = wrapInLatexQuantity( &
                toString(self.in.units), units%toLatexString())
    end function toLatexStringInFullPrecision

    function toLatexStringInWithPrecision( &
            self, units, significant_digits) result(string)
        use iso_varying_string, only: VARYING_STRING, operator(//)
        use Miscellaneous_m, only: wrapInLatexQuantity
        use strff, only: toString

        class(Density_t), intent(in) :: self
        class(DensityUnit_t), intent(in) :: units
        integer, intent(in) :: significant_digits
        type(VARYING_STRING) :: string

        string = wrapInLatexQuantity( &
                toString(self.in.units, significant_digits), &
                units%toLatexString())
    end function toLatexStringInWithPrecision

    function unitFromStringBasicC(string, errors) result(unit)
        use erloff, only: ErrorList_t, Module_, Procedure_
        use iso_varying_string, only: var_str

        character(len=*), intent(in) :: string
        type(ErrorList_t), intent(out) :: errors
        type(DensityUnit_t) :: unit

        type(ErrorList_t) :: errors_

        unit = densityUnitFromString( &
                var_str(string), PROVIDED_UNITS, errors_)
        call errors%appendErrors( &
                errors_, &
                Module_("Density_m"), &
                Procedure_("unitFromStringBasicC"))
    end function unitFromStringBasicC

    function unitFromStringBasicS(string, errors) result(unit)
        use erloff, only: ErrorList_t, Module_, Procedure_
        use iso_varying_string, only: VARYING_STRING

        type(VARYING_STRING), intent(in) :: string
        type(ErrorList_t), intent(out) :: errors
        type(DensityUnit_t) :: unit

        type(ErrorList_t) :: errors_

        unit = densityUnitFromString( &
                string, PROVIDED_UNITS, errors_)
        call errors%appendErrors( &
                errors_, &
                Module_("Density_m"), &
                Procedure_("unitFromStringBasicS"))
    end function unitFromStringBasicS

    function unitFromStringWithUnitsC(string, units, errors) result(unit)
        use erloff, only: ErrorList_t, Module_, Procedure_
        use iso_varying_string, only: var_str

        character(len=*), intent(in) :: string
        type(DensityUnit_t), intent(in) :: units(:)
        type(ErrorList_t), intent(out) :: errors
        type(DensityUnit_t) :: unit

        type(ErrorList_t) :: errors_

        unit = densityUnitFromString(var_str(string), units, errors_)
        call errors%appendErrors( &
                errors_, &
                Module_("Density_m"), &
                Procedure_("unitFromStringWithUnitsC"))
    end function unitFromStringWithUnitsC

    function unitFromStringWithUnitsS(string, units, errors) result(unit)
        use erloff, only: ErrorList_t, Fatal, Module_, Procedure_
        use iso_varying_string, only: VARYING_STRING, operator(==), operator(//)
        use Miscellaneous_m, only: UNKNOWN_UNIT
        use strff, only: join

        type(VARYING_STRING), intent(in) :: string
        type(DensityUnit_t), intent(in) :: units(:)
        type(ErrorList_t), intent(out) :: errors
        type(DensityUnit_t) :: unit

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
                    Module_("Density_m"), &
                    Procedure_("unitFromStringWithUnitsS"), &
                    '"' // string // '", known units: [' // join(unit_strings, ', ') // ']' ))
        end if
    end function unitFromStringWithUnitsS

    function unitToString(self) result(string)
        use iso_varying_string, only: VARYING_STRING, assignment(=)

        class(DensityUnit_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = trim(self%symbol)
    end function unitToString

    function unitToGnuplotString(self) result(string)
        use iso_varying_string, only: VARYING_STRING, assignment(=)

        class(DensityUnit_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = trim(self%gnuplot_symbol)
    end function unitToGnuplotString

    function unitToLatexString(self) result(string)
        use iso_varying_string, only: VARYING_STRING, assignment(=)

        class(DensityUnit_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = trim(self%latex_symbol)
    end function unitToLatexString
end module Density_m
