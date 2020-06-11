module Density_m
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
            GRAMS_PER_CUBIC_METER_PER_KILOGRAMS_PER_CUBIC_METER
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

    type, public :: Density_t
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
    end type Density_t

    type, abstract, public :: DensityUnit_t
        double precision :: conversion_factor
    contains
        procedure(justUnitToString), deferred :: unitToString
        procedure(unitWithValueToString), deferred :: valueToString
        generic :: toString => unitToString, valueToString
        procedure(parseAsI), deferred :: parseAs
    end type DensityUnit_t

    type, extends(DensityUnit_t), public :: DensitySimpleUnit_t
        character(len=20) :: symbol
    contains
        procedure :: unitToString => simpleUnitToString
        procedure :: valueToString => simpleValueToString
        procedure :: parseAs => simpleParseAs
    end type DensitySimpleUnit_t

    type, extends(DensityUnit_t), public :: DensityGnuplotUnit_t
        character(len=50) :: symbol
    contains
        procedure :: unitToString => gnuplotUnitToString
        procedure :: valueToString => gnuplotValueToString
        procedure :: parseAs => gnuplotParseAs
    end type DensityGnuplotUnit_t

    abstract interface
        elemental function justUnitToString(self) result(string)
            import DensityUnit_t, VARYING_STRING
            class(DensityUnit_t), intent(in) :: self
            type(VARYING_STRING) :: string
        end function justUnitToString

        pure function unitWithValueToString(self, value_) result(string)
            import DensityUnit_t, VARYING_STRING
            class(DensityUnit_t), intent(in) :: self
            type(VARYING_STRING), intent(in) :: value_
            type(VARYING_STRING) :: string
        end function unitWithValueToString

        pure subroutine parseAsI(self, string, errors, density)
            import ErrorList_t, Density_t, DensityUnit_t, VARYING_STRING
            class(DensityUnit_t), intent(in) :: self
            type(VARYING_STRING), intent(in) :: string
            type(ErrorList_t), intent(out) :: errors
            type(Density_t), intent(out) :: density
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
        module procedure sumDensity
    end interface sum

    type(DensitySimpleUnit_t), parameter, public :: GRAMS_PER_CUBIC_METER = &
            DensitySimpleUnit_t( &
                    conversion_factor = GRAMS_PER_CUBIC_METER_PER_KILOGRAMS_PER_CUBIC_METER, &
                    symbol = "g/m^3")
    type(DensityGnuplotUnit_t), parameter, public :: GRAMS_PER_CUBIC_METER_GNUPLOT = &
            DensityGnuplotUnit_t( &
                    conversion_factor = GRAMS_PER_CUBIC_METER_PER_KILOGRAMS_PER_CUBIC_METER, &
                    symbol = "g/m^3")
    type(DensitySimpleUnit_t), parameter, public :: KILOGRAMS_PER_CUBIC_METER = &
            DensitySimpleUnit_t( &
                    conversion_factor = 1.0d0, &
                    symbol = "kg/m^3")
    type(DensityGnuplotUnit_t), parameter, public :: KILOGRAMS_PER_CUBIC_METER_GNUPLOT = &
            DensityGnuplotUnit_t( &
                    conversion_factor = 1.0d0, &
                    symbol = "kg/m^3")

    type(DensitySimpleUnit_t), public :: DEFAULT_OUTPUT_UNITS = KILOGRAMS_PER_CUBIC_METER

    type(DensitySimpleUnit_t), parameter, public :: PROVIDED_UNITS(*) = &
            [GRAMS_PER_CUBIC_METER, KILOGRAMS_PER_CUBIC_METER]
    type(DensityGnuplotUnit_t), parameter, public :: PROVIDED_GNUPLOT_UNITS(*) = &
            [GRAMS_PER_CUBIC_METER_GNUPLOT, KILOGRAMS_PER_CUBIC_METER_GNUPLOT]

    public :: operator(.unit.), fromString, selectUnit, sum
contains
    pure subroutine fromStringBasicC(string, errors, density)
        character(len=*), intent(in) :: string
        type(ErrorList_t), intent(out) :: errors
        type(Density_t), intent(out) :: density

        type(ErrorList_t) :: errors_

        call fromString( &
                var_str(string), PROVIDED_UNITS, errors_, density)
        call errors%appendErrors( &
                errors_, &
                Module_("Density_m"), &
                Procedure_("fromStringBasicC"))
    end subroutine fromStringBasicC

    pure subroutine fromStringBasicS(string, errors, density)
        type(VARYING_STRING), intent(in) :: string
        type(ErrorList_t), intent(out) :: errors
        type(Density_t), intent(out) :: density

        type(ErrorList_t) :: errors_

        call fromString( &
                string, PROVIDED_UNITS, errors_, density)
        call errors%appendErrors( &
                errors_, &
                Module_("Density_m"), &
                Procedure_("fromStringBasicS"))
    end subroutine fromStringBasicS

    pure subroutine fromStringWithUnitsC(string, units, errors, density)
        character(len=*), intent(in) :: string
        class(DensityUnit_t), intent(in) :: units(:)
        type(ErrorList_t), intent(out) :: errors
        type(Density_t), intent(out) :: density

        type(ErrorList_t) :: errors_

        call fromString( &
                var_str(string), units, errors_, density)
        call errors%appendErrors( &
                errors_, &
                Module_("Density_m"), &
                Procedure_("fromStringWithUnitsC"))
    end subroutine fromStringWithUnitsC

    pure subroutine fromStringWithUnitsS(string, units, errors, density)
        type(VARYING_STRING), intent(in) :: string
        class(DensityUnit_t), intent(in) :: units(:)
        type(ErrorList_t), intent(out) :: errors
        type(Density_t), intent(out) :: density

        type(ErrorList_t) :: all_errors(size(units))
        integer :: i

        do i = 1, size(units)
            call units(i)%parseAs(string, all_errors(i), density)
            if (.not. all_errors(i)%hasAny()) return
        end do
        do i = 1, size(units)
            call errors%appendErrors( &
                    all_errors(i), &
                    Module_("Density_m"), &
                    Procedure_("fromStringWithUnitsS"))
        end do
    end subroutine fromStringWithUnitsS

    elemental function fromUnits(value_, units) result(density)
        double precision, intent(in) :: value_
        class(DensityUnit_t), intent(in) :: units
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

    pure function sumDensity(densitys)
        type(Density_t), intent(in) :: densitys(:)
        type(Density_t) :: sumDensity

        sumDensity%kilograms_per_cubic_meter = sum(densitys%kilograms_per_cubic_meter)
    end function sumDensity

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
        class(Density_t), intent(in) :: lhs
        class(Density_t), intent(in) :: rhs
        logical :: equal_

        equal_ = lhs%kilograms_per_cubic_meter .safeEq. rhs%kilograms_per_cubic_meter
    end function equal_

    elemental function equalWithinAbsolute(lhs, rhs, within)
        class(Density_t), intent(in) :: lhs
        class(Density_t), intent(in) :: rhs
        class(Density_t), intent(in) :: within
        logical :: equalWithinAbsolute

        equalWithinAbsolute = equalWithinAbsolute_( &
                lhs%kilograms_per_cubic_meter, rhs%kilograms_per_cubic_meter, within%kilograms_per_cubic_meter)
    end function equalWithinAbsolute

    elemental function equalWithinRelative(lhs, rhs, within)
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

    elemental function toStringFullPrecision(self) result(string)
        class(Density_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = self%toStringIn(DEFAULT_OUTPUT_UNITS)
    end function toStringFullPrecision

    elemental function toStringWithPrecision(self, significant_digits) result(string)
        class(Density_t), intent(in) :: self
        integer, intent(in) :: significant_digits
        type(VARYING_STRING) :: string

        string = self%toStringIn(DEFAULT_OUTPUT_UNITS, significant_digits)
    end function toStringWithPrecision

    elemental function toStringInFullPrecision(self, units) result(string)
        class(Density_t), intent(in) :: self
        class(DensityUnit_t), intent(in) :: units
        type(VARYING_STRING) :: string

        string = units%toString(toString(self.in.units))
    end function toStringInFullPrecision

    elemental function toStringInWithPrecision( &
            self, units, significant_digits) result(string)
        class(Density_t), intent(in) :: self
        class(DensityUnit_t), intent(in) :: units
        integer, intent(in) :: significant_digits
        type(VARYING_STRING) :: string

        string = units%toString(toString(self.in.units, significant_digits))
    end function toStringInWithPrecision

    elemental function simpleUnitToString(self) result(string)
        class(DensitySimpleUnit_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = trim(self%symbol)
    end function simpleUnitToString

    pure function simpleValueToString(self, value_) result(string)
        class(DensitySimpleUnit_t), intent(in) :: self
        type(VARYING_STRING), intent(in) :: value_
        type(VARYING_STRING) :: string

        string = value_ // " " // self%toString()
    end function simpleValueToString

    pure subroutine simpleParseAs(self, string, errors, density)
        class(DensitySimpleUnit_t), intent(in) :: self
        type(VARYING_STRING), intent(in) :: string
        type(ErrorList_t), intent(out) :: errors
        type(Density_t), intent(out) :: density

        type(ParseResult_t) :: parse_result

        parse_result = parseWith(theParser, string)
        if (parse_result%ok) then
            select type (the_number => parse_result%parsed)
            type is (ParsedRational_t)
                density = the_number%value_.unit.self
            end select
        else
            call errors%appendError(Fatal( &
                    PARSE_ERROR, &
                    Module_("Density_m"), &
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

    pure subroutine gnuplotParseAs(self, string, errors, density)
        class(DensityGnuplotUnit_t), intent(in) :: self
        type(VARYING_STRING), intent(in) :: string
        type(ErrorList_t), intent(out) :: errors
        type(Density_t), intent(out) :: density

        type(ParseResult_t) :: parse_result

        parse_result = parseWith(theParser, string)
        if (parse_result%ok) then
            select type (the_number => parse_result%parsed)
            type is (ParsedRational_t)
                density = the_number%value_.unit.self
            end select
        else
            call errors%appendError(Fatal( &
                    PARSE_ERROR, &
                    Module_("Density_m"), &
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
        class(DensityGnuplotUnit_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = trim(self%symbol)
    end function gnuplotUnitToString

    pure function gnuplotValueToString(self, value_) result(string)
        class(DensityGnuplotUnit_t), intent(in) :: self
        type(VARYING_STRING), intent(in) :: value_
        type(VARYING_STRING) :: string

        string = value_ // " " // self%toString()
    end function gnuplotValueToString

    pure subroutine simpleUnitFromStringC(string, errors, unit)
        character(len=*), intent(in) :: string
        type(ErrorList_t), intent(out) :: errors
        type(DensitySimpleUnit_t), intent(out) :: unit

        type(ErrorList_t) :: errors_

        call fromString(var_str(string), PROVIDED_UNITS, errors_, unit)
        call errors%appendErrors( &
                errors_, &
                Module_("Density_m"), &
                Procedure_("simpleUnitFromStringC"))
    end subroutine simpleUnitFromStringC

    pure subroutine simpleUnitFromStringS(string, errors, unit)
        type(VARYING_STRING), intent(in) :: string
        type(ErrorList_t), intent(out) :: errors
        type(DensitySimpleUnit_t), intent(out) :: unit

        type(ErrorList_t) :: errors_

        call fromString(string, PROVIDED_UNITS, errors_, unit)
        call errors%appendErrors( &
                errors_, &
                Module_("Density_m"), &
                Procedure_("simpleUnitFromStringS"))
    end subroutine simpleUnitFromStringS

    pure subroutine simpleUnitFromStringWithUnitsC(string, units, errors, unit)
        character(len=*), intent(in) :: string
        type(DensitySimpleUnit_t), intent(in) :: units(:)
        type(ErrorList_t), intent(out) :: errors
        type(DensitySimpleUnit_t), intent(out) :: unit

        type(ErrorList_t) :: errors_

        call fromString(var_str(string), units, errors_, unit)
        call errors%appendErrors( &
                errors_, &
                Module_("Density_m"), &
                Procedure_("simpleUnitFromStringWithUnitsC"))
    end subroutine simpleUnitFromStringWithUnitsC

    pure subroutine simpleUnitFromStringWithUnitsS(string, units, errors, unit)
        type(VARYING_STRING), intent(in) :: string
        type(DensitySimpleUnit_t), intent(in) :: units(:)
        type(ErrorList_t), intent(out) :: errors
        type(DensitySimpleUnit_t), intent(out) :: unit

        type(ErrorList_t) :: errors_
        integer :: which_unit

        call selectUnit(string, units, errors, which_unit)
        if (errors_%hasAny()) then
            call errors%appendErrors( &
                    errors_, &
                    Module_("Density_m"), &
                    Procedure_("simpleUnitFromStringWithUnitsS"))
        else
            unit = units(which_unit)
        end if
    end subroutine simpleUnitFromStringWithUnitsS

    pure subroutine selectUnit(string, units, errors, index)
        type(VARYING_STRING), intent(in) :: string
        class(DensityUnit_t), intent(in) :: units(:)
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
                Module_("Density_m"), &
                Procedure_("selectUnit"), &
                '"' // string // '", known units: [' // join(unit_strings, ', ') // ']'))
    end subroutine selectUnit
end module Density_m
