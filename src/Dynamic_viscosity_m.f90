module Dynamic_viscosity_m
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
    use quaff_Conversion_factors_m, only: MEGAPASCAL_SECONDS_PER_PASCAL_SECOND
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
    end type DynamicViscosity_t

    type, abstract, public :: DynamicViscosityUnit_t
        double precision :: conversion_factor
    contains
        procedure(justUnitToString), deferred :: unitToString
        procedure(unitWithValueToString), deferred :: valueToString
        generic :: toString => unitToString, valueToString
        procedure(parseAsI), deferred :: parseAs
    end type DynamicViscosityUnit_t

    type, extends(DynamicViscosityUnit_t), public :: DynamicViscositySimpleUnit_t
        character(len=20) :: symbol
    contains
        procedure :: unitToString => simpleUnitToString
        procedure :: valueToString => simpleValueToString
        procedure :: parseAs => simpleParseAs
    end type DynamicViscositySimpleUnit_t

    abstract interface
        elemental function justUnitToString(self) result(string)
            import DynamicViscosityUnit_t, VARYING_STRING
            class(DynamicViscosityUnit_t), intent(in) :: self
            type(VARYING_STRING) :: string
        end function justUnitToString

        pure function unitWithValueToString(self, value_) result(string)
            import DynamicViscosityUnit_t, VARYING_STRING
            class(DynamicViscosityUnit_t), intent(in) :: self
            type(VARYING_STRING), intent(in) :: value_
            type(VARYING_STRING) :: string
        end function unitWithValueToString

        pure subroutine parseAsI(self, string, errors, dynamic_viscosity)
            import ErrorList_t, DynamicViscosity_t, DynamicViscosityUnit_t, VARYING_STRING
            class(DynamicViscosityUnit_t), intent(in) :: self
            type(VARYING_STRING), intent(in) :: string
            type(ErrorList_t), intent(out) :: errors
            type(DynamicViscosity_t), intent(out) :: dynamic_viscosity
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
        module procedure sumDynamicViscosity
    end interface sum

    type(DynamicViscositySimpleUnit_t), parameter, public :: MEGAPASCAL_SECONDS = &
            DynamicViscositySimpleUnit_t( &
                    conversion_factor = MEGAPASCAL_SECONDS_PER_PASCAL_SECOND, &
                    symbol = "MPa s")
    type(DynamicViscositySimpleUnit_t), parameter, public :: PASCAL_SECONDS = &
            DynamicViscositySimpleUnit_t( &
                    conversion_factor = 1.0d0, &
                    symbol = "Pa s")

    type(DynamicViscositySimpleUnit_t), public :: DEFAULT_OUTPUT_UNITS = PASCAL_SECONDS

    type(DynamicViscositySimpleUnit_t), parameter, public :: PROVIDED_UNITS(*) = &
            [MEGAPASCAL_SECONDS, PASCAL_SECONDS]

    public :: operator(.unit.), fromString, selectUnit, sum
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
        class(DynamicViscosityUnit_t), intent(in) :: units(:)
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
        class(DynamicViscosityUnit_t), intent(in) :: units(:)
        type(ErrorList_t), intent(out) :: errors
        type(DynamicViscosity_t), intent(out) :: dynamic_viscosity

        type(ErrorList_t) :: all_errors(size(units))
        integer :: i

        do i = 1, size(units)
            call units(i)%parseAs(string, all_errors(i), dynamic_viscosity)
            if (.not. all_errors(i)%hasAny()) return
        end do
        do i = 1, size(units)
            call errors%appendErrors( &
                    all_errors(i), &
                    Module_("Dynamic_viscosity_m"), &
                    Procedure_("fromStringWithUnitsS"))
        end do
    end subroutine fromStringWithUnitsS

    elemental function fromUnits(value_, units) result(dynamic_viscosity)
        double precision, intent(in) :: value_
        class(DynamicViscosityUnit_t), intent(in) :: units
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

        string = units%toString(toString(self.in.units))
    end function toStringInFullPrecision

    elemental function toStringInWithPrecision( &
            self, units, significant_digits) result(string)
        class(DynamicViscosity_t), intent(in) :: self
        class(DynamicViscosityUnit_t), intent(in) :: units
        integer, intent(in) :: significant_digits
        type(VARYING_STRING) :: string

        string = units%toString(toString(self.in.units, significant_digits))
    end function toStringInWithPrecision

    elemental function simpleUnitToString(self) result(string)
        class(DynamicViscositySimpleUnit_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = trim(self%symbol)
    end function simpleUnitToString

    pure function simpleValueToString(self, value_) result(string)
        class(DynamicViscositySimpleUnit_t), intent(in) :: self
        type(VARYING_STRING), intent(in) :: value_
        type(VARYING_STRING) :: string

        string = value_ // " " // self%toString()
    end function simpleValueToString

    pure subroutine simpleParseAs(self, string, errors, dynamic_viscosity)
        class(DynamicViscositySimpleUnit_t), intent(in) :: self
        type(VARYING_STRING), intent(in) :: string
        type(ErrorList_t), intent(out) :: errors
        type(DynamicViscosity_t), intent(out) :: dynamic_viscosity

        type(ParseResult_t) :: parse_result

        parse_result = parseWith(theParser, string)
        if (parse_result%ok) then
            select type (the_number => parse_result%parsed)
            type is (ParsedRational_t)
                dynamic_viscosity = the_number%value_.unit.self
            end select
        else
            call errors%appendError(Fatal( &
                    PARSE_ERROR, &
                    Module_("Dynamic_viscosity_m"), &
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
        type(DynamicViscositySimpleUnit_t), intent(out) :: unit

        type(ErrorList_t) :: errors_

        call fromString(var_str(string), PROVIDED_UNITS, errors_, unit)
        call errors%appendErrors( &
                errors_, &
                Module_("Dynamic_viscosity_m"), &
                Procedure_("simpleUnitFromStringC"))
    end subroutine simpleUnitFromStringC

    pure subroutine simpleUnitFromStringS(string, errors, unit)
        type(VARYING_STRING), intent(in) :: string
        type(ErrorList_t), intent(out) :: errors
        type(DynamicViscositySimpleUnit_t), intent(out) :: unit

        type(ErrorList_t) :: errors_

        call fromString(string, PROVIDED_UNITS, errors_, unit)
        call errors%appendErrors( &
                errors_, &
                Module_("Dynamic_viscosity_m"), &
                Procedure_("simpleUnitFromStringS"))
    end subroutine simpleUnitFromStringS

    pure subroutine simpleUnitFromStringWithUnitsC(string, units, errors, unit)
        character(len=*), intent(in) :: string
        type(DynamicViscositySimpleUnit_t), intent(in) :: units(:)
        type(ErrorList_t), intent(out) :: errors
        type(DynamicViscositySimpleUnit_t), intent(out) :: unit

        type(ErrorList_t) :: errors_

        call fromString(var_str(string), units, errors_, unit)
        call errors%appendErrors( &
                errors_, &
                Module_("Dynamic_viscosity_m"), &
                Procedure_("simpleUnitFromStringWithUnitsC"))
    end subroutine simpleUnitFromStringWithUnitsC

    pure subroutine simpleUnitFromStringWithUnitsS(string, units, errors, unit)
        type(VARYING_STRING), intent(in) :: string
        type(DynamicViscositySimpleUnit_t), intent(in) :: units(:)
        type(ErrorList_t), intent(out) :: errors
        type(DynamicViscositySimpleUnit_t), intent(out) :: unit

        type(ErrorList_t) :: errors_
        integer :: which_unit

        call selectUnit(string, units, errors, which_unit)
        if (errors_%hasAny()) then
            call errors%appendErrors( &
                    errors_, &
                    Module_("Dynamic_viscosity_m"), &
                    Procedure_("simpleUnitFromStringWithUnitsS"))
        else
            unit = units(which_unit)
        end if
    end subroutine simpleUnitFromStringWithUnitsS

    pure subroutine selectUnit(string, units, errors, index)
        type(VARYING_STRING), intent(in) :: string
        class(DynamicViscosityUnit_t), intent(in) :: units(:)
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
                Module_("Dynamic_viscosity_m"), &
                Procedure_("selectUnit"), &
                '"' // string // '", known units: [' // join(unit_strings, ', ') // ']'))
    end subroutine selectUnit
end module Dynamic_viscosity_m
