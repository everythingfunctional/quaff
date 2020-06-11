module Burnup_m
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
            MEGAWATT_DAYS_PER_TON_PER_WATT_SECONDS_PER_KILOGRAM
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

    type, public :: Burnup_t
        double precision :: watt_seconds_per_kilogram
    contains
        private
        procedure :: toUnits
        generic, public :: operator(.in.) => toUnits
        procedure, pass(burnup) :: doubleTimesBurnup
        procedure, pass(burnup) :: integerTimesBurnup
        procedure, pass(burnup) :: burnupTimesDouble
        procedure, pass(burnup) :: burnupTimesInteger
        generic, public :: operator(*) => &
                doubleTimesBurnup, &
                integerTimesBurnup, &
                burnupTimesDouble, &
                burnupTimesInteger
        procedure :: burnupDividedByDouble
        procedure :: burnupDividedByInteger
        procedure, pass(numerator) :: burnupDividedByBurnup
        generic, public :: operator(/) => &
                burnupDividedByDouble, &
                burnupDividedByInteger, &
                burnupDividedByBurnup
        procedure :: burnupPlusBurnup
        generic, public :: operator(+) => burnupPlusBurnup
        procedure :: burnupMinusBurnup
        generic, public :: operator(-) => burnupMinusBurnup
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
    end type Burnup_t

    type, abstract, public :: BurnupUnit_t
        double precision :: conversion_factor
    contains
        procedure(justUnitToString), deferred :: unitToString
        procedure(unitWithValueToString), deferred :: valueToString
        generic :: toString => unitToString, valueToString
        procedure(parseAsI), deferred :: parseAs
    end type BurnupUnit_t

    type, extends(BurnupUnit_t), public :: BurnupSimpleUnit_t
        character(len=20) :: symbol
    contains
        procedure :: unitToString => simpleUnitToString
        procedure :: valueToString => simpleValueToString
        procedure :: parseAs => simpleParseAs
    end type BurnupSimpleUnit_t

    abstract interface
        elemental function justUnitToString(self) result(string)
            import BurnupUnit_t, VARYING_STRING
            class(BurnupUnit_t), intent(in) :: self
            type(VARYING_STRING) :: string
        end function justUnitToString

        pure function unitWithValueToString(self, value_) result(string)
            import BurnupUnit_t, VARYING_STRING
            class(BurnupUnit_t), intent(in) :: self
            type(VARYING_STRING), intent(in) :: value_
            type(VARYING_STRING) :: string
        end function unitWithValueToString

        pure subroutine parseAsI(self, string, errors, burnup)
            import ErrorList_t, Burnup_t, BurnupUnit_t, VARYING_STRING
            class(BurnupUnit_t), intent(in) :: self
            type(VARYING_STRING), intent(in) :: string
            type(ErrorList_t), intent(out) :: errors
            type(Burnup_t), intent(out) :: burnup
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
        module procedure sumBurnup
    end interface sum

    type(BurnupSimpleUnit_t), parameter, public :: MEGAWATT_DAYS_PER_TON = &
            BurnupSimpleUnit_t( &
                    conversion_factor = MEGAWATT_DAYS_PER_TON_PER_WATT_SECONDS_PER_KILOGRAM, &
                    symbol = "(MW d)/t")
    type(BurnupSimpleUnit_t), parameter, public :: WATT_SECONDS_PER_KILOGRAM = &
            BurnupSimpleUnit_t( &
                    conversion_factor = 1.0d0, &
                    symbol = "(W s)/kg")

    type(BurnupSimpleUnit_t), public :: DEFAULT_OUTPUT_UNITS = WATT_SECONDS_PER_KILOGRAM

    type(BurnupSimpleUnit_t), parameter, public :: PROVIDED_UNITS(*) = &
            [MEGAWATT_DAYS_PER_TON, WATT_SECONDS_PER_KILOGRAM]

    public :: operator(.unit.), fromString, selectUnit, sum
contains
    pure subroutine fromStringBasicC(string, errors, burnup)
        character(len=*), intent(in) :: string
        type(ErrorList_t), intent(out) :: errors
        type(Burnup_t), intent(out) :: burnup

        type(ErrorList_t) :: errors_

        call fromString( &
                var_str(string), PROVIDED_UNITS, errors_, burnup)
        call errors%appendErrors( &
                errors_, &
                Module_("Burnup_m"), &
                Procedure_("fromStringBasicC"))
    end subroutine fromStringBasicC

    pure subroutine fromStringBasicS(string, errors, burnup)
        type(VARYING_STRING), intent(in) :: string
        type(ErrorList_t), intent(out) :: errors
        type(Burnup_t), intent(out) :: burnup

        type(ErrorList_t) :: errors_

        call fromString( &
                string, PROVIDED_UNITS, errors_, burnup)
        call errors%appendErrors( &
                errors_, &
                Module_("Burnup_m"), &
                Procedure_("fromStringBasicS"))
    end subroutine fromStringBasicS

    pure subroutine fromStringWithUnitsC(string, units, errors, burnup)
        character(len=*), intent(in) :: string
        class(BurnupUnit_t), intent(in) :: units(:)
        type(ErrorList_t), intent(out) :: errors
        type(Burnup_t), intent(out) :: burnup

        type(ErrorList_t) :: errors_

        call fromString( &
                var_str(string), units, errors_, burnup)
        call errors%appendErrors( &
                errors_, &
                Module_("Burnup_m"), &
                Procedure_("fromStringWithUnitsC"))
    end subroutine fromStringWithUnitsC

    pure subroutine fromStringWithUnitsS(string, units, errors, burnup)
        type(VARYING_STRING), intent(in) :: string
        class(BurnupUnit_t), intent(in) :: units(:)
        type(ErrorList_t), intent(out) :: errors
        type(Burnup_t), intent(out) :: burnup

        type(ErrorList_t) :: all_errors(size(units))
        integer :: i

        do i = 1, size(units)
            call units(i)%parseAs(string, all_errors(i), burnup)
            if (.not. all_errors(i)%hasAny()) return
        end do
        do i = 1, size(units)
            call errors%appendErrors( &
                    all_errors(i), &
                    Module_("Burnup_m"), &
                    Procedure_("fromStringWithUnitsS"))
        end do
    end subroutine fromStringWithUnitsS

    elemental function fromUnits(value_, units) result(burnup)
        double precision, intent(in) :: value_
        class(BurnupUnit_t), intent(in) :: units
        type(Burnup_t) :: burnup

        burnup%watt_seconds_per_kilogram = value_ / units%conversion_factor
    end function fromUnits

    elemental function toUnits(self, units) result(burnup)
        class(Burnup_t), intent(in) :: self
        class(BurnupUnit_t), intent(in) :: units
        double precision :: burnup

        burnup = self%watt_seconds_per_kilogram * units%conversion_factor
    end function toUnits

    elemental function doubleTimesBurnup( &
            multiplier, burnup) result(new_burnup)
        double precision, intent(in) :: multiplier
        class(Burnup_t), intent(in) :: burnup
        type(Burnup_t) :: new_burnup

        new_burnup%watt_seconds_per_kilogram = &
                multiplier * burnup%watt_seconds_per_kilogram
    end function doubleTimesBurnup

    elemental function integerTimesBurnup( &
            multiplier, burnup) result(new_burnup)
        integer, intent(in) :: multiplier
        class(Burnup_t), intent(in) :: burnup
        type(Burnup_t) :: new_burnup

        new_burnup%watt_seconds_per_kilogram = &
                dble(multiplier) * burnup%watt_seconds_per_kilogram
    end function integerTimesBurnup

    elemental function burnupTimesDouble( &
            burnup, multiplier) result(new_burnup)
        class(Burnup_t), intent(in) :: burnup
        double precision, intent(in) :: multiplier
        type(Burnup_t) :: new_burnup

        new_burnup%watt_seconds_per_kilogram = &
                burnup%watt_seconds_per_kilogram * multiplier
    end function burnupTimesDouble

    elemental function burnupTimesInteger( &
            burnup, multiplier) result(new_burnup)
        class(Burnup_t), intent(in) :: burnup
        integer, intent(in) :: multiplier
        type(Burnup_t) :: new_burnup

        new_burnup%watt_seconds_per_kilogram = &
                burnup%watt_seconds_per_kilogram * dble(multiplier)
    end function burnupTimesInteger

    elemental function burnupDividedByDouble( &
            burnup, divisor) result(new_burnup)
        class(Burnup_t), intent(in) :: burnup
        double precision, intent(in) :: divisor
        type(Burnup_t) :: new_burnup

        new_burnup%watt_seconds_per_kilogram = &
                burnup%watt_seconds_per_kilogram / divisor
    end function burnupDividedByDouble

    elemental function burnupDividedByInteger( &
            burnup, divisor) result(new_burnup)
        class(Burnup_t), intent(in) :: burnup
        integer, intent(in) :: divisor
        type(Burnup_t) :: new_burnup

        new_burnup%watt_seconds_per_kilogram = &
                burnup%watt_seconds_per_kilogram / dble(divisor)
    end function burnupDividedByInteger

    elemental function burnupDividedByBurnup( &
            numerator, denomenator) result(ratio)
        class(Burnup_t), intent(in) :: numerator
        class(Burnup_t), intent(in) :: denomenator
        double precision :: ratio

        ratio = numerator%watt_seconds_per_kilogram / denomenator%watt_seconds_per_kilogram
    end function burnupDividedByBurnup

    elemental function burnupPlusBurnup( &
            burnup1, burnup2) result(new_burnup)
        class(Burnup_t), intent(in) :: burnup1
        class(Burnup_t), intent(in) :: burnup2
        type(Burnup_t) :: new_burnup

        new_burnup%watt_seconds_per_kilogram = &
                burnup1%watt_seconds_per_kilogram + burnup2%watt_seconds_per_kilogram
    end function burnupPlusBurnup

    elemental function burnupMinusBurnup( &
            burnup1, burnup2) result(new_burnup)
        class(Burnup_t), intent(in) :: burnup1
        class(Burnup_t), intent(in) :: burnup2
        type(Burnup_t) :: new_burnup

        new_burnup%watt_seconds_per_kilogram = &
                burnup1%watt_seconds_per_kilogram - burnup2%watt_seconds_per_kilogram
    end function burnupMinusBurnup

    pure function sumBurnup(burnups)
        type(Burnup_t), intent(in) :: burnups(:)
        type(Burnup_t) :: sumBurnup

        sumBurnup%watt_seconds_per_kilogram = sum(burnups%watt_seconds_per_kilogram)
    end function sumBurnup

    elemental function greaterThan(lhs, rhs)
        class(Burnup_t), intent(in) :: lhs
        class(Burnup_t), intent(in) :: rhs
        logical :: greaterThan

        greaterThan = lhs%watt_seconds_per_kilogram > rhs%watt_seconds_per_kilogram
    end function greaterThan

    elemental function lessThan(lhs,rhs)
        class(Burnup_t), intent(in) :: lhs
        class(Burnup_t), intent(in) :: rhs
        logical :: lessThan

        lessThan = lhs%watt_seconds_per_kilogram < rhs%watt_seconds_per_kilogram
    end function lessThan

    elemental function greaterThanOrEqual(lhs, rhs)
        class(Burnup_t), intent(in) :: lhs
        class(Burnup_t), intent(in) :: rhs
        logical :: greaterThanOrEqual

        greaterThanOrEqual = lhs%watt_seconds_per_kilogram >= rhs%watt_seconds_per_kilogram
    end function greaterThanOrEqual

    elemental function lessThanOrEqual(lhs, rhs)
        class(Burnup_t), intent(in) :: lhs
        class(Burnup_t), intent(in) :: rhs
        logical :: lessThanOrEqual

        lessThanOrEqual = lhs%watt_seconds_per_kilogram <= rhs%watt_seconds_per_kilogram
    end function lessThanOrEqual

    elemental function equal_(lhs,rhs)
        class(Burnup_t), intent(in) :: lhs
        class(Burnup_t), intent(in) :: rhs
        logical :: equal_

        equal_ = lhs%watt_seconds_per_kilogram .safeEq. rhs%watt_seconds_per_kilogram
    end function equal_

    elemental function equalWithinAbsolute(lhs, rhs, within)
        class(Burnup_t), intent(in) :: lhs
        class(Burnup_t), intent(in) :: rhs
        class(Burnup_t), intent(in) :: within
        logical :: equalWithinAbsolute

        equalWithinAbsolute = equalWithinAbsolute_( &
                lhs%watt_seconds_per_kilogram, rhs%watt_seconds_per_kilogram, within%watt_seconds_per_kilogram)
    end function equalWithinAbsolute

    elemental function equalWithinRelative(lhs, rhs, within)
        class(Burnup_t), intent(in) :: lhs
        class(Burnup_t), intent(in) :: rhs
        double precision, intent(in) :: within
        logical :: equalWithinRelative

        equalWithinRelative = equalWithinRelative_( &
                lhs%watt_seconds_per_kilogram, rhs%watt_seconds_per_kilogram, within)
    end function equalWithinRelative

    elemental function notEqual(lhs, rhs)
        class(Burnup_t), intent(in) :: lhs
        class(Burnup_t), intent(in) :: rhs
        logical :: notEqual

        notEqual = .not. lhs == rhs
    end function notEqual

    elemental function toStringFullPrecision(self) result(string)
        class(Burnup_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = self%toStringIn(DEFAULT_OUTPUT_UNITS)
    end function toStringFullPrecision

    elemental function toStringWithPrecision(self, significant_digits) result(string)
        class(Burnup_t), intent(in) :: self
        integer, intent(in) :: significant_digits
        type(VARYING_STRING) :: string

        string = self%toStringIn(DEFAULT_OUTPUT_UNITS, significant_digits)
    end function toStringWithPrecision

    elemental function toStringInFullPrecision(self, units) result(string)
        class(Burnup_t), intent(in) :: self
        class(BurnupUnit_t), intent(in) :: units
        type(VARYING_STRING) :: string

        string = units%toString(toString(self.in.units))
    end function toStringInFullPrecision

    elemental function toStringInWithPrecision( &
            self, units, significant_digits) result(string)
        class(Burnup_t), intent(in) :: self
        class(BurnupUnit_t), intent(in) :: units
        integer, intent(in) :: significant_digits
        type(VARYING_STRING) :: string

        string = units%toString(toString(self.in.units, significant_digits))
    end function toStringInWithPrecision

    elemental function simpleUnitToString(self) result(string)
        class(BurnupSimpleUnit_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = trim(self%symbol)
    end function simpleUnitToString

    pure function simpleValueToString(self, value_) result(string)
        class(BurnupSimpleUnit_t), intent(in) :: self
        type(VARYING_STRING), intent(in) :: value_
        type(VARYING_STRING) :: string

        string = value_ // " " // self%toString()
    end function simpleValueToString

    pure subroutine simpleParseAs(self, string, errors, burnup)
        class(BurnupSimpleUnit_t), intent(in) :: self
        type(VARYING_STRING), intent(in) :: string
        type(ErrorList_t), intent(out) :: errors
        type(Burnup_t), intent(out) :: burnup

        type(ParseResult_t) :: parse_result

        parse_result = parseWith(theParser, string)
        if (parse_result%ok) then
            select type (the_number => parse_result%parsed)
            type is (ParsedRational_t)
                burnup = the_number%value_.unit.self
            end select
        else
            call errors%appendError(Fatal( &
                    PARSE_ERROR, &
                    Module_("Burnup_m"), &
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
        type(BurnupSimpleUnit_t), intent(out) :: unit

        type(ErrorList_t) :: errors_

        call fromString(var_str(string), PROVIDED_UNITS, errors_, unit)
        call errors%appendErrors( &
                errors_, &
                Module_("Burnup_m"), &
                Procedure_("simpleUnitFromStringC"))
    end subroutine simpleUnitFromStringC

    pure subroutine simpleUnitFromStringS(string, errors, unit)
        type(VARYING_STRING), intent(in) :: string
        type(ErrorList_t), intent(out) :: errors
        type(BurnupSimpleUnit_t), intent(out) :: unit

        type(ErrorList_t) :: errors_

        call fromString(string, PROVIDED_UNITS, errors_, unit)
        call errors%appendErrors( &
                errors_, &
                Module_("Burnup_m"), &
                Procedure_("simpleUnitFromStringS"))
    end subroutine simpleUnitFromStringS

    pure subroutine simpleUnitFromStringWithUnitsC(string, units, errors, unit)
        character(len=*), intent(in) :: string
        type(BurnupSimpleUnit_t), intent(in) :: units(:)
        type(ErrorList_t), intent(out) :: errors
        type(BurnupSimpleUnit_t), intent(out) :: unit

        type(ErrorList_t) :: errors_

        call fromString(var_str(string), units, errors_, unit)
        call errors%appendErrors( &
                errors_, &
                Module_("Burnup_m"), &
                Procedure_("simpleUnitFromStringWithUnitsC"))
    end subroutine simpleUnitFromStringWithUnitsC

    pure subroutine simpleUnitFromStringWithUnitsS(string, units, errors, unit)
        type(VARYING_STRING), intent(in) :: string
        type(BurnupSimpleUnit_t), intent(in) :: units(:)
        type(ErrorList_t), intent(out) :: errors
        type(BurnupSimpleUnit_t), intent(out) :: unit

        type(ErrorList_t) :: errors_
        integer :: which_unit

        call selectUnit(string, units, errors, which_unit)
        if (errors_%hasAny()) then
            call errors%appendErrors( &
                    errors_, &
                    Module_("Burnup_m"), &
                    Procedure_("simpleUnitFromStringWithUnitsS"))
        else
            unit = units(which_unit)
        end if
    end subroutine simpleUnitFromStringWithUnitsS

    pure subroutine selectUnit(string, units, errors, index)
        type(VARYING_STRING), intent(in) :: string
        class(BurnupUnit_t), intent(in) :: units(:)
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
                Module_("Burnup_m"), &
                Procedure_("selectUnit"), &
                '"' // string // '", known units: [' // join(unit_strings, ', ') // ']'))
    end subroutine selectUnit
end module Burnup_m
