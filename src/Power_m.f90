module Power_m
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
            BTU_PER_HOUR_PER_WATT, &
            CALORIES_PER_SECOND_PER_WATT, &
            MEGABTU_PER_HOUR_PER_WATT, &
            MEGAWATTS_PER_WATT
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

    type, public :: Power_t
        double precision :: watts
    contains
        private
        procedure :: toUnits
        generic, public :: operator(.in.) => toUnits
        procedure, pass(power) :: doubleTimesPower
        procedure, pass(power) :: integerTimesPower
        procedure, pass(power) :: powerTimesDouble
        procedure, pass(power) :: powerTimesInteger
        generic, public :: operator(*) => &
                doubleTimesPower, &
                integerTimesPower, &
                powerTimesDouble, &
                powerTimesInteger
        procedure :: powerDividedByDouble
        procedure :: powerDividedByInteger
        procedure, pass(numerator) :: powerDividedByPower
        generic, public :: operator(/) => &
                powerDividedByDouble, &
                powerDividedByInteger, &
                powerDividedByPower
        procedure :: powerPlusPower
        generic, public :: operator(+) => powerPlusPower
        procedure :: powerMinusPower
        generic, public :: operator(-) => powerMinusPower
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
    end type Power_t

    type, abstract, public :: PowerUnit_t
        double precision :: conversion_factor
    contains
        procedure(justUnitToString), deferred :: unitToString
        procedure(unitWithValueToString), deferred :: valueToString
        generic :: toString => unitToString, valueToString
        procedure(parseAsI), deferred :: parseAs
    end type PowerUnit_t

    type, extends(PowerUnit_t), public :: PowerSimpleUnit_t
        character(len=20) :: symbol
    contains
        procedure :: unitToString => simpleUnitToString
        procedure :: valueToString => simpleValueToString
        procedure :: parseAs => simpleParseAs
    end type PowerSimpleUnit_t

    abstract interface
        elemental function justUnitToString(self) result(string)
            import PowerUnit_t, VARYING_STRING
            class(PowerUnit_t), intent(in) :: self
            type(VARYING_STRING) :: string
        end function justUnitToString

        pure function unitWithValueToString(self, value_) result(string)
            import PowerUnit_t, VARYING_STRING
            class(PowerUnit_t), intent(in) :: self
            type(VARYING_STRING), intent(in) :: value_
            type(VARYING_STRING) :: string
        end function unitWithValueToString

        pure subroutine parseAsI(self, string, errors, power)
            import ErrorList_t, Power_t, PowerUnit_t, VARYING_STRING
            class(PowerUnit_t), intent(in) :: self
            type(VARYING_STRING), intent(in) :: string
            type(ErrorList_t), intent(out) :: errors
            type(Power_t), intent(out) :: power
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
        module procedure sumPower
    end interface sum

    type(PowerSimpleUnit_t), parameter, public :: BTU_PER_HOUR = &
            PowerSimpleUnit_t( &
                    conversion_factor = BTU_PER_HOUR_PER_WATT, &
                    symbol = "BTU/hr")
    type(PowerSimpleUnit_t), parameter, public :: CALORIES_PER_SECOND = &
            PowerSimpleUnit_t( &
                    conversion_factor = CALORIES_PER_SECOND_PER_WATT, &
                    symbol = "cal/s")
    type(PowerSimpleUnit_t), parameter, public :: MEGABTU_PER_HOUR = &
            PowerSimpleUnit_t( &
                    conversion_factor = MEGABTU_PER_HOUR_PER_WATT, &
                    symbol = "MBTU/hr")
    type(PowerSimpleUnit_t), parameter, public :: MEGAWATTS = &
            PowerSimpleUnit_t( &
                    conversion_factor = MEGAWATTS_PER_WATT, &
                    symbol = "MW")
    type(PowerSimpleUnit_t), parameter, public :: WATTS = &
            PowerSimpleUnit_t( &
                    conversion_factor = 1.0d0, &
                    symbol = "W")

    type(PowerSimpleUnit_t), public :: DEFAULT_OUTPUT_UNITS = WATTS

    type(PowerSimpleUnit_t), parameter, public :: PROVIDED_UNITS(*) = &
            [BTU_PER_HOUR, &
            CALORIES_PER_SECOND, &
            MEGABTU_PER_HOUR, &
            MEGAWATTS, &
            WATTS]

    public :: operator(.unit.), fromString, selectUnit, sum
contains
    pure subroutine fromStringBasicC(string, errors, power)
        character(len=*), intent(in) :: string
        type(ErrorList_t), intent(out) :: errors
        type(Power_t), intent(out) :: power

        type(ErrorList_t) :: errors_

        call fromString( &
                var_str(string), PROVIDED_UNITS, errors_, power)
        call errors%appendErrors( &
                errors_, &
                Module_("Power_m"), &
                Procedure_("fromStringBasicC"))
    end subroutine fromStringBasicC

    pure subroutine fromStringBasicS(string, errors, power)
        type(VARYING_STRING), intent(in) :: string
        type(ErrorList_t), intent(out) :: errors
        type(Power_t), intent(out) :: power

        type(ErrorList_t) :: errors_

        call fromString( &
                string, PROVIDED_UNITS, errors_, power)
        call errors%appendErrors( &
                errors_, &
                Module_("Power_m"), &
                Procedure_("fromStringBasicS"))
    end subroutine fromStringBasicS

    pure subroutine fromStringWithUnitsC(string, units, errors, power)
        character(len=*), intent(in) :: string
        class(PowerUnit_t), intent(in) :: units(:)
        type(ErrorList_t), intent(out) :: errors
        type(Power_t), intent(out) :: power

        type(ErrorList_t) :: errors_

        call fromString( &
                var_str(string), units, errors_, power)
        call errors%appendErrors( &
                errors_, &
                Module_("Power_m"), &
                Procedure_("fromStringWithUnitsC"))
    end subroutine fromStringWithUnitsC

    pure subroutine fromStringWithUnitsS(string, units, errors, power)
        type(VARYING_STRING), intent(in) :: string
        class(PowerUnit_t), intent(in) :: units(:)
        type(ErrorList_t), intent(out) :: errors
        type(Power_t), intent(out) :: power

        type(ErrorList_t) :: all_errors(size(units))
        integer :: i

        do i = 1, size(units)
            call units(i)%parseAs(string, all_errors(i), power)
            if (.not. all_errors(i)%hasAny()) return
        end do
        do i = 1, size(units)
            call errors%appendErrors( &
                    all_errors(i), &
                    Module_("Power_m"), &
                    Procedure_("fromStringWithUnitsS"))
        end do
    end subroutine fromStringWithUnitsS

    elemental function fromUnits(value_, units) result(power)
        double precision, intent(in) :: value_
        class(PowerUnit_t), intent(in) :: units
        type(Power_t) :: power

        power%watts = value_ / units%conversion_factor
    end function fromUnits

    elemental function toUnits(self, units) result(power)
        class(Power_t), intent(in) :: self
        class(PowerUnit_t), intent(in) :: units
        double precision :: power

        power = self%watts * units%conversion_factor
    end function toUnits

    elemental function doubleTimesPower( &
            multiplier, power) result(new_power)
        double precision, intent(in) :: multiplier
        class(Power_t), intent(in) :: power
        type(Power_t) :: new_power

        new_power%watts = &
                multiplier * power%watts
    end function doubleTimesPower

    elemental function integerTimesPower( &
            multiplier, power) result(new_power)
        integer, intent(in) :: multiplier
        class(Power_t), intent(in) :: power
        type(Power_t) :: new_power

        new_power%watts = &
                dble(multiplier) * power%watts
    end function integerTimesPower

    elemental function powerTimesDouble( &
            power, multiplier) result(new_power)
        class(Power_t), intent(in) :: power
        double precision, intent(in) :: multiplier
        type(Power_t) :: new_power

        new_power%watts = &
                power%watts * multiplier
    end function powerTimesDouble

    elemental function powerTimesInteger( &
            power, multiplier) result(new_power)
        class(Power_t), intent(in) :: power
        integer, intent(in) :: multiplier
        type(Power_t) :: new_power

        new_power%watts = &
                power%watts * dble(multiplier)
    end function powerTimesInteger

    elemental function powerDividedByDouble( &
            power, divisor) result(new_power)
        class(Power_t), intent(in) :: power
        double precision, intent(in) :: divisor
        type(Power_t) :: new_power

        new_power%watts = &
                power%watts / divisor
    end function powerDividedByDouble

    elemental function powerDividedByInteger( &
            power, divisor) result(new_power)
        class(Power_t), intent(in) :: power
        integer, intent(in) :: divisor
        type(Power_t) :: new_power

        new_power%watts = &
                power%watts / dble(divisor)
    end function powerDividedByInteger

    elemental function powerDividedByPower( &
            numerator, denomenator) result(ratio)
        class(Power_t), intent(in) :: numerator
        class(Power_t), intent(in) :: denomenator
        double precision :: ratio

        ratio = numerator%watts / denomenator%watts
    end function powerDividedByPower

    elemental function powerPlusPower( &
            power1, power2) result(new_power)
        class(Power_t), intent(in) :: power1
        class(Power_t), intent(in) :: power2
        type(Power_t) :: new_power

        new_power%watts = &
                power1%watts + power2%watts
    end function powerPlusPower

    elemental function powerMinusPower( &
            power1, power2) result(new_power)
        class(Power_t), intent(in) :: power1
        class(Power_t), intent(in) :: power2
        type(Power_t) :: new_power

        new_power%watts = &
                power1%watts - power2%watts
    end function powerMinusPower

    pure function sumPower(powers)
        type(Power_t), intent(in) :: powers(:)
        type(Power_t) :: sumPower

        sumPower%watts = sum(powers%watts)
    end function sumPower

    elemental function greaterThan(lhs, rhs)
        class(Power_t), intent(in) :: lhs
        class(Power_t), intent(in) :: rhs
        logical :: greaterThan

        greaterThan = lhs%watts > rhs%watts
    end function greaterThan

    elemental function lessThan(lhs,rhs)
        class(Power_t), intent(in) :: lhs
        class(Power_t), intent(in) :: rhs
        logical :: lessThan

        lessThan = lhs%watts < rhs%watts
    end function lessThan

    elemental function greaterThanOrEqual(lhs, rhs)
        class(Power_t), intent(in) :: lhs
        class(Power_t), intent(in) :: rhs
        logical :: greaterThanOrEqual

        greaterThanOrEqual = lhs%watts >= rhs%watts
    end function greaterThanOrEqual

    elemental function lessThanOrEqual(lhs, rhs)
        class(Power_t), intent(in) :: lhs
        class(Power_t), intent(in) :: rhs
        logical :: lessThanOrEqual

        lessThanOrEqual = lhs%watts <= rhs%watts
    end function lessThanOrEqual

    elemental function equal_(lhs,rhs)
        class(Power_t), intent(in) :: lhs
        class(Power_t), intent(in) :: rhs
        logical :: equal_

        equal_ = lhs%watts .safeEq. rhs%watts
    end function equal_

    elemental function equalWithinAbsolute(lhs, rhs, within)
        class(Power_t), intent(in) :: lhs
        class(Power_t), intent(in) :: rhs
        class(Power_t), intent(in) :: within
        logical :: equalWithinAbsolute

        equalWithinAbsolute = equalWithinAbsolute_( &
                lhs%watts, rhs%watts, within%watts)
    end function equalWithinAbsolute

    elemental function equalWithinRelative(lhs, rhs, within)
        class(Power_t), intent(in) :: lhs
        class(Power_t), intent(in) :: rhs
        double precision, intent(in) :: within
        logical :: equalWithinRelative

        equalWithinRelative = equalWithinRelative_( &
                lhs%watts, rhs%watts, within)
    end function equalWithinRelative

    elemental function notEqual(lhs, rhs)
        class(Power_t), intent(in) :: lhs
        class(Power_t), intent(in) :: rhs
        logical :: notEqual

        notEqual = .not. lhs == rhs
    end function notEqual

    elemental function toStringFullPrecision(self) result(string)
        class(Power_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = self%toStringIn(DEFAULT_OUTPUT_UNITS)
    end function toStringFullPrecision

    elemental function toStringWithPrecision(self, significant_digits) result(string)
        class(Power_t), intent(in) :: self
        integer, intent(in) :: significant_digits
        type(VARYING_STRING) :: string

        string = self%toStringIn(DEFAULT_OUTPUT_UNITS, significant_digits)
    end function toStringWithPrecision

    elemental function toStringInFullPrecision(self, units) result(string)
        class(Power_t), intent(in) :: self
        class(PowerUnit_t), intent(in) :: units
        type(VARYING_STRING) :: string

        string = units%toString(toString(self.in.units))
    end function toStringInFullPrecision

    elemental function toStringInWithPrecision( &
            self, units, significant_digits) result(string)
        class(Power_t), intent(in) :: self
        class(PowerUnit_t), intent(in) :: units
        integer, intent(in) :: significant_digits
        type(VARYING_STRING) :: string

        string = units%toString(toString(self.in.units, significant_digits))
    end function toStringInWithPrecision

    elemental function simpleUnitToString(self) result(string)
        class(PowerSimpleUnit_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = trim(self%symbol)
    end function simpleUnitToString

    pure function simpleValueToString(self, value_) result(string)
        class(PowerSimpleUnit_t), intent(in) :: self
        type(VARYING_STRING), intent(in) :: value_
        type(VARYING_STRING) :: string

        string = value_ // " " // self%toString()
    end function simpleValueToString

    pure subroutine simpleParseAs(self, string, errors, power)
        class(PowerSimpleUnit_t), intent(in) :: self
        type(VARYING_STRING), intent(in) :: string
        type(ErrorList_t), intent(out) :: errors
        type(Power_t), intent(out) :: power

        type(ParseResult_t) :: parse_result

        parse_result = parseWith(theParser, string)
        if (parse_result%ok) then
            select type (the_number => parse_result%parsed)
            type is (ParsedRational_t)
                power = the_number%value_.unit.self
            end select
        else
            call errors%appendError(Fatal( &
                    PARSE_ERROR, &
                    Module_("Power_m"), &
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
        type(PowerSimpleUnit_t), intent(out) :: unit

        type(ErrorList_t) :: errors_

        call fromString(var_str(string), PROVIDED_UNITS, errors_, unit)
        call errors%appendErrors( &
                errors_, &
                Module_("Power_m"), &
                Procedure_("simpleUnitFromStringC"))
    end subroutine simpleUnitFromStringC

    pure subroutine simpleUnitFromStringS(string, errors, unit)
        type(VARYING_STRING), intent(in) :: string
        type(ErrorList_t), intent(out) :: errors
        type(PowerSimpleUnit_t), intent(out) :: unit

        type(ErrorList_t) :: errors_

        call fromString(string, PROVIDED_UNITS, errors_, unit)
        call errors%appendErrors( &
                errors_, &
                Module_("Power_m"), &
                Procedure_("simpleUnitFromStringS"))
    end subroutine simpleUnitFromStringS

    pure subroutine simpleUnitFromStringWithUnitsC(string, units, errors, unit)
        character(len=*), intent(in) :: string
        type(PowerSimpleUnit_t), intent(in) :: units(:)
        type(ErrorList_t), intent(out) :: errors
        type(PowerSimpleUnit_t), intent(out) :: unit

        type(ErrorList_t) :: errors_

        call fromString(var_str(string), units, errors_, unit)
        call errors%appendErrors( &
                errors_, &
                Module_("Power_m"), &
                Procedure_("simpleUnitFromStringWithUnitsC"))
    end subroutine simpleUnitFromStringWithUnitsC

    pure subroutine simpleUnitFromStringWithUnitsS(string, units, errors, unit)
        type(VARYING_STRING), intent(in) :: string
        type(PowerSimpleUnit_t), intent(in) :: units(:)
        type(ErrorList_t), intent(out) :: errors
        type(PowerSimpleUnit_t), intent(out) :: unit

        type(ErrorList_t) :: errors_
        integer :: which_unit

        call selectUnit(string, units, errors, which_unit)
        if (errors_%hasAny()) then
            call errors%appendErrors( &
                    errors_, &
                    Module_("Power_m"), &
                    Procedure_("simpleUnitFromStringWithUnitsS"))
        else
            unit = units(which_unit)
        end if
    end subroutine simpleUnitFromStringWithUnitsS

    pure subroutine selectUnit(string, units, errors, index)
        type(VARYING_STRING), intent(in) :: string
        class(PowerUnit_t), intent(in) :: units(:)
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
                Module_("Power_m"), &
                Procedure_("selectUnit"), &
                '"' // string // '", known units: [' // join(unit_strings, ', ') // ']'))
    end subroutine selectUnit
end module Power_m
