module Length_m
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
            CENTIMETERS_PER_METER, &
            FEET_PER_METER, &
            INCHES_PER_METER, &
            MICROINCHES_PER_METER, &
            MICROMETERS_PER_METER
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

    type, public :: Length_t
        double precision :: meters
    contains
        private
        procedure :: toUnits
        generic, public :: operator(.in.) => toUnits
        procedure, pass(length) :: doubleTimesLength
        procedure, pass(length) :: integerTimesLength
        procedure, pass(length) :: lengthTimesDouble
        procedure, pass(length) :: lengthTimesInteger
        generic, public :: operator(*) => &
                doubleTimesLength, &
                integerTimesLength, &
                lengthTimesDouble, &
                lengthTimesInteger
        procedure :: lengthDividedByDouble
        procedure :: lengthDividedByInteger
        procedure, pass(numerator) :: lengthDividedByLength
        generic, public :: operator(/) => &
                lengthDividedByDouble, &
                lengthDividedByInteger, &
                lengthDividedByLength
        procedure :: lengthPlusLength
        generic, public :: operator(+) => lengthPlusLength
        procedure :: lengthMinusLength
        generic, public :: operator(-) => lengthMinusLength
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
    end type Length_t

    type, abstract, public :: LengthUnit_t
        double precision :: conversion_factor
    contains
        procedure(justUnitToString), deferred :: unitToString
        procedure(unitWithValueToString), deferred :: valueToString
        generic :: toString => unitToString, valueToString
        procedure(parseAsI), deferred :: parseAs
    end type LengthUnit_t

    type, extends(LengthUnit_t), public :: LengthSimpleUnit_t
        character(len=20) :: symbol
    contains
        procedure :: unitToString => simpleUnitToString
        procedure :: valueToString => simpleValueToString
        procedure :: parseAs => simpleParseAs
    end type LengthSimpleUnit_t

    abstract interface
        elemental function justUnitToString(self) result(string)
            import LengthUnit_t, VARYING_STRING
            class(LengthUnit_t), intent(in) :: self
            type(VARYING_STRING) :: string
        end function justUnitToString

        pure function unitWithValueToString(self, value_) result(string)
            import LengthUnit_t, VARYING_STRING
            class(LengthUnit_t), intent(in) :: self
            type(VARYING_STRING), intent(in) :: value_
            type(VARYING_STRING) :: string
        end function unitWithValueToString

        pure subroutine parseAsI(self, string, errors, length)
            import ErrorList_t, Length_t, LengthUnit_t, VARYING_STRING
            class(LengthUnit_t), intent(in) :: self
            type(VARYING_STRING), intent(in) :: string
            type(ErrorList_t), intent(out) :: errors
            type(Length_t), intent(out) :: length
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
        module procedure sumLength
    end interface sum

    type(LengthSimpleUnit_t), parameter, public :: CENTIMETERS = &
            LengthSimpleUnit_t( &
                    conversion_factor = CENTIMETERS_PER_METER, &
                    symbol = "cm")
    type(LengthSimpleUnit_t), parameter, public :: FEET = &
            LengthSimpleUnit_t( &
                    conversion_factor = FEET_PER_METER, &
                    symbol = "ft")
    type(LengthSimpleUnit_t), parameter, public :: INCHES = &
            LengthSimpleUnit_t( &
                    conversion_factor = INCHES_PER_METER, &
                    symbol = "in")
    type(LengthSimpleUnit_t), parameter, public :: METERS = &
            LengthSimpleUnit_t( &
                    conversion_factor = 1.0d0, &
                    symbol = "m")
    type(LengthSimpleUnit_t), parameter, public :: MICROINCHES = &
            LengthSimpleUnit_t( &
                    conversion_factor = MICROINCHES_PER_METER, &
                    symbol = "uin")
    type(LengthSimpleUnit_t), parameter, public :: MICROMETERS = &
            LengthSimpleUnit_t( &
                    conversion_factor = MICROMETERS_PER_METER, &
                    symbol = "um")

    type(LengthSimpleUnit_t), public :: DEFAULT_OUTPUT_UNITS = METERS

    type(LengthSimpleUnit_t), parameter, public :: PROVIDED_UNITS(*) = &
            [CENTIMETERS, FEET, INCHES, METERS, MICROINCHES, MICROMETERS]

    public :: operator(.unit.), fromString, selectUnit, sum
contains
    pure subroutine fromStringBasicC(string, errors, length)
        character(len=*), intent(in) :: string
        type(ErrorList_t), intent(out) :: errors
        type(Length_t), intent(out) :: length

        type(ErrorList_t) :: errors_

        call fromString( &
                var_str(string), PROVIDED_UNITS, errors_, length)
        call errors%appendErrors( &
                errors_, &
                Module_("Length_m"), &
                Procedure_("fromStringBasicC"))
    end subroutine fromStringBasicC

    pure subroutine fromStringBasicS(string, errors, length)
        type(VARYING_STRING), intent(in) :: string
        type(ErrorList_t), intent(out) :: errors
        type(Length_t), intent(out) :: length

        type(ErrorList_t) :: errors_

        call fromString( &
                string, PROVIDED_UNITS, errors_, length)
        call errors%appendErrors( &
                errors_, &
                Module_("Length_m"), &
                Procedure_("fromStringBasicS"))
    end subroutine fromStringBasicS

    pure subroutine fromStringWithUnitsC(string, units, errors, length)
        character(len=*), intent(in) :: string
        class(LengthUnit_t), intent(in) :: units(:)
        type(ErrorList_t), intent(out) :: errors
        type(Length_t), intent(out) :: length

        type(ErrorList_t) :: errors_

        call fromString( &
                var_str(string), units, errors_, length)
        call errors%appendErrors( &
                errors_, &
                Module_("Length_m"), &
                Procedure_("fromStringWithUnitsC"))
    end subroutine fromStringWithUnitsC

    pure subroutine fromStringWithUnitsS(string, units, errors, length)
        type(VARYING_STRING), intent(in) :: string
        class(LengthUnit_t), intent(in) :: units(:)
        type(ErrorList_t), intent(out) :: errors
        type(Length_t), intent(out) :: length

        type(ErrorList_t) :: all_errors(size(units))
        integer :: i

        do i = 1, size(units)
            call units(i)%parseAs(string, all_errors(i), length)
            if (.not. all_errors(i)%hasAny()) return
        end do
        do i = 1, size(units)
            call errors%appendErrors( &
                    all_errors(i), &
                    Module_("Length_m"), &
                    Procedure_("fromStringWithUnitsS"))
        end do
    end subroutine fromStringWithUnitsS

    elemental function fromUnits(value_, units) result(length)
        double precision, intent(in) :: value_
        class(LengthUnit_t), intent(in) :: units
        type(Length_t) :: length

        length%meters = value_ / units%conversion_factor
    end function fromUnits

    elemental function toUnits(self, units) result(length)
        class(Length_t), intent(in) :: self
        class(LengthUnit_t), intent(in) :: units
        double precision :: length

        length = self%meters * units%conversion_factor
    end function toUnits

    elemental function doubleTimesLength( &
            multiplier, length) result(new_length)
        double precision, intent(in) :: multiplier
        class(Length_t), intent(in) :: length
        type(Length_t) :: new_length

        new_length%meters = &
                multiplier * length%meters
    end function doubleTimesLength

    elemental function integerTimesLength( &
            multiplier, length) result(new_length)
        integer, intent(in) :: multiplier
        class(Length_t), intent(in) :: length
        type(Length_t) :: new_length

        new_length%meters = &
                dble(multiplier) * length%meters
    end function integerTimesLength

    elemental function lengthTimesDouble( &
            length, multiplier) result(new_length)
        class(Length_t), intent(in) :: length
        double precision, intent(in) :: multiplier
        type(Length_t) :: new_length

        new_length%meters = &
                length%meters * multiplier
    end function lengthTimesDouble

    elemental function lengthTimesInteger( &
            length, multiplier) result(new_length)
        class(Length_t), intent(in) :: length
        integer, intent(in) :: multiplier
        type(Length_t) :: new_length

        new_length%meters = &
                length%meters * dble(multiplier)
    end function lengthTimesInteger

    elemental function lengthDividedByDouble( &
            length, divisor) result(new_length)
        class(Length_t), intent(in) :: length
        double precision, intent(in) :: divisor
        type(Length_t) :: new_length

        new_length%meters = &
                length%meters / divisor
    end function lengthDividedByDouble

    elemental function lengthDividedByInteger( &
            length, divisor) result(new_length)
        class(Length_t), intent(in) :: length
        integer, intent(in) :: divisor
        type(Length_t) :: new_length

        new_length%meters = &
                length%meters / dble(divisor)
    end function lengthDividedByInteger

    elemental function lengthDividedByLength( &
            numerator, denomenator) result(ratio)
        class(Length_t), intent(in) :: numerator
        class(Length_t), intent(in) :: denomenator
        double precision :: ratio

        ratio = numerator%meters / denomenator%meters
    end function lengthDividedByLength

    elemental function lengthPlusLength( &
            length1, length2) result(new_length)
        class(Length_t), intent(in) :: length1
        class(Length_t), intent(in) :: length2
        type(Length_t) :: new_length

        new_length%meters = &
                length1%meters + length2%meters
    end function lengthPlusLength

    elemental function lengthMinusLength( &
            length1, length2) result(new_length)
        class(Length_t), intent(in) :: length1
        class(Length_t), intent(in) :: length2
        type(Length_t) :: new_length

        new_length%meters = &
                length1%meters - length2%meters
    end function lengthMinusLength

    pure function sumLength(lengths)
        type(Length_t), intent(in) :: lengths(:)
        type(Length_t) :: sumLength

        sumLength%meters = sum(lengths%meters)
    end function sumLength

    elemental function greaterThan(lhs, rhs)
        class(Length_t), intent(in) :: lhs
        class(Length_t), intent(in) :: rhs
        logical :: greaterThan

        greaterThan = lhs%meters > rhs%meters
    end function greaterThan

    elemental function lessThan(lhs,rhs)
        class(Length_t), intent(in) :: lhs
        class(Length_t), intent(in) :: rhs
        logical :: lessThan

        lessThan = lhs%meters < rhs%meters
    end function lessThan

    elemental function greaterThanOrEqual(lhs, rhs)
        class(Length_t), intent(in) :: lhs
        class(Length_t), intent(in) :: rhs
        logical :: greaterThanOrEqual

        greaterThanOrEqual = lhs%meters >= rhs%meters
    end function greaterThanOrEqual

    elemental function lessThanOrEqual(lhs, rhs)
        class(Length_t), intent(in) :: lhs
        class(Length_t), intent(in) :: rhs
        logical :: lessThanOrEqual

        lessThanOrEqual = lhs%meters <= rhs%meters
    end function lessThanOrEqual

    elemental function equal_(lhs,rhs)
        class(Length_t), intent(in) :: lhs
        class(Length_t), intent(in) :: rhs
        logical :: equal_

        equal_ = lhs%meters .safeEq. rhs%meters
    end function equal_

    elemental function equalWithinAbsolute(lhs, rhs, within)
        class(Length_t), intent(in) :: lhs
        class(Length_t), intent(in) :: rhs
        class(Length_t), intent(in) :: within
        logical :: equalWithinAbsolute

        equalWithinAbsolute = equalWithinAbsolute_( &
                lhs%meters, rhs%meters, within%meters)
    end function equalWithinAbsolute

    elemental function equalWithinRelative(lhs, rhs, within)
        class(Length_t), intent(in) :: lhs
        class(Length_t), intent(in) :: rhs
        double precision, intent(in) :: within
        logical :: equalWithinRelative

        equalWithinRelative = equalWithinRelative_( &
                lhs%meters, rhs%meters, within)
    end function equalWithinRelative

    elemental function notEqual(lhs, rhs)
        class(Length_t), intent(in) :: lhs
        class(Length_t), intent(in) :: rhs
        logical :: notEqual

        notEqual = .not. lhs == rhs
    end function notEqual

    elemental function toStringFullPrecision(self) result(string)
        class(Length_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = self%toStringIn(DEFAULT_OUTPUT_UNITS)
    end function toStringFullPrecision

    elemental function toStringWithPrecision(self, significant_digits) result(string)
        class(Length_t), intent(in) :: self
        integer, intent(in) :: significant_digits
        type(VARYING_STRING) :: string

        string = self%toStringIn(DEFAULT_OUTPUT_UNITS, significant_digits)
    end function toStringWithPrecision

    elemental function toStringInFullPrecision(self, units) result(string)
        class(Length_t), intent(in) :: self
        class(LengthUnit_t), intent(in) :: units
        type(VARYING_STRING) :: string

        string = units%toString(toString(self.in.units))
    end function toStringInFullPrecision

    elemental function toStringInWithPrecision( &
            self, units, significant_digits) result(string)
        class(Length_t), intent(in) :: self
        class(LengthUnit_t), intent(in) :: units
        integer, intent(in) :: significant_digits
        type(VARYING_STRING) :: string

        string = units%toString(toString(self.in.units, significant_digits))
    end function toStringInWithPrecision

    elemental function simpleUnitToString(self) result(string)
        class(LengthSimpleUnit_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = trim(self%symbol)
    end function simpleUnitToString

    pure function simpleValueToString(self, value_) result(string)
        class(LengthSimpleUnit_t), intent(in) :: self
        type(VARYING_STRING), intent(in) :: value_
        type(VARYING_STRING) :: string

        string = value_ // " " // self%toString()
    end function simpleValueToString

    pure subroutine simpleParseAs(self, string, errors, length)
        class(LengthSimpleUnit_t), intent(in) :: self
        type(VARYING_STRING), intent(in) :: string
        type(ErrorList_t), intent(out) :: errors
        type(Length_t), intent(out) :: length

        type(ParseResult_t) :: parse_result

        parse_result = parseWith(theParser, string)
        if (parse_result%ok) then
            select type (the_number => parse_result%parsed)
            type is (ParsedRational_t)
                length = the_number%value_.unit.self
            end select
        else
            call errors%appendError(Fatal( &
                    PARSE_ERROR, &
                    Module_("Length_m"), &
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
        type(LengthSimpleUnit_t), intent(out) :: unit

        type(ErrorList_t) :: errors_

        call fromString(var_str(string), PROVIDED_UNITS, errors_, unit)
        call errors%appendErrors( &
                errors_, &
                Module_("Length_m"), &
                Procedure_("simpleUnitFromStringC"))
    end subroutine simpleUnitFromStringC

    pure subroutine simpleUnitFromStringS(string, errors, unit)
        type(VARYING_STRING), intent(in) :: string
        type(ErrorList_t), intent(out) :: errors
        type(LengthSimpleUnit_t), intent(out) :: unit

        type(ErrorList_t) :: errors_

        call fromString(string, PROVIDED_UNITS, errors_, unit)
        call errors%appendErrors( &
                errors_, &
                Module_("Length_m"), &
                Procedure_("simpleUnitFromStringS"))
    end subroutine simpleUnitFromStringS

    pure subroutine simpleUnitFromStringWithUnitsC(string, units, errors, unit)
        character(len=*), intent(in) :: string
        type(LengthSimpleUnit_t), intent(in) :: units(:)
        type(ErrorList_t), intent(out) :: errors
        type(LengthSimpleUnit_t), intent(out) :: unit

        type(ErrorList_t) :: errors_

        call fromString(var_str(string), units, errors_, unit)
        call errors%appendErrors( &
                errors_, &
                Module_("Length_m"), &
                Procedure_("simpleUnitFromStringWithUnitsC"))
    end subroutine simpleUnitFromStringWithUnitsC

    pure subroutine simpleUnitFromStringWithUnitsS(string, units, errors, unit)
        type(VARYING_STRING), intent(in) :: string
        type(LengthSimpleUnit_t), intent(in) :: units(:)
        type(ErrorList_t), intent(out) :: errors
        type(LengthSimpleUnit_t), intent(out) :: unit

        type(ErrorList_t) :: errors_
        integer :: which_unit

        call selectUnit(string, units, errors, which_unit)
        if (errors_%hasAny()) then
            call errors%appendErrors( &
                    errors_, &
                    Module_("Length_m"), &
                    Procedure_("simpleUnitFromStringWithUnitsS"))
        else
            unit = units(which_unit)
        end if
    end subroutine simpleUnitFromStringWithUnitsS

    pure subroutine selectUnit(string, units, errors, index)
        type(VARYING_STRING), intent(in) :: string
        class(LengthUnit_t), intent(in) :: units(:)
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
                Module_("Length_m"), &
                Procedure_("selectUnit"), &
                '"' // string // '", known units: [' // join(unit_strings, ', ') // ']'))
    end subroutine selectUnit
end module Length_m
