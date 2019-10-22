module Length_m
    use Conversion_factors_m, only: CENTIMETERS_PER_METER

    implicit none
    private

    type, public :: Length_t
        private
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
        procedure, public :: toString
        procedure, public :: toStringIn
    end type Length_t

    type, public :: LengthUnit_t
        double precision :: conversion_factor
        character(len=10) :: symbol
    contains
        procedure :: toString => unitToString
    end type LengthUnit_t

    interface operator(.unit.)
        module procedure fromUnits
    end interface operator(.unit.)

    interface lengthFromString
        module procedure fromStringBasicC
        module procedure fromStringBasicS
        module procedure fromStringWithUnitsC
        module procedure fromStringWithUnitsS
    end interface lengthFromString

    interface lengthUnitFromString
        module procedure unitFromStringBasicC
        module procedure unitFromStringBasicS
        module procedure unitFromStringWithUnitsC
        module procedure unitFromStringWithUnitsS
    end interface lengthUnitFromString

    type(LengthUnit_t), parameter, public :: CENTIMETERS = LengthUnit_t( &
            conversion_factor = CENTIMETERS_PER_METER, &
            symbol = "cm")
    type(LengthUnit_t), parameter, public :: METERS = LengthUnit_t( &
            conversion_factor = 1.0d0, &
            symbol = "m")

    type(LengthUnit_t), public :: DEFAULT_OUTPUT_UNITS = METERS

    type(LengthUnit_t), parameter, public :: PROVIDED_UNITS(*) = &
            [CENTIMETERS, METERS]

    public :: &
            operator(.unit.), &
            lengthFromString, &
            lengthUnitFromString
contains
    function fromStringBasicC(string, errors) result(length)
        use Error_list_m, only: ErrorList_t
        use iso_varying_string, only: var_str
        use Module_m, only: Module_
        use Procedure_m, only: Procedure_

        character(len=*), intent(in) :: string
        type(ErrorList_t), intent(out) :: errors
        type(Length_t) :: length

        type(ErrorList_t) :: errors_

        length = lengthFromString( &
                var_str(string), PROVIDED_UNITS, errors_)
        call errors%appendErrors( &
                errors_, &
                Module_("Length_m"), &
                Procedure_("fromStringBasicC"))
    end function fromStringBasicC

    function fromStringBasicS(string, errors) result(length)
        use Error_list_m, only: ErrorList_t
        use iso_varying_string, only: VARYING_STRING
        use Module_m, only: Module_
        use Procedure_m, only: Procedure_

        type(VARYING_STRING), intent(in) :: string
        type(ErrorList_t), intent(out) :: errors
        type(Length_t) :: length

        type(ErrorList_t) :: errors_

        length = lengthFromString( &
                string, PROVIDED_UNITS, errors_)
        call errors%appendErrors( &
                errors_, &
                Module_("Length_m"), &
                Procedure_("fromStringBasicS"))
    end function fromStringBasicS

    function fromStringWithUnitsC(string, units, errors) result(length)
        use Error_list_m, only: ErrorList_t
        use iso_varying_string, only: var_str
        use Module_m, only: Module_
        use Procedure_m, only: Procedure_

        character(len=*), intent(in) :: string
        type(LengthUnit_t), intent(in) :: units(:)
        type(ErrorList_t), intent(out) :: errors
        type(Length_t) :: length

        type(ErrorList_t) :: errors_

        length = lengthFromString( &
                var_str(string), units, errors_)
        call errors%appendErrors( &
                errors_, &
                Module_("Length_m"), &
                Procedure_("fromStringWithUnitsC"))
    end function fromStringWithUnitsC

    function fromStringWithUnitsS(string, units, errors) result(length)
        use Error_list_m, only: ErrorList_t
        use iso_varying_string, only: &
                VARYING_STRING, &
                assignment(=), &
                operator(//), &
                operator(==), &
                len, &
                split
        use Message_m, only: Fatal
        use Miscellaneous_m, only: PARSE_ERROR
        use Module_m, only: Module_
        use Procedure_m, only: Procedure_
        use strff, only: join

        type(VARYING_STRING), intent(in) :: string
        type(LengthUnit_t), intent(in) :: units(:)
        type(ErrorList_t), intent(out) :: errors
        type(Length_t) :: length

        double precision :: number
        character(len=100) :: number_chars
        type(VARYING_STRING) :: number_string
        integer :: status
        type(VARYING_STRING) :: symbol
        type(LengthUnit_t) :: unit
        type(ErrorList_t) :: unit_errors

        length%meters = 0.0d0
        symbol = string
        call split(symbol, number_string, " ")
        if (len(symbol) == 0) then
            call errors%appendError(Fatal( &
                    PARSE_ERROR, &
                    Module_("Length_m"), &
                    Procedure_("fromStringWithUnitsS"), &
                    'No unit symbol found in string "' // string // '"'))
            return
        end if
        number_chars = number_string
        read(number_chars, *, iostat=status) number
        if (status /= 0) then
            call errors%appendError(Fatal( &
                    PARSE_ERROR, &
                    Module_("Length_m"), &
                    Procedure_("fromStringWithUnitsS"), &
                    'Error parsing number from string "' // number_string // '"'))
        end if
        unit = lengthUnitFromString(symbol, units, unit_errors)
        length = number.unit.unit
        call errors%appendErrors( &
                unit_errors, &
                Module_("Length_m"), &
                Procedure_("fromStringWithUnitsS"))
    end function fromStringWithUnitsS

    elemental function fromUnits(value_, units) result(length)
        double precision, intent(in) :: value_
        type(LengthUnit_t), intent(in) :: units
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
        use Miscellaneous_m, only: operator(.safeEq.)

        class(Length_t), intent(in) :: lhs
        class(Length_t), intent(in) :: rhs
        logical :: equal_

        equal_ = lhs%meters .safeEq. rhs%meters
    end function equal_

    function equalWithinAbsolute(lhs, rhs, within)
        use Miscellaneous_m, only: equalWithinAbsolute_ => equalWithinAbsolute

        class(Length_t), intent(in) :: lhs
        class(Length_t), intent(in) :: rhs
        class(Length_t), intent(in) :: within
        logical :: equalWithinAbsolute

        equalWithinAbsolute = equalWithinAbsolute_( &
                lhs%meters, rhs%meters, within%meters)
    end function equalWithinAbsolute

    function equalWithinRelative(lhs, rhs, within)
        use Miscellaneous_m, only: equalWithinRelative_ => equalWithinRelative

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

    function toString(self) result(string)
        use iso_varying_string, only: VARYING_STRING

        class(Length_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = self%toStringIn(DEFAULT_OUTPUT_UNITS)
    end function toString

    function toStringIn(self, units) result(string)
        use iso_varying_string, only: VARYING_STRING, operator(//)
        use strff, only: toString

        class(Length_t), intent(in) :: self
        class(LengthUnit_t), intent(in) :: units
        type(VARYING_STRING) :: string

        string = toString(self.in.units) // " " // units%toString()
    end function toStringIn

    function unitFromStringBasicC(string, errors) result(unit)
        use Error_list_m, only: ErrorList_t
        use iso_varying_string, only: var_str
        use Module_m, only: Module_
        use Procedure_m, only: Procedure_

        character(len=*), intent(in) :: string
        type(ErrorList_t), intent(out) :: errors
        type(LengthUnit_t) :: unit

        type(ErrorList_t) :: errors_

        unit = lengthUnitFromString( &
                var_str(string), PROVIDED_UNITS, errors_)
        call errors%appendErrors( &
                errors_, &
                Module_("Length_m"), &
                Procedure_("unitFromStringBasicC"))
    end function unitFromStringBasicC

    function unitFromStringBasicS(string, errors) result(unit)
        use Error_list_m, only: ErrorList_t
        use iso_varying_string, only: VARYING_STRING
        use Module_m, only: Module_
        use Procedure_m, only: Procedure_

        type(VARYING_STRING), intent(in) :: string
        type(ErrorList_t), intent(out) :: errors
        type(LengthUnit_t) :: unit

        type(ErrorList_t) :: errors_

        unit = lengthUnitFromString( &
                string, PROVIDED_UNITS, errors_)
        call errors%appendErrors( &
                errors_, &
                Module_("Length_m"), &
                Procedure_("unitFromStringBasicS"))
    end function unitFromStringBasicS

    function unitFromStringWithUnitsC(string, units, errors) result(unit)
        use Error_list_m, only: ErrorList_t
        use iso_varying_string, only: var_str
        use Module_m, only: Module_
        use Procedure_m, only: Procedure_

        character(len=*), intent(in) :: string
        type(LengthUnit_t), intent(in) :: units(:)
        type(ErrorList_t), intent(out) :: errors
        type(LengthUnit_t) :: unit

        type(ErrorList_t) :: errors_

        unit = lengthUnitFromString(var_str(string), units, errors_)
        call errors%appendErrors( &
                errors_, &
                Module_("Length_m"), &
                Procedure_("unitFromStringWithUnitsC"))
    end function unitFromStringWithUnitsC

    function unitFromStringWithUnitsS(string, units, errors) result(unit)
        use Error_list_m, only: ErrorList_t
        use iso_varying_string, only: VARYING_STRING, operator(==), operator(//)
        use Message_m, only: Fatal
        use Miscellaneous_m, only: UNKNOWN_UNIT
        use Module_m, only: Module_
        use Procedure_m, only: Procedure_
        use strff, only: join

        type(VARYING_STRING), intent(in) :: string
        type(LengthUnit_t), intent(in) :: units(:)
        type(ErrorList_t), intent(out) :: errors
        type(LengthUnit_t) :: unit

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
                    Module_("Length_m"), &
                    Procedure_("unitFromStringWithUnitsS"), &
                    '"' // string // '", known units: [' // join(unit_strings, ', ') // ']' ))
        end if
    end function unitFromStringWithUnitsS

    function unitToString(self) result(string)
        use iso_varying_string, only: VARYING_STRING, assignment(=)

        class(LengthUnit_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = trim(self%symbol)
    end function unitToString
end module Length_m
