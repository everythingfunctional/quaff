module Length_m
    use Units_m, only: Unit_t

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

    type, public, extends(Unit_t) :: LengthUnit_t
    contains
        procedure :: toString => unitToString
    end type LengthUnit_t

    interface operator(.unit.)
        module procedure fromUnits
    end interface operator(.unit.)

    type(LengthUnit_t), parameter, public :: CENTIMETERS = LengthUnit_t( &
            multiplier = 100.0d0, &
            symbol = "cm")
    type(LengthUnit_t), parameter, public :: METERS = LengthUnit_t( &
            multiplier = 1.0d0, &
            symbol = "m")

    type(LengthUnit_t), public :: DEFAULT_OUTPUT_UNITS = METERS

    type(LengthUnit_t), parameter, public :: PROVIDED_UNITS(*) = &
            [CENTIMETERS, METERS]

    public :: operator(.unit.)
contains
    function fromUnits(value_, units) result(length)
        double precision, intent(in) :: value_
        type(LengthUnit_t), intent(in) :: units
        type(Length_t) :: length

        length%meters = value_ / units%multiplier
    end function fromUnits

    function toUnits(self, units) result(length)
        class(Length_t), intent(in) :: self
        class(LengthUnit_t), intent(in) :: units
        double precision :: length

        length = self%meters * units%multiplier
    end function toUnits

    function doubleTimesLength(multiplier, length) result(new_length)
        double precision, intent(in) :: multiplier
        class(Length_t), intent(in) :: length
        type(Length_t) :: new_length

        new_length%meters = multiplier * length%meters
    end function doubleTimesLength

    function integerTimesLength(multiplier, length) result(new_length)
        integer, intent(in) :: multiplier
        class(Length_t), intent(in) :: length
        type(Length_t) :: new_length

        new_length%meters = dble(multiplier) * length%meters
    end function integerTimesLength

    function lengthTimesDouble(length, multiplier) result(new_length)
        class(Length_t), intent(in) :: length
        double precision, intent(in) :: multiplier
        type(Length_t) :: new_length

        new_length%meters = length%meters * multiplier
    end function lengthTimesDouble

    function lengthTimesInteger(length, multiplier) result(new_length)
        class(Length_t), intent(in) :: length
        integer, intent(in) :: multiplier
        type(Length_t) :: new_length

        new_length%meters = length%meters * dble(multiplier)
    end function lengthTimesInteger

    function lengthDividedByDouble(length, divisor) result(new_length)
        class(Length_t), intent(in) :: length
        double precision, intent(in) :: divisor
        type(Length_t) :: new_length

        new_length%meters = length%meters / divisor
    end function lengthDividedByDouble

    function lengthDividedByInteger(length, divisor) result(new_length)
        class(Length_t), intent(in) :: length
        integer, intent(in) :: divisor
        type(Length_t) :: new_length

        new_length%meters = length%meters / dble(divisor)
    end function lengthDividedByInteger

    function lengthDividedByLength(numerator, denomenator) result(ratio)
        class(Length_t), intent(in) :: numerator
        class(Length_t), intent(in) :: denomenator
        double precision :: ratio

        ratio = numerator%meters / denomenator%meters
    end function lengthDividedByLength

    function lengthPlusLength(length1, length2) result(new_length)
        class(Length_t), intent(in) :: length1
        class(Length_t), intent(in) :: length2
        type(Length_t) :: new_length

        new_length%meters = length1%meters + length2%meters
    end function lengthPlusLength

    function lengthMinusLength(length1, length2) result(new_length)
        class(Length_t), intent(in) :: length1
        class(Length_t), intent(in) :: length2
        type(Length_t) :: new_length

        new_length%meters = length1%meters - length2%meters
    end function lengthMinusLength

    function greaterThan(lhs, rhs)
        class(Length_t), intent(in) :: lhs
        class(Length_t), intent(in) :: rhs
        logical :: greaterThan

        greaterThan = lhs%meters > rhs%meters
    end function greaterThan

    function lessThan(lhs,rhs)
        class(Length_t), intent(in) :: lhs
        class(Length_t), intent(in) :: rhs
        logical :: lessThan

        lessThan = lhs%meters < rhs%meters
    end function lessThan

    function greaterThanOrEqual(lhs, rhs)
        class(Length_t), intent(in) :: lhs
        class(Length_t), intent(in) :: rhs
        logical :: greaterThanOrEqual

        greaterThanOrEqual = lhs%meters >= rhs%meters
    end function greaterThanOrEqual

    function lessThanOrEqual(lhs, rhs)
        class(Length_t), intent(in) :: lhs
        class(Length_t), intent(in) :: rhs
        logical :: lessThanOrEqual

        lessThanOrEqual = lhs%meters <= rhs%meters
    end function lessThanOrEqual

    function equal_(lhs,rhs)
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

    function notEqual(lhs, rhs)
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

    function unitToString(self) result(string)
        use iso_varying_string, only: VARYING_STRING, assignment(=)

        class(LengthUnit_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = trim(self%symbol)
    end function unitToString
end module Length_m
