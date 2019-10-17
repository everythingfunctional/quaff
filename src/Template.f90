module QuantityCamel_m
    use Units_m, only: Unit_t

    implicit none
    private

    type, public :: QuantityCamel_t
        private
        double precision :: units_lower
    contains
        private
        procedure :: toUnits
        generic, public :: operator(.in.) => toUnits
        procedure, pass(quantity_lower) :: doubleTimesQuantityCamel
        procedure, pass(quantity_lower) :: integerTimesQuantityCamel
        procedure, pass(quantity_lower) :: quantitySnakeTimesDouble
        procedure, pass(quantity_lower) :: quantitySnakeTimesInteger
        generic, public :: operator(*) => &
                doubleTimesQuantityCamel, &
                integerTimesQuantityCamel, &
                quantitySnakeTimesDouble, &
                quantitySnakeTimesInteger
        procedure :: quantitySnakeDividedByDouble
        procedure :: quantitySnakeDividedByInteger
        procedure, pass(numerator) :: quantitySnakeDividedByQuantityCamel
        generic, public :: operator(/) => &
                quantitySnakeDividedByDouble, &
                quantitySnakeDividedByInteger, &
                quantitySnakeDividedByQuantityCamel
        procedure :: quantitySnakePlusQuantityCamel
        generic, public :: operator(+) => quantitySnakePlusQuantityCamel
        procedure :: quantitySnakeMinusQuantityCamel
        generic, public :: operator(-) => quantitySnakeMinusQuantityCamel
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
    end type QuantityCamel_t

    type, public, extends(Unit_t) :: QuantityCamelUnit_t
    contains
        procedure :: toString => unitToString
    end type QuantityCamelUnit_t

    interface operator(.unit.)
        module procedure fromUnits
    end interface operator(.unit.)

    type(QuantityCamelUnit_t), parameter, public :: UNITS_CAPITAL = QuantityCamelUnit_t( &
            multiplier = 1.0d0, &
            symbol = "symbol")

    type(QuantityCamelUnit_t), public :: DEFAULT_OUTPUT_UNITS = UNITS_CAPITAL

    type(QuantityCamelUnit_t), parameter, public :: PROVIDED_UNITS(*) = &
            [UNITS_CAPITAL]

    public :: operator(.unit.)
contains
    function fromUnits(value_, units) result(quantity_lower)
        double precision, intent(in) :: value_
        type(QuantityCamelUnit_t), intent(in) :: units
        type(QuantityCamel_t) :: quantity_lower

        quantity_lower%units_lower = value_ / units%multiplier
    end function fromUnits

    function toUnits(self, units) result(quantity_lower)
        class(QuantityCamel_t), intent(in) :: self
        class(QuantityCamelUnit_t), intent(in) :: units
        double precision :: quantity_lower

        quantity_lower = self%units_lower * units%multiplier
    end function toUnits

    function doubleTimesQuantityCamel(multiplier, quantity_lower) result(new_quantity_lower)
        double precision, intent(in) :: multiplier
        class(QuantityCamel_t), intent(in) :: quantity_lower
        type(QuantityCamel_t) :: new_quantity_lower

        new_quantity_lower%units_lower = multiplier * quantity_lower%units_lower
    end function doubleTimesQuantityCamel

    function integerTimesQuantityCamel(multiplier, quantity_lower) result(new_quantity_lower)
        integer, intent(in) :: multiplier
        class(QuantityCamel_t), intent(in) :: quantity_lower
        type(QuantityCamel_t) :: new_quantity_lower

        new_quantity_lower%units_lower = dble(multiplier) * quantity_lower%units_lower
    end function integerTimesQuantityCamel

    function quantitySnakeTimesDouble(quantity_lower, multiplier) result(new_quantity_lower)
        class(QuantityCamel_t), intent(in) :: quantity_lower
        double precision, intent(in) :: multiplier
        type(QuantityCamel_t) :: new_quantity_lower

        new_quantity_lower%units_lower = quantity_lower%units_lower * multiplier
    end function quantitySnakeTimesDouble

    function quantitySnakeTimesInteger(quantity_lower, multiplier) result(new_quantity_lower)
        class(QuantityCamel_t), intent(in) :: quantity_lower
        integer, intent(in) :: multiplier
        type(QuantityCamel_t) :: new_quantity_lower

        new_quantity_lower%units_lower = quantity_lower%units_lower * dble(multiplier)
    end function quantitySnakeTimesInteger

    function quantitySnakeDividedByDouble(quantity_lower, divisor) result(new_quantity_lower)
        class(QuantityCamel_t), intent(in) :: quantity_lower
        double precision, intent(in) :: divisor
        type(QuantityCamel_t) :: new_quantity_lower

        new_quantity_lower%units_lower = quantity_lower%units_lower / divisor
    end function quantitySnakeDividedByDouble

    function quantitySnakeDividedByInteger(quantity_lower, divisor) result(new_quantity_lower)
        class(QuantityCamel_t), intent(in) :: quantity_lower
        integer, intent(in) :: divisor
        type(QuantityCamel_t) :: new_quantity_lower

        new_quantity_lower%units_lower = quantity_lower%units_lower / dble(divisor)
    end function quantitySnakeDividedByInteger

    function quantitySnakeDividedByQuantityCamel(numerator, denomenator) result(ratio)
        class(QuantityCamel_t), intent(in) :: numerator
        class(QuantityCamel_t), intent(in) :: denomenator
        double precision :: ratio

        ratio = numerator%units_lower / denomenator%units_lower
    end function quantitySnakeDividedByQuantityCamel

    function quantitySnakePlusQuantityCamel(quantity_lower1, quantity_lower2) result(new_quantity_lower)
        class(QuantityCamel_t), intent(in) :: quantity_lower1
        class(QuantityCamel_t), intent(in) :: quantity_lower2
        type(QuantityCamel_t) :: new_quantity_lower

        new_quantity_lower%units_lower = quantity_lower1%units_lower + quantity_lower2%units_lower
    end function quantitySnakePlusQuantityCamel

    function quantitySnakeMinusQuantityCamel(quantity_lower1, quantity_lower2) result(new_quantity_lower)
        class(QuantityCamel_t), intent(in) :: quantity_lower1
        class(QuantityCamel_t), intent(in) :: quantity_lower2
        type(QuantityCamel_t) :: new_quantity_lower

        new_quantity_lower%units_lower = quantity_lower1%units_lower - quantity_lower2%units_lower
    end function quantitySnakeMinusQuantityCamel

    function greaterThan(lhs, rhs)
        class(QuantityCamel_t), intent(in) :: lhs
        class(QuantityCamel_t), intent(in) :: rhs
        logical :: greaterThan

        greaterThan = lhs%units_lower > rhs%units_lower
    end function greaterThan

    function lessThan(lhs,rhs)
        class(QuantityCamel_t), intent(in) :: lhs
        class(QuantityCamel_t), intent(in) :: rhs
        logical :: lessThan

        lessThan = lhs%units_lower < rhs%units_lower
    end function lessThan

    function greaterThanOrEqual(lhs, rhs)
        class(QuantityCamel_t), intent(in) :: lhs
        class(QuantityCamel_t), intent(in) :: rhs
        logical :: greaterThanOrEqual

        greaterThanOrEqual = lhs%units_lower >= rhs%units_lower
    end function greaterThanOrEqual

    function lessThanOrEqual(lhs, rhs)
        class(QuantityCamel_t), intent(in) :: lhs
        class(QuantityCamel_t), intent(in) :: rhs
        logical :: lessThanOrEqual

        lessThanOrEqual = lhs%units_lower <= rhs%units_lower
    end function lessThanOrEqual

    function equal_(lhs,rhs)
        use Miscellaneous_m, only: operator(.safeEq.)

        class(QuantityCamel_t), intent(in) :: lhs
        class(QuantityCamel_t), intent(in) :: rhs
        logical :: equal_

        equal_ = lhs%units_lower .safeEq. rhs%units_lower
    end function equal_

    function equalWithinAbsolute(lhs, rhs, within)
        use Miscellaneous_m, only: equalWithinAbsolute_ => equalWithinAbsolute

        class(QuantityCamel_t), intent(in) :: lhs
        class(QuantityCamel_t), intent(in) :: rhs
        class(QuantityCamel_t), intent(in) :: within
        logical :: equalWithinAbsolute

        equalWithinAbsolute = equalWithinAbsolute_( &
                lhs%units_lower, rhs%units_lower, within%units_lower)
    end function equalWithinAbsolute

    function equalWithinRelative(lhs, rhs, within)
        use Miscellaneous_m, only: equalWithinRelative_ => equalWithinRelative

        class(QuantityCamel_t), intent(in) :: lhs
        class(QuantityCamel_t), intent(in) :: rhs
        double precision, intent(in) :: within
        logical :: equalWithinRelative

        equalWithinRelative = equalWithinRelative_( &
                lhs%units_lower, rhs%units_lower, within)
    end function equalWithinRelative

    function notEqual(lhs, rhs)
        class(QuantityCamel_t), intent(in) :: lhs
        class(QuantityCamel_t), intent(in) :: rhs
        logical :: notEqual

        notEqual = .not. lhs == rhs
    end function notEqual

    function toString(self) result(string)
        use iso_varying_string, only: VARYING_STRING

        class(QuantityCamel_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = self%toStringIn(DEFAULT_OUTPUT_UNITS)
    end function toString

    function toStringIn(self, units) result(string)
        use iso_varying_string, only: VARYING_STRING, operator(//)
        use strff, only: toString

        class(QuantityCamel_t), intent(in) :: self
        class(QuantityCamelUnit_t), intent(in) :: units
        type(VARYING_STRING) :: string

        string = toString(self.in.units) // " " // units%toString()
    end function toStringIn

    function unitToString(self) result(string)
        use iso_varying_string, only: VARYING_STRING, assignment(=)

        class(QuantityCamelUnit_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = trim(self%symbol)
    end function unitToString
end module QuantityCamel_m
