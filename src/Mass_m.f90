module Mass_m
    use Conversion_factors_m, only: GRAMS_PER_KILOGRAM
    use Units_m, only: Unit_t

    implicit none
    private

    type, public :: Mass_t
        private
        double precision :: kilograms
    contains
        private
        procedure :: toUnits
        generic, public :: operator(.in.) => toUnits
        procedure, pass(mass) :: doubleTimesMass
        procedure, pass(mass) :: integerTimesMass
        procedure, pass(mass) :: massTimesDouble
        procedure, pass(mass) :: massTimesInteger
        generic, public :: operator(*) => &
                doubleTimesMass, &
                integerTimesMass, &
                massTimesDouble, &
                massTimesInteger
        procedure :: massDividedByDouble
        procedure :: massDividedByInteger
        procedure, pass(numerator) :: massDividedByMass
        generic, public :: operator(/) => &
                massDividedByDouble, &
                massDividedByInteger, &
                massDividedByMass
        procedure :: massPlusMass
        generic, public :: operator(+) => massPlusMass
        procedure :: massMinusMass
        generic, public :: operator(-) => massMinusMass
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
    end type Mass_t

    type, public, extends(Unit_t) :: MassUnit_t
    contains
        procedure :: toString => unitToString
    end type MassUnit_t

    interface operator(.unit.)
        module procedure fromUnits
    end interface operator(.unit.)

    type(MassUnit_t), parameter, public :: GRAMS = MassUnit_t( &
            multiplier = GRAMS_PER_KILOGRAM, &
            symbol = "g")
    type(MassUnit_t), parameter, public :: KILOGRAMS = MassUnit_t( &
            multiplier = 1.0d0, &
            symbol = "kg")

    type(MassUnit_t), public :: DEFAULT_OUTPUT_UNITS = KILOGRAMS

    type(MassUnit_t), parameter, public :: PROVIDED_UNITS(*) = &
            [GRAMS, KILOGRAMS]

    public :: operator(.unit.)
contains
    function fromUnits(value_, units) result(mass)
        double precision, intent(in) :: value_
        type(MassUnit_t), intent(in) :: units
        type(Mass_t) :: mass

        mass%kilograms = value_ / units%multiplier
    end function fromUnits

    function toUnits(self, units) result(mass)
        class(Mass_t), intent(in) :: self
        class(MassUnit_t), intent(in) :: units
        double precision :: mass

        mass = self%kilograms * units%multiplier
    end function toUnits

    function doubleTimesMass(multiplier, mass) result(new_mass)
        double precision, intent(in) :: multiplier
        class(Mass_t), intent(in) :: mass
        type(Mass_t) :: new_mass

        new_mass%kilograms = multiplier * mass%kilograms
    end function doubleTimesMass

    function integerTimesMass(multiplier, mass) result(new_mass)
        integer, intent(in) :: multiplier
        class(Mass_t), intent(in) :: mass
        type(Mass_t) :: new_mass

        new_mass%kilograms = dble(multiplier) * mass%kilograms
    end function integerTimesMass

    function massTimesDouble(mass, multiplier) result(new_mass)
        class(Mass_t), intent(in) :: mass
        double precision, intent(in) :: multiplier
        type(Mass_t) :: new_mass

        new_mass%kilograms = mass%kilograms * multiplier
    end function massTimesDouble

    function massTimesInteger(mass, multiplier) result(new_mass)
        class(Mass_t), intent(in) :: mass
        integer, intent(in) :: multiplier
        type(Mass_t) :: new_mass

        new_mass%kilograms = mass%kilograms * dble(multiplier)
    end function massTimesInteger

    function massDividedByDouble(mass, divisor) result(new_mass)
        class(Mass_t), intent(in) :: mass
        double precision, intent(in) :: divisor
        type(Mass_t) :: new_mass

        new_mass%kilograms = mass%kilograms / divisor
    end function massDividedByDouble

    function massDividedByInteger(mass, divisor) result(new_mass)
        class(Mass_t), intent(in) :: mass
        integer, intent(in) :: divisor
        type(Mass_t) :: new_mass

        new_mass%kilograms = mass%kilograms / dble(divisor)
    end function massDividedByInteger

    function massDividedByMass(numerator, denomenator) result(ratio)
        class(Mass_t), intent(in) :: numerator
        class(Mass_t), intent(in) :: denomenator
        double precision :: ratio

        ratio = numerator%kilograms / denomenator%kilograms
    end function massDividedByMass

    function massPlusMass(mass1, mass2) result(new_mass)
        class(Mass_t), intent(in) :: mass1
        class(Mass_t), intent(in) :: mass2
        type(Mass_t) :: new_mass

        new_mass%kilograms = mass1%kilograms + mass2%kilograms
    end function massPlusMass

    function massMinusMass(mass1, mass2) result(new_mass)
        class(Mass_t), intent(in) :: mass1
        class(Mass_t), intent(in) :: mass2
        type(Mass_t) :: new_mass

        new_mass%kilograms = mass1%kilograms - mass2%kilograms
    end function massMinusMass

    function greaterThan(lhs, rhs)
        class(Mass_t), intent(in) :: lhs
        class(Mass_t), intent(in) :: rhs
        logical :: greaterThan

        greaterThan = lhs%kilograms > rhs%kilograms
    end function greaterThan

    function lessThan(lhs,rhs)
        class(Mass_t), intent(in) :: lhs
        class(Mass_t), intent(in) :: rhs
        logical :: lessThan

        lessThan = lhs%kilograms < rhs%kilograms
    end function lessThan

    function greaterThanOrEqual(lhs, rhs)
        class(Mass_t), intent(in) :: lhs
        class(Mass_t), intent(in) :: rhs
        logical :: greaterThanOrEqual

        greaterThanOrEqual = lhs%kilograms >= rhs%kilograms
    end function greaterThanOrEqual

    function lessThanOrEqual(lhs, rhs)
        class(Mass_t), intent(in) :: lhs
        class(Mass_t), intent(in) :: rhs
        logical :: lessThanOrEqual

        lessThanOrEqual = lhs%kilograms <= rhs%kilograms
    end function lessThanOrEqual

    function equal_(lhs,rhs)
        use Miscellaneous_m, only: operator(.safeEq.)

        class(Mass_t), intent(in) :: lhs
        class(Mass_t), intent(in) :: rhs
        logical :: equal_

        equal_ = lhs%kilograms .safeEq. rhs%kilograms
    end function equal_

    function equalWithinAbsolute(lhs, rhs, within)
        use Miscellaneous_m, only: equalWithinAbsolute_ => equalWithinAbsolute

        class(Mass_t), intent(in) :: lhs
        class(Mass_t), intent(in) :: rhs
        class(Mass_t), intent(in) :: within
        logical :: equalWithinAbsolute

        equalWithinAbsolute = equalWithinAbsolute_( &
                lhs%kilograms, rhs%kilograms, within%kilograms)
    end function equalWithinAbsolute

    function equalWithinRelative(lhs, rhs, within)
        use Miscellaneous_m, only: equalWithinRelative_ => equalWithinRelative

        class(Mass_t), intent(in) :: lhs
        class(Mass_t), intent(in) :: rhs
        double precision, intent(in) :: within
        logical :: equalWithinRelative

        equalWithinRelative = equalWithinRelative_( &
                lhs%kilograms, rhs%kilograms, within)
    end function equalWithinRelative

    function notEqual(lhs, rhs)
        class(Mass_t), intent(in) :: lhs
        class(Mass_t), intent(in) :: rhs
        logical :: notEqual

        notEqual = .not. lhs == rhs
    end function notEqual

    function toString(self) result(string)
        use iso_varying_string, only: VARYING_STRING

        class(Mass_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = self%toStringIn(DEFAULT_OUTPUT_UNITS)
    end function toString

    function toStringIn(self, units) result(string)
        use iso_varying_string, only: VARYING_STRING, operator(//)
        use strff, only: toString

        class(Mass_t), intent(in) :: self
        class(MassUnit_t), intent(in) :: units
        type(VARYING_STRING) :: string

        string = toString(self.in.units) // " " // units%toString()
    end function toStringIn

    function unitToString(self) result(string)
        use iso_varying_string, only: VARYING_STRING, assignment(=)

        class(MassUnit_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = trim(self%symbol)
    end function unitToString
end module Mass_m