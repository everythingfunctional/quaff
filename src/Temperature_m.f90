module Temperature_m
    use Conversion_factors_m, only: &
            CELSIUS_KELVIN_DIFFERENCE, &
            FAHRENHEIT_RANKINE_DIFFERENCE, &
            RANKINE_PER_KELVIN
    use Units_m, only: Unit_t

    implicit none
    private

    type, public :: Temperature_t
        private
        double precision :: kelvin
    contains
        private
        procedure :: toUnits
        generic, public :: operator(.in.) => toUnits
        procedure, pass(temperature) :: doubleTimesTemperature
        procedure, pass(temperature) :: integerTimesTemperature
        procedure, pass(temperature) :: temperatureTimesDouble
        procedure, pass(temperature) :: temperatureTimesInteger
        generic, public :: operator(*) => &
                doubleTimesTemperature, &
                integerTimesTemperature, &
                temperatureTimesDouble, &
                temperatureTimesInteger
        procedure :: temperatureDividedByDouble
        procedure :: temperatureDividedByInteger
        procedure, pass(numerator) :: temperatureDividedByTemperature
        generic, public :: operator(/) => &
                temperatureDividedByDouble, &
                temperatureDividedByInteger, &
                temperatureDividedByTemperature
        procedure :: temperaturePlusTemperature
        generic, public :: operator(+) => temperaturePlusTemperature
        procedure :: temperatureMinusTemperature
        generic, public :: operator(-) => temperatureMinusTemperature
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
    end type Temperature_t

    type, public, extends(Unit_t) :: TemperatureUnit_t
        double precision :: difference
    contains
        procedure :: toString => unitToString
    end type TemperatureUnit_t

    interface operator(.unit.)
        module procedure fromUnits
    end interface operator(.unit.)

    type(TemperatureUnit_t), parameter, public :: CELSIUS = TemperatureUnit_t( &
            multiplier = 1.0d0, &
            difference = CELSIUS_KELVIN_DIFFERENCE, &
            symbol = "C")
    type(TemperatureUnit_t), parameter, public :: FAHRENHEIT = TemperatureUnit_t( &
            multiplier = RANKINE_PER_KELVIN, &
            difference = FAHRENHEIT_RANKINE_DIFFERENCE, &
            symbol = "F")
    type(TemperatureUnit_t), parameter, public :: KELVIN = TemperatureUnit_t( &
            multiplier = 1.0d0, &
            difference = 0.0d0, &
            symbol = "K")
    type(TemperatureUnit_t), parameter, public :: RANKINE = TemperatureUnit_t( &
            multiplier = RANKINE_PER_KELVIN, &
            difference = 0.0d0, &
            symbol = "R")

    type(TemperatureUnit_t), public :: DEFAULT_OUTPUT_UNITS = KELVIN

    type(TemperatureUnit_t), parameter, public :: PROVIDED_UNITS(*) = &
            [CELSIUS, FAHRENHEIT, KELVIN, RANKINE]

    public :: operator(.unit.)
contains
    function fromUnits(value_, units) result(temperature)
        double precision, intent(in) :: value_
        type(TemperatureUnit_t), intent(in) :: units
        type(Temperature_t) :: temperature

        temperature%kelvin = (value_ + units%difference) / units%multiplier
    end function fromUnits

    function toUnits(self, units) result(temperature)
        class(Temperature_t), intent(in) :: self
        class(TemperatureUnit_t), intent(in) :: units
        double precision :: temperature

        temperature = self%kelvin * units%multiplier - units%difference
    end function toUnits

    function doubleTimesTemperature(multiplier, temperature) result(new_temperature)
        double precision, intent(in) :: multiplier
        class(Temperature_t), intent(in) :: temperature
        type(Temperature_t) :: new_temperature

        new_temperature%kelvin = multiplier * temperature%kelvin
    end function doubleTimesTemperature

    function integerTimesTemperature(multiplier, temperature) result(new_temperature)
        integer, intent(in) :: multiplier
        class(Temperature_t), intent(in) :: temperature
        type(Temperature_t) :: new_temperature

        new_temperature%kelvin = dble(multiplier) * temperature%kelvin
    end function integerTimesTemperature

    function temperatureTimesDouble(temperature, multiplier) result(new_temperature)
        class(Temperature_t), intent(in) :: temperature
        double precision, intent(in) :: multiplier
        type(Temperature_t) :: new_temperature

        new_temperature%kelvin = temperature%kelvin * multiplier
    end function temperatureTimesDouble

    function temperatureTimesInteger(temperature, multiplier) result(new_temperature)
        class(Temperature_t), intent(in) :: temperature
        integer, intent(in) :: multiplier
        type(Temperature_t) :: new_temperature

        new_temperature%kelvin = temperature%kelvin * dble(multiplier)
    end function temperatureTimesInteger

    function temperatureDividedByDouble(temperature, divisor) result(new_temperature)
        class(Temperature_t), intent(in) :: temperature
        double precision, intent(in) :: divisor
        type(Temperature_t) :: new_temperature

        new_temperature%kelvin = temperature%kelvin / divisor
    end function temperatureDividedByDouble

    function temperatureDividedByInteger(temperature, divisor) result(new_temperature)
        class(Temperature_t), intent(in) :: temperature
        integer, intent(in) :: divisor
        type(Temperature_t) :: new_temperature

        new_temperature%kelvin = temperature%kelvin / dble(divisor)
    end function temperatureDividedByInteger

    function temperatureDividedByTemperature(numerator, denomenator) result(ratio)
        class(Temperature_t), intent(in) :: numerator
        class(Temperature_t), intent(in) :: denomenator
        double precision :: ratio

        ratio = numerator%kelvin / denomenator%kelvin
    end function temperatureDividedByTemperature

    function temperaturePlusTemperature(temperature1, temperature2) result(new_temperature)
        class(Temperature_t), intent(in) :: temperature1
        class(Temperature_t), intent(in) :: temperature2
        type(Temperature_t) :: new_temperature

        new_temperature%kelvin = temperature1%kelvin + temperature2%kelvin
    end function temperaturePlusTemperature

    function temperatureMinusTemperature(temperature1, temperature2) result(new_temperature)
        class(Temperature_t), intent(in) :: temperature1
        class(Temperature_t), intent(in) :: temperature2
        type(Temperature_t) :: new_temperature

        new_temperature%kelvin = temperature1%kelvin - temperature2%kelvin
    end function temperatureMinusTemperature

    function greaterThan(lhs, rhs)
        class(Temperature_t), intent(in) :: lhs
        class(Temperature_t), intent(in) :: rhs
        logical :: greaterThan

        greaterThan = lhs%kelvin > rhs%kelvin
    end function greaterThan

    function lessThan(lhs,rhs)
        class(Temperature_t), intent(in) :: lhs
        class(Temperature_t), intent(in) :: rhs
        logical :: lessThan

        lessThan = lhs%kelvin < rhs%kelvin
    end function lessThan

    function greaterThanOrEqual(lhs, rhs)
        class(Temperature_t), intent(in) :: lhs
        class(Temperature_t), intent(in) :: rhs
        logical :: greaterThanOrEqual

        greaterThanOrEqual = lhs%kelvin >= rhs%kelvin
    end function greaterThanOrEqual

    function lessThanOrEqual(lhs, rhs)
        class(Temperature_t), intent(in) :: lhs
        class(Temperature_t), intent(in) :: rhs
        logical :: lessThanOrEqual

        lessThanOrEqual = lhs%kelvin <= rhs%kelvin
    end function lessThanOrEqual

    function equal_(lhs,rhs)
        use Miscellaneous_m, only: operator(.safeEq.)

        class(Temperature_t), intent(in) :: lhs
        class(Temperature_t), intent(in) :: rhs
        logical :: equal_

        equal_ = lhs%kelvin .safeEq. rhs%kelvin
    end function equal_

    function equalWithinAbsolute(lhs, rhs, within)
        use Miscellaneous_m, only: equalWithinAbsolute_ => equalWithinAbsolute

        class(Temperature_t), intent(in) :: lhs
        class(Temperature_t), intent(in) :: rhs
        class(Temperature_t), intent(in) :: within
        logical :: equalWithinAbsolute

        equalWithinAbsolute = equalWithinAbsolute_( &
                lhs%kelvin, rhs%kelvin, within%kelvin)
    end function equalWithinAbsolute

    function equalWithinRelative(lhs, rhs, within)
        use Miscellaneous_m, only: equalWithinRelative_ => equalWithinRelative

        class(Temperature_t), intent(in) :: lhs
        class(Temperature_t), intent(in) :: rhs
        double precision, intent(in) :: within
        logical :: equalWithinRelative

        equalWithinRelative = equalWithinRelative_( &
                lhs%kelvin, rhs%kelvin, within)
    end function equalWithinRelative

    function notEqual(lhs, rhs)
        class(Temperature_t), intent(in) :: lhs
        class(Temperature_t), intent(in) :: rhs
        logical :: notEqual

        notEqual = .not. lhs == rhs
    end function notEqual

    function toString(self) result(string)
        use iso_varying_string, only: VARYING_STRING

        class(Temperature_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = self%toStringIn(DEFAULT_OUTPUT_UNITS)
    end function toString

    function toStringIn(self, units) result(string)
        use iso_varying_string, only: VARYING_STRING, operator(//)
        use strff, only: toString

        class(Temperature_t), intent(in) :: self
        class(TemperatureUnit_t), intent(in) :: units
        type(VARYING_STRING) :: string

        string = toString(self.in.units) // " " // units%toString()
    end function toStringIn

    function unitToString(self) result(string)
        use iso_varying_string, only: VARYING_STRING, assignment(=)

        class(TemperatureUnit_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = trim(self%symbol)
    end function unitToString
end module Temperature_m