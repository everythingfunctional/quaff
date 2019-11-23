module Temperature_m
    use Conversion_factors_m, only: &
            CELSIUS_KELVIN_DIFFERENCE, &
            FAHRENHEIT_RANKINE_DIFFERENCE, &
            RANKINE_PER_KELVIN
    use erloff, only: ErrorList_t, Fatal, Module_, Procedure_
    use iso_varying_string, only: &
            VARYING_STRING, &
            assignment(=), &
            operator(==), &
            operator(//), &
            len, &
            split, &
            var_str
    use Miscellaneous_m, only: &
            operator(.safeEq.), &
            equalWithinAbsolute_ => equalWithinAbsolute, &
            equalWithinRelative_ => equalWithinRelative, &
            wrapInLatexQuantity, &
            PARSE_ERROR, &
            UNKNOWN_UNIT
    use strff, only: join, toString

    implicit none
    private

    type, public :: Temperature_t
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
        procedure :: toStringFullPrecision
        procedure :: toStringWithPrecision
        generic, public :: toString => &
                toStringFullPrecision, toStringWithPrecision
        procedure :: toStringInFullPrecision
        procedure :: toStringInWithPrecision
        generic, public :: toStringIn => &
                toStringInFullPrecision, toStringInWithPrecision
        procedure :: toGnuplotStringFullPrecision
        procedure :: toGnuplotStringWithPrecision
        generic, public :: toGnuplotString => &
                toGnuplotStringFullPrecision, toGnuplotStringWithPrecision
        procedure :: toGnuplotStringInFullPrecision
        procedure :: toGnuplotStringInWithPrecision
        generic, public :: toGnuplotStringIn => &
                toGnuplotStringInFullPrecision, toGnuplotStringInWithPrecision
        procedure :: toLatexStringFullPrecision
        procedure :: toLatexStringWithPrecision
        generic, public :: toLatexString => &
                toLatexStringFullPrecision, toLatexStringWithPrecision
        procedure :: toLatexStringInFullPrecision
        procedure :: toLatexStringInWithPrecision
        generic, public :: toLatexStringIn => &
                toLatexStringInFullPrecision, toLatexStringInWithPrecision
    end type Temperature_t

    type, public :: TemperatureUnit_t
        double precision :: conversion_factor
        double precision :: difference
        character(len=10) :: symbol
        character(len=50) :: gnuplot_symbol
        character(len=100) :: latex_symbol
    contains
        procedure :: toString => unitToString
        procedure :: toGnuplotString => unitToGnuplotString
        procedure :: toLatexString => unitToLatexString
    end type TemperatureUnit_t

    interface operator(.unit.)
        module procedure fromUnits
    end interface operator(.unit.)

    interface fromString
        module procedure fromStringBasicC
        module procedure fromStringBasicS
        module procedure fromStringWithUnitsC
        module procedure fromStringWithUnitsS
        module procedure unitFromStringBasicC
        module procedure unitFromStringBasicS
        module procedure unitFromStringWithUnitsC
        module procedure unitFromStringWithUnitsS
    end interface fromString

    type(TemperatureUnit_t), parameter, public :: CELSIUS = TemperatureUnit_t( &
            conversion_factor = 1.0d0, &
            difference = CELSIUS_KELVIN_DIFFERENCE, &
            symbol = "C", &
            gnuplot_symbol = "{/Symbol \260}C", &
            latex_symbol = "\celsius")
    type(TemperatureUnit_t), parameter, public :: FAHRENHEIT = TemperatureUnit_t( &
            conversion_factor = RANKINE_PER_KELVIN, &
            difference = FAHRENHEIT_RANKINE_DIFFERENCE, &
            symbol = "F", &
            gnuplot_symbol = "{/Symbold \260}F", &
            latex_symbol = "\fahrenheit")
    type(TemperatureUnit_t), parameter, public :: KELVIN = TemperatureUnit_t( &
            conversion_factor = 1.0d0, &
            difference = 0.0d0, &
            symbol = "K", &
            gnuplot_symbol = "K", &
            latex_symbol = "\kelvin")
    type(TemperatureUnit_t), parameter, public :: RANKINE = TemperatureUnit_t( &
            conversion_factor = RANKINE_PER_KELVIN, &
            difference = 0.0d0, &
            symbol = "R", &
            gnuplot_symbol = "{/Symbold \260}R", &
            latex_symbol = "\rankine")

    type(TemperatureUnit_t), public :: DEFAULT_OUTPUT_UNITS = KELVIN

    type(TemperatureUnit_t), parameter, public :: PROVIDED_UNITS(*) = &
            [CELSIUS, FAHRENHEIT, KELVIN, RANKINE]

    public :: operator(.unit.), fromString
contains
    pure subroutine fromStringBasicC(string, errors, temperature)
        character(len=*), intent(in) :: string
        type(ErrorList_t), intent(out) :: errors
        type(Temperature_t), intent(out) :: temperature

        type(ErrorList_t) :: errors_

        call fromString( &
                var_str(string), PROVIDED_UNITS, errors_, temperature)
        call errors%appendErrors( &
                errors_, &
                Module_("Temperature_m"), &
                Procedure_("fromStringBasicC"))
    end subroutine fromStringBasicC

    pure subroutine fromStringBasicS(string, errors, temperature)
        type(VARYING_STRING), intent(in) :: string
        type(ErrorList_t), intent(out) :: errors
        type(Temperature_t), intent(out) :: temperature

        type(ErrorList_t) :: errors_

        call fromString( &
                string, PROVIDED_UNITS, errors_, temperature)
        call errors%appendErrors( &
                errors_, &
                Module_("Temperature_m"), &
                Procedure_("fromStringBasicS"))
    end subroutine fromStringBasicS

    pure subroutine fromStringWithUnitsC(string, units, errors, temperature)
        character(len=*), intent(in) :: string
        type(TemperatureUnit_t), intent(in) :: units(:)
        type(ErrorList_t), intent(out) :: errors
        type(Temperature_t), intent(out) :: temperature

        type(ErrorList_t) :: errors_

        call fromString( &
                var_str(string), units, errors_, temperature)
        call errors%appendErrors( &
                errors_, &
                Module_("Temperature_m"), &
                Procedure_("fromStringWithUnitsC"))
    end subroutine fromStringWithUnitsC

    pure subroutine fromStringWithUnitsS(string, units, errors, temperature)
        type(VARYING_STRING), intent(in) :: string
        type(TemperatureUnit_t), intent(in) :: units(:)
        type(ErrorList_t), intent(out) :: errors
        type(Temperature_t), intent(out) :: temperature

        double precision :: number
        character(len=100) :: number_chars
        type(VARYING_STRING) :: number_string
        integer :: status
        type(VARYING_STRING) :: symbol
        type(TemperatureUnit_t) :: unit
        type(ErrorList_t) :: unit_errors

        temperature%kelvin = 0.0d0
        symbol = string
        call split(symbol, number_string, " ")
        if (len(symbol) == 0) then
            call errors%appendError(Fatal( &
                    PARSE_ERROR, &
                    Module_("Temperature_m"), &
                    Procedure_("fromStringWithUnitsS"), &
                    'No unit symbol found in string "' // string // '"'))
            return
        end if
        number_chars = number_string
        read(number_chars, *, iostat=status) number
        if (status /= 0) then
            call errors%appendError(Fatal( &
                    PARSE_ERROR, &
                    Module_("Temperature_m"), &
                    Procedure_("fromStringWithUnitsS"), &
                    'Error parsing number from string "' // number_string // '"'))
        end if
        call fromString(symbol, units, unit_errors, unit)
        temperature = number.unit.unit
        call errors%appendErrors( &
                unit_errors, &
                Module_("Temperature_m"), &
                Procedure_("fromStringWithUnitsS"))
    end subroutine fromStringWithUnitsS

    elemental function fromUnits(value_, units) result(temperature)
        double precision, intent(in) :: value_
        type(TemperatureUnit_t), intent(in) :: units
        type(Temperature_t) :: temperature

        temperature%kelvin = (value_ + units%difference) / units%conversion_factor
    end function fromUnits

    elemental function toUnits(self, units) result(temperature)
        class(Temperature_t), intent(in) :: self
        class(TemperatureUnit_t), intent(in) :: units
        double precision :: temperature

        temperature = self%kelvin * units%conversion_factor - units%difference
    end function toUnits

    elemental function doubleTimesTemperature( &
            multiplier, temperature) result(new_temperature)
        double precision, intent(in) :: multiplier
        class(Temperature_t), intent(in) :: temperature
        type(Temperature_t) :: new_temperature

        new_temperature%kelvin = &
                multiplier * temperature%kelvin
    end function doubleTimesTemperature

    elemental function integerTimesTemperature( &
            multiplier, temperature) result(new_temperature)
        integer, intent(in) :: multiplier
        class(Temperature_t), intent(in) :: temperature
        type(Temperature_t) :: new_temperature

        new_temperature%kelvin = &
                dble(multiplier) * temperature%kelvin
    end function integerTimesTemperature

    elemental function temperatureTimesDouble( &
            temperature, multiplier) result(new_temperature)
        class(Temperature_t), intent(in) :: temperature
        double precision, intent(in) :: multiplier
        type(Temperature_t) :: new_temperature

        new_temperature%kelvin = &
                temperature%kelvin * multiplier
    end function temperatureTimesDouble

    elemental function temperatureTimesInteger( &
            temperature, multiplier) result(new_temperature)
        class(Temperature_t), intent(in) :: temperature
        integer, intent(in) :: multiplier
        type(Temperature_t) :: new_temperature

        new_temperature%kelvin = &
                temperature%kelvin * dble(multiplier)
    end function temperatureTimesInteger

    elemental function temperatureDividedByDouble( &
            temperature, divisor) result(new_temperature)
        class(Temperature_t), intent(in) :: temperature
        double precision, intent(in) :: divisor
        type(Temperature_t) :: new_temperature

        new_temperature%kelvin = &
                temperature%kelvin / divisor
    end function temperatureDividedByDouble

    elemental function temperatureDividedByInteger( &
            temperature, divisor) result(new_temperature)
        class(Temperature_t), intent(in) :: temperature
        integer, intent(in) :: divisor
        type(Temperature_t) :: new_temperature

        new_temperature%kelvin = &
                temperature%kelvin / dble(divisor)
    end function temperatureDividedByInteger

    elemental function temperatureDividedByTemperature( &
            numerator, denomenator) result(ratio)
        class(Temperature_t), intent(in) :: numerator
        class(Temperature_t), intent(in) :: denomenator
        double precision :: ratio

        ratio = numerator%kelvin / denomenator%kelvin
    end function temperatureDividedByTemperature

    elemental function temperaturePlusTemperature( &
            temperature1, temperature2) result(new_temperature)
        class(Temperature_t), intent(in) :: temperature1
        class(Temperature_t), intent(in) :: temperature2
        type(Temperature_t) :: new_temperature

        new_temperature%kelvin = &
                temperature1%kelvin + temperature2%kelvin
    end function temperaturePlusTemperature

    elemental function temperatureMinusTemperature( &
            temperature1, temperature2) result(new_temperature)
        class(Temperature_t), intent(in) :: temperature1
        class(Temperature_t), intent(in) :: temperature2
        type(Temperature_t) :: new_temperature

        new_temperature%kelvin = &
                temperature1%kelvin - temperature2%kelvin
    end function temperatureMinusTemperature

    elemental function greaterThan(lhs, rhs)
        class(Temperature_t), intent(in) :: lhs
        class(Temperature_t), intent(in) :: rhs
        logical :: greaterThan

        greaterThan = lhs%kelvin > rhs%kelvin
    end function greaterThan

    elemental function lessThan(lhs,rhs)
        class(Temperature_t), intent(in) :: lhs
        class(Temperature_t), intent(in) :: rhs
        logical :: lessThan

        lessThan = lhs%kelvin < rhs%kelvin
    end function lessThan

    elemental function greaterThanOrEqual(lhs, rhs)
        class(Temperature_t), intent(in) :: lhs
        class(Temperature_t), intent(in) :: rhs
        logical :: greaterThanOrEqual

        greaterThanOrEqual = lhs%kelvin >= rhs%kelvin
    end function greaterThanOrEqual

    elemental function lessThanOrEqual(lhs, rhs)
        class(Temperature_t), intent(in) :: lhs
        class(Temperature_t), intent(in) :: rhs
        logical :: lessThanOrEqual

        lessThanOrEqual = lhs%kelvin <= rhs%kelvin
    end function lessThanOrEqual

    elemental function equal_(lhs,rhs)
        class(Temperature_t), intent(in) :: lhs
        class(Temperature_t), intent(in) :: rhs
        logical :: equal_

        equal_ = lhs%kelvin .safeEq. rhs%kelvin
    end function equal_

    elemental function equalWithinAbsolute(lhs, rhs, within)
        class(Temperature_t), intent(in) :: lhs
        class(Temperature_t), intent(in) :: rhs
        class(Temperature_t), intent(in) :: within
        logical :: equalWithinAbsolute

        equalWithinAbsolute = equalWithinAbsolute_( &
                lhs%kelvin, rhs%kelvin, within%kelvin)
    end function equalWithinAbsolute

    elemental function equalWithinRelative(lhs, rhs, within)
        class(Temperature_t), intent(in) :: lhs
        class(Temperature_t), intent(in) :: rhs
        double precision, intent(in) :: within
        logical :: equalWithinRelative

        equalWithinRelative = equalWithinRelative_( &
                lhs%kelvin, rhs%kelvin, within)
    end function equalWithinRelative

    elemental function notEqual(lhs, rhs)
        class(Temperature_t), intent(in) :: lhs
        class(Temperature_t), intent(in) :: rhs
        logical :: notEqual

        notEqual = .not. lhs == rhs
    end function notEqual

    elemental function toStringFullPrecision(self) result(string)
        class(Temperature_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = self%toStringIn(DEFAULT_OUTPUT_UNITS)
    end function toStringFullPrecision

    elemental function toStringWithPrecision(self, significant_digits) result(string)
        class(Temperature_t), intent(in) :: self
        integer, intent(in) :: significant_digits
        type(VARYING_STRING) :: string

        string = self%toStringIn(DEFAULT_OUTPUT_UNITS, significant_digits)
    end function toStringWithPrecision

    elemental function toStringInFullPrecision(self, units) result(string)
        class(Temperature_t), intent(in) :: self
        class(TemperatureUnit_t), intent(in) :: units
        type(VARYING_STRING) :: string

        string = toString(self.in.units) // " " // units%toString()
    end function toStringInFullPrecision

    elemental function toStringInWithPrecision( &
            self, units, significant_digits) result(string)
        class(Temperature_t), intent(in) :: self
        class(TemperatureUnit_t), intent(in) :: units
        integer, intent(in) :: significant_digits
        type(VARYING_STRING) :: string

        string = &
                toString(self.in.units, significant_digits) &
                // " " // units%toString()
    end function toStringInWithPrecision

    elemental function toGnuplotStringFullPrecision(self) result(string)
        class(Temperature_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = self%toGnuplotStringIn(DEFAULT_OUTPUT_UNITS)
    end function toGnuplotStringFullPrecision

    elemental function toGnuplotStringWithPrecision( &
            self, significant_digits) result(string)
        class(Temperature_t), intent(in) :: self
        integer, intent(in) :: significant_digits
        type(VARYING_STRING) :: string

        string = self%toGnuplotStringIn( &
                DEFAULT_OUTPUT_UNITS, significant_digits)
    end function toGnuplotStringWithPrecision

    elemental function toGnuplotStringInFullPrecision(self, units) result(string)
        class(Temperature_t), intent(in) :: self
        class(TemperatureUnit_t), intent(in) :: units
        type(VARYING_STRING) :: string

        string = toString(self.in.units) // " " // units%toGnuplotString()
    end function toGnuplotStringInFullPrecision

    elemental function toGnuplotStringInWithPrecision( &
            self, units, significant_digits) result(string)
        class(Temperature_t), intent(in) :: self
        class(TemperatureUnit_t), intent(in) :: units
        integer, intent(in) :: significant_digits
        type(VARYING_STRING) :: string

        string = &
                toString(self.in.units, significant_digits) &
                // " " // units%toGnuplotString()
    end function toGnuplotStringInWithPrecision

    elemental function toLatexStringFullPrecision(self) result(string)
        class(Temperature_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = self%toLatexStringIn(DEFAULT_OUTPUT_UNITS)
    end function toLatexStringFullPrecision

    elemental function toLatexStringWithPrecision(self, significant_digits) result(string)
        class(Temperature_t), intent(in) :: self
        integer, intent(in) :: significant_digits
        type(VARYING_STRING) :: string

        string = self%toLatexStringIn(DEFAULT_OUTPUT_UNITS, significant_digits)
    end function toLatexStringWithPrecision

    elemental function toLatexStringInFullPrecision(self, units) result(string)
        class(Temperature_t), intent(in) :: self
        class(TemperatureUnit_t), intent(in) :: units
        type(VARYING_STRING) :: string

        string = wrapInLatexQuantity( &
                toString(self.in.units), units%toLatexString())
    end function toLatexStringInFullPrecision

    elemental function toLatexStringInWithPrecision( &
            self, units, significant_digits) result(string)
        class(Temperature_t), intent(in) :: self
        class(TemperatureUnit_t), intent(in) :: units
        integer, intent(in) :: significant_digits
        type(VARYING_STRING) :: string

        string = wrapInLatexQuantity( &
                toString(self.in.units, significant_digits), &
                units%toLatexString())
    end function toLatexStringInWithPrecision

    pure subroutine unitFromStringBasicC(string, errors, unit)
        character(len=*), intent(in) :: string
        type(ErrorList_t), intent(out) :: errors
        type(TemperatureUnit_t), intent(out) :: unit

        type(ErrorList_t) :: errors_

        call fromString( &
                var_str(string), PROVIDED_UNITS, errors_, unit)
        call errors%appendErrors( &
                errors_, &
                Module_("Temperature_m"), &
                Procedure_("unitFromStringBasicC"))
    end subroutine unitFromStringBasicC

    pure subroutine unitFromStringBasicS(string, errors, unit)
        type(VARYING_STRING), intent(in) :: string
        type(ErrorList_t), intent(out) :: errors
        type(TemperatureUnit_t), intent(out) :: unit

        type(ErrorList_t) :: errors_

        call fromString( &
                string, PROVIDED_UNITS, errors_, unit)
        call errors%appendErrors( &
                errors_, &
                Module_("Temperature_m"), &
                Procedure_("unitFromStringBasicS"))
    end subroutine unitFromStringBasicS

    pure subroutine unitFromStringWithUnitsC(string, units, errors, unit)
        character(len=*), intent(in) :: string
        type(TemperatureUnit_t), intent(in) :: units(:)
        type(ErrorList_t), intent(out) :: errors
        type(TemperatureUnit_t), intent(out) :: unit

        type(ErrorList_t) :: errors_

        call fromString(var_str(string), units, errors_, unit)
        call errors%appendErrors( &
                errors_, &
                Module_("Temperature_m"), &
                Procedure_("unitFromStringWithUnitsC"))
    end subroutine unitFromStringWithUnitsC

    pure subroutine unitFromStringWithUnitsS(string, units, errors, unit)
        type(VARYING_STRING), intent(in) :: string
        type(TemperatureUnit_t), intent(in) :: units(:)
        type(ErrorList_t), intent(out) :: errors
        type(TemperatureUnit_t), intent(out) :: unit

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
                    Module_("Temperature_m"), &
                    Procedure_("unitFromStringWithUnitsS"), &
                    '"' // string // '", known units: [' // join(unit_strings, ', ') // ']' ))
        end if
    end subroutine unitFromStringWithUnitsS

    elemental function unitToString(self) result(string)
        class(TemperatureUnit_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = trim(self%symbol)
    end function unitToString

    elemental function unitToGnuplotString(self) result(string)
        class(TemperatureUnit_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = trim(self%gnuplot_symbol)
    end function unitToGnuplotString

    elemental function unitToLatexString(self) result(string)
        class(TemperatureUnit_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = trim(self%latex_symbol)
    end function unitToLatexString
end module Temperature_m
