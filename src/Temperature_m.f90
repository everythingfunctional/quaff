module Temperature_m
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
            CELSIUS_KELVIN_DIFFERENCE, &
            FAHRENHEIT_RANKINE_DIFFERENCE, &
            RANKINE_PER_KELVIN
    use quaff_Utilities_m, only: &
            operator(.safeEq.), &
            equalWithinAbsolute_ => equalWithinAbsolute, &
            equalWithinRelative_ => equalWithinRelative, &
            parseCloseBrace, &
            parseOpenBrace, &
            parseSI, &
            parseSpace, &
            wrapInLatexQuantity, &
            wrapInLatexUnit, &
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
    end type Temperature_t

    type, abstract, public :: TemperatureUnit_t
        double precision :: conversion_factor
        double precision :: difference
    contains
        procedure(justUnitToString), deferred :: unitToString
        procedure(unitWithValueToString), deferred :: valueToString
        generic :: toString => unitToString, valueToString
        procedure(parseAsI), deferred :: parseAs
    end type TemperatureUnit_t

    type, extends(TemperatureUnit_t), public :: TemperatureSimpleUnit_t
        character(len=20) :: symbol
    contains
        procedure :: unitToString => simpleUnitToString
        procedure :: valueToString => simpleValueToString
        procedure :: parseAs => simpleParseAs
    end type TemperatureSimpleUnit_t

    type, extends(TemperatureUnit_t), public :: TemperatureGnuplotUnit_t
        character(len=50) :: symbol
    contains
        procedure :: unitToString => gnuplotUnitToString
        procedure :: valueToString => gnuplotValueToString
        procedure :: parseAs => gnuplotParseAs
    end type TemperatureGnuplotUnit_t

    type, extends(TemperatureUnit_t), public :: TemperatureLatexUnit_t
        character(len=100) :: symbol
    contains
        procedure :: unitToString => latexUnitToString
        procedure :: valueToString => latexValueToString
        procedure :: parseAs => latexParseAs
    end type TemperatureLatexUnit_t

    abstract interface
        elemental function justUnitToString(self) result(string)
            import TemperatureUnit_t, VARYING_STRING
            class(TemperatureUnit_t), intent(in) :: self
            type(VARYING_STRING) :: string
        end function justUnitToString

        pure function unitWithValueToString(self, value_) result(string)
            import TemperatureUnit_t, VARYING_STRING
            class(TemperatureUnit_t), intent(in) :: self
            type(VARYING_STRING), intent(in) :: value_
            type(VARYING_STRING) :: string
        end function unitWithValueToString

        pure subroutine parseAsI(self, string, errors, temperature)
            import ErrorList_t, Temperature_t, TemperatureUnit_t, VARYING_STRING
            class(TemperatureUnit_t), intent(in) :: self
            type(VARYING_STRING), intent(in) :: string
            type(ErrorList_t), intent(out) :: errors
            type(Temperature_t), intent(out) :: temperature
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
        module procedure sumTemperature
    end interface sum

    type(TemperatureSimpleUnit_t), parameter, public :: CELSIUS = &
            TemperatureSimpleUnit_t( &
                    conversion_factor = 1.0d0, &
                    difference = CELSIUS_KELVIN_DIFFERENCE, &
                    symbol = "C")
    type(TemperatureGnuplotUnit_t), parameter, public :: CELSIUS_GNUPLOT = &
            TemperatureGnuplotUnit_t( &
                    conversion_factor = 1.0d0, &
                    difference = CELSIUS_KELVIN_DIFFERENCE, &
                    symbol = "{/Symbol \260}C")
    type(TemperatureLatexUnit_t), parameter, public :: CELSIUS_LATEX = &
            TemperatureLatexUnit_t( &
                    conversion_factor = 1.0d0, &
                    difference = CELSIUS_KELVIN_DIFFERENCE, &
                    symbol = "\degreeCelsius")
    type(TemperatureSimpleUnit_t), parameter, public :: FAHRENHEIT = &
            TemperatureSimpleUnit_t( &
                    conversion_factor = RANKINE_PER_KELVIN, &
                    difference = FAHRENHEIT_RANKINE_DIFFERENCE, &
                    symbol = "F")
    type(TemperatureGnuplotUnit_t), parameter, public :: FAHRENHEIT_GNUPLOT = &
            TemperatureGnuplotUnit_t( &
                    conversion_factor = RANKINE_PER_KELVIN, &
                    difference = FAHRENHEIT_RANKINE_DIFFERENCE, &
                    symbol = "{/Symbol \260}F")
    type(TemperatureSimpleUnit_t), parameter, public :: KELVIN = &
            TemperatureSimpleUnit_t( &
                    conversion_factor = 1.0d0, &
                    difference = 0.0d0, &
                    symbol = "K")
    type(TemperatureGnuplotUnit_t), parameter, public :: KELVIN_GNUPLOT = &
            TemperatureGnuplotUnit_t( &
                    conversion_factor = 1.0d0, &
                    difference = 0.0d0, &
                    symbol = "K")
    type(TemperatureLatexUnit_t), parameter, public :: KELVIN_LATEX = &
            TemperatureLatexUnit_t( &
                    conversion_factor = 1.0d0, &
                    difference = 0.0d0, &
                    symbol = "\kelvin")
    type(TemperatureSimpleUnit_t), parameter, public :: RANKINE = &
            TemperatureSimpleUnit_t( &
                    conversion_factor = RANKINE_PER_KELVIN, &
                    difference = 0.0d0, &
                    symbol = "R")
    type(TemperatureGnuplotUnit_t), parameter, public :: RANKINE_GNUPLOT = &
            TemperatureGnuplotUnit_t( &
                    conversion_factor = RANKINE_PER_KELVIN, &
                    difference = 0.0d0, &
                    symbol = "{/Symbol \260}R")

    type(TemperatureSimpleUnit_t), public :: DEFAULT_OUTPUT_UNITS = KELVIN

    type(TemperatureSimpleUnit_t), parameter, public :: PROVIDED_UNITS(*) = &
            [CELSIUS, FAHRENHEIT, KELVIN, RANKINE]
    type(TemperatureGnuplotUnit_t), parameter, public :: PROVIDED_GNUPLOT_UNITS(*) = &
            [CELSIUS_GNUPLOT, FAHRENHEIT_GNUPLOT, KELVIN_GNUPLOT, RANKINE_GNUPLOT]
    type(TemperatureLatexUnit_t), parameter, public :: PROVIDED_LATEX_UNITS(*) = &
            [CELSIUS_LATEX, KELVIN_LATEX]

    public :: operator(.unit.), fromString, selectUnit, sum
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
        class(TemperatureUnit_t), intent(in) :: units(:)
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
        class(TemperatureUnit_t), intent(in) :: units(:)
        type(ErrorList_t), intent(out) :: errors
        type(Temperature_t), intent(out) :: temperature

        type(ErrorList_t) :: all_errors(size(units))
        integer :: i

        do i = 1, size(units)
            call units(i)%parseAs(string, all_errors(i), temperature)
            if (.not. all_errors(i)%hasAny()) return
        end do
        do i = 1, size(units)
            call errors%appendErrors( &
                    all_errors(i), &
                    Module_("Temperature_m"), &
                    Procedure_("fromStringWithUnitsS"))
        end do
    end subroutine fromStringWithUnitsS

    elemental function fromUnits(value_, units) result(temperature)
        double precision, intent(in) :: value_
        class(TemperatureUnit_t), intent(in) :: units
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

    pure function sumTemperature(temperatures)
        type(Temperature_t), intent(in) :: temperatures(:)
        type(Temperature_t) :: sumTemperature

        sumTemperature%kelvin = sum(temperatures%kelvin)
    end function sumTemperature

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

        string = units%toString(toString(self.in.units))
    end function toStringInFullPrecision

    elemental function toStringInWithPrecision( &
            self, units, significant_digits) result(string)
        class(Temperature_t), intent(in) :: self
        class(TemperatureUnit_t), intent(in) :: units
        integer, intent(in) :: significant_digits
        type(VARYING_STRING) :: string

        string = units%toString(toString(self.in.units, significant_digits))
    end function toStringInWithPrecision

    elemental function simpleUnitToString(self) result(string)
        class(TemperatureSimpleUnit_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = trim(self%symbol)
    end function simpleUnitToString

    pure function simpleValueToString(self, value_) result(string)
        class(TemperatureSimpleUnit_t), intent(in) :: self
        type(VARYING_STRING), intent(in) :: value_
        type(VARYING_STRING) :: string

        string = value_ // " " // self%toString()
    end function simpleValueToString

    pure subroutine simpleParseAs(self, string, errors, temperature)
        class(TemperatureSimpleUnit_t), intent(in) :: self
        type(VARYING_STRING), intent(in) :: string
        type(ErrorList_t), intent(out) :: errors
        type(Temperature_t), intent(out) :: temperature

        type(ParseResult_t) :: parse_result

        parse_result = parseWith(theParser, string)
        if (parse_result%ok) then
            select type (the_number => parse_result%parsed)
            type is (ParsedRational_t)
                temperature = the_number%value_.unit.self
            end select
        else
            call errors%appendError(Fatal( &
                    PARSE_ERROR, &
                    Module_("Temperature_m"), &
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

    pure subroutine gnuplotParseAs(self, string, errors, temperature)
        class(TemperatureGnuplotUnit_t), intent(in) :: self
        type(VARYING_STRING), intent(in) :: string
        type(ErrorList_t), intent(out) :: errors
        type(Temperature_t), intent(out) :: temperature

        type(ParseResult_t) :: parse_result

        parse_result = parseWith(theParser, string)
        if (parse_result%ok) then
            select type (the_number => parse_result%parsed)
            type is (ParsedRational_t)
                temperature = the_number%value_.unit.self
            end select
        else
            call errors%appendError(Fatal( &
                    PARSE_ERROR, &
                    Module_("Temperature_m"), &
                    Procedure_("gnuplotParseAs"), &
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
    end subroutine gnuplotParseAs

    pure subroutine latexParseAs(self, string, errors, temperature)
        class(TemperatureLatexUnit_t), intent(in) :: self
        type(VARYING_STRING), intent(in) :: string
        type(ErrorList_t), intent(out) :: errors
        type(Temperature_t), intent(out) :: temperature

        type(ParseResult_t) :: parse_result

        parse_result = parseWith(theParser, string)
        if (parse_result%ok) then
            select type (the_number => parse_result%parsed)
            type is (ParsedRational_t)
                temperature = the_number%value_.unit.self
            end select
        else
            call errors%appendError(Fatal( &
                    PARSE_ERROR, &
                    Module_("Temperature_m"), &
                    Procedure_("latexParseAs"), &
                    parse_result%message))
        end if
    contains
        pure function theParser(state_) result(result_)
            type(State_t), intent(in) :: state_
            type(ParserOutput_t) :: result_

            result_ = thenDrop( &
                    thenDrop( &
                            thenDrop( &
                                    thenDrop( &
                                            dropThen( &
                                                    dropThen(parseSI, parseOpenBrace, state_), &
                                                    parseRational), &
                                            parseCloseBrace), &
                                    parseOpenBrace), &
                            parseUnit), &
                    parseCloseBrace)
        end function theParser

        pure function parseUnit(state_) result(result_)
            type(State_t), intent(in) :: state_
            type(ParserOutput_t) :: result_

            result_ = parseString(trim(self%symbol), state_)
        end function parseUnit
    end subroutine latexParseAs

    elemental function gnuplotUnitToString(self) result(string)
        class(TemperatureGnuplotUnit_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = trim(self%symbol)
    end function gnuplotUnitToString

    pure function gnuplotValueToString(self, value_) result(string)
        class(TemperatureGnuplotUnit_t), intent(in) :: self
        type(VARYING_STRING), intent(in) :: value_
        type(VARYING_STRING) :: string

        string = value_ // " " // self%toString()
    end function gnuplotValueToString

    elemental function latexUnitToString(self) result(string)
        class(TemperatureLatexUnit_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = wrapInLatexUnit(trim(self%symbol))
    end function latexUnitToString

    pure function latexValueToString(self, value_) result(string)
        class(TemperatureLatexUnit_t), intent(in) :: self
        type(VARYING_STRING), intent(in) :: value_
        type(VARYING_STRING) :: string

        string = wrapInLatexQuantity(value_, trim(self%symbol))
    end function latexValueToString

    pure subroutine simpleUnitFromStringC(string, errors, unit)
        character(len=*), intent(in) :: string
        type(ErrorList_t), intent(out) :: errors
        type(TemperatureSimpleUnit_t), intent(out) :: unit

        type(ErrorList_t) :: errors_

        call fromString(var_str(string), PROVIDED_UNITS, errors_, unit)
        call errors%appendErrors( &
                errors_, &
                Module_("Temperature_m"), &
                Procedure_("simpleUnitFromStringC"))
    end subroutine simpleUnitFromStringC

    pure subroutine simpleUnitFromStringS(string, errors, unit)
        type(VARYING_STRING), intent(in) :: string
        type(ErrorList_t), intent(out) :: errors
        type(TemperatureSimpleUnit_t), intent(out) :: unit

        type(ErrorList_t) :: errors_

        call fromString(string, PROVIDED_UNITS, errors_, unit)
        call errors%appendErrors( &
                errors_, &
                Module_("Temperature_m"), &
                Procedure_("simpleUnitFromStringS"))
    end subroutine simpleUnitFromStringS

    pure subroutine simpleUnitFromStringWithUnitsC(string, units, errors, unit)
        character(len=*), intent(in) :: string
        type(TemperatureSimpleUnit_t), intent(in) :: units(:)
        type(ErrorList_t), intent(out) :: errors
        type(TemperatureSimpleUnit_t), intent(out) :: unit

        type(ErrorList_t) :: errors_

        call fromString(var_str(string), units, errors_, unit)
        call errors%appendErrors( &
                errors_, &
                Module_("Temperature_m"), &
                Procedure_("simpleUnitFromStringWithUnitsC"))
    end subroutine simpleUnitFromStringWithUnitsC

    pure subroutine simpleUnitFromStringWithUnitsS(string, units, errors, unit)
        type(VARYING_STRING), intent(in) :: string
        type(TemperatureSimpleUnit_t), intent(in) :: units(:)
        type(ErrorList_t), intent(out) :: errors
        type(TemperatureSimpleUnit_t), intent(out) :: unit

        type(ErrorList_t) :: errors_
        integer :: which_unit

        call selectUnit(string, units, errors, which_unit)
        if (errors_%hasAny()) then
            call errors%appendErrors( &
                    errors_, &
                    Module_("Temperature_m"), &
                    Procedure_("simpleUnitFromStringWithUnitsS"))
        else
            unit = units(which_unit)
        end if
    end subroutine simpleUnitFromStringWithUnitsS

    pure subroutine selectUnit(string, units, errors, index)
        type(VARYING_STRING), intent(in) :: string
        class(TemperatureUnit_t), intent(in) :: units(:)
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
                Module_("Temperature_m"), &
                Procedure_("selectUnit"), &
                '"' // string // '", known units: [' // join(unit_strings, ', ') // ']'))
    end subroutine selectUnit
end module Temperature_m
