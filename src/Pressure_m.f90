module Pressure_m
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
            BAR_PER_PASCAL, &
            DYNES_PER_SQUARE_CENTIMETER_PER_PASCAL, &
            KILOPASCALS_PER_PASCAL, &
            KILOPONDS_PER_SQUARE_CENTIMETER_PER_PASCAL, &
            MEGAPASCALS_PER_PASCAL, &
            POUNDS_PER_SQUARE_INCH_PER_PASCAL
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

    type, public :: Pressure_t
        double precision :: pascals
    contains
        private
        procedure :: toUnits
        generic, public :: operator(.in.) => toUnits
        procedure, pass(pressure) :: doubleTimesPressure
        procedure, pass(pressure) :: integerTimesPressure
        procedure, pass(pressure) :: pressureTimesDouble
        procedure, pass(pressure) :: pressureTimesInteger
        generic, public :: operator(*) => &
                doubleTimesPressure, &
                integerTimesPressure, &
                pressureTimesDouble, &
                pressureTimesInteger
        procedure :: pressureDividedByDouble
        procedure :: pressureDividedByInteger
        procedure, pass(numerator) :: pressureDividedByPressure
        generic, public :: operator(/) => &
                pressureDividedByDouble, &
                pressureDividedByInteger, &
                pressureDividedByPressure
        procedure :: pressurePlusPressure
        generic, public :: operator(+) => pressurePlusPressure
        procedure :: pressureMinusPressure
        generic, public :: operator(-) => pressureMinusPressure
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
    end type Pressure_t

    type, abstract, public :: PressureUnit_t
        double precision :: conversion_factor
    contains
        procedure(justUnitToString), deferred :: unitToString
        procedure(unitWithValueToString), deferred :: valueToString
        generic :: toString => unitToString, valueToString
        procedure(parseAsI), deferred :: parseAs
    end type PressureUnit_t

    type, extends(PressureUnit_t), public :: PressureSimpleUnit_t
        character(len=20) :: symbol
    contains
        procedure :: unitToString => simpleUnitToString
        procedure :: valueToString => simpleValueToString
        procedure :: parseAs => simpleParseAs
    end type PressureSimpleUnit_t

    type, extends(PressureUnit_t), public :: PressureGnuplotUnit_t
        character(len=50) :: symbol
    contains
        procedure :: unitToString => gnuplotUnitToString
        procedure :: valueToString => gnuplotValueToString
        procedure :: parseAs => gnuplotParseAs
    end type PressureGnuplotUnit_t

    abstract interface
        elemental function justUnitToString(self) result(string)
            import PressureUnit_t, VARYING_STRING
            class(PressureUnit_t), intent(in) :: self
            type(VARYING_STRING) :: string
        end function justUnitToString

        pure function unitWithValueToString(self, value_) result(string)
            import PressureUnit_t, VARYING_STRING
            class(PressureUnit_t), intent(in) :: self
            type(VARYING_STRING), intent(in) :: value_
            type(VARYING_STRING) :: string
        end function unitWithValueToString

        pure subroutine parseAsI(self, string, errors, pressure)
            import ErrorList_t, Pressure_t, PressureUnit_t, VARYING_STRING
            class(PressureUnit_t), intent(in) :: self
            type(VARYING_STRING), intent(in) :: string
            type(ErrorList_t), intent(out) :: errors
            type(Pressure_t), intent(out) :: pressure
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
        module procedure sumPressure
    end interface sum

    type(PressureSimpleUnit_t), parameter, public :: BAR = &
            PressureSimpleUnit_t( &
                    conversion_factor = BAR_PER_PASCAL, &
                    symbol = "bar")
    type(PressureGnuplotUnit_t), parameter, public :: BAR_GNUPLOT = &
            PressureGnuplotUnit_t( &
                    conversion_factor = BAR_PER_PASCAL, &
                    symbol = "bar")
    type(PressureSimpleUnit_t), parameter, public :: DYNES_PER_SQUARE_CENTIMETER = &
            PressureSimpleUnit_t( &
                    conversion_factor = DYNES_PER_SQUARE_CENTIMETER_PER_PASCAL, &
                    symbol = "dyn/cm^2")
    type(PressureGnuplotUnit_t), parameter, public :: DYNES_PER_SQUARE_CENTIMETER_GNUPLOT = &
            PressureGnuplotUnit_t( &
                    conversion_factor = DYNES_PER_SQUARE_CENTIMETER_PER_PASCAL, &
                    symbol = "dyn/cm^2")
    type(PressureSimpleUnit_t), parameter, public :: KILOPASCALS = &
            PressureSimpleUnit_t( &
                    conversion_factor = KILOPASCALS_PER_PASCAL, &
                    symbol = "kPa")
    type(PressureGnuplotUnit_t), parameter, public :: KILOPASCALS_GNUPLOT = &
            PressureGnuplotUnit_t( &
                    conversion_factor = KILOPASCALS_PER_PASCAL, &
                    symbol = "kPa")
    type(PressureSimpleUnit_t), parameter, public :: KILOPONDS_PER_SQUARE_CENTIMETER = &
            PressureSimpleUnit_t( &
                    conversion_factor = KILOPONDS_PER_SQUARE_CENTIMETER_PER_PASCAL, &
                    symbol = "kp/cm^2")
    type(PressureGnuplotUnit_t), parameter, public :: KILOPONDS_PER_SQUARE_CENTIMETER_GNUPLOT = &
            PressureGnuplotUnit_t( &
                    conversion_factor = KILOPONDS_PER_SQUARE_CENTIMETER_PER_PASCAL, &
                    symbol = "kp/cm^2")
    type(PressureSimpleUnit_t), parameter, public :: MEGAPASCALS = &
            PressureSimpleUnit_t( &
                    conversion_factor = MEGAPASCALS_PER_PASCAL, &
                    symbol = "MPa")
    type(PressureGnuplotUnit_t), parameter, public :: MEGAPASCALS_GNUPLOT = &
            PressureGnuplotUnit_t( &
                    conversion_factor = MEGAPASCALS_PER_PASCAL, &
                    symbol = "MPa")
    type(PressureSimpleUnit_t), parameter, public :: PASCALS = &
            PressureSimpleUnit_t( &
                    conversion_factor = 1.0d0, &
                    symbol = "Pa")
    type(PressureGnuplotUnit_t), parameter, public :: PASCALS_GNUPLOT = &
            PressureGnuplotUnit_t( &
                    conversion_factor = 1.0d0, &
                    symbol = "Pa")
    type(PressureSimpleUnit_t), parameter, public :: POUNDS_PER_SQUARE_INCH = &
            PressureSimpleUnit_t( &
                    conversion_factor = POUNDS_PER_SQUARE_INCH_PER_PASCAL, &
                    symbol = "psi")
    type(PressureGnuplotUnit_t), parameter, public :: POUNDS_PER_SQUARE_INCH_GNUPLOT = &
            PressureGnuplotUnit_t( &
                    conversion_factor = POUNDS_PER_SQUARE_INCH_PER_PASCAL, &
                    symbol = "psi")

    type(PressureSimpleUnit_t), public :: DEFAULT_OUTPUT_UNITS = PASCALS

    type(PressureSimpleUnit_t), parameter, public :: PROVIDED_UNITS(*) = &
            [BAR, &
            DYNES_PER_SQUARE_CENTIMETER, &
            KILOPASCALS, &
            KILOPONDS_PER_SQUARE_CENTIMETER, &
            MEGAPASCALS, &
            PASCALS, &
            POUNDS_PER_SQUARE_INCH]
    type(PressureGnuplotUnit_t), parameter, public :: PROVIDED_GNUPLOT_UNITS(*) = &
            [BAR_GNUPLOT, &
            DYNES_PER_SQUARE_CENTIMETER_GNUPLOT, &
            KILOPASCALS_GNUPLOT, &
            KILOPONDS_PER_SQUARE_CENTIMETER_GNUPLOT, &
            MEGAPASCALS_GNUPLOT, &
            PASCALS_GNUPLOT, &
            POUNDS_PER_SQUARE_INCH_GNUPLOT]

    public :: operator(.unit.), fromString, selectUnit, sum
contains
    pure subroutine fromStringBasicC(string, errors, pressure)
        character(len=*), intent(in) :: string
        type(ErrorList_t), intent(out) :: errors
        type(Pressure_t), intent(out) :: pressure

        type(ErrorList_t) :: errors_

        call fromString( &
                var_str(string), PROVIDED_UNITS, errors_, pressure)
        call errors%appendErrors( &
                errors_, &
                Module_("Pressure_m"), &
                Procedure_("fromStringBasicC"))
    end subroutine fromStringBasicC

    pure subroutine fromStringBasicS(string, errors, pressure)
        type(VARYING_STRING), intent(in) :: string
        type(ErrorList_t), intent(out) :: errors
        type(Pressure_t), intent(out) :: pressure

        type(ErrorList_t) :: errors_

        call fromString( &
                string, PROVIDED_UNITS, errors_, pressure)
        call errors%appendErrors( &
                errors_, &
                Module_("Pressure_m"), &
                Procedure_("fromStringBasicS"))
    end subroutine fromStringBasicS

    pure subroutine fromStringWithUnitsC(string, units, errors, pressure)
        character(len=*), intent(in) :: string
        class(PressureUnit_t), intent(in) :: units(:)
        type(ErrorList_t), intent(out) :: errors
        type(Pressure_t), intent(out) :: pressure

        type(ErrorList_t) :: errors_

        call fromString( &
                var_str(string), units, errors_, pressure)
        call errors%appendErrors( &
                errors_, &
                Module_("Pressure_m"), &
                Procedure_("fromStringWithUnitsC"))
    end subroutine fromStringWithUnitsC

    pure subroutine fromStringWithUnitsS(string, units, errors, pressure)
        type(VARYING_STRING), intent(in) :: string
        class(PressureUnit_t), intent(in) :: units(:)
        type(ErrorList_t), intent(out) :: errors
        type(Pressure_t), intent(out) :: pressure

        type(ErrorList_t) :: all_errors(size(units))
        integer :: i

        do i = 1, size(units)
            call units(i)%parseAs(string, all_errors(i), pressure)
            if (.not. all_errors(i)%hasAny()) return
        end do
        do i = 1, size(units)
            call errors%appendErrors( &
                    all_errors(i), &
                    Module_("Pressure_m"), &
                    Procedure_("fromStringWithUnitsS"))
        end do
    end subroutine fromStringWithUnitsS

    elemental function fromUnits(value_, units) result(pressure)
        double precision, intent(in) :: value_
        class(PressureUnit_t), intent(in) :: units
        type(Pressure_t) :: pressure

        pressure%pascals = value_ / units%conversion_factor
    end function fromUnits

    elemental function toUnits(self, units) result(pressure)
        class(Pressure_t), intent(in) :: self
        class(PressureUnit_t), intent(in) :: units
        double precision :: pressure

        pressure = self%pascals * units%conversion_factor
    end function toUnits

    elemental function doubleTimesPressure( &
            multiplier, pressure) result(new_pressure)
        double precision, intent(in) :: multiplier
        class(Pressure_t), intent(in) :: pressure
        type(Pressure_t) :: new_pressure

        new_pressure%pascals = &
                multiplier * pressure%pascals
    end function doubleTimesPressure

    elemental function integerTimesPressure( &
            multiplier, pressure) result(new_pressure)
        integer, intent(in) :: multiplier
        class(Pressure_t), intent(in) :: pressure
        type(Pressure_t) :: new_pressure

        new_pressure%pascals = &
                dble(multiplier) * pressure%pascals
    end function integerTimesPressure

    elemental function pressureTimesDouble( &
            pressure, multiplier) result(new_pressure)
        class(Pressure_t), intent(in) :: pressure
        double precision, intent(in) :: multiplier
        type(Pressure_t) :: new_pressure

        new_pressure%pascals = &
                pressure%pascals * multiplier
    end function pressureTimesDouble

    elemental function pressureTimesInteger( &
            pressure, multiplier) result(new_pressure)
        class(Pressure_t), intent(in) :: pressure
        integer, intent(in) :: multiplier
        type(Pressure_t) :: new_pressure

        new_pressure%pascals = &
                pressure%pascals * dble(multiplier)
    end function pressureTimesInteger

    elemental function pressureDividedByDouble( &
            pressure, divisor) result(new_pressure)
        class(Pressure_t), intent(in) :: pressure
        double precision, intent(in) :: divisor
        type(Pressure_t) :: new_pressure

        new_pressure%pascals = &
                pressure%pascals / divisor
    end function pressureDividedByDouble

    elemental function pressureDividedByInteger( &
            pressure, divisor) result(new_pressure)
        class(Pressure_t), intent(in) :: pressure
        integer, intent(in) :: divisor
        type(Pressure_t) :: new_pressure

        new_pressure%pascals = &
                pressure%pascals / dble(divisor)
    end function pressureDividedByInteger

    elemental function pressureDividedByPressure( &
            numerator, denomenator) result(ratio)
        class(Pressure_t), intent(in) :: numerator
        class(Pressure_t), intent(in) :: denomenator
        double precision :: ratio

        ratio = numerator%pascals / denomenator%pascals
    end function pressureDividedByPressure

    elemental function pressurePlusPressure( &
            pressure1, pressure2) result(new_pressure)
        class(Pressure_t), intent(in) :: pressure1
        class(Pressure_t), intent(in) :: pressure2
        type(Pressure_t) :: new_pressure

        new_pressure%pascals = &
                pressure1%pascals + pressure2%pascals
    end function pressurePlusPressure

    elemental function pressureMinusPressure( &
            pressure1, pressure2) result(new_pressure)
        class(Pressure_t), intent(in) :: pressure1
        class(Pressure_t), intent(in) :: pressure2
        type(Pressure_t) :: new_pressure

        new_pressure%pascals = &
                pressure1%pascals - pressure2%pascals
    end function pressureMinusPressure

    pure function sumPressure(pressures)
        type(Pressure_t), intent(in) :: pressures(:)
        type(Pressure_t) :: sumPressure

        sumPressure%pascals = sum(pressures%pascals)
    end function sumPressure

    elemental function greaterThan(lhs, rhs)
        class(Pressure_t), intent(in) :: lhs
        class(Pressure_t), intent(in) :: rhs
        logical :: greaterThan

        greaterThan = lhs%pascals > rhs%pascals
    end function greaterThan

    elemental function lessThan(lhs,rhs)
        class(Pressure_t), intent(in) :: lhs
        class(Pressure_t), intent(in) :: rhs
        logical :: lessThan

        lessThan = lhs%pascals < rhs%pascals
    end function lessThan

    elemental function greaterThanOrEqual(lhs, rhs)
        class(Pressure_t), intent(in) :: lhs
        class(Pressure_t), intent(in) :: rhs
        logical :: greaterThanOrEqual

        greaterThanOrEqual = lhs%pascals >= rhs%pascals
    end function greaterThanOrEqual

    elemental function lessThanOrEqual(lhs, rhs)
        class(Pressure_t), intent(in) :: lhs
        class(Pressure_t), intent(in) :: rhs
        logical :: lessThanOrEqual

        lessThanOrEqual = lhs%pascals <= rhs%pascals
    end function lessThanOrEqual

    elemental function equal_(lhs,rhs)
        class(Pressure_t), intent(in) :: lhs
        class(Pressure_t), intent(in) :: rhs
        logical :: equal_

        equal_ = lhs%pascals .safeEq. rhs%pascals
    end function equal_

    elemental function equalWithinAbsolute(lhs, rhs, within)
        class(Pressure_t), intent(in) :: lhs
        class(Pressure_t), intent(in) :: rhs
        class(Pressure_t), intent(in) :: within
        logical :: equalWithinAbsolute

        equalWithinAbsolute = equalWithinAbsolute_( &
                lhs%pascals, rhs%pascals, within%pascals)
    end function equalWithinAbsolute

    elemental function equalWithinRelative(lhs, rhs, within)
        class(Pressure_t), intent(in) :: lhs
        class(Pressure_t), intent(in) :: rhs
        double precision, intent(in) :: within
        logical :: equalWithinRelative

        equalWithinRelative = equalWithinRelative_( &
                lhs%pascals, rhs%pascals, within)
    end function equalWithinRelative

    elemental function notEqual(lhs, rhs)
        class(Pressure_t), intent(in) :: lhs
        class(Pressure_t), intent(in) :: rhs
        logical :: notEqual

        notEqual = .not. lhs == rhs
    end function notEqual

    elemental function toStringFullPrecision(self) result(string)
        class(Pressure_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = self%toStringIn(DEFAULT_OUTPUT_UNITS)
    end function toStringFullPrecision

    elemental function toStringWithPrecision(self, significant_digits) result(string)
        class(Pressure_t), intent(in) :: self
        integer, intent(in) :: significant_digits
        type(VARYING_STRING) :: string

        string = self%toStringIn(DEFAULT_OUTPUT_UNITS, significant_digits)
    end function toStringWithPrecision

    elemental function toStringInFullPrecision(self, units) result(string)
        class(Pressure_t), intent(in) :: self
        class(PressureUnit_t), intent(in) :: units
        type(VARYING_STRING) :: string

        string = units%toString(toString(self.in.units))
    end function toStringInFullPrecision

    elemental function toStringInWithPrecision( &
            self, units, significant_digits) result(string)
        class(Pressure_t), intent(in) :: self
        class(PressureUnit_t), intent(in) :: units
        integer, intent(in) :: significant_digits
        type(VARYING_STRING) :: string

        string = units%toString(toString(self.in.units, significant_digits))
    end function toStringInWithPrecision

    elemental function simpleUnitToString(self) result(string)
        class(PressureSimpleUnit_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = trim(self%symbol)
    end function simpleUnitToString

    pure function simpleValueToString(self, value_) result(string)
        class(PressureSimpleUnit_t), intent(in) :: self
        type(VARYING_STRING), intent(in) :: value_
        type(VARYING_STRING) :: string

        string = value_ // " " // self%toString()
    end function simpleValueToString

    pure subroutine simpleParseAs(self, string, errors, pressure)
        class(PressureSimpleUnit_t), intent(in) :: self
        type(VARYING_STRING), intent(in) :: string
        type(ErrorList_t), intent(out) :: errors
        type(Pressure_t), intent(out) :: pressure

        type(ParseResult_t) :: parse_result

        parse_result = parseWith(theParser, string)
        if (parse_result%ok) then
            select type (the_number => parse_result%parsed)
            type is (ParsedRational_t)
                pressure = the_number%value_.unit.self
            end select
        else
            call errors%appendError(Fatal( &
                    PARSE_ERROR, &
                    Module_("Pressure_m"), &
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

    pure subroutine gnuplotParseAs(self, string, errors, pressure)
        class(PressureGnuplotUnit_t), intent(in) :: self
        type(VARYING_STRING), intent(in) :: string
        type(ErrorList_t), intent(out) :: errors
        type(Pressure_t), intent(out) :: pressure

        type(ParseResult_t) :: parse_result

        parse_result = parseWith(theParser, string)
        if (parse_result%ok) then
            select type (the_number => parse_result%parsed)
            type is (ParsedRational_t)
                pressure = the_number%value_.unit.self
            end select
        else
            call errors%appendError(Fatal( &
                    PARSE_ERROR, &
                    Module_("Pressure_m"), &
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

    elemental function gnuplotUnitToString(self) result(string)
        class(PressureGnuplotUnit_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = trim(self%symbol)
    end function gnuplotUnitToString

    pure function gnuplotValueToString(self, value_) result(string)
        class(PressureGnuplotUnit_t), intent(in) :: self
        type(VARYING_STRING), intent(in) :: value_
        type(VARYING_STRING) :: string

        string = value_ // " " // self%toString()
    end function gnuplotValueToString

    pure subroutine simpleUnitFromStringC(string, errors, unit)
        character(len=*), intent(in) :: string
        type(ErrorList_t), intent(out) :: errors
        type(PressureSimpleUnit_t), intent(out) :: unit

        type(ErrorList_t) :: errors_

        call fromString(var_str(string), PROVIDED_UNITS, errors_, unit)
        call errors%appendErrors( &
                errors_, &
                Module_("Pressure_m"), &
                Procedure_("simpleUnitFromStringC"))
    end subroutine simpleUnitFromStringC

    pure subroutine simpleUnitFromStringS(string, errors, unit)
        type(VARYING_STRING), intent(in) :: string
        type(ErrorList_t), intent(out) :: errors
        type(PressureSimpleUnit_t), intent(out) :: unit

        type(ErrorList_t) :: errors_

        call fromString(string, PROVIDED_UNITS, errors_, unit)
        call errors%appendErrors( &
                errors_, &
                Module_("Pressure_m"), &
                Procedure_("simpleUnitFromStringS"))
    end subroutine simpleUnitFromStringS

    pure subroutine simpleUnitFromStringWithUnitsC(string, units, errors, unit)
        character(len=*), intent(in) :: string
        type(PressureSimpleUnit_t), intent(in) :: units(:)
        type(ErrorList_t), intent(out) :: errors
        type(PressureSimpleUnit_t), intent(out) :: unit

        type(ErrorList_t) :: errors_

        call fromString(var_str(string), units, errors_, unit)
        call errors%appendErrors( &
                errors_, &
                Module_("Pressure_m"), &
                Procedure_("simpleUnitFromStringWithUnitsC"))
    end subroutine simpleUnitFromStringWithUnitsC

    pure subroutine simpleUnitFromStringWithUnitsS(string, units, errors, unit)
        type(VARYING_STRING), intent(in) :: string
        type(PressureSimpleUnit_t), intent(in) :: units(:)
        type(ErrorList_t), intent(out) :: errors
        type(PressureSimpleUnit_t), intent(out) :: unit

        type(ErrorList_t) :: errors_
        integer :: which_unit

        call selectUnit(string, units, errors, which_unit)
        if (errors_%hasAny()) then
            call errors%appendErrors( &
                    errors_, &
                    Module_("Pressure_m"), &
                    Procedure_("simpleUnitFromStringWithUnitsS"))
        else
            unit = units(which_unit)
        end if
    end subroutine simpleUnitFromStringWithUnitsS

    pure subroutine selectUnit(string, units, errors, index)
        type(VARYING_STRING), intent(in) :: string
        class(PressureUnit_t), intent(in) :: units(:)
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
                Module_("Pressure_m"), &
                Procedure_("selectUnit"), &
                '"' // string // '", known units: [' // join(unit_strings, ', ') // ']'))
    end subroutine selectUnit
end module Pressure_m
