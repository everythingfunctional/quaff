module Pressure_m
    use Conversion_factors_m, only: &
            DYNES_PER_SQUARE_CENTIMETER_PER_PASCAL, &
            KILOPASCALS_PER_PASCAL, &
            KILOPONDS_PER_SQUARE_CENTIMETER_PER_PASCAL, &
            MEGAPASCALS_PER_PASCAL, &
            POUNDS_PER_SQUARE_INCH_PER_PASCAL
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
    end type Pressure_t

    type, public :: PressureUnit_t
        double precision :: conversion_factor
        character(len=20) :: symbol
        character(len=50) :: gnuplot_symbol
        character(len=100) :: latex_symbol
    contains
        procedure :: toString => unitToString
        procedure :: toGnuplotString => unitToGnuplotString
        procedure :: toLatexString => unitToLatexString
    end type PressureUnit_t

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

    interface sum
        module procedure sumPressure
    end interface sum

    type(PressureUnit_t), parameter, public :: DYNES_PER_SQUARE_CENTIMETER = &
            PressureUnit_t( &
                    conversion_factor = DYNES_PER_SQUARE_CENTIMETER_PER_PASCAL, &
                    symbol = "dyn/cm^2", &
                    gnuplot_symbol = "dyn/cm^2", &
                    latex_symbol = "\dyne\per\square\centi\meter")
    type(PressureUnit_t), parameter, public :: KILOPASCALS = &
            PressureUnit_t( &
                    conversion_factor = KILOPASCALS_PER_PASCAL, &
                    symbol = "kPa", &
                    gnuplot_symbol = "kPa", &
                    latex_symbol = "\kilo\pascal")
    type(PressureUnit_t), parameter, public :: KILOPONDS_PER_SQUARE_CENTIMETER = &
            PressureUnit_t( &
                    conversion_factor = KILOPONDS_PER_SQUARE_CENTIMETER_PER_PASCAL, &
                    symbol = "kp/cm^2", &
                    gnuplot_symbol = "kp/cm^2", &
                    latex_symbol = "\kilopond\per\square\centi\meter")
    type(PressureUnit_t), parameter, public :: MEGAPASCALS = &
            PressureUnit_t( &
                    conversion_factor = MEGAPASCALS_PER_PASCAL, &
                    symbol = "MPa", &
                    gnuplot_symbol = "MPa", &
                    latex_symbol = "\mega\pascal")
    type(PressureUnit_t), parameter, public :: PASCALS = &
            PressureUnit_t( &
                    conversion_factor = 1.0d0, &
                    symbol = "Pa", &
                    gnuplot_symbol = "Pa", &
                    latex_symbol = "\pascal")
    type(PressureUnit_t), parameter, public :: POUNDS_PER_SQUARE_INCH = &
            PressureUnit_t( &
                    conversion_factor = POUNDS_PER_SQUARE_INCH_PER_PASCAL, &
                    symbol = "psi", &
                    gnuplot_symbol = "psi", &
                    latex_symbol = "\psi")

    type(PressureUnit_t), public :: DEFAULT_OUTPUT_UNITS = PASCALS

    type(PressureUnit_t), parameter, public :: PROVIDED_UNITS(*) = &
            [DYNES_PER_SQUARE_CENTIMETER, &
            KILOPASCALS, &
            KILOPONDS_PER_SQUARE_CENTIMETER, &
            MEGAPASCALS, &
            PASCALS, &
            POUNDS_PER_SQUARE_INCH]

    public :: operator(.unit.), fromString, sum
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
        type(PressureUnit_t), intent(in) :: units(:)
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
        type(PressureUnit_t), intent(in) :: units(:)
        type(ErrorList_t), intent(out) :: errors
        type(Pressure_t), intent(out) :: pressure

        double precision :: number
        character(len=100) :: number_chars
        type(VARYING_STRING) :: number_string
        integer :: status
        type(VARYING_STRING) :: symbol
        type(PressureUnit_t) :: unit
        type(ErrorList_t) :: unit_errors

        pressure%pascals = 0.0d0
        symbol = string
        call split(symbol, number_string, " ")
        if (len(symbol) == 0) then
            call errors%appendError(Fatal( &
                    PARSE_ERROR, &
                    Module_("Pressure_m"), &
                    Procedure_("fromStringWithUnitsS"), &
                    'No unit symbol found in string "' // string // '"'))
            return
        end if
        number_chars = number_string
        read(number_chars, *, iostat=status) number
        if (status /= 0) then
            call errors%appendError(Fatal( &
                    PARSE_ERROR, &
                    Module_("Pressure_m"), &
                    Procedure_("fromStringWithUnitsS"), &
                    'Error parsing number from string "' // number_string // '"'))
        end if
        call fromString(symbol, units, unit_errors, unit)
        pressure = number.unit.unit
        call errors%appendErrors( &
                unit_errors, &
                Module_("Pressure_m"), &
                Procedure_("fromStringWithUnitsS"))
    end subroutine fromStringWithUnitsS

    elemental function fromUnits(value_, units) result(pressure)
        double precision, intent(in) :: value_
        type(PressureUnit_t), intent(in) :: units
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

        string = toString(self.in.units) // " " // units%toString()
    end function toStringInFullPrecision

    elemental function toStringInWithPrecision( &
            self, units, significant_digits) result(string)
        class(Pressure_t), intent(in) :: self
        class(PressureUnit_t), intent(in) :: units
        integer, intent(in) :: significant_digits
        type(VARYING_STRING) :: string

        string = &
                toString(self.in.units, significant_digits) &
                // " " // units%toString()
    end function toStringInWithPrecision

    elemental function toGnuplotStringFullPrecision(self) result(string)
        class(Pressure_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = self%toGnuplotStringIn(DEFAULT_OUTPUT_UNITS)
    end function toGnuplotStringFullPrecision

    elemental function toGnuplotStringWithPrecision( &
            self, significant_digits) result(string)
        class(Pressure_t), intent(in) :: self
        integer, intent(in) :: significant_digits
        type(VARYING_STRING) :: string

        string = self%toGnuplotStringIn( &
                DEFAULT_OUTPUT_UNITS, significant_digits)
    end function toGnuplotStringWithPrecision

    elemental function toGnuplotStringInFullPrecision(self, units) result(string)
        class(Pressure_t), intent(in) :: self
        class(PressureUnit_t), intent(in) :: units
        type(VARYING_STRING) :: string

        string = toString(self.in.units) // " " // units%toGnuplotString()
    end function toGnuplotStringInFullPrecision

    elemental function toGnuplotStringInWithPrecision( &
            self, units, significant_digits) result(string)
        class(Pressure_t), intent(in) :: self
        class(PressureUnit_t), intent(in) :: units
        integer, intent(in) :: significant_digits
        type(VARYING_STRING) :: string

        string = &
                toString(self.in.units, significant_digits) &
                // " " // units%toGnuplotString()
    end function toGnuplotStringInWithPrecision

    elemental function toLatexStringFullPrecision(self) result(string)
        class(Pressure_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = self%toLatexStringIn(DEFAULT_OUTPUT_UNITS)
    end function toLatexStringFullPrecision

    elemental function toLatexStringWithPrecision(self, significant_digits) result(string)
        class(Pressure_t), intent(in) :: self
        integer, intent(in) :: significant_digits
        type(VARYING_STRING) :: string

        string = self%toLatexStringIn(DEFAULT_OUTPUT_UNITS, significant_digits)
    end function toLatexStringWithPrecision

    elemental function toLatexStringInFullPrecision(self, units) result(string)
        class(Pressure_t), intent(in) :: self
        class(PressureUnit_t), intent(in) :: units
        type(VARYING_STRING) :: string

        string = wrapInLatexQuantity( &
                toString(self.in.units), units%toLatexString())
    end function toLatexStringInFullPrecision

    elemental function toLatexStringInWithPrecision( &
            self, units, significant_digits) result(string)
        class(Pressure_t), intent(in) :: self
        class(PressureUnit_t), intent(in) :: units
        integer, intent(in) :: significant_digits
        type(VARYING_STRING) :: string

        string = wrapInLatexQuantity( &
                toString(self.in.units, significant_digits), &
                units%toLatexString())
    end function toLatexStringInWithPrecision

    pure subroutine unitFromStringBasicC(string, errors, unit)
        character(len=*), intent(in) :: string
        type(ErrorList_t), intent(out) :: errors
        type(PressureUnit_t), intent(out) :: unit

        type(ErrorList_t) :: errors_

        call fromString( &
                var_str(string), PROVIDED_UNITS, errors_, unit)
        call errors%appendErrors( &
                errors_, &
                Module_("Pressure_m"), &
                Procedure_("unitFromStringBasicC"))
    end subroutine unitFromStringBasicC

    pure subroutine unitFromStringBasicS(string, errors, unit)
        type(VARYING_STRING), intent(in) :: string
        type(ErrorList_t), intent(out) :: errors
        type(PressureUnit_t), intent(out) :: unit

        type(ErrorList_t) :: errors_

        call fromString( &
                string, PROVIDED_UNITS, errors_, unit)
        call errors%appendErrors( &
                errors_, &
                Module_("Pressure_m"), &
                Procedure_("unitFromStringBasicS"))
    end subroutine unitFromStringBasicS

    pure subroutine unitFromStringWithUnitsC(string, units, errors, unit)
        character(len=*), intent(in) :: string
        type(PressureUnit_t), intent(in) :: units(:)
        type(ErrorList_t), intent(out) :: errors
        type(PressureUnit_t), intent(out) :: unit

        type(ErrorList_t) :: errors_

        call fromString(var_str(string), units, errors_, unit)
        call errors%appendErrors( &
                errors_, &
                Module_("Pressure_m"), &
                Procedure_("unitFromStringWithUnitsC"))
    end subroutine unitFromStringWithUnitsC

    pure subroutine unitFromStringWithUnitsS(string, units, errors, unit)
        type(VARYING_STRING), intent(in) :: string
        type(PressureUnit_t), intent(in) :: units(:)
        type(ErrorList_t), intent(out) :: errors
        type(PressureUnit_t), intent(out) :: unit

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
                    Module_("Pressure_m"), &
                    Procedure_("unitFromStringWithUnitsS"), &
                    '"' // string // '", known units: [' // join(unit_strings, ', ') // ']' ))
        end if
    end subroutine unitFromStringWithUnitsS

    elemental function unitToString(self) result(string)
        class(PressureUnit_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = trim(self%symbol)
    end function unitToString

    elemental function unitToGnuplotString(self) result(string)
        class(PressureUnit_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = trim(self%gnuplot_symbol)
    end function unitToGnuplotString

    elemental function unitToLatexString(self) result(string)
        class(PressureUnit_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = trim(self%latex_symbol)
    end function unitToLatexString
end module Pressure_m
