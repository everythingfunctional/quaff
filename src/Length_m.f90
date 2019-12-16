module Length_m
    use Conversion_factors_m, only: &
            CENTIMETERS_PER_METER, &
            FEET_PER_METER, &
            INCHES_PER_METER, &
            MICROINCHES_PER_METER, &
            MICROMETERS_PER_METER
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
    end type Length_t

    type, public :: LengthUnit_t
        double precision :: conversion_factor
        character(len=10) :: symbol
        character(len=50) :: gnuplot_symbol
        character(len=100) :: latex_symbol
    contains
        procedure :: toString => unitToString
        procedure :: toGnuplotString => unitToGnuplotString
        procedure :: toLatexString => unitToLatexString
    end type LengthUnit_t

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

    type(LengthUnit_t), parameter, public :: CENTIMETERS = &
            LengthUnit_t( &
                    conversion_factor = CENTIMETERS_PER_METER, &
                    symbol = "cm", &
                    gnuplot_symbol = "cm", &
                    latex_symbol = "\centi\meter")
    type(LengthUnit_t), parameter, public :: FEET = &
            LengthUnit_t( &
                    conversion_factor = FEET_PER_METER, &
                    symbol = "ft", &
                    gnuplot_symbol = "ft", &
                    latex_symbol = "\foot")
    type(LengthUnit_t), parameter, public :: INCHES = &
            LengthUnit_t( &
                    conversion_factor = INCHES_PER_METER, &
                    symbol = "in", &
                    gnuplot_symbol = "in", &
                    latex_symbol = "\inch")
    type(LengthUnit_t), parameter, public :: METERS = &
            LengthUnit_t( &
                    conversion_factor = 1.0d0, &
                    symbol = "m", &
                    gnuplot_symbol = "m", &
                    latex_symbol = "\meter")
    type(LengthUnit_t), parameter, public :: MICROINCHES = &
            LengthUnit_t( &
                    conversion_factor = MICROINCHES_PER_METER, &
                    symbol = "uin", &
                    gnuplot_symbol = "{/Symbol m}in", &
                    latex_symbol = "\micro\inch")
    type(LengthUnit_t), parameter, public :: MICROMETERS = &
            LengthUnit_t( &
                    conversion_factor = MICROMETERS_PER_METER, &
                    symbol = "um", &
                    gnuplot_symbol = "{/Symbol m}m", &
                    latex_symbol = "\micro\meter")

    type(LengthUnit_t), public :: DEFAULT_OUTPUT_UNITS = METERS

    type(LengthUnit_t), parameter, public :: PROVIDED_UNITS(*) = &
            [CENTIMETERS, FEET, INCHES, METERS, MICROINCHES, MICROMETERS]

    public :: operator(.unit.), fromString
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
        type(LengthUnit_t), intent(in) :: units(:)
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
        type(LengthUnit_t), intent(in) :: units(:)
        type(ErrorList_t), intent(out) :: errors
        type(Length_t), intent(out) :: length

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
        call fromString(symbol, units, unit_errors, unit)
        length = number.unit.unit
        call errors%appendErrors( &
                unit_errors, &
                Module_("Length_m"), &
                Procedure_("fromStringWithUnitsS"))
    end subroutine fromStringWithUnitsS

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

        string = toString(self.in.units) // " " // units%toString()
    end function toStringInFullPrecision

    elemental function toStringInWithPrecision( &
            self, units, significant_digits) result(string)
        class(Length_t), intent(in) :: self
        class(LengthUnit_t), intent(in) :: units
        integer, intent(in) :: significant_digits
        type(VARYING_STRING) :: string

        string = &
                toString(self.in.units, significant_digits) &
                // " " // units%toString()
    end function toStringInWithPrecision

    elemental function toGnuplotStringFullPrecision(self) result(string)
        class(Length_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = self%toGnuplotStringIn(DEFAULT_OUTPUT_UNITS)
    end function toGnuplotStringFullPrecision

    elemental function toGnuplotStringWithPrecision( &
            self, significant_digits) result(string)
        class(Length_t), intent(in) :: self
        integer, intent(in) :: significant_digits
        type(VARYING_STRING) :: string

        string = self%toGnuplotStringIn( &
                DEFAULT_OUTPUT_UNITS, significant_digits)
    end function toGnuplotStringWithPrecision

    elemental function toGnuplotStringInFullPrecision(self, units) result(string)
        class(Length_t), intent(in) :: self
        class(LengthUnit_t), intent(in) :: units
        type(VARYING_STRING) :: string

        string = toString(self.in.units) // " " // units%toGnuplotString()
    end function toGnuplotStringInFullPrecision

    elemental function toGnuplotStringInWithPrecision( &
            self, units, significant_digits) result(string)
        class(Length_t), intent(in) :: self
        class(LengthUnit_t), intent(in) :: units
        integer, intent(in) :: significant_digits
        type(VARYING_STRING) :: string

        string = &
                toString(self.in.units, significant_digits) &
                // " " // units%toGnuplotString()
    end function toGnuplotStringInWithPrecision

    elemental function toLatexStringFullPrecision(self) result(string)
        class(Length_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = self%toLatexStringIn(DEFAULT_OUTPUT_UNITS)
    end function toLatexStringFullPrecision

    elemental function toLatexStringWithPrecision(self, significant_digits) result(string)
        class(Length_t), intent(in) :: self
        integer, intent(in) :: significant_digits
        type(VARYING_STRING) :: string

        string = self%toLatexStringIn(DEFAULT_OUTPUT_UNITS, significant_digits)
    end function toLatexStringWithPrecision

    elemental function toLatexStringInFullPrecision(self, units) result(string)
        class(Length_t), intent(in) :: self
        class(LengthUnit_t), intent(in) :: units
        type(VARYING_STRING) :: string

        string = wrapInLatexQuantity( &
                toString(self.in.units), units%toLatexString())
    end function toLatexStringInFullPrecision

    elemental function toLatexStringInWithPrecision( &
            self, units, significant_digits) result(string)
        class(Length_t), intent(in) :: self
        class(LengthUnit_t), intent(in) :: units
        integer, intent(in) :: significant_digits
        type(VARYING_STRING) :: string

        string = wrapInLatexQuantity( &
                toString(self.in.units, significant_digits), &
                units%toLatexString())
    end function toLatexStringInWithPrecision

    pure subroutine unitFromStringBasicC(string, errors, unit)
        character(len=*), intent(in) :: string
        type(ErrorList_t), intent(out) :: errors
        type(LengthUnit_t), intent(out) :: unit

        type(ErrorList_t) :: errors_

        call fromString( &
                var_str(string), PROVIDED_UNITS, errors_, unit)
        call errors%appendErrors( &
                errors_, &
                Module_("Length_m"), &
                Procedure_("unitFromStringBasicC"))
    end subroutine unitFromStringBasicC

    pure subroutine unitFromStringBasicS(string, errors, unit)
        type(VARYING_STRING), intent(in) :: string
        type(ErrorList_t), intent(out) :: errors
        type(LengthUnit_t), intent(out) :: unit

        type(ErrorList_t) :: errors_

        call fromString( &
                string, PROVIDED_UNITS, errors_, unit)
        call errors%appendErrors( &
                errors_, &
                Module_("Length_m"), &
                Procedure_("unitFromStringBasicS"))
    end subroutine unitFromStringBasicS

    pure subroutine unitFromStringWithUnitsC(string, units, errors, unit)
        character(len=*), intent(in) :: string
        type(LengthUnit_t), intent(in) :: units(:)
        type(ErrorList_t), intent(out) :: errors
        type(LengthUnit_t), intent(out) :: unit

        type(ErrorList_t) :: errors_

        call fromString(var_str(string), units, errors_, unit)
        call errors%appendErrors( &
                errors_, &
                Module_("Length_m"), &
                Procedure_("unitFromStringWithUnitsC"))
    end subroutine unitFromStringWithUnitsC

    pure subroutine unitFromStringWithUnitsS(string, units, errors, unit)
        type(VARYING_STRING), intent(in) :: string
        type(LengthUnit_t), intent(in) :: units(:)
        type(ErrorList_t), intent(out) :: errors
        type(LengthUnit_t), intent(out) :: unit

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
    end subroutine unitFromStringWithUnitsS

    elemental function unitToString(self) result(string)
        class(LengthUnit_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = trim(self%symbol)
    end function unitToString

    elemental function unitToGnuplotString(self) result(string)
        class(LengthUnit_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = trim(self%gnuplot_symbol)
    end function unitToGnuplotString

    elemental function unitToLatexString(self) result(string)
        class(LengthUnit_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = trim(self%latex_symbol)
    end function unitToLatexString
end module Length_m
