module Angle_m
    use Conversion_factors_m, only: DEGREES_PER_RADIAN
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

    type, public :: Angle_t
        double precision :: radians
    contains
        private
        procedure :: toUnits
        generic, public :: operator(.in.) => toUnits
        procedure, pass(angle) :: doubleTimesAngle
        procedure, pass(angle) :: integerTimesAngle
        procedure, pass(angle) :: angleTimesDouble
        procedure, pass(angle) :: angleTimesInteger
        generic, public :: operator(*) => &
                doubleTimesAngle, &
                integerTimesAngle, &
                angleTimesDouble, &
                angleTimesInteger
        procedure :: angleDividedByDouble
        procedure :: angleDividedByInteger
        procedure, pass(numerator) :: angleDividedByAngle
        generic, public :: operator(/) => &
                angleDividedByDouble, &
                angleDividedByInteger, &
                angleDividedByAngle
        procedure :: anglePlusAngle
        generic, public :: operator(+) => anglePlusAngle
        procedure :: angleMinusAngle
        generic, public :: operator(-) => angleMinusAngle
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
    end type Angle_t

    type, public :: AngleUnit_t
        double precision :: conversion_factor
        character(len=10) :: symbol
        character(len=50) :: gnuplot_symbol
        character(len=100) :: latex_symbol
    contains
        procedure :: toString => unitToString
        procedure :: toGnuplotString => unitToGnuplotString
        procedure :: toLatexString => unitToLatexString
    end type AngleUnit_t

    interface operator(.unit.)
        module procedure fromUnits
    end interface operator(.unit.)

    interface sin
        module procedure sin_
    end interface sin

    interface cos
        module procedure cos_
    end interface cos

    interface tan
        module procedure tan_
    end interface tan

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

    type(AngleUnit_t), parameter, public :: DEGREES = &
            AngleUnit_t( &
                    conversion_factor = DEGREES_PER_RADIAN, &
                    symbol = "deg", &
                    gnuplot_symbol = "{/Symbol \260}", &
                    latex_symbol = "\degree")
    type(AngleUnit_t), parameter, public :: RADIANS = &
            AngleUnit_t( &
                    conversion_factor = 1.0d0, &
                    symbol = "rad", &
                    gnuplot_symbol = "rad", &
                    latex_symbol = "\radian")

    type(AngleUnit_t), public :: DEFAULT_OUTPUT_UNITS = RADIANS

    type(AngleUnit_t), parameter, public :: PROVIDED_UNITS(*) = &
            [DEGREES, RADIANS]

    public :: &
            operator(.unit.), &
            fromString, &
            sin, &
            cos, &
            tan, &
            asin_, &
            acos_, &
            atan_, &
            atan2_
contains
    pure subroutine fromStringBasicC(string, errors, angle)
        character(len=*), intent(in) :: string
        type(ErrorList_t), intent(out) :: errors
        type(Angle_t), intent(out) :: angle

        type(ErrorList_t) :: errors_

        call fromString( &
                var_str(string), PROVIDED_UNITS, errors_, angle)
        call errors%appendErrors( &
                errors_, &
                Module_("Angle_m"), &
                Procedure_("fromStringBasicC"))
    end subroutine fromStringBasicC

    pure subroutine fromStringBasicS(string, errors, angle)
        type(VARYING_STRING), intent(in) :: string
        type(ErrorList_t), intent(out) :: errors
        type(Angle_t), intent(out) :: angle

        type(ErrorList_t) :: errors_

        call fromString( &
                string, PROVIDED_UNITS, errors_, angle)
        call errors%appendErrors( &
                errors_, &
                Module_("Angle_m"), &
                Procedure_("fromStringBasicS"))
    end subroutine fromStringBasicS

    pure subroutine fromStringWithUnitsC(string, units, errors, angle)
        character(len=*), intent(in) :: string
        type(AngleUnit_t), intent(in) :: units(:)
        type(ErrorList_t), intent(out) :: errors
        type(Angle_t), intent(out) :: angle

        type(ErrorList_t) :: errors_

        call fromString( &
                var_str(string), units, errors_, angle)
        call errors%appendErrors( &
                errors_, &
                Module_("Angle_m"), &
                Procedure_("fromStringWithUnitsC"))
    end subroutine fromStringWithUnitsC

    pure subroutine fromStringWithUnitsS(string, units, errors, angle)
        type(VARYING_STRING), intent(in) :: string
        type(AngleUnit_t), intent(in) :: units(:)
        type(ErrorList_t), intent(out) :: errors
        type(Angle_t), intent(out) :: angle

        double precision :: number
        character(len=100) :: number_chars
        type(VARYING_STRING) :: number_string
        integer :: status
        type(VARYING_STRING) :: symbol
        type(AngleUnit_t) :: unit
        type(ErrorList_t) :: unit_errors

        angle%radians = 0.0d0
        symbol = string
        call split(symbol, number_string, " ")
        if (len(symbol) == 0) then
            call errors%appendError(Fatal( &
                    PARSE_ERROR, &
                    Module_("Angle_m"), &
                    Procedure_("fromStringWithUnitsS"), &
                    'No unit symbol found in string "' // string // '"'))
            return
        end if
        number_chars = number_string
        read(number_chars, *, iostat=status) number
        if (status /= 0) then
            call errors%appendError(Fatal( &
                    PARSE_ERROR, &
                    Module_("Angle_m"), &
                    Procedure_("fromStringWithUnitsS"), &
                    'Error parsing number from string "' // number_string // '"'))
        end if
        call fromString(symbol, units, unit_errors, unit)
        angle = number.unit.unit
        call errors%appendErrors( &
                unit_errors, &
                Module_("Angle_m"), &
                Procedure_("fromStringWithUnitsS"))
    end subroutine fromStringWithUnitsS

    elemental function fromUnits(value_, units) result(angle)
        double precision, intent(in) :: value_
        type(AngleUnit_t), intent(in) :: units
        type(Angle_t) :: angle

        angle%radians = value_ / units%conversion_factor
    end function fromUnits

    elemental function toUnits(self, units) result(angle)
        class(Angle_t), intent(in) :: self
        class(AngleUnit_t), intent(in) :: units
        double precision :: angle

        angle = self%radians * units%conversion_factor
    end function toUnits

    elemental function doubleTimesAngle( &
            multiplier, angle) result(new_angle)
        double precision, intent(in) :: multiplier
        class(Angle_t), intent(in) :: angle
        type(Angle_t) :: new_angle

        new_angle%radians = &
                multiplier * angle%radians
    end function doubleTimesAngle

    elemental function integerTimesAngle( &
            multiplier, angle) result(new_angle)
        integer, intent(in) :: multiplier
        class(Angle_t), intent(in) :: angle
        type(Angle_t) :: new_angle

        new_angle%radians = &
                dble(multiplier) * angle%radians
    end function integerTimesAngle

    elemental function angleTimesDouble( &
            angle, multiplier) result(new_angle)
        class(Angle_t), intent(in) :: angle
        double precision, intent(in) :: multiplier
        type(Angle_t) :: new_angle

        new_angle%radians = &
                angle%radians * multiplier
    end function angleTimesDouble

    elemental function angleTimesInteger( &
            angle, multiplier) result(new_angle)
        class(Angle_t), intent(in) :: angle
        integer, intent(in) :: multiplier
        type(Angle_t) :: new_angle

        new_angle%radians = &
                angle%radians * dble(multiplier)
    end function angleTimesInteger

    elemental function angleDividedByDouble( &
            angle, divisor) result(new_angle)
        class(Angle_t), intent(in) :: angle
        double precision, intent(in) :: divisor
        type(Angle_t) :: new_angle

        new_angle%radians = &
                angle%radians / divisor
    end function angleDividedByDouble

    elemental function angleDividedByInteger( &
            angle, divisor) result(new_angle)
        class(Angle_t), intent(in) :: angle
        integer, intent(in) :: divisor
        type(Angle_t) :: new_angle

        new_angle%radians = &
                angle%radians / dble(divisor)
    end function angleDividedByInteger

    elemental function angleDividedByAngle( &
            numerator, denomenator) result(ratio)
        class(Angle_t), intent(in) :: numerator
        class(Angle_t), intent(in) :: denomenator
        double precision :: ratio

        ratio = numerator%radians / denomenator%radians
    end function angleDividedByAngle

    elemental function anglePlusAngle( &
            angle1, angle2) result(new_angle)
        class(Angle_t), intent(in) :: angle1
        class(Angle_t), intent(in) :: angle2
        type(Angle_t) :: new_angle

        new_angle%radians = &
                angle1%radians + angle2%radians
    end function anglePlusAngle

    elemental function angleMinusAngle( &
            angle1, angle2) result(new_angle)
        class(Angle_t), intent(in) :: angle1
        class(Angle_t), intent(in) :: angle2
        type(Angle_t) :: new_angle

        new_angle%radians = &
                angle1%radians - angle2%radians
    end function angleMinusAngle

    elemental function greaterThan(lhs, rhs)
        class(Angle_t), intent(in) :: lhs
        class(Angle_t), intent(in) :: rhs
        logical :: greaterThan

        greaterThan = lhs%radians > rhs%radians
    end function greaterThan

    elemental function lessThan(lhs,rhs)
        class(Angle_t), intent(in) :: lhs
        class(Angle_t), intent(in) :: rhs
        logical :: lessThan

        lessThan = lhs%radians < rhs%radians
    end function lessThan

    elemental function greaterThanOrEqual(lhs, rhs)
        class(Angle_t), intent(in) :: lhs
        class(Angle_t), intent(in) :: rhs
        logical :: greaterThanOrEqual

        greaterThanOrEqual = lhs%radians >= rhs%radians
    end function greaterThanOrEqual

    elemental function lessThanOrEqual(lhs, rhs)
        class(Angle_t), intent(in) :: lhs
        class(Angle_t), intent(in) :: rhs
        logical :: lessThanOrEqual

        lessThanOrEqual = lhs%radians <= rhs%radians
    end function lessThanOrEqual

    elemental function equal_(lhs,rhs)
        class(Angle_t), intent(in) :: lhs
        class(Angle_t), intent(in) :: rhs
        logical :: equal_

        equal_ = lhs%radians .safeEq. rhs%radians
    end function equal_

    elemental function equalWithinAbsolute(lhs, rhs, within)
        class(Angle_t), intent(in) :: lhs
        class(Angle_t), intent(in) :: rhs
        class(Angle_t), intent(in) :: within
        logical :: equalWithinAbsolute

        equalWithinAbsolute = equalWithinAbsolute_( &
                lhs%radians, rhs%radians, within%radians)
    end function equalWithinAbsolute

    elemental function equalWithinRelative(lhs, rhs, within)
        class(Angle_t), intent(in) :: lhs
        class(Angle_t), intent(in) :: rhs
        double precision, intent(in) :: within
        logical :: equalWithinRelative

        equalWithinRelative = equalWithinRelative_( &
                lhs%radians, rhs%radians, within)
    end function equalWithinRelative

    elemental function notEqual(lhs, rhs)
        class(Angle_t), intent(in) :: lhs
        class(Angle_t), intent(in) :: rhs
        logical :: notEqual

        notEqual = .not. lhs == rhs
    end function notEqual

    elemental function toStringFullPrecision(self) result(string)
        class(Angle_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = self%toStringIn(DEFAULT_OUTPUT_UNITS)
    end function toStringFullPrecision

    elemental function toStringWithPrecision(self, significant_digits) result(string)
        class(Angle_t), intent(in) :: self
        integer, intent(in) :: significant_digits
        type(VARYING_STRING) :: string

        string = self%toStringIn(DEFAULT_OUTPUT_UNITS, significant_digits)
    end function toStringWithPrecision

    elemental function toStringInFullPrecision(self, units) result(string)
        class(Angle_t), intent(in) :: self
        class(AngleUnit_t), intent(in) :: units
        type(VARYING_STRING) :: string

        string = toString(self.in.units) // " " // units%toString()
    end function toStringInFullPrecision

    elemental function toStringInWithPrecision( &
            self, units, significant_digits) result(string)
        class(Angle_t), intent(in) :: self
        class(AngleUnit_t), intent(in) :: units
        integer, intent(in) :: significant_digits
        type(VARYING_STRING) :: string

        string = &
                toString(self.in.units, significant_digits) &
                // " " // units%toString()
    end function toStringInWithPrecision

    elemental function toGnuplotStringFullPrecision(self) result(string)
        class(Angle_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = self%toGnuplotStringIn(DEFAULT_OUTPUT_UNITS)
    end function toGnuplotStringFullPrecision

    elemental function toGnuplotStringWithPrecision( &
            self, significant_digits) result(string)
        class(Angle_t), intent(in) :: self
        integer, intent(in) :: significant_digits
        type(VARYING_STRING) :: string

        string = self%toGnuplotStringIn( &
                DEFAULT_OUTPUT_UNITS, significant_digits)
    end function toGnuplotStringWithPrecision

    elemental function toGnuplotStringInFullPrecision(self, units) result(string)
        class(Angle_t), intent(in) :: self
        class(AngleUnit_t), intent(in) :: units
        type(VARYING_STRING) :: string

        string = toString(self.in.units) // " " // units%toGnuplotString()
    end function toGnuplotStringInFullPrecision

    elemental function toGnuplotStringInWithPrecision( &
            self, units, significant_digits) result(string)
        class(Angle_t), intent(in) :: self
        class(AngleUnit_t), intent(in) :: units
        integer, intent(in) :: significant_digits
        type(VARYING_STRING) :: string

        string = &
                toString(self.in.units, significant_digits) &
                // " " // units%toGnuplotString()
    end function toGnuplotStringInWithPrecision

    elemental function toLatexStringFullPrecision(self) result(string)
        class(Angle_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = self%toLatexStringIn(DEFAULT_OUTPUT_UNITS)
    end function toLatexStringFullPrecision

    elemental function toLatexStringWithPrecision(self, significant_digits) result(string)
        class(Angle_t), intent(in) :: self
        integer, intent(in) :: significant_digits
        type(VARYING_STRING) :: string

        string = self%toLatexStringIn(DEFAULT_OUTPUT_UNITS, significant_digits)
    end function toLatexStringWithPrecision

    elemental function toLatexStringInFullPrecision(self, units) result(string)
        class(Angle_t), intent(in) :: self
        class(AngleUnit_t), intent(in) :: units
        type(VARYING_STRING) :: string

        string = wrapInLatexQuantity( &
                toString(self.in.units), units%toLatexString())
    end function toLatexStringInFullPrecision

    elemental function toLatexStringInWithPrecision( &
            self, units, significant_digits) result(string)
        class(Angle_t), intent(in) :: self
        class(AngleUnit_t), intent(in) :: units
        integer, intent(in) :: significant_digits
        type(VARYING_STRING) :: string

        string = wrapInLatexQuantity( &
                toString(self.in.units, significant_digits), &
                units%toLatexString())
    end function toLatexStringInWithPrecision

    pure subroutine unitFromStringBasicC(string, errors, unit)
        character(len=*), intent(in) :: string
        type(ErrorList_t), intent(out) :: errors
        type(AngleUnit_t), intent(out) :: unit

        type(ErrorList_t) :: errors_

        call fromString( &
                var_str(string), PROVIDED_UNITS, errors_, unit)
        call errors%appendErrors( &
                errors_, &
                Module_("Angle_m"), &
                Procedure_("unitFromStringBasicC"))
    end subroutine unitFromStringBasicC

    pure subroutine unitFromStringBasicS(string, errors, unit)
        type(VARYING_STRING), intent(in) :: string
        type(ErrorList_t), intent(out) :: errors
        type(AngleUnit_t), intent(out) :: unit

        type(ErrorList_t) :: errors_

        call fromString( &
                string, PROVIDED_UNITS, errors_, unit)
        call errors%appendErrors( &
                errors_, &
                Module_("Angle_m"), &
                Procedure_("unitFromStringBasicS"))
    end subroutine unitFromStringBasicS

    pure subroutine unitFromStringWithUnitsC(string, units, errors, unit)
        character(len=*), intent(in) :: string
        type(AngleUnit_t), intent(in) :: units(:)
        type(ErrorList_t), intent(out) :: errors
        type(AngleUnit_t), intent(out) :: unit

        type(ErrorList_t) :: errors_

        call fromString(var_str(string), units, errors_, unit)
        call errors%appendErrors( &
                errors_, &
                Module_("Angle_m"), &
                Procedure_("unitFromStringWithUnitsC"))
    end subroutine unitFromStringWithUnitsC

    pure subroutine unitFromStringWithUnitsS(string, units, errors, unit)
        type(VARYING_STRING), intent(in) :: string
        type(AngleUnit_t), intent(in) :: units(:)
        type(ErrorList_t), intent(out) :: errors
        type(AngleUnit_t), intent(out) :: unit

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
                    Module_("Angle_m"), &
                    Procedure_("unitFromStringWithUnitsS"), &
                    '"' // string // '", known units: [' // join(unit_strings, ', ') // ']' ))
        end if
    end subroutine unitFromStringWithUnitsS

    elemental function unitToString(self) result(string)
        class(AngleUnit_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = trim(self%symbol)
    end function unitToString

    elemental function unitToGnuplotString(self) result(string)
        class(AngleUnit_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = trim(self%gnuplot_symbol)
    end function unitToGnuplotString

    elemental function unitToLatexString(self) result(string)
        class(AngleUnit_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = trim(self%latex_symbol)
    end function unitToLatexString

    elemental function sin_(angle)
        type(Angle_t), intent(in) :: angle
        double precision :: sin_

        sin_ = sin(angle%radians)
    end function sin_

    elemental function cos_(angle)
        type(Angle_t), intent(in) :: angle
        double precision :: cos_

        cos_ = cos(angle%radians)
    end function cos_

    elemental function tan_(angle)
        type(Angle_t), intent(in) :: angle
        double precision :: tan_

        tan_ = tan(angle%radians)
    end function tan_

    elemental function asin_(number) result(angle)
        double precision, intent(in) :: number
        type(Angle_t) :: angle

        angle%radians = asin(number)
    end function asin_

    elemental function acos_(number) result(angle)
        double precision, intent(in) :: number
        type(Angle_t) :: angle

        angle%radians = acos(number)
    end function acos_

    elemental function atan_(number) result(angle)
        double precision, intent(in) :: number
        type(Angle_t) :: angle

        angle%radians = atan(number)
    end function atan_

    elemental function atan2_(y, x) result(angle)
        double precision, intent(in) :: y
        double precision, intent(in) :: x
        type(Angle_t) :: angle

        angle%radians = atan2(y, x)
    end function atan2_
end module Angle_m
