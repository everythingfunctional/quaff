module Angle_m
    use Conversion_factors_m, only: DEGREES_PER_RADIAN

    implicit none
    private

    type, public :: Angle_t
        private
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
        procedure, public :: toString
        procedure, public :: toStringIn
    end type Angle_t

    type, public :: AngleUnit_t
        double precision :: conversion_factor
        character(len=10) :: symbol
    contains
        procedure :: toString => unitToString
    end type AngleUnit_t

    interface operator(.unit.)
        module procedure fromUnits
    end interface operator(.unit.)

    interface angleFromString
        module procedure fromStringBasicC
        module procedure fromStringBasicS
        module procedure fromStringWithUnitsC
        module procedure fromStringWithUnitsS
    end interface angleFromString

    interface angleUnitFromString
        module procedure unitFromStringBasicC
        module procedure unitFromStringBasicS
        module procedure unitFromStringWithUnitsC
        module procedure unitFromStringWithUnitsS
    end interface angleUnitFromString

    type(AngleUnit_t), parameter, public :: DEGREES = &
            AngleUnit_t( &
                    conversion_factor = DEGREES_PER_RADIAN, &
                    symbol = "deg")
    type(AngleUnit_t), parameter, public :: RADIANS = &
            AngleUnit_t( &
                    conversion_factor = 1.0d0, &
                    symbol = "rad")

    type(AngleUnit_t), public :: DEFAULT_OUTPUT_UNITS = RADIANS

    type(AngleUnit_t), parameter, public :: PROVIDED_UNITS(*) = &
            [DEGREES, RADIANS]

    public :: &
            operator(.unit.), &
            angleFromString, &
            angleUnitFromString
contains
    function fromStringBasicC(string, errors) result(angle)
        use Error_list_m, only: ErrorList_t
        use iso_varying_string, only: var_str
        use Module_m, only: Module_
        use Procedure_m, only: Procedure_

        character(len=*), intent(in) :: string
        type(ErrorList_t), intent(out) :: errors
        type(Angle_t) :: angle

        type(ErrorList_t) :: errors_

        angle = angleFromString( &
                var_str(string), PROVIDED_UNITS, errors_)
        call errors%appendErrors( &
                errors_, &
                Module_("Angle_m"), &
                Procedure_("fromStringBasicC"))
    end function fromStringBasicC

    function fromStringBasicS(string, errors) result(angle)
        use Error_list_m, only: ErrorList_t
        use iso_varying_string, only: VARYING_STRING
        use Module_m, only: Module_
        use Procedure_m, only: Procedure_

        type(VARYING_STRING), intent(in) :: string
        type(ErrorList_t), intent(out) :: errors
        type(Angle_t) :: angle

        type(ErrorList_t) :: errors_

        angle = angleFromString( &
                string, PROVIDED_UNITS, errors_)
        call errors%appendErrors( &
                errors_, &
                Module_("Angle_m"), &
                Procedure_("fromStringBasicS"))
    end function fromStringBasicS

    function fromStringWithUnitsC(string, units, errors) result(angle)
        use Error_list_m, only: ErrorList_t
        use iso_varying_string, only: var_str
        use Module_m, only: Module_
        use Procedure_m, only: Procedure_

        character(len=*), intent(in) :: string
        type(AngleUnit_t), intent(in) :: units(:)
        type(ErrorList_t), intent(out) :: errors
        type(Angle_t) :: angle

        type(ErrorList_t) :: errors_

        angle = angleFromString( &
                var_str(string), units, errors_)
        call errors%appendErrors( &
                errors_, &
                Module_("Angle_m"), &
                Procedure_("fromStringWithUnitsC"))
    end function fromStringWithUnitsC

    function fromStringWithUnitsS(string, units, errors) result(angle)
        use Error_list_m, only: ErrorList_t
        use iso_varying_string, only: &
                VARYING_STRING, &
                assignment(=), &
                operator(//), &
                operator(==), &
                len, &
                split
        use Message_m, only: Fatal
        use Miscellaneous_m, only: PARSE_ERROR
        use Module_m, only: Module_
        use Procedure_m, only: Procedure_
        use strff, only: join

        type(VARYING_STRING), intent(in) :: string
        type(AngleUnit_t), intent(in) :: units(:)
        type(ErrorList_t), intent(out) :: errors
        type(Angle_t) :: angle

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
        unit = angleUnitFromString(symbol, units, unit_errors)
        angle = number.unit.unit
        call errors%appendErrors( &
                unit_errors, &
                Module_("Angle_m"), &
                Procedure_("fromStringWithUnitsS"))
    end function fromStringWithUnitsS

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
        use Miscellaneous_m, only: operator(.safeEq.)

        class(Angle_t), intent(in) :: lhs
        class(Angle_t), intent(in) :: rhs
        logical :: equal_

        equal_ = lhs%radians .safeEq. rhs%radians
    end function equal_

    function equalWithinAbsolute(lhs, rhs, within)
        use Miscellaneous_m, only: equalWithinAbsolute_ => equalWithinAbsolute

        class(Angle_t), intent(in) :: lhs
        class(Angle_t), intent(in) :: rhs
        class(Angle_t), intent(in) :: within
        logical :: equalWithinAbsolute

        equalWithinAbsolute = equalWithinAbsolute_( &
                lhs%radians, rhs%radians, within%radians)
    end function equalWithinAbsolute

    function equalWithinRelative(lhs, rhs, within)
        use Miscellaneous_m, only: equalWithinRelative_ => equalWithinRelative

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

    function toString(self) result(string)
        use iso_varying_string, only: VARYING_STRING

        class(Angle_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = self%toStringIn(DEFAULT_OUTPUT_UNITS)
    end function toString

    function toStringIn(self, units) result(string)
        use iso_varying_string, only: VARYING_STRING, operator(//)
        use strff, only: toString

        class(Angle_t), intent(in) :: self
        class(AngleUnit_t), intent(in) :: units
        type(VARYING_STRING) :: string

        string = toString(self.in.units) // " " // units%toString()
    end function toStringIn

    function unitFromStringBasicC(string, errors) result(unit)
        use Error_list_m, only: ErrorList_t
        use iso_varying_string, only: var_str
        use Module_m, only: Module_
        use Procedure_m, only: Procedure_

        character(len=*), intent(in) :: string
        type(ErrorList_t), intent(out) :: errors
        type(AngleUnit_t) :: unit

        type(ErrorList_t) :: errors_

        unit = angleUnitFromString( &
                var_str(string), PROVIDED_UNITS, errors_)
        call errors%appendErrors( &
                errors_, &
                Module_("Angle_m"), &
                Procedure_("unitFromStringBasicC"))
    end function unitFromStringBasicC

    function unitFromStringBasicS(string, errors) result(unit)
        use Error_list_m, only: ErrorList_t
        use iso_varying_string, only: VARYING_STRING
        use Module_m, only: Module_
        use Procedure_m, only: Procedure_

        type(VARYING_STRING), intent(in) :: string
        type(ErrorList_t), intent(out) :: errors
        type(AngleUnit_t) :: unit

        type(ErrorList_t) :: errors_

        unit = angleUnitFromString( &
                string, PROVIDED_UNITS, errors_)
        call errors%appendErrors( &
                errors_, &
                Module_("Angle_m"), &
                Procedure_("unitFromStringBasicS"))
    end function unitFromStringBasicS

    function unitFromStringWithUnitsC(string, units, errors) result(unit)
        use Error_list_m, only: ErrorList_t
        use iso_varying_string, only: var_str
        use Module_m, only: Module_
        use Procedure_m, only: Procedure_

        character(len=*), intent(in) :: string
        type(AngleUnit_t), intent(in) :: units(:)
        type(ErrorList_t), intent(out) :: errors
        type(AngleUnit_t) :: unit

        type(ErrorList_t) :: errors_

        unit = angleUnitFromString(var_str(string), units, errors_)
        call errors%appendErrors( &
                errors_, &
                Module_("Angle_m"), &
                Procedure_("unitFromStringWithUnitsC"))
    end function unitFromStringWithUnitsC

    function unitFromStringWithUnitsS(string, units, errors) result(unit)
        use Error_list_m, only: ErrorList_t
        use iso_varying_string, only: VARYING_STRING, operator(==), operator(//)
        use Message_m, only: Fatal
        use Miscellaneous_m, only: UNKNOWN_UNIT
        use Module_m, only: Module_
        use Procedure_m, only: Procedure_
        use strff, only: join

        type(VARYING_STRING), intent(in) :: string
        type(AngleUnit_t), intent(in) :: units(:)
        type(ErrorList_t), intent(out) :: errors
        type(AngleUnit_t) :: unit

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
    end function unitFromStringWithUnitsS

    function unitToString(self) result(string)
        use iso_varying_string, only: VARYING_STRING, assignment(=)

        class(AngleUnit_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = trim(self%symbol)
    end function unitToString
end module Angle_m
