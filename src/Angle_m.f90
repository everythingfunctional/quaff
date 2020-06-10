module Angle_m
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
    use quaff_Conversion_factors_m, only: DEGREES_PER_RADIAN
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
    end type Angle_t

    type, abstract, public :: AngleUnit_t
        double precision :: conversion_factor
    contains
        procedure(justUnitToString), deferred :: unitToString
        procedure(unitWithValueToString), deferred :: valueToString
        generic :: toString => unitToString, valueToString
        procedure(parseAsI), deferred :: parseAs
    end type AngleUnit_t

    type, extends(AngleUnit_t), public :: AngleSimpleUnit_t
        character(len=20) :: symbol
    contains
        procedure :: unitToString => simpleUnitToString
        procedure :: valueToString => simpleValueToString
        procedure :: parseAs => simpleParseAs
    end type AngleSimpleUnit_t

    type, extends(AngleUnit_t), public :: AngleGnuplotUnit_t
        character(len=50) :: symbol
    contains
        procedure :: unitToString => gnuplotUnitToString
        procedure :: valueToString => gnuplotValueToString
        procedure :: parseAs => gnuplotParseAs
    end type AngleGnuplotUnit_t

    type, extends(AngleUnit_t), public :: AngleLatexUnit_t
        character(len=100) :: symbol
    contains
        procedure :: unitToString => latexUnitToString
        procedure :: valueToString => latexValueToString
        procedure :: parseAs => latexParseAs
    end type AngleLatexUnit_t

    abstract interface
        elemental function justUnitToString(self) result(string)
            import AngleUnit_t, VARYING_STRING
            class(AngleUnit_t), intent(in) :: self
            type(VARYING_STRING) :: string
        end function justUnitToString

        pure function unitWithValueToString(self, value_) result(string)
            import AngleUnit_t, VARYING_STRING
            class(AngleUnit_t), intent(in) :: self
            type(VARYING_STRING), intent(in) :: value_
            type(VARYING_STRING) :: string
        end function unitWithValueToString

        pure subroutine parseAsI(self, string, errors, angle)
            import ErrorList_t, Angle_t, AngleUnit_t, VARYING_STRING
            class(AngleUnit_t), intent(in) :: self
            type(VARYING_STRING), intent(in) :: string
            type(ErrorList_t), intent(out) :: errors
            type(Angle_t), intent(out) :: angle
        end subroutine parseAsI
    end interface

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
        module procedure simpleUnitFromStringC
        module procedure simpleUnitFromStringS
        module procedure simpleUnitFromStringWithUnitsC
        module procedure simpleUnitFromStringWithUnitsS
    end interface fromString

    interface sum
        module procedure sumAngle
    end interface sum

    type(AngleSimpleUnit_t), parameter, public :: DEGREES = &
            AngleSimpleUnit_t( &
                    conversion_factor = DEGREES_PER_RADIAN, &
                    symbol = "deg")
    type(AngleGnuplotUnit_t), parameter, public :: DEGREES_GNUPLOT = &
            AngleGnuplotUnit_t( &
                    conversion_factor = DEGREES_PER_RADIAN, &
                    symbol = "{/Symbol \260}")
    type(AngleLatexUnit_t), parameter, public :: DEGREES_LATEX = &
            AngleLatexUnit_t( &
                    conversion_factor = DEGREES_PER_RADIAN, &
                    symbol = "\degree")
    type(AngleSimpleUnit_t), parameter, public :: RADIANS = &
            AngleSimpleUnit_t( &
                    conversion_factor = 1.0d0, &
                    symbol = "rad")
    type(AngleGnuplotUnit_t), parameter, public :: RADIANS_GNUPLOT = &
            AngleGnuplotUnit_t( &
                    conversion_factor = 1.0d0, &
                    symbol = "rad")
    type(AngleLatexUnit_t), parameter, public :: RADIANS_LATEX = &
            AngleLatexUnit_t( &
                    conversion_factor = 1.0d0, &
                    symbol = "\radian")

    type(AngleSimpleUnit_t), public :: DEFAULT_OUTPUT_UNITS = RADIANS

    type(AngleSimpleUnit_t), parameter, public :: PROVIDED_UNITS(*) = &
            [DEGREES, RADIANS]
    type(AngleGnuplotUnit_t), parameter, public :: PROVIDED_GNUPLOT_UNITS(*) = &
            [DEGREES_GNUPLOT, RADIANS_GNUPLOT]
    type(AngleLatexUnit_t), parameter, public :: PROVIDED_LATEX_UNITS(*) = &
            [DEGREES_LATEX, RADIANS_LATEX]

    public :: &
            operator(.unit.), &
            fromString, &
            selectUnit, &
            sum, &
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
        class(AngleUnit_t), intent(in) :: units(:)
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
        class(AngleUnit_t), intent(in) :: units(:)
        type(ErrorList_t), intent(out) :: errors
        type(Angle_t), intent(out) :: angle

        type(ErrorList_t) :: all_errors(size(units))
        integer :: i

        do i = 1, size(units)
            call units(i)%parseAs(string, all_errors(i), angle)
            if (.not. all_errors(i)%hasAny()) return
        end do
        do i = 1, size(units)
            call errors%appendErrors( &
                    all_errors(i), &
                    Module_("Angle_m"), &
                    Procedure_("fromStringWithUnitsS"))
        end do
    end subroutine fromStringWithUnitsS

    elemental function fromUnits(value_, units) result(angle)
        double precision, intent(in) :: value_
        class(AngleUnit_t), intent(in) :: units
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

    pure function sumAngle(angles)
        type(Angle_t), intent(in) :: angles(:)
        type(Angle_t) :: sumAngle

        sumAngle%radians = sum(angles%radians)
    end function sumAngle

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

        string = units%toString(toString(self.in.units))
    end function toStringInFullPrecision

    elemental function toStringInWithPrecision( &
            self, units, significant_digits) result(string)
        class(Angle_t), intent(in) :: self
        class(AngleUnit_t), intent(in) :: units
        integer, intent(in) :: significant_digits
        type(VARYING_STRING) :: string

        string = units%toString(toString(self.in.units, significant_digits))
    end function toStringInWithPrecision

    elemental function simpleUnitToString(self) result(string)
        class(AngleSimpleUnit_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = trim(self%symbol)
    end function simpleUnitToString

    pure function simpleValueToString(self, value_) result(string)
        class(AngleSimpleUnit_t), intent(in) :: self
        type(VARYING_STRING), intent(in) :: value_
        type(VARYING_STRING) :: string

        string = value_ // " " // self%toString()
    end function simpleValueToString

    pure subroutine simpleParseAs(self, string, errors, angle)
        class(AngleSimpleUnit_t), intent(in) :: self
        type(VARYING_STRING), intent(in) :: string
        type(ErrorList_t), intent(out) :: errors
        type(Angle_t), intent(out) :: angle

        type(ParseResult_t) :: parse_result

        parse_result = parseWith(theParser, string)
        if (parse_result%ok) then
            select type (the_number => parse_result%parsed)
            type is (ParsedRational_t)
                angle = the_number%value_.unit.self
            end select
        else
            call errors%appendError(Fatal( &
                    PARSE_ERROR, &
                    Module_("Angle_m"), &
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

    pure subroutine gnuplotParseAs(self, string, errors, angle)
        class(AngleGnuplotUnit_t), intent(in) :: self
        type(VARYING_STRING), intent(in) :: string
        type(ErrorList_t), intent(out) :: errors
        type(Angle_t), intent(out) :: angle

        type(ParseResult_t) :: parse_result

        parse_result = parseWith(theParser, string)
        if (parse_result%ok) then
            select type (the_number => parse_result%parsed)
            type is (ParsedRational_t)
                angle = the_number%value_.unit.self
            end select
        else
            call errors%appendError(Fatal( &
                    PARSE_ERROR, &
                    Module_("Angle_m"), &
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

    pure subroutine latexParseAs(self, string, errors, angle)
        class(AngleLatexUnit_t), intent(in) :: self
        type(VARYING_STRING), intent(in) :: string
        type(ErrorList_t), intent(out) :: errors
        type(Angle_t), intent(out) :: angle

        type(ParseResult_t) :: parse_result

        parse_result = parseWith(theParser, string)
        if (parse_result%ok) then
            select type (the_number => parse_result%parsed)
            type is (ParsedRational_t)
                angle = the_number%value_.unit.self
            end select
        else
            call errors%appendError(Fatal( &
                    PARSE_ERROR, &
                    Module_("Angle_m"), &
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
        class(AngleGnuplotUnit_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = trim(self%symbol)
    end function gnuplotUnitToString

    pure function gnuplotValueToString(self, value_) result(string)
        class(AngleGnuplotUnit_t), intent(in) :: self
        type(VARYING_STRING), intent(in) :: value_
        type(VARYING_STRING) :: string

        string = value_ // " " // self%toString()
    end function gnuplotValueToString

    elemental function latexUnitToString(self) result(string)
        class(AngleLatexUnit_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = wrapInLatexUnit(trim(self%symbol))
    end function latexUnitToString

    pure function latexValueToString(self, value_) result(string)
        class(AngleLatexUnit_t), intent(in) :: self
        type(VARYING_STRING), intent(in) :: value_
        type(VARYING_STRING) :: string

        string = wrapInLatexQuantity(value_, trim(self%symbol))
    end function latexValueToString

    pure subroutine simpleUnitFromStringC(string, errors, unit)
        character(len=*), intent(in) :: string
        type(ErrorList_t), intent(out) :: errors
        type(AngleSimpleUnit_t), intent(out) :: unit

        type(ErrorList_t) :: errors_

        call fromString(var_str(string), PROVIDED_UNITS, errors_, unit)
        call errors%appendErrors( &
                errors_, &
                Module_("Angle_m"), &
                Procedure_("simpleUnitFromStringC"))
    end subroutine simpleUnitFromStringC

    pure subroutine simpleUnitFromStringS(string, errors, unit)
        type(VARYING_STRING), intent(in) :: string
        type(ErrorList_t), intent(out) :: errors
        type(AngleSimpleUnit_t), intent(out) :: unit

        type(ErrorList_t) :: errors_

        call fromString(string, PROVIDED_UNITS, errors_, unit)
        call errors%appendErrors( &
                errors_, &
                Module_("Angle_m"), &
                Procedure_("simpleUnitFromStringS"))
    end subroutine simpleUnitFromStringS

    pure subroutine simpleUnitFromStringWithUnitsC(string, units, errors, unit)
        character(len=*), intent(in) :: string
        type(AngleSimpleUnit_t), intent(in) :: units(:)
        type(ErrorList_t), intent(out) :: errors
        type(AngleSimpleUnit_t), intent(out) :: unit

        type(ErrorList_t) :: errors_

        call fromString(var_str(string), units, errors_, unit)
        call errors%appendErrors( &
                errors_, &
                Module_("Angle_m"), &
                Procedure_("simpleUnitFromStringWithUnitsC"))
    end subroutine simpleUnitFromStringWithUnitsC

    pure subroutine simpleUnitFromStringWithUnitsS(string, units, errors, unit)
        type(VARYING_STRING), intent(in) :: string
        type(AngleSimpleUnit_t), intent(in) :: units(:)
        type(ErrorList_t), intent(out) :: errors
        type(AngleSimpleUnit_t), intent(out) :: unit

        type(ErrorList_t) :: errors_
        integer :: which_unit

        call selectUnit(string, units, errors, which_unit)
        if (errors_%hasAny()) then
            call errors%appendErrors( &
                    errors_, &
                    Module_("Angle_m"), &
                    Procedure_("simpleUnitFromStringWithUnitsS"))
        else
            unit = units(which_unit)
        end if
    end subroutine simpleUnitFromStringWithUnitsS

    pure subroutine selectUnit(string, units, errors, index)
        type(VARYING_STRING), intent(in) :: string
        class(AngleUnit_t), intent(in) :: units(:)
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
                Module_("Angle_m"), &
                Procedure_("selectUnit"), &
                '"' // string // '", known units: [' // join(unit_strings, ', ') // ']'))
    end subroutine selectUnit

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
