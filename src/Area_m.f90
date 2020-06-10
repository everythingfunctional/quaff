module Area_m
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
            SQUARE_CENTIMETERS_PER_SQUARE_METER, &
            SQUARE_FEET_PER_SQUARE_METER, &
            SQUARE_INCHES_PER_SQUARE_METER
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

    type, public :: Area_t
        double precision :: square_meters
    contains
        private
        procedure :: toUnits
        generic, public :: operator(.in.) => toUnits
        procedure, pass(area) :: doubleTimesArea
        procedure, pass(area) :: integerTimesArea
        procedure, pass(area) :: areaTimesDouble
        procedure, pass(area) :: areaTimesInteger
        generic, public :: operator(*) => &
                doubleTimesArea, &
                integerTimesArea, &
                areaTimesDouble, &
                areaTimesInteger
        procedure :: areaDividedByDouble
        procedure :: areaDividedByInteger
        procedure, pass(numerator) :: areaDividedByArea
        generic, public :: operator(/) => &
                areaDividedByDouble, &
                areaDividedByInteger, &
                areaDividedByArea
        procedure :: areaPlusArea
        generic, public :: operator(+) => areaPlusArea
        procedure :: areaMinusArea
        generic, public :: operator(-) => areaMinusArea
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
    end type Area_t

    type, abstract, public :: AreaUnit_t
        double precision :: conversion_factor
    contains
        procedure(justUnitToString), deferred :: unitToString
        procedure(unitWithValueToString), deferred :: valueToString
        generic :: toString => unitToString, valueToString
        procedure(parseAsI), deferred :: parseAs
    end type AreaUnit_t

    type, extends(AreaUnit_t), public :: AreaSimpleUnit_t
        character(len=20) :: symbol
    contains
        procedure :: unitToString => simpleUnitToString
        procedure :: valueToString => simpleValueToString
        procedure :: parseAs => simpleParseAs
    end type AreaSimpleUnit_t

    type, extends(AreaUnit_t), public :: AreaGnuplotUnit_t
        character(len=50) :: symbol
    contains
        procedure :: unitToString => gnuplotUnitToString
        procedure :: valueToString => gnuplotValueToString
        procedure :: parseAs => gnuplotParseAs
    end type AreaGnuplotUnit_t

    type, extends(AreaUnit_t), public :: AreaLatexUnit_t
        character(len=100) :: symbol
    contains
        procedure :: unitToString => latexUnitToString
        procedure :: valueToString => latexValueToString
        procedure :: parseAs => latexParseAs
    end type AreaLatexUnit_t

    abstract interface
        elemental function justUnitToString(self) result(string)
            import AreaUnit_t, VARYING_STRING
            class(AreaUnit_t), intent(in) :: self
            type(VARYING_STRING) :: string
        end function justUnitToString

        pure function unitWithValueToString(self, value_) result(string)
            import AreaUnit_t, VARYING_STRING
            class(AreaUnit_t), intent(in) :: self
            type(VARYING_STRING), intent(in) :: value_
            type(VARYING_STRING) :: string
        end function unitWithValueToString

        pure subroutine parseAsI(self, string, errors, area)
            import ErrorList_t, Area_t, AreaUnit_t, VARYING_STRING
            class(AreaUnit_t), intent(in) :: self
            type(VARYING_STRING), intent(in) :: string
            type(ErrorList_t), intent(out) :: errors
            type(Area_t), intent(out) :: area
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
        module procedure sumArea
    end interface sum

    type(AreaSimpleUnit_t), parameter, public :: SQUARE_CENTIMETERS = &
            AreaSimpleUnit_t( &
                    conversion_factor = SQUARE_CENTIMETERS_PER_SQUARE_METER, &
                    symbol = "cm^2")
    type(AreaGnuplotUnit_t), parameter, public :: SQUARE_CENTIMETERS_GNUPLOT = &
            AreaGnuplotUnit_t( &
                    conversion_factor = SQUARE_CENTIMETERS_PER_SQUARE_METER, &
                    symbol = "cm^2")
    type(AreaLatexUnit_t), parameter, public :: SQUARE_CENTIMETERS_LATEX = &
            AreaLatexUnit_t( &
                    conversion_factor = SQUARE_CENTIMETERS_PER_SQUARE_METER, &
                    symbol = "\square\centi\meter")
    type(AreaSimpleUnit_t), parameter, public :: SQUARE_FEET = &
            AreaSimpleUnit_t( &
                    conversion_factor = SQUARE_FEET_PER_SQUARE_METER, &
                    symbol = "ft^2")
    type(AreaGnuplotUnit_t), parameter, public :: SQUARE_FEET_GNUPLOT = &
            AreaGnuplotUnit_t( &
                    conversion_factor = SQUARE_FEET_PER_SQUARE_METER, &
                    symbol = "ft^2")
    type(AreaSimpleUnit_t), parameter, public :: SQUARE_INCHES = &
            AreaSimpleUnit_t( &
                    conversion_factor = SQUARE_INCHES_PER_SQUARE_METER, &
                    symbol = "in^2")
    type(AreaGnuplotUnit_t), parameter, public :: SQUARE_INCHES_GNUPLOT = &
            AreaGnuplotUnit_t( &
                    conversion_factor = SQUARE_INCHES_PER_SQUARE_METER, &
                    symbol = "in^2")
    type(AreaSimpleUnit_t), parameter, public :: SQUARE_METERS = &
            AreaSimpleUnit_t( &
                    conversion_factor = 1.0d0, &
                    symbol = "m^2")
    type(AreaGnuplotUnit_t), parameter, public :: SQUARE_METERS_GNUPLOT = &
            AreaGnuplotUnit_t( &
                    conversion_factor = 1.0d0, &
                    symbol = "m^2")
    type(AreaLatexUnit_t), parameter, public :: SQUARE_METERS_LATEX = &
            AreaLatexUnit_t( &
                    conversion_factor = 1.0d0, &
                    symbol = "\square\meter")

    type(AreaSimpleUnit_t), public :: DEFAULT_OUTPUT_UNITS = SQUARE_METERS

    type(AreaSimpleUnit_t), parameter, public :: PROVIDED_UNITS(*) = &
            [SQUARE_CENTIMETERS, SQUARE_FEET, SQUARE_INCHES, SQUARE_METERS]
    type(AreaGnuplotUnit_t), parameter, public :: PROVIDED_GNUPLOT_UNITS(*) = &
            [SQUARE_CENTIMETERS_GNUPLOT, &
            SQUARE_FEET_GNUPLOT, &
            SQUARE_INCHES_GNUPLOT, &
            SQUARE_METERS_GNUPLOT]
    type(AreaLatexUnit_t), parameter, public :: PROVIDED_LATEX_UNITS(*) = &
            [SQUARE_CENTIMETERS_LATEX, SQUARE_METERS_LATEX]

    public :: operator(.unit.), fromString, selectUnit, sum
contains
    pure subroutine fromStringBasicC(string, errors, area)
        character(len=*), intent(in) :: string
        type(ErrorList_t), intent(out) :: errors
        type(Area_t), intent(out) :: area

        type(ErrorList_t) :: errors_

        call fromString( &
                var_str(string), PROVIDED_UNITS, errors_, area)
        call errors%appendErrors( &
                errors_, &
                Module_("Area_m"), &
                Procedure_("fromStringBasicC"))
    end subroutine fromStringBasicC

    pure subroutine fromStringBasicS(string, errors, area)
        type(VARYING_STRING), intent(in) :: string
        type(ErrorList_t), intent(out) :: errors
        type(Area_t), intent(out) :: area

        type(ErrorList_t) :: errors_

        call fromString( &
                string, PROVIDED_UNITS, errors_, area)
        call errors%appendErrors( &
                errors_, &
                Module_("Area_m"), &
                Procedure_("fromStringBasicS"))
    end subroutine fromStringBasicS

    pure subroutine fromStringWithUnitsC(string, units, errors, area)
        character(len=*), intent(in) :: string
        class(AreaUnit_t), intent(in) :: units(:)
        type(ErrorList_t), intent(out) :: errors
        type(Area_t), intent(out) :: area

        type(ErrorList_t) :: errors_

        call fromString( &
                var_str(string), units, errors_, area)
        call errors%appendErrors( &
                errors_, &
                Module_("Area_m"), &
                Procedure_("fromStringWithUnitsC"))
    end subroutine fromStringWithUnitsC

    pure subroutine fromStringWithUnitsS(string, units, errors, area)
        type(VARYING_STRING), intent(in) :: string
        class(AreaUnit_t), intent(in) :: units(:)
        type(ErrorList_t), intent(out) :: errors
        type(Area_t), intent(out) :: area

        type(ErrorList_t) :: all_errors(size(units))
        integer :: i

        do i = 1, size(units)
            call units(i)%parseAs(string, all_errors(i), area)
            if (.not. all_errors(i)%hasAny()) return
        end do
        do i = 1, size(units)
            call errors%appendErrors( &
                    all_errors(i), &
                    Module_("Area_m"), &
                    Procedure_("fromStringWithUnitsS"))
        end do
    end subroutine fromStringWithUnitsS

    elemental function fromUnits(value_, units) result(area)
        double precision, intent(in) :: value_
        class(AreaUnit_t), intent(in) :: units
        type(Area_t) :: area

        area%square_meters = value_ / units%conversion_factor
    end function fromUnits

    elemental function toUnits(self, units) result(area)
        class(Area_t), intent(in) :: self
        class(AreaUnit_t), intent(in) :: units
        double precision :: area

        area = self%square_meters * units%conversion_factor
    end function toUnits

    elemental function doubleTimesArea( &
            multiplier, area) result(new_area)
        double precision, intent(in) :: multiplier
        class(Area_t), intent(in) :: area
        type(Area_t) :: new_area

        new_area%square_meters = &
                multiplier * area%square_meters
    end function doubleTimesArea

    elemental function integerTimesArea( &
            multiplier, area) result(new_area)
        integer, intent(in) :: multiplier
        class(Area_t), intent(in) :: area
        type(Area_t) :: new_area

        new_area%square_meters = &
                dble(multiplier) * area%square_meters
    end function integerTimesArea

    elemental function areaTimesDouble( &
            area, multiplier) result(new_area)
        class(Area_t), intent(in) :: area
        double precision, intent(in) :: multiplier
        type(Area_t) :: new_area

        new_area%square_meters = &
                area%square_meters * multiplier
    end function areaTimesDouble

    elemental function areaTimesInteger( &
            area, multiplier) result(new_area)
        class(Area_t), intent(in) :: area
        integer, intent(in) :: multiplier
        type(Area_t) :: new_area

        new_area%square_meters = &
                area%square_meters * dble(multiplier)
    end function areaTimesInteger

    elemental function areaDividedByDouble( &
            area, divisor) result(new_area)
        class(Area_t), intent(in) :: area
        double precision, intent(in) :: divisor
        type(Area_t) :: new_area

        new_area%square_meters = &
                area%square_meters / divisor
    end function areaDividedByDouble

    elemental function areaDividedByInteger( &
            area, divisor) result(new_area)
        class(Area_t), intent(in) :: area
        integer, intent(in) :: divisor
        type(Area_t) :: new_area

        new_area%square_meters = &
                area%square_meters / dble(divisor)
    end function areaDividedByInteger

    elemental function areaDividedByArea( &
            numerator, denomenator) result(ratio)
        class(Area_t), intent(in) :: numerator
        class(Area_t), intent(in) :: denomenator
        double precision :: ratio

        ratio = numerator%square_meters / denomenator%square_meters
    end function areaDividedByArea

    elemental function areaPlusArea( &
            area1, area2) result(new_area)
        class(Area_t), intent(in) :: area1
        class(Area_t), intent(in) :: area2
        type(Area_t) :: new_area

        new_area%square_meters = &
                area1%square_meters + area2%square_meters
    end function areaPlusArea

    elemental function areaMinusArea( &
            area1, area2) result(new_area)
        class(Area_t), intent(in) :: area1
        class(Area_t), intent(in) :: area2
        type(Area_t) :: new_area

        new_area%square_meters = &
                area1%square_meters - area2%square_meters
    end function areaMinusArea

    pure function sumArea(areas)
        type(Area_t), intent(in) :: areas(:)
        type(Area_t) :: sumArea

        sumArea%square_meters = sum(areas%square_meters)
    end function sumArea

    elemental function greaterThan(lhs, rhs)
        class(Area_t), intent(in) :: lhs
        class(Area_t), intent(in) :: rhs
        logical :: greaterThan

        greaterThan = lhs%square_meters > rhs%square_meters
    end function greaterThan

    elemental function lessThan(lhs,rhs)
        class(Area_t), intent(in) :: lhs
        class(Area_t), intent(in) :: rhs
        logical :: lessThan

        lessThan = lhs%square_meters < rhs%square_meters
    end function lessThan

    elemental function greaterThanOrEqual(lhs, rhs)
        class(Area_t), intent(in) :: lhs
        class(Area_t), intent(in) :: rhs
        logical :: greaterThanOrEqual

        greaterThanOrEqual = lhs%square_meters >= rhs%square_meters
    end function greaterThanOrEqual

    elemental function lessThanOrEqual(lhs, rhs)
        class(Area_t), intent(in) :: lhs
        class(Area_t), intent(in) :: rhs
        logical :: lessThanOrEqual

        lessThanOrEqual = lhs%square_meters <= rhs%square_meters
    end function lessThanOrEqual

    elemental function equal_(lhs,rhs)
        class(Area_t), intent(in) :: lhs
        class(Area_t), intent(in) :: rhs
        logical :: equal_

        equal_ = lhs%square_meters .safeEq. rhs%square_meters
    end function equal_

    elemental function equalWithinAbsolute(lhs, rhs, within)
        class(Area_t), intent(in) :: lhs
        class(Area_t), intent(in) :: rhs
        class(Area_t), intent(in) :: within
        logical :: equalWithinAbsolute

        equalWithinAbsolute = equalWithinAbsolute_( &
                lhs%square_meters, rhs%square_meters, within%square_meters)
    end function equalWithinAbsolute

    elemental function equalWithinRelative(lhs, rhs, within)
        class(Area_t), intent(in) :: lhs
        class(Area_t), intent(in) :: rhs
        double precision, intent(in) :: within
        logical :: equalWithinRelative

        equalWithinRelative = equalWithinRelative_( &
                lhs%square_meters, rhs%square_meters, within)
    end function equalWithinRelative

    elemental function notEqual(lhs, rhs)
        class(Area_t), intent(in) :: lhs
        class(Area_t), intent(in) :: rhs
        logical :: notEqual

        notEqual = .not. lhs == rhs
    end function notEqual

    elemental function toStringFullPrecision(self) result(string)
        class(Area_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = self%toStringIn(DEFAULT_OUTPUT_UNITS)
    end function toStringFullPrecision

    elemental function toStringWithPrecision(self, significant_digits) result(string)
        class(Area_t), intent(in) :: self
        integer, intent(in) :: significant_digits
        type(VARYING_STRING) :: string

        string = self%toStringIn(DEFAULT_OUTPUT_UNITS, significant_digits)
    end function toStringWithPrecision

    elemental function toStringInFullPrecision(self, units) result(string)
        class(Area_t), intent(in) :: self
        class(AreaUnit_t), intent(in) :: units
        type(VARYING_STRING) :: string

        string = units%toString(toString(self.in.units))
    end function toStringInFullPrecision

    elemental function toStringInWithPrecision( &
            self, units, significant_digits) result(string)
        class(Area_t), intent(in) :: self
        class(AreaUnit_t), intent(in) :: units
        integer, intent(in) :: significant_digits
        type(VARYING_STRING) :: string

        string = units%toString(toString(self.in.units, significant_digits))
    end function toStringInWithPrecision

    elemental function simpleUnitToString(self) result(string)
        class(AreaSimpleUnit_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = trim(self%symbol)
    end function simpleUnitToString

    pure function simpleValueToString(self, value_) result(string)
        class(AreaSimpleUnit_t), intent(in) :: self
        type(VARYING_STRING), intent(in) :: value_
        type(VARYING_STRING) :: string

        string = value_ // " " // self%toString()
    end function simpleValueToString

    pure subroutine simpleParseAs(self, string, errors, area)
        class(AreaSimpleUnit_t), intent(in) :: self
        type(VARYING_STRING), intent(in) :: string
        type(ErrorList_t), intent(out) :: errors
        type(Area_t), intent(out) :: area

        type(ParseResult_t) :: parse_result

        parse_result = parseWith(theParser, string)
        if (parse_result%ok) then
            select type (the_number => parse_result%parsed)
            type is (ParsedRational_t)
                area = the_number%value_.unit.self
            end select
        else
            call errors%appendError(Fatal( &
                    PARSE_ERROR, &
                    Module_("Area_m"), &
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

    pure subroutine gnuplotParseAs(self, string, errors, area)
        class(AreaGnuplotUnit_t), intent(in) :: self
        type(VARYING_STRING), intent(in) :: string
        type(ErrorList_t), intent(out) :: errors
        type(Area_t), intent(out) :: area

        type(ParseResult_t) :: parse_result

        parse_result = parseWith(theParser, string)
        if (parse_result%ok) then
            select type (the_number => parse_result%parsed)
            type is (ParsedRational_t)
                area = the_number%value_.unit.self
            end select
        else
            call errors%appendError(Fatal( &
                    PARSE_ERROR, &
                    Module_("Area_m"), &
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

    pure subroutine latexParseAs(self, string, errors, area)
        class(AreaLatexUnit_t), intent(in) :: self
        type(VARYING_STRING), intent(in) :: string
        type(ErrorList_t), intent(out) :: errors
        type(Area_t), intent(out) :: area

        type(ParseResult_t) :: parse_result

        parse_result = parseWith(theParser, string)
        if (parse_result%ok) then
            select type (the_number => parse_result%parsed)
            type is (ParsedRational_t)
                area = the_number%value_.unit.self
            end select
        else
            call errors%appendError(Fatal( &
                    PARSE_ERROR, &
                    Module_("Area_m"), &
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
        class(AreaGnuplotUnit_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = trim(self%symbol)
    end function gnuplotUnitToString

    pure function gnuplotValueToString(self, value_) result(string)
        class(AreaGnuplotUnit_t), intent(in) :: self
        type(VARYING_STRING), intent(in) :: value_
        type(VARYING_STRING) :: string

        string = value_ // " " // self%toString()
    end function gnuplotValueToString

    elemental function latexUnitToString(self) result(string)
        class(AreaLatexUnit_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = wrapInLatexUnit(trim(self%symbol))
    end function latexUnitToString

    pure function latexValueToString(self, value_) result(string)
        class(AreaLatexUnit_t), intent(in) :: self
        type(VARYING_STRING), intent(in) :: value_
        type(VARYING_STRING) :: string

        string = wrapInLatexQuantity(value_, trim(self%symbol))
    end function latexValueToString

    pure subroutine simpleUnitFromStringC(string, errors, unit)
        character(len=*), intent(in) :: string
        type(ErrorList_t), intent(out) :: errors
        type(AreaSimpleUnit_t), intent(out) :: unit

        type(ErrorList_t) :: errors_

        call fromString(var_str(string), PROVIDED_UNITS, errors_, unit)
        call errors%appendErrors( &
                errors_, &
                Module_("Area_m"), &
                Procedure_("simpleUnitFromStringC"))
    end subroutine simpleUnitFromStringC

    pure subroutine simpleUnitFromStringS(string, errors, unit)
        type(VARYING_STRING), intent(in) :: string
        type(ErrorList_t), intent(out) :: errors
        type(AreaSimpleUnit_t), intent(out) :: unit

        type(ErrorList_t) :: errors_

        call fromString(string, PROVIDED_UNITS, errors_, unit)
        call errors%appendErrors( &
                errors_, &
                Module_("Area_m"), &
                Procedure_("simpleUnitFromStringS"))
    end subroutine simpleUnitFromStringS

    pure subroutine simpleUnitFromStringWithUnitsC(string, units, errors, unit)
        character(len=*), intent(in) :: string
        type(AreaSimpleUnit_t), intent(in) :: units(:)
        type(ErrorList_t), intent(out) :: errors
        type(AreaSimpleUnit_t), intent(out) :: unit

        type(ErrorList_t) :: errors_

        call fromString(var_str(string), units, errors_, unit)
        call errors%appendErrors( &
                errors_, &
                Module_("Area_m"), &
                Procedure_("simpleUnitFromStringWithUnitsC"))
    end subroutine simpleUnitFromStringWithUnitsC

    pure subroutine simpleUnitFromStringWithUnitsS(string, units, errors, unit)
        type(VARYING_STRING), intent(in) :: string
        type(AreaSimpleUnit_t), intent(in) :: units(:)
        type(ErrorList_t), intent(out) :: errors
        type(AreaSimpleUnit_t), intent(out) :: unit

        type(ErrorList_t) :: errors_
        integer :: which_unit

        call selectUnit(string, units, errors, which_unit)
        if (errors_%hasAny()) then
            call errors%appendErrors( &
                    errors_, &
                    Module_("Area_m"), &
                    Procedure_("simpleUnitFromStringWithUnitsS"))
        else
            unit = units(which_unit)
        end if
    end subroutine simpleUnitFromStringWithUnitsS

    pure subroutine selectUnit(string, units, errors, index)
        type(VARYING_STRING), intent(in) :: string
        class(AreaUnit_t), intent(in) :: units(:)
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
                Module_("Area_m"), &
                Procedure_("selectUnit"), &
                '"' // string // '", known units: [' // join(unit_strings, ', ') // ']'))
    end subroutine selectUnit
end module Area_m
