module Enthalpy_m
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
            KILOJOULES_PER_KILOGRAM_PER_JOULES_PER_KILOGRAM
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

    type, public :: Enthalpy_t
        double precision :: joules_per_kilogram
    contains
        private
        procedure :: toUnits
        generic, public :: operator(.in.) => toUnits
        procedure, pass(enthalpy) :: doubleTimesEnthalpy
        procedure, pass(enthalpy) :: integerTimesEnthalpy
        procedure, pass(enthalpy) :: enthalpyTimesDouble
        procedure, pass(enthalpy) :: enthalpyTimesInteger
        generic, public :: operator(*) => &
                doubleTimesEnthalpy, &
                integerTimesEnthalpy, &
                enthalpyTimesDouble, &
                enthalpyTimesInteger
        procedure :: enthalpyDividedByDouble
        procedure :: enthalpyDividedByInteger
        procedure, pass(numerator) :: enthalpyDividedByEnthalpy
        generic, public :: operator(/) => &
                enthalpyDividedByDouble, &
                enthalpyDividedByInteger, &
                enthalpyDividedByEnthalpy
        procedure :: enthalpyPlusEnthalpy
        generic, public :: operator(+) => enthalpyPlusEnthalpy
        procedure :: enthalpyMinusEnthalpy
        generic, public :: operator(-) => enthalpyMinusEnthalpy
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
    end type Enthalpy_t

    type, abstract, public :: EnthalpyUnit_t
        double precision :: conversion_factor
    contains
        procedure(justUnitToString), deferred :: unitToString
        procedure(unitWithValueToString), deferred :: valueToString
        generic :: toString => unitToString, valueToString
        procedure(parseAsI), deferred :: parseAs
    end type EnthalpyUnit_t

    type, extends(EnthalpyUnit_t), public :: EnthalpySimpleUnit_t
        character(len=20) :: symbol
    contains
        procedure :: unitToString => simpleUnitToString
        procedure :: valueToString => simpleValueToString
        procedure :: parseAs => simpleParseAs
    end type EnthalpySimpleUnit_t

    type, extends(EnthalpyUnit_t), public :: EnthalpyGnuplotUnit_t
        character(len=50) :: symbol
    contains
        procedure :: unitToString => gnuplotUnitToString
        procedure :: valueToString => gnuplotValueToString
        procedure :: parseAs => gnuplotParseAs
    end type EnthalpyGnuplotUnit_t

    type, extends(EnthalpyUnit_t), public :: EnthalpyLatexUnit_t
        character(len=100) :: symbol
    contains
        procedure :: unitToString => latexUnitToString
        procedure :: valueToString => latexValueToString
        procedure :: parseAs => latexParseAs
    end type EnthalpyLatexUnit_t

    abstract interface
        elemental function justUnitToString(self) result(string)
            import EnthalpyUnit_t, VARYING_STRING
            class(EnthalpyUnit_t), intent(in) :: self
            type(VARYING_STRING) :: string
        end function justUnitToString

        pure function unitWithValueToString(self, value_) result(string)
            import EnthalpyUnit_t, VARYING_STRING
            class(EnthalpyUnit_t), intent(in) :: self
            type(VARYING_STRING), intent(in) :: value_
            type(VARYING_STRING) :: string
        end function unitWithValueToString

        pure subroutine parseAsI(self, string, errors, enthalpy)
            import ErrorList_t, Enthalpy_t, EnthalpyUnit_t, VARYING_STRING
            class(EnthalpyUnit_t), intent(in) :: self
            type(VARYING_STRING), intent(in) :: string
            type(ErrorList_t), intent(out) :: errors
            type(Enthalpy_t), intent(out) :: enthalpy
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
        module procedure sumEnthalpy
    end interface sum

    type(EnthalpySimpleUnit_t), parameter, public :: JOULES_PER_KILOGRAM = &
            EnthalpySimpleUnit_t( &
                    conversion_factor = 1.0d0, &
                    symbol = "J/kg")
    type(EnthalpyGnuplotUnit_t), parameter, public :: JOULES_PER_KILOGRAM_GNUPLOT = &
            EnthalpyGnuplotUnit_t( &
                    conversion_factor = 1.0d0, &
                    symbol = "J/kg")
    type(EnthalpyLatexUnit_t), parameter, public :: JOULES_PER_KILOGRAM_LATEX = &
            EnthalpyLatexUnit_t( &
                    conversion_factor = 1.0d0, &
                    symbol = "\joule\per\kilo\gram")
    type(EnthalpySimpleUnit_t), parameter, public :: KILOJOULES_PER_KILOGRAM = &
            EnthalpySimpleUnit_t( &
                    conversion_factor = KILOJOULES_PER_KILOGRAM_PER_JOULES_PER_KILOGRAM, &
                    symbol = "kJ/kg")
    type(EnthalpyGnuplotUnit_t), parameter, public :: KILOJOULES_PER_KILOGRAM_GNUPLOT = &
            EnthalpyGnuplotUnit_t( &
                    conversion_factor = KILOJOULES_PER_KILOGRAM_PER_JOULES_PER_KILOGRAM, &
                    symbol = "kJ/kg")
    type(EnthalpyLatexUnit_t), parameter, public :: KILOJOULES_PER_KILOGRAM_LATEX = &
            EnthalpyLatexUnit_t( &
                    conversion_factor = KILOJOULES_PER_KILOGRAM_PER_JOULES_PER_KILOGRAM, &
                    symbol = "\kilo\joule\per\kilo\gram")

    type(EnthalpySimpleUnit_t), public :: DEFAULT_OUTPUT_UNITS = JOULES_PER_KILOGRAM

    type(EnthalpySimpleUnit_t), parameter, public :: PROVIDED_UNITS(*) = &
            [JOULES_PER_KILOGRAM, KILOJOULES_PER_KILOGRAM]
    type(EnthalpyGnuplotUnit_t), parameter, public :: PROVIDED_GNUPLOT_UNITS(*) = &
            [JOULES_PER_KILOGRAM_GNUPLOT, KILOJOULES_PER_KILOGRAM_GNUPLOT]
    type(EnthalpyLatexUnit_t), parameter, public :: PROVIDED_LATEX_UNITS(*) = &
            [JOULES_PER_KILOGRAM_LATEX, KILOJOULES_PER_KILOGRAM_LATEX]

    public :: operator(.unit.), fromString, selectUnit, sum
contains
    pure subroutine fromStringBasicC(string, errors, enthalpy)
        character(len=*), intent(in) :: string
        type(ErrorList_t), intent(out) :: errors
        type(Enthalpy_t), intent(out) :: enthalpy

        type(ErrorList_t) :: errors_

        call fromString( &
                var_str(string), PROVIDED_UNITS, errors_, enthalpy)
        call errors%appendErrors( &
                errors_, &
                Module_("Enthalpy_m"), &
                Procedure_("fromStringBasicC"))
    end subroutine fromStringBasicC

    pure subroutine fromStringBasicS(string, errors, enthalpy)
        type(VARYING_STRING), intent(in) :: string
        type(ErrorList_t), intent(out) :: errors
        type(Enthalpy_t), intent(out) :: enthalpy

        type(ErrorList_t) :: errors_

        call fromString( &
                string, PROVIDED_UNITS, errors_, enthalpy)
        call errors%appendErrors( &
                errors_, &
                Module_("Enthalpy_m"), &
                Procedure_("fromStringBasicS"))
    end subroutine fromStringBasicS

    pure subroutine fromStringWithUnitsC(string, units, errors, enthalpy)
        character(len=*), intent(in) :: string
        class(EnthalpyUnit_t), intent(in) :: units(:)
        type(ErrorList_t), intent(out) :: errors
        type(Enthalpy_t), intent(out) :: enthalpy

        type(ErrorList_t) :: errors_

        call fromString( &
                var_str(string), units, errors_, enthalpy)
        call errors%appendErrors( &
                errors_, &
                Module_("Enthalpy_m"), &
                Procedure_("fromStringWithUnitsC"))
    end subroutine fromStringWithUnitsC

    pure subroutine fromStringWithUnitsS(string, units, errors, enthalpy)
        type(VARYING_STRING), intent(in) :: string
        class(EnthalpyUnit_t), intent(in) :: units(:)
        type(ErrorList_t), intent(out) :: errors
        type(Enthalpy_t), intent(out) :: enthalpy

        type(ErrorList_t) :: all_errors(size(units))
        integer :: i

        do i = 1, size(units)
            call units(i)%parseAs(string, all_errors(i), enthalpy)
            if (.not. all_errors(i)%hasAny()) return
        end do
        do i = 1, size(units)
            call errors%appendErrors( &
                    all_errors(i), &
                    Module_("Enthalpy_m"), &
                    Procedure_("fromStringWithUnitsS"))
        end do
    end subroutine fromStringWithUnitsS

    elemental function fromUnits(value_, units) result(enthalpy)
        double precision, intent(in) :: value_
        class(EnthalpyUnit_t), intent(in) :: units
        type(Enthalpy_t) :: enthalpy

        enthalpy%joules_per_kilogram = value_ / units%conversion_factor
    end function fromUnits

    elemental function toUnits(self, units) result(enthalpy)
        class(Enthalpy_t), intent(in) :: self
        class(EnthalpyUnit_t), intent(in) :: units
        double precision :: enthalpy

        enthalpy = self%joules_per_kilogram * units%conversion_factor
    end function toUnits

    elemental function doubleTimesEnthalpy( &
            multiplier, enthalpy) result(new_enthalpy)
        double precision, intent(in) :: multiplier
        class(Enthalpy_t), intent(in) :: enthalpy
        type(Enthalpy_t) :: new_enthalpy

        new_enthalpy%joules_per_kilogram = &
                multiplier * enthalpy%joules_per_kilogram
    end function doubleTimesEnthalpy

    elemental function integerTimesEnthalpy( &
            multiplier, enthalpy) result(new_enthalpy)
        integer, intent(in) :: multiplier
        class(Enthalpy_t), intent(in) :: enthalpy
        type(Enthalpy_t) :: new_enthalpy

        new_enthalpy%joules_per_kilogram = &
                dble(multiplier) * enthalpy%joules_per_kilogram
    end function integerTimesEnthalpy

    elemental function enthalpyTimesDouble( &
            enthalpy, multiplier) result(new_enthalpy)
        class(Enthalpy_t), intent(in) :: enthalpy
        double precision, intent(in) :: multiplier
        type(Enthalpy_t) :: new_enthalpy

        new_enthalpy%joules_per_kilogram = &
                enthalpy%joules_per_kilogram * multiplier
    end function enthalpyTimesDouble

    elemental function enthalpyTimesInteger( &
            enthalpy, multiplier) result(new_enthalpy)
        class(Enthalpy_t), intent(in) :: enthalpy
        integer, intent(in) :: multiplier
        type(Enthalpy_t) :: new_enthalpy

        new_enthalpy%joules_per_kilogram = &
                enthalpy%joules_per_kilogram * dble(multiplier)
    end function enthalpyTimesInteger

    elemental function enthalpyDividedByDouble( &
            enthalpy, divisor) result(new_enthalpy)
        class(Enthalpy_t), intent(in) :: enthalpy
        double precision, intent(in) :: divisor
        type(Enthalpy_t) :: new_enthalpy

        new_enthalpy%joules_per_kilogram = &
                enthalpy%joules_per_kilogram / divisor
    end function enthalpyDividedByDouble

    elemental function enthalpyDividedByInteger( &
            enthalpy, divisor) result(new_enthalpy)
        class(Enthalpy_t), intent(in) :: enthalpy
        integer, intent(in) :: divisor
        type(Enthalpy_t) :: new_enthalpy

        new_enthalpy%joules_per_kilogram = &
                enthalpy%joules_per_kilogram / dble(divisor)
    end function enthalpyDividedByInteger

    elemental function enthalpyDividedByEnthalpy( &
            numerator, denomenator) result(ratio)
        class(Enthalpy_t), intent(in) :: numerator
        class(Enthalpy_t), intent(in) :: denomenator
        double precision :: ratio

        ratio = numerator%joules_per_kilogram / denomenator%joules_per_kilogram
    end function enthalpyDividedByEnthalpy

    elemental function enthalpyPlusEnthalpy( &
            enthalpy1, enthalpy2) result(new_enthalpy)
        class(Enthalpy_t), intent(in) :: enthalpy1
        class(Enthalpy_t), intent(in) :: enthalpy2
        type(Enthalpy_t) :: new_enthalpy

        new_enthalpy%joules_per_kilogram = &
                enthalpy1%joules_per_kilogram + enthalpy2%joules_per_kilogram
    end function enthalpyPlusEnthalpy

    elemental function enthalpyMinusEnthalpy( &
            enthalpy1, enthalpy2) result(new_enthalpy)
        class(Enthalpy_t), intent(in) :: enthalpy1
        class(Enthalpy_t), intent(in) :: enthalpy2
        type(Enthalpy_t) :: new_enthalpy

        new_enthalpy%joules_per_kilogram = &
                enthalpy1%joules_per_kilogram - enthalpy2%joules_per_kilogram
    end function enthalpyMinusEnthalpy

    pure function sumEnthalpy(enthalpys)
        type(Enthalpy_t), intent(in) :: enthalpys(:)
        type(Enthalpy_t) :: sumEnthalpy

        sumEnthalpy%joules_per_kilogram = sum(enthalpys%joules_per_kilogram)
    end function sumEnthalpy

    elemental function greaterThan(lhs, rhs)
        class(Enthalpy_t), intent(in) :: lhs
        class(Enthalpy_t), intent(in) :: rhs
        logical :: greaterThan

        greaterThan = lhs%joules_per_kilogram > rhs%joules_per_kilogram
    end function greaterThan

    elemental function lessThan(lhs,rhs)
        class(Enthalpy_t), intent(in) :: lhs
        class(Enthalpy_t), intent(in) :: rhs
        logical :: lessThan

        lessThan = lhs%joules_per_kilogram < rhs%joules_per_kilogram
    end function lessThan

    elemental function greaterThanOrEqual(lhs, rhs)
        class(Enthalpy_t), intent(in) :: lhs
        class(Enthalpy_t), intent(in) :: rhs
        logical :: greaterThanOrEqual

        greaterThanOrEqual = lhs%joules_per_kilogram >= rhs%joules_per_kilogram
    end function greaterThanOrEqual

    elemental function lessThanOrEqual(lhs, rhs)
        class(Enthalpy_t), intent(in) :: lhs
        class(Enthalpy_t), intent(in) :: rhs
        logical :: lessThanOrEqual

        lessThanOrEqual = lhs%joules_per_kilogram <= rhs%joules_per_kilogram
    end function lessThanOrEqual

    elemental function equal_(lhs,rhs)
        class(Enthalpy_t), intent(in) :: lhs
        class(Enthalpy_t), intent(in) :: rhs
        logical :: equal_

        equal_ = lhs%joules_per_kilogram .safeEq. rhs%joules_per_kilogram
    end function equal_

    elemental function equalWithinAbsolute(lhs, rhs, within)
        class(Enthalpy_t), intent(in) :: lhs
        class(Enthalpy_t), intent(in) :: rhs
        class(Enthalpy_t), intent(in) :: within
        logical :: equalWithinAbsolute

        equalWithinAbsolute = equalWithinAbsolute_( &
                lhs%joules_per_kilogram, rhs%joules_per_kilogram, within%joules_per_kilogram)
    end function equalWithinAbsolute

    elemental function equalWithinRelative(lhs, rhs, within)
        class(Enthalpy_t), intent(in) :: lhs
        class(Enthalpy_t), intent(in) :: rhs
        double precision, intent(in) :: within
        logical :: equalWithinRelative

        equalWithinRelative = equalWithinRelative_( &
                lhs%joules_per_kilogram, rhs%joules_per_kilogram, within)
    end function equalWithinRelative

    elemental function notEqual(lhs, rhs)
        class(Enthalpy_t), intent(in) :: lhs
        class(Enthalpy_t), intent(in) :: rhs
        logical :: notEqual

        notEqual = .not. lhs == rhs
    end function notEqual

    elemental function toStringFullPrecision(self) result(string)
        class(Enthalpy_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = self%toStringIn(DEFAULT_OUTPUT_UNITS)
    end function toStringFullPrecision

    elemental function toStringWithPrecision(self, significant_digits) result(string)
        class(Enthalpy_t), intent(in) :: self
        integer, intent(in) :: significant_digits
        type(VARYING_STRING) :: string

        string = self%toStringIn(DEFAULT_OUTPUT_UNITS, significant_digits)
    end function toStringWithPrecision

    elemental function toStringInFullPrecision(self, units) result(string)
        class(Enthalpy_t), intent(in) :: self
        class(EnthalpyUnit_t), intent(in) :: units
        type(VARYING_STRING) :: string

        string = units%toString(toString(self.in.units))
    end function toStringInFullPrecision

    elemental function toStringInWithPrecision( &
            self, units, significant_digits) result(string)
        class(Enthalpy_t), intent(in) :: self
        class(EnthalpyUnit_t), intent(in) :: units
        integer, intent(in) :: significant_digits
        type(VARYING_STRING) :: string

        string = units%toString(toString(self.in.units, significant_digits))
    end function toStringInWithPrecision

    elemental function simpleUnitToString(self) result(string)
        class(EnthalpySimpleUnit_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = trim(self%symbol)
    end function simpleUnitToString

    pure function simpleValueToString(self, value_) result(string)
        class(EnthalpySimpleUnit_t), intent(in) :: self
        type(VARYING_STRING), intent(in) :: value_
        type(VARYING_STRING) :: string

        string = value_ // " " // self%toString()
    end function simpleValueToString

    pure subroutine simpleParseAs(self, string, errors, enthalpy)
        class(EnthalpySimpleUnit_t), intent(in) :: self
        type(VARYING_STRING), intent(in) :: string
        type(ErrorList_t), intent(out) :: errors
        type(Enthalpy_t), intent(out) :: enthalpy

        type(ParseResult_t) :: parse_result

        parse_result = parseWith(theParser, string)
        if (parse_result%ok) then
            select type (the_number => parse_result%parsed)
            type is (ParsedRational_t)
                enthalpy = the_number%value_.unit.self
            end select
        else
            call errors%appendError(Fatal( &
                    PARSE_ERROR, &
                    Module_("Enthalpy_m"), &
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

    pure subroutine gnuplotParseAs(self, string, errors, enthalpy)
        class(EnthalpyGnuplotUnit_t), intent(in) :: self
        type(VARYING_STRING), intent(in) :: string
        type(ErrorList_t), intent(out) :: errors
        type(Enthalpy_t), intent(out) :: enthalpy

        type(ParseResult_t) :: parse_result

        parse_result = parseWith(theParser, string)
        if (parse_result%ok) then
            select type (the_number => parse_result%parsed)
            type is (ParsedRational_t)
                enthalpy = the_number%value_.unit.self
            end select
        else
            call errors%appendError(Fatal( &
                    PARSE_ERROR, &
                    Module_("Enthalpy_m"), &
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

    pure subroutine latexParseAs(self, string, errors, enthalpy)
        class(EnthalpyLatexUnit_t), intent(in) :: self
        type(VARYING_STRING), intent(in) :: string
        type(ErrorList_t), intent(out) :: errors
        type(Enthalpy_t), intent(out) :: enthalpy

        type(ParseResult_t) :: parse_result

        parse_result = parseWith(theParser, string)
        if (parse_result%ok) then
            select type (the_number => parse_result%parsed)
            type is (ParsedRational_t)
                enthalpy = the_number%value_.unit.self
            end select
        else
            call errors%appendError(Fatal( &
                    PARSE_ERROR, &
                    Module_("Enthalpy_m"), &
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
        class(EnthalpyGnuplotUnit_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = trim(self%symbol)
    end function gnuplotUnitToString

    pure function gnuplotValueToString(self, value_) result(string)
        class(EnthalpyGnuplotUnit_t), intent(in) :: self
        type(VARYING_STRING), intent(in) :: value_
        type(VARYING_STRING) :: string

        string = value_ // " " // self%toString()
    end function gnuplotValueToString

    elemental function latexUnitToString(self) result(string)
        class(EnthalpyLatexUnit_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = wrapInLatexUnit(trim(self%symbol))
    end function latexUnitToString

    pure function latexValueToString(self, value_) result(string)
        class(EnthalpyLatexUnit_t), intent(in) :: self
        type(VARYING_STRING), intent(in) :: value_
        type(VARYING_STRING) :: string

        string = wrapInLatexQuantity(value_, trim(self%symbol))
    end function latexValueToString

    pure subroutine simpleUnitFromStringC(string, errors, unit)
        character(len=*), intent(in) :: string
        type(ErrorList_t), intent(out) :: errors
        type(EnthalpySimpleUnit_t), intent(out) :: unit

        type(ErrorList_t) :: errors_

        call fromString(var_str(string), PROVIDED_UNITS, errors_, unit)
        call errors%appendErrors( &
                errors_, &
                Module_("Enthalpy_m"), &
                Procedure_("simpleUnitFromStringC"))
    end subroutine simpleUnitFromStringC

    pure subroutine simpleUnitFromStringS(string, errors, unit)
        type(VARYING_STRING), intent(in) :: string
        type(ErrorList_t), intent(out) :: errors
        type(EnthalpySimpleUnit_t), intent(out) :: unit

        type(ErrorList_t) :: errors_

        call fromString(string, PROVIDED_UNITS, errors_, unit)
        call errors%appendErrors( &
                errors_, &
                Module_("Enthalpy_m"), &
                Procedure_("simpleUnitFromStringS"))
    end subroutine simpleUnitFromStringS

    pure subroutine simpleUnitFromStringWithUnitsC(string, units, errors, unit)
        character(len=*), intent(in) :: string
        type(EnthalpySimpleUnit_t), intent(in) :: units(:)
        type(ErrorList_t), intent(out) :: errors
        type(EnthalpySimpleUnit_t), intent(out) :: unit

        type(ErrorList_t) :: errors_

        call fromString(var_str(string), units, errors_, unit)
        call errors%appendErrors( &
                errors_, &
                Module_("Enthalpy_m"), &
                Procedure_("simpleUnitFromStringWithUnitsC"))
    end subroutine simpleUnitFromStringWithUnitsC

    pure subroutine simpleUnitFromStringWithUnitsS(string, units, errors, unit)
        type(VARYING_STRING), intent(in) :: string
        type(EnthalpySimpleUnit_t), intent(in) :: units(:)
        type(ErrorList_t), intent(out) :: errors
        type(EnthalpySimpleUnit_t), intent(out) :: unit

        type(ErrorList_t) :: errors_
        integer :: which_unit

        call selectUnit(string, units, errors, which_unit)
        if (errors_%hasAny()) then
            call errors%appendErrors( &
                    errors_, &
                    Module_("Enthalpy_m"), &
                    Procedure_("simpleUnitFromStringWithUnitsS"))
        else
            unit = units(which_unit)
        end if
    end subroutine simpleUnitFromStringWithUnitsS

    pure subroutine selectUnit(string, units, errors, index)
        type(VARYING_STRING), intent(in) :: string
        class(EnthalpyUnit_t), intent(in) :: units(:)
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
                Module_("Enthalpy_m"), &
                Procedure_("selectUnit"), &
                '"' // string // '", known units: [' // join(unit_strings, ', ') // ']'))
    end subroutine selectUnit
end module Enthalpy_m
