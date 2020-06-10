module Molar_mass_m
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
    use quaff_Conversion_factors_m, only: GRAMS_PER_MOL_PER_KILOGRAMS_PER_MOL
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

    type, public :: MolarMass_t
        double precision :: kilograms_per_mol
    contains
        private
        procedure :: toUnits
        generic, public :: operator(.in.) => toUnits
        procedure, pass(molar_mass) :: doubleTimesMolarMass
        procedure, pass(molar_mass) :: integerTimesMolarMass
        procedure, pass(molar_mass) :: molarMassTimesDouble
        procedure, pass(molar_mass) :: molarMassTimesInteger
        generic, public :: operator(*) => &
                doubleTimesMolarMass, &
                integerTimesMolarMass, &
                molarMassTimesDouble, &
                molarMassTimesInteger
        procedure :: molarMassDividedByDouble
        procedure :: molarMassDividedByInteger
        procedure, pass(numerator) :: molarMassDividedByMolarMass
        generic, public :: operator(/) => &
                molarMassDividedByDouble, &
                molarMassDividedByInteger, &
                molarMassDividedByMolarMass
        procedure :: molarMassPlusMolarMass
        generic, public :: operator(+) => molarMassPlusMolarMass
        procedure :: molarMassMinusMolarMass
        generic, public :: operator(-) => molarMassMinusMolarMass
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
    end type MolarMass_t

    type, abstract, public :: MolarMassUnit_t
        double precision :: conversion_factor
    contains
        procedure(justUnitToString), deferred :: unitToString
        procedure(unitWithValueToString), deferred :: valueToString
        generic :: toString => unitToString, valueToString
        procedure(parseAsI), deferred :: parseAs
    end type MolarMassUnit_t

    type, extends(MolarMassUnit_t), public :: MolarMassSimpleUnit_t
        character(len=20) :: symbol
    contains
        procedure :: unitToString => simpleUnitToString
        procedure :: valueToString => simpleValueToString
        procedure :: parseAs => simpleParseAs
    end type MolarMassSimpleUnit_t

    type, extends(MolarMassUnit_t), public :: MolarMassGnuplotUnit_t
        character(len=50) :: symbol
    contains
        procedure :: unitToString => gnuplotUnitToString
        procedure :: valueToString => gnuplotValueToString
        procedure :: parseAs => gnuplotParseAs
    end type MolarMassGnuplotUnit_t

    type, extends(MolarMassUnit_t), public :: MolarMassLatexUnit_t
        character(len=100) :: symbol
    contains
        procedure :: unitToString => latexUnitToString
        procedure :: valueToString => latexValueToString
        procedure :: parseAs => latexParseAs
    end type MolarMassLatexUnit_t

    abstract interface
        elemental function justUnitToString(self) result(string)
            import MolarMassUnit_t, VARYING_STRING
            class(MolarMassUnit_t), intent(in) :: self
            type(VARYING_STRING) :: string
        end function justUnitToString

        pure function unitWithValueToString(self, value_) result(string)
            import MolarMassUnit_t, VARYING_STRING
            class(MolarMassUnit_t), intent(in) :: self
            type(VARYING_STRING), intent(in) :: value_
            type(VARYING_STRING) :: string
        end function unitWithValueToString

        pure subroutine parseAsI(self, string, errors, molar_mass)
            import ErrorList_t, MolarMass_t, MolarMassUnit_t, VARYING_STRING
            class(MolarMassUnit_t), intent(in) :: self
            type(VARYING_STRING), intent(in) :: string
            type(ErrorList_t), intent(out) :: errors
            type(MolarMass_t), intent(out) :: molar_mass
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
        module procedure sumMolarMass
    end interface sum

    type(MolarMassSimpleUnit_t), parameter, public :: GRAMS_PER_MOL = &
            MolarMassSimpleUnit_t( &
                    conversion_factor = GRAMS_PER_MOL_PER_KILOGRAMS_PER_MOL, &
                    symbol = "g/mol")
    type(MolarMassGnuplotUnit_t), parameter, public :: GRAMS_PER_MOL_GNUPLOT = &
            MolarMassGnuplotUnit_t( &
                    conversion_factor = GRAMS_PER_MOL_PER_KILOGRAMS_PER_MOL, &
                    symbol = "g/mol")
    type(MolarMassLatexUnit_t), parameter, public :: GRAMS_PER_MOL_LATEX = &
            MolarMassLatexUnit_t( &
                    conversion_factor = GRAMS_PER_MOL_PER_KILOGRAMS_PER_MOL, &
                    symbol = "\gram\per\mole")
    type(MolarMassSimpleUnit_t), parameter, public :: KILOGRAMS_PER_MOL = &
            MolarMassSimpleUnit_t( &
                    conversion_factor = 1.0d0, &
                    symbol = "kg/mol")
    type(MolarMassGnuplotUnit_t), parameter, public :: KILOGRAMS_PER_MOL_GNUPLOT = &
            MolarMassGnuplotUnit_t( &
                    conversion_factor = 1.0d0, &
                    symbol = "kg/mol")
    type(MolarMassLatexUnit_t), parameter, public :: KILOGRAMS_PER_MOL_LATEX = &
            MolarMassLatexUnit_t( &
                    conversion_factor = 1.0d0, &
                    symbol = "\kilo\gram\per\mole")

    type(MolarMassSimpleUnit_t), public :: DEFAULT_OUTPUT_UNITS = KILOGRAMS_PER_MOL

    type(MolarMassSimpleUnit_t), parameter, public :: PROVIDED_UNITS(*) = &
            [GRAMS_PER_MOL, KILOGRAMS_PER_MOL]
    type(MolarMassGnuplotUnit_t), parameter, public :: PROVIDED_GNUPLOT_UNITS(*) = &
            [GRAMS_PER_MOL_GNUPLOT, KILOGRAMS_PER_MOL_GNUPLOT]
    type(MolarMassLatexUnit_t), parameter, public :: PROVIDED_LATEX_UNITS(*) = &
            [GRAMS_PER_MOL_LATEX, KILOGRAMS_PER_MOL_LATEX]

    public :: operator(.unit.), fromString, selectUnit, sum
contains
    pure subroutine fromStringBasicC(string, errors, molar_mass)
        character(len=*), intent(in) :: string
        type(ErrorList_t), intent(out) :: errors
        type(MolarMass_t), intent(out) :: molar_mass

        type(ErrorList_t) :: errors_

        call fromString( &
                var_str(string), PROVIDED_UNITS, errors_, molar_mass)
        call errors%appendErrors( &
                errors_, &
                Module_("Molar_mass_m"), &
                Procedure_("fromStringBasicC"))
    end subroutine fromStringBasicC

    pure subroutine fromStringBasicS(string, errors, molar_mass)
        type(VARYING_STRING), intent(in) :: string
        type(ErrorList_t), intent(out) :: errors
        type(MolarMass_t), intent(out) :: molar_mass

        type(ErrorList_t) :: errors_

        call fromString( &
                string, PROVIDED_UNITS, errors_, molar_mass)
        call errors%appendErrors( &
                errors_, &
                Module_("Molar_mass_m"), &
                Procedure_("fromStringBasicS"))
    end subroutine fromStringBasicS

    pure subroutine fromStringWithUnitsC(string, units, errors, molar_mass)
        character(len=*), intent(in) :: string
        class(MolarMassUnit_t), intent(in) :: units(:)
        type(ErrorList_t), intent(out) :: errors
        type(MolarMass_t), intent(out) :: molar_mass

        type(ErrorList_t) :: errors_

        call fromString( &
                var_str(string), units, errors_, molar_mass)
        call errors%appendErrors( &
                errors_, &
                Module_("Molar_mass_m"), &
                Procedure_("fromStringWithUnitsC"))
    end subroutine fromStringWithUnitsC

    pure subroutine fromStringWithUnitsS(string, units, errors, molar_mass)
        type(VARYING_STRING), intent(in) :: string
        class(MolarMassUnit_t), intent(in) :: units(:)
        type(ErrorList_t), intent(out) :: errors
        type(MolarMass_t), intent(out) :: molar_mass

        type(ErrorList_t) :: all_errors(size(units))
        integer :: i

        do i = 1, size(units)
            call units(i)%parseAs(string, all_errors(i), molar_mass)
            if (.not. all_errors(i)%hasAny()) return
        end do
        do i = 1, size(units)
            call errors%appendErrors( &
                    all_errors(i), &
                    Module_("Molar_mass_m"), &
                    Procedure_("fromStringWithUnitsS"))
        end do
    end subroutine fromStringWithUnitsS

    elemental function fromUnits(value_, units) result(molar_mass)
        double precision, intent(in) :: value_
        class(MolarMassUnit_t), intent(in) :: units
        type(MolarMass_t) :: molar_mass

        molar_mass%kilograms_per_mol = value_ / units%conversion_factor
    end function fromUnits

    elemental function toUnits(self, units) result(molar_mass)
        class(MolarMass_t), intent(in) :: self
        class(MolarMassUnit_t), intent(in) :: units
        double precision :: molar_mass

        molar_mass = self%kilograms_per_mol * units%conversion_factor
    end function toUnits

    elemental function doubleTimesMolarMass( &
            multiplier, molar_mass) result(new_molar_mass)
        double precision, intent(in) :: multiplier
        class(MolarMass_t), intent(in) :: molar_mass
        type(MolarMass_t) :: new_molar_mass

        new_molar_mass%kilograms_per_mol = &
                multiplier * molar_mass%kilograms_per_mol
    end function doubleTimesMolarMass

    elemental function integerTimesMolarMass( &
            multiplier, molar_mass) result(new_molar_mass)
        integer, intent(in) :: multiplier
        class(MolarMass_t), intent(in) :: molar_mass
        type(MolarMass_t) :: new_molar_mass

        new_molar_mass%kilograms_per_mol = &
                dble(multiplier) * molar_mass%kilograms_per_mol
    end function integerTimesMolarMass

    elemental function molarMassTimesDouble( &
            molar_mass, multiplier) result(new_molar_mass)
        class(MolarMass_t), intent(in) :: molar_mass
        double precision, intent(in) :: multiplier
        type(MolarMass_t) :: new_molar_mass

        new_molar_mass%kilograms_per_mol = &
                molar_mass%kilograms_per_mol * multiplier
    end function molarMassTimesDouble

    elemental function molarMassTimesInteger( &
            molar_mass, multiplier) result(new_molar_mass)
        class(MolarMass_t), intent(in) :: molar_mass
        integer, intent(in) :: multiplier
        type(MolarMass_t) :: new_molar_mass

        new_molar_mass%kilograms_per_mol = &
                molar_mass%kilograms_per_mol * dble(multiplier)
    end function molarMassTimesInteger

    elemental function molarMassDividedByDouble( &
            molar_mass, divisor) result(new_molar_mass)
        class(MolarMass_t), intent(in) :: molar_mass
        double precision, intent(in) :: divisor
        type(MolarMass_t) :: new_molar_mass

        new_molar_mass%kilograms_per_mol = &
                molar_mass%kilograms_per_mol / divisor
    end function molarMassDividedByDouble

    elemental function molarMassDividedByInteger( &
            molar_mass, divisor) result(new_molar_mass)
        class(MolarMass_t), intent(in) :: molar_mass
        integer, intent(in) :: divisor
        type(MolarMass_t) :: new_molar_mass

        new_molar_mass%kilograms_per_mol = &
                molar_mass%kilograms_per_mol / dble(divisor)
    end function molarMassDividedByInteger

    elemental function molarMassDividedByMolarMass( &
            numerator, denomenator) result(ratio)
        class(MolarMass_t), intent(in) :: numerator
        class(MolarMass_t), intent(in) :: denomenator
        double precision :: ratio

        ratio = numerator%kilograms_per_mol / denomenator%kilograms_per_mol
    end function molarMassDividedByMolarMass

    elemental function molarMassPlusMolarMass( &
            molar_mass1, molar_mass2) result(new_molar_mass)
        class(MolarMass_t), intent(in) :: molar_mass1
        class(MolarMass_t), intent(in) :: molar_mass2
        type(MolarMass_t) :: new_molar_mass

        new_molar_mass%kilograms_per_mol = &
                molar_mass1%kilograms_per_mol + molar_mass2%kilograms_per_mol
    end function molarMassPlusMolarMass

    elemental function molarMassMinusMolarMass( &
            molar_mass1, molar_mass2) result(new_molar_mass)
        class(MolarMass_t), intent(in) :: molar_mass1
        class(MolarMass_t), intent(in) :: molar_mass2
        type(MolarMass_t) :: new_molar_mass

        new_molar_mass%kilograms_per_mol = &
                molar_mass1%kilograms_per_mol - molar_mass2%kilograms_per_mol
    end function molarMassMinusMolarMass

    pure function sumMolarMass(molar_masss)
        type(MolarMass_t), intent(in) :: molar_masss(:)
        type(MolarMass_t) :: sumMolarMass

        sumMolarMass%kilograms_per_mol = sum(molar_masss%kilograms_per_mol)
    end function sumMolarMass

    elemental function greaterThan(lhs, rhs)
        class(MolarMass_t), intent(in) :: lhs
        class(MolarMass_t), intent(in) :: rhs
        logical :: greaterThan

        greaterThan = lhs%kilograms_per_mol > rhs%kilograms_per_mol
    end function greaterThan

    elemental function lessThan(lhs,rhs)
        class(MolarMass_t), intent(in) :: lhs
        class(MolarMass_t), intent(in) :: rhs
        logical :: lessThan

        lessThan = lhs%kilograms_per_mol < rhs%kilograms_per_mol
    end function lessThan

    elemental function greaterThanOrEqual(lhs, rhs)
        class(MolarMass_t), intent(in) :: lhs
        class(MolarMass_t), intent(in) :: rhs
        logical :: greaterThanOrEqual

        greaterThanOrEqual = lhs%kilograms_per_mol >= rhs%kilograms_per_mol
    end function greaterThanOrEqual

    elemental function lessThanOrEqual(lhs, rhs)
        class(MolarMass_t), intent(in) :: lhs
        class(MolarMass_t), intent(in) :: rhs
        logical :: lessThanOrEqual

        lessThanOrEqual = lhs%kilograms_per_mol <= rhs%kilograms_per_mol
    end function lessThanOrEqual

    elemental function equal_(lhs,rhs)
        class(MolarMass_t), intent(in) :: lhs
        class(MolarMass_t), intent(in) :: rhs
        logical :: equal_

        equal_ = lhs%kilograms_per_mol .safeEq. rhs%kilograms_per_mol
    end function equal_

    elemental function equalWithinAbsolute(lhs, rhs, within)
        class(MolarMass_t), intent(in) :: lhs
        class(MolarMass_t), intent(in) :: rhs
        class(MolarMass_t), intent(in) :: within
        logical :: equalWithinAbsolute

        equalWithinAbsolute = equalWithinAbsolute_( &
                lhs%kilograms_per_mol, rhs%kilograms_per_mol, within%kilograms_per_mol)
    end function equalWithinAbsolute

    elemental function equalWithinRelative(lhs, rhs, within)
        class(MolarMass_t), intent(in) :: lhs
        class(MolarMass_t), intent(in) :: rhs
        double precision, intent(in) :: within
        logical :: equalWithinRelative

        equalWithinRelative = equalWithinRelative_( &
                lhs%kilograms_per_mol, rhs%kilograms_per_mol, within)
    end function equalWithinRelative

    elemental function notEqual(lhs, rhs)
        class(MolarMass_t), intent(in) :: lhs
        class(MolarMass_t), intent(in) :: rhs
        logical :: notEqual

        notEqual = .not. lhs == rhs
    end function notEqual

    elemental function toStringFullPrecision(self) result(string)
        class(MolarMass_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = self%toStringIn(DEFAULT_OUTPUT_UNITS)
    end function toStringFullPrecision

    elemental function toStringWithPrecision(self, significant_digits) result(string)
        class(MolarMass_t), intent(in) :: self
        integer, intent(in) :: significant_digits
        type(VARYING_STRING) :: string

        string = self%toStringIn(DEFAULT_OUTPUT_UNITS, significant_digits)
    end function toStringWithPrecision

    elemental function toStringInFullPrecision(self, units) result(string)
        class(MolarMass_t), intent(in) :: self
        class(MolarMassUnit_t), intent(in) :: units
        type(VARYING_STRING) :: string

        string = units%toString(toString(self.in.units))
    end function toStringInFullPrecision

    elemental function toStringInWithPrecision( &
            self, units, significant_digits) result(string)
        class(MolarMass_t), intent(in) :: self
        class(MolarMassUnit_t), intent(in) :: units
        integer, intent(in) :: significant_digits
        type(VARYING_STRING) :: string

        string = units%toString(toString(self.in.units, significant_digits))
    end function toStringInWithPrecision

    elemental function simpleUnitToString(self) result(string)
        class(MolarMassSimpleUnit_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = trim(self%symbol)
    end function simpleUnitToString

    pure function simpleValueToString(self, value_) result(string)
        class(MolarMassSimpleUnit_t), intent(in) :: self
        type(VARYING_STRING), intent(in) :: value_
        type(VARYING_STRING) :: string

        string = value_ // " " // self%toString()
    end function simpleValueToString

    pure subroutine simpleParseAs(self, string, errors, molar_mass)
        class(MolarMassSimpleUnit_t), intent(in) :: self
        type(VARYING_STRING), intent(in) :: string
        type(ErrorList_t), intent(out) :: errors
        type(MolarMass_t), intent(out) :: molar_mass

        type(ParseResult_t) :: parse_result

        parse_result = parseWith(theParser, string)
        if (parse_result%ok) then
            select type (the_number => parse_result%parsed)
            type is (ParsedRational_t)
                molar_mass = the_number%value_.unit.self
            end select
        else
            call errors%appendError(Fatal( &
                    PARSE_ERROR, &
                    Module_("Molar_mass_m"), &
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

    pure subroutine gnuplotParseAs(self, string, errors, molar_mass)
        class(MolarMassGnuplotUnit_t), intent(in) :: self
        type(VARYING_STRING), intent(in) :: string
        type(ErrorList_t), intent(out) :: errors
        type(MolarMass_t), intent(out) :: molar_mass

        type(ParseResult_t) :: parse_result

        parse_result = parseWith(theParser, string)
        if (parse_result%ok) then
            select type (the_number => parse_result%parsed)
            type is (ParsedRational_t)
                molar_mass = the_number%value_.unit.self
            end select
        else
            call errors%appendError(Fatal( &
                    PARSE_ERROR, &
                    Module_("Molar_mass_m"), &
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

    pure subroutine latexParseAs(self, string, errors, molar_mass)
        class(MolarMassLatexUnit_t), intent(in) :: self
        type(VARYING_STRING), intent(in) :: string
        type(ErrorList_t), intent(out) :: errors
        type(MolarMass_t), intent(out) :: molar_mass

        type(ParseResult_t) :: parse_result

        parse_result = parseWith(theParser, string)
        if (parse_result%ok) then
            select type (the_number => parse_result%parsed)
            type is (ParsedRational_t)
                molar_mass = the_number%value_.unit.self
            end select
        else
            call errors%appendError(Fatal( &
                    PARSE_ERROR, &
                    Module_("Molar_mass_m"), &
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
        class(MolarMassGnuplotUnit_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = trim(self%symbol)
    end function gnuplotUnitToString

    pure function gnuplotValueToString(self, value_) result(string)
        class(MolarMassGnuplotUnit_t), intent(in) :: self
        type(VARYING_STRING), intent(in) :: value_
        type(VARYING_STRING) :: string

        string = value_ // " " // self%toString()
    end function gnuplotValueToString

    elemental function latexUnitToString(self) result(string)
        class(MolarMassLatexUnit_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = wrapInLatexUnit(trim(self%symbol))
    end function latexUnitToString

    pure function latexValueToString(self, value_) result(string)
        class(MolarMassLatexUnit_t), intent(in) :: self
        type(VARYING_STRING), intent(in) :: value_
        type(VARYING_STRING) :: string

        string = wrapInLatexQuantity(value_, trim(self%symbol))
    end function latexValueToString

    pure subroutine simpleUnitFromStringC(string, errors, unit)
        character(len=*), intent(in) :: string
        type(ErrorList_t), intent(out) :: errors
        type(MolarMassSimpleUnit_t), intent(out) :: unit

        type(ErrorList_t) :: errors_

        call fromString(var_str(string), PROVIDED_UNITS, errors_, unit)
        call errors%appendErrors( &
                errors_, &
                Module_("Molar_mass_m"), &
                Procedure_("simpleUnitFromStringC"))
    end subroutine simpleUnitFromStringC

    pure subroutine simpleUnitFromStringS(string, errors, unit)
        type(VARYING_STRING), intent(in) :: string
        type(ErrorList_t), intent(out) :: errors
        type(MolarMassSimpleUnit_t), intent(out) :: unit

        type(ErrorList_t) :: errors_

        call fromString(string, PROVIDED_UNITS, errors_, unit)
        call errors%appendErrors( &
                errors_, &
                Module_("Molar_mass_m"), &
                Procedure_("simpleUnitFromStringS"))
    end subroutine simpleUnitFromStringS

    pure subroutine simpleUnitFromStringWithUnitsC(string, units, errors, unit)
        character(len=*), intent(in) :: string
        type(MolarMassSimpleUnit_t), intent(in) :: units(:)
        type(ErrorList_t), intent(out) :: errors
        type(MolarMassSimpleUnit_t), intent(out) :: unit

        type(ErrorList_t) :: errors_

        call fromString(var_str(string), units, errors_, unit)
        call errors%appendErrors( &
                errors_, &
                Module_("Molar_mass_m"), &
                Procedure_("simpleUnitFromStringWithUnitsC"))
    end subroutine simpleUnitFromStringWithUnitsC

    pure subroutine simpleUnitFromStringWithUnitsS(string, units, errors, unit)
        type(VARYING_STRING), intent(in) :: string
        type(MolarMassSimpleUnit_t), intent(in) :: units(:)
        type(ErrorList_t), intent(out) :: errors
        type(MolarMassSimpleUnit_t), intent(out) :: unit

        type(ErrorList_t) :: errors_
        integer :: which_unit

        call selectUnit(string, units, errors, which_unit)
        if (errors_%hasAny()) then
            call errors%appendErrors( &
                    errors_, &
                    Module_("Molar_mass_m"), &
                    Procedure_("simpleUnitFromStringWithUnitsS"))
        else
            unit = units(which_unit)
        end if
    end subroutine simpleUnitFromStringWithUnitsS

    pure subroutine selectUnit(string, units, errors, index)
        type(VARYING_STRING), intent(in) :: string
        class(MolarMassUnit_t), intent(in) :: units(:)
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
                Module_("Molar_mass_m"), &
                Procedure_("selectUnit"), &
                '"' // string // '", known units: [' // join(unit_strings, ', ') // ']'))
    end subroutine selectUnit
end module Molar_mass_m
