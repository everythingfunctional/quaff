module Mass_m
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
            GRAMS_PER_KILOGRAM, &
            POUNDS_PER_KILOGRAM, &
            TONS_PER_KILOGRAM
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

    type, public :: Mass_t
        double precision :: kilograms
    contains
        private
        procedure :: toUnits
        generic, public :: operator(.in.) => toUnits
        procedure, pass(mass) :: doubleTimesMass
        procedure, pass(mass) :: integerTimesMass
        procedure, pass(mass) :: massTimesDouble
        procedure, pass(mass) :: massTimesInteger
        generic, public :: operator(*) => &
                doubleTimesMass, &
                integerTimesMass, &
                massTimesDouble, &
                massTimesInteger
        procedure :: massDividedByDouble
        procedure :: massDividedByInteger
        procedure, pass(numerator) :: massDividedByMass
        generic, public :: operator(/) => &
                massDividedByDouble, &
                massDividedByInteger, &
                massDividedByMass
        procedure :: massPlusMass
        generic, public :: operator(+) => massPlusMass
        procedure :: massMinusMass
        generic, public :: operator(-) => massMinusMass
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
    end type Mass_t

    type, abstract, public :: MassUnit_t
        double precision :: conversion_factor
    contains
        procedure(justUnitToString), deferred :: unitToString
        procedure(unitWithValueToString), deferred :: valueToString
        generic :: toString => unitToString, valueToString
        procedure(parseAsI), deferred :: parseAs
    end type MassUnit_t

    type, extends(MassUnit_t), public :: MassSimpleUnit_t
        character(len=20) :: symbol
    contains
        procedure :: unitToString => simpleUnitToString
        procedure :: valueToString => simpleValueToString
        procedure :: parseAs => simpleParseAs
    end type MassSimpleUnit_t

    type, extends(MassUnit_t), public :: MassGnuplotUnit_t
        character(len=50) :: symbol
    contains
        procedure :: unitToString => gnuplotUnitToString
        procedure :: valueToString => gnuplotValueToString
        procedure :: parseAs => gnuplotParseAs
    end type MassGnuplotUnit_t

    abstract interface
        elemental function justUnitToString(self) result(string)
            import MassUnit_t, VARYING_STRING
            class(MassUnit_t), intent(in) :: self
            type(VARYING_STRING) :: string
        end function justUnitToString

        pure function unitWithValueToString(self, value_) result(string)
            import MassUnit_t, VARYING_STRING
            class(MassUnit_t), intent(in) :: self
            type(VARYING_STRING), intent(in) :: value_
            type(VARYING_STRING) :: string
        end function unitWithValueToString

        pure subroutine parseAsI(self, string, errors, mass)
            import ErrorList_t, Mass_t, MassUnit_t, VARYING_STRING
            class(MassUnit_t), intent(in) :: self
            type(VARYING_STRING), intent(in) :: string
            type(ErrorList_t), intent(out) :: errors
            type(Mass_t), intent(out) :: mass
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
        module procedure sumMass
    end interface sum

    type(MassSimpleUnit_t), parameter, public :: GRAMS = &
            MassSimpleUnit_t( &
                    conversion_factor = GRAMS_PER_KILOGRAM, &
                    symbol = "g")
    type(MassGnuplotUnit_t), parameter, public :: GRAMS_GNUPLOT = &
            MassGnuplotUnit_t( &
                    conversion_factor = GRAMS_PER_KILOGRAM, &
                    symbol = "g")
    type(MassSimpleUnit_t), parameter, public :: KILOGRAMS = &
            MassSimpleUnit_t( &
                    conversion_factor = 1.0d0, &
                    symbol = "kg")
    type(MassGnuplotUnit_t), parameter, public :: KILOGRAMS_GNUPLOT = &
            MassGnuplotUnit_t( &
                    conversion_factor = 1.0d0, &
                    symbol = "kg")
    type(MassSimpleUnit_t), parameter, public :: POUNDS_MASS = &
            MassSimpleUnit_t( &
                    conversion_factor = POUNDS_PER_KILOGRAM, &
                    symbol = "lbm")
    type(MassGnuplotUnit_t), parameter, public :: POUNDS_MASS_GNUPLOT = &
            MassGnuplotUnit_t( &
                    conversion_factor = POUNDS_PER_KILOGRAM, &
                    symbol = "lbm")
    type(MassSimpleUnit_t), parameter, public :: TONS = &
            MassSimpleUnit_t( &
                    conversion_factor = TONS_PER_KILOGRAM, &
                    symbol = "t")
    type(MassGnuplotUnit_t), parameter, public :: TONS_GNUPLOT = &
            MassGnuplotUnit_t( &
                    conversion_factor = TONS_PER_KILOGRAM, &
                    symbol = "t")

    type(MassSimpleUnit_t), public :: DEFAULT_OUTPUT_UNITS = KILOGRAMS

    type(MassSimpleUnit_t), parameter, public :: PROVIDED_UNITS(*) = &
            [GRAMS, KILOGRAMS, POUNDS_MASS, TONS]
    type(MassGnuplotUnit_t), parameter, public :: PROVIDED_GNUPLOT_UNITS(*) = &
            [GRAMS_GNUPLOT, KILOGRAMS_GNUPLOT, POUNDS_MASS_GNUPLOT, TONS_GNUPLOT]

    public :: operator(.unit.), fromString, selectUnit, sum
contains
    pure subroutine fromStringBasicC(string, errors, mass)
        character(len=*), intent(in) :: string
        type(ErrorList_t), intent(out) :: errors
        type(Mass_t), intent(out) :: mass

        type(ErrorList_t) :: errors_

        call fromString( &
                var_str(string), PROVIDED_UNITS, errors_, mass)
        call errors%appendErrors( &
                errors_, &
                Module_("Mass_m"), &
                Procedure_("fromStringBasicC"))
    end subroutine fromStringBasicC

    pure subroutine fromStringBasicS(string, errors, mass)
        type(VARYING_STRING), intent(in) :: string
        type(ErrorList_t), intent(out) :: errors
        type(Mass_t), intent(out) :: mass

        type(ErrorList_t) :: errors_

        call fromString( &
                string, PROVIDED_UNITS, errors_, mass)
        call errors%appendErrors( &
                errors_, &
                Module_("Mass_m"), &
                Procedure_("fromStringBasicS"))
    end subroutine fromStringBasicS

    pure subroutine fromStringWithUnitsC(string, units, errors, mass)
        character(len=*), intent(in) :: string
        class(MassUnit_t), intent(in) :: units(:)
        type(ErrorList_t), intent(out) :: errors
        type(Mass_t), intent(out) :: mass

        type(ErrorList_t) :: errors_

        call fromString( &
                var_str(string), units, errors_, mass)
        call errors%appendErrors( &
                errors_, &
                Module_("Mass_m"), &
                Procedure_("fromStringWithUnitsC"))
    end subroutine fromStringWithUnitsC

    pure subroutine fromStringWithUnitsS(string, units, errors, mass)
        type(VARYING_STRING), intent(in) :: string
        class(MassUnit_t), intent(in) :: units(:)
        type(ErrorList_t), intent(out) :: errors
        type(Mass_t), intent(out) :: mass

        type(ErrorList_t) :: all_errors(size(units))
        integer :: i

        do i = 1, size(units)
            call units(i)%parseAs(string, all_errors(i), mass)
            if (.not. all_errors(i)%hasAny()) return
        end do
        do i = 1, size(units)
            call errors%appendErrors( &
                    all_errors(i), &
                    Module_("Mass_m"), &
                    Procedure_("fromStringWithUnitsS"))
        end do
    end subroutine fromStringWithUnitsS

    elemental function fromUnits(value_, units) result(mass)
        double precision, intent(in) :: value_
        class(MassUnit_t), intent(in) :: units
        type(Mass_t) :: mass

        mass%kilograms = value_ / units%conversion_factor
    end function fromUnits

    elemental function toUnits(self, units) result(mass)
        class(Mass_t), intent(in) :: self
        class(MassUnit_t), intent(in) :: units
        double precision :: mass

        mass = self%kilograms * units%conversion_factor
    end function toUnits

    elemental function doubleTimesMass( &
            multiplier, mass) result(new_mass)
        double precision, intent(in) :: multiplier
        class(Mass_t), intent(in) :: mass
        type(Mass_t) :: new_mass

        new_mass%kilograms = &
                multiplier * mass%kilograms
    end function doubleTimesMass

    elemental function integerTimesMass( &
            multiplier, mass) result(new_mass)
        integer, intent(in) :: multiplier
        class(Mass_t), intent(in) :: mass
        type(Mass_t) :: new_mass

        new_mass%kilograms = &
                dble(multiplier) * mass%kilograms
    end function integerTimesMass

    elemental function massTimesDouble( &
            mass, multiplier) result(new_mass)
        class(Mass_t), intent(in) :: mass
        double precision, intent(in) :: multiplier
        type(Mass_t) :: new_mass

        new_mass%kilograms = &
                mass%kilograms * multiplier
    end function massTimesDouble

    elemental function massTimesInteger( &
            mass, multiplier) result(new_mass)
        class(Mass_t), intent(in) :: mass
        integer, intent(in) :: multiplier
        type(Mass_t) :: new_mass

        new_mass%kilograms = &
                mass%kilograms * dble(multiplier)
    end function massTimesInteger

    elemental function massDividedByDouble( &
            mass, divisor) result(new_mass)
        class(Mass_t), intent(in) :: mass
        double precision, intent(in) :: divisor
        type(Mass_t) :: new_mass

        new_mass%kilograms = &
                mass%kilograms / divisor
    end function massDividedByDouble

    elemental function massDividedByInteger( &
            mass, divisor) result(new_mass)
        class(Mass_t), intent(in) :: mass
        integer, intent(in) :: divisor
        type(Mass_t) :: new_mass

        new_mass%kilograms = &
                mass%kilograms / dble(divisor)
    end function massDividedByInteger

    elemental function massDividedByMass( &
            numerator, denomenator) result(ratio)
        class(Mass_t), intent(in) :: numerator
        class(Mass_t), intent(in) :: denomenator
        double precision :: ratio

        ratio = numerator%kilograms / denomenator%kilograms
    end function massDividedByMass

    elemental function massPlusMass( &
            mass1, mass2) result(new_mass)
        class(Mass_t), intent(in) :: mass1
        class(Mass_t), intent(in) :: mass2
        type(Mass_t) :: new_mass

        new_mass%kilograms = &
                mass1%kilograms + mass2%kilograms
    end function massPlusMass

    elemental function massMinusMass( &
            mass1, mass2) result(new_mass)
        class(Mass_t), intent(in) :: mass1
        class(Mass_t), intent(in) :: mass2
        type(Mass_t) :: new_mass

        new_mass%kilograms = &
                mass1%kilograms - mass2%kilograms
    end function massMinusMass

    pure function sumMass(masss)
        type(Mass_t), intent(in) :: masss(:)
        type(Mass_t) :: sumMass

        sumMass%kilograms = sum(masss%kilograms)
    end function sumMass

    elemental function greaterThan(lhs, rhs)
        class(Mass_t), intent(in) :: lhs
        class(Mass_t), intent(in) :: rhs
        logical :: greaterThan

        greaterThan = lhs%kilograms > rhs%kilograms
    end function greaterThan

    elemental function lessThan(lhs,rhs)
        class(Mass_t), intent(in) :: lhs
        class(Mass_t), intent(in) :: rhs
        logical :: lessThan

        lessThan = lhs%kilograms < rhs%kilograms
    end function lessThan

    elemental function greaterThanOrEqual(lhs, rhs)
        class(Mass_t), intent(in) :: lhs
        class(Mass_t), intent(in) :: rhs
        logical :: greaterThanOrEqual

        greaterThanOrEqual = lhs%kilograms >= rhs%kilograms
    end function greaterThanOrEqual

    elemental function lessThanOrEqual(lhs, rhs)
        class(Mass_t), intent(in) :: lhs
        class(Mass_t), intent(in) :: rhs
        logical :: lessThanOrEqual

        lessThanOrEqual = lhs%kilograms <= rhs%kilograms
    end function lessThanOrEqual

    elemental function equal_(lhs,rhs)
        class(Mass_t), intent(in) :: lhs
        class(Mass_t), intent(in) :: rhs
        logical :: equal_

        equal_ = lhs%kilograms .safeEq. rhs%kilograms
    end function equal_

    elemental function equalWithinAbsolute(lhs, rhs, within)
        class(Mass_t), intent(in) :: lhs
        class(Mass_t), intent(in) :: rhs
        class(Mass_t), intent(in) :: within
        logical :: equalWithinAbsolute

        equalWithinAbsolute = equalWithinAbsolute_( &
                lhs%kilograms, rhs%kilograms, within%kilograms)
    end function equalWithinAbsolute

    elemental function equalWithinRelative(lhs, rhs, within)
        class(Mass_t), intent(in) :: lhs
        class(Mass_t), intent(in) :: rhs
        double precision, intent(in) :: within
        logical :: equalWithinRelative

        equalWithinRelative = equalWithinRelative_( &
                lhs%kilograms, rhs%kilograms, within)
    end function equalWithinRelative

    elemental function notEqual(lhs, rhs)
        class(Mass_t), intent(in) :: lhs
        class(Mass_t), intent(in) :: rhs
        logical :: notEqual

        notEqual = .not. lhs == rhs
    end function notEqual

    elemental function toStringFullPrecision(self) result(string)
        class(Mass_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = self%toStringIn(DEFAULT_OUTPUT_UNITS)
    end function toStringFullPrecision

    elemental function toStringWithPrecision(self, significant_digits) result(string)
        class(Mass_t), intent(in) :: self
        integer, intent(in) :: significant_digits
        type(VARYING_STRING) :: string

        string = self%toStringIn(DEFAULT_OUTPUT_UNITS, significant_digits)
    end function toStringWithPrecision

    elemental function toStringInFullPrecision(self, units) result(string)
        class(Mass_t), intent(in) :: self
        class(MassUnit_t), intent(in) :: units
        type(VARYING_STRING) :: string

        string = units%toString(toString(self.in.units))
    end function toStringInFullPrecision

    elemental function toStringInWithPrecision( &
            self, units, significant_digits) result(string)
        class(Mass_t), intent(in) :: self
        class(MassUnit_t), intent(in) :: units
        integer, intent(in) :: significant_digits
        type(VARYING_STRING) :: string

        string = units%toString(toString(self.in.units, significant_digits))
    end function toStringInWithPrecision

    elemental function simpleUnitToString(self) result(string)
        class(MassSimpleUnit_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = trim(self%symbol)
    end function simpleUnitToString

    pure function simpleValueToString(self, value_) result(string)
        class(MassSimpleUnit_t), intent(in) :: self
        type(VARYING_STRING), intent(in) :: value_
        type(VARYING_STRING) :: string

        string = value_ // " " // self%toString()
    end function simpleValueToString

    pure subroutine simpleParseAs(self, string, errors, mass)
        class(MassSimpleUnit_t), intent(in) :: self
        type(VARYING_STRING), intent(in) :: string
        type(ErrorList_t), intent(out) :: errors
        type(Mass_t), intent(out) :: mass

        type(ParseResult_t) :: parse_result

        parse_result = parseWith(theParser, string)
        if (parse_result%ok) then
            select type (the_number => parse_result%parsed)
            type is (ParsedRational_t)
                mass = the_number%value_.unit.self
            end select
        else
            call errors%appendError(Fatal( &
                    PARSE_ERROR, &
                    Module_("Mass_m"), &
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

    pure subroutine gnuplotParseAs(self, string, errors, mass)
        class(MassGnuplotUnit_t), intent(in) :: self
        type(VARYING_STRING), intent(in) :: string
        type(ErrorList_t), intent(out) :: errors
        type(Mass_t), intent(out) :: mass

        type(ParseResult_t) :: parse_result

        parse_result = parseWith(theParser, string)
        if (parse_result%ok) then
            select type (the_number => parse_result%parsed)
            type is (ParsedRational_t)
                mass = the_number%value_.unit.self
            end select
        else
            call errors%appendError(Fatal( &
                    PARSE_ERROR, &
                    Module_("Mass_m"), &
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
        class(MassGnuplotUnit_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = trim(self%symbol)
    end function gnuplotUnitToString

    pure function gnuplotValueToString(self, value_) result(string)
        class(MassGnuplotUnit_t), intent(in) :: self
        type(VARYING_STRING), intent(in) :: value_
        type(VARYING_STRING) :: string

        string = value_ // " " // self%toString()
    end function gnuplotValueToString

    pure subroutine simpleUnitFromStringC(string, errors, unit)
        character(len=*), intent(in) :: string
        type(ErrorList_t), intent(out) :: errors
        type(MassSimpleUnit_t), intent(out) :: unit

        type(ErrorList_t) :: errors_

        call fromString(var_str(string), PROVIDED_UNITS, errors_, unit)
        call errors%appendErrors( &
                errors_, &
                Module_("Mass_m"), &
                Procedure_("simpleUnitFromStringC"))
    end subroutine simpleUnitFromStringC

    pure subroutine simpleUnitFromStringS(string, errors, unit)
        type(VARYING_STRING), intent(in) :: string
        type(ErrorList_t), intent(out) :: errors
        type(MassSimpleUnit_t), intent(out) :: unit

        type(ErrorList_t) :: errors_

        call fromString(string, PROVIDED_UNITS, errors_, unit)
        call errors%appendErrors( &
                errors_, &
                Module_("Mass_m"), &
                Procedure_("simpleUnitFromStringS"))
    end subroutine simpleUnitFromStringS

    pure subroutine simpleUnitFromStringWithUnitsC(string, units, errors, unit)
        character(len=*), intent(in) :: string
        type(MassSimpleUnit_t), intent(in) :: units(:)
        type(ErrorList_t), intent(out) :: errors
        type(MassSimpleUnit_t), intent(out) :: unit

        type(ErrorList_t) :: errors_

        call fromString(var_str(string), units, errors_, unit)
        call errors%appendErrors( &
                errors_, &
                Module_("Mass_m"), &
                Procedure_("simpleUnitFromStringWithUnitsC"))
    end subroutine simpleUnitFromStringWithUnitsC

    pure subroutine simpleUnitFromStringWithUnitsS(string, units, errors, unit)
        type(VARYING_STRING), intent(in) :: string
        type(MassSimpleUnit_t), intent(in) :: units(:)
        type(ErrorList_t), intent(out) :: errors
        type(MassSimpleUnit_t), intent(out) :: unit

        type(ErrorList_t) :: errors_
        integer :: which_unit

        call selectUnit(string, units, errors, which_unit)
        if (errors_%hasAny()) then
            call errors%appendErrors( &
                    errors_, &
                    Module_("Mass_m"), &
                    Procedure_("simpleUnitFromStringWithUnitsS"))
        else
            unit = units(which_unit)
        end if
    end subroutine simpleUnitFromStringWithUnitsS

    pure subroutine selectUnit(string, units, errors, index)
        type(VARYING_STRING), intent(in) :: string
        class(MassUnit_t), intent(in) :: units(:)
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
                Module_("Mass_m"), &
                Procedure_("selectUnit"), &
                '"' // string // '", known units: [' // join(unit_strings, ', ') // ']'))
    end subroutine selectUnit
end module Mass_m
