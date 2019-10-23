module Mass_m
    use Conversion_factors_m, only: GRAMS_PER_KILOGRAM

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
    end type Mass_t

    type, public :: MassUnit_t
        double precision :: conversion_factor
        character(len=10) :: symbol
        character(len=50) :: gnuplot_symbol
        character(len=100) :: latex_symbol
    contains
        procedure :: toString => unitToString
        procedure :: toGnuplotString => unitToGnuplotString
        procedure :: toLatexString => unitToLatexString
    end type MassUnit_t

    interface operator(.unit.)
        module procedure fromUnits
    end interface operator(.unit.)

    interface massFromString
        module procedure fromStringBasicC
        module procedure fromStringBasicS
        module procedure fromStringWithUnitsC
        module procedure fromStringWithUnitsS
    end interface massFromString

    interface massUnitFromString
        module procedure unitFromStringBasicC
        module procedure unitFromStringBasicS
        module procedure unitFromStringWithUnitsC
        module procedure unitFromStringWithUnitsS
    end interface massUnitFromString

    type(MassUnit_t), parameter, public :: GRAMS = &
            MassUnit_t( &
                    conversion_factor = 1.0d0, &
                    symbol = "g", &
                    gnuplot_symbol = "g", &
                    latex_symbol = "\gram")
    type(MassUnit_t), parameter, public :: KILOGRAMS = &
            MassUnit_t( &
                    conversion_factor = 1.0d0, &
                    symbol = "kg", &
                    gnuplot_symbol = "kg", &
                    latex_symbol = "\kilo\gram")

    type(MassUnit_t), public :: DEFAULT_OUTPUT_UNITS = KILOGRAMS

    type(MassUnit_t), parameter, public :: PROVIDED_UNITS(*) = &
            [GRAMS, KILOGRAMS]

    public :: &
            operator(.unit.), &
            massFromString, &
            massUnitFromString
contains
    function fromStringBasicC(string, errors) result(mass)
        use erloff, only: ErrorList_t, Module_, Procedure_
        use iso_varying_string, only: var_str

        character(len=*), intent(in) :: string
        type(ErrorList_t), intent(out) :: errors
        type(Mass_t) :: mass

        type(ErrorList_t) :: errors_

        mass = massFromString( &
                var_str(string), PROVIDED_UNITS, errors_)
        call errors%appendErrors( &
                errors_, &
                Module_("Mass_m"), &
                Procedure_("fromStringBasicC"))
    end function fromStringBasicC

    function fromStringBasicS(string, errors) result(mass)
        use erloff, only: ErrorList_t, Module_, Procedure_
        use iso_varying_string, only: VARYING_STRING

        type(VARYING_STRING), intent(in) :: string
        type(ErrorList_t), intent(out) :: errors
        type(Mass_t) :: mass

        type(ErrorList_t) :: errors_

        mass = massFromString( &
                string, PROVIDED_UNITS, errors_)
        call errors%appendErrors( &
                errors_, &
                Module_("Mass_m"), &
                Procedure_("fromStringBasicS"))
    end function fromStringBasicS

    function fromStringWithUnitsC(string, units, errors) result(mass)
        use erloff, only: ErrorList_t, Module_, Procedure_
        use iso_varying_string, only: var_str

        character(len=*), intent(in) :: string
        type(MassUnit_t), intent(in) :: units(:)
        type(ErrorList_t), intent(out) :: errors
        type(Mass_t) :: mass

        type(ErrorList_t) :: errors_

        mass = massFromString( &
                var_str(string), units, errors_)
        call errors%appendErrors( &
                errors_, &
                Module_("Mass_m"), &
                Procedure_("fromStringWithUnitsC"))
    end function fromStringWithUnitsC

    function fromStringWithUnitsS(string, units, errors) result(mass)
        use erloff, only: ErrorList_t, Fatal, Module_, Procedure_
        use iso_varying_string, only: &
                VARYING_STRING, &
                assignment(=), &
                operator(//), &
                operator(==), &
                len, &
                split
        use Miscellaneous_m, only: PARSE_ERROR
        use strff, only: join

        type(VARYING_STRING), intent(in) :: string
        type(MassUnit_t), intent(in) :: units(:)
        type(ErrorList_t), intent(out) :: errors
        type(Mass_t) :: mass

        double precision :: number
        character(len=100) :: number_chars
        type(VARYING_STRING) :: number_string
        integer :: status
        type(VARYING_STRING) :: symbol
        type(MassUnit_t) :: unit
        type(ErrorList_t) :: unit_errors

        mass%kilograms = 0.0d0
        symbol = string
        call split(symbol, number_string, " ")
        if (len(symbol) == 0) then
            call errors%appendError(Fatal( &
                    PARSE_ERROR, &
                    Module_("Mass_m"), &
                    Procedure_("fromStringWithUnitsS"), &
                    'No unit symbol found in string "' // string // '"'))
            return
        end if
        number_chars = number_string
        read(number_chars, *, iostat=status) number
        if (status /= 0) then
            call errors%appendError(Fatal( &
                    PARSE_ERROR, &
                    Module_("Mass_m"), &
                    Procedure_("fromStringWithUnitsS"), &
                    'Error parsing number from string "' // number_string // '"'))
        end if
        unit = massUnitFromString(symbol, units, unit_errors)
        mass = number.unit.unit
        call errors%appendErrors( &
                unit_errors, &
                Module_("Mass_m"), &
                Procedure_("fromStringWithUnitsS"))
    end function fromStringWithUnitsS

    elemental function fromUnits(value_, units) result(mass)
        double precision, intent(in) :: value_
        type(MassUnit_t), intent(in) :: units
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
        use Miscellaneous_m, only: operator(.safeEq.)

        class(Mass_t), intent(in) :: lhs
        class(Mass_t), intent(in) :: rhs
        logical :: equal_

        equal_ = lhs%kilograms .safeEq. rhs%kilograms
    end function equal_

    function equalWithinAbsolute(lhs, rhs, within)
        use Miscellaneous_m, only: equalWithinAbsolute_ => equalWithinAbsolute

        class(Mass_t), intent(in) :: lhs
        class(Mass_t), intent(in) :: rhs
        class(Mass_t), intent(in) :: within
        logical :: equalWithinAbsolute

        equalWithinAbsolute = equalWithinAbsolute_( &
                lhs%kilograms, rhs%kilograms, within%kilograms)
    end function equalWithinAbsolute

    function equalWithinRelative(lhs, rhs, within)
        use Miscellaneous_m, only: equalWithinRelative_ => equalWithinRelative

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

    function toStringFullPrecision(self) result(string)
        use iso_varying_string, only: VARYING_STRING

        class(Mass_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = self%toStringIn(DEFAULT_OUTPUT_UNITS)
    end function toStringFullPrecision

    function toStringWithPrecision(self, significant_digits) result(string)
        use iso_varying_string, only: VARYING_STRING

        class(Mass_t), intent(in) :: self
        integer, intent(in) :: significant_digits
        type(VARYING_STRING) :: string

        string = self%toStringIn(DEFAULT_OUTPUT_UNITS, significant_digits)
    end function toStringWithPrecision

    function toStringInFullPrecision(self, units) result(string)
        use iso_varying_string, only: VARYING_STRING, operator(//)
        use strff, only: toString

        class(Mass_t), intent(in) :: self
        class(MassUnit_t), intent(in) :: units
        type(VARYING_STRING) :: string

        string = toString(self.in.units) // " " // units%toString()
    end function toStringInFullPrecision

    function toStringInWithPrecision( &
            self, units, significant_digits) result(string)
        use iso_varying_string, only: VARYING_STRING, operator(//)
        use strff, only: toString

        class(Mass_t), intent(in) :: self
        class(MassUnit_t), intent(in) :: units
        integer, intent(in) :: significant_digits
        type(VARYING_STRING) :: string

        string = &
                toString(self.in.units, significant_digits) &
                // " " // units%toString()
    end function toStringInWithPrecision

    function toGnuplotStringFullPrecision(self) result(string)
        use iso_varying_string, only: VARYING_STRING

        class(Mass_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = self%toGnuplotStringIn(DEFAULT_OUTPUT_UNITS)
    end function toGnuplotStringFullPrecision

    function toGnuplotStringWithPrecision( &
            self, significant_digits) result(string)
        use iso_varying_string, only: VARYING_STRING

        class(Mass_t), intent(in) :: self
        integer, intent(in) :: significant_digits
        type(VARYING_STRING) :: string

        string = self%toGnuplotStringIn( &
                DEFAULT_OUTPUT_UNITS, significant_digits)
    end function toGnuplotStringWithPrecision

    function toGnuplotStringInFullPrecision(self, units) result(string)
        use iso_varying_string, only: VARYING_STRING, operator(//)
        use strff, only: toString

        class(Mass_t), intent(in) :: self
        class(MassUnit_t), intent(in) :: units
        type(VARYING_STRING) :: string

        string = toString(self.in.units) // " " // units%toGnuplotString()
    end function toGnuplotStringInFullPrecision

    function toGnuplotStringInWithPrecision( &
            self, units, significant_digits) result(string)
        use iso_varying_string, only: VARYING_STRING, operator(//)
        use strff, only: toString

        class(Mass_t), intent(in) :: self
        class(MassUnit_t), intent(in) :: units
        integer, intent(in) :: significant_digits
        type(VARYING_STRING) :: string

        string = &
                toString(self.in.units, significant_digits) &
                // " " // units%toGnuplotString()
    end function toGnuplotStringInWithPrecision

    function toLatexStringFullPrecision(self) result(string)
        use iso_varying_string, only: VARYING_STRING

        class(Mass_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = self%toLatexStringIn(DEFAULT_OUTPUT_UNITS)
    end function toLatexStringFullPrecision

    function toLatexStringWithPrecision(self, significant_digits) result(string)
        use iso_varying_string, only: VARYING_STRING

        class(Mass_t), intent(in) :: self
        integer, intent(in) :: significant_digits
        type(VARYING_STRING) :: string

        string = self%toLatexStringIn(DEFAULT_OUTPUT_UNITS, significant_digits)
    end function toLatexStringWithPrecision

    function toLatexStringInFullPrecision(self, units) result(string)
        use iso_varying_string, only: VARYING_STRING, operator(//)
        use Miscellaneous_m, only: wrapInLatexQuantity
        use strff, only: toString

        class(Mass_t), intent(in) :: self
        class(MassUnit_t), intent(in) :: units
        type(VARYING_STRING) :: string

        string = wrapInLatexQuantity( &
                toString(self.in.units), units%toLatexString())
    end function toLatexStringInFullPrecision

    function toLatexStringInWithPrecision( &
            self, units, significant_digits) result(string)
        use iso_varying_string, only: VARYING_STRING, operator(//)
        use Miscellaneous_m, only: wrapInLatexQuantity
        use strff, only: toString

        class(Mass_t), intent(in) :: self
        class(MassUnit_t), intent(in) :: units
        integer, intent(in) :: significant_digits
        type(VARYING_STRING) :: string

        string = wrapInLatexQuantity( &
                toString(self.in.units, significant_digits), &
                units%toLatexString())
    end function toLatexStringInWithPrecision

    function unitFromStringBasicC(string, errors) result(unit)
        use erloff, only: ErrorList_t, Module_, Procedure_
        use iso_varying_string, only: var_str

        character(len=*), intent(in) :: string
        type(ErrorList_t), intent(out) :: errors
        type(MassUnit_t) :: unit

        type(ErrorList_t) :: errors_

        unit = massUnitFromString( &
                var_str(string), PROVIDED_UNITS, errors_)
        call errors%appendErrors( &
                errors_, &
                Module_("Mass_m"), &
                Procedure_("unitFromStringBasicC"))
    end function unitFromStringBasicC

    function unitFromStringBasicS(string, errors) result(unit)
        use erloff, only: ErrorList_t, Module_, Procedure_
        use iso_varying_string, only: VARYING_STRING

        type(VARYING_STRING), intent(in) :: string
        type(ErrorList_t), intent(out) :: errors
        type(MassUnit_t) :: unit

        type(ErrorList_t) :: errors_

        unit = massUnitFromString( &
                string, PROVIDED_UNITS, errors_)
        call errors%appendErrors( &
                errors_, &
                Module_("Mass_m"), &
                Procedure_("unitFromStringBasicS"))
    end function unitFromStringBasicS

    function unitFromStringWithUnitsC(string, units, errors) result(unit)
        use erloff, only: ErrorList_t, Module_, Procedure_
        use iso_varying_string, only: var_str

        character(len=*), intent(in) :: string
        type(MassUnit_t), intent(in) :: units(:)
        type(ErrorList_t), intent(out) :: errors
        type(MassUnit_t) :: unit

        type(ErrorList_t) :: errors_

        unit = massUnitFromString(var_str(string), units, errors_)
        call errors%appendErrors( &
                errors_, &
                Module_("Mass_m"), &
                Procedure_("unitFromStringWithUnitsC"))
    end function unitFromStringWithUnitsC

    function unitFromStringWithUnitsS(string, units, errors) result(unit)
        use erloff, only: ErrorList_t, Fatal, Module_, Procedure_
        use iso_varying_string, only: VARYING_STRING, operator(==), operator(//)
        use Miscellaneous_m, only: UNKNOWN_UNIT
        use strff, only: join

        type(VARYING_STRING), intent(in) :: string
        type(MassUnit_t), intent(in) :: units(:)
        type(ErrorList_t), intent(out) :: errors
        type(MassUnit_t) :: unit

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
                    Module_("Mass_m"), &
                    Procedure_("unitFromStringWithUnitsS"), &
                    '"' // string // '", known units: [' // join(unit_strings, ', ') // ']' ))
        end if
    end function unitFromStringWithUnitsS

    function unitToString(self) result(string)
        use iso_varying_string, only: VARYING_STRING, assignment(=)

        class(MassUnit_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = trim(self%symbol)
    end function unitToString

    function unitToGnuplotString(self) result(string)
        use iso_varying_string, only: VARYING_STRING, assignment(=)

        class(MassUnit_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = trim(self%gnuplot_symbol)
    end function unitToGnuplotString

    function unitToLatexString(self) result(string)
        use iso_varying_string, only: VARYING_STRING, assignment(=)

        class(MassUnit_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = trim(self%latex_symbol)
    end function unitToLatexString
end module Mass_m
