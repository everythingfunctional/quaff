module Burnup_m
    use Conversion_factors_m, only: MEGAWATT_DAYS_PER_TON_PER_WATT_SECONDS_PER_KILOGRAM
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

    type, public :: Burnup_t
        double precision :: watt_seconds_per_kilogram
    contains
        private
        procedure :: toUnits
        generic, public :: operator(.in.) => toUnits
        procedure, pass(burnup) :: doubleTimesBurnup
        procedure, pass(burnup) :: integerTimesBurnup
        procedure, pass(burnup) :: burnupTimesDouble
        procedure, pass(burnup) :: burnupTimesInteger
        generic, public :: operator(*) => &
                doubleTimesBurnup, &
                integerTimesBurnup, &
                burnupTimesDouble, &
                burnupTimesInteger
        procedure :: burnupDividedByDouble
        procedure :: burnupDividedByInteger
        procedure, pass(numerator) :: burnupDividedByBurnup
        generic, public :: operator(/) => &
                burnupDividedByDouble, &
                burnupDividedByInteger, &
                burnupDividedByBurnup
        procedure :: burnupPlusBurnup
        generic, public :: operator(+) => burnupPlusBurnup
        procedure :: burnupMinusBurnup
        generic, public :: operator(-) => burnupMinusBurnup
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
    end type Burnup_t

    type, public :: BurnupUnit_t
        double precision :: conversion_factor
        character(len=10) :: symbol
        character(len=50) :: gnuplot_symbol
        character(len=100) :: latex_symbol
    contains
        procedure :: toString => unitToString
        procedure :: toGnuplotString => unitToGnuplotString
        procedure :: toLatexString => unitToLatexString
    end type BurnupUnit_t

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
        module procedure sumBurnup
    end interface sum

    type(BurnupUnit_t), parameter, public :: MEGAWATT_DAYS_PER_TON = &
            BurnupUnit_t( &
                    conversion_factor = MEGAWATT_DAYS_PER_TON_PER_WATT_SECONDS_PER_KILOGRAM, &
                    symbol = "(MW s)/kg", &
                    gnuplot_symbol = "(MW s)/kg", &
                    latex_symbol = "\mega\watt\day\per\ton")
    type(BurnupUnit_t), parameter, public :: WATT_SECONDS_PER_KILOGRAM = &
            BurnupUnit_t( &
                    conversion_factor = 1.0d0, &
                    symbol = "(W s)/kg", &
                    gnuplot_symbol = "(W s)/kg", &
                    latex_symbol = "\watt\second\per\kilo\gram")

    type(BurnupUnit_t), public :: DEFAULT_OUTPUT_UNITS = WATT_SECONDS_PER_KILOGRAM

    type(BurnupUnit_t), parameter, public :: PROVIDED_UNITS(*) = &
            [MEGAWATT_DAYS_PER_TON, WATT_SECONDS_PER_KILOGRAM]

    public :: operator(.unit.), fromString, sum
contains
    pure subroutine fromStringBasicC(string, errors, burnup)
        character(len=*), intent(in) :: string
        type(ErrorList_t), intent(out) :: errors
        type(Burnup_t), intent(out) :: burnup

        type(ErrorList_t) :: errors_

        call fromString( &
                var_str(string), PROVIDED_UNITS, errors_, burnup)
        call errors%appendErrors( &
                errors_, &
                Module_("Burnup_m"), &
                Procedure_("fromStringBasicC"))
    end subroutine fromStringBasicC

    pure subroutine fromStringBasicS(string, errors, burnup)
        type(VARYING_STRING), intent(in) :: string
        type(ErrorList_t), intent(out) :: errors
        type(Burnup_t), intent(out) :: burnup

        type(ErrorList_t) :: errors_

        call fromString( &
                string, PROVIDED_UNITS, errors_, burnup)
        call errors%appendErrors( &
                errors_, &
                Module_("Burnup_m"), &
                Procedure_("fromStringBasicS"))
    end subroutine fromStringBasicS

    pure subroutine fromStringWithUnitsC(string, units, errors, burnup)
        character(len=*), intent(in) :: string
        type(BurnupUnit_t), intent(in) :: units(:)
        type(ErrorList_t), intent(out) :: errors
        type(Burnup_t), intent(out) :: burnup

        type(ErrorList_t) :: errors_

        call fromString( &
                var_str(string), units, errors_, burnup)
        call errors%appendErrors( &
                errors_, &
                Module_("Burnup_m"), &
                Procedure_("fromStringWithUnitsC"))
    end subroutine fromStringWithUnitsC

    pure subroutine fromStringWithUnitsS(string, units, errors, burnup)
        type(VARYING_STRING), intent(in) :: string
        type(BurnupUnit_t), intent(in) :: units(:)
        type(ErrorList_t), intent(out) :: errors
        type(Burnup_t), intent(out) :: burnup

        double precision :: number
        character(len=100) :: number_chars
        type(VARYING_STRING) :: number_string
        integer :: status
        type(VARYING_STRING) :: symbol
        type(BurnupUnit_t) :: unit
        type(ErrorList_t) :: unit_errors

        burnup%watt_seconds_per_kilogram = 0.0d0
        symbol = string
        call split(symbol, number_string, " ")
        if (len(symbol) == 0) then
            call errors%appendError(Fatal( &
                    PARSE_ERROR, &
                    Module_("Burnup_m"), &
                    Procedure_("fromStringWithUnitsS"), &
                    'No unit symbol found in string "' // string // '"'))
            return
        end if
        number_chars = number_string
        read(number_chars, *, iostat=status) number
        if (status /= 0) then
            call errors%appendError(Fatal( &
                    PARSE_ERROR, &
                    Module_("Burnup_m"), &
                    Procedure_("fromStringWithUnitsS"), &
                    'Error parsing number from string "' // number_string // '"'))
        end if
        call fromString(symbol, units, unit_errors, unit)
        burnup = number.unit.unit
        call errors%appendErrors( &
                unit_errors, &
                Module_("Burnup_m"), &
                Procedure_("fromStringWithUnitsS"))
    end subroutine fromStringWithUnitsS

    elemental function fromUnits(value_, units) result(burnup)
        double precision, intent(in) :: value_
        type(BurnupUnit_t), intent(in) :: units
        type(Burnup_t) :: burnup

        burnup%watt_seconds_per_kilogram = value_ / units%conversion_factor
    end function fromUnits

    elemental function toUnits(self, units) result(burnup)
        class(Burnup_t), intent(in) :: self
        class(BurnupUnit_t), intent(in) :: units
        double precision :: burnup

        burnup = self%watt_seconds_per_kilogram * units%conversion_factor
    end function toUnits

    elemental function doubleTimesBurnup( &
            multiplier, burnup) result(new_burnup)
        double precision, intent(in) :: multiplier
        class(Burnup_t), intent(in) :: burnup
        type(Burnup_t) :: new_burnup

        new_burnup%watt_seconds_per_kilogram = &
                multiplier * burnup%watt_seconds_per_kilogram
    end function doubleTimesBurnup

    elemental function integerTimesBurnup( &
            multiplier, burnup) result(new_burnup)
        integer, intent(in) :: multiplier
        class(Burnup_t), intent(in) :: burnup
        type(Burnup_t) :: new_burnup

        new_burnup%watt_seconds_per_kilogram = &
                dble(multiplier) * burnup%watt_seconds_per_kilogram
    end function integerTimesBurnup

    elemental function burnupTimesDouble( &
            burnup, multiplier) result(new_burnup)
        class(Burnup_t), intent(in) :: burnup
        double precision, intent(in) :: multiplier
        type(Burnup_t) :: new_burnup

        new_burnup%watt_seconds_per_kilogram = &
                burnup%watt_seconds_per_kilogram * multiplier
    end function burnupTimesDouble

    elemental function burnupTimesInteger( &
            burnup, multiplier) result(new_burnup)
        class(Burnup_t), intent(in) :: burnup
        integer, intent(in) :: multiplier
        type(Burnup_t) :: new_burnup

        new_burnup%watt_seconds_per_kilogram = &
                burnup%watt_seconds_per_kilogram * dble(multiplier)
    end function burnupTimesInteger

    elemental function burnupDividedByDouble( &
            burnup, divisor) result(new_burnup)
        class(Burnup_t), intent(in) :: burnup
        double precision, intent(in) :: divisor
        type(Burnup_t) :: new_burnup

        new_burnup%watt_seconds_per_kilogram = &
                burnup%watt_seconds_per_kilogram / divisor
    end function burnupDividedByDouble

    elemental function burnupDividedByInteger( &
            burnup, divisor) result(new_burnup)
        class(Burnup_t), intent(in) :: burnup
        integer, intent(in) :: divisor
        type(Burnup_t) :: new_burnup

        new_burnup%watt_seconds_per_kilogram = &
                burnup%watt_seconds_per_kilogram / dble(divisor)
    end function burnupDividedByInteger

    elemental function burnupDividedByBurnup( &
            numerator, denomenator) result(ratio)
        class(Burnup_t), intent(in) :: numerator
        class(Burnup_t), intent(in) :: denomenator
        double precision :: ratio

        ratio = numerator%watt_seconds_per_kilogram / denomenator%watt_seconds_per_kilogram
    end function burnupDividedByBurnup

    elemental function burnupPlusBurnup( &
            burnup1, burnup2) result(new_burnup)
        class(Burnup_t), intent(in) :: burnup1
        class(Burnup_t), intent(in) :: burnup2
        type(Burnup_t) :: new_burnup

        new_burnup%watt_seconds_per_kilogram = &
                burnup1%watt_seconds_per_kilogram + burnup2%watt_seconds_per_kilogram
    end function burnupPlusBurnup

    elemental function burnupMinusBurnup( &
            burnup1, burnup2) result(new_burnup)
        class(Burnup_t), intent(in) :: burnup1
        class(Burnup_t), intent(in) :: burnup2
        type(Burnup_t) :: new_burnup

        new_burnup%watt_seconds_per_kilogram = &
                burnup1%watt_seconds_per_kilogram - burnup2%watt_seconds_per_kilogram
    end function burnupMinusBurnup

    pure function sumBurnup(burnups)
        type(Burnup_t), intent(in) :: burnups(:)
        type(Burnup_t) :: sumBurnup

        sumBurnup%watt_seconds_per_kilogram = sum(burnups%watt_seconds_per_kilogram)
    end function sumBurnup

    elemental function greaterThan(lhs, rhs)
        class(Burnup_t), intent(in) :: lhs
        class(Burnup_t), intent(in) :: rhs
        logical :: greaterThan

        greaterThan = lhs%watt_seconds_per_kilogram > rhs%watt_seconds_per_kilogram
    end function greaterThan

    elemental function lessThan(lhs,rhs)
        class(Burnup_t), intent(in) :: lhs
        class(Burnup_t), intent(in) :: rhs
        logical :: lessThan

        lessThan = lhs%watt_seconds_per_kilogram < rhs%watt_seconds_per_kilogram
    end function lessThan

    elemental function greaterThanOrEqual(lhs, rhs)
        class(Burnup_t), intent(in) :: lhs
        class(Burnup_t), intent(in) :: rhs
        logical :: greaterThanOrEqual

        greaterThanOrEqual = lhs%watt_seconds_per_kilogram >= rhs%watt_seconds_per_kilogram
    end function greaterThanOrEqual

    elemental function lessThanOrEqual(lhs, rhs)
        class(Burnup_t), intent(in) :: lhs
        class(Burnup_t), intent(in) :: rhs
        logical :: lessThanOrEqual

        lessThanOrEqual = lhs%watt_seconds_per_kilogram <= rhs%watt_seconds_per_kilogram
    end function lessThanOrEqual

    elemental function equal_(lhs,rhs)
        class(Burnup_t), intent(in) :: lhs
        class(Burnup_t), intent(in) :: rhs
        logical :: equal_

        equal_ = lhs%watt_seconds_per_kilogram .safeEq. rhs%watt_seconds_per_kilogram
    end function equal_

    elemental function equalWithinAbsolute(lhs, rhs, within)
        class(Burnup_t), intent(in) :: lhs
        class(Burnup_t), intent(in) :: rhs
        class(Burnup_t), intent(in) :: within
        logical :: equalWithinAbsolute

        equalWithinAbsolute = equalWithinAbsolute_( &
                lhs%watt_seconds_per_kilogram, rhs%watt_seconds_per_kilogram, within%watt_seconds_per_kilogram)
    end function equalWithinAbsolute

    elemental function equalWithinRelative(lhs, rhs, within)
        class(Burnup_t), intent(in) :: lhs
        class(Burnup_t), intent(in) :: rhs
        double precision, intent(in) :: within
        logical :: equalWithinRelative

        equalWithinRelative = equalWithinRelative_( &
                lhs%watt_seconds_per_kilogram, rhs%watt_seconds_per_kilogram, within)
    end function equalWithinRelative

    elemental function notEqual(lhs, rhs)
        class(Burnup_t), intent(in) :: lhs
        class(Burnup_t), intent(in) :: rhs
        logical :: notEqual

        notEqual = .not. lhs == rhs
    end function notEqual

    elemental function toStringFullPrecision(self) result(string)
        class(Burnup_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = self%toStringIn(DEFAULT_OUTPUT_UNITS)
    end function toStringFullPrecision

    elemental function toStringWithPrecision(self, significant_digits) result(string)
        class(Burnup_t), intent(in) :: self
        integer, intent(in) :: significant_digits
        type(VARYING_STRING) :: string

        string = self%toStringIn(DEFAULT_OUTPUT_UNITS, significant_digits)
    end function toStringWithPrecision

    elemental function toStringInFullPrecision(self, units) result(string)
        class(Burnup_t), intent(in) :: self
        class(BurnupUnit_t), intent(in) :: units
        type(VARYING_STRING) :: string

        string = toString(self.in.units) // " " // units%toString()
    end function toStringInFullPrecision

    elemental function toStringInWithPrecision( &
            self, units, significant_digits) result(string)
        class(Burnup_t), intent(in) :: self
        class(BurnupUnit_t), intent(in) :: units
        integer, intent(in) :: significant_digits
        type(VARYING_STRING) :: string

        string = &
                toString(self.in.units, significant_digits) &
                // " " // units%toString()
    end function toStringInWithPrecision

    elemental function toGnuplotStringFullPrecision(self) result(string)
        class(Burnup_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = self%toGnuplotStringIn(DEFAULT_OUTPUT_UNITS)
    end function toGnuplotStringFullPrecision

    elemental function toGnuplotStringWithPrecision( &
            self, significant_digits) result(string)
        class(Burnup_t), intent(in) :: self
        integer, intent(in) :: significant_digits
        type(VARYING_STRING) :: string

        string = self%toGnuplotStringIn( &
                DEFAULT_OUTPUT_UNITS, significant_digits)
    end function toGnuplotStringWithPrecision

    elemental function toGnuplotStringInFullPrecision(self, units) result(string)
        class(Burnup_t), intent(in) :: self
        class(BurnupUnit_t), intent(in) :: units
        type(VARYING_STRING) :: string

        string = toString(self.in.units) // " " // units%toGnuplotString()
    end function toGnuplotStringInFullPrecision

    elemental function toGnuplotStringInWithPrecision( &
            self, units, significant_digits) result(string)
        class(Burnup_t), intent(in) :: self
        class(BurnupUnit_t), intent(in) :: units
        integer, intent(in) :: significant_digits
        type(VARYING_STRING) :: string

        string = &
                toString(self.in.units, significant_digits) &
                // " " // units%toGnuplotString()
    end function toGnuplotStringInWithPrecision

    elemental function toLatexStringFullPrecision(self) result(string)
        class(Burnup_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = self%toLatexStringIn(DEFAULT_OUTPUT_UNITS)
    end function toLatexStringFullPrecision

    elemental function toLatexStringWithPrecision(self, significant_digits) result(string)
        class(Burnup_t), intent(in) :: self
        integer, intent(in) :: significant_digits
        type(VARYING_STRING) :: string

        string = self%toLatexStringIn(DEFAULT_OUTPUT_UNITS, significant_digits)
    end function toLatexStringWithPrecision

    elemental function toLatexStringInFullPrecision(self, units) result(string)
        class(Burnup_t), intent(in) :: self
        class(BurnupUnit_t), intent(in) :: units
        type(VARYING_STRING) :: string

        string = wrapInLatexQuantity( &
                toString(self.in.units), units%toLatexString())
    end function toLatexStringInFullPrecision

    elemental function toLatexStringInWithPrecision( &
            self, units, significant_digits) result(string)
        class(Burnup_t), intent(in) :: self
        class(BurnupUnit_t), intent(in) :: units
        integer, intent(in) :: significant_digits
        type(VARYING_STRING) :: string

        string = wrapInLatexQuantity( &
                toString(self.in.units, significant_digits), &
                units%toLatexString())
    end function toLatexStringInWithPrecision

    pure subroutine unitFromStringBasicC(string, errors, unit)
        character(len=*), intent(in) :: string
        type(ErrorList_t), intent(out) :: errors
        type(BurnupUnit_t), intent(out) :: unit

        type(ErrorList_t) :: errors_

        call fromString( &
                var_str(string), PROVIDED_UNITS, errors_, unit)
        call errors%appendErrors( &
                errors_, &
                Module_("Burnup_m"), &
                Procedure_("unitFromStringBasicC"))
    end subroutine unitFromStringBasicC

    pure subroutine unitFromStringBasicS(string, errors, unit)
        type(VARYING_STRING), intent(in) :: string
        type(ErrorList_t), intent(out) :: errors
        type(BurnupUnit_t), intent(out) :: unit

        type(ErrorList_t) :: errors_

        call fromString( &
                string, PROVIDED_UNITS, errors_, unit)
        call errors%appendErrors( &
                errors_, &
                Module_("Burnup_m"), &
                Procedure_("unitFromStringBasicS"))
    end subroutine unitFromStringBasicS

    pure subroutine unitFromStringWithUnitsC(string, units, errors, unit)
        character(len=*), intent(in) :: string
        type(BurnupUnit_t), intent(in) :: units(:)
        type(ErrorList_t), intent(out) :: errors
        type(BurnupUnit_t), intent(out) :: unit

        type(ErrorList_t) :: errors_

        call fromString(var_str(string), units, errors_, unit)
        call errors%appendErrors( &
                errors_, &
                Module_("Burnup_m"), &
                Procedure_("unitFromStringWithUnitsC"))
    end subroutine unitFromStringWithUnitsC

    pure subroutine unitFromStringWithUnitsS(string, units, errors, unit)
        type(VARYING_STRING), intent(in) :: string
        type(BurnupUnit_t), intent(in) :: units(:)
        type(ErrorList_t), intent(out) :: errors
        type(BurnupUnit_t), intent(out) :: unit

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
                    Module_("Burnup_m"), &
                    Procedure_("unitFromStringWithUnitsS"), &
                    '"' // string // '", known units: [' // join(unit_strings, ', ') // ']' ))
        end if
    end subroutine unitFromStringWithUnitsS

    elemental function unitToString(self) result(string)
        class(BurnupUnit_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = trim(self%symbol)
    end function unitToString

    elemental function unitToGnuplotString(self) result(string)
        class(BurnupUnit_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = trim(self%gnuplot_symbol)
    end function unitToGnuplotString

    elemental function unitToLatexString(self) result(string)
        class(BurnupUnit_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = trim(self%latex_symbol)
    end function unitToLatexString
end module Burnup_m
