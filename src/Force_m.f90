module Force_m
    use Conversion_factors_m, only: &
            DYNES_PER_NEWTON, &
            KILOPONDS_PER_NEWTON, &
            MILLINEWTONS_PER_NEWTON, &
            POUNDS_PER_NEWTON
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

    type, public :: Force_t
        double precision :: newtons
    contains
        private
        procedure :: toUnits
        generic, public :: operator(.in.) => toUnits
        procedure, pass(force) :: doubleTimesForce
        procedure, pass(force) :: integerTimesForce
        procedure, pass(force) :: forceTimesDouble
        procedure, pass(force) :: forceTimesInteger
        generic, public :: operator(*) => &
                doubleTimesForce, &
                integerTimesForce, &
                forceTimesDouble, &
                forceTimesInteger
        procedure :: forceDividedByDouble
        procedure :: forceDividedByInteger
        procedure, pass(numerator) :: forceDividedByForce
        generic, public :: operator(/) => &
                forceDividedByDouble, &
                forceDividedByInteger, &
                forceDividedByForce
        procedure :: forcePlusForce
        generic, public :: operator(+) => forcePlusForce
        procedure :: forceMinusForce
        generic, public :: operator(-) => forceMinusForce
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
    end type Force_t

    type, public :: ForceUnit_t
        double precision :: conversion_factor
        character(len=10) :: symbol
        character(len=50) :: gnuplot_symbol
        character(len=100) :: latex_symbol
    contains
        procedure :: toString => unitToString
        procedure :: toGnuplotString => unitToGnuplotString
        procedure :: toLatexString => unitToLatexString
    end type ForceUnit_t

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
        module procedure sumForce
    end interface sum

    type(ForceUnit_t), parameter, public :: DYNES = &
            ForceUnit_t( &
                    conversion_factor = DYNES_PER_NEWTON, &
                    symbol = "dyn", &
                    gnuplot_symbol = "dyn", &
                    latex_symbol = "\dyne")
    type(ForceUnit_t), parameter, public :: KILOPONDS = &
            ForceUnit_t( &
                    conversion_factor = KILOPONDS_PER_NEWTON, &
                    symbol = "kp", &
                    gnuplot_symbol = "kp", &
                    latex_symbol = "\kilopond")
    type(ForceUnit_t), parameter, public :: MILLI_NEWTONS = &
            ForceUnit_t( &
                    conversion_factor = MILLINEWTONS_PER_NEWTON, &
                    symbol = "mN", &
                    gnuplot_symbol = "mN", &
                    latex_symbol = "\newton")
    type(ForceUnit_t), parameter, public :: NEWTONS = &
            ForceUnit_t( &
                    conversion_factor = 1.0d0, &
                    symbol = "N", &
                    gnuplot_symbol = "N", &
                    latex_symbol = "\newton")
    type(ForceUnit_t), parameter, public :: POUNDS_FORCE = &
            ForceUnit_t( &
                    conversion_factor = POUNDS_PER_NEWTON, &
                    symbol = "lbf", &
                    gnuplot_symbol = "lbf", &
                    latex_symbol = "\poundforce")

    type(ForceUnit_t), public :: DEFAULT_OUTPUT_UNITS = NEWTONS

    type(ForceUnit_t), parameter, public :: PROVIDED_UNITS(*) = &
            [DYNES, KILOPONDS, MILLI_NEWTONS, NEWTONS, POUNDS_FORCE]

    public :: operator(.unit.), fromString, sum
contains
    pure subroutine fromStringBasicC(string, errors, force)
        character(len=*), intent(in) :: string
        type(ErrorList_t), intent(out) :: errors
        type(Force_t), intent(out) :: force

        type(ErrorList_t) :: errors_

        call fromString( &
                var_str(string), PROVIDED_UNITS, errors_, force)
        call errors%appendErrors( &
                errors_, &
                Module_("Force_m"), &
                Procedure_("fromStringBasicC"))
    end subroutine fromStringBasicC

    pure subroutine fromStringBasicS(string, errors, force)
        type(VARYING_STRING), intent(in) :: string
        type(ErrorList_t), intent(out) :: errors
        type(Force_t), intent(out) :: force

        type(ErrorList_t) :: errors_

        call fromString( &
                string, PROVIDED_UNITS, errors_, force)
        call errors%appendErrors( &
                errors_, &
                Module_("Force_m"), &
                Procedure_("fromStringBasicS"))
    end subroutine fromStringBasicS

    pure subroutine fromStringWithUnitsC(string, units, errors, force)
        character(len=*), intent(in) :: string
        type(ForceUnit_t), intent(in) :: units(:)
        type(ErrorList_t), intent(out) :: errors
        type(Force_t), intent(out) :: force

        type(ErrorList_t) :: errors_

        call fromString( &
                var_str(string), units, errors_, force)
        call errors%appendErrors( &
                errors_, &
                Module_("Force_m"), &
                Procedure_("fromStringWithUnitsC"))
    end subroutine fromStringWithUnitsC

    pure subroutine fromStringWithUnitsS(string, units, errors, force)
        type(VARYING_STRING), intent(in) :: string
        type(ForceUnit_t), intent(in) :: units(:)
        type(ErrorList_t), intent(out) :: errors
        type(Force_t), intent(out) :: force

        double precision :: number
        character(len=100) :: number_chars
        type(VARYING_STRING) :: number_string
        integer :: status
        type(VARYING_STRING) :: symbol
        type(ForceUnit_t) :: unit
        type(ErrorList_t) :: unit_errors

        force%newtons = 0.0d0
        symbol = string
        call split(symbol, number_string, " ")
        if (len(symbol) == 0) then
            call errors%appendError(Fatal( &
                    PARSE_ERROR, &
                    Module_("Force_m"), &
                    Procedure_("fromStringWithUnitsS"), &
                    'No unit symbol found in string "' // string // '"'))
            return
        end if
        number_chars = number_string
        read(number_chars, *, iostat=status) number
        if (status /= 0) then
            call errors%appendError(Fatal( &
                    PARSE_ERROR, &
                    Module_("Force_m"), &
                    Procedure_("fromStringWithUnitsS"), &
                    'Error parsing number from string "' // number_string // '"'))
        end if
        call fromString(symbol, units, unit_errors, unit)
        force = number.unit.unit
        call errors%appendErrors( &
                unit_errors, &
                Module_("Force_m"), &
                Procedure_("fromStringWithUnitsS"))
    end subroutine fromStringWithUnitsS

    elemental function fromUnits(value_, units) result(force)
        double precision, intent(in) :: value_
        type(ForceUnit_t), intent(in) :: units
        type(Force_t) :: force

        force%newtons = value_ / units%conversion_factor
    end function fromUnits

    elemental function toUnits(self, units) result(force)
        class(Force_t), intent(in) :: self
        class(ForceUnit_t), intent(in) :: units
        double precision :: force

        force = self%newtons * units%conversion_factor
    end function toUnits

    elemental function doubleTimesForce( &
            multiplier, force) result(new_force)
        double precision, intent(in) :: multiplier
        class(Force_t), intent(in) :: force
        type(Force_t) :: new_force

        new_force%newtons = &
                multiplier * force%newtons
    end function doubleTimesForce

    elemental function integerTimesForce( &
            multiplier, force) result(new_force)
        integer, intent(in) :: multiplier
        class(Force_t), intent(in) :: force
        type(Force_t) :: new_force

        new_force%newtons = &
                dble(multiplier) * force%newtons
    end function integerTimesForce

    elemental function forceTimesDouble( &
            force, multiplier) result(new_force)
        class(Force_t), intent(in) :: force
        double precision, intent(in) :: multiplier
        type(Force_t) :: new_force

        new_force%newtons = &
                force%newtons * multiplier
    end function forceTimesDouble

    elemental function forceTimesInteger( &
            force, multiplier) result(new_force)
        class(Force_t), intent(in) :: force
        integer, intent(in) :: multiplier
        type(Force_t) :: new_force

        new_force%newtons = &
                force%newtons * dble(multiplier)
    end function forceTimesInteger

    elemental function forceDividedByDouble( &
            force, divisor) result(new_force)
        class(Force_t), intent(in) :: force
        double precision, intent(in) :: divisor
        type(Force_t) :: new_force

        new_force%newtons = &
                force%newtons / divisor
    end function forceDividedByDouble

    elemental function forceDividedByInteger( &
            force, divisor) result(new_force)
        class(Force_t), intent(in) :: force
        integer, intent(in) :: divisor
        type(Force_t) :: new_force

        new_force%newtons = &
                force%newtons / dble(divisor)
    end function forceDividedByInteger

    elemental function forceDividedByForce( &
            numerator, denomenator) result(ratio)
        class(Force_t), intent(in) :: numerator
        class(Force_t), intent(in) :: denomenator
        double precision :: ratio

        ratio = numerator%newtons / denomenator%newtons
    end function forceDividedByForce

    elemental function forcePlusForce( &
            force1, force2) result(new_force)
        class(Force_t), intent(in) :: force1
        class(Force_t), intent(in) :: force2
        type(Force_t) :: new_force

        new_force%newtons = &
                force1%newtons + force2%newtons
    end function forcePlusForce

    elemental function forceMinusForce( &
            force1, force2) result(new_force)
        class(Force_t), intent(in) :: force1
        class(Force_t), intent(in) :: force2
        type(Force_t) :: new_force

        new_force%newtons = &
                force1%newtons - force2%newtons
    end function forceMinusForce

    pure function sumForce(forces)
        type(Force_t), intent(in) :: forces(:)
        type(Force_t) :: sumForce

        sumForce%newtons = sum(forces%newtons)
    end function sumForce

    elemental function greaterThan(lhs, rhs)
        class(Force_t), intent(in) :: lhs
        class(Force_t), intent(in) :: rhs
        logical :: greaterThan

        greaterThan = lhs%newtons > rhs%newtons
    end function greaterThan

    elemental function lessThan(lhs,rhs)
        class(Force_t), intent(in) :: lhs
        class(Force_t), intent(in) :: rhs
        logical :: lessThan

        lessThan = lhs%newtons < rhs%newtons
    end function lessThan

    elemental function greaterThanOrEqual(lhs, rhs)
        class(Force_t), intent(in) :: lhs
        class(Force_t), intent(in) :: rhs
        logical :: greaterThanOrEqual

        greaterThanOrEqual = lhs%newtons >= rhs%newtons
    end function greaterThanOrEqual

    elemental function lessThanOrEqual(lhs, rhs)
        class(Force_t), intent(in) :: lhs
        class(Force_t), intent(in) :: rhs
        logical :: lessThanOrEqual

        lessThanOrEqual = lhs%newtons <= rhs%newtons
    end function lessThanOrEqual

    elemental function equal_(lhs,rhs)
        class(Force_t), intent(in) :: lhs
        class(Force_t), intent(in) :: rhs
        logical :: equal_

        equal_ = lhs%newtons .safeEq. rhs%newtons
    end function equal_

    elemental function equalWithinAbsolute(lhs, rhs, within)
        class(Force_t), intent(in) :: lhs
        class(Force_t), intent(in) :: rhs
        class(Force_t), intent(in) :: within
        logical :: equalWithinAbsolute

        equalWithinAbsolute = equalWithinAbsolute_( &
                lhs%newtons, rhs%newtons, within%newtons)
    end function equalWithinAbsolute

    elemental function equalWithinRelative(lhs, rhs, within)
        class(Force_t), intent(in) :: lhs
        class(Force_t), intent(in) :: rhs
        double precision, intent(in) :: within
        logical :: equalWithinRelative

        equalWithinRelative = equalWithinRelative_( &
                lhs%newtons, rhs%newtons, within)
    end function equalWithinRelative

    elemental function notEqual(lhs, rhs)
        class(Force_t), intent(in) :: lhs
        class(Force_t), intent(in) :: rhs
        logical :: notEqual

        notEqual = .not. lhs == rhs
    end function notEqual

    elemental function toStringFullPrecision(self) result(string)
        class(Force_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = self%toStringIn(DEFAULT_OUTPUT_UNITS)
    end function toStringFullPrecision

    elemental function toStringWithPrecision(self, significant_digits) result(string)
        class(Force_t), intent(in) :: self
        integer, intent(in) :: significant_digits
        type(VARYING_STRING) :: string

        string = self%toStringIn(DEFAULT_OUTPUT_UNITS, significant_digits)
    end function toStringWithPrecision

    elemental function toStringInFullPrecision(self, units) result(string)
        class(Force_t), intent(in) :: self
        class(ForceUnit_t), intent(in) :: units
        type(VARYING_STRING) :: string

        string = toString(self.in.units) // " " // units%toString()
    end function toStringInFullPrecision

    elemental function toStringInWithPrecision( &
            self, units, significant_digits) result(string)
        class(Force_t), intent(in) :: self
        class(ForceUnit_t), intent(in) :: units
        integer, intent(in) :: significant_digits
        type(VARYING_STRING) :: string

        string = &
                toString(self.in.units, significant_digits) &
                // " " // units%toString()
    end function toStringInWithPrecision

    elemental function toGnuplotStringFullPrecision(self) result(string)
        class(Force_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = self%toGnuplotStringIn(DEFAULT_OUTPUT_UNITS)
    end function toGnuplotStringFullPrecision

    elemental function toGnuplotStringWithPrecision( &
            self, significant_digits) result(string)
        class(Force_t), intent(in) :: self
        integer, intent(in) :: significant_digits
        type(VARYING_STRING) :: string

        string = self%toGnuplotStringIn( &
                DEFAULT_OUTPUT_UNITS, significant_digits)
    end function toGnuplotStringWithPrecision

    elemental function toGnuplotStringInFullPrecision(self, units) result(string)
        class(Force_t), intent(in) :: self
        class(ForceUnit_t), intent(in) :: units
        type(VARYING_STRING) :: string

        string = toString(self.in.units) // " " // units%toGnuplotString()
    end function toGnuplotStringInFullPrecision

    elemental function toGnuplotStringInWithPrecision( &
            self, units, significant_digits) result(string)
        class(Force_t), intent(in) :: self
        class(ForceUnit_t), intent(in) :: units
        integer, intent(in) :: significant_digits
        type(VARYING_STRING) :: string

        string = &
                toString(self.in.units, significant_digits) &
                // " " // units%toGnuplotString()
    end function toGnuplotStringInWithPrecision

    elemental function toLatexStringFullPrecision(self) result(string)
        class(Force_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = self%toLatexStringIn(DEFAULT_OUTPUT_UNITS)
    end function toLatexStringFullPrecision

    elemental function toLatexStringWithPrecision(self, significant_digits) result(string)
        class(Force_t), intent(in) :: self
        integer, intent(in) :: significant_digits
        type(VARYING_STRING) :: string

        string = self%toLatexStringIn(DEFAULT_OUTPUT_UNITS, significant_digits)
    end function toLatexStringWithPrecision

    elemental function toLatexStringInFullPrecision(self, units) result(string)
        class(Force_t), intent(in) :: self
        class(ForceUnit_t), intent(in) :: units
        type(VARYING_STRING) :: string

        string = wrapInLatexQuantity( &
                toString(self.in.units), units%toLatexString())
    end function toLatexStringInFullPrecision

    elemental function toLatexStringInWithPrecision( &
            self, units, significant_digits) result(string)
        class(Force_t), intent(in) :: self
        class(ForceUnit_t), intent(in) :: units
        integer, intent(in) :: significant_digits
        type(VARYING_STRING) :: string

        string = wrapInLatexQuantity( &
                toString(self.in.units, significant_digits), &
                units%toLatexString())
    end function toLatexStringInWithPrecision

    pure subroutine unitFromStringBasicC(string, errors, unit)
        character(len=*), intent(in) :: string
        type(ErrorList_t), intent(out) :: errors
        type(ForceUnit_t), intent(out) :: unit

        type(ErrorList_t) :: errors_

        call fromString( &
                var_str(string), PROVIDED_UNITS, errors_, unit)
        call errors%appendErrors( &
                errors_, &
                Module_("Force_m"), &
                Procedure_("unitFromStringBasicC"))
    end subroutine unitFromStringBasicC

    pure subroutine unitFromStringBasicS(string, errors, unit)
        type(VARYING_STRING), intent(in) :: string
        type(ErrorList_t), intent(out) :: errors
        type(ForceUnit_t), intent(out) :: unit

        type(ErrorList_t) :: errors_

        call fromString( &
                string, PROVIDED_UNITS, errors_, unit)
        call errors%appendErrors( &
                errors_, &
                Module_("Force_m"), &
                Procedure_("unitFromStringBasicS"))
    end subroutine unitFromStringBasicS

    pure subroutine unitFromStringWithUnitsC(string, units, errors, unit)
        character(len=*), intent(in) :: string
        type(ForceUnit_t), intent(in) :: units(:)
        type(ErrorList_t), intent(out) :: errors
        type(ForceUnit_t), intent(out) :: unit

        type(ErrorList_t) :: errors_

        call fromString(var_str(string), units, errors_, unit)
        call errors%appendErrors( &
                errors_, &
                Module_("Force_m"), &
                Procedure_("unitFromStringWithUnitsC"))
    end subroutine unitFromStringWithUnitsC

    pure subroutine unitFromStringWithUnitsS(string, units, errors, unit)
        type(VARYING_STRING), intent(in) :: string
        type(ForceUnit_t), intent(in) :: units(:)
        type(ErrorList_t), intent(out) :: errors
        type(ForceUnit_t), intent(out) :: unit

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
                    Module_("Force_m"), &
                    Procedure_("unitFromStringWithUnitsS"), &
                    '"' // string // '", known units: [' // join(unit_strings, ', ') // ']' ))
        end if
    end subroutine unitFromStringWithUnitsS

    elemental function unitToString(self) result(string)
        class(ForceUnit_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = trim(self%symbol)
    end function unitToString

    elemental function unitToGnuplotString(self) result(string)
        class(ForceUnit_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = trim(self%gnuplot_symbol)
    end function unitToGnuplotString

    elemental function unitToLatexString(self) result(string)
        class(ForceUnit_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = trim(self%latex_symbol)
    end function unitToLatexString
end module Force_m
