module Quantity_module_m
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

    type, public :: QuantityCamel_t
        double precision :: units_lower
    contains
        private
        procedure :: toUnits
        generic, public :: operator(.in.) => toUnits
        procedure, pass(quantity_lower) :: doubleTimesQuantityCamel
        procedure, pass(quantity_lower) :: integerTimesQuantityCamel
        procedure, pass(quantity_lower) :: quantitySnakeTimesDouble
        procedure, pass(quantity_lower) :: quantitySnakeTimesInteger
        generic, public :: operator(*) => &
                doubleTimesQuantityCamel, &
                integerTimesQuantityCamel, &
                quantitySnakeTimesDouble, &
                quantitySnakeTimesInteger
        procedure :: quantitySnakeDividedByDouble
        procedure :: quantitySnakeDividedByInteger
        procedure, pass(numerator) :: quantitySnakeDividedByQuantityCamel
        generic, public :: operator(/) => &
                quantitySnakeDividedByDouble, &
                quantitySnakeDividedByInteger, &
                quantitySnakeDividedByQuantityCamel
        procedure :: quantitySnakePlusQuantityCamel
        generic, public :: operator(+) => quantitySnakePlusQuantityCamel
        procedure :: quantitySnakeMinusQuantityCamel
        generic, public :: operator(-) => quantitySnakeMinusQuantityCamel
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
    end type QuantityCamel_t

    type, public :: QuantityCamelUnit_t
        double precision :: conversion_factor
        character(len=10) :: symbol
        character(len=50) :: gnuplot_symbol
        character(len=100) :: latex_symbol
    contains
        procedure :: toString => unitToString
        procedure :: toGnuplotString => unitToGnuplotString
        procedure :: toLatexString => unitToLatexString
    end type QuantityCamelUnit_t

    interface operator(.unit.)
        module procedure fromUnits
    end interface operator(.unit.)

    interface quantitySnakeFromString
        module procedure fromStringBasicC
        module procedure fromStringBasicS
        module procedure fromStringWithUnitsC
        module procedure fromStringWithUnitsS
    end interface quantitySnakeFromString

    interface quantitySnakeUnitFromString
        module procedure unitFromStringBasicC
        module procedure unitFromStringBasicS
        module procedure unitFromStringWithUnitsC
        module procedure unitFromStringWithUnitsS
    end interface quantitySnakeUnitFromString

    type(QuantityCamelUnit_t), parameter, public :: UNITS_CAPITAL = &
            QuantityCamelUnit_t( &
                    conversion_factor = 1.0d0, &
                    symbol = "unit_sym", &
                    gnuplot_symbol = "gnu_sym", &
                    latex_symbol = "ltx_sym")
    type(QuantityCamelUnit_t), parameter, public :: UNITS_CAPITAL2 = &
            QuantityCamelUnit_t( &
                    conversion_factor = 1.0d0, &
                    symbol = "unit_sym", &
                    gnuplot_symbol = "gnu_sym", &
                    latex_symbol = "ltx_sym")

    type(QuantityCamelUnit_t), public :: DEFAULT_OUTPUT_UNITS = UNITS_CAPITAL

    type(QuantityCamelUnit_t), parameter, public :: PROVIDED_UNITS(*) = &
            [UNITS_CAPITAL, UNITS_CAPITAL2]

    public :: &
            operator(.unit.), &
            quantitySnakeFromString, &
            quantitySnakeUnitFromString
contains
    function fromStringBasicC(string, errors) result(quantity_lower)
        character(len=*), intent(in) :: string
        type(ErrorList_t), intent(out) :: errors
        type(QuantityCamel_t) :: quantity_lower

        type(ErrorList_t) :: errors_

        quantity_lower = quantitySnakeFromString( &
                var_str(string), PROVIDED_UNITS, errors_)
        call errors%appendErrors( &
                errors_, &
                Module_("Quantity_module_m"), &
                Procedure_("fromStringBasicC"))
    end function fromStringBasicC

    function fromStringBasicS(string, errors) result(quantity_lower)
        type(VARYING_STRING), intent(in) :: string
        type(ErrorList_t), intent(out) :: errors
        type(QuantityCamel_t) :: quantity_lower

        type(ErrorList_t) :: errors_

        quantity_lower = quantitySnakeFromString( &
                string, PROVIDED_UNITS, errors_)
        call errors%appendErrors( &
                errors_, &
                Module_("Quantity_module_m"), &
                Procedure_("fromStringBasicS"))
    end function fromStringBasicS

    function fromStringWithUnitsC(string, units, errors) result(quantity_lower)
        character(len=*), intent(in) :: string
        type(QuantityCamelUnit_t), intent(in) :: units(:)
        type(ErrorList_t), intent(out) :: errors
        type(QuantityCamel_t) :: quantity_lower

        type(ErrorList_t) :: errors_

        quantity_lower = quantitySnakeFromString( &
                var_str(string), units, errors_)
        call errors%appendErrors( &
                errors_, &
                Module_("Quantity_module_m"), &
                Procedure_("fromStringWithUnitsC"))
    end function fromStringWithUnitsC

    function fromStringWithUnitsS(string, units, errors) result(quantity_lower)
        type(VARYING_STRING), intent(in) :: string
        type(QuantityCamelUnit_t), intent(in) :: units(:)
        type(ErrorList_t), intent(out) :: errors
        type(QuantityCamel_t) :: quantity_lower

        double precision :: number
        character(len=100) :: number_chars
        type(VARYING_STRING) :: number_string
        integer :: status
        type(VARYING_STRING) :: symbol
        type(QuantityCamelUnit_t) :: unit
        type(ErrorList_t) :: unit_errors

        quantity_lower%units_lower = 0.0d0
        symbol = string
        call split(symbol, number_string, " ")
        if (len(symbol) == 0) then
            call errors%appendError(Fatal( &
                    PARSE_ERROR, &
                    Module_("Quantity_module_m"), &
                    Procedure_("fromStringWithUnitsS"), &
                    'No unit symbol found in string "' // string // '"'))
            return
        end if
        number_chars = number_string
        read(number_chars, *, iostat=status) number
        if (status /= 0) then
            call errors%appendError(Fatal( &
                    PARSE_ERROR, &
                    Module_("Quantity_module_m"), &
                    Procedure_("fromStringWithUnitsS"), &
                    'Error parsing number from string "' // number_string // '"'))
        end if
        unit = quantitySnakeUnitFromString(symbol, units, unit_errors)
        quantity_lower = number.unit.unit
        call errors%appendErrors( &
                unit_errors, &
                Module_("Quantity_module_m"), &
                Procedure_("fromStringWithUnitsS"))
    end function fromStringWithUnitsS

    elemental function fromUnits(value_, units) result(quantity_lower)
        double precision, intent(in) :: value_
        type(QuantityCamelUnit_t), intent(in) :: units
        type(QuantityCamel_t) :: quantity_lower

        quantity_lower%units_lower = value_ / units%conversion_factor
    end function fromUnits

    elemental function toUnits(self, units) result(quantity_lower)
        class(QuantityCamel_t), intent(in) :: self
        class(QuantityCamelUnit_t), intent(in) :: units
        double precision :: quantity_lower

        quantity_lower = self%units_lower * units%conversion_factor
    end function toUnits

    elemental function doubleTimesQuantityCamel( &
            multiplier, quantity_lower) result(new_quantity_lower)
        double precision, intent(in) :: multiplier
        class(QuantityCamel_t), intent(in) :: quantity_lower
        type(QuantityCamel_t) :: new_quantity_lower

        new_quantity_lower%units_lower = &
                multiplier * quantity_lower%units_lower
    end function doubleTimesQuantityCamel

    elemental function integerTimesQuantityCamel( &
            multiplier, quantity_lower) result(new_quantity_lower)
        integer, intent(in) :: multiplier
        class(QuantityCamel_t), intent(in) :: quantity_lower
        type(QuantityCamel_t) :: new_quantity_lower

        new_quantity_lower%units_lower = &
                dble(multiplier) * quantity_lower%units_lower
    end function integerTimesQuantityCamel

    elemental function quantitySnakeTimesDouble( &
            quantity_lower, multiplier) result(new_quantity_lower)
        class(QuantityCamel_t), intent(in) :: quantity_lower
        double precision, intent(in) :: multiplier
        type(QuantityCamel_t) :: new_quantity_lower

        new_quantity_lower%units_lower = &
                quantity_lower%units_lower * multiplier
    end function quantitySnakeTimesDouble

    elemental function quantitySnakeTimesInteger( &
            quantity_lower, multiplier) result(new_quantity_lower)
        class(QuantityCamel_t), intent(in) :: quantity_lower
        integer, intent(in) :: multiplier
        type(QuantityCamel_t) :: new_quantity_lower

        new_quantity_lower%units_lower = &
                quantity_lower%units_lower * dble(multiplier)
    end function quantitySnakeTimesInteger

    elemental function quantitySnakeDividedByDouble( &
            quantity_lower, divisor) result(new_quantity_lower)
        class(QuantityCamel_t), intent(in) :: quantity_lower
        double precision, intent(in) :: divisor
        type(QuantityCamel_t) :: new_quantity_lower

        new_quantity_lower%units_lower = &
                quantity_lower%units_lower / divisor
    end function quantitySnakeDividedByDouble

    elemental function quantitySnakeDividedByInteger( &
            quantity_lower, divisor) result(new_quantity_lower)
        class(QuantityCamel_t), intent(in) :: quantity_lower
        integer, intent(in) :: divisor
        type(QuantityCamel_t) :: new_quantity_lower

        new_quantity_lower%units_lower = &
                quantity_lower%units_lower / dble(divisor)
    end function quantitySnakeDividedByInteger

    elemental function quantitySnakeDividedByQuantityCamel( &
            numerator, denomenator) result(ratio)
        class(QuantityCamel_t), intent(in) :: numerator
        class(QuantityCamel_t), intent(in) :: denomenator
        double precision :: ratio

        ratio = numerator%units_lower / denomenator%units_lower
    end function quantitySnakeDividedByQuantityCamel

    elemental function quantitySnakePlusQuantityCamel( &
            quantity_lower1, quantity_lower2) result(new_quantity_lower)
        class(QuantityCamel_t), intent(in) :: quantity_lower1
        class(QuantityCamel_t), intent(in) :: quantity_lower2
        type(QuantityCamel_t) :: new_quantity_lower

        new_quantity_lower%units_lower = &
                quantity_lower1%units_lower + quantity_lower2%units_lower
    end function quantitySnakePlusQuantityCamel

    elemental function quantitySnakeMinusQuantityCamel( &
            quantity_lower1, quantity_lower2) result(new_quantity_lower)
        class(QuantityCamel_t), intent(in) :: quantity_lower1
        class(QuantityCamel_t), intent(in) :: quantity_lower2
        type(QuantityCamel_t) :: new_quantity_lower

        new_quantity_lower%units_lower = &
                quantity_lower1%units_lower - quantity_lower2%units_lower
    end function quantitySnakeMinusQuantityCamel

    elemental function greaterThan(lhs, rhs)
        class(QuantityCamel_t), intent(in) :: lhs
        class(QuantityCamel_t), intent(in) :: rhs
        logical :: greaterThan

        greaterThan = lhs%units_lower > rhs%units_lower
    end function greaterThan

    elemental function lessThan(lhs,rhs)
        class(QuantityCamel_t), intent(in) :: lhs
        class(QuantityCamel_t), intent(in) :: rhs
        logical :: lessThan

        lessThan = lhs%units_lower < rhs%units_lower
    end function lessThan

    elemental function greaterThanOrEqual(lhs, rhs)
        class(QuantityCamel_t), intent(in) :: lhs
        class(QuantityCamel_t), intent(in) :: rhs
        logical :: greaterThanOrEqual

        greaterThanOrEqual = lhs%units_lower >= rhs%units_lower
    end function greaterThanOrEqual

    elemental function lessThanOrEqual(lhs, rhs)
        class(QuantityCamel_t), intent(in) :: lhs
        class(QuantityCamel_t), intent(in) :: rhs
        logical :: lessThanOrEqual

        lessThanOrEqual = lhs%units_lower <= rhs%units_lower
    end function lessThanOrEqual

    elemental function equal_(lhs,rhs)
        class(QuantityCamel_t), intent(in) :: lhs
        class(QuantityCamel_t), intent(in) :: rhs
        logical :: equal_

        equal_ = lhs%units_lower .safeEq. rhs%units_lower
    end function equal_

    elemental function equalWithinAbsolute(lhs, rhs, within)
        class(QuantityCamel_t), intent(in) :: lhs
        class(QuantityCamel_t), intent(in) :: rhs
        class(QuantityCamel_t), intent(in) :: within
        logical :: equalWithinAbsolute

        equalWithinAbsolute = equalWithinAbsolute_( &
                lhs%units_lower, rhs%units_lower, within%units_lower)
    end function equalWithinAbsolute

    elemental function equalWithinRelative(lhs, rhs, within)
        class(QuantityCamel_t), intent(in) :: lhs
        class(QuantityCamel_t), intent(in) :: rhs
        double precision, intent(in) :: within
        logical :: equalWithinRelative

        equalWithinRelative = equalWithinRelative_( &
                lhs%units_lower, rhs%units_lower, within)
    end function equalWithinRelative

    elemental function notEqual(lhs, rhs)
        class(QuantityCamel_t), intent(in) :: lhs
        class(QuantityCamel_t), intent(in) :: rhs
        logical :: notEqual

        notEqual = .not. lhs == rhs
    end function notEqual

    elemental function toStringFullPrecision(self) result(string)
        class(QuantityCamel_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = self%toStringIn(DEFAULT_OUTPUT_UNITS)
    end function toStringFullPrecision

    elemental function toStringWithPrecision(self, significant_digits) result(string)
        class(QuantityCamel_t), intent(in) :: self
        integer, intent(in) :: significant_digits
        type(VARYING_STRING) :: string

        string = self%toStringIn(DEFAULT_OUTPUT_UNITS, significant_digits)
    end function toStringWithPrecision

    elemental function toStringInFullPrecision(self, units) result(string)
        class(QuantityCamel_t), intent(in) :: self
        class(QuantityCamelUnit_t), intent(in) :: units
        type(VARYING_STRING) :: string

        string = toString(self.in.units) // " " // units%toString()
    end function toStringInFullPrecision

    elemental function toStringInWithPrecision( &
            self, units, significant_digits) result(string)
        class(QuantityCamel_t), intent(in) :: self
        class(QuantityCamelUnit_t), intent(in) :: units
        integer, intent(in) :: significant_digits
        type(VARYING_STRING) :: string

        string = &
                toString(self.in.units, significant_digits) &
                // " " // units%toString()
    end function toStringInWithPrecision

    elemental function toGnuplotStringFullPrecision(self) result(string)
        class(QuantityCamel_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = self%toGnuplotStringIn(DEFAULT_OUTPUT_UNITS)
    end function toGnuplotStringFullPrecision

    elemental function toGnuplotStringWithPrecision( &
            self, significant_digits) result(string)
        class(QuantityCamel_t), intent(in) :: self
        integer, intent(in) :: significant_digits
        type(VARYING_STRING) :: string

        string = self%toGnuplotStringIn( &
                DEFAULT_OUTPUT_UNITS, significant_digits)
    end function toGnuplotStringWithPrecision

    elemental function toGnuplotStringInFullPrecision(self, units) result(string)
        class(QuantityCamel_t), intent(in) :: self
        class(QuantityCamelUnit_t), intent(in) :: units
        type(VARYING_STRING) :: string

        string = toString(self.in.units) // " " // units%toGnuplotString()
    end function toGnuplotStringInFullPrecision

    elemental function toGnuplotStringInWithPrecision( &
            self, units, significant_digits) result(string)
        class(QuantityCamel_t), intent(in) :: self
        class(QuantityCamelUnit_t), intent(in) :: units
        integer, intent(in) :: significant_digits
        type(VARYING_STRING) :: string

        string = &
                toString(self.in.units, significant_digits) &
                // " " // units%toGnuplotString()
    end function toGnuplotStringInWithPrecision

    elemental function toLatexStringFullPrecision(self) result(string)
        class(QuantityCamel_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = self%toLatexStringIn(DEFAULT_OUTPUT_UNITS)
    end function toLatexStringFullPrecision

    elemental function toLatexStringWithPrecision(self, significant_digits) result(string)
        class(QuantityCamel_t), intent(in) :: self
        integer, intent(in) :: significant_digits
        type(VARYING_STRING) :: string

        string = self%toLatexStringIn(DEFAULT_OUTPUT_UNITS, significant_digits)
    end function toLatexStringWithPrecision

    elemental function toLatexStringInFullPrecision(self, units) result(string)
        class(QuantityCamel_t), intent(in) :: self
        class(QuantityCamelUnit_t), intent(in) :: units
        type(VARYING_STRING) :: string

        string = wrapInLatexQuantity( &
                toString(self.in.units), units%toLatexString())
    end function toLatexStringInFullPrecision

    elemental function toLatexStringInWithPrecision( &
            self, units, significant_digits) result(string)
        class(QuantityCamel_t), intent(in) :: self
        class(QuantityCamelUnit_t), intent(in) :: units
        integer, intent(in) :: significant_digits
        type(VARYING_STRING) :: string

        string = wrapInLatexQuantity( &
                toString(self.in.units, significant_digits), &
                units%toLatexString())
    end function toLatexStringInWithPrecision

    function unitFromStringBasicC(string, errors) result(unit)
        character(len=*), intent(in) :: string
        type(ErrorList_t), intent(out) :: errors
        type(QuantityCamelUnit_t) :: unit

        type(ErrorList_t) :: errors_

        unit = quantitySnakeUnitFromString( &
                var_str(string), PROVIDED_UNITS, errors_)
        call errors%appendErrors( &
                errors_, &
                Module_("Quantity_module_m"), &
                Procedure_("unitFromStringBasicC"))
    end function unitFromStringBasicC

    function unitFromStringBasicS(string, errors) result(unit)
        type(VARYING_STRING), intent(in) :: string
        type(ErrorList_t), intent(out) :: errors
        type(QuantityCamelUnit_t) :: unit

        type(ErrorList_t) :: errors_

        unit = quantitySnakeUnitFromString( &
                string, PROVIDED_UNITS, errors_)
        call errors%appendErrors( &
                errors_, &
                Module_("Quantity_module_m"), &
                Procedure_("unitFromStringBasicS"))
    end function unitFromStringBasicS

    function unitFromStringWithUnitsC(string, units, errors) result(unit)
        character(len=*), intent(in) :: string
        type(QuantityCamelUnit_t), intent(in) :: units(:)
        type(ErrorList_t), intent(out) :: errors
        type(QuantityCamelUnit_t) :: unit

        type(ErrorList_t) :: errors_

        unit = quantitySnakeUnitFromString(var_str(string), units, errors_)
        call errors%appendErrors( &
                errors_, &
                Module_("Quantity_module_m"), &
                Procedure_("unitFromStringWithUnitsC"))
    end function unitFromStringWithUnitsC

    function unitFromStringWithUnitsS(string, units, errors) result(unit)
        type(VARYING_STRING), intent(in) :: string
        type(QuantityCamelUnit_t), intent(in) :: units(:)
        type(ErrorList_t), intent(out) :: errors
        type(QuantityCamelUnit_t) :: unit

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
                    Module_("Quantity_module_m"), &
                    Procedure_("unitFromStringWithUnitsS"), &
                    '"' // string // '", known units: [' // join(unit_strings, ', ') // ']' ))
        end if
    end function unitFromStringWithUnitsS

    elemental function unitToString(self) result(string)
        class(QuantityCamelUnit_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = trim(self%symbol)
    end function unitToString

    elemental function unitToGnuplotString(self) result(string)
        class(QuantityCamelUnit_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = trim(self%gnuplot_symbol)
    end function unitToGnuplotString

    elemental function unitToLatexString(self) result(string)
        class(QuantityCamelUnit_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = trim(self%latex_symbol)
    end function unitToLatexString
end module Quantity_module_m
