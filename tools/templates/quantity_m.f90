module quaff_quantity_m
    use erloff, only: error_list_t, fatal_t, module_t, procedure_t
    use iso_varying_string, only: &
            varying_string, &
            assignment(=), &
            operator(==), &
            operator(//), &
            var_str
    use parff, only: &
            parse_result_t, &
            parsed_rational_t, &
            parser_output_t, &
            state_t, &
            either, &
            parse_end_of_input, &
            parse_rational, &
            parse_string, &
            parse_whitespace, &
            parse_with, &
            then_drop
    use quaff_utilities_m, only: &
            operator(.safeEq.), &
            equal_within_absolute, &
            equal_within_relative, &
            parse_space, &
            PARSE_ERROR, &
            UNKNOWN_UNIT
    use strff, only: join, to_string

    implicit none
    private
    public :: &
            quantity_t, &
            fallible_quantity_t, &
            quantity_unit_t, &
            fallible_quantity_unit_t, &
            quantity_simple_unit_t, &
            operator(.unit.), &
            parse_quantity, &
            parse_quantity_unit, &
            sum, &
            DEFAULT_OUTPUT_UNITS, &
            PROVIDED_UNITS, &
            METERS, &
            METERS2

    type :: quantity_t
        double precision :: meters
    contains
        private
        procedure :: to_units
        generic, public :: operator(.in.) => to_units
        procedure, pass(quantity) :: double_times_quantity
        procedure, pass(quantity) :: integer_times_quantity
        procedure :: quantity_times_double
        procedure :: quantity_times_integer
        generic, public :: operator(*) => &
                double_times_quantity, &
                integer_times_quantity, &
                quantity_times_double, &
                quantity_times_integer
        procedure :: quantity_divided_by_double
        procedure :: quantity_divided_by_integer
        procedure :: quantity_divided_by_quantity
        generic, public :: operator(/) => &
                quantity_divided_by_double, &
                quantity_divided_by_integer, &
                quantity_divided_by_quantity
        procedure :: quantity_plus_quantity
        generic, public :: operator(+) => quantity_plus_quantity
        procedure :: quantity_minus_quantity
        generic, public :: operator(-) => quantity_minus_quantity
        procedure :: greater_than
        generic, public :: operator(>) => greater_than
        procedure :: less_than
        generic, public :: operator(<) => less_than
        procedure :: greater_than_or_equal
        generic, public :: operator(>=) => greater_than_or_equal
        procedure :: less_than_or_equal
        generic, public :: operator(<=) => less_than_or_equal
        procedure :: equal_
        generic, public :: operator(==) => equal_
        procedure :: equal_within_absolute_
        procedure :: equal_within_relative_
        generic, public :: equal => &
                equal_, equal_within_absolute_, equal_within_relative_
        procedure :: not_equal
        generic, public :: operator(/=) => not_equal
        procedure :: to_string_full_precision
        procedure :: to_string_with_precision
        generic, public :: to_string => &
                to_string_full_precision, to_string_with_precision
        procedure :: to_string_in_full_precision
        procedure :: to_string_in_with_precision
        generic, public :: to_string_in => &
                to_string_in_full_precision, to_string_in_with_precision
    end type

    type :: fallible_quantity_t
        private
        type(quantity_t) :: quantity_ = quantity_t(0.0d0)
        type(error_list_t) :: errors_
    contains
        private
        procedure, public :: failed => fallible_quantity_failed
        procedure, public :: quantity => fallible_quantity_quantity
        procedure, public :: errors => fallible_quantity_errors
    end type

    type, abstract :: quantity_unit_t
        double precision :: conversion_factor
    contains
        procedure(unit_to_string_i), deferred :: unit_to_string
        procedure(value_to_string_i), deferred :: value_to_string
        generic :: to_string => unit_to_string, value_to_string
        procedure(parse_as_i), deferred :: parse_as
    end type

    type :: fallible_quantity_unit_t
        private
        class(quantity_unit_t), allocatable :: unit_
        type(error_list_t) :: errors_
    contains
        private
        procedure, public :: failed => fallible_quantity_unit_failed
        procedure, public :: unit => fallible_quantity_unit_unit
        procedure, public :: errors => fallible_quantity_unit_errors
    end type

    type, extends(quantity_unit_t) :: quantity_simple_unit_t
        character(len=20) :: symbol
    contains
        procedure :: unit_to_string => simple_unit_to_string
        procedure :: value_to_string => simple_value_to_string
        procedure :: parse_as => simple_parse_as
    end type

    abstract interface
        elemental function unit_to_string_i(self) result(string)
            import :: quantity_unit_t, varying_string

            implicit none

            class(quantity_unit_t), intent(in) :: self
            type(varying_string) :: string
        end function

        pure function value_to_string_i(self, value_) result(string)
            import :: quantity_unit_t, varying_string

            implicit none

            class(quantity_unit_t), intent(in) :: self
            type(varying_string), intent(in) :: value_
            type(varying_string) :: string
        end function

        function parse_as_i(self, string) result(fallible_quantity)
            import :: quantity_unit_t, fallible_quantity_t, varying_string

            implicit none

            class(quantity_unit_t), intent(in) :: self
            type(varying_string), intent(in) :: string
            type(fallible_quantity_t) :: fallible_quantity
        end function
    end interface

    interface operator(.unit.)
        module procedure from_units
    end interface

    interface fallible_quantity_t
        module procedure fallible_quantity_from_quantity
        module procedure fallible_quantity_from_errors
        module procedure fallible_quantity_from_fallible_quantity
    end interface

    interface fallible_quantity_unit_t
        module procedure fallible_quantity_unit_from_unit
        module procedure fallible_quantity_unit_from_errors
        module procedure fallible_quantity_unit_from_fallible_quantity_unit
    end interface

    interface parse_quantity
        module procedure parse_quantity_c
        module procedure parse_quantity_s
        module procedure parse_quantity_with_units_c
        module procedure parse_quantity_with_units_s
    end interface

    interface parse_quantity_unit
        module procedure parse_quantity_unit_c
        module procedure parse_quantity_unit_s
        module procedure parse_quantity_unit_with_units_c
        module procedure parse_quantity_unit_with_units_s
    end interface

    interface sum
        module procedure sum_quantity
    end interface

    type(quantity_simple_unit_t), parameter :: METERS = &
            quantity_simple_unit_t( &
                    conversion_factor = 1.0d0, &
                    symbol = "SYM")
    type(quantity_simple_unit_t), parameter :: METERS2 = &
            quantity_simple_unit_t( &
                    conversion_factor = 1.0d0, &
                    symbol = "SYM")

    type(quantity_simple_unit_t) :: DEFAULT_OUTPUT_UNITS = METERS

    type(quantity_simple_unit_t), parameter :: PROVIDED_UNITS(*) = &
            [METERS, METERS2]

    character(len=*), parameter :: MODULE_NAME = "quaff_quantity_m"
contains
    function parse_quantity_c(string) result(fallible_quantity)
        character(len=*), intent(in) :: string
        type(fallible_quantity_t) :: fallible_quantity

        fallible_quantity = fallible_quantity_t( &
                parse_quantity(var_str(string), PROVIDED_UNITS), &
                module_t(MODULE_NAME), &
                procedure_t("parse_quantity_c"))
    end function

    function parse_quantity_s(string) result(fallible_quantity)
        type(varying_string), intent(in) :: string
        type(fallible_quantity_t) :: fallible_quantity

        fallible_quantity = fallible_quantity_t( &
                parse_quantity(string, PROVIDED_UNITS), &
                module_t(MODULE_NAME), &
                procedure_t("parse_quantity_s"))
    end function

    function parse_quantity_with_units_c( &
            string, units) result(fallible_quantity)
        character(len=*), intent(in) :: string
        class(quantity_unit_t), intent(in) :: units(:)
        type(fallible_quantity_t) :: fallible_quantity

        fallible_quantity = fallible_quantity_t( &
                parse_quantity(var_str(string), units), &
                module_t(MODULE_NAME), &
                procedure_t("parse_quantity_with_units_c"))
    end function

    function parse_quantity_with_units_s( &
            string, units) result(fallible_quantity)
        type(varying_string), intent(in) :: string
        class(quantity_unit_t), intent(in) :: units(:)
        type(fallible_quantity_t) :: fallible_quantity

        integer :: i

        do i = 1, size(units)
            fallible_quantity = units(i)%parse_as(string)
            if (.not. fallible_quantity%failed()) return
        end do
        fallible_quantity = fallible_quantity_t(error_list_t(fatal_t( &
                PARSE_ERROR, &
                module_t(MODULE_NAME), &
                procedure_t("parse_quantity_with_units_s"), &
                "Unable to parse '" // string // "' as a quantity_t. Tried with units: " &
                // join(units%to_string(), ", "))))
    end function

    elemental function from_units(value_, units) result(quantity)
        double precision, intent(in) :: value_
        class(quantity_unit_t), intent(in) :: units
        type(quantity_t) :: quantity

        quantity%meters = value_ / units%conversion_factor
    end function

    elemental function to_units(self, units) result(quantity)
        class(quantity_t), intent(in) :: self
        class(quantity_unit_t), intent(in) :: units
        double precision :: quantity

        quantity = self%meters * units%conversion_factor
    end function

    elemental function double_times_quantity( &
            multiplier, quantity) result(new_quantity)
        double precision, intent(in) :: multiplier
        class(quantity_t), intent(in) :: quantity
        type(quantity_t) :: new_quantity

        new_quantity%meters = &
                multiplier * quantity%meters
    end function

    elemental function integer_times_quantity( &
            multiplier, quantity) result(new_quantity)
        integer, intent(in) :: multiplier
        class(quantity_t), intent(in) :: quantity
        type(quantity_t) :: new_quantity

        new_quantity%meters = &
                dble(multiplier) * quantity%meters
    end function

    elemental function quantity_times_double( &
            quantity, multiplier) result(new_quantity)
        class(quantity_t), intent(in) :: quantity
        double precision, intent(in) :: multiplier
        type(quantity_t) :: new_quantity

        new_quantity%meters = &
                quantity%meters * multiplier
    end function

    elemental function quantity_times_integer( &
            quantity, multiplier) result(new_quantity)
        class(quantity_t), intent(in) :: quantity
        integer, intent(in) :: multiplier
        type(quantity_t) :: new_quantity

        new_quantity%meters = &
                quantity%meters * dble(multiplier)
    end function

    elemental function quantity_divided_by_double( &
            quantity, divisor) result(new_quantity)
        class(quantity_t), intent(in) :: quantity
        double precision, intent(in) :: divisor
        type(quantity_t) :: new_quantity

        new_quantity%meters = &
                quantity%meters / divisor
    end function

    elemental function quantity_divided_by_integer( &
            quantity, divisor) result(new_quantity)
        class(quantity_t), intent(in) :: quantity
        integer, intent(in) :: divisor
        type(quantity_t) :: new_quantity

        new_quantity%meters = &
                quantity%meters / dble(divisor)
    end function

    elemental function quantity_divided_by_quantity( &
            numerator, denomenator) result(ratio)
        class(quantity_t), intent(in) :: numerator
        type(quantity_t), intent(in) :: denomenator
        double precision :: ratio

        ratio = numerator%meters / denomenator%meters
    end function

    elemental function quantity_plus_quantity( &
            lhs, rhs) result(sum_)
        class(quantity_t), intent(in) :: lhs
        type(quantity_t), intent(in) :: rhs
        type(quantity_t) :: sum_

        sum_%meters = lhs%meters + rhs%meters
    end function

    elemental function quantity_minus_quantity( &
            lhs, rhs) result(difference)
        class(quantity_t), intent(in) :: lhs
        type(quantity_t), intent(in) :: rhs
        type(quantity_t) :: difference

        difference%meters = lhs%meters - rhs%meters
    end function

    pure function sum_quantity(quantitys) result(sum_)
        type(quantity_t), intent(in) :: quantitys(:)
        type(quantity_t) :: sum_

        sum_%meters = sum(quantitys%meters)
    end function

    elemental function greater_than(lhs, rhs)
        class(quantity_t), intent(in) :: lhs
        type(quantity_t), intent(in) :: rhs
        logical :: greater_than

        greater_than = lhs%meters > rhs%meters
    end function

    elemental function less_than(lhs, rhs)
        class(quantity_t), intent(in) :: lhs
        type(quantity_t), intent(in) :: rhs
        logical :: less_than

        less_than = lhs%meters < rhs%meters
    end function

    elemental function greater_than_or_equal(lhs, rhs)
        class(quantity_t), intent(in) :: lhs
        type(quantity_t), intent(in) :: rhs
        logical :: greater_than_or_equal

        greater_than_or_equal = lhs%meters >= rhs%meters
    end function

    elemental function less_than_or_equal(lhs, rhs)
        class(quantity_t), intent(in) :: lhs
        type(quantity_t), intent(in) :: rhs
        logical :: less_than_or_equal

        less_than_or_equal = lhs%meters <= rhs%meters
    end function

    elemental function equal_(lhs, rhs)
        class(quantity_t), intent(in) :: lhs
        type(quantity_t), intent(in) :: rhs
        logical :: equal_

        equal_ = lhs%meters .safeEq. rhs%meters
    end function

    elemental function equal_within_absolute_(lhs, rhs, within)
        class(quantity_t), intent(in) :: lhs
        type(quantity_t), intent(in) :: rhs
        type(quantity_t), intent(in) :: within
        logical :: equal_within_absolute_

        equal_within_absolute_ = equal_within_absolute( &
                lhs%meters, rhs%meters, within%meters)
    end function

    elemental function equal_within_relative_(lhs, rhs, within)
        class(quantity_t), intent(in) :: lhs
        type(quantity_t), intent(in) :: rhs
        double precision, intent(in) :: within
        logical :: equal_within_relative_

        equal_within_relative_ = equal_within_relative( &
                lhs%meters, rhs%meters, within)
    end function

    elemental function not_equal(lhs, rhs)
        class(quantity_t), intent(in) :: lhs
        type(quantity_t), intent(in) :: rhs
        logical :: not_equal

        not_equal = .not. lhs == rhs
    end function

    elemental function to_string_full_precision(self) result(string)
        class(quantity_t), intent(in) :: self
        type(varying_string) :: string

        string = self%to_string_in(DEFAULT_OUTPUT_UNITS)
    end function

    elemental function to_string_with_precision(self, significant_digits) result(string)
        class(quantity_t), intent(in) :: self
        integer, intent(in) :: significant_digits
        type(varying_string) :: string

        string = self%to_string_in(DEFAULT_OUTPUT_UNITS, significant_digits)
    end function

    elemental function to_string_in_full_precision(self, units) result(string)
        class(quantity_t), intent(in) :: self
        class(quantity_unit_t), intent(in) :: units
        type(varying_string) :: string

        string = units%to_string(to_string(self.in.units))
    end function

    elemental function to_string_in_with_precision( &
            self, units, significant_digits) result(string)
        class(quantity_t), intent(in) :: self
        class(quantity_unit_t), intent(in) :: units
        integer, intent(in) :: significant_digits
        type(varying_string) :: string

        string = units%to_string(to_string(self.in.units, significant_digits))
    end function

    function fallible_quantity_from_quantity( &
            quantity) result(fallible_quantity)
        type(quantity_t), intent(in) :: quantity
        type(fallible_quantity_t) :: fallible_quantity

        fallible_quantity%quantity_ = quantity
    end function

    function fallible_quantity_from_errors( &
            errors) result(fallible_quantity)
        type(error_list_t), intent(in) :: errors
        type(fallible_quantity_t) :: fallible_quantity

        fallible_quantity%errors_ = errors
    end function

    function fallible_quantity_from_fallible_quantity( &
            fallible_quantity, &
            module_, &
            procedure_) &
            result(new_fallible_quantity)
        type(fallible_quantity_t), intent(in) :: fallible_quantity
        type(module_t), intent(in) :: module_
        type(procedure_t), intent(in) :: procedure_
        type(fallible_quantity_t) :: new_fallible_quantity

        if (fallible_quantity%failed()) then
            new_fallible_quantity%errors_ = error_list_t( &
                    fallible_quantity%errors_, module_, procedure_)
        else
            new_fallible_quantity%quantity_ = fallible_quantity%quantity_
        end if
    end function

    elemental function fallible_quantity_failed( &
            self) result(failed)
        class(fallible_quantity_t), intent(in) :: self
        logical :: failed

        failed = self%errors_%has_any()
    end function

    elemental function fallible_quantity_quantity( &
            self) result(quantity)
        class(fallible_quantity_t), intent(in) :: self
        type(quantity_t) :: quantity

        quantity = self%quantity_
    end function

    impure elemental function fallible_quantity_errors( &
            self) result(errors)
        class(fallible_quantity_t), intent(in) :: self
        type(error_list_t) :: errors

        errors = self%errors_
    end function

    function fallible_quantity_unit_from_unit( &
            unit) result(fallible_quantity_unit)
        class(quantity_unit_t), intent(in) :: unit
        type(fallible_quantity_unit_t) :: fallible_quantity_unit

        allocate(fallible_quantity_unit%unit_, source = unit)
    end function

    function fallible_quantity_unit_from_errors( &
            errors) result(fallible_quantity_unit)
        type(error_list_t), intent(in) :: errors
        type(fallible_quantity_unit_t) :: fallible_quantity_unit

        fallible_quantity_unit%errors_ = errors
    end function

    function fallible_quantity_unit_from_fallible_quantity_unit( &
            fallible_quantity_unit, &
            module_, &
            procedure_) &
            result(new_fallible_quantity_unit)
        type(fallible_quantity_unit_t), intent(in) :: fallible_quantity_unit
        type(module_t), intent(in) :: module_
        type(procedure_t), intent(in) :: procedure_
        type(fallible_quantity_unit_t) :: new_fallible_quantity_unit

        if (fallible_quantity_unit%failed()) then
            new_fallible_quantity_unit%errors_ = error_list_t( &
                    fallible_quantity_unit%errors_, module_, procedure_)
        else
            allocate(new_fallible_quantity_unit%unit_, source = &
                    fallible_quantity_unit%unit_)
        end if
    end function

    elemental function fallible_quantity_unit_failed( &
            self) result(failed)
        class(fallible_quantity_unit_t), intent(in) :: self
        logical :: failed

        failed = self%errors_%has_any()
    end function

    function fallible_quantity_unit_unit( &
            self) result(unit)
        class(fallible_quantity_unit_t), intent(in) :: self
        class(quantity_unit_t), allocatable :: unit

        allocate(unit, source = self%unit_)
    end function

    impure elemental function fallible_quantity_unit_errors( &
            self) result(errors)
        class(fallible_quantity_unit_t), intent(in) :: self
        type(error_list_t) :: errors

        errors = self%errors_
    end function

    elemental function simple_unit_to_string(self) result(string)
        class(quantity_simple_unit_t), intent(in) :: self
        type(varying_string) :: string

        string = trim(self%symbol)
    end function

    pure function simple_value_to_string(self, value_) result(string)
        class(quantity_simple_unit_t), intent(in) :: self
        type(varying_string), intent(in) :: value_
        type(varying_string) :: string

        string = value_ // " " // self%to_string()
    end function

    function simple_parse_as(self, string) result(fallible_quantity)
        class(quantity_simple_unit_t), intent(in) :: self
        type(varying_string), intent(in) :: string
        type(fallible_quantity_t) :: fallible_quantity

        type(parse_result_t) :: parse_result

        parse_result = parse_with(the_parser, string)
        if (parse_result%ok) then
            select type (the_number => parse_result%parsed)
            type is (parsed_rational_t)
                fallible_quantity = fallible_quantity_t(the_number%value_.unit.self)
            end select
        else
            fallible_quantity = fallible_quantity_t(error_list_t(fatal_t( &
                    PARSE_ERROR, &
                    module_t(MODULE_NAME), &
                    procedure_t("simple_parse_as"), &
                    parse_result%message)))
        end if
    contains
        function the_parser(state_) result(result_)
            type(state_t), intent(in) :: state_
            type(parser_output_t) :: result_

            result_ = then_drop( &
                    then_drop(parse_rational, parse_space, state_), &
                    parse_unit)
        end function

        function parse_unit(state_) result(result_)
            type(state_t), intent(in) :: state_
            type(parser_output_t) :: result_

            result_ = then_drop( &
                    parse_string(trim(self%symbol), state_), &
                    parse_end)
        end function

        function parse_end(state_) result(result_)
            type(state_t), intent(in) :: state_
            type(parser_output_t) :: result_

            result_ = either(parse_end_of_input, parse_whitespace, state_)
        end function
    end function

    function parse_quantity_unit_c(string) result(fallible_quantity_unit)
        character(len=*), intent(in) :: string
        type(fallible_quantity_unit_t) :: fallible_quantity_unit

        fallible_quantity_unit = fallible_quantity_unit_t( &
                parse_quantity_unit(var_str(string), PROVIDED_UNITS), &
                module_t(MODULE_NAME), &
                procedure_t("parse_quantity_unit_c"))
    end function

    function parse_quantity_unit_s(string) result(fallible_quantity_unit)
        type(varying_string), intent(in) :: string
        type(fallible_quantity_unit_t) :: fallible_quantity_unit

        fallible_quantity_unit = fallible_quantity_unit_t( &
                parse_quantity_unit(string, PROVIDED_UNITS), &
                module_t(MODULE_NAME), &
                procedure_t("parse_quantity_unit_s"))
    end function

    function parse_quantity_unit_with_units_c( &
            string, units) result(fallible_quantity_unit)
        character(len=*), intent(in) :: string
        class(quantity_unit_t), intent(in) :: units(:)
        type(fallible_quantity_unit_t) :: fallible_quantity_unit

        fallible_quantity_unit = fallible_quantity_unit_t( &
                parse_quantity_unit(var_str(string), units), &
                module_t(MODULE_NAME), &
                procedure_t("parse_quantity_unit_with_units_c"))
    end function

    function parse_quantity_unit_with_units_s( &
            string, units) result(fallible_quantity_unit)
        type(varying_string), intent(in) :: string
        class(quantity_unit_t), intent(in) :: units(:)
        type(fallible_quantity_unit_t) :: fallible_quantity_unit

        integer :: i
        type(varying_string) :: unit_strings(size(units))

        do i = 1, size(units)
            if (string == units(i)%to_string()) then
                fallible_quantity_unit = fallible_quantity_unit_t(units(i))
                return
            end if
        end do
        do i = 1, size(units)
            unit_strings(i) = units(i)%to_string()
        end do
        fallible_quantity_unit = fallible_quantity_unit_t(error_list_t(fatal_t( &
                UNKNOWN_UNIT, &
                module_t(MODULE_NAME), &
                procedure_t("parse_quantity_unit_with_units_s"), &
                '"' // string // '", known units: [' // join(unit_strings, ', ') // ']')))
    end function
end module
