module quaff_mass_rate_m
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
    use quaff_conversion_factors_m, only: GRAMS_PER_KILOGRAM
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
            mass_rate_t, &
            fallible_mass_rate_t, &
            mass_rate_unit_t, &
            fallible_mass_rate_unit_t, &
            mass_rate_simple_unit_t, &
            operator(.unit.), &
            parse_mass_rate, &
            parse_mass_rate_unit, &
            sum, &
            abs, &
            DEFAULT_OUTPUT_UNITS, &
            PROVIDED_UNITS, &
            GRAMS_PER_SECOND, &
            KILOGRAMS_PER_SECOND

    type :: mass_rate_t
        double precision :: kilograms_per_second
    contains
        private
        procedure :: to_units
        generic, public :: operator(.in.) => to_units
        procedure, pass(mass_rate) :: double_times_mass_rate
        procedure, pass(mass_rate) :: integer_times_mass_rate
        procedure :: mass_rate_times_double
        procedure :: mass_rate_times_integer
        generic, public :: operator(*) => &
                double_times_mass_rate, &
                integer_times_mass_rate, &
                mass_rate_times_double, &
                mass_rate_times_integer
        procedure :: mass_rate_divided_by_double
        procedure :: mass_rate_divided_by_integer
        procedure :: mass_rate_divided_by_mass_rate
        generic, public :: operator(/) => &
                mass_rate_divided_by_double, &
                mass_rate_divided_by_integer, &
                mass_rate_divided_by_mass_rate
        procedure :: mass_rate_plus_mass_rate
        generic, public :: operator(+) => mass_rate_plus_mass_rate
        procedure :: negate_mass_rate
        procedure :: mass_rate_minus_mass_rate
        generic, public :: operator(-) => &
                negate_mass_rate, &
                mass_rate_minus_mass_rate
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

    type :: fallible_mass_rate_t
        private
        type(mass_rate_t) :: mass_rate_ = mass_rate_t(0.0d0)
        type(error_list_t) :: errors_
    contains
        private
        procedure, public :: failed => fallible_mass_rate_failed
        procedure, public :: mass_rate => fallible_mass_rate_mass_rate
        procedure, public :: errors => fallible_mass_rate_errors
    end type

    type, abstract :: mass_rate_unit_t
        double precision :: conversion_factor
    contains
        procedure(unit_to_string_i), deferred :: unit_to_string
        procedure(value_to_string_i), deferred :: value_to_string
        generic :: to_string => unit_to_string, value_to_string
        procedure(parse_as_i), deferred :: parse_as
    end type

    type :: fallible_mass_rate_unit_t
        private
        class(mass_rate_unit_t), allocatable :: unit_
        type(error_list_t) :: errors_
    contains
        private
        procedure, public :: failed => fallible_mass_rate_unit_failed
        procedure, public :: unit => fallible_mass_rate_unit_unit
        procedure, public :: errors => fallible_mass_rate_unit_errors
    end type

    type, extends(mass_rate_unit_t) :: mass_rate_simple_unit_t
        character(len=20) :: symbol
    contains
        procedure :: unit_to_string => simple_unit_to_string
        procedure :: value_to_string => simple_value_to_string
        procedure :: parse_as => simple_parse_as
    end type

    abstract interface
        elemental function unit_to_string_i(self) result(string)
            import :: mass_rate_unit_t, varying_string

            implicit none

            class(mass_rate_unit_t), intent(in) :: self
            type(varying_string) :: string
        end function

        pure function value_to_string_i(self, value_) result(string)
            import :: mass_rate_unit_t, varying_string

            implicit none

            class(mass_rate_unit_t), intent(in) :: self
            type(varying_string), intent(in) :: value_
            type(varying_string) :: string
        end function

        function parse_as_i(self, string) result(fallible_mass_rate)
            import :: mass_rate_unit_t, fallible_mass_rate_t, varying_string

            implicit none

            class(mass_rate_unit_t), intent(in) :: self
            type(varying_string), intent(in) :: string
            type(fallible_mass_rate_t) :: fallible_mass_rate
        end function
    end interface

    interface operator(.unit.)
        module procedure from_units
    end interface

    interface fallible_mass_rate_t
        module procedure fallible_mass_rate_from_mass_rate
        module procedure fallible_mass_rate_from_errors
        module procedure fallible_mass_rate_from_fallible_mass_rate
    end interface

    interface fallible_mass_rate_unit_t
        module procedure fallible_mass_rate_unit_from_unit
        module procedure fallible_mass_rate_unit_from_errors
        module procedure fallible_mass_rate_unit_from_fallible_mass_rate_unit
    end interface

    interface parse_mass_rate
        module procedure parse_mass_rate_c
        module procedure parse_mass_rate_s
        module procedure parse_mass_rate_with_units_c
        module procedure parse_mass_rate_with_units_s
    end interface

    interface parse_mass_rate_unit
        module procedure parse_mass_rate_unit_c
        module procedure parse_mass_rate_unit_s
        module procedure parse_mass_rate_unit_with_units_c
        module procedure parse_mass_rate_unit_with_units_s
    end interface

    interface abs
        module procedure abs_mass_rate
    end interface

    interface sum
        module procedure sum_mass_rate
    end interface

    type(mass_rate_simple_unit_t), parameter :: GRAMS_PER_SECOND = &
            mass_rate_simple_unit_t( &
                    conversion_factor = GRAMS_PER_KILOGRAM, &
                    symbol = "g/s")
    type(mass_rate_simple_unit_t), parameter :: KILOGRAMS_PER_SECOND = &
            mass_rate_simple_unit_t( &
                    conversion_factor = 1.0d0, &
                    symbol = "kg/s")

    type(mass_rate_simple_unit_t) :: DEFAULT_OUTPUT_UNITS = KILOGRAMS_PER_SECOND

    type(mass_rate_simple_unit_t), parameter :: PROVIDED_UNITS(*) = &
            [GRAMS_PER_SECOND, KILOGRAMS_PER_SECOND]

    character(len=*), parameter :: MODULE_NAME = "quaff_mass_rate_m"
contains
    function parse_mass_rate_c(string) result(fallible_mass_rate)
        character(len=*), intent(in) :: string
        type(fallible_mass_rate_t) :: fallible_mass_rate

        fallible_mass_rate = fallible_mass_rate_t( &
                parse_mass_rate(var_str(string), PROVIDED_UNITS), &
                module_t(MODULE_NAME), &
                procedure_t("parse_mass_rate_c"))
    end function

    function parse_mass_rate_s(string) result(fallible_mass_rate)
        type(varying_string), intent(in) :: string
        type(fallible_mass_rate_t) :: fallible_mass_rate

        fallible_mass_rate = fallible_mass_rate_t( &
                parse_mass_rate(string, PROVIDED_UNITS), &
                module_t(MODULE_NAME), &
                procedure_t("parse_mass_rate_s"))
    end function

    function parse_mass_rate_with_units_c( &
            string, units) result(fallible_mass_rate)
        character(len=*), intent(in) :: string
        class(mass_rate_unit_t), intent(in) :: units(:)
        type(fallible_mass_rate_t) :: fallible_mass_rate

        fallible_mass_rate = fallible_mass_rate_t( &
                parse_mass_rate(var_str(string), units), &
                module_t(MODULE_NAME), &
                procedure_t("parse_mass_rate_with_units_c"))
    end function

    function parse_mass_rate_with_units_s( &
            string, units) result(fallible_mass_rate)
        type(varying_string), intent(in) :: string
        class(mass_rate_unit_t), intent(in) :: units(:)
        type(fallible_mass_rate_t) :: fallible_mass_rate

        integer :: i

        do i = 1, size(units)
            fallible_mass_rate = units(i)%parse_as(string)
            if (.not. fallible_mass_rate%failed()) return
        end do
        fallible_mass_rate = fallible_mass_rate_t(error_list_t(fatal_t( &
                PARSE_ERROR, &
                module_t(MODULE_NAME), &
                procedure_t("parse_mass_rate_with_units_s"), &
                "Unable to parse '" // string // "' as a mass_rate_t. Tried with units: " &
                // join(units%to_string(), ", "))))
    end function

    elemental function from_units(value_, units) result(mass_rate)
        double precision, intent(in) :: value_
        class(mass_rate_unit_t), intent(in) :: units
        type(mass_rate_t) :: mass_rate

        mass_rate%kilograms_per_second = value_ / units%conversion_factor
    end function

    elemental function to_units(self, units) result(mass_rate)
        class(mass_rate_t), intent(in) :: self
        class(mass_rate_unit_t), intent(in) :: units
        double precision :: mass_rate

        mass_rate = self%kilograms_per_second * units%conversion_factor
    end function

    elemental function double_times_mass_rate( &
            multiplier, mass_rate) result(new_mass_rate)
        double precision, intent(in) :: multiplier
        class(mass_rate_t), intent(in) :: mass_rate
        type(mass_rate_t) :: new_mass_rate

        new_mass_rate%kilograms_per_second = &
                multiplier * mass_rate%kilograms_per_second
    end function

    elemental function integer_times_mass_rate( &
            multiplier, mass_rate) result(new_mass_rate)
        integer, intent(in) :: multiplier
        class(mass_rate_t), intent(in) :: mass_rate
        type(mass_rate_t) :: new_mass_rate

        new_mass_rate%kilograms_per_second = &
                dble(multiplier) * mass_rate%kilograms_per_second
    end function

    elemental function mass_rate_times_double( &
            mass_rate, multiplier) result(new_mass_rate)
        class(mass_rate_t), intent(in) :: mass_rate
        double precision, intent(in) :: multiplier
        type(mass_rate_t) :: new_mass_rate

        new_mass_rate%kilograms_per_second = &
                mass_rate%kilograms_per_second * multiplier
    end function

    elemental function mass_rate_times_integer( &
            mass_rate, multiplier) result(new_mass_rate)
        class(mass_rate_t), intent(in) :: mass_rate
        integer, intent(in) :: multiplier
        type(mass_rate_t) :: new_mass_rate

        new_mass_rate%kilograms_per_second = &
                mass_rate%kilograms_per_second * dble(multiplier)
    end function

    elemental function mass_rate_divided_by_double( &
            mass_rate, divisor) result(new_mass_rate)
        class(mass_rate_t), intent(in) :: mass_rate
        double precision, intent(in) :: divisor
        type(mass_rate_t) :: new_mass_rate

        new_mass_rate%kilograms_per_second = &
                mass_rate%kilograms_per_second / divisor
    end function

    elemental function mass_rate_divided_by_integer( &
            mass_rate, divisor) result(new_mass_rate)
        class(mass_rate_t), intent(in) :: mass_rate
        integer, intent(in) :: divisor
        type(mass_rate_t) :: new_mass_rate

        new_mass_rate%kilograms_per_second = &
                mass_rate%kilograms_per_second / dble(divisor)
    end function

    elemental function mass_rate_divided_by_mass_rate( &
            numerator, denomenator) result(ratio)
        class(mass_rate_t), intent(in) :: numerator
        type(mass_rate_t), intent(in) :: denomenator
        double precision :: ratio

        ratio = numerator%kilograms_per_second / denomenator%kilograms_per_second
    end function

    elemental function mass_rate_plus_mass_rate( &
            lhs, rhs) result(sum_)
        class(mass_rate_t), intent(in) :: lhs
        type(mass_rate_t), intent(in) :: rhs
        type(mass_rate_t) :: sum_

        sum_%kilograms_per_second = lhs%kilograms_per_second + rhs%kilograms_per_second
    end function

    elemental function negate_mass_rate(self) result(negated)
        class(mass_rate_t), intent(in) :: self
        type(mass_rate_t) :: negated

        negated%kilograms_per_second = -self%kilograms_per_second
    end function

    elemental function mass_rate_minus_mass_rate( &
            lhs, rhs) result(difference)
        class(mass_rate_t), intent(in) :: lhs
        type(mass_rate_t), intent(in) :: rhs
        type(mass_rate_t) :: difference

        difference%kilograms_per_second = lhs%kilograms_per_second - rhs%kilograms_per_second
    end function

    pure function abs_mass_rate(mass_rate) result(abs_)
        type(mass_rate_t), intent(in) :: mass_rate
        type(mass_rate_t) :: abs_

        abs_%kilograms_per_second = abs(mass_rate%kilograms_per_second)
    end function

    pure function sum_mass_rate(mass_rates) result(sum_)
        type(mass_rate_t), intent(in) :: mass_rates(:)
        type(mass_rate_t) :: sum_

        sum_%kilograms_per_second = sum(mass_rates%kilograms_per_second)
    end function

    elemental function greater_than(lhs, rhs)
        class(mass_rate_t), intent(in) :: lhs
        type(mass_rate_t), intent(in) :: rhs
        logical :: greater_than

        greater_than = lhs%kilograms_per_second > rhs%kilograms_per_second
    end function

    elemental function less_than(lhs, rhs)
        class(mass_rate_t), intent(in) :: lhs
        type(mass_rate_t), intent(in) :: rhs
        logical :: less_than

        less_than = lhs%kilograms_per_second < rhs%kilograms_per_second
    end function

    elemental function greater_than_or_equal(lhs, rhs)
        class(mass_rate_t), intent(in) :: lhs
        type(mass_rate_t), intent(in) :: rhs
        logical :: greater_than_or_equal

        greater_than_or_equal = lhs%kilograms_per_second >= rhs%kilograms_per_second
    end function

    elemental function less_than_or_equal(lhs, rhs)
        class(mass_rate_t), intent(in) :: lhs
        type(mass_rate_t), intent(in) :: rhs
        logical :: less_than_or_equal

        less_than_or_equal = lhs%kilograms_per_second <= rhs%kilograms_per_second
    end function

    elemental function equal_(lhs, rhs)
        class(mass_rate_t), intent(in) :: lhs
        type(mass_rate_t), intent(in) :: rhs
        logical :: equal_

        equal_ = lhs%kilograms_per_second .safeEq. rhs%kilograms_per_second
    end function

    elemental function equal_within_absolute_(lhs, rhs, within)
        class(mass_rate_t), intent(in) :: lhs
        type(mass_rate_t), intent(in) :: rhs
        type(mass_rate_t), intent(in) :: within
        logical :: equal_within_absolute_

        equal_within_absolute_ = equal_within_absolute( &
                lhs%kilograms_per_second, rhs%kilograms_per_second, within%kilograms_per_second)
    end function

    elemental function equal_within_relative_(lhs, rhs, within)
        class(mass_rate_t), intent(in) :: lhs
        type(mass_rate_t), intent(in) :: rhs
        double precision, intent(in) :: within
        logical :: equal_within_relative_

        equal_within_relative_ = equal_within_relative( &
                lhs%kilograms_per_second, rhs%kilograms_per_second, within)
    end function

    elemental function not_equal(lhs, rhs)
        class(mass_rate_t), intent(in) :: lhs
        type(mass_rate_t), intent(in) :: rhs
        logical :: not_equal

        not_equal = .not. lhs == rhs
    end function

    elemental function to_string_full_precision(self) result(string)
        class(mass_rate_t), intent(in) :: self
        type(varying_string) :: string

        string = self%to_string_in(DEFAULT_OUTPUT_UNITS)
    end function

    elemental function to_string_with_precision(self, significant_digits) result(string)
        class(mass_rate_t), intent(in) :: self
        integer, intent(in) :: significant_digits
        type(varying_string) :: string

        string = self%to_string_in(DEFAULT_OUTPUT_UNITS, significant_digits)
    end function

    elemental function to_string_in_full_precision(self, units) result(string)
        class(mass_rate_t), intent(in) :: self
        class(mass_rate_unit_t), intent(in) :: units
        type(varying_string) :: string

        string = units%to_string(to_string(self.in.units))
    end function

    elemental function to_string_in_with_precision( &
            self, units, significant_digits) result(string)
        class(mass_rate_t), intent(in) :: self
        class(mass_rate_unit_t), intent(in) :: units
        integer, intent(in) :: significant_digits
        type(varying_string) :: string

        string = units%to_string(to_string(self.in.units, significant_digits))
    end function

    function fallible_mass_rate_from_mass_rate( &
            mass_rate) result(fallible_mass_rate)
        type(mass_rate_t), intent(in) :: mass_rate
        type(fallible_mass_rate_t) :: fallible_mass_rate

        fallible_mass_rate%mass_rate_ = mass_rate
    end function

    function fallible_mass_rate_from_errors( &
            errors) result(fallible_mass_rate)
        type(error_list_t), intent(in) :: errors
        type(fallible_mass_rate_t) :: fallible_mass_rate

        fallible_mass_rate%errors_ = errors
    end function

    function fallible_mass_rate_from_fallible_mass_rate( &
            fallible_mass_rate, &
            module_, &
            procedure_) &
            result(new_fallible_mass_rate)
        type(fallible_mass_rate_t), intent(in) :: fallible_mass_rate
        type(module_t), intent(in) :: module_
        type(procedure_t), intent(in) :: procedure_
        type(fallible_mass_rate_t) :: new_fallible_mass_rate

        if (fallible_mass_rate%failed()) then
            new_fallible_mass_rate%errors_ = error_list_t( &
                    fallible_mass_rate%errors_, module_, procedure_)
        else
            new_fallible_mass_rate%mass_rate_ = fallible_mass_rate%mass_rate_
        end if
    end function

    elemental function fallible_mass_rate_failed( &
            self) result(failed)
        class(fallible_mass_rate_t), intent(in) :: self
        logical :: failed

        failed = self%errors_%has_any()
    end function

    elemental function fallible_mass_rate_mass_rate( &
            self) result(mass_rate)
        class(fallible_mass_rate_t), intent(in) :: self
        type(mass_rate_t) :: mass_rate

        mass_rate = self%mass_rate_
    end function

    impure elemental function fallible_mass_rate_errors( &
            self) result(errors)
        class(fallible_mass_rate_t), intent(in) :: self
        type(error_list_t) :: errors

        errors = self%errors_
    end function

    function fallible_mass_rate_unit_from_unit( &
            unit) result(fallible_mass_rate_unit)
        class(mass_rate_unit_t), intent(in) :: unit
        type(fallible_mass_rate_unit_t) :: fallible_mass_rate_unit

        allocate(fallible_mass_rate_unit%unit_, source = unit)
    end function

    function fallible_mass_rate_unit_from_errors( &
            errors) result(fallible_mass_rate_unit)
        type(error_list_t), intent(in) :: errors
        type(fallible_mass_rate_unit_t) :: fallible_mass_rate_unit

        fallible_mass_rate_unit%errors_ = errors
    end function

    function fallible_mass_rate_unit_from_fallible_mass_rate_unit( &
            fallible_mass_rate_unit, &
            module_, &
            procedure_) &
            result(new_fallible_mass_rate_unit)
        type(fallible_mass_rate_unit_t), intent(in) :: fallible_mass_rate_unit
        type(module_t), intent(in) :: module_
        type(procedure_t), intent(in) :: procedure_
        type(fallible_mass_rate_unit_t) :: new_fallible_mass_rate_unit

        if (fallible_mass_rate_unit%failed()) then
            new_fallible_mass_rate_unit%errors_ = error_list_t( &
                    fallible_mass_rate_unit%errors_, module_, procedure_)
        else
            allocate(new_fallible_mass_rate_unit%unit_, source = &
                    fallible_mass_rate_unit%unit_)
        end if
    end function

    elemental function fallible_mass_rate_unit_failed( &
            self) result(failed)
        class(fallible_mass_rate_unit_t), intent(in) :: self
        logical :: failed

        failed = self%errors_%has_any()
    end function

    function fallible_mass_rate_unit_unit( &
            self) result(unit)
        class(fallible_mass_rate_unit_t), intent(in) :: self
        class(mass_rate_unit_t), allocatable :: unit

        allocate(unit, source = self%unit_)
    end function

    impure elemental function fallible_mass_rate_unit_errors( &
            self) result(errors)
        class(fallible_mass_rate_unit_t), intent(in) :: self
        type(error_list_t) :: errors

        errors = self%errors_
    end function

    elemental function simple_unit_to_string(self) result(string)
        class(mass_rate_simple_unit_t), intent(in) :: self
        type(varying_string) :: string

        string = trim(self%symbol)
    end function

    pure function simple_value_to_string(self, value_) result(string)
        class(mass_rate_simple_unit_t), intent(in) :: self
        type(varying_string), intent(in) :: value_
        type(varying_string) :: string

        string = value_ // " " // self%to_string()
    end function

    function simple_parse_as(self, string) result(fallible_mass_rate)
        class(mass_rate_simple_unit_t), intent(in) :: self
        type(varying_string), intent(in) :: string
        type(fallible_mass_rate_t) :: fallible_mass_rate

        type(parse_result_t) :: parse_result

        parse_result = parse_with(the_parser, string)
        if (parse_result%ok) then
            select type (the_number => parse_result%parsed)
            type is (parsed_rational_t)
                fallible_mass_rate = fallible_mass_rate_t(the_number%value_.unit.self)
            end select
        else
            fallible_mass_rate = fallible_mass_rate_t(error_list_t(fatal_t( &
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

    function parse_mass_rate_unit_c(string) result(fallible_mass_rate_unit)
        character(len=*), intent(in) :: string
        type(fallible_mass_rate_unit_t) :: fallible_mass_rate_unit

        fallible_mass_rate_unit = fallible_mass_rate_unit_t( &
                parse_mass_rate_unit(var_str(string), PROVIDED_UNITS), &
                module_t(MODULE_NAME), &
                procedure_t("parse_mass_rate_unit_c"))
    end function

    function parse_mass_rate_unit_s(string) result(fallible_mass_rate_unit)
        type(varying_string), intent(in) :: string
        type(fallible_mass_rate_unit_t) :: fallible_mass_rate_unit

        fallible_mass_rate_unit = fallible_mass_rate_unit_t( &
                parse_mass_rate_unit(string, PROVIDED_UNITS), &
                module_t(MODULE_NAME), &
                procedure_t("parse_mass_rate_unit_s"))
    end function

    function parse_mass_rate_unit_with_units_c( &
            string, units) result(fallible_mass_rate_unit)
        character(len=*), intent(in) :: string
        class(mass_rate_unit_t), intent(in) :: units(:)
        type(fallible_mass_rate_unit_t) :: fallible_mass_rate_unit

        fallible_mass_rate_unit = fallible_mass_rate_unit_t( &
                parse_mass_rate_unit(var_str(string), units), &
                module_t(MODULE_NAME), &
                procedure_t("parse_mass_rate_unit_with_units_c"))
    end function

    function parse_mass_rate_unit_with_units_s( &
            string, units) result(fallible_mass_rate_unit)
        type(varying_string), intent(in) :: string
        class(mass_rate_unit_t), intent(in) :: units(:)
        type(fallible_mass_rate_unit_t) :: fallible_mass_rate_unit

        integer :: i
        type(varying_string), allocatable :: unit_strings(:)

        do i = 1, size(units)
            if (string == units(i)%to_string()) then
                fallible_mass_rate_unit = fallible_mass_rate_unit_t(units(i))
                return
            end if
        end do
        allocate(unit_strings(size(units)))
        do i = 1, size(units)
            unit_strings(i) = units(i)%to_string()
        end do
        fallible_mass_rate_unit = fallible_mass_rate_unit_t(error_list_t(fatal_t( &
                UNKNOWN_UNIT, &
                module_t(MODULE_NAME), &
                procedure_t("parse_mass_rate_unit_with_units_s"), &
                '"' // string // '", known units: [' // join(unit_strings, ', ') // ']')))
    end function
end module
