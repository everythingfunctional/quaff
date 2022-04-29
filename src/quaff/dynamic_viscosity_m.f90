module quaff_dynamic_viscosity_m
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
            parse_rational, &
            parse_string, &
            parse_with, &
            then_drop
    use quaff_conversion_factors_m, only: MEGAPASCAL_SECONDS_PER_PASCAL_SECOND
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
            dynamic_viscosity_t, &
            fallible_dynamic_viscosity_t, &
            dynamic_viscosity_unit_t, &
            fallible_dynamic_viscosity_unit_t, &
            dynamic_viscosity_simple_unit_t, &
            operator(.unit.), &
            parse_dynamic_viscosity, &
            parse_dynamic_viscosity_unit, &
            sum, &
            DEFAULT_OUTPUT_UNITS, &
            PROVIDED_UNITS, &
            MEGAPASCAL_SECONDS, &
            PASCAL_SECONDS

    type :: dynamic_viscosity_t
        double precision :: pascal_seconds
    contains
        private
        procedure :: to_units
        generic, public :: operator(.in.) => to_units
        procedure, pass(dynamic_viscosity) :: double_times_dynamic_viscosity
        procedure, pass(dynamic_viscosity) :: integer_times_dynamic_viscosity
        procedure :: dynamic_viscosity_times_double
        procedure :: dynamic_viscosity_times_integer
        generic, public :: operator(*) => &
                double_times_dynamic_viscosity, &
                integer_times_dynamic_viscosity, &
                dynamic_viscosity_times_double, &
                dynamic_viscosity_times_integer
        procedure :: dynamic_viscosity_divided_by_double
        procedure :: dynamic_viscosity_divided_by_integer
        procedure :: dynamic_viscosity_divided_by_dynamic_viscosity
        generic, public :: operator(/) => &
                dynamic_viscosity_divided_by_double, &
                dynamic_viscosity_divided_by_integer, &
                dynamic_viscosity_divided_by_dynamic_viscosity
        procedure :: dynamic_viscosity_plus_dynamic_viscosity
        generic, public :: operator(+) => dynamic_viscosity_plus_dynamic_viscosity
        procedure :: dynamic_viscosity_minus_dynamic_viscosity
        generic, public :: operator(-) => dynamic_viscosity_minus_dynamic_viscosity
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

    type :: fallible_dynamic_viscosity_t
        private
        type(dynamic_viscosity_t) :: dynamic_viscosity_ = dynamic_viscosity_t(0.0d0)
        type(error_list_t) :: errors_
    contains
        private
        procedure, public :: failed => fallible_dynamic_viscosity_failed
        procedure, public :: dynamic_viscosity => fallible_dynamic_viscosity_dynamic_viscosity
        procedure, public :: errors => fallible_dynamic_viscosity_errors
    end type

    type, abstract :: dynamic_viscosity_unit_t
        double precision :: conversion_factor
    contains
        procedure(unit_to_string_i), deferred :: unit_to_string
        procedure(value_to_string_i), deferred :: value_to_string
        generic :: to_string => unit_to_string, value_to_string
        procedure(parse_as_i), deferred :: parse_as
    end type

    type :: fallible_dynamic_viscosity_unit_t
        private
        class(dynamic_viscosity_unit_t), allocatable :: unit_
        type(error_list_t) :: errors_
    contains
        private
        procedure, public :: failed => fallible_dynamic_viscosity_unit_failed
        procedure, public :: unit => fallible_dynamic_viscosity_unit_unit
        procedure, public :: errors => fallible_dynamic_viscosity_unit_errors
    end type

    type, extends(dynamic_viscosity_unit_t) :: dynamic_viscosity_simple_unit_t
        character(len=20) :: symbol
    contains
        procedure :: unit_to_string => simple_unit_to_string
        procedure :: value_to_string => simple_value_to_string
        procedure :: parse_as => simple_parse_as
    end type

    abstract interface
        elemental function unit_to_string_i(self) result(string)
            import :: dynamic_viscosity_unit_t, varying_string

            implicit none

            class(dynamic_viscosity_unit_t), intent(in) :: self
            type(varying_string) :: string
        end function

        pure function value_to_string_i(self, value_) result(string)
            import :: dynamic_viscosity_unit_t, varying_string

            implicit none

            class(dynamic_viscosity_unit_t), intent(in) :: self
            type(varying_string), intent(in) :: value_
            type(varying_string) :: string
        end function

        function parse_as_i(self, string) result(fallible_dynamic_viscosity)
            import :: dynamic_viscosity_unit_t, fallible_dynamic_viscosity_t, varying_string

            implicit none

            class(dynamic_viscosity_unit_t), intent(in) :: self
            type(varying_string), intent(in) :: string
            type(fallible_dynamic_viscosity_t) :: fallible_dynamic_viscosity
        end function
    end interface

    interface operator(.unit.)
        module procedure from_units
    end interface

    interface fallible_dynamic_viscosity_t
        module procedure fallible_dynamic_viscosity_from_dynamic_viscosity
        module procedure fallible_dynamic_viscosity_from_errors
        module procedure fallible_dynamic_viscosity_from_fallible_dynamic_viscosity
    end interface

    interface fallible_dynamic_viscosity_unit_t
        module procedure fallible_dynamic_viscosity_unit_from_unit
        module procedure fallible_dynamic_viscosity_unit_from_errors
        module procedure fallible_unit_from_fallible_unit
    end interface

    interface parse_dynamic_viscosity
        module procedure parse_dynamic_viscosity_c
        module procedure parse_dynamic_viscosity_s
        module procedure parse_dynamic_viscosity_with_units_c
        module procedure parse_dynamic_viscosity_with_units_s
    end interface

    interface parse_dynamic_viscosity_unit
        module procedure parse_dynamic_viscosity_unit_c
        module procedure parse_dynamic_viscosity_unit_s
        module procedure parse_dynamic_viscosity_unit_with_units_c
        module procedure parse_dynamic_viscosity_unit_with_units_s
    end interface

    interface sum
        module procedure sum_dynamic_viscosity
    end interface

    type(dynamic_viscosity_simple_unit_t), parameter :: MEGAPASCAL_SECONDS = &
            dynamic_viscosity_simple_unit_t( &
                    conversion_factor = MEGAPASCAL_SECONDS_PER_PASCAL_SECOND, &
                    symbol = "MPa s")
    type(dynamic_viscosity_simple_unit_t), parameter :: PASCAL_SECONDS = &
            dynamic_viscosity_simple_unit_t( &
                    conversion_factor = 1.0d0, &
                    symbol = "Pa s")

    type(dynamic_viscosity_simple_unit_t) :: DEFAULT_OUTPUT_UNITS = PASCAL_SECONDS

    type(dynamic_viscosity_simple_unit_t), parameter :: PROVIDED_UNITS(*) = &
    [MEGAPASCAL_SECONDS, PASCAL_SECONDS]

    character(len=*), parameter :: MODULE_NAME = "quaff_dynamic_viscosity_m"
contains
    function parse_dynamic_viscosity_c(string) result(fallible_dynamic_viscosity)
        character(len=*), intent(in) :: string
        type(fallible_dynamic_viscosity_t) :: fallible_dynamic_viscosity

        fallible_dynamic_viscosity = fallible_dynamic_viscosity_t( &
                parse_dynamic_viscosity(var_str(string), PROVIDED_UNITS), &
                module_t(MODULE_NAME), &
                procedure_t("parse_dynamic_viscosity_c"))
    end function

    function parse_dynamic_viscosity_s(string) result(fallible_dynamic_viscosity)
        type(varying_string), intent(in) :: string
        type(fallible_dynamic_viscosity_t) :: fallible_dynamic_viscosity

        fallible_dynamic_viscosity = fallible_dynamic_viscosity_t( &
                parse_dynamic_viscosity(string, PROVIDED_UNITS), &
                module_t(MODULE_NAME), &
                procedure_t("parse_dynamic_viscosity_s"))
    end function

    function parse_dynamic_viscosity_with_units_c( &
            string, units) result(fallible_dynamic_viscosity)
        character(len=*), intent(in) :: string
        class(dynamic_viscosity_unit_t), intent(in) :: units(:)
        type(fallible_dynamic_viscosity_t) :: fallible_dynamic_viscosity

        fallible_dynamic_viscosity = fallible_dynamic_viscosity_t( &
                parse_dynamic_viscosity(var_str(string), units), &
                module_t(MODULE_NAME), &
                procedure_t("parse_dynamic_viscosity_with_units_c"))
    end function

    function parse_dynamic_viscosity_with_units_s( &
            string, units) result(fallible_dynamic_viscosity)
        type(varying_string), intent(in) :: string
        class(dynamic_viscosity_unit_t), intent(in) :: units(:)
        type(fallible_dynamic_viscosity_t) :: fallible_dynamic_viscosity

        type(fallible_dynamic_viscosity_t) :: all_attempts(size(units))
        integer :: i

        do i = 1, size(units)
            all_attempts(i) = units(i)%parse_as(string)
            if (.not. all_attempts(i)%failed()) then
                fallible_dynamic_viscosity = all_attempts(i)
                return
            end if
        end do
        fallible_dynamic_viscosity = fallible_dynamic_viscosity_t(error_list_t( &
                all_attempts%errors(), &
                module_t(MODULE_NAME), &
                procedure_t("parse_dynamic_viscosity_with_units_s")))
    end function

    elemental function from_units(value_, units) result(dynamic_viscosity)
        double precision, intent(in) :: value_
        class(dynamic_viscosity_unit_t), intent(in) :: units
        type(dynamic_viscosity_t) :: dynamic_viscosity

        dynamic_viscosity%pascal_seconds = value_ / units%conversion_factor
    end function

    elemental function to_units(self, units) result(dynamic_viscosity)
        class(dynamic_viscosity_t), intent(in) :: self
        class(dynamic_viscosity_unit_t), intent(in) :: units
        double precision :: dynamic_viscosity

        dynamic_viscosity = self%pascal_seconds * units%conversion_factor
    end function

    elemental function double_times_dynamic_viscosity( &
            multiplier, dynamic_viscosity) result(new_dynamic_viscosity)
        double precision, intent(in) :: multiplier
        class(dynamic_viscosity_t), intent(in) :: dynamic_viscosity
        type(dynamic_viscosity_t) :: new_dynamic_viscosity

        new_dynamic_viscosity%pascal_seconds = &
                multiplier * dynamic_viscosity%pascal_seconds
    end function

    elemental function integer_times_dynamic_viscosity( &
            multiplier, dynamic_viscosity) result(new_dynamic_viscosity)
        integer, intent(in) :: multiplier
        class(dynamic_viscosity_t), intent(in) :: dynamic_viscosity
        type(dynamic_viscosity_t) :: new_dynamic_viscosity

        new_dynamic_viscosity%pascal_seconds = &
                dble(multiplier) * dynamic_viscosity%pascal_seconds
    end function

    elemental function dynamic_viscosity_times_double( &
            dynamic_viscosity, multiplier) result(new_dynamic_viscosity)
        class(dynamic_viscosity_t), intent(in) :: dynamic_viscosity
        double precision, intent(in) :: multiplier
        type(dynamic_viscosity_t) :: new_dynamic_viscosity

        new_dynamic_viscosity%pascal_seconds = &
                dynamic_viscosity%pascal_seconds * multiplier
    end function

    elemental function dynamic_viscosity_times_integer( &
            dynamic_viscosity, multiplier) result(new_dynamic_viscosity)
        class(dynamic_viscosity_t), intent(in) :: dynamic_viscosity
        integer, intent(in) :: multiplier
        type(dynamic_viscosity_t) :: new_dynamic_viscosity

        new_dynamic_viscosity%pascal_seconds = &
                dynamic_viscosity%pascal_seconds * dble(multiplier)
    end function

    elemental function dynamic_viscosity_divided_by_double( &
            dynamic_viscosity, divisor) result(new_dynamic_viscosity)
        class(dynamic_viscosity_t), intent(in) :: dynamic_viscosity
        double precision, intent(in) :: divisor
        type(dynamic_viscosity_t) :: new_dynamic_viscosity

        new_dynamic_viscosity%pascal_seconds = &
                dynamic_viscosity%pascal_seconds / divisor
    end function

    elemental function dynamic_viscosity_divided_by_integer( &
            dynamic_viscosity, divisor) result(new_dynamic_viscosity)
        class(dynamic_viscosity_t), intent(in) :: dynamic_viscosity
        integer, intent(in) :: divisor
        type(dynamic_viscosity_t) :: new_dynamic_viscosity

        new_dynamic_viscosity%pascal_seconds = &
                dynamic_viscosity%pascal_seconds / dble(divisor)
    end function

    elemental function dynamic_viscosity_divided_by_dynamic_viscosity( &
            numerator, denomenator) result(ratio)
        class(dynamic_viscosity_t), intent(in) :: numerator
        type(dynamic_viscosity_t), intent(in) :: denomenator
        double precision :: ratio

        ratio = numerator%pascal_seconds / denomenator%pascal_seconds
    end function

    elemental function dynamic_viscosity_plus_dynamic_viscosity( &
            lhs, rhs) result(sum_)
        class(dynamic_viscosity_t), intent(in) :: lhs
        type(dynamic_viscosity_t), intent(in) :: rhs
        type(dynamic_viscosity_t) :: sum_

        sum_%pascal_seconds = lhs%pascal_seconds + rhs%pascal_seconds
    end function

    elemental function dynamic_viscosity_minus_dynamic_viscosity( &
            lhs, rhs) result(difference)
        class(dynamic_viscosity_t), intent(in) :: lhs
        type(dynamic_viscosity_t), intent(in) :: rhs
        type(dynamic_viscosity_t) :: difference

        difference%pascal_seconds = lhs%pascal_seconds - rhs%pascal_seconds
    end function

    pure function sum_dynamic_viscosity(dynamic_viscositys) result(sum_)
        type(dynamic_viscosity_t), intent(in) :: dynamic_viscositys(:)
        type(dynamic_viscosity_t) :: sum_

        sum_%pascal_seconds = sum(dynamic_viscositys%pascal_seconds)
    end function

    elemental function greater_than(lhs, rhs)
        class(dynamic_viscosity_t), intent(in) :: lhs
        type(dynamic_viscosity_t), intent(in) :: rhs
        logical :: greater_than

        greater_than = lhs%pascal_seconds > rhs%pascal_seconds
    end function

    elemental function less_than(lhs, rhs)
        class(dynamic_viscosity_t), intent(in) :: lhs
        type(dynamic_viscosity_t), intent(in) :: rhs
        logical :: less_than

        less_than = lhs%pascal_seconds < rhs%pascal_seconds
    end function

    elemental function greater_than_or_equal(lhs, rhs)
        class(dynamic_viscosity_t), intent(in) :: lhs
        type(dynamic_viscosity_t), intent(in) :: rhs
        logical :: greater_than_or_equal

        greater_than_or_equal = lhs%pascal_seconds >= rhs%pascal_seconds
    end function

    elemental function less_than_or_equal(lhs, rhs)
        class(dynamic_viscosity_t), intent(in) :: lhs
        type(dynamic_viscosity_t), intent(in) :: rhs
        logical :: less_than_or_equal

        less_than_or_equal = lhs%pascal_seconds <= rhs%pascal_seconds
    end function

    elemental function equal_(lhs, rhs)
        class(dynamic_viscosity_t), intent(in) :: lhs
        type(dynamic_viscosity_t), intent(in) :: rhs
        logical :: equal_

        equal_ = lhs%pascal_seconds .safeEq. rhs%pascal_seconds
    end function

    elemental function equal_within_absolute_(lhs, rhs, within)
        class(dynamic_viscosity_t), intent(in) :: lhs
        type(dynamic_viscosity_t), intent(in) :: rhs
        type(dynamic_viscosity_t), intent(in) :: within
        logical :: equal_within_absolute_

        equal_within_absolute_ = equal_within_absolute( &
                lhs%pascal_seconds, rhs%pascal_seconds, within%pascal_seconds)
    end function

    elemental function equal_within_relative_(lhs, rhs, within)
        class(dynamic_viscosity_t), intent(in) :: lhs
        type(dynamic_viscosity_t), intent(in) :: rhs
        double precision, intent(in) :: within
        logical :: equal_within_relative_

        equal_within_relative_ = equal_within_relative( &
                lhs%pascal_seconds, rhs%pascal_seconds, within)
    end function

    elemental function not_equal(lhs, rhs)
        class(dynamic_viscosity_t), intent(in) :: lhs
        type(dynamic_viscosity_t), intent(in) :: rhs
        logical :: not_equal

        not_equal = .not. lhs == rhs
    end function

    elemental function to_string_full_precision(self) result(string)
        class(dynamic_viscosity_t), intent(in) :: self
        type(varying_string) :: string

        string = self%to_string_in(DEFAULT_OUTPUT_UNITS)
    end function

    elemental function to_string_with_precision(self, significant_digits) result(string)
        class(dynamic_viscosity_t), intent(in) :: self
        integer, intent(in) :: significant_digits
        type(varying_string) :: string

        string = self%to_string_in(DEFAULT_OUTPUT_UNITS, significant_digits)
    end function

    elemental function to_string_in_full_precision(self, units) result(string)
        class(dynamic_viscosity_t), intent(in) :: self
        class(dynamic_viscosity_unit_t), intent(in) :: units
        type(varying_string) :: string

        string = units%to_string(to_string(self.in.units))
    end function

    elemental function to_string_in_with_precision( &
            self, units, significant_digits) result(string)
        class(dynamic_viscosity_t), intent(in) :: self
        class(dynamic_viscosity_unit_t), intent(in) :: units
        integer, intent(in) :: significant_digits
        type(varying_string) :: string

        string = units%to_string(to_string(self.in.units, significant_digits))
    end function

    function fallible_dynamic_viscosity_from_dynamic_viscosity( &
            dynamic_viscosity) result(fallible_dynamic_viscosity)
        type(dynamic_viscosity_t), intent(in) :: dynamic_viscosity
        type(fallible_dynamic_viscosity_t) :: fallible_dynamic_viscosity

        fallible_dynamic_viscosity%dynamic_viscosity_ = dynamic_viscosity
    end function

    function fallible_dynamic_viscosity_from_errors( &
            errors) result(fallible_dynamic_viscosity)
        type(error_list_t), intent(in) :: errors
        type(fallible_dynamic_viscosity_t) :: fallible_dynamic_viscosity

        fallible_dynamic_viscosity%errors_ = errors
    end function

    function fallible_dynamic_viscosity_from_fallible_dynamic_viscosity( &
            fallible_dynamic_viscosity, &
            module_, &
            procedure_) &
            result(new_fallible_dynamic_viscosity)
        type(fallible_dynamic_viscosity_t), intent(in) :: fallible_dynamic_viscosity
        type(module_t), intent(in) :: module_
        type(procedure_t), intent(in) :: procedure_
        type(fallible_dynamic_viscosity_t) :: new_fallible_dynamic_viscosity

        if (fallible_dynamic_viscosity%failed()) then
            new_fallible_dynamic_viscosity%errors_ = error_list_t( &
                    fallible_dynamic_viscosity%errors_, module_, procedure_)
        else
            new_fallible_dynamic_viscosity%dynamic_viscosity_ = fallible_dynamic_viscosity%dynamic_viscosity_
        end if
    end function

    elemental function fallible_dynamic_viscosity_failed( &
            self) result(failed)
        class(fallible_dynamic_viscosity_t), intent(in) :: self
        logical :: failed

        failed = self%errors_%has_any()
    end function

    elemental function fallible_dynamic_viscosity_dynamic_viscosity( &
            self) result(dynamic_viscosity)
        class(fallible_dynamic_viscosity_t), intent(in) :: self
        type(dynamic_viscosity_t) :: dynamic_viscosity

        dynamic_viscosity = self%dynamic_viscosity_
    end function

    impure elemental function fallible_dynamic_viscosity_errors( &
            self) result(errors)
        class(fallible_dynamic_viscosity_t), intent(in) :: self
        type(error_list_t) :: errors

        errors = self%errors_
    end function

    function fallible_dynamic_viscosity_unit_from_unit( &
            unit) result(fallible_dynamic_viscosity_unit)
        class(dynamic_viscosity_unit_t), intent(in) :: unit
        type(fallible_dynamic_viscosity_unit_t) :: fallible_dynamic_viscosity_unit

        allocate(fallible_dynamic_viscosity_unit%unit_, source = unit)
    end function

    function fallible_dynamic_viscosity_unit_from_errors( &
            errors) result(fallible_dynamic_viscosity_unit)
        type(error_list_t), intent(in) :: errors
        type(fallible_dynamic_viscosity_unit_t) :: fallible_dynamic_viscosity_unit

        fallible_dynamic_viscosity_unit%errors_ = errors
    end function

    function fallible_unit_from_fallible_unit( &
            fallible_dynamic_viscosity_unit, &
            module_, &
            procedure_) &
            result(new_fallible_dynamic_viscosity_unit)
        type(fallible_dynamic_viscosity_unit_t), intent(in) :: fallible_dynamic_viscosity_unit
        type(module_t), intent(in) :: module_
        type(procedure_t), intent(in) :: procedure_
        type(fallible_dynamic_viscosity_unit_t) :: new_fallible_dynamic_viscosity_unit

        if (fallible_dynamic_viscosity_unit%failed()) then
            new_fallible_dynamic_viscosity_unit%errors_ = error_list_t( &
                    fallible_dynamic_viscosity_unit%errors_, module_, procedure_)
        else
            allocate(new_fallible_dynamic_viscosity_unit%unit_, source = &
                    fallible_dynamic_viscosity_unit%unit_)
        end if
    end function

    elemental function fallible_dynamic_viscosity_unit_failed( &
            self) result(failed)
        class(fallible_dynamic_viscosity_unit_t), intent(in) :: self
        logical :: failed

        failed = self%errors_%has_any()
    end function

    function fallible_dynamic_viscosity_unit_unit( &
            self) result(unit)
        class(fallible_dynamic_viscosity_unit_t), intent(in) :: self
        class(dynamic_viscosity_unit_t), allocatable :: unit

        allocate(unit, source = self%unit_)
    end function

    impure elemental function fallible_dynamic_viscosity_unit_errors( &
            self) result(errors)
        class(fallible_dynamic_viscosity_unit_t), intent(in) :: self
        type(error_list_t) :: errors

        errors = self%errors_
    end function

    elemental function simple_unit_to_string(self) result(string)
        class(dynamic_viscosity_simple_unit_t), intent(in) :: self
        type(varying_string) :: string

        string = trim(self%symbol)
    end function

    pure function simple_value_to_string(self, value_) result(string)
        class(dynamic_viscosity_simple_unit_t), intent(in) :: self
        type(varying_string), intent(in) :: value_
        type(varying_string) :: string

        string = value_ // " " // self%to_string()
    end function

    function simple_parse_as(self, string) result(fallible_dynamic_viscosity)
        class(dynamic_viscosity_simple_unit_t), intent(in) :: self
        type(varying_string), intent(in) :: string
        type(fallible_dynamic_viscosity_t) :: fallible_dynamic_viscosity

        type(parse_result_t) :: parse_result

        parse_result = parse_with(the_parser, string)
        if (parse_result%ok) then
            select type (the_number => parse_result%parsed)
            type is (parsed_rational_t)
                fallible_dynamic_viscosity = fallible_dynamic_viscosity_t(the_number%value_.unit.self)
            end select
        else
            fallible_dynamic_viscosity = fallible_dynamic_viscosity_t(error_list_t(fatal_t( &
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

            result_ = parse_string(trim(self%symbol), state_)
        end function
    end function

    function parse_dynamic_viscosity_unit_c(string) result(fallible_dynamic_viscosity_unit)
        character(len=*), intent(in) :: string
        type(fallible_dynamic_viscosity_unit_t) :: fallible_dynamic_viscosity_unit

        fallible_dynamic_viscosity_unit = fallible_dynamic_viscosity_unit_t( &
                parse_dynamic_viscosity_unit(var_str(string), PROVIDED_UNITS), &
                module_t(MODULE_NAME), &
                procedure_t("parse_dynamic_viscosity_unit_c"))
    end function

    function parse_dynamic_viscosity_unit_s(string) result(fallible_dynamic_viscosity_unit)
        type(varying_string), intent(in) :: string
        type(fallible_dynamic_viscosity_unit_t) :: fallible_dynamic_viscosity_unit

        fallible_dynamic_viscosity_unit = fallible_dynamic_viscosity_unit_t( &
                parse_dynamic_viscosity_unit(string, PROVIDED_UNITS), &
                module_t(MODULE_NAME), &
                procedure_t("parse_dynamic_viscosity_unit_s"))
    end function

    function parse_dynamic_viscosity_unit_with_units_c( &
            string, units) result(fallible_dynamic_viscosity_unit)
        character(len=*), intent(in) :: string
        class(dynamic_viscosity_unit_t), intent(in) :: units(:)
        type(fallible_dynamic_viscosity_unit_t) :: fallible_dynamic_viscosity_unit

        fallible_dynamic_viscosity_unit = fallible_dynamic_viscosity_unit_t( &
                parse_dynamic_viscosity_unit(var_str(string), units), &
                module_t(MODULE_NAME), &
                procedure_t("parse_dynamic_viscosity_unit_with_units_c"))
    end function

    function parse_dynamic_viscosity_unit_with_units_s( &
            string, units) result(fallible_dynamic_viscosity_unit)
        type(varying_string), intent(in) :: string
        class(dynamic_viscosity_unit_t), intent(in) :: units(:)
        type(fallible_dynamic_viscosity_unit_t) :: fallible_dynamic_viscosity_unit

        integer :: i
        type(varying_string) :: unit_strings(size(units))

        do i = 1, size(units)
            if (string == units(i)%to_string()) then
                fallible_dynamic_viscosity_unit = fallible_dynamic_viscosity_unit_t(units(i))
                return
            end if
        end do
        do i = 1, size(units)
            unit_strings(i) = units(i)%to_string()
        end do
        fallible_dynamic_viscosity_unit = fallible_dynamic_viscosity_unit_t(error_list_t(fatal_t( &
                UNKNOWN_UNIT, &
                module_t(MODULE_NAME), &
                procedure_t("parse_dynamic_viscosity_unit_with_units_s"), &
                '"' // string // '", known units: [' // join(unit_strings, ', ') // ']')))
    end function
end module
