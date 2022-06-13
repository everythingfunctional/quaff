module quaff_burnup_m
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
    use quaff_conversion_factors_m, only: &
            MEGAWATT_DAYS_PER_TON_PER_WATT_SECONDS_PER_KILOGRAM
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
            burnup_t, &
            fallible_burnup_t, &
            burnup_unit_t, &
            fallible_burnup_unit_t, &
            burnup_simple_unit_t, &
            operator(.unit.), &
            parse_burnup, &
            parse_burnup_unit, &
            sum, &
            abs, &
            DEFAULT_OUTPUT_UNITS, &
            PROVIDED_UNITS, &
            MEGAWATT_DAYS_PER_TON, &
            WATT_SECONDS_PER_KILOGRAM

    type :: burnup_t
        double precision :: watt_seconds_per_kilogram
    contains
        private
        procedure :: to_units
        generic, public :: operator(.in.) => to_units
        procedure, pass(burnup) :: double_times_burnup
        procedure, pass(burnup) :: integer_times_burnup
        procedure :: burnup_times_double
        procedure :: burnup_times_integer
        generic, public :: operator(*) => &
                double_times_burnup, &
                integer_times_burnup, &
                burnup_times_double, &
                burnup_times_integer
        procedure :: burnup_divided_by_double
        procedure :: burnup_divided_by_integer
        procedure :: burnup_divided_by_burnup
        generic, public :: operator(/) => &
                burnup_divided_by_double, &
                burnup_divided_by_integer, &
                burnup_divided_by_burnup
        procedure :: burnup_plus_burnup
        generic, public :: operator(+) => burnup_plus_burnup
        procedure :: burnup_minus_burnup
        generic, public :: operator(-) => burnup_minus_burnup
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

    type :: fallible_burnup_t
        private
        type(burnup_t) :: burnup_ = burnup_t(0.0d0)
        type(error_list_t) :: errors_
    contains
        private
        procedure, public :: failed => fallible_burnup_failed
        procedure, public :: burnup => fallible_burnup_burnup
        procedure, public :: errors => fallible_burnup_errors
    end type

    type, abstract :: burnup_unit_t
        double precision :: conversion_factor
    contains
        procedure(unit_to_string_i), deferred :: unit_to_string
        procedure(value_to_string_i), deferred :: value_to_string
        generic :: to_string => unit_to_string, value_to_string
        procedure(parse_as_i), deferred :: parse_as
    end type

    type :: fallible_burnup_unit_t
        private
        class(burnup_unit_t), allocatable :: unit_
        type(error_list_t) :: errors_
    contains
        private
        procedure, public :: failed => fallible_burnup_unit_failed
        procedure, public :: unit => fallible_burnup_unit_unit
        procedure, public :: errors => fallible_burnup_unit_errors
    end type

    type, extends(burnup_unit_t) :: burnup_simple_unit_t
        character(len=20) :: symbol
    contains
        procedure :: unit_to_string => simple_unit_to_string
        procedure :: value_to_string => simple_value_to_string
        procedure :: parse_as => simple_parse_as
    end type

    abstract interface
        elemental function unit_to_string_i(self) result(string)
            import :: burnup_unit_t, varying_string

            implicit none

            class(burnup_unit_t), intent(in) :: self
            type(varying_string) :: string
        end function

        pure function value_to_string_i(self, value_) result(string)
            import :: burnup_unit_t, varying_string

            implicit none

            class(burnup_unit_t), intent(in) :: self
            type(varying_string), intent(in) :: value_
            type(varying_string) :: string
        end function

        function parse_as_i(self, string) result(fallible_burnup)
            import :: burnup_unit_t, fallible_burnup_t, varying_string

            implicit none

            class(burnup_unit_t), intent(in) :: self
            type(varying_string), intent(in) :: string
            type(fallible_burnup_t) :: fallible_burnup
        end function
    end interface

    interface operator(.unit.)
        module procedure from_units
    end interface

    interface fallible_burnup_t
        module procedure fallible_burnup_from_burnup
        module procedure fallible_burnup_from_errors
        module procedure fallible_burnup_from_fallible_burnup
    end interface

    interface fallible_burnup_unit_t
        module procedure fallible_burnup_unit_from_unit
        module procedure fallible_burnup_unit_from_errors
        module procedure fallible_burnup_unit_from_fallible_burnup_unit
    end interface

    interface parse_burnup
        module procedure parse_burnup_c
        module procedure parse_burnup_s
        module procedure parse_burnup_with_units_c
        module procedure parse_burnup_with_units_s
    end interface

    interface parse_burnup_unit
        module procedure parse_burnup_unit_c
        module procedure parse_burnup_unit_s
        module procedure parse_burnup_unit_with_units_c
        module procedure parse_burnup_unit_with_units_s
    end interface

    interface abs
        module procedure abs_burnup
    end interface

    interface sum
        module procedure sum_burnup
    end interface

    type(burnup_simple_unit_t), parameter :: MEGAWATT_DAYS_PER_TON = &
            burnup_simple_unit_t( &
                    conversion_factor = MEGAWATT_DAYS_PER_TON_PER_WATT_SECONDS_PER_KILOGRAM, &
                    symbol = "(MW d)/t")
    type(burnup_simple_unit_t), parameter :: WATT_SECONDS_PER_KILOGRAM = &
            burnup_simple_unit_t( &
                    conversion_factor = 1.0d0, &
                    symbol = "(W s)/kg")

    type(burnup_simple_unit_t) :: DEFAULT_OUTPUT_UNITS = WATT_SECONDS_PER_KILOGRAM

    type(burnup_simple_unit_t), parameter :: PROVIDED_UNITS(*) = &
            [MEGAWATT_DAYS_PER_TON, WATT_SECONDS_PER_KILOGRAM]

    character(len=*), parameter :: MODULE_NAME = "quaff_burnup_m"
contains
    function parse_burnup_c(string) result(fallible_burnup)
        character(len=*), intent(in) :: string
        type(fallible_burnup_t) :: fallible_burnup

        fallible_burnup = fallible_burnup_t( &
                parse_burnup(var_str(string), PROVIDED_UNITS), &
                module_t(MODULE_NAME), &
                procedure_t("parse_burnup_c"))
    end function

    function parse_burnup_s(string) result(fallible_burnup)
        type(varying_string), intent(in) :: string
        type(fallible_burnup_t) :: fallible_burnup

        fallible_burnup = fallible_burnup_t( &
                parse_burnup(string, PROVIDED_UNITS), &
                module_t(MODULE_NAME), &
                procedure_t("parse_burnup_s"))
    end function

    function parse_burnup_with_units_c( &
            string, units) result(fallible_burnup)
        character(len=*), intent(in) :: string
        class(burnup_unit_t), intent(in) :: units(:)
        type(fallible_burnup_t) :: fallible_burnup

        fallible_burnup = fallible_burnup_t( &
                parse_burnup(var_str(string), units), &
                module_t(MODULE_NAME), &
                procedure_t("parse_burnup_with_units_c"))
    end function

    function parse_burnup_with_units_s( &
            string, units) result(fallible_burnup)
        type(varying_string), intent(in) :: string
        class(burnup_unit_t), intent(in) :: units(:)
        type(fallible_burnup_t) :: fallible_burnup

        integer :: i

        do i = 1, size(units)
            fallible_burnup = units(i)%parse_as(string)
            if (.not. fallible_burnup%failed()) return
        end do
        fallible_burnup = fallible_burnup_t(error_list_t(fatal_t( &
                PARSE_ERROR, &
                module_t(MODULE_NAME), &
                procedure_t("parse_burnup_with_units_s"), &
                "Unable to parse '" // string // "' as a burnup_t. Tried with units: " &
                // join(units%to_string(), ", "))))
    end function

    elemental function from_units(value_, units) result(burnup)
        double precision, intent(in) :: value_
        class(burnup_unit_t), intent(in) :: units
        type(burnup_t) :: burnup

        burnup%watt_seconds_per_kilogram = value_ / units%conversion_factor
    end function

    elemental function to_units(self, units) result(burnup)
        class(burnup_t), intent(in) :: self
        class(burnup_unit_t), intent(in) :: units
        double precision :: burnup

        burnup = self%watt_seconds_per_kilogram * units%conversion_factor
    end function

    elemental function double_times_burnup( &
            multiplier, burnup) result(new_burnup)
        double precision, intent(in) :: multiplier
        class(burnup_t), intent(in) :: burnup
        type(burnup_t) :: new_burnup

        new_burnup%watt_seconds_per_kilogram = &
                multiplier * burnup%watt_seconds_per_kilogram
    end function

    elemental function integer_times_burnup( &
            multiplier, burnup) result(new_burnup)
        integer, intent(in) :: multiplier
        class(burnup_t), intent(in) :: burnup
        type(burnup_t) :: new_burnup

        new_burnup%watt_seconds_per_kilogram = &
                dble(multiplier) * burnup%watt_seconds_per_kilogram
    end function

    elemental function burnup_times_double( &
            burnup, multiplier) result(new_burnup)
        class(burnup_t), intent(in) :: burnup
        double precision, intent(in) :: multiplier
        type(burnup_t) :: new_burnup

        new_burnup%watt_seconds_per_kilogram = &
                burnup%watt_seconds_per_kilogram * multiplier
    end function

    elemental function burnup_times_integer( &
            burnup, multiplier) result(new_burnup)
        class(burnup_t), intent(in) :: burnup
        integer, intent(in) :: multiplier
        type(burnup_t) :: new_burnup

        new_burnup%watt_seconds_per_kilogram = &
                burnup%watt_seconds_per_kilogram * dble(multiplier)
    end function

    elemental function burnup_divided_by_double( &
            burnup, divisor) result(new_burnup)
        class(burnup_t), intent(in) :: burnup
        double precision, intent(in) :: divisor
        type(burnup_t) :: new_burnup

        new_burnup%watt_seconds_per_kilogram = &
                burnup%watt_seconds_per_kilogram / divisor
    end function

    elemental function burnup_divided_by_integer( &
            burnup, divisor) result(new_burnup)
        class(burnup_t), intent(in) :: burnup
        integer, intent(in) :: divisor
        type(burnup_t) :: new_burnup

        new_burnup%watt_seconds_per_kilogram = &
                burnup%watt_seconds_per_kilogram / dble(divisor)
    end function

    elemental function burnup_divided_by_burnup( &
            numerator, denomenator) result(ratio)
        class(burnup_t), intent(in) :: numerator
        type(burnup_t), intent(in) :: denomenator
        double precision :: ratio

        ratio = numerator%watt_seconds_per_kilogram / denomenator%watt_seconds_per_kilogram
    end function

    elemental function burnup_plus_burnup( &
            lhs, rhs) result(sum_)
        class(burnup_t), intent(in) :: lhs
        type(burnup_t), intent(in) :: rhs
        type(burnup_t) :: sum_

        sum_%watt_seconds_per_kilogram = lhs%watt_seconds_per_kilogram + rhs%watt_seconds_per_kilogram
    end function

    elemental function burnup_minus_burnup( &
            lhs, rhs) result(difference)
        class(burnup_t), intent(in) :: lhs
        type(burnup_t), intent(in) :: rhs
        type(burnup_t) :: difference

        difference%watt_seconds_per_kilogram = lhs%watt_seconds_per_kilogram - rhs%watt_seconds_per_kilogram
    end function

    pure function abs_burnup(burnup) result(abs_)
        class(burnup_t), intent(in) :: burnup
        type(burnup_t) :: abs_

        abs_%watt_seconds_per_kilogram = abs(burnup%watt_seconds_per_kilogram)
    end function

    pure function sum_burnup(burnups) result(sum_)
        type(burnup_t), intent(in) :: burnups(:)
        type(burnup_t) :: sum_

        sum_%watt_seconds_per_kilogram = sum(burnups%watt_seconds_per_kilogram)
    end function

    elemental function greater_than(lhs, rhs)
        class(burnup_t), intent(in) :: lhs
        type(burnup_t), intent(in) :: rhs
        logical :: greater_than

        greater_than = lhs%watt_seconds_per_kilogram > rhs%watt_seconds_per_kilogram
    end function

    elemental function less_than(lhs, rhs)
        class(burnup_t), intent(in) :: lhs
        type(burnup_t), intent(in) :: rhs
        logical :: less_than

        less_than = lhs%watt_seconds_per_kilogram < rhs%watt_seconds_per_kilogram
    end function

    elemental function greater_than_or_equal(lhs, rhs)
        class(burnup_t), intent(in) :: lhs
        type(burnup_t), intent(in) :: rhs
        logical :: greater_than_or_equal

        greater_than_or_equal = lhs%watt_seconds_per_kilogram >= rhs%watt_seconds_per_kilogram
    end function

    elemental function less_than_or_equal(lhs, rhs)
        class(burnup_t), intent(in) :: lhs
        type(burnup_t), intent(in) :: rhs
        logical :: less_than_or_equal

        less_than_or_equal = lhs%watt_seconds_per_kilogram <= rhs%watt_seconds_per_kilogram
    end function

    elemental function equal_(lhs, rhs)
        class(burnup_t), intent(in) :: lhs
        type(burnup_t), intent(in) :: rhs
        logical :: equal_

        equal_ = lhs%watt_seconds_per_kilogram .safeEq. rhs%watt_seconds_per_kilogram
    end function

    elemental function equal_within_absolute_(lhs, rhs, within)
        class(burnup_t), intent(in) :: lhs
        type(burnup_t), intent(in) :: rhs
        type(burnup_t), intent(in) :: within
        logical :: equal_within_absolute_

        equal_within_absolute_ = equal_within_absolute( &
                lhs%watt_seconds_per_kilogram, rhs%watt_seconds_per_kilogram, within%watt_seconds_per_kilogram)
    end function

    elemental function equal_within_relative_(lhs, rhs, within)
        class(burnup_t), intent(in) :: lhs
        type(burnup_t), intent(in) :: rhs
        double precision, intent(in) :: within
        logical :: equal_within_relative_

        equal_within_relative_ = equal_within_relative( &
                lhs%watt_seconds_per_kilogram, rhs%watt_seconds_per_kilogram, within)
    end function

    elemental function not_equal(lhs, rhs)
        class(burnup_t), intent(in) :: lhs
        type(burnup_t), intent(in) :: rhs
        logical :: not_equal

        not_equal = .not. lhs == rhs
    end function

    elemental function to_string_full_precision(self) result(string)
        class(burnup_t), intent(in) :: self
        type(varying_string) :: string

        string = self%to_string_in(DEFAULT_OUTPUT_UNITS)
    end function

    elemental function to_string_with_precision(self, significant_digits) result(string)
        class(burnup_t), intent(in) :: self
        integer, intent(in) :: significant_digits
        type(varying_string) :: string

        string = self%to_string_in(DEFAULT_OUTPUT_UNITS, significant_digits)
    end function

    elemental function to_string_in_full_precision(self, units) result(string)
        class(burnup_t), intent(in) :: self
        class(burnup_unit_t), intent(in) :: units
        type(varying_string) :: string

        string = units%to_string(to_string(self.in.units))
    end function

    elemental function to_string_in_with_precision( &
            self, units, significant_digits) result(string)
        class(burnup_t), intent(in) :: self
        class(burnup_unit_t), intent(in) :: units
        integer, intent(in) :: significant_digits
        type(varying_string) :: string

        string = units%to_string(to_string(self.in.units, significant_digits))
    end function

    function fallible_burnup_from_burnup( &
            burnup) result(fallible_burnup)
        type(burnup_t), intent(in) :: burnup
        type(fallible_burnup_t) :: fallible_burnup

        fallible_burnup%burnup_ = burnup
    end function

    function fallible_burnup_from_errors( &
            errors) result(fallible_burnup)
        type(error_list_t), intent(in) :: errors
        type(fallible_burnup_t) :: fallible_burnup

        fallible_burnup%errors_ = errors
    end function

    function fallible_burnup_from_fallible_burnup( &
            fallible_burnup, &
            module_, &
            procedure_) &
            result(new_fallible_burnup)
        type(fallible_burnup_t), intent(in) :: fallible_burnup
        type(module_t), intent(in) :: module_
        type(procedure_t), intent(in) :: procedure_
        type(fallible_burnup_t) :: new_fallible_burnup

        if (fallible_burnup%failed()) then
            new_fallible_burnup%errors_ = error_list_t( &
                    fallible_burnup%errors_, module_, procedure_)
        else
            new_fallible_burnup%burnup_ = fallible_burnup%burnup_
        end if
    end function

    elemental function fallible_burnup_failed( &
            self) result(failed)
        class(fallible_burnup_t), intent(in) :: self
        logical :: failed

        failed = self%errors_%has_any()
    end function

    elemental function fallible_burnup_burnup( &
            self) result(burnup)
        class(fallible_burnup_t), intent(in) :: self
        type(burnup_t) :: burnup

        burnup = self%burnup_
    end function

    impure elemental function fallible_burnup_errors( &
            self) result(errors)
        class(fallible_burnup_t), intent(in) :: self
        type(error_list_t) :: errors

        errors = self%errors_
    end function

    function fallible_burnup_unit_from_unit( &
            unit) result(fallible_burnup_unit)
        class(burnup_unit_t), intent(in) :: unit
        type(fallible_burnup_unit_t) :: fallible_burnup_unit

        allocate(fallible_burnup_unit%unit_, source = unit)
    end function

    function fallible_burnup_unit_from_errors( &
            errors) result(fallible_burnup_unit)
        type(error_list_t), intent(in) :: errors
        type(fallible_burnup_unit_t) :: fallible_burnup_unit

        fallible_burnup_unit%errors_ = errors
    end function

    function fallible_burnup_unit_from_fallible_burnup_unit( &
            fallible_burnup_unit, &
            module_, &
            procedure_) &
            result(new_fallible_burnup_unit)
        type(fallible_burnup_unit_t), intent(in) :: fallible_burnup_unit
        type(module_t), intent(in) :: module_
        type(procedure_t), intent(in) :: procedure_
        type(fallible_burnup_unit_t) :: new_fallible_burnup_unit

        if (fallible_burnup_unit%failed()) then
            new_fallible_burnup_unit%errors_ = error_list_t( &
                    fallible_burnup_unit%errors_, module_, procedure_)
        else
            allocate(new_fallible_burnup_unit%unit_, source = &
                    fallible_burnup_unit%unit_)
        end if
    end function

    elemental function fallible_burnup_unit_failed( &
            self) result(failed)
        class(fallible_burnup_unit_t), intent(in) :: self
        logical :: failed

        failed = self%errors_%has_any()
    end function

    function fallible_burnup_unit_unit( &
            self) result(unit)
        class(fallible_burnup_unit_t), intent(in) :: self
        class(burnup_unit_t), allocatable :: unit

        allocate(unit, source = self%unit_)
    end function

    impure elemental function fallible_burnup_unit_errors( &
            self) result(errors)
        class(fallible_burnup_unit_t), intent(in) :: self
        type(error_list_t) :: errors

        errors = self%errors_
    end function

    elemental function simple_unit_to_string(self) result(string)
        class(burnup_simple_unit_t), intent(in) :: self
        type(varying_string) :: string

        string = trim(self%symbol)
    end function

    pure function simple_value_to_string(self, value_) result(string)
        class(burnup_simple_unit_t), intent(in) :: self
        type(varying_string), intent(in) :: value_
        type(varying_string) :: string

        string = value_ // " " // self%to_string()
    end function

    function simple_parse_as(self, string) result(fallible_burnup)
        class(burnup_simple_unit_t), intent(in) :: self
        type(varying_string), intent(in) :: string
        type(fallible_burnup_t) :: fallible_burnup

        type(parse_result_t) :: parse_result

        parse_result = parse_with(the_parser, string)
        if (parse_result%ok) then
            select type (the_number => parse_result%parsed)
            type is (parsed_rational_t)
                fallible_burnup = fallible_burnup_t(the_number%value_.unit.self)
            end select
        else
            fallible_burnup = fallible_burnup_t(error_list_t(fatal_t( &
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

    function parse_burnup_unit_c(string) result(fallible_burnup_unit)
        character(len=*), intent(in) :: string
        type(fallible_burnup_unit_t) :: fallible_burnup_unit

        fallible_burnup_unit = fallible_burnup_unit_t( &
                parse_burnup_unit(var_str(string), PROVIDED_UNITS), &
                module_t(MODULE_NAME), &
                procedure_t("parse_burnup_unit_c"))
    end function

    function parse_burnup_unit_s(string) result(fallible_burnup_unit)
        type(varying_string), intent(in) :: string
        type(fallible_burnup_unit_t) :: fallible_burnup_unit

        fallible_burnup_unit = fallible_burnup_unit_t( &
                parse_burnup_unit(string, PROVIDED_UNITS), &
                module_t(MODULE_NAME), &
                procedure_t("parse_burnup_unit_s"))
    end function

    function parse_burnup_unit_with_units_c( &
            string, units) result(fallible_burnup_unit)
        character(len=*), intent(in) :: string
        class(burnup_unit_t), intent(in) :: units(:)
        type(fallible_burnup_unit_t) :: fallible_burnup_unit

        fallible_burnup_unit = fallible_burnup_unit_t( &
                parse_burnup_unit(var_str(string), units), &
                module_t(MODULE_NAME), &
                procedure_t("parse_burnup_unit_with_units_c"))
    end function

    function parse_burnup_unit_with_units_s( &
            string, units) result(fallible_burnup_unit)
        type(varying_string), intent(in) :: string
        class(burnup_unit_t), intent(in) :: units(:)
        type(fallible_burnup_unit_t) :: fallible_burnup_unit

        integer :: i
        type(varying_string) :: unit_strings(size(units))

        do i = 1, size(units)
            if (string == units(i)%to_string()) then
                fallible_burnup_unit = fallible_burnup_unit_t(units(i))
                return
            end if
        end do
        do i = 1, size(units)
            unit_strings(i) = units(i)%to_string()
        end do
        fallible_burnup_unit = fallible_burnup_unit_t(error_list_t(fatal_t( &
                UNKNOWN_UNIT, &
                module_t(MODULE_NAME), &
                procedure_t("parse_burnup_unit_with_units_s"), &
                '"' // string // '", known units: [' // join(unit_strings, ', ') // ']')))
    end function
end module
