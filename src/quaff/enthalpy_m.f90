module quaff_enthalpy_m
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
            KILOJOULES_PER_KILOGRAM_PER_JOULES_PER_KILOGRAM
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
            enthalpy_t, &
            fallible_enthalpy_t, &
            enthalpy_unit_t, &
            fallible_enthalpy_unit_t, &
            enthalpy_simple_unit_t, &
            operator(.unit.), &
            parse_enthalpy, &
            parse_enthalpy_unit, &
            sum, &
            DEFAULT_OUTPUT_UNITS, &
            PROVIDED_UNITS, &
            JOULES_PER_KILOGRAM, &
            KILOJOULES_PER_KILOGRAM

    type :: enthalpy_t
        double precision :: joules_per_kilogram
    contains
        private
        procedure :: to_units
        generic, public :: operator(.in.) => to_units
        procedure, pass(enthalpy) :: double_times_enthalpy
        procedure, pass(enthalpy) :: integer_times_enthalpy
        procedure :: enthalpy_times_double
        procedure :: enthalpy_times_integer
        generic, public :: operator(*) => &
                double_times_enthalpy, &
                integer_times_enthalpy, &
                enthalpy_times_double, &
                enthalpy_times_integer
        procedure :: enthalpy_divided_by_double
        procedure :: enthalpy_divided_by_integer
        procedure :: enthalpy_divided_by_enthalpy
        generic, public :: operator(/) => &
                enthalpy_divided_by_double, &
                enthalpy_divided_by_integer, &
                enthalpy_divided_by_enthalpy
        procedure :: enthalpy_plus_enthalpy
        generic, public :: operator(+) => enthalpy_plus_enthalpy
        procedure :: enthalpy_minus_enthalpy
        generic, public :: operator(-) => enthalpy_minus_enthalpy
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

    type :: fallible_enthalpy_t
        private
        type(enthalpy_t) :: enthalpy_ = enthalpy_t(0.0d0)
        type(error_list_t) :: errors_
    contains
        private
        procedure, public :: failed => fallible_enthalpy_failed
        procedure, public :: enthalpy => fallible_enthalpy_enthalpy
        procedure, public :: errors => fallible_enthalpy_errors
    end type

    type, abstract :: enthalpy_unit_t
        double precision :: conversion_factor
    contains
        procedure(unit_to_string_i), deferred :: unit_to_string
        procedure(value_to_string_i), deferred :: value_to_string
        generic :: to_string => unit_to_string, value_to_string
        procedure(parse_as_i), deferred :: parse_as
    end type

    type :: fallible_enthalpy_unit_t
        private
        class(enthalpy_unit_t), allocatable :: unit_
        type(error_list_t) :: errors_
    contains
        private
        procedure, public :: failed => fallible_enthalpy_unit_failed
        procedure, public :: unit => fallible_enthalpy_unit_unit
        procedure, public :: errors => fallible_enthalpy_unit_errors
    end type

    type, extends(enthalpy_unit_t) :: enthalpy_simple_unit_t
        character(len=20) :: symbol
    contains
        procedure :: unit_to_string => simple_unit_to_string
        procedure :: value_to_string => simple_value_to_string
        procedure :: parse_as => simple_parse_as
    end type

    abstract interface
        elemental function unit_to_string_i(self) result(string)
            import :: enthalpy_unit_t, varying_string

            implicit none

            class(enthalpy_unit_t), intent(in) :: self
            type(varying_string) :: string
        end function

        pure function value_to_string_i(self, value_) result(string)
            import :: enthalpy_unit_t, varying_string

            implicit none

            class(enthalpy_unit_t), intent(in) :: self
            type(varying_string), intent(in) :: value_
            type(varying_string) :: string
        end function

        function parse_as_i(self, string) result(fallible_enthalpy)
            import :: enthalpy_unit_t, fallible_enthalpy_t, varying_string

            implicit none

            class(enthalpy_unit_t), intent(in) :: self
            type(varying_string), intent(in) :: string
            type(fallible_enthalpy_t) :: fallible_enthalpy
        end function
    end interface

    interface operator(.unit.)
        module procedure from_units
    end interface

    interface fallible_enthalpy_t
        module procedure fallible_enthalpy_from_enthalpy
        module procedure fallible_enthalpy_from_errors
        module procedure fallible_enthalpy_from_fallible_enthalpy
    end interface

    interface fallible_enthalpy_unit_t
        module procedure fallible_enthalpy_unit_from_unit
        module procedure fallible_enthalpy_unit_from_errors
        module procedure fallible_enthalpy_unit_from_fallible_enthalpy_unit
    end interface

    interface parse_enthalpy
        module procedure parse_enthalpy_c
        module procedure parse_enthalpy_s
        module procedure parse_enthalpy_with_units_c
        module procedure parse_enthalpy_with_units_s
    end interface

    interface parse_enthalpy_unit
        module procedure parse_enthalpy_unit_c
        module procedure parse_enthalpy_unit_s
        module procedure parse_enthalpy_unit_with_units_c
        module procedure parse_enthalpy_unit_with_units_s
    end interface

    interface sum
        module procedure sum_enthalpy
    end interface

    type(enthalpy_simple_unit_t), parameter :: JOULES_PER_KILOGRAM = &
            enthalpy_simple_unit_t( &
                    conversion_factor = 1.0d0, &
                    symbol = "J/kg")
    type(enthalpy_simple_unit_t), parameter :: KILOJOULES_PER_KILOGRAM = &
            enthalpy_simple_unit_t( &
                    conversion_factor = KILOJOULES_PER_KILOGRAM_PER_JOULES_PER_KILOGRAM, &
                    symbol = "kJ/kg")

    type(enthalpy_simple_unit_t) :: DEFAULT_OUTPUT_UNITS = JOULES_PER_KILOGRAM

    type(enthalpy_simple_unit_t), parameter :: PROVIDED_UNITS(*) = &
            [JOULES_PER_KILOGRAM, KILOJOULES_PER_KILOGRAM]

    character(len=*), parameter :: MODULE_NAME = "quaff_enthalpy_m"
contains
    function parse_enthalpy_c(string) result(fallible_enthalpy)
        character(len=*), intent(in) :: string
        type(fallible_enthalpy_t) :: fallible_enthalpy

        fallible_enthalpy = fallible_enthalpy_t( &
                parse_enthalpy(var_str(string), PROVIDED_UNITS), &
                module_t(MODULE_NAME), &
                procedure_t("parse_enthalpy_c"))
    end function

    function parse_enthalpy_s(string) result(fallible_enthalpy)
        type(varying_string), intent(in) :: string
        type(fallible_enthalpy_t) :: fallible_enthalpy

        fallible_enthalpy = fallible_enthalpy_t( &
                parse_enthalpy(string, PROVIDED_UNITS), &
                module_t(MODULE_NAME), &
                procedure_t("parse_enthalpy_s"))
    end function

    function parse_enthalpy_with_units_c( &
            string, units) result(fallible_enthalpy)
        character(len=*), intent(in) :: string
        class(enthalpy_unit_t), intent(in) :: units(:)
        type(fallible_enthalpy_t) :: fallible_enthalpy

        fallible_enthalpy = fallible_enthalpy_t( &
                parse_enthalpy(var_str(string), units), &
                module_t(MODULE_NAME), &
                procedure_t("parse_enthalpy_with_units_c"))
    end function

    function parse_enthalpy_with_units_s( &
            string, units) result(fallible_enthalpy)
        type(varying_string), intent(in) :: string
        class(enthalpy_unit_t), intent(in) :: units(:)
        type(fallible_enthalpy_t) :: fallible_enthalpy

        integer :: i

        do i = 1, size(units)
            fallible_enthalpy = units(i)%parse_as(string)
            if (.not. fallible_enthalpy%failed()) return
        end do
        fallible_enthalpy = fallible_enthalpy_t(error_list_t(fatal_t( &
                PARSE_ERROR, &
                module_t(MODULE_NAME), &
                procedure_t("parse_enthalpy_with_units_s"), &
                "Unable to parse '" // string // "' as a enthalpy_t. Tried with units: " &
                // join(units%to_string(), ", "))))
    end function

    elemental function from_units(value_, units) result(enthalpy)
        double precision, intent(in) :: value_
        class(enthalpy_unit_t), intent(in) :: units
        type(enthalpy_t) :: enthalpy

        enthalpy%joules_per_kilogram = value_ / units%conversion_factor
    end function

    elemental function to_units(self, units) result(enthalpy)
        class(enthalpy_t), intent(in) :: self
        class(enthalpy_unit_t), intent(in) :: units
        double precision :: enthalpy

        enthalpy = self%joules_per_kilogram * units%conversion_factor
    end function

    elemental function double_times_enthalpy( &
            multiplier, enthalpy) result(new_enthalpy)
        double precision, intent(in) :: multiplier
        class(enthalpy_t), intent(in) :: enthalpy
        type(enthalpy_t) :: new_enthalpy

        new_enthalpy%joules_per_kilogram = &
                multiplier * enthalpy%joules_per_kilogram
    end function

    elemental function integer_times_enthalpy( &
            multiplier, enthalpy) result(new_enthalpy)
        integer, intent(in) :: multiplier
        class(enthalpy_t), intent(in) :: enthalpy
        type(enthalpy_t) :: new_enthalpy

        new_enthalpy%joules_per_kilogram = &
                dble(multiplier) * enthalpy%joules_per_kilogram
    end function

    elemental function enthalpy_times_double( &
            enthalpy, multiplier) result(new_enthalpy)
        class(enthalpy_t), intent(in) :: enthalpy
        double precision, intent(in) :: multiplier
        type(enthalpy_t) :: new_enthalpy

        new_enthalpy%joules_per_kilogram = &
                enthalpy%joules_per_kilogram * multiplier
    end function

    elemental function enthalpy_times_integer( &
            enthalpy, multiplier) result(new_enthalpy)
        class(enthalpy_t), intent(in) :: enthalpy
        integer, intent(in) :: multiplier
        type(enthalpy_t) :: new_enthalpy

        new_enthalpy%joules_per_kilogram = &
                enthalpy%joules_per_kilogram * dble(multiplier)
    end function

    elemental function enthalpy_divided_by_double( &
            enthalpy, divisor) result(new_enthalpy)
        class(enthalpy_t), intent(in) :: enthalpy
        double precision, intent(in) :: divisor
        type(enthalpy_t) :: new_enthalpy

        new_enthalpy%joules_per_kilogram = &
                enthalpy%joules_per_kilogram / divisor
    end function

    elemental function enthalpy_divided_by_integer( &
            enthalpy, divisor) result(new_enthalpy)
        class(enthalpy_t), intent(in) :: enthalpy
        integer, intent(in) :: divisor
        type(enthalpy_t) :: new_enthalpy

        new_enthalpy%joules_per_kilogram = &
                enthalpy%joules_per_kilogram / dble(divisor)
    end function

    elemental function enthalpy_divided_by_enthalpy( &
            numerator, denomenator) result(ratio)
        class(enthalpy_t), intent(in) :: numerator
        type(enthalpy_t), intent(in) :: denomenator
        double precision :: ratio

        ratio = numerator%joules_per_kilogram / denomenator%joules_per_kilogram
    end function

    elemental function enthalpy_plus_enthalpy( &
            lhs, rhs) result(sum_)
        class(enthalpy_t), intent(in) :: lhs
        type(enthalpy_t), intent(in) :: rhs
        type(enthalpy_t) :: sum_

        sum_%joules_per_kilogram = lhs%joules_per_kilogram + rhs%joules_per_kilogram
    end function

    elemental function enthalpy_minus_enthalpy( &
            lhs, rhs) result(difference)
        class(enthalpy_t), intent(in) :: lhs
        type(enthalpy_t), intent(in) :: rhs
        type(enthalpy_t) :: difference

        difference%joules_per_kilogram = lhs%joules_per_kilogram - rhs%joules_per_kilogram
    end function

    pure function sum_enthalpy(enthalpys) result(sum_)
        type(enthalpy_t), intent(in) :: enthalpys(:)
        type(enthalpy_t) :: sum_

        sum_%joules_per_kilogram = sum(enthalpys%joules_per_kilogram)
    end function

    elemental function greater_than(lhs, rhs)
        class(enthalpy_t), intent(in) :: lhs
        type(enthalpy_t), intent(in) :: rhs
        logical :: greater_than

        greater_than = lhs%joules_per_kilogram > rhs%joules_per_kilogram
    end function

    elemental function less_than(lhs, rhs)
        class(enthalpy_t), intent(in) :: lhs
        type(enthalpy_t), intent(in) :: rhs
        logical :: less_than

        less_than = lhs%joules_per_kilogram < rhs%joules_per_kilogram
    end function

    elemental function greater_than_or_equal(lhs, rhs)
        class(enthalpy_t), intent(in) :: lhs
        type(enthalpy_t), intent(in) :: rhs
        logical :: greater_than_or_equal

        greater_than_or_equal = lhs%joules_per_kilogram >= rhs%joules_per_kilogram
    end function

    elemental function less_than_or_equal(lhs, rhs)
        class(enthalpy_t), intent(in) :: lhs
        type(enthalpy_t), intent(in) :: rhs
        logical :: less_than_or_equal

        less_than_or_equal = lhs%joules_per_kilogram <= rhs%joules_per_kilogram
    end function

    elemental function equal_(lhs, rhs)
        class(enthalpy_t), intent(in) :: lhs
        type(enthalpy_t), intent(in) :: rhs
        logical :: equal_

        equal_ = lhs%joules_per_kilogram .safeEq. rhs%joules_per_kilogram
    end function

    elemental function equal_within_absolute_(lhs, rhs, within)
        class(enthalpy_t), intent(in) :: lhs
        type(enthalpy_t), intent(in) :: rhs
        type(enthalpy_t), intent(in) :: within
        logical :: equal_within_absolute_

        equal_within_absolute_ = equal_within_absolute( &
                lhs%joules_per_kilogram, rhs%joules_per_kilogram, within%joules_per_kilogram)
    end function

    elemental function equal_within_relative_(lhs, rhs, within)
        class(enthalpy_t), intent(in) :: lhs
        type(enthalpy_t), intent(in) :: rhs
        double precision, intent(in) :: within
        logical :: equal_within_relative_

        equal_within_relative_ = equal_within_relative( &
                lhs%joules_per_kilogram, rhs%joules_per_kilogram, within)
    end function

    elemental function not_equal(lhs, rhs)
        class(enthalpy_t), intent(in) :: lhs
        type(enthalpy_t), intent(in) :: rhs
        logical :: not_equal

        not_equal = .not. lhs == rhs
    end function

    elemental function to_string_full_precision(self) result(string)
        class(enthalpy_t), intent(in) :: self
        type(varying_string) :: string

        string = self%to_string_in(DEFAULT_OUTPUT_UNITS)
    end function

    elemental function to_string_with_precision(self, significant_digits) result(string)
        class(enthalpy_t), intent(in) :: self
        integer, intent(in) :: significant_digits
        type(varying_string) :: string

        string = self%to_string_in(DEFAULT_OUTPUT_UNITS, significant_digits)
    end function

    elemental function to_string_in_full_precision(self, units) result(string)
        class(enthalpy_t), intent(in) :: self
        class(enthalpy_unit_t), intent(in) :: units
        type(varying_string) :: string

        string = units%to_string(to_string(self.in.units))
    end function

    elemental function to_string_in_with_precision( &
            self, units, significant_digits) result(string)
        class(enthalpy_t), intent(in) :: self
        class(enthalpy_unit_t), intent(in) :: units
        integer, intent(in) :: significant_digits
        type(varying_string) :: string

        string = units%to_string(to_string(self.in.units, significant_digits))
    end function

    function fallible_enthalpy_from_enthalpy( &
            enthalpy) result(fallible_enthalpy)
        type(enthalpy_t), intent(in) :: enthalpy
        type(fallible_enthalpy_t) :: fallible_enthalpy

        fallible_enthalpy%enthalpy_ = enthalpy
    end function

    function fallible_enthalpy_from_errors( &
            errors) result(fallible_enthalpy)
        type(error_list_t), intent(in) :: errors
        type(fallible_enthalpy_t) :: fallible_enthalpy

        fallible_enthalpy%errors_ = errors
    end function

    function fallible_enthalpy_from_fallible_enthalpy( &
            fallible_enthalpy, &
            module_, &
            procedure_) &
            result(new_fallible_enthalpy)
        type(fallible_enthalpy_t), intent(in) :: fallible_enthalpy
        type(module_t), intent(in) :: module_
        type(procedure_t), intent(in) :: procedure_
        type(fallible_enthalpy_t) :: new_fallible_enthalpy

        if (fallible_enthalpy%failed()) then
            new_fallible_enthalpy%errors_ = error_list_t( &
                    fallible_enthalpy%errors_, module_, procedure_)
        else
            new_fallible_enthalpy%enthalpy_ = fallible_enthalpy%enthalpy_
        end if
    end function

    elemental function fallible_enthalpy_failed( &
            self) result(failed)
        class(fallible_enthalpy_t), intent(in) :: self
        logical :: failed

        failed = self%errors_%has_any()
    end function

    elemental function fallible_enthalpy_enthalpy( &
            self) result(enthalpy)
        class(fallible_enthalpy_t), intent(in) :: self
        type(enthalpy_t) :: enthalpy

        enthalpy = self%enthalpy_
    end function

    impure elemental function fallible_enthalpy_errors( &
            self) result(errors)
        class(fallible_enthalpy_t), intent(in) :: self
        type(error_list_t) :: errors

        errors = self%errors_
    end function

    function fallible_enthalpy_unit_from_unit( &
            unit) result(fallible_enthalpy_unit)
        class(enthalpy_unit_t), intent(in) :: unit
        type(fallible_enthalpy_unit_t) :: fallible_enthalpy_unit

        allocate(fallible_enthalpy_unit%unit_, source = unit)
    end function

    function fallible_enthalpy_unit_from_errors( &
            errors) result(fallible_enthalpy_unit)
        type(error_list_t), intent(in) :: errors
        type(fallible_enthalpy_unit_t) :: fallible_enthalpy_unit

        fallible_enthalpy_unit%errors_ = errors
    end function

    function fallible_enthalpy_unit_from_fallible_enthalpy_unit( &
            fallible_enthalpy_unit, &
            module_, &
            procedure_) &
            result(new_fallible_enthalpy_unit)
        type(fallible_enthalpy_unit_t), intent(in) :: fallible_enthalpy_unit
        type(module_t), intent(in) :: module_
        type(procedure_t), intent(in) :: procedure_
        type(fallible_enthalpy_unit_t) :: new_fallible_enthalpy_unit

        if (fallible_enthalpy_unit%failed()) then
            new_fallible_enthalpy_unit%errors_ = error_list_t( &
                    fallible_enthalpy_unit%errors_, module_, procedure_)
        else
            allocate(new_fallible_enthalpy_unit%unit_, source = &
                    fallible_enthalpy_unit%unit_)
        end if
    end function

    elemental function fallible_enthalpy_unit_failed( &
            self) result(failed)
        class(fallible_enthalpy_unit_t), intent(in) :: self
        logical :: failed

        failed = self%errors_%has_any()
    end function

    function fallible_enthalpy_unit_unit( &
            self) result(unit)
        class(fallible_enthalpy_unit_t), intent(in) :: self
        class(enthalpy_unit_t), allocatable :: unit

        allocate(unit, source = self%unit_)
    end function

    impure elemental function fallible_enthalpy_unit_errors( &
            self) result(errors)
        class(fallible_enthalpy_unit_t), intent(in) :: self
        type(error_list_t) :: errors

        errors = self%errors_
    end function

    elemental function simple_unit_to_string(self) result(string)
        class(enthalpy_simple_unit_t), intent(in) :: self
        type(varying_string) :: string

        string = trim(self%symbol)
    end function

    pure function simple_value_to_string(self, value_) result(string)
        class(enthalpy_simple_unit_t), intent(in) :: self
        type(varying_string), intent(in) :: value_
        type(varying_string) :: string

        string = value_ // " " // self%to_string()
    end function

    function simple_parse_as(self, string) result(fallible_enthalpy)
        class(enthalpy_simple_unit_t), intent(in) :: self
        type(varying_string), intent(in) :: string
        type(fallible_enthalpy_t) :: fallible_enthalpy

        type(parse_result_t) :: parse_result

        parse_result = parse_with(the_parser, string)
        if (parse_result%ok) then
            select type (the_number => parse_result%parsed)
            type is (parsed_rational_t)
                fallible_enthalpy = fallible_enthalpy_t(the_number%value_.unit.self)
            end select
        else
            fallible_enthalpy = fallible_enthalpy_t(error_list_t(fatal_t( &
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

    function parse_enthalpy_unit_c(string) result(fallible_enthalpy_unit)
        character(len=*), intent(in) :: string
        type(fallible_enthalpy_unit_t) :: fallible_enthalpy_unit

        fallible_enthalpy_unit = fallible_enthalpy_unit_t( &
                parse_enthalpy_unit(var_str(string), PROVIDED_UNITS), &
                module_t(MODULE_NAME), &
                procedure_t("parse_enthalpy_unit_c"))
    end function

    function parse_enthalpy_unit_s(string) result(fallible_enthalpy_unit)
        type(varying_string), intent(in) :: string
        type(fallible_enthalpy_unit_t) :: fallible_enthalpy_unit

        fallible_enthalpy_unit = fallible_enthalpy_unit_t( &
                parse_enthalpy_unit(string, PROVIDED_UNITS), &
                module_t(MODULE_NAME), &
                procedure_t("parse_enthalpy_unit_s"))
    end function

    function parse_enthalpy_unit_with_units_c( &
            string, units) result(fallible_enthalpy_unit)
        character(len=*), intent(in) :: string
        class(enthalpy_unit_t), intent(in) :: units(:)
        type(fallible_enthalpy_unit_t) :: fallible_enthalpy_unit

        fallible_enthalpy_unit = fallible_enthalpy_unit_t( &
                parse_enthalpy_unit(var_str(string), units), &
                module_t(MODULE_NAME), &
                procedure_t("parse_enthalpy_unit_with_units_c"))
    end function

    function parse_enthalpy_unit_with_units_s( &
            string, units) result(fallible_enthalpy_unit)
        type(varying_string), intent(in) :: string
        class(enthalpy_unit_t), intent(in) :: units(:)
        type(fallible_enthalpy_unit_t) :: fallible_enthalpy_unit

        integer :: i
        type(varying_string) :: unit_strings(size(units))

        do i = 1, size(units)
            if (string == units(i)%to_string()) then
                fallible_enthalpy_unit = fallible_enthalpy_unit_t(units(i))
                return
            end if
        end do
        do i = 1, size(units)
            unit_strings(i) = units(i)%to_string()
        end do
        fallible_enthalpy_unit = fallible_enthalpy_unit_t(error_list_t(fatal_t( &
                UNKNOWN_UNIT, &
                module_t(MODULE_NAME), &
                procedure_t("parse_enthalpy_unit_with_units_s"), &
                '"' // string // '", known units: [' // join(unit_strings, ', ') // ']')))
    end function
end module
