module acceleration_m
    use erloff, only: error_list_t, fatal_t, module_t, procedure_t
    use iso_varying_string, only: &
            varying_string, &
            assignment(=), &
            operator(==), &
            operator(//), &
            len, &
            split, &
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
    use quaff_conversion_factors_m, only: &
            CENTIMETERS_PER_SQUARE_SECOND_PER_METERS_PER_SQUARE_SECOND, &
            FEET_PER_SQUARE_SECOND_PER_METERS_PER_SQUARE_SECOND
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
            acceleration_t, &
            fallible_acceleration_t, &
            acceleration_unit_t, &
            fallible_acceleration_unit_t, &
            acceleration_simple_unit_t, &
            operator(.unit.), &
            parse_acceleration, &
            parse_acceleration_unit, &
            sum, &
            DEFAULT_OUTPUT_UNITS, &
            PROVIDED_UNITS, &
            CENTIMETERS_PER_SQUARE_SECOND, &
            FEET_PER_SQUARE_SECOND, &
            METERS_PER_SQUARE_SECOND

    type :: acceleration_t
        double precision :: meters_per_square_second
    contains
        private
        procedure :: to_units
        generic, public :: operator(.in.) => to_units
        procedure, pass(acceleration) :: double_times_acceleration
        procedure, pass(acceleration) :: integer_times_acceleration
        procedure :: acceleration_times_double
        procedure :: acceleration_times_integer
        generic, public :: operator(*) => &
                double_times_acceleration, &
                integer_times_acceleration, &
                acceleration_times_double, &
                acceleration_times_integer
        procedure :: acceleration_divided_by_double
        procedure :: acceleration_divided_by_integer
        procedure :: acceleration_divided_by_acceleration
        generic, public :: operator(/) => &
                acceleration_divided_by_double, &
                acceleration_divided_by_integer, &
                acceleration_divided_by_acceleration
        procedure :: acceleration_plus_acceleration
        generic, public :: operator(+) => acceleration_plus_acceleration
        procedure :: acceleration_minus_acceleration
        generic, public :: operator(-) => acceleration_minus_acceleration
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

    type :: fallible_acceleration_t
        private
        type(acceleration_t) :: acceleration_
        type(error_list_t) :: errors_
    contains
        private
        procedure, public :: failed => fallible_acceleration_failed
        procedure, public :: acceleration => fallible_acceleration_acceleration
        procedure, public :: errors => fallible_acceleration_errors
    end type

    type, abstract :: acceleration_unit_t
        double precision :: conversion_factor
    contains
        procedure(unit_to_string_i), deferred :: unit_to_string
        procedure(value_to_string_i), deferred :: value_to_string
        generic :: to_string => unit_to_string, value_to_string
        procedure(parse_as_i), deferred :: parse_as
    end type

    type :: fallible_acceleration_unit_t
        private
        class(acceleration_unit_t), allocatable :: unit_
        type(error_list_t) :: errors_
    contains
        private
        procedure, public :: failed => fallible_acceleration_unit_failed
        procedure, public :: unit => fallible_acceleration_unit_unit
        procedure, public :: errors => fallible_acceleration_unit_errors
    end type

    type, extends(acceleration_unit_t) :: acceleration_simple_unit_t
        character(len=20) :: symbol
    contains
        procedure :: unit_to_string => simple_unit_to_string
        procedure :: value_to_string => simple_value_to_string
        procedure :: parse_as => simple_parse_as
    end type

    abstract interface
        elemental function unit_to_string_i(self) result(string)
            import :: acceleration_unit_t, varying_string

            implicit none

            class(acceleration_unit_t), intent(in) :: self
            type(varying_string) :: string
        end function

        pure function value_to_string_i(self, value_) result(string)
            import :: acceleration_unit_t, varying_string

            implicit none

            class(acceleration_unit_t), intent(in) :: self
            type(varying_string), intent(in) :: value_
            type(varying_string) :: string
        end function

        function parse_as_i(self, string) result(fallible_acceleration)
            import :: acceleration_unit_t, fallible_acceleration_t, varying_string

            implicit none

            class(acceleration_unit_t), intent(in) :: self
            type(varying_string), intent(in) :: string
            type(fallible_acceleration_t) :: fallible_acceleration
        end function
    end interface

    interface operator(.unit.)
        module procedure from_units
    end interface

    interface fallible_acceleration_t
        module procedure fallible_acceleration_from_acceleration
        module procedure fallible_acceleration_from_errors
        module procedure fallible_acceleration_from_fallible_acceleration
    end interface

    interface fallible_acceleration_unit_t
        module procedure fallible_acceleration_unit_from_unit
        module procedure fallible_acceleration_unit_from_errors
        module procedure fallible_acceleration_unit_from_fallible_acceleration_unit
    end interface

    interface parse_acceleration
        module procedure parse_acceleration_c
        module procedure parse_acceleration_s
        module procedure parse_acceleration_with_units_c
        module procedure parse_acceleration_with_units_s
    end interface

    interface parse_acceleration_unit
        module procedure parse_acceleration_unit_c
        module procedure parse_acceleration_unit_s
        module procedure parse_acceleration_unit_with_units_c
        module procedure parse_acceleration_unit_with_units_s
    end interface

    interface sum
        module procedure sum_acceleration
    end interface

    type(acceleration_simple_unit_t), parameter :: CENTIMETERS_PER_SQUARE_SECOND = &
            acceleration_simple_unit_t( &
                    conversion_factor = CENTIMETERS_PER_SQUARE_SECOND_PER_METERS_PER_SQUARE_SECOND, &
                    symbol = "cm/s^2")
    type(acceleration_simple_unit_t), parameter :: FEET_PER_SQUARE_SECOND = &
            acceleration_simple_unit_t( &
                    conversion_factor = FEET_PER_SQUARE_SECOND_PER_METERS_PER_SQUARE_SECOND, &
                    symbol = "ft/s^2")
    type(acceleration_simple_unit_t), parameter :: METERS_PER_SQUARE_SECOND = &
            acceleration_simple_unit_t( &
                    conversion_factor = 1.0d0, &
                    symbol = "m/s^2")

    type(acceleration_simple_unit_t) :: DEFAULT_OUTPUT_UNITS = METERS_PER_SQUARE_SECOND

    type(acceleration_simple_unit_t), parameter :: PROVIDED_UNITS(*) = &
            [ CENTIMETERS_PER_SQUARE_SECOND &
            , FEET_PER_SQUARE_SECOND &
            , METERS_PER_SQUARE_SECOND &
            ]

    character(len=*), parameter :: MODULE_NAME = "acceleration_m"
contains
    function parse_acceleration_c(string) result(fallible_acceleration)
        character(len=*), intent(in) :: string
        type(fallible_acceleration_t) :: fallible_acceleration

        fallible_acceleration = fallible_acceleration_t( &
                parse_acceleration(var_str(string), PROVIDED_UNITS), &
                module_t(MODULE_NAME), &
                procedure_t("parse_acceleration_c"))
    end function

    function parse_acceleration_s(string) result(fallible_acceleration)
        type(varying_string), intent(in) :: string
        type(fallible_acceleration_t) :: fallible_acceleration

        fallible_acceleration = fallible_acceleration_t( &
                parse_acceleration(string, PROVIDED_UNITS), &
                module_t(MODULE_NAME), &
                procedure_t("parse_acceleration_s"))
    end function

    function parse_acceleration_with_units_c( &
            string, units) result(fallible_acceleration)
        character(len=*), intent(in) :: string
        class(acceleration_unit_t), intent(in) :: units(:)
        type(fallible_acceleration_t) :: fallible_acceleration

        fallible_acceleration = fallible_acceleration_t( &
                parse_acceleration(var_str(string), units), &
                module_t(MODULE_NAME), &
                procedure_t("parse_acceleration_with_units_c"))
    end function

    function parse_acceleration_with_units_s( &
            string, units) result(fallible_acceleration)
        type(varying_string), intent(in) :: string
        class(acceleration_unit_t), intent(in) :: units(:)
        type(fallible_acceleration_t) :: fallible_acceleration

        type(fallible_acceleration_t) :: all_attempts(size(units))
        integer :: i

        do i = 1, size(units)
            all_attempts(i) = units(i)%parse_as(string)
            if (.not. all_attempts(i)%failed()) then
                fallible_acceleration = all_attempts(i)
                return
            end if
        end do
        fallible_acceleration = fallible_acceleration_t(error_list_t( &
                all_attempts%errors(), &
                module_t(MODULE_NAME), &
                procedure_t("parse_acceleration_with_units_s")))
    end function

    elemental function from_units(value_, units) result(acceleration)
        double precision, intent(in) :: value_
        class(acceleration_unit_t), intent(in) :: units
        type(acceleration_t) :: acceleration

        acceleration%meters_per_square_second = value_ / units%conversion_factor
    end function

    elemental function to_units(self, units) result(acceleration)
        class(acceleration_t), intent(in) :: self
        class(acceleration_unit_t), intent(in) :: units
        double precision :: acceleration

        acceleration = self%meters_per_square_second * units%conversion_factor
    end function

    elemental function double_times_acceleration( &
            multiplier, acceleration) result(new_acceleration)
        double precision, intent(in) :: multiplier
        class(acceleration_t), intent(in) :: acceleration
        type(acceleration_t) :: new_acceleration

        new_acceleration%meters_per_square_second = &
                multiplier * acceleration%meters_per_square_second
    end function

    elemental function integer_times_acceleration( &
            multiplier, acceleration) result(new_acceleration)
        integer, intent(in) :: multiplier
        class(acceleration_t), intent(in) :: acceleration
        type(acceleration_t) :: new_acceleration

        new_acceleration%meters_per_square_second = &
                dble(multiplier) * acceleration%meters_per_square_second
    end function

    elemental function acceleration_times_double( &
            acceleration, multiplier) result(new_acceleration)
        class(acceleration_t), intent(in) :: acceleration
        double precision, intent(in) :: multiplier
        type(acceleration_t) :: new_acceleration

        new_acceleration%meters_per_square_second = &
                acceleration%meters_per_square_second * multiplier
    end function

    elemental function acceleration_times_integer( &
            acceleration, multiplier) result(new_acceleration)
        class(acceleration_t), intent(in) :: acceleration
        integer, intent(in) :: multiplier
        type(acceleration_t) :: new_acceleration

        new_acceleration%meters_per_square_second = &
                acceleration%meters_per_square_second * dble(multiplier)
    end function

    elemental function acceleration_divided_by_double( &
            acceleration, divisor) result(new_acceleration)
        class(acceleration_t), intent(in) :: acceleration
        double precision, intent(in) :: divisor
        type(acceleration_t) :: new_acceleration

        new_acceleration%meters_per_square_second = &
                acceleration%meters_per_square_second / divisor
    end function

    elemental function acceleration_divided_by_integer( &
            acceleration, divisor) result(new_acceleration)
        class(acceleration_t), intent(in) :: acceleration
        integer, intent(in) :: divisor
        type(acceleration_t) :: new_acceleration

        new_acceleration%meters_per_square_second = &
                acceleration%meters_per_square_second / dble(divisor)
    end function

    elemental function acceleration_divided_by_acceleration( &
            numerator, denomenator) result(ratio)
        class(acceleration_t), intent(in) :: numerator
        type(acceleration_t), intent(in) :: denomenator
        double precision :: ratio

        ratio = numerator%meters_per_square_second / denomenator%meters_per_square_second
    end function

    elemental function acceleration_plus_acceleration( &
            lhs, rhs) result(sum_)
        class(acceleration_t), intent(in) :: lhs
        type(acceleration_t), intent(in) :: rhs
        type(acceleration_t) :: sum_

        sum_%meters_per_square_second = lhs%meters_per_square_second + rhs%meters_per_square_second
    end function

    elemental function acceleration_minus_acceleration( &
            lhs, rhs) result(difference)
        class(acceleration_t), intent(in) :: lhs
        type(acceleration_t), intent(in) :: rhs
        type(acceleration_t) :: difference

        difference%meters_per_square_second = lhs%meters_per_square_second - rhs%meters_per_square_second
    end function

    pure function sum_acceleration(accelerations) result(sum_)
        type(acceleration_t), intent(in) :: accelerations(:)
        type(acceleration_t) :: sum_

        sum_%meters_per_square_second = sum(accelerations%meters_per_square_second)
    end function

    elemental function greater_than(lhs, rhs)
        class(acceleration_t), intent(in) :: lhs
        type(acceleration_t), intent(in) :: rhs
        logical :: greater_than

        greater_than = lhs%meters_per_square_second > rhs%meters_per_square_second
    end function

    elemental function less_than(lhs, rhs)
        class(acceleration_t), intent(in) :: lhs
        type(acceleration_t), intent(in) :: rhs
        logical :: less_than

        less_than = lhs%meters_per_square_second < rhs%meters_per_square_second
    end function

    elemental function greater_than_or_equal(lhs, rhs)
        class(acceleration_t), intent(in) :: lhs
        type(acceleration_t), intent(in) :: rhs
        logical :: greater_than_or_equal

        greater_than_or_equal = lhs%meters_per_square_second >= rhs%meters_per_square_second
    end function

    elemental function less_than_or_equal(lhs, rhs)
        class(acceleration_t), intent(in) :: lhs
        type(acceleration_t), intent(in) :: rhs
        logical :: less_than_or_equal

        less_than_or_equal = lhs%meters_per_square_second <= rhs%meters_per_square_second
    end function

    elemental function equal_(lhs, rhs)
        class(acceleration_t), intent(in) :: lhs
        type(acceleration_t), intent(in) :: rhs
        logical :: equal_

        equal_ = lhs%meters_per_square_second .safeEq. rhs%meters_per_square_second
    end function

    elemental function equal_within_absolute_(lhs, rhs, within)
        class(acceleration_t), intent(in) :: lhs
        type(acceleration_t), intent(in) :: rhs
        type(acceleration_t), intent(in) :: within
        logical :: equal_within_absolute_

        equal_within_absolute_ = equal_within_absolute( &
                lhs%meters_per_square_second, rhs%meters_per_square_second, within%meters_per_square_second)
    end function

    elemental function equal_within_relative_(lhs, rhs, within)
        class(acceleration_t), intent(in) :: lhs
        type(acceleration_t), intent(in) :: rhs
        double precision, intent(in) :: within
        logical :: equal_within_relative_

        equal_within_relative_ = equal_within_relative( &
                lhs%meters_per_square_second, rhs%meters_per_square_second, within)
    end function

    elemental function not_equal(lhs, rhs)
        class(acceleration_t), intent(in) :: lhs
        type(acceleration_t), intent(in) :: rhs
        logical :: not_equal

        not_equal = .not. lhs == rhs
    end function

    elemental function to_string_full_precision(self) result(string)
        class(acceleration_t), intent(in) :: self
        type(varying_string) :: string

        string = self%to_string_in(DEFAULT_OUTPUT_UNITS)
    end function

    elemental function to_string_with_precision(self, significant_digits) result(string)
        class(acceleration_t), intent(in) :: self
        integer, intent(in) :: significant_digits
        type(varying_string) :: string

        string = self%to_string_in(DEFAULT_OUTPUT_UNITS, significant_digits)
    end function

    elemental function to_string_in_full_precision(self, units) result(string)
        class(acceleration_t), intent(in) :: self
        class(acceleration_unit_t), intent(in) :: units
        type(varying_string) :: string

        string = units%to_string(to_string(self.in.units))
    end function

    elemental function to_string_in_with_precision( &
            self, units, significant_digits) result(string)
        class(acceleration_t), intent(in) :: self
        class(acceleration_unit_t), intent(in) :: units
        integer, intent(in) :: significant_digits
        type(varying_string) :: string

        string = units%to_string(to_string(self.in.units, significant_digits))
    end function

    function fallible_acceleration_from_acceleration( &
            acceleration) result(fallible_acceleration)
        type(acceleration_t), intent(in) :: acceleration
        type(fallible_acceleration_t) :: fallible_acceleration

        fallible_acceleration%acceleration_ = acceleration
    end function

    function fallible_acceleration_from_errors( &
            errors) result(fallible_acceleration)
        type(error_list_t), intent(in) :: errors
        type(fallible_acceleration_t) :: fallible_acceleration

        fallible_acceleration%errors_ = errors
    end function

    function fallible_acceleration_from_fallible_acceleration( &
            fallible_acceleration, &
            module_, &
            procedure_) &
            result(new_fallible_acceleration)
        type(fallible_acceleration_t), intent(in) :: fallible_acceleration
        type(module_t), intent(in) :: module_
        type(procedure_t), intent(in) :: procedure_
        type(fallible_acceleration_t) :: new_fallible_acceleration

        if (fallible_acceleration%failed()) then
            new_fallible_acceleration%errors_ = error_list_t( &
                    fallible_acceleration%errors_, module_, procedure_)
        else
            new_fallible_acceleration%acceleration_ = fallible_acceleration%acceleration_
        end if
    end function

    elemental function fallible_acceleration_failed( &
            self) result(failed)
        class(fallible_acceleration_t), intent(in) :: self
        logical :: failed

        failed = self%errors_%has_any()
    end function

    elemental function fallible_acceleration_acceleration( &
            self) result(acceleration)
        class(fallible_acceleration_t), intent(in) :: self
        type(acceleration_t) :: acceleration

        acceleration = self%acceleration_
    end function

    impure elemental function fallible_acceleration_errors( &
            self) result(errors)
        class(fallible_acceleration_t), intent(in) :: self
        type(error_list_t) :: errors

        errors = self%errors_
    end function

    function fallible_acceleration_unit_from_unit( &
            unit) result(fallible_acceleration_unit)
        class(acceleration_unit_t), intent(in) :: unit
        type(fallible_acceleration_unit_t) :: fallible_acceleration_unit

        allocate(fallible_acceleration_unit%unit_, source = unit)
    end function

    function fallible_acceleration_unit_from_errors( &
            errors) result(fallible_acceleration_unit)
        type(error_list_t), intent(in) :: errors
        type(fallible_acceleration_unit_t) :: fallible_acceleration_unit

        fallible_acceleration_unit%errors_ = errors
    end function

    function fallible_acceleration_unit_from_fallible_acceleration_unit( &
            fallible_acceleration_unit, &
            module_, &
            procedure_) &
            result(new_fallible_acceleration_unit)
        type(fallible_acceleration_unit_t), intent(in) :: fallible_acceleration_unit
        type(module_t), intent(in) :: module_
        type(procedure_t), intent(in) :: procedure_
        type(fallible_acceleration_unit_t) :: new_fallible_acceleration_unit

        if (fallible_acceleration_unit%failed()) then
            new_fallible_acceleration_unit%errors_ = error_list_t( &
                    fallible_acceleration_unit%errors_, module_, procedure_)
        else
            allocate(new_fallible_acceleration_unit%unit_, source = &
                    fallible_acceleration_unit%unit_)
        end if
    end function

    elemental function fallible_acceleration_unit_failed( &
            self) result(failed)
        class(fallible_acceleration_unit_t), intent(in) :: self
        logical :: failed

        failed = self%errors_%has_any()
    end function

    function fallible_acceleration_unit_unit( &
            self) result(unit)
        class(fallible_acceleration_unit_t), intent(in) :: self
        class(acceleration_unit_t), allocatable :: unit

        allocate(unit, source = self%unit_)
    end function

    impure elemental function fallible_acceleration_unit_errors( &
            self) result(errors)
        class(fallible_acceleration_unit_t), intent(in) :: self
        type(error_list_t) :: errors

        errors = self%errors_
    end function

    elemental function simple_unit_to_string(self) result(string)
        class(acceleration_simple_unit_t), intent(in) :: self
        type(varying_string) :: string

        string = trim(self%symbol)
    end function

    pure function simple_value_to_string(self, value_) result(string)
        class(acceleration_simple_unit_t), intent(in) :: self
        type(varying_string), intent(in) :: value_
        type(varying_string) :: string

        string = value_ // " " // self%to_string()
    end function

    function simple_parse_as(self, string) result(fallible_acceleration)
        class(acceleration_simple_unit_t), intent(in) :: self
        type(varying_string), intent(in) :: string
        type(fallible_acceleration_t) :: fallible_acceleration

        type(parse_result_t) :: parse_result

        parse_result = parse_with(the_parser, string)
        if (parse_result%ok) then
            select type (the_number => parse_result%parsed)
            type is (parsed_rational_t)
                fallible_acceleration = fallible_acceleration_t(the_number%value_.unit.self)
            end select
        else
            fallible_acceleration = fallible_acceleration_t(error_list_t(fatal_t( &
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

    function parse_acceleration_unit_c(string) result(fallible_acceleration_unit)
        character(len=*), intent(in) :: string
        type(fallible_acceleration_unit_t) :: fallible_acceleration_unit

        fallible_acceleration_unit = fallible_acceleration_unit_t( &
                parse_acceleration_unit(var_str(string), PROVIDED_UNITS), &
                module_t(MODULE_NAME), &
                procedure_t("parse_acceleration_unit_c"))
    end function

    function parse_acceleration_unit_s(string) result(fallible_acceleration_unit)
        type(varying_string), intent(in) :: string
        type(fallible_acceleration_unit_t) :: fallible_acceleration_unit

        fallible_acceleration_unit = fallible_acceleration_unit_t( &
                parse_acceleration_unit(string, PROVIDED_UNITS), &
                module_t(MODULE_NAME), &
                procedure_t("parse_acceleration_unit_s"))
    end function

    function parse_acceleration_unit_with_units_c( &
            string, units) result(fallible_acceleration_unit)
        character(len=*), intent(in) :: string
        class(acceleration_unit_t), intent(in) :: units(:)
        type(fallible_acceleration_unit_t) :: fallible_acceleration_unit

        fallible_acceleration_unit = fallible_acceleration_unit_t( &
                parse_acceleration_unit(var_str(string), units), &
                module_t(MODULE_NAME), &
                procedure_t("parse_acceleration_unit_with_units_c"))
    end function

    function parse_acceleration_unit_with_units_s( &
            string, units) result(fallible_acceleration_unit)
        type(varying_string), intent(in) :: string
        class(acceleration_unit_t), intent(in) :: units(:)
        type(fallible_acceleration_unit_t) :: fallible_acceleration_unit

        integer :: i
        type(varying_string) :: unit_strings(size(units))

        do i = 1, size(units)
            if (string == units(i)%to_string()) then
                fallible_acceleration_unit = fallible_acceleration_unit_t(units(i))
                return
            end if
        end do
        do i = 1, size(units)
            unit_strings(i) = units(i)%to_string()
        end do
        fallible_acceleration_unit = fallible_acceleration_unit_t(error_list_t(fatal_t( &
                UNKNOWN_UNIT, &
                module_t(MODULE_NAME), &
                procedure_t("parse_acceleration_unit_with_units_s"), &
                '"' // string // '", known units: [' // join(unit_strings, ', ') // ']')))
    end function
end module
