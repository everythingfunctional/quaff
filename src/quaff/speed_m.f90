module quaff_speed_m
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
            CENTIMETERS_PER_SECOND_PER_METERS_PER_SECOND, &
            FEET_PER_SECOND_PER_METERS_PER_SECOND, &
            MILLIMETERS_PER_SECOND_PER_METERS_PER_SECOND
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
            speed_t, &
            fallible_speed_t, &
            speed_unit_t, &
            fallible_speed_unit_t, &
            speed_simple_unit_t, &
            operator(.unit.), &
            parse_speed, &
            parse_speed_unit, &
            sum, &
            abs, &
            DEFAULT_OUTPUT_UNITS, &
            PROVIDED_UNITS, &
            CENTIMETERS_PER_SECOND, &
            FEET_PER_SECOND, &
            METERS_PER_SECOND, &
            MILLIMETERS_PER_SECOND

    type :: speed_t
        double precision :: meters_per_second
    contains
        private
        procedure :: to_units
        generic, public :: operator(.in.) => to_units
        procedure, pass(speed) :: double_times_speed
        procedure, pass(speed) :: integer_times_speed
        procedure :: speed_times_double
        procedure :: speed_times_integer
        generic, public :: operator(*) => &
                double_times_speed, &
                integer_times_speed, &
                speed_times_double, &
                speed_times_integer
        procedure :: speed_divided_by_double
        procedure :: speed_divided_by_integer
        procedure :: speed_divided_by_speed
        generic, public :: operator(/) => &
                speed_divided_by_double, &
                speed_divided_by_integer, &
                speed_divided_by_speed
        procedure :: speed_plus_speed
        generic, public :: operator(+) => speed_plus_speed
        procedure :: negate_speed
        procedure :: speed_minus_speed
        generic, public :: operator(-) => &
                negate_speed, &
                speed_minus_speed
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

    type :: fallible_speed_t
        private
        type(speed_t) :: speed_ = speed_t(0.0d0)
        type(error_list_t) :: errors_
    contains
        private
        procedure, public :: failed => fallible_speed_failed
        procedure, public :: speed => fallible_speed_speed
        procedure, public :: errors => fallible_speed_errors
    end type

    type, abstract :: speed_unit_t
        double precision :: conversion_factor
    contains
        procedure(unit_to_string_i), deferred :: unit_to_string
        procedure(value_to_string_i), deferred :: value_to_string
        generic :: to_string => unit_to_string, value_to_string
        procedure(parse_as_i), deferred :: parse_as
    end type

    type :: fallible_speed_unit_t
        private
        class(speed_unit_t), allocatable :: unit_
        type(error_list_t) :: errors_
    contains
        private
        procedure, public :: failed => fallible_speed_unit_failed
        procedure, public :: unit => fallible_speed_unit_unit
        procedure, public :: errors => fallible_speed_unit_errors
    end type

    type, extends(speed_unit_t) :: speed_simple_unit_t
        character(len=20) :: symbol
    contains
        procedure :: unit_to_string => simple_unit_to_string
        procedure :: value_to_string => simple_value_to_string
        procedure :: parse_as => simple_parse_as
    end type

    abstract interface
        elemental function unit_to_string_i(self) result(string)
            import :: speed_unit_t, varying_string

            implicit none

            class(speed_unit_t), intent(in) :: self
            type(varying_string) :: string
        end function

        pure function value_to_string_i(self, value_) result(string)
            import :: speed_unit_t, varying_string

            implicit none

            class(speed_unit_t), intent(in) :: self
            type(varying_string), intent(in) :: value_
            type(varying_string) :: string
        end function

        function parse_as_i(self, string) result(fallible_speed)
            import :: speed_unit_t, fallible_speed_t, varying_string

            implicit none

            class(speed_unit_t), intent(in) :: self
            type(varying_string), intent(in) :: string
            type(fallible_speed_t) :: fallible_speed
        end function
    end interface

    interface operator(.unit.)
        module procedure from_units
    end interface

    interface fallible_speed_t
        module procedure fallible_speed_from_speed
        module procedure fallible_speed_from_errors
        module procedure fallible_speed_from_fallible_speed
    end interface

    interface fallible_speed_unit_t
        module procedure fallible_speed_unit_from_unit
        module procedure fallible_speed_unit_from_errors
        module procedure fallible_speed_unit_from_fallible_speed_unit
    end interface

    interface parse_speed
        module procedure parse_speed_c
        module procedure parse_speed_s
        module procedure parse_speed_with_units_c
        module procedure parse_speed_with_units_s
    end interface

    interface parse_speed_unit
        module procedure parse_speed_unit_c
        module procedure parse_speed_unit_s
        module procedure parse_speed_unit_with_units_c
        module procedure parse_speed_unit_with_units_s
    end interface

    interface abs
        module procedure abs_speed
    end interface

    interface sum
        module procedure sum_speed
    end interface

    type(speed_simple_unit_t), parameter :: CENTIMETERS_PER_SECOND = &
            speed_simple_unit_t( &
                    conversion_factor = CENTIMETERS_PER_SECOND_PER_METERS_PER_SECOND, &
                    symbol = "cm/s")
    type(speed_simple_unit_t), parameter :: FEET_PER_SECOND = &
            speed_simple_unit_t( &
                    conversion_factor = FEET_PER_SECOND_PER_METERS_PER_SECOND, &
                    symbol = "ft/s")
    type(speed_simple_unit_t), parameter :: METERS_PER_SECOND = &
            speed_simple_unit_t( &
                    conversion_factor = 1.0d0, &
                    symbol = "m/s")
    type(speed_simple_unit_t), parameter :: MILLIMETERS_PER_SECOND = &
            speed_simple_unit_t( &
                    conversion_factor = MILLIMETERS_PER_SECOND_PER_METERS_PER_SECOND, &
                    symbol = "mm/s")

    type(speed_simple_unit_t) :: DEFAULT_OUTPUT_UNITS = METERS_PER_SECOND

    type(speed_simple_unit_t), parameter :: PROVIDED_UNITS(*) = &
            [CENTIMETERS_PER_SECOND, FEET_PER_SECOND, METERS_PER_SECOND, MILLIMETERS_PER_SECOND]

    character(len=*), parameter :: MODULE_NAME = "quaff_speed_m"
contains
    function parse_speed_c(string) result(fallible_speed)
        character(len=*), intent(in) :: string
        type(fallible_speed_t) :: fallible_speed

        fallible_speed = fallible_speed_t( &
                parse_speed(var_str(string), PROVIDED_UNITS), &
                module_t(MODULE_NAME), &
                procedure_t("parse_speed_c"))
    end function

    function parse_speed_s(string) result(fallible_speed)
        type(varying_string), intent(in) :: string
        type(fallible_speed_t) :: fallible_speed

        fallible_speed = fallible_speed_t( &
                parse_speed(string, PROVIDED_UNITS), &
                module_t(MODULE_NAME), &
                procedure_t("parse_speed_s"))
    end function

    function parse_speed_with_units_c( &
            string, units) result(fallible_speed)
        character(len=*), intent(in) :: string
        class(speed_unit_t), intent(in) :: units(:)
        type(fallible_speed_t) :: fallible_speed

        fallible_speed = fallible_speed_t( &
                parse_speed(var_str(string), units), &
                module_t(MODULE_NAME), &
                procedure_t("parse_speed_with_units_c"))
    end function

    function parse_speed_with_units_s( &
            string, units) result(fallible_speed)
        type(varying_string), intent(in) :: string
        class(speed_unit_t), intent(in) :: units(:)
        type(fallible_speed_t) :: fallible_speed

        integer :: i

        do i = 1, size(units)
            fallible_speed = units(i)%parse_as(string)
            if (.not. fallible_speed%failed()) return
        end do
        fallible_speed = fallible_speed_t(error_list_t(fatal_t( &
                PARSE_ERROR, &
                module_t(MODULE_NAME), &
                procedure_t("parse_speed_with_units_s"), &
                "Unable to parse '" // string // "' as a speed_t. Tried with units: " &
                // join(units%to_string(), ", "))))
    end function

    elemental function from_units(value_, units) result(speed)
        double precision, intent(in) :: value_
        class(speed_unit_t), intent(in) :: units
        type(speed_t) :: speed

        speed%meters_per_second = value_ / units%conversion_factor
    end function

    elemental function to_units(self, units) result(speed)
        class(speed_t), intent(in) :: self
        class(speed_unit_t), intent(in) :: units
        double precision :: speed

        speed = self%meters_per_second * units%conversion_factor
    end function

    elemental function double_times_speed( &
            multiplier, speed) result(new_speed)
        double precision, intent(in) :: multiplier
        class(speed_t), intent(in) :: speed
        type(speed_t) :: new_speed

        new_speed%meters_per_second = &
                multiplier * speed%meters_per_second
    end function

    elemental function integer_times_speed( &
            multiplier, speed) result(new_speed)
        integer, intent(in) :: multiplier
        class(speed_t), intent(in) :: speed
        type(speed_t) :: new_speed

        new_speed%meters_per_second = &
                dble(multiplier) * speed%meters_per_second
    end function

    elemental function speed_times_double( &
            speed, multiplier) result(new_speed)
        class(speed_t), intent(in) :: speed
        double precision, intent(in) :: multiplier
        type(speed_t) :: new_speed

        new_speed%meters_per_second = &
                speed%meters_per_second * multiplier
    end function

    elemental function speed_times_integer( &
            speed, multiplier) result(new_speed)
        class(speed_t), intent(in) :: speed
        integer, intent(in) :: multiplier
        type(speed_t) :: new_speed

        new_speed%meters_per_second = &
                speed%meters_per_second * dble(multiplier)
    end function

    elemental function speed_divided_by_double( &
            speed, divisor) result(new_speed)
        class(speed_t), intent(in) :: speed
        double precision, intent(in) :: divisor
        type(speed_t) :: new_speed

        new_speed%meters_per_second = &
                speed%meters_per_second / divisor
    end function

    elemental function speed_divided_by_integer( &
            speed, divisor) result(new_speed)
        class(speed_t), intent(in) :: speed
        integer, intent(in) :: divisor
        type(speed_t) :: new_speed

        new_speed%meters_per_second = &
                speed%meters_per_second / dble(divisor)
    end function

    elemental function speed_divided_by_speed( &
            numerator, denomenator) result(ratio)
        class(speed_t), intent(in) :: numerator
        type(speed_t), intent(in) :: denomenator
        double precision :: ratio

        ratio = numerator%meters_per_second / denomenator%meters_per_second
    end function

    elemental function speed_plus_speed( &
            lhs, rhs) result(sum_)
        class(speed_t), intent(in) :: lhs
        type(speed_t), intent(in) :: rhs
        type(speed_t) :: sum_

        sum_%meters_per_second = lhs%meters_per_second + rhs%meters_per_second
    end function

    elemental function negate_speed(self) result(negated)
        class(speed_t), intent(in) :: self
        type(speed_t) :: negated

        negated%meters_per_second = -self%meters_per_second
    end function

    elemental function speed_minus_speed( &
            lhs, rhs) result(difference)
        class(speed_t), intent(in) :: lhs
        type(speed_t), intent(in) :: rhs
        type(speed_t) :: difference

        difference%meters_per_second = lhs%meters_per_second - rhs%meters_per_second
    end function

    pure function abs_speed(speed) result(abs_)
        type(speed_t), intent(in) :: speed
        type(speed_t) :: abs_

        abs_%meters_per_second = abs(speed%meters_per_second)
    end function

    pure function sum_speed(speeds) result(sum_)
        type(speed_t), intent(in) :: speeds(:)
        type(speed_t) :: sum_

        sum_%meters_per_second = sum(speeds%meters_per_second)
    end function

    elemental function greater_than(lhs, rhs)
        class(speed_t), intent(in) :: lhs
        type(speed_t), intent(in) :: rhs
        logical :: greater_than

        greater_than = lhs%meters_per_second > rhs%meters_per_second
    end function

    elemental function less_than(lhs, rhs)
        class(speed_t), intent(in) :: lhs
        type(speed_t), intent(in) :: rhs
        logical :: less_than

        less_than = lhs%meters_per_second < rhs%meters_per_second
    end function

    elemental function greater_than_or_equal(lhs, rhs)
        class(speed_t), intent(in) :: lhs
        type(speed_t), intent(in) :: rhs
        logical :: greater_than_or_equal

        greater_than_or_equal = lhs%meters_per_second >= rhs%meters_per_second
    end function

    elemental function less_than_or_equal(lhs, rhs)
        class(speed_t), intent(in) :: lhs
        type(speed_t), intent(in) :: rhs
        logical :: less_than_or_equal

        less_than_or_equal = lhs%meters_per_second <= rhs%meters_per_second
    end function

    elemental function equal_(lhs, rhs)
        class(speed_t), intent(in) :: lhs
        type(speed_t), intent(in) :: rhs
        logical :: equal_

        equal_ = lhs%meters_per_second .safeEq. rhs%meters_per_second
    end function

    elemental function equal_within_absolute_(lhs, rhs, within)
        class(speed_t), intent(in) :: lhs
        type(speed_t), intent(in) :: rhs
        type(speed_t), intent(in) :: within
        logical :: equal_within_absolute_

        equal_within_absolute_ = equal_within_absolute( &
                lhs%meters_per_second, rhs%meters_per_second, within%meters_per_second)
    end function

    elemental function equal_within_relative_(lhs, rhs, within)
        class(speed_t), intent(in) :: lhs
        type(speed_t), intent(in) :: rhs
        double precision, intent(in) :: within
        logical :: equal_within_relative_

        equal_within_relative_ = equal_within_relative( &
                lhs%meters_per_second, rhs%meters_per_second, within)
    end function

    elemental function not_equal(lhs, rhs)
        class(speed_t), intent(in) :: lhs
        type(speed_t), intent(in) :: rhs
        logical :: not_equal

        not_equal = .not. lhs == rhs
    end function

    elemental function to_string_full_precision(self) result(string)
        class(speed_t), intent(in) :: self
        type(varying_string) :: string

        string = self%to_string_in(DEFAULT_OUTPUT_UNITS)
    end function

    elemental function to_string_with_precision(self, significant_digits) result(string)
        class(speed_t), intent(in) :: self
        integer, intent(in) :: significant_digits
        type(varying_string) :: string

        string = self%to_string_in(DEFAULT_OUTPUT_UNITS, significant_digits)
    end function

    elemental function to_string_in_full_precision(self, units) result(string)
        class(speed_t), intent(in) :: self
        class(speed_unit_t), intent(in) :: units
        type(varying_string) :: string

        string = units%to_string(to_string(self.in.units))
    end function

    elemental function to_string_in_with_precision( &
            self, units, significant_digits) result(string)
        class(speed_t), intent(in) :: self
        class(speed_unit_t), intent(in) :: units
        integer, intent(in) :: significant_digits
        type(varying_string) :: string

        string = units%to_string(to_string(self.in.units, significant_digits))
    end function

    function fallible_speed_from_speed( &
            speed) result(fallible_speed)
        type(speed_t), intent(in) :: speed
        type(fallible_speed_t) :: fallible_speed

        fallible_speed%speed_ = speed
    end function

    function fallible_speed_from_errors( &
            errors) result(fallible_speed)
        type(error_list_t), intent(in) :: errors
        type(fallible_speed_t) :: fallible_speed

        fallible_speed%errors_ = errors
    end function

    function fallible_speed_from_fallible_speed( &
            fallible_speed, &
            module_, &
            procedure_) &
            result(new_fallible_speed)
        type(fallible_speed_t), intent(in) :: fallible_speed
        type(module_t), intent(in) :: module_
        type(procedure_t), intent(in) :: procedure_
        type(fallible_speed_t) :: new_fallible_speed

        if (fallible_speed%failed()) then
            new_fallible_speed%errors_ = error_list_t( &
                    fallible_speed%errors_, module_, procedure_)
        else
            new_fallible_speed%speed_ = fallible_speed%speed_
        end if
    end function

    elemental function fallible_speed_failed( &
            self) result(failed)
        class(fallible_speed_t), intent(in) :: self
        logical :: failed

        failed = self%errors_%has_any()
    end function

    elemental function fallible_speed_speed( &
            self) result(speed)
        class(fallible_speed_t), intent(in) :: self
        type(speed_t) :: speed

        speed = self%speed_
    end function

    impure elemental function fallible_speed_errors( &
            self) result(errors)
        class(fallible_speed_t), intent(in) :: self
        type(error_list_t) :: errors

        errors = self%errors_
    end function

    function fallible_speed_unit_from_unit( &
            unit) result(fallible_speed_unit)
        class(speed_unit_t), intent(in) :: unit
        type(fallible_speed_unit_t) :: fallible_speed_unit

        allocate(fallible_speed_unit%unit_, source = unit)
    end function

    function fallible_speed_unit_from_errors( &
            errors) result(fallible_speed_unit)
        type(error_list_t), intent(in) :: errors
        type(fallible_speed_unit_t) :: fallible_speed_unit

        fallible_speed_unit%errors_ = errors
    end function

    function fallible_speed_unit_from_fallible_speed_unit( &
            fallible_speed_unit, &
            module_, &
            procedure_) &
            result(new_fallible_speed_unit)
        type(fallible_speed_unit_t), intent(in) :: fallible_speed_unit
        type(module_t), intent(in) :: module_
        type(procedure_t), intent(in) :: procedure_
        type(fallible_speed_unit_t) :: new_fallible_speed_unit

        if (fallible_speed_unit%failed()) then
            new_fallible_speed_unit%errors_ = error_list_t( &
                    fallible_speed_unit%errors_, module_, procedure_)
        else
            allocate(new_fallible_speed_unit%unit_, source = &
                    fallible_speed_unit%unit_)
        end if
    end function

    elemental function fallible_speed_unit_failed( &
            self) result(failed)
        class(fallible_speed_unit_t), intent(in) :: self
        logical :: failed

        failed = self%errors_%has_any()
    end function

    function fallible_speed_unit_unit( &
            self) result(unit)
        class(fallible_speed_unit_t), intent(in) :: self
        class(speed_unit_t), allocatable :: unit

        allocate(unit, source = self%unit_)
    end function

    impure elemental function fallible_speed_unit_errors( &
            self) result(errors)
        class(fallible_speed_unit_t), intent(in) :: self
        type(error_list_t) :: errors

        errors = self%errors_
    end function

    elemental function simple_unit_to_string(self) result(string)
        class(speed_simple_unit_t), intent(in) :: self
        type(varying_string) :: string

        string = trim(self%symbol)
    end function

    pure function simple_value_to_string(self, value_) result(string)
        class(speed_simple_unit_t), intent(in) :: self
        type(varying_string), intent(in) :: value_
        type(varying_string) :: string

        string = value_ // " " // self%to_string()
    end function

    function simple_parse_as(self, string) result(fallible_speed)
        class(speed_simple_unit_t), intent(in) :: self
        type(varying_string), intent(in) :: string
        type(fallible_speed_t) :: fallible_speed

        type(parse_result_t) :: parse_result

        parse_result = parse_with(the_parser, string)
        if (parse_result%ok) then
            select type (the_number => parse_result%parsed)
            type is (parsed_rational_t)
                fallible_speed = fallible_speed_t(the_number%value_.unit.self)
            end select
        else
            fallible_speed = fallible_speed_t(error_list_t(fatal_t( &
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

    function parse_speed_unit_c(string) result(fallible_speed_unit)
        character(len=*), intent(in) :: string
        type(fallible_speed_unit_t) :: fallible_speed_unit

        fallible_speed_unit = fallible_speed_unit_t( &
                parse_speed_unit(var_str(string), PROVIDED_UNITS), &
                module_t(MODULE_NAME), &
                procedure_t("parse_speed_unit_c"))
    end function

    function parse_speed_unit_s(string) result(fallible_speed_unit)
        type(varying_string), intent(in) :: string
        type(fallible_speed_unit_t) :: fallible_speed_unit

        fallible_speed_unit = fallible_speed_unit_t( &
                parse_speed_unit(string, PROVIDED_UNITS), &
                module_t(MODULE_NAME), &
                procedure_t("parse_speed_unit_s"))
    end function

    function parse_speed_unit_with_units_c( &
            string, units) result(fallible_speed_unit)
        character(len=*), intent(in) :: string
        class(speed_unit_t), intent(in) :: units(:)
        type(fallible_speed_unit_t) :: fallible_speed_unit

        fallible_speed_unit = fallible_speed_unit_t( &
                parse_speed_unit(var_str(string), units), &
                module_t(MODULE_NAME), &
                procedure_t("parse_speed_unit_with_units_c"))
    end function

    function parse_speed_unit_with_units_s( &
            string, units) result(fallible_speed_unit)
        type(varying_string), intent(in) :: string
        class(speed_unit_t), intent(in) :: units(:)
        type(fallible_speed_unit_t) :: fallible_speed_unit

        integer :: i
        type(varying_string), allocatable :: unit_strings(:)

        do i = 1, size(units)
            if (string == units(i)%to_string()) then
                fallible_speed_unit = fallible_speed_unit_t(units(i))
                return
            end if
        end do
        allocate(unit_strings(size(units)))
        do i = 1, size(units)
            unit_strings(i) = units(i)%to_string()
        end do
        fallible_speed_unit = fallible_speed_unit_t(error_list_t(fatal_t( &
                UNKNOWN_UNIT, &
                module_t(MODULE_NAME), &
                procedure_t("parse_speed_unit_with_units_s"), &
                '"' // string // '", known units: [' // join(unit_strings, ', ') // ']')))
    end function
end module
