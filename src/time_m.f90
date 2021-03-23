module time_m
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
            DAYS_PER_SECOND, &
            HOURS_PER_SECOND, &
            MINUTES_PER_SECOND
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
            time_t, &
            fallible_time_t, &
            time_unit_t, &
            fallible_time_unit_t, &
            time_simple_unit_t, &
            operator(.unit.), &
            parse_time, &
            parse_time_unit, &
            sum, &
            DEFAULT_OUTPUT_UNITS, &
            PROVIDED_UNITS, &
            DAYS, &
            HOURS, &
            MINUTES, &
            SECONDS

    type :: time_t
        double precision :: seconds
    contains
        private
        procedure :: to_units
        generic, public :: operator(.in.) => to_units
        procedure, pass(time) :: double_times_time
        procedure, pass(time) :: integer_times_time
        procedure :: time_times_double
        procedure :: time_times_integer
        generic, public :: operator(*) => &
                double_times_time, &
                integer_times_time, &
                time_times_double, &
                time_times_integer
        procedure :: time_divided_by_double
        procedure :: time_divided_by_integer
        procedure :: time_divided_by_time
        generic, public :: operator(/) => &
                time_divided_by_double, &
                time_divided_by_integer, &
                time_divided_by_time
        procedure :: time_plus_time
        generic, public :: operator(+) => time_plus_time
        procedure :: time_minus_time
        generic, public :: operator(-) => time_minus_time
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

    type :: fallible_time_t
        private
        type(time_t) :: time_
        type(error_list_t) :: errors_
    contains
        private
        procedure, public :: failed => fallible_time_failed
        procedure, public :: time => fallible_time_time
        procedure, public :: errors => fallible_time_errors
    end type

    type, abstract :: time_unit_t
        double precision :: conversion_factor
    contains
        procedure(unit_to_string_i), deferred :: unit_to_string
        procedure(value_to_string_i), deferred :: value_to_string
        generic :: to_string => unit_to_string, value_to_string
        procedure(parse_as_i), deferred :: parse_as
    end type

    type :: fallible_time_unit_t
        private
        class(time_unit_t), allocatable :: unit_
        type(error_list_t) :: errors_
    contains
        private
        procedure, public :: failed => fallible_time_unit_failed
        procedure, public :: unit => fallible_time_unit_unit
        procedure, public :: errors => fallible_time_unit_errors
    end type

    type, extends(time_unit_t) :: time_simple_unit_t
        character(len=20) :: symbol
    contains
        procedure :: unit_to_string => simple_unit_to_string
        procedure :: value_to_string => simple_value_to_string
        procedure :: parse_as => simple_parse_as
    end type

    abstract interface
        elemental function unit_to_string_i(self) result(string)
            import :: time_unit_t, varying_string

            implicit none

            class(time_unit_t), intent(in) :: self
            type(varying_string) :: string
        end function

        pure function value_to_string_i(self, value_) result(string)
            import :: time_unit_t, varying_string

            implicit none

            class(time_unit_t), intent(in) :: self
            type(varying_string), intent(in) :: value_
            type(varying_string) :: string
        end function

        function parse_as_i(self, string) result(fallible_time)
            import :: time_unit_t, fallible_time_t, varying_string

            implicit none

            class(time_unit_t), intent(in) :: self
            type(varying_string), intent(in) :: string
            type(fallible_time_t) :: fallible_time
        end function
    end interface

    interface operator(.unit.)
        module procedure from_units
    end interface

    interface fallible_time_t
        module procedure fallible_time_from_time
        module procedure fallible_time_from_errors
        module procedure fallible_time_from_fallible_time
    end interface

    interface fallible_time_unit_t
        module procedure fallible_time_unit_from_unit
        module procedure fallible_time_unit_from_errors
        module procedure fallible_time_unit_from_fallible_time_unit
    end interface

    interface parse_time
        module procedure parse_time_c
        module procedure parse_time_s
        module procedure parse_time_with_units_c
        module procedure parse_time_with_units_s
    end interface

    interface parse_time_unit
        module procedure parse_time_unit_c
        module procedure parse_time_unit_s
        module procedure parse_time_unit_with_units_c
        module procedure parse_time_unit_with_units_s
    end interface

    interface sum
        module procedure sum_time
    end interface

    type(time_simple_unit_t), parameter :: DAYS = &
            time_simple_unit_t( &
                    conversion_factor = DAYS_PER_SECOND, &
                    symbol = "d")
    type(time_simple_unit_t), parameter :: HOURS = &
            time_simple_unit_t( &
                    conversion_factor = HOURS_PER_SECOND, &
                    symbol = "h")
    type(time_simple_unit_t), parameter :: MINUTES = &
            time_simple_unit_t( &
                    conversion_factor = MINUTES_PER_SECOND, &
                    symbol = "m")
    type(time_simple_unit_t), parameter :: SECONDS = &
            time_simple_unit_t( &
                    conversion_factor = 1.0d0, &
                    symbol = "s")

    type(time_simple_unit_t) :: DEFAULT_OUTPUT_UNITS = SECONDS

    type(time_simple_unit_t), parameter :: PROVIDED_UNITS(*) = &
            [DAYS, HOURS, MINUTES, SECONDS]

    character(len=*), parameter :: MODULE_NAME = "time_m"
contains
    function parse_time_c(string) result(fallible_time)
        character(len=*), intent(in) :: string
        type(fallible_time_t) :: fallible_time

        fallible_time = fallible_time_t( &
                parse_time(var_str(string), PROVIDED_UNITS), &
                module_t(MODULE_NAME), &
                procedure_t("parse_time_c"))
    end function

    function parse_time_s(string) result(fallible_time)
        type(varying_string), intent(in) :: string
        type(fallible_time_t) :: fallible_time

        fallible_time = fallible_time_t( &
                parse_time(string, PROVIDED_UNITS), &
                module_t(MODULE_NAME), &
                procedure_t("parse_time_s"))
    end function

    function parse_time_with_units_c( &
            string, units) result(fallible_time)
        character(len=*), intent(in) :: string
        class(time_unit_t), intent(in) :: units(:)
        type(fallible_time_t) :: fallible_time

        fallible_time = fallible_time_t( &
                parse_time(var_str(string), units), &
                module_t(MODULE_NAME), &
                procedure_t("parse_time_with_units_c"))
    end function

    function parse_time_with_units_s( &
            string, units) result(fallible_time)
        type(varying_string), intent(in) :: string
        class(time_unit_t), intent(in) :: units(:)
        type(fallible_time_t) :: fallible_time

        type(fallible_time_t) :: all_attempts(size(units))
        integer :: i

        do i = 1, size(units)
            all_attempts(i) = units(i)%parse_as(string)
            if (.not. all_attempts(i)%failed()) then
                fallible_time = all_attempts(i)
                return
            end if
        end do
        fallible_time = fallible_time_t(error_list_t( &
                all_attempts%errors(), &
                module_t(MODULE_NAME), &
                procedure_t("parse_time_with_units_s")))
    end function

    elemental function from_units(value_, units) result(time)
        double precision, intent(in) :: value_
        class(time_unit_t), intent(in) :: units
        type(time_t) :: time

        time%seconds = value_ / units%conversion_factor
    end function

    elemental function to_units(self, units) result(time)
        class(time_t), intent(in) :: self
        class(time_unit_t), intent(in) :: units
        double precision :: time

        time = self%seconds * units%conversion_factor
    end function

    elemental function double_times_time( &
            multiplier, time) result(new_time)
        double precision, intent(in) :: multiplier
        class(time_t), intent(in) :: time
        type(time_t) :: new_time

        new_time%seconds = &
                multiplier * time%seconds
    end function

    elemental function integer_times_time( &
            multiplier, time) result(new_time)
        integer, intent(in) :: multiplier
        class(time_t), intent(in) :: time
        type(time_t) :: new_time

        new_time%seconds = &
                dble(multiplier) * time%seconds
    end function

    elemental function time_times_double( &
            time, multiplier) result(new_time)
        class(time_t), intent(in) :: time
        double precision, intent(in) :: multiplier
        type(time_t) :: new_time

        new_time%seconds = &
                time%seconds * multiplier
    end function

    elemental function time_times_integer( &
            time, multiplier) result(new_time)
        class(time_t), intent(in) :: time
        integer, intent(in) :: multiplier
        type(time_t) :: new_time

        new_time%seconds = &
                time%seconds * dble(multiplier)
    end function

    elemental function time_divided_by_double( &
            time, divisor) result(new_time)
        class(time_t), intent(in) :: time
        double precision, intent(in) :: divisor
        type(time_t) :: new_time

        new_time%seconds = &
                time%seconds / divisor
    end function

    elemental function time_divided_by_integer( &
            time, divisor) result(new_time)
        class(time_t), intent(in) :: time
        integer, intent(in) :: divisor
        type(time_t) :: new_time

        new_time%seconds = &
                time%seconds / dble(divisor)
    end function

    elemental function time_divided_by_time( &
            numerator, denomenator) result(ratio)
        class(time_t), intent(in) :: numerator
        type(time_t), intent(in) :: denomenator
        double precision :: ratio

        ratio = numerator%seconds / denomenator%seconds
    end function

    elemental function time_plus_time( &
            lhs, rhs) result(sum_)
        class(time_t), intent(in) :: lhs
        type(time_t), intent(in) :: rhs
        type(time_t) :: sum_

        sum_%seconds = lhs%seconds + rhs%seconds
    end function

    elemental function time_minus_time( &
            lhs, rhs) result(difference)
        class(time_t), intent(in) :: lhs
        type(time_t), intent(in) :: rhs
        type(time_t) :: difference

        difference%seconds = lhs%seconds - rhs%seconds
    end function

    pure function sum_time(times) result(sum_)
        type(time_t), intent(in) :: times(:)
        type(time_t) :: sum_

        sum_%seconds = sum(times%seconds)
    end function

    elemental function greater_than(lhs, rhs)
        class(time_t), intent(in) :: lhs
        type(time_t), intent(in) :: rhs
        logical :: greater_than

        greater_than = lhs%seconds > rhs%seconds
    end function

    elemental function less_than(lhs, rhs)
        class(time_t), intent(in) :: lhs
        type(time_t), intent(in) :: rhs
        logical :: less_than

        less_than = lhs%seconds < rhs%seconds
    end function

    elemental function greater_than_or_equal(lhs, rhs)
        class(time_t), intent(in) :: lhs
        type(time_t), intent(in) :: rhs
        logical :: greater_than_or_equal

        greater_than_or_equal = lhs%seconds >= rhs%seconds
    end function

    elemental function less_than_or_equal(lhs, rhs)
        class(time_t), intent(in) :: lhs
        type(time_t), intent(in) :: rhs
        logical :: less_than_or_equal

        less_than_or_equal = lhs%seconds <= rhs%seconds
    end function

    elemental function equal_(lhs, rhs)
        class(time_t), intent(in) :: lhs
        type(time_t), intent(in) :: rhs
        logical :: equal_

        equal_ = lhs%seconds .safeEq. rhs%seconds
    end function

    elemental function equal_within_absolute_(lhs, rhs, within)
        class(time_t), intent(in) :: lhs
        type(time_t), intent(in) :: rhs
        type(time_t), intent(in) :: within
        logical :: equal_within_absolute_

        equal_within_absolute_ = equal_within_absolute( &
                lhs%seconds, rhs%seconds, within%seconds)
    end function

    elemental function equal_within_relative_(lhs, rhs, within)
        class(time_t), intent(in) :: lhs
        type(time_t), intent(in) :: rhs
        double precision, intent(in) :: within
        logical :: equal_within_relative_

        equal_within_relative_ = equal_within_relative( &
                lhs%seconds, rhs%seconds, within)
    end function

    elemental function not_equal(lhs, rhs)
        class(time_t), intent(in) :: lhs
        type(time_t), intent(in) :: rhs
        logical :: not_equal

        not_equal = .not. lhs == rhs
    end function

    elemental function to_string_full_precision(self) result(string)
        class(time_t), intent(in) :: self
        type(varying_string) :: string

        string = self%to_string_in(DEFAULT_OUTPUT_UNITS)
    end function

    elemental function to_string_with_precision(self, significant_digits) result(string)
        class(time_t), intent(in) :: self
        integer, intent(in) :: significant_digits
        type(varying_string) :: string

        string = self%to_string_in(DEFAULT_OUTPUT_UNITS, significant_digits)
    end function

    elemental function to_string_in_full_precision(self, units) result(string)
        class(time_t), intent(in) :: self
        class(time_unit_t), intent(in) :: units
        type(varying_string) :: string

        string = units%to_string(to_string(self.in.units))
    end function

    elemental function to_string_in_with_precision( &
            self, units, significant_digits) result(string)
        class(time_t), intent(in) :: self
        class(time_unit_t), intent(in) :: units
        integer, intent(in) :: significant_digits
        type(varying_string) :: string

        string = units%to_string(to_string(self.in.units, significant_digits))
    end function

    function fallible_time_from_time( &
            time) result(fallible_time)
        type(time_t), intent(in) :: time
        type(fallible_time_t) :: fallible_time

        fallible_time%time_ = time
    end function

    function fallible_time_from_errors( &
            errors) result(fallible_time)
        type(error_list_t), intent(in) :: errors
        type(fallible_time_t) :: fallible_time

        fallible_time%errors_ = errors
    end function

    function fallible_time_from_fallible_time( &
            fallible_time, &
            module_, &
            procedure_) &
            result(new_fallible_time)
        type(fallible_time_t), intent(in) :: fallible_time
        type(module_t), intent(in) :: module_
        type(procedure_t), intent(in) :: procedure_
        type(fallible_time_t) :: new_fallible_time

        if (fallible_time%failed()) then
            new_fallible_time%errors_ = error_list_t( &
                    fallible_time%errors_, module_, procedure_)
        else
            new_fallible_time%time_ = fallible_time%time_
        end if
    end function

    elemental function fallible_time_failed( &
            self) result(failed)
        class(fallible_time_t), intent(in) :: self
        logical :: failed

        failed = self%errors_%has_any()
    end function

    elemental function fallible_time_time( &
            self) result(time)
        class(fallible_time_t), intent(in) :: self
        type(time_t) :: time

        time = self%time_
    end function

    impure elemental function fallible_time_errors( &
            self) result(errors)
        class(fallible_time_t), intent(in) :: self
        type(error_list_t) :: errors

        errors = self%errors_
    end function

    function fallible_time_unit_from_unit( &
            unit) result(fallible_time_unit)
        class(time_unit_t), intent(in) :: unit
        type(fallible_time_unit_t) :: fallible_time_unit

        allocate(fallible_time_unit%unit_, source = unit)
    end function

    function fallible_time_unit_from_errors( &
            errors) result(fallible_time_unit)
        type(error_list_t), intent(in) :: errors
        type(fallible_time_unit_t) :: fallible_time_unit

        fallible_time_unit%errors_ = errors
    end function

    function fallible_time_unit_from_fallible_time_unit( &
            fallible_time_unit, &
            module_, &
            procedure_) &
            result(new_fallible_time_unit)
        type(fallible_time_unit_t), intent(in) :: fallible_time_unit
        type(module_t), intent(in) :: module_
        type(procedure_t), intent(in) :: procedure_
        type(fallible_time_unit_t) :: new_fallible_time_unit

        if (fallible_time_unit%failed()) then
            new_fallible_time_unit%errors_ = error_list_t( &
                    fallible_time_unit%errors_, module_, procedure_)
        else
            allocate(new_fallible_time_unit%unit_, source = &
                    fallible_time_unit%unit_)
        end if
    end function

    elemental function fallible_time_unit_failed( &
            self) result(failed)
        class(fallible_time_unit_t), intent(in) :: self
        logical :: failed

        failed = self%errors_%has_any()
    end function

    function fallible_time_unit_unit( &
            self) result(unit)
        class(fallible_time_unit_t), intent(in) :: self
        class(time_unit_t), allocatable :: unit

        allocate(unit, source = self%unit_)
    end function

    impure elemental function fallible_time_unit_errors( &
            self) result(errors)
        class(fallible_time_unit_t), intent(in) :: self
        type(error_list_t) :: errors

        errors = self%errors_
    end function

    elemental function simple_unit_to_string(self) result(string)
        class(time_simple_unit_t), intent(in) :: self
        type(varying_string) :: string

        string = trim(self%symbol)
    end function

    pure function simple_value_to_string(self, value_) result(string)
        class(time_simple_unit_t), intent(in) :: self
        type(varying_string), intent(in) :: value_
        type(varying_string) :: string

        string = value_ // " " // self%to_string()
    end function

    function simple_parse_as(self, string) result(fallible_time)
        class(time_simple_unit_t), intent(in) :: self
        type(varying_string), intent(in) :: string
        type(fallible_time_t) :: fallible_time

        type(parse_result_t) :: parse_result

        parse_result = parse_with(the_parser, string)
        if (parse_result%ok()) then
            select type (the_number => parse_result%parsed())
            type is (parsed_rational_t)
                fallible_time = fallible_time_t(the_number%value_().unit.self)
            end select
        else
            fallible_time = fallible_time_t(error_list_t(fatal_t( &
                    PARSE_ERROR, &
                    module_t(MODULE_NAME), &
                    procedure_t("simple_parse_as"), &
                    parse_result%message())))
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

    function parse_time_unit_c(string) result(fallible_time_unit)
        character(len=*), intent(in) :: string
        type(fallible_time_unit_t) :: fallible_time_unit

        fallible_time_unit = fallible_time_unit_t( &
                parse_time_unit(var_str(string), PROVIDED_UNITS), &
                module_t(MODULE_NAME), &
                procedure_t("parse_time_unit_c"))
    end function

    function parse_time_unit_s(string) result(fallible_time_unit)
        type(varying_string), intent(in) :: string
        type(fallible_time_unit_t) :: fallible_time_unit

        fallible_time_unit = fallible_time_unit_t( &
                parse_time_unit(string, PROVIDED_UNITS), &
                module_t(MODULE_NAME), &
                procedure_t("parse_time_unit_s"))
    end function

    function parse_time_unit_with_units_c( &
            string, units) result(fallible_time_unit)
        character(len=*), intent(in) :: string
        class(time_unit_t), intent(in) :: units(:)
        type(fallible_time_unit_t) :: fallible_time_unit

        fallible_time_unit = fallible_time_unit_t( &
                parse_time_unit(var_str(string), units), &
                module_t(MODULE_NAME), &
                procedure_t("parse_time_unit_with_units_c"))
    end function

    function parse_time_unit_with_units_s( &
            string, units) result(fallible_time_unit)
        type(varying_string), intent(in) :: string
        class(time_unit_t), intent(in) :: units(:)
        type(fallible_time_unit_t) :: fallible_time_unit

        integer :: i
        type(varying_string) :: unit_strings(size(units))

        do i = 1, size(units)
            if (string == units(i)%to_string()) then
                fallible_time_unit = fallible_time_unit_t(units(i))
                return
            end if
        end do
        do i = 1, size(units)
            unit_strings(i) = units(i)%to_string()
        end do
        fallible_time_unit = fallible_time_unit_t(error_list_t(fatal_t( &
                UNKNOWN_UNIT, &
                module_t(MODULE_NAME), &
                procedure_t("parse_time_unit_with_units_s"), &
                '"' // string // '", known units: [' // join(unit_strings, ', ') // ']')))
    end function
end module
