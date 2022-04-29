module frequency_m
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
    use quaff_conversion_factors_m, only: &
            SECONDS_PER_MINUTE, &
            SECONDS_PER_YEAR
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
            frequency_t, &
            fallible_frequency_t, &
            frequency_unit_t, &
            fallible_frequency_unit_t, &
            frequency_simple_unit_t, &
            operator(.unit.), &
            parse_frequency, &
            parse_frequency_unit, &
            sum, &
            DEFAULT_OUTPUT_UNITS, &
            PROVIDED_UNITS, &
            HERTZ, &
            PER_SECOND, &
            PER_MINUTE, &
            PER_YEAR

    type :: frequency_t
        double precision :: hertz
    contains
        private
        procedure :: to_units
        generic, public :: operator(.in.) => to_units
        procedure, pass(frequency) :: double_times_frequency
        procedure, pass(frequency) :: integer_times_frequency
        procedure :: frequency_times_double
        procedure :: frequency_times_integer
        generic, public :: operator(*) => &
                double_times_frequency, &
                integer_times_frequency, &
                frequency_times_double, &
                frequency_times_integer
        procedure :: frequency_divided_by_double
        procedure :: frequency_divided_by_integer
        procedure :: frequency_divided_by_frequency
        generic, public :: operator(/) => &
                frequency_divided_by_double, &
                frequency_divided_by_integer, &
                frequency_divided_by_frequency
        procedure :: frequency_plus_frequency
        generic, public :: operator(+) => frequency_plus_frequency
        procedure :: frequency_minus_frequency
        generic, public :: operator(-) => frequency_minus_frequency
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

    type :: fallible_frequency_t
        private
        type(frequency_t) :: frequency_ = frequency_t(0.0d0)
        type(error_list_t) :: errors_
    contains
        private
        procedure, public :: failed => fallible_frequency_failed
        procedure, public :: frequency => fallible_frequency_frequency
        procedure, public :: errors => fallible_frequency_errors
    end type

    type, abstract :: frequency_unit_t
        double precision :: conversion_factor
    contains
        procedure(unit_to_string_i), deferred :: unit_to_string
        procedure(value_to_string_i), deferred :: value_to_string
        generic :: to_string => unit_to_string, value_to_string
        procedure(parse_as_i), deferred :: parse_as
    end type

    type :: fallible_frequency_unit_t
        private
        class(frequency_unit_t), allocatable :: unit_
        type(error_list_t) :: errors_
    contains
        private
        procedure, public :: failed => fallible_frequency_unit_failed
        procedure, public :: unit => fallible_frequency_unit_unit
        procedure, public :: errors => fallible_frequency_unit_errors
    end type

    type, extends(frequency_unit_t) :: frequency_simple_unit_t
        character(len=20) :: symbol
    contains
        procedure :: unit_to_string => simple_unit_to_string
        procedure :: value_to_string => simple_value_to_string
        procedure :: parse_as => simple_parse_as
    end type

    abstract interface
        elemental function unit_to_string_i(self) result(string)
            import :: frequency_unit_t, varying_string

            implicit none

            class(frequency_unit_t), intent(in) :: self
            type(varying_string) :: string
        end function

        pure function value_to_string_i(self, value_) result(string)
            import :: frequency_unit_t, varying_string

            implicit none

            class(frequency_unit_t), intent(in) :: self
            type(varying_string), intent(in) :: value_
            type(varying_string) :: string
        end function

        function parse_as_i(self, string) result(fallible_frequency)
            import :: frequency_unit_t, fallible_frequency_t, varying_string

            implicit none

            class(frequency_unit_t), intent(in) :: self
            type(varying_string), intent(in) :: string
            type(fallible_frequency_t) :: fallible_frequency
        end function
    end interface

    interface operator(.unit.)
        module procedure from_units
    end interface

    interface fallible_frequency_t
        module procedure fallible_frequency_from_frequency
        module procedure fallible_frequency_from_errors
        module procedure fallible_frequency_from_fallible_frequency
    end interface

    interface fallible_frequency_unit_t
        module procedure fallible_frequency_unit_from_unit
        module procedure fallible_frequency_unit_from_errors
        module procedure fallible_frequency_unit_from_fallible_frequency_unit
    end interface

    interface parse_frequency
        module procedure parse_frequency_c
        module procedure parse_frequency_s
        module procedure parse_frequency_with_units_c
        module procedure parse_frequency_with_units_s
    end interface

    interface parse_frequency_unit
        module procedure parse_frequency_unit_c
        module procedure parse_frequency_unit_s
        module procedure parse_frequency_unit_with_units_c
        module procedure parse_frequency_unit_with_units_s
    end interface

    interface sum
        module procedure sum_frequency
    end interface

    type(frequency_simple_unit_t), parameter :: HERTZ = &
            frequency_simple_unit_t( &
                    conversion_factor = 1.0d0, &
                    symbol = "Hz")
    type(frequency_simple_unit_t), parameter :: PER_SECOND = &
            frequency_simple_unit_t( &
                    conversion_factor = 1.0d0, &
                    symbol = "1/s")
    type(frequency_simple_unit_t), parameter :: PER_MINUTE = &
            frequency_simple_unit_t( &
                    conversion_factor = SECONDS_PER_MINUTE, &
                    symbol = "1/m")
    type(frequency_simple_unit_t), parameter :: PER_YEAR = &
            frequency_simple_unit_t( &
                    conversion_factor = SECONDS_PER_YEAR, &
                    symbol = "1/yr")

    type(frequency_simple_unit_t) :: DEFAULT_OUTPUT_UNITS = HERTZ

    type(frequency_simple_unit_t), parameter :: PROVIDED_UNITS(*) = &
            [HERTZ, PER_SECOND, PER_MINUTE, PER_YEAR]

    character(len=*), parameter :: MODULE_NAME = "frequency_m"
contains
    function parse_frequency_c(string) result(fallible_frequency)
        character(len=*), intent(in) :: string
        type(fallible_frequency_t) :: fallible_frequency

        fallible_frequency = fallible_frequency_t( &
                parse_frequency(var_str(string), PROVIDED_UNITS), &
                module_t(MODULE_NAME), &
                procedure_t("parse_frequency_c"))
    end function

    function parse_frequency_s(string) result(fallible_frequency)
        type(varying_string), intent(in) :: string
        type(fallible_frequency_t) :: fallible_frequency

        fallible_frequency = fallible_frequency_t( &
                parse_frequency(string, PROVIDED_UNITS), &
                module_t(MODULE_NAME), &
                procedure_t("parse_frequency_s"))
    end function

    function parse_frequency_with_units_c( &
            string, units) result(fallible_frequency)
        character(len=*), intent(in) :: string
        class(frequency_unit_t), intent(in) :: units(:)
        type(fallible_frequency_t) :: fallible_frequency

        fallible_frequency = fallible_frequency_t( &
                parse_frequency(var_str(string), units), &
                module_t(MODULE_NAME), &
                procedure_t("parse_frequency_with_units_c"))
    end function

    function parse_frequency_with_units_s( &
            string, units) result(fallible_frequency)
        type(varying_string), intent(in) :: string
        class(frequency_unit_t), intent(in) :: units(:)
        type(fallible_frequency_t) :: fallible_frequency

        type(fallible_frequency_t) :: all_attempts(size(units))
        integer :: i

        do i = 1, size(units)
            all_attempts(i) = units(i)%parse_as(string)
            if (.not. all_attempts(i)%failed()) then
                fallible_frequency = all_attempts(i)
                return
            end if
        end do
        fallible_frequency = fallible_frequency_t(error_list_t( &
                all_attempts%errors(), &
                module_t(MODULE_NAME), &
                procedure_t("parse_frequency_with_units_s")))
    end function

    elemental function from_units(value_, units) result(frequency)
        double precision, intent(in) :: value_
        class(frequency_unit_t), intent(in) :: units
        type(frequency_t) :: frequency

        frequency%hertz = value_ / units%conversion_factor
    end function

    elemental function to_units(self, units) result(frequency)
        class(frequency_t), intent(in) :: self
        class(frequency_unit_t), intent(in) :: units
        double precision :: frequency

        frequency = self%hertz * units%conversion_factor
    end function

    elemental function double_times_frequency( &
            multiplier, frequency) result(new_frequency)
        double precision, intent(in) :: multiplier
        class(frequency_t), intent(in) :: frequency
        type(frequency_t) :: new_frequency

        new_frequency%hertz = &
                multiplier * frequency%hertz
    end function

    elemental function integer_times_frequency( &
            multiplier, frequency) result(new_frequency)
        integer, intent(in) :: multiplier
        class(frequency_t), intent(in) :: frequency
        type(frequency_t) :: new_frequency

        new_frequency%hertz = &
                dble(multiplier) * frequency%hertz
    end function

    elemental function frequency_times_double( &
            frequency, multiplier) result(new_frequency)
        class(frequency_t), intent(in) :: frequency
        double precision, intent(in) :: multiplier
        type(frequency_t) :: new_frequency

        new_frequency%hertz = &
                frequency%hertz * multiplier
    end function

    elemental function frequency_times_integer( &
            frequency, multiplier) result(new_frequency)
        class(frequency_t), intent(in) :: frequency
        integer, intent(in) :: multiplier
        type(frequency_t) :: new_frequency

        new_frequency%hertz = &
                frequency%hertz * dble(multiplier)
    end function

    elemental function frequency_divided_by_double( &
            frequency, divisor) result(new_frequency)
        class(frequency_t), intent(in) :: frequency
        double precision, intent(in) :: divisor
        type(frequency_t) :: new_frequency

        new_frequency%hertz = &
                frequency%hertz / divisor
    end function

    elemental function frequency_divided_by_integer( &
            frequency, divisor) result(new_frequency)
        class(frequency_t), intent(in) :: frequency
        integer, intent(in) :: divisor
        type(frequency_t) :: new_frequency

        new_frequency%hertz = &
                frequency%hertz / dble(divisor)
    end function

    elemental function frequency_divided_by_frequency( &
            numerator, denomenator) result(ratio)
        class(frequency_t), intent(in) :: numerator
        type(frequency_t), intent(in) :: denomenator
        double precision :: ratio

        ratio = numerator%hertz / denomenator%hertz
    end function

    elemental function frequency_plus_frequency( &
            lhs, rhs) result(sum_)
        class(frequency_t), intent(in) :: lhs
        type(frequency_t), intent(in) :: rhs
        type(frequency_t) :: sum_

        sum_%hertz = lhs%hertz + rhs%hertz
    end function

    elemental function frequency_minus_frequency( &
            lhs, rhs) result(difference)
        class(frequency_t), intent(in) :: lhs
        type(frequency_t), intent(in) :: rhs
        type(frequency_t) :: difference

        difference%hertz = lhs%hertz - rhs%hertz
    end function

    pure function sum_frequency(frequencys) result(sum_)
        type(frequency_t), intent(in) :: frequencys(:)
        type(frequency_t) :: sum_

        sum_%hertz = sum(frequencys%hertz)
    end function

    elemental function greater_than(lhs, rhs)
        class(frequency_t), intent(in) :: lhs
        type(frequency_t), intent(in) :: rhs
        logical :: greater_than

        greater_than = lhs%hertz > rhs%hertz
    end function

    elemental function less_than(lhs, rhs)
        class(frequency_t), intent(in) :: lhs
        type(frequency_t), intent(in) :: rhs
        logical :: less_than

        less_than = lhs%hertz < rhs%hertz
    end function

    elemental function greater_than_or_equal(lhs, rhs)
        class(frequency_t), intent(in) :: lhs
        type(frequency_t), intent(in) :: rhs
        logical :: greater_than_or_equal

        greater_than_or_equal = lhs%hertz >= rhs%hertz
    end function

    elemental function less_than_or_equal(lhs, rhs)
        class(frequency_t), intent(in) :: lhs
        type(frequency_t), intent(in) :: rhs
        logical :: less_than_or_equal

        less_than_or_equal = lhs%hertz <= rhs%hertz
    end function

    elemental function equal_(lhs, rhs)
        class(frequency_t), intent(in) :: lhs
        type(frequency_t), intent(in) :: rhs
        logical :: equal_

        equal_ = lhs%hertz .safeEq. rhs%hertz
    end function

    elemental function equal_within_absolute_(lhs, rhs, within)
        class(frequency_t), intent(in) :: lhs
        type(frequency_t), intent(in) :: rhs
        type(frequency_t), intent(in) :: within
        logical :: equal_within_absolute_

        equal_within_absolute_ = equal_within_absolute( &
                lhs%hertz, rhs%hertz, within%hertz)
    end function

    elemental function equal_within_relative_(lhs, rhs, within)
        class(frequency_t), intent(in) :: lhs
        type(frequency_t), intent(in) :: rhs
        double precision, intent(in) :: within
        logical :: equal_within_relative_

        equal_within_relative_ = equal_within_relative( &
                lhs%hertz, rhs%hertz, within)
    end function

    elemental function not_equal(lhs, rhs)
        class(frequency_t), intent(in) :: lhs
        type(frequency_t), intent(in) :: rhs
        logical :: not_equal

        not_equal = .not. lhs == rhs
    end function

    elemental function to_string_full_precision(self) result(string)
        class(frequency_t), intent(in) :: self
        type(varying_string) :: string

        string = self%to_string_in(DEFAULT_OUTPUT_UNITS)
    end function

    elemental function to_string_with_precision(self, significant_digits) result(string)
        class(frequency_t), intent(in) :: self
        integer, intent(in) :: significant_digits
        type(varying_string) :: string

        string = self%to_string_in(DEFAULT_OUTPUT_UNITS, significant_digits)
    end function

    elemental function to_string_in_full_precision(self, units) result(string)
        class(frequency_t), intent(in) :: self
        class(frequency_unit_t), intent(in) :: units
        type(varying_string) :: string

        string = units%to_string(to_string(self.in.units))
    end function

    elemental function to_string_in_with_precision( &
            self, units, significant_digits) result(string)
        class(frequency_t), intent(in) :: self
        class(frequency_unit_t), intent(in) :: units
        integer, intent(in) :: significant_digits
        type(varying_string) :: string

        string = units%to_string(to_string(self.in.units, significant_digits))
    end function

    function fallible_frequency_from_frequency( &
            frequency) result(fallible_frequency)
        type(frequency_t), intent(in) :: frequency
        type(fallible_frequency_t) :: fallible_frequency

        fallible_frequency%frequency_ = frequency
    end function

    function fallible_frequency_from_errors( &
            errors) result(fallible_frequency)
        type(error_list_t), intent(in) :: errors
        type(fallible_frequency_t) :: fallible_frequency

        fallible_frequency%errors_ = errors
    end function

    function fallible_frequency_from_fallible_frequency( &
            fallible_frequency, &
            module_, &
            procedure_) &
            result(new_fallible_frequency)
        type(fallible_frequency_t), intent(in) :: fallible_frequency
        type(module_t), intent(in) :: module_
        type(procedure_t), intent(in) :: procedure_
        type(fallible_frequency_t) :: new_fallible_frequency

        if (fallible_frequency%failed()) then
            new_fallible_frequency%errors_ = error_list_t( &
                    fallible_frequency%errors_, module_, procedure_)
        else
            new_fallible_frequency%frequency_ = fallible_frequency%frequency_
        end if
    end function

    elemental function fallible_frequency_failed( &
            self) result(failed)
        class(fallible_frequency_t), intent(in) :: self
        logical :: failed

        failed = self%errors_%has_any()
    end function

    elemental function fallible_frequency_frequency( &
            self) result(frequency)
        class(fallible_frequency_t), intent(in) :: self
        type(frequency_t) :: frequency

        frequency = self%frequency_
    end function

    impure elemental function fallible_frequency_errors( &
            self) result(errors)
        class(fallible_frequency_t), intent(in) :: self
        type(error_list_t) :: errors

        errors = self%errors_
    end function

    function fallible_frequency_unit_from_unit( &
            unit) result(fallible_frequency_unit)
        class(frequency_unit_t), intent(in) :: unit
        type(fallible_frequency_unit_t) :: fallible_frequency_unit

        allocate(fallible_frequency_unit%unit_, source = unit)
    end function

    function fallible_frequency_unit_from_errors( &
            errors) result(fallible_frequency_unit)
        type(error_list_t), intent(in) :: errors
        type(fallible_frequency_unit_t) :: fallible_frequency_unit

        fallible_frequency_unit%errors_ = errors
    end function

    function fallible_frequency_unit_from_fallible_frequency_unit( &
            fallible_frequency_unit, &
            module_, &
            procedure_) &
            result(new_fallible_frequency_unit)
        type(fallible_frequency_unit_t), intent(in) :: fallible_frequency_unit
        type(module_t), intent(in) :: module_
        type(procedure_t), intent(in) :: procedure_
        type(fallible_frequency_unit_t) :: new_fallible_frequency_unit

        if (fallible_frequency_unit%failed()) then
            new_fallible_frequency_unit%errors_ = error_list_t( &
                    fallible_frequency_unit%errors_, module_, procedure_)
        else
            allocate(new_fallible_frequency_unit%unit_, source = &
                    fallible_frequency_unit%unit_)
        end if
    end function

    elemental function fallible_frequency_unit_failed( &
            self) result(failed)
        class(fallible_frequency_unit_t), intent(in) :: self
        logical :: failed

        failed = self%errors_%has_any()
    end function

    function fallible_frequency_unit_unit( &
            self) result(unit)
        class(fallible_frequency_unit_t), intent(in) :: self
        class(frequency_unit_t), allocatable :: unit

        allocate(unit, source = self%unit_)
    end function

    impure elemental function fallible_frequency_unit_errors( &
            self) result(errors)
        class(fallible_frequency_unit_t), intent(in) :: self
        type(error_list_t) :: errors

        errors = self%errors_
    end function

    elemental function simple_unit_to_string(self) result(string)
        class(frequency_simple_unit_t), intent(in) :: self
        type(varying_string) :: string

        string = trim(self%symbol)
    end function

    pure function simple_value_to_string(self, value_) result(string)
        class(frequency_simple_unit_t), intent(in) :: self
        type(varying_string), intent(in) :: value_
        type(varying_string) :: string

        string = value_ // " " // self%to_string()
    end function

    function simple_parse_as(self, string) result(fallible_frequency)
        class(frequency_simple_unit_t), intent(in) :: self
        type(varying_string), intent(in) :: string
        type(fallible_frequency_t) :: fallible_frequency

        type(parse_result_t) :: parse_result

        parse_result = parse_with(the_parser, string)
        if (parse_result%ok) then
            select type (the_number => parse_result%parsed)
            type is (parsed_rational_t)
                fallible_frequency = fallible_frequency_t(the_number%value_.unit.self)
            end select
        else
            fallible_frequency = fallible_frequency_t(error_list_t(fatal_t( &
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

    function parse_frequency_unit_c(string) result(fallible_frequency_unit)
        character(len=*), intent(in) :: string
        type(fallible_frequency_unit_t) :: fallible_frequency_unit

        fallible_frequency_unit = fallible_frequency_unit_t( &
                parse_frequency_unit(var_str(string), PROVIDED_UNITS), &
                module_t(MODULE_NAME), &
                procedure_t("parse_frequency_unit_c"))
    end function

    function parse_frequency_unit_s(string) result(fallible_frequency_unit)
        type(varying_string), intent(in) :: string
        type(fallible_frequency_unit_t) :: fallible_frequency_unit

        fallible_frequency_unit = fallible_frequency_unit_t( &
                parse_frequency_unit(string, PROVIDED_UNITS), &
                module_t(MODULE_NAME), &
                procedure_t("parse_frequency_unit_s"))
    end function

    function parse_frequency_unit_with_units_c( &
            string, units) result(fallible_frequency_unit)
        character(len=*), intent(in) :: string
        class(frequency_unit_t), intent(in) :: units(:)
        type(fallible_frequency_unit_t) :: fallible_frequency_unit

        fallible_frequency_unit = fallible_frequency_unit_t( &
                parse_frequency_unit(var_str(string), units), &
                module_t(MODULE_NAME), &
                procedure_t("parse_frequency_unit_with_units_c"))
    end function

    function parse_frequency_unit_with_units_s( &
            string, units) result(fallible_frequency_unit)
        type(varying_string), intent(in) :: string
        class(frequency_unit_t), intent(in) :: units(:)
        type(fallible_frequency_unit_t) :: fallible_frequency_unit

        integer :: i
        type(varying_string) :: unit_strings(size(units))

        do i = 1, size(units)
            if (string == units(i)%to_string()) then
                fallible_frequency_unit = fallible_frequency_unit_t(units(i))
                return
            end if
        end do
        do i = 1, size(units)
            unit_strings(i) = units(i)%to_string()
        end do
        fallible_frequency_unit = fallible_frequency_unit_t(error_list_t(fatal_t( &
                UNKNOWN_UNIT, &
                module_t(MODULE_NAME), &
                procedure_t("parse_frequency_unit_with_units_s"), &
                '"' // string // '", known units: [' // join(unit_strings, ', ') // ']')))
    end function
end module
