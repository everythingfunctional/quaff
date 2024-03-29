module quaff_power_m
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
            BTU_PER_HOUR_PER_WATT, &
            CALORIES_PER_SECOND_PER_WATT, &
            MEGABTU_PER_HOUR_PER_WATT, &
            MEGAWATTS_PER_WATT
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
            power_t, &
            fallible_power_t, &
            power_unit_t, &
            fallible_power_unit_t, &
            power_simple_unit_t, &
            operator(.unit.), &
            parse_power, &
            parse_power_unit, &
            sum, &
            abs, &
            DEFAULT_OUTPUT_UNITS, &
            PROVIDED_UNITS, &
            BTU_PER_HOUR, &
            CALORIES_PER_SECOND, &
            MEGABTU_PER_HOUR, &
            MEGAWATTS, &
            WATTS

    type :: power_t
        double precision :: watts
    contains
        private
        procedure :: to_units
        generic, public :: operator(.in.) => to_units
        procedure, pass(power) :: double_times_power
        procedure, pass(power) :: integer_times_power
        procedure :: power_times_double
        procedure :: power_times_integer
        generic, public :: operator(*) => &
                double_times_power, &
                integer_times_power, &
                power_times_double, &
                power_times_integer
        procedure :: power_divided_by_double
        procedure :: power_divided_by_integer
        procedure :: power_divided_by_power
        generic, public :: operator(/) => &
                power_divided_by_double, &
                power_divided_by_integer, &
                power_divided_by_power
        procedure :: power_plus_power
        generic, public :: operator(+) => power_plus_power
        procedure :: negate_power
        procedure :: power_minus_power
        generic, public :: operator(-) => &
                negate_power, &
                power_minus_power
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

    type :: fallible_power_t
        private
        type(power_t) :: power_ = power_t(0.0d0)
        type(error_list_t) :: errors_
    contains
        private
        procedure, public :: failed => fallible_power_failed
        procedure, public :: power => fallible_power_power
        procedure, public :: errors => fallible_power_errors
    end type

    type, abstract :: power_unit_t
        double precision :: conversion_factor
    contains
        procedure(unit_to_string_i), deferred :: unit_to_string
        procedure(value_to_string_i), deferred :: value_to_string
        generic :: to_string => unit_to_string, value_to_string
        procedure(parse_as_i), deferred :: parse_as
    end type

    type :: fallible_power_unit_t
        private
        class(power_unit_t), allocatable :: unit_
        type(error_list_t) :: errors_
    contains
        private
        procedure, public :: failed => fallible_power_unit_failed
        procedure, public :: unit => fallible_power_unit_unit
        procedure, public :: errors => fallible_power_unit_errors
    end type

    type, extends(power_unit_t) :: power_simple_unit_t
        character(len=20) :: symbol
    contains
        procedure :: unit_to_string => simple_unit_to_string
        procedure :: value_to_string => simple_value_to_string
        procedure :: parse_as => simple_parse_as
    end type

    abstract interface
        elemental function unit_to_string_i(self) result(string)
            import :: power_unit_t, varying_string

            implicit none

            class(power_unit_t), intent(in) :: self
            type(varying_string) :: string
        end function

        pure function value_to_string_i(self, value_) result(string)
            import :: power_unit_t, varying_string

            implicit none

            class(power_unit_t), intent(in) :: self
            type(varying_string), intent(in) :: value_
            type(varying_string) :: string
        end function

        function parse_as_i(self, string) result(fallible_power)
            import :: power_unit_t, fallible_power_t, varying_string

            implicit none

            class(power_unit_t), intent(in) :: self
            type(varying_string), intent(in) :: string
            type(fallible_power_t) :: fallible_power
        end function
    end interface

    interface operator(.unit.)
        module procedure from_units
    end interface

    interface fallible_power_t
        module procedure fallible_power_from_power
        module procedure fallible_power_from_errors
        module procedure fallible_power_from_fallible_power
    end interface

    interface fallible_power_unit_t
        module procedure fallible_power_unit_from_unit
        module procedure fallible_power_unit_from_errors
        module procedure fallible_power_unit_from_fallible_power_unit
    end interface

    interface parse_power
        module procedure parse_power_c
        module procedure parse_power_s
        module procedure parse_power_with_units_c
        module procedure parse_power_with_units_s
    end interface

    interface parse_power_unit
        module procedure parse_power_unit_c
        module procedure parse_power_unit_s
        module procedure parse_power_unit_with_units_c
        module procedure parse_power_unit_with_units_s
    end interface

    interface abs
        module procedure abs_power
    end interface

    interface sum
        module procedure sum_power
    end interface

    type(power_simple_unit_t), parameter :: BTU_PER_HOUR = &
            power_simple_unit_t( &
                    conversion_factor = BTU_PER_HOUR_PER_WATT, &
                    symbol = "BTU/h")
    type(power_simple_unit_t), parameter :: CALORIES_PER_SECOND = &
            power_simple_unit_t( &
                    conversion_factor = CALORIES_PER_SECOND_PER_WATT, &
                    symbol = "cal/s")
    type(power_simple_unit_t), parameter :: MEGABTU_PER_HOUR = &
            power_simple_unit_t( &
                    conversion_factor = MEGABTU_PER_HOUR_PER_WATT, &
                    symbol = "MBTU/h")
    type(power_simple_unit_t), parameter :: MEGAWATTS = &
            power_simple_unit_t( &
                    conversion_factor = MEGAWATTS_PER_WATT, &
                    symbol = "MW")
    type(power_simple_unit_t), parameter :: WATTS = &
            power_simple_unit_t( &
                    conversion_factor = 1.0d0, &
                    symbol = "W")

    type(power_simple_unit_t) :: DEFAULT_OUTPUT_UNITS = WATTS

    type(power_simple_unit_t), parameter :: PROVIDED_UNITS(*) = &
            [ BTU_PER_HOUR &
            , CALORIES_PER_SECOND &
            , MEGABTU_PER_HOUR &
            , MEGAWATTS &
            , WATTS &
            ]

    character(len=*), parameter :: MODULE_NAME = "quaff_power_m"
contains
    function parse_power_c(string) result(fallible_power)
        character(len=*), intent(in) :: string
        type(fallible_power_t) :: fallible_power

        fallible_power = fallible_power_t( &
                parse_power(var_str(string), PROVIDED_UNITS), &
                module_t(MODULE_NAME), &
                procedure_t("parse_power_c"))
    end function

    function parse_power_s(string) result(fallible_power)
        type(varying_string), intent(in) :: string
        type(fallible_power_t) :: fallible_power

        fallible_power = fallible_power_t( &
                parse_power(string, PROVIDED_UNITS), &
                module_t(MODULE_NAME), &
                procedure_t("parse_power_s"))
    end function

    function parse_power_with_units_c( &
            string, units) result(fallible_power)
        character(len=*), intent(in) :: string
        class(power_unit_t), intent(in) :: units(:)
        type(fallible_power_t) :: fallible_power

        fallible_power = fallible_power_t( &
                parse_power(var_str(string), units), &
                module_t(MODULE_NAME), &
                procedure_t("parse_power_with_units_c"))
    end function

    function parse_power_with_units_s( &
            string, units) result(fallible_power)
        type(varying_string), intent(in) :: string
        class(power_unit_t), intent(in) :: units(:)
        type(fallible_power_t) :: fallible_power

        integer :: i

        do i = 1, size(units)
            fallible_power = units(i)%parse_as(string)
            if (.not. fallible_power%failed()) return
        end do
        fallible_power = fallible_power_t(error_list_t(fatal_t( &
                PARSE_ERROR, &
                module_t(MODULE_NAME), &
                procedure_t("parse_power_with_units_s"), &
                "Unable to parse '" // string // "' as a power_t. Tried with units: " &
                // join(units%to_string(), ", "))))
    end function

    elemental function from_units(value_, units) result(power)
        double precision, intent(in) :: value_
        class(power_unit_t), intent(in) :: units
        type(power_t) :: power

        power%watts = value_ / units%conversion_factor
    end function

    elemental function to_units(self, units) result(power)
        class(power_t), intent(in) :: self
        class(power_unit_t), intent(in) :: units
        double precision :: power

        power = self%watts * units%conversion_factor
    end function

    elemental function double_times_power( &
            multiplier, power) result(new_power)
        double precision, intent(in) :: multiplier
        class(power_t), intent(in) :: power
        type(power_t) :: new_power

        new_power%watts = &
                multiplier * power%watts
    end function

    elemental function integer_times_power( &
            multiplier, power) result(new_power)
        integer, intent(in) :: multiplier
        class(power_t), intent(in) :: power
        type(power_t) :: new_power

        new_power%watts = &
                dble(multiplier) * power%watts
    end function

    elemental function power_times_double( &
            power, multiplier) result(new_power)
        class(power_t), intent(in) :: power
        double precision, intent(in) :: multiplier
        type(power_t) :: new_power

        new_power%watts = &
                power%watts * multiplier
    end function

    elemental function power_times_integer( &
            power, multiplier) result(new_power)
        class(power_t), intent(in) :: power
        integer, intent(in) :: multiplier
        type(power_t) :: new_power

        new_power%watts = &
                power%watts * dble(multiplier)
    end function

    elemental function power_divided_by_double( &
            power, divisor) result(new_power)
        class(power_t), intent(in) :: power
        double precision, intent(in) :: divisor
        type(power_t) :: new_power

        new_power%watts = &
                power%watts / divisor
    end function

    elemental function power_divided_by_integer( &
            power, divisor) result(new_power)
        class(power_t), intent(in) :: power
        integer, intent(in) :: divisor
        type(power_t) :: new_power

        new_power%watts = &
                power%watts / dble(divisor)
    end function

    elemental function power_divided_by_power( &
            numerator, denomenator) result(ratio)
        class(power_t), intent(in) :: numerator
        type(power_t), intent(in) :: denomenator
        double precision :: ratio

        ratio = numerator%watts / denomenator%watts
    end function

    elemental function power_plus_power( &
            lhs, rhs) result(sum_)
        class(power_t), intent(in) :: lhs
        type(power_t), intent(in) :: rhs
        type(power_t) :: sum_

        sum_%watts = lhs%watts + rhs%watts
    end function

    elemental function negate_power(self) result(negated)
        class(power_t), intent(in) :: self
        type(power_t) :: negated

        negated%watts = -self%watts
    end function

    elemental function power_minus_power( &
            lhs, rhs) result(difference)
        class(power_t), intent(in) :: lhs
        type(power_t), intent(in) :: rhs
        type(power_t) :: difference

        difference%watts = lhs%watts - rhs%watts
    end function

    pure function abs_power(power) result(abs_)
        type(power_t), intent(in) :: power
        type(power_t) :: abs_

        abs_%watts = abs(power%watts)
    end function

    pure function sum_power(powers) result(sum_)
        type(power_t), intent(in) :: powers(:)
        type(power_t) :: sum_

        sum_%watts = sum(powers%watts)
    end function

    elemental function greater_than(lhs, rhs)
        class(power_t), intent(in) :: lhs
        type(power_t), intent(in) :: rhs
        logical :: greater_than

        greater_than = lhs%watts > rhs%watts
    end function

    elemental function less_than(lhs, rhs)
        class(power_t), intent(in) :: lhs
        type(power_t), intent(in) :: rhs
        logical :: less_than

        less_than = lhs%watts < rhs%watts
    end function

    elemental function greater_than_or_equal(lhs, rhs)
        class(power_t), intent(in) :: lhs
        type(power_t), intent(in) :: rhs
        logical :: greater_than_or_equal

        greater_than_or_equal = lhs%watts >= rhs%watts
    end function

    elemental function less_than_or_equal(lhs, rhs)
        class(power_t), intent(in) :: lhs
        type(power_t), intent(in) :: rhs
        logical :: less_than_or_equal

        less_than_or_equal = lhs%watts <= rhs%watts
    end function

    elemental function equal_(lhs, rhs)
        class(power_t), intent(in) :: lhs
        type(power_t), intent(in) :: rhs
        logical :: equal_

        equal_ = lhs%watts .safeEq. rhs%watts
    end function

    elemental function equal_within_absolute_(lhs, rhs, within)
        class(power_t), intent(in) :: lhs
        type(power_t), intent(in) :: rhs
        type(power_t), intent(in) :: within
        logical :: equal_within_absolute_

        equal_within_absolute_ = equal_within_absolute( &
                lhs%watts, rhs%watts, within%watts)
    end function

    elemental function equal_within_relative_(lhs, rhs, within)
        class(power_t), intent(in) :: lhs
        type(power_t), intent(in) :: rhs
        double precision, intent(in) :: within
        logical :: equal_within_relative_

        equal_within_relative_ = equal_within_relative( &
                lhs%watts, rhs%watts, within)
    end function

    elemental function not_equal(lhs, rhs)
        class(power_t), intent(in) :: lhs
        type(power_t), intent(in) :: rhs
        logical :: not_equal

        not_equal = .not. lhs == rhs
    end function

    elemental function to_string_full_precision(self) result(string)
        class(power_t), intent(in) :: self
        type(varying_string) :: string

        string = self%to_string_in(DEFAULT_OUTPUT_UNITS)
    end function

    elemental function to_string_with_precision(self, significant_digits) result(string)
        class(power_t), intent(in) :: self
        integer, intent(in) :: significant_digits
        type(varying_string) :: string

        string = self%to_string_in(DEFAULT_OUTPUT_UNITS, significant_digits)
    end function

    elemental function to_string_in_full_precision(self, units) result(string)
        class(power_t), intent(in) :: self
        class(power_unit_t), intent(in) :: units
        type(varying_string) :: string

        string = units%to_string(to_string(self.in.units))
    end function

    elemental function to_string_in_with_precision( &
            self, units, significant_digits) result(string)
        class(power_t), intent(in) :: self
        class(power_unit_t), intent(in) :: units
        integer, intent(in) :: significant_digits
        type(varying_string) :: string

        string = units%to_string(to_string(self.in.units, significant_digits))
    end function

    function fallible_power_from_power( &
            power) result(fallible_power)
        type(power_t), intent(in) :: power
        type(fallible_power_t) :: fallible_power

        fallible_power%power_ = power
    end function

    function fallible_power_from_errors( &
            errors) result(fallible_power)
        type(error_list_t), intent(in) :: errors
        type(fallible_power_t) :: fallible_power

        fallible_power%errors_ = errors
    end function

    function fallible_power_from_fallible_power( &
            fallible_power, &
            module_, &
            procedure_) &
            result(new_fallible_power)
        type(fallible_power_t), intent(in) :: fallible_power
        type(module_t), intent(in) :: module_
        type(procedure_t), intent(in) :: procedure_
        type(fallible_power_t) :: new_fallible_power

        if (fallible_power%failed()) then
            new_fallible_power%errors_ = error_list_t( &
                    fallible_power%errors_, module_, procedure_)
        else
            new_fallible_power%power_ = fallible_power%power_
        end if
    end function

    elemental function fallible_power_failed( &
            self) result(failed)
        class(fallible_power_t), intent(in) :: self
        logical :: failed

        failed = self%errors_%has_any()
    end function

    elemental function fallible_power_power( &
            self) result(power)
        class(fallible_power_t), intent(in) :: self
        type(power_t) :: power

        power = self%power_
    end function

    impure elemental function fallible_power_errors( &
            self) result(errors)
        class(fallible_power_t), intent(in) :: self
        type(error_list_t) :: errors

        errors = self%errors_
    end function

    function fallible_power_unit_from_unit( &
            unit) result(fallible_power_unit)
        class(power_unit_t), intent(in) :: unit
        type(fallible_power_unit_t) :: fallible_power_unit

        allocate(fallible_power_unit%unit_, source = unit)
    end function

    function fallible_power_unit_from_errors( &
            errors) result(fallible_power_unit)
        type(error_list_t), intent(in) :: errors
        type(fallible_power_unit_t) :: fallible_power_unit

        fallible_power_unit%errors_ = errors
    end function

    function fallible_power_unit_from_fallible_power_unit( &
            fallible_power_unit, &
            module_, &
            procedure_) &
            result(new_fallible_power_unit)
        type(fallible_power_unit_t), intent(in) :: fallible_power_unit
        type(module_t), intent(in) :: module_
        type(procedure_t), intent(in) :: procedure_
        type(fallible_power_unit_t) :: new_fallible_power_unit

        if (fallible_power_unit%failed()) then
            new_fallible_power_unit%errors_ = error_list_t( &
                    fallible_power_unit%errors_, module_, procedure_)
        else
            allocate(new_fallible_power_unit%unit_, source = &
                    fallible_power_unit%unit_)
        end if
    end function

    elemental function fallible_power_unit_failed( &
            self) result(failed)
        class(fallible_power_unit_t), intent(in) :: self
        logical :: failed

        failed = self%errors_%has_any()
    end function

    function fallible_power_unit_unit( &
            self) result(unit)
        class(fallible_power_unit_t), intent(in) :: self
        class(power_unit_t), allocatable :: unit

        allocate(unit, source = self%unit_)
    end function

    impure elemental function fallible_power_unit_errors( &
            self) result(errors)
        class(fallible_power_unit_t), intent(in) :: self
        type(error_list_t) :: errors

        errors = self%errors_
    end function

    elemental function simple_unit_to_string(self) result(string)
        class(power_simple_unit_t), intent(in) :: self
        type(varying_string) :: string

        string = trim(self%symbol)
    end function

    pure function simple_value_to_string(self, value_) result(string)
        class(power_simple_unit_t), intent(in) :: self
        type(varying_string), intent(in) :: value_
        type(varying_string) :: string

        string = value_ // " " // self%to_string()
    end function

    function simple_parse_as(self, string) result(fallible_power)
        class(power_simple_unit_t), intent(in) :: self
        type(varying_string), intent(in) :: string
        type(fallible_power_t) :: fallible_power

        type(parse_result_t) :: parse_result

        parse_result = parse_with(the_parser, string)
        if (parse_result%ok) then
            select type (the_number => parse_result%parsed)
            type is (parsed_rational_t)
                fallible_power = fallible_power_t(the_number%value_.unit.self)
            end select
        else
            fallible_power = fallible_power_t(error_list_t(fatal_t( &
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

    function parse_power_unit_c(string) result(fallible_power_unit)
        character(len=*), intent(in) :: string
        type(fallible_power_unit_t) :: fallible_power_unit

        fallible_power_unit = fallible_power_unit_t( &
                parse_power_unit(var_str(string), PROVIDED_UNITS), &
                module_t(MODULE_NAME), &
                procedure_t("parse_power_unit_c"))
    end function

    function parse_power_unit_s(string) result(fallible_power_unit)
        type(varying_string), intent(in) :: string
        type(fallible_power_unit_t) :: fallible_power_unit

        fallible_power_unit = fallible_power_unit_t( &
                parse_power_unit(string, PROVIDED_UNITS), &
                module_t(MODULE_NAME), &
                procedure_t("parse_power_unit_s"))
    end function

    function parse_power_unit_with_units_c( &
            string, units) result(fallible_power_unit)
        character(len=*), intent(in) :: string
        class(power_unit_t), intent(in) :: units(:)
        type(fallible_power_unit_t) :: fallible_power_unit

        fallible_power_unit = fallible_power_unit_t( &
                parse_power_unit(var_str(string), units), &
                module_t(MODULE_NAME), &
                procedure_t("parse_power_unit_with_units_c"))
    end function

    function parse_power_unit_with_units_s( &
            string, units) result(fallible_power_unit)
        type(varying_string), intent(in) :: string
        class(power_unit_t), intent(in) :: units(:)
        type(fallible_power_unit_t) :: fallible_power_unit

        integer :: i
        type(varying_string), allocatable :: unit_strings(:)

        do i = 1, size(units)
            if (string == units(i)%to_string()) then
                fallible_power_unit = fallible_power_unit_t(units(i))
                return
            end if
        end do
        allocate(unit_strings(size(units)))
        do i = 1, size(units)
            unit_strings(i) = units(i)%to_string()
        end do
        fallible_power_unit = fallible_power_unit_t(error_list_t(fatal_t( &
                UNKNOWN_UNIT, &
                module_t(MODULE_NAME), &
                procedure_t("parse_power_unit_with_units_s"), &
                '"' // string // '", known units: [' // join(unit_strings, ', ') // ']')))
    end function
end module
