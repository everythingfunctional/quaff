module quaff_amount_temperature_m
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
    use quaff_conversion_factors_m, only: KILO_PER_BASE
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
            amount_temperature_t, &
            fallible_amount_temperature_t, &
            amount_temperature_unit_t, &
            fallible_amount_temperature_unit_t, &
            amount_temperature_simple_unit_t, &
            operator(.unit.), &
            parse_amount_temperature, &
            parse_amount_temperature_unit, &
            sum, &
            abs, &
            DEFAULT_OUTPUT_UNITS, &
            PROVIDED_UNITS, &
            KILOMOLS_KELVIN, &
            MOLS_KELVIN

    type :: amount_temperature_t
        double precision :: mols_kelvin
    contains
        private
        procedure :: to_units
        generic, public :: operator(.in.) => to_units
        procedure, pass(amount_temperature) :: double_times_amount_temperature
        procedure, pass(amount_temperature) :: integer_times_amount_temperature
        procedure :: amount_temperature_times_double
        procedure :: amount_temperature_times_integer
        generic, public :: operator(*) => &
                double_times_amount_temperature, &
                integer_times_amount_temperature, &
                amount_temperature_times_double, &
                amount_temperature_times_integer
        procedure :: amount_temperature_divided_by_double
        procedure :: amount_temperature_divided_by_integer
        procedure :: amount_temperature_divided_by_amount_temperature
        generic, public :: operator(/) => &
                amount_temperature_divided_by_double, &
                amount_temperature_divided_by_integer, &
                amount_temperature_divided_by_amount_temperature
        procedure :: amount_temperature_plus_amount_temperature
        generic, public :: operator(+) => amount_temperature_plus_amount_temperature
        procedure :: negate_amount_temperature
        procedure :: amount_temperature_minus_amount_temperature
        generic, public :: operator(-) => &
                negate_amount_temperature, &
                amount_temperature_minus_amount_temperature
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

    type :: fallible_amount_temperature_t
        private
        type(amount_temperature_t) :: amount_temperature_ = amount_temperature_t(0.0d0)
        type(error_list_t) :: errors_
    contains
        private
        procedure, public :: failed => fallible_amount_temperature_failed
        procedure, public :: amount_temperature => fallible_amount_temperature_amount_temperature
        procedure, public :: errors => fallible_amount_temperature_errors
    end type

    type, abstract :: amount_temperature_unit_t
        double precision :: conversion_factor
    contains
        procedure(unit_to_string_i), deferred :: unit_to_string
        procedure(value_to_string_i), deferred :: value_to_string
        generic :: to_string => unit_to_string, value_to_string
        procedure(parse_as_i), deferred :: parse_as
    end type

    type :: fallible_amount_temperature_unit_t
        private
        class(amount_temperature_unit_t), allocatable :: unit_
        type(error_list_t) :: errors_
    contains
        private
        procedure, public :: failed => fallible_amount_temperature_unit_failed
        procedure, public :: unit => fallible_amount_temperature_unit_unit
        procedure, public :: errors => fallible_amount_temperature_unit_errors
    end type

    type, extends(amount_temperature_unit_t) :: amount_temperature_simple_unit_t
        character(len=20) :: symbol
    contains
        procedure :: unit_to_string => simple_unit_to_string
        procedure :: value_to_string => simple_value_to_string
        procedure :: parse_as => simple_parse_as
    end type

    abstract interface
        elemental function unit_to_string_i(self) result(string)
            import :: amount_temperature_unit_t, varying_string

            implicit none

            class(amount_temperature_unit_t), intent(in) :: self
            type(varying_string) :: string
        end function

        pure function value_to_string_i(self, value_) result(string)
            import :: amount_temperature_unit_t, varying_string

            implicit none

            class(amount_temperature_unit_t), intent(in) :: self
            type(varying_string), intent(in) :: value_
            type(varying_string) :: string
        end function

        function parse_as_i(self, string) result(fallible_amount_temperature)
            import :: amount_temperature_unit_t, fallible_amount_temperature_t, varying_string

            implicit none

            class(amount_temperature_unit_t), intent(in) :: self
            type(varying_string), intent(in) :: string
            type(fallible_amount_temperature_t) :: fallible_amount_temperature
        end function
    end interface

    interface operator(.unit.)
        module procedure from_units
    end interface

    interface fallible_amount_temperature_t
        module procedure fallible_amount_temperature_from_amount_temperature
        module procedure fallible_amount_temperature_from_errors
        module procedure fallible_amount_temperature_from_fallible_amount_temperature
    end interface

    interface fallible_amount_temperature_unit_t
        module procedure fallible_amount_temperature_unit_from_unit
        module procedure fallible_amount_temperature_unit_from_errors
        module procedure fallible_amt_temp_unit_from_fallible_amt_temp_unit
    end interface

    interface parse_amount_temperature
        module procedure parse_amount_temperature_c
        module procedure parse_amount_temperature_s
        module procedure parse_amount_temperature_with_units_c
        module procedure parse_amount_temperature_with_units_s
    end interface

    interface parse_amount_temperature_unit
        module procedure parse_amount_temperature_unit_c
        module procedure parse_amount_temperature_unit_s
        module procedure parse_amount_temperature_unit_with_units_c
        module procedure parse_amount_temperature_unit_with_units_s
    end interface

    interface abs
        module procedure abs_amount_temperature
    end interface

    interface sum
        module procedure sum_amount_temperature
    end interface

    type(amount_temperature_simple_unit_t), parameter :: KILOMOLS_KELVIN = &
            amount_temperature_simple_unit_t( &
                    conversion_factor = KILO_PER_BASE, &
                    symbol = "kmol K")
    type(amount_temperature_simple_unit_t), parameter :: MOLS_KELVIN = &
            amount_temperature_simple_unit_t( &
                    conversion_factor = 1.0d0, &
                    symbol = "mol K")

    type(amount_temperature_simple_unit_t) :: DEFAULT_OUTPUT_UNITS = MOLS_KELVIN

    type(amount_temperature_simple_unit_t), parameter :: PROVIDED_UNITS(*) = &
            [KILOMOLS_KELVIN, MOLS_KELVIN]

    character(len=*), parameter :: MODULE_NAME = "quaff_amount_temperature_m"
contains
    function parse_amount_temperature_c(string) result(fallible_amount_temperature)
        character(len=*), intent(in) :: string
        type(fallible_amount_temperature_t) :: fallible_amount_temperature

        fallible_amount_temperature = fallible_amount_temperature_t( &
                parse_amount_temperature(var_str(string), PROVIDED_UNITS), &
                module_t(MODULE_NAME), &
                procedure_t("parse_amount_temperature_c"))
    end function

    function parse_amount_temperature_s(string) result(fallible_amount_temperature)
        type(varying_string), intent(in) :: string
        type(fallible_amount_temperature_t) :: fallible_amount_temperature

        fallible_amount_temperature = fallible_amount_temperature_t( &
                parse_amount_temperature(string, PROVIDED_UNITS), &
                module_t(MODULE_NAME), &
                procedure_t("parse_amount_temperature_s"))
    end function

    function parse_amount_temperature_with_units_c( &
            string, units) result(fallible_amount_temperature)
        character(len=*), intent(in) :: string
        class(amount_temperature_unit_t), intent(in) :: units(:)
        type(fallible_amount_temperature_t) :: fallible_amount_temperature

        fallible_amount_temperature = fallible_amount_temperature_t( &
                parse_amount_temperature(var_str(string), units), &
                module_t(MODULE_NAME), &
                procedure_t("parse_amount_temperature_with_units_c"))
    end function

    function parse_amount_temperature_with_units_s( &
            string, units) result(fallible_amount_temperature)
        type(varying_string), intent(in) :: string
        class(amount_temperature_unit_t), intent(in) :: units(:)
        type(fallible_amount_temperature_t) :: fallible_amount_temperature

        integer :: i

        do i = 1, size(units)
            fallible_amount_temperature = units(i)%parse_as(string)
            if (.not. fallible_amount_temperature%failed()) return
        end do
        fallible_amount_temperature = fallible_amount_temperature_t(error_list_t(fatal_t( &
                PARSE_ERROR, &
                module_t(MODULE_NAME), &
                procedure_t("parse_amount_temperature_with_units_s"), &
                "Unable to parse '" // string // "' as a amount_temperature_t. Tried with units: " &
                // join(units%to_string(), ", "))))
    end function

    elemental function from_units(value_, units) result(amount_temperature)
        double precision, intent(in) :: value_
        class(amount_temperature_unit_t), intent(in) :: units
        type(amount_temperature_t) :: amount_temperature

        amount_temperature%mols_kelvin = value_ / units%conversion_factor
    end function

    elemental function to_units(self, units) result(amount_temperature)
        class(amount_temperature_t), intent(in) :: self
        class(amount_temperature_unit_t), intent(in) :: units
        double precision :: amount_temperature

        amount_temperature = self%mols_kelvin * units%conversion_factor
    end function

    elemental function double_times_amount_temperature( &
            multiplier, amount_temperature) result(new_amount_temperature)
        double precision, intent(in) :: multiplier
        class(amount_temperature_t), intent(in) :: amount_temperature
        type(amount_temperature_t) :: new_amount_temperature

        new_amount_temperature%mols_kelvin = &
                multiplier * amount_temperature%mols_kelvin
    end function

    elemental function integer_times_amount_temperature( &
            multiplier, amount_temperature) result(new_amount_temperature)
        integer, intent(in) :: multiplier
        class(amount_temperature_t), intent(in) :: amount_temperature
        type(amount_temperature_t) :: new_amount_temperature

        new_amount_temperature%mols_kelvin = &
                dble(multiplier) * amount_temperature%mols_kelvin
    end function

    elemental function amount_temperature_times_double( &
            amount_temperature, multiplier) result(new_amount_temperature)
        class(amount_temperature_t), intent(in) :: amount_temperature
        double precision, intent(in) :: multiplier
        type(amount_temperature_t) :: new_amount_temperature

        new_amount_temperature%mols_kelvin = &
                amount_temperature%mols_kelvin * multiplier
    end function

    elemental function amount_temperature_times_integer( &
            amount_temperature, multiplier) result(new_amount_temperature)
        class(amount_temperature_t), intent(in) :: amount_temperature
        integer, intent(in) :: multiplier
        type(amount_temperature_t) :: new_amount_temperature

        new_amount_temperature%mols_kelvin = &
                amount_temperature%mols_kelvin * dble(multiplier)
    end function

    elemental function amount_temperature_divided_by_double( &
            amount_temperature, divisor) result(new_amount_temperature)
        class(amount_temperature_t), intent(in) :: amount_temperature
        double precision, intent(in) :: divisor
        type(amount_temperature_t) :: new_amount_temperature

        new_amount_temperature%mols_kelvin = &
                amount_temperature%mols_kelvin / divisor
    end function

    elemental function amount_temperature_divided_by_integer( &
            amount_temperature, divisor) result(new_amount_temperature)
        class(amount_temperature_t), intent(in) :: amount_temperature
        integer, intent(in) :: divisor
        type(amount_temperature_t) :: new_amount_temperature

        new_amount_temperature%mols_kelvin = &
                amount_temperature%mols_kelvin / dble(divisor)
    end function

    elemental function amount_temperature_divided_by_amount_temperature( &
            numerator, denomenator) result(ratio)
        class(amount_temperature_t), intent(in) :: numerator
        type(amount_temperature_t), intent(in) :: denomenator
        double precision :: ratio

        ratio = numerator%mols_kelvin / denomenator%mols_kelvin
    end function

    elemental function amount_temperature_plus_amount_temperature( &
            lhs, rhs) result(sum_)
        class(amount_temperature_t), intent(in) :: lhs
        type(amount_temperature_t), intent(in) :: rhs
        type(amount_temperature_t) :: sum_

        sum_%mols_kelvin = lhs%mols_kelvin + rhs%mols_kelvin
    end function

    elemental function negate_amount_temperature(self) result(negated)
        class(amount_temperature_t), intent(in) :: self
        type(amount_temperature_t) :: negated

        negated%mols_kelvin = -self%mols_kelvin
    end function

    elemental function amount_temperature_minus_amount_temperature( &
            lhs, rhs) result(difference)
        class(amount_temperature_t), intent(in) :: lhs
        type(amount_temperature_t), intent(in) :: rhs
        type(amount_temperature_t) :: difference

        difference%mols_kelvin = lhs%mols_kelvin - rhs%mols_kelvin
    end function

    elemental function abs_amount_temperature(amount_temperature) result(abs_)
    class(amount_temperature_t), intent(in) :: amount_temperature
    type(amount_temperature_t) :: abs_

      abs_%mols_kelvin = abs(amount_temperature%mols_kelvin)
    end function

    pure function sum_amount_temperature(amount_temperatures) result(sum_)
        type(amount_temperature_t), intent(in) :: amount_temperatures(:)
        type(amount_temperature_t) :: sum_

        sum_%mols_kelvin = sum(amount_temperatures%mols_kelvin)
    end function

    elemental function greater_than(lhs, rhs)
        class(amount_temperature_t), intent(in) :: lhs
        type(amount_temperature_t), intent(in) :: rhs
        logical :: greater_than

        greater_than = lhs%mols_kelvin > rhs%mols_kelvin
    end function

    elemental function less_than(lhs, rhs)
        class(amount_temperature_t), intent(in) :: lhs
        type(amount_temperature_t), intent(in) :: rhs
        logical :: less_than

        less_than = lhs%mols_kelvin < rhs%mols_kelvin
    end function

    elemental function greater_than_or_equal(lhs, rhs)
        class(amount_temperature_t), intent(in) :: lhs
        type(amount_temperature_t), intent(in) :: rhs
        logical :: greater_than_or_equal

        greater_than_or_equal = lhs%mols_kelvin >= rhs%mols_kelvin
    end function

    elemental function less_than_or_equal(lhs, rhs)
        class(amount_temperature_t), intent(in) :: lhs
        type(amount_temperature_t), intent(in) :: rhs
        logical :: less_than_or_equal

        less_than_or_equal = lhs%mols_kelvin <= rhs%mols_kelvin
    end function

    elemental function equal_(lhs, rhs)
        class(amount_temperature_t), intent(in) :: lhs
        type(amount_temperature_t), intent(in) :: rhs
        logical :: equal_

        equal_ = lhs%mols_kelvin .safeEq. rhs%mols_kelvin
    end function

    elemental function equal_within_absolute_(lhs, rhs, within)
        class(amount_temperature_t), intent(in) :: lhs
        type(amount_temperature_t), intent(in) :: rhs
        type(amount_temperature_t), intent(in) :: within
        logical :: equal_within_absolute_

        equal_within_absolute_ = equal_within_absolute( &
                lhs%mols_kelvin, rhs%mols_kelvin, within%mols_kelvin)
    end function

    elemental function equal_within_relative_(lhs, rhs, within)
        class(amount_temperature_t), intent(in) :: lhs
        type(amount_temperature_t), intent(in) :: rhs
        double precision, intent(in) :: within
        logical :: equal_within_relative_

        equal_within_relative_ = equal_within_relative( &
                lhs%mols_kelvin, rhs%mols_kelvin, within)
    end function

    elemental function not_equal(lhs, rhs)
        class(amount_temperature_t), intent(in) :: lhs
        type(amount_temperature_t), intent(in) :: rhs
        logical :: not_equal

        not_equal = .not. lhs == rhs
    end function

    elemental function to_string_full_precision(self) result(string)
        class(amount_temperature_t), intent(in) :: self
        type(varying_string) :: string

        string = self%to_string_in(DEFAULT_OUTPUT_UNITS)
    end function

    elemental function to_string_with_precision(self, significant_digits) result(string)
        class(amount_temperature_t), intent(in) :: self
        integer, intent(in) :: significant_digits
        type(varying_string) :: string

        string = self%to_string_in(DEFAULT_OUTPUT_UNITS, significant_digits)
    end function

    elemental function to_string_in_full_precision(self, units) result(string)
        class(amount_temperature_t), intent(in) :: self
        class(amount_temperature_unit_t), intent(in) :: units
        type(varying_string) :: string

        string = units%to_string(to_string(self.in.units))
    end function

    elemental function to_string_in_with_precision( &
            self, units, significant_digits) result(string)
        class(amount_temperature_t), intent(in) :: self
        class(amount_temperature_unit_t), intent(in) :: units
        integer, intent(in) :: significant_digits
        type(varying_string) :: string

        string = units%to_string(to_string(self.in.units, significant_digits))
    end function

    function fallible_amount_temperature_from_amount_temperature( &
            amount_temperature) result(fallible_amount_temperature)
        type(amount_temperature_t), intent(in) :: amount_temperature
        type(fallible_amount_temperature_t) :: fallible_amount_temperature

        fallible_amount_temperature%amount_temperature_ = amount_temperature
    end function

    function fallible_amount_temperature_from_errors( &
            errors) result(fallible_amount_temperature)
        type(error_list_t), intent(in) :: errors
        type(fallible_amount_temperature_t) :: fallible_amount_temperature

        fallible_amount_temperature%errors_ = errors
    end function

    function fallible_amount_temperature_from_fallible_amount_temperature( &
            fallible_amount_temperature, &
            module_, &
            procedure_) &
            result(new_fallible_amount_temperature)
        type(fallible_amount_temperature_t), intent(in) :: fallible_amount_temperature
        type(module_t), intent(in) :: module_
        type(procedure_t), intent(in) :: procedure_
        type(fallible_amount_temperature_t) :: new_fallible_amount_temperature

        if (fallible_amount_temperature%failed()) then
            new_fallible_amount_temperature%errors_ = error_list_t( &
                    fallible_amount_temperature%errors_, module_, procedure_)
        else
            new_fallible_amount_temperature%amount_temperature_ = fallible_amount_temperature%amount_temperature_
        end if
    end function

    elemental function fallible_amount_temperature_failed( &
            self) result(failed)
        class(fallible_amount_temperature_t), intent(in) :: self
        logical :: failed

        failed = self%errors_%has_any()
    end function

    elemental function fallible_amount_temperature_amount_temperature( &
            self) result(amount_temperature)
        class(fallible_amount_temperature_t), intent(in) :: self
        type(amount_temperature_t) :: amount_temperature

        amount_temperature = self%amount_temperature_
    end function

    impure elemental function fallible_amount_temperature_errors( &
            self) result(errors)
        class(fallible_amount_temperature_t), intent(in) :: self
        type(error_list_t) :: errors

        errors = self%errors_
    end function

    function fallible_amount_temperature_unit_from_unit( &
            unit) result(fallible_amount_temperature_unit)
        class(amount_temperature_unit_t), intent(in) :: unit
        type(fallible_amount_temperature_unit_t) :: fallible_amount_temperature_unit

        allocate(fallible_amount_temperature_unit%unit_, source = unit)
    end function

    function fallible_amount_temperature_unit_from_errors( &
            errors) result(fallible_amount_temperature_unit)
        type(error_list_t), intent(in) :: errors
        type(fallible_amount_temperature_unit_t) :: fallible_amount_temperature_unit

        fallible_amount_temperature_unit%errors_ = errors
    end function

    function fallible_amt_temp_unit_from_fallible_amt_temp_unit( &
            fallible_amount_temperature_unit, &
            module_, &
            procedure_) &
            result(new_fallible_amount_temperature_unit)
        type(fallible_amount_temperature_unit_t), intent(in) :: fallible_amount_temperature_unit
        type(module_t), intent(in) :: module_
        type(procedure_t), intent(in) :: procedure_
        type(fallible_amount_temperature_unit_t) :: new_fallible_amount_temperature_unit

        if (fallible_amount_temperature_unit%failed()) then
            new_fallible_amount_temperature_unit%errors_ = error_list_t( &
                    fallible_amount_temperature_unit%errors_, module_, procedure_)
        else
            allocate(new_fallible_amount_temperature_unit%unit_, source = &
                    fallible_amount_temperature_unit%unit_)
        end if
    end function

    elemental function fallible_amount_temperature_unit_failed( &
            self) result(failed)
        class(fallible_amount_temperature_unit_t), intent(in) :: self
        logical :: failed

        failed = self%errors_%has_any()
    end function

    function fallible_amount_temperature_unit_unit( &
            self) result(unit)
        class(fallible_amount_temperature_unit_t), intent(in) :: self
        class(amount_temperature_unit_t), allocatable :: unit

        allocate(unit, source = self%unit_)
    end function

    impure elemental function fallible_amount_temperature_unit_errors( &
            self) result(errors)
        class(fallible_amount_temperature_unit_t), intent(in) :: self
        type(error_list_t) :: errors

        errors = self%errors_
    end function

    elemental function simple_unit_to_string(self) result(string)
        class(amount_temperature_simple_unit_t), intent(in) :: self
        type(varying_string) :: string

        string = trim(self%symbol)
    end function

    pure function simple_value_to_string(self, value_) result(string)
        class(amount_temperature_simple_unit_t), intent(in) :: self
        type(varying_string), intent(in) :: value_
        type(varying_string) :: string

        string = value_ // " " // self%to_string()
    end function

    function simple_parse_as(self, string) result(fallible_amount_temperature)
        class(amount_temperature_simple_unit_t), intent(in) :: self
        type(varying_string), intent(in) :: string
        type(fallible_amount_temperature_t) :: fallible_amount_temperature

        type(parse_result_t) :: parse_result

        parse_result = parse_with(the_parser, string)
        if (parse_result%ok) then
            select type (the_number => parse_result%parsed)
            type is (parsed_rational_t)
                fallible_amount_temperature = fallible_amount_temperature_t(the_number%value_.unit.self)
            end select
        else
            fallible_amount_temperature = fallible_amount_temperature_t(error_list_t(fatal_t( &
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

    function parse_amount_temperature_unit_c(string) result(fallible_amount_temperature_unit)
        character(len=*), intent(in) :: string
        type(fallible_amount_temperature_unit_t) :: fallible_amount_temperature_unit

        fallible_amount_temperature_unit = fallible_amount_temperature_unit_t( &
                parse_amount_temperature_unit(var_str(string), PROVIDED_UNITS), &
                module_t(MODULE_NAME), &
                procedure_t("parse_amount_temperature_unit_c"))
    end function

    function parse_amount_temperature_unit_s(string) result(fallible_amount_temperature_unit)
        type(varying_string), intent(in) :: string
        type(fallible_amount_temperature_unit_t) :: fallible_amount_temperature_unit

        fallible_amount_temperature_unit = fallible_amount_temperature_unit_t( &
                parse_amount_temperature_unit(string, PROVIDED_UNITS), &
                module_t(MODULE_NAME), &
                procedure_t("parse_amount_temperature_unit_s"))
    end function

    function parse_amount_temperature_unit_with_units_c( &
            string, units) result(fallible_amount_temperature_unit)
        character(len=*), intent(in) :: string
        class(amount_temperature_unit_t), intent(in) :: units(:)
        type(fallible_amount_temperature_unit_t) :: fallible_amount_temperature_unit

        fallible_amount_temperature_unit = fallible_amount_temperature_unit_t( &
                parse_amount_temperature_unit(var_str(string), units), &
                module_t(MODULE_NAME), &
                procedure_t("parse_amount_temperature_unit_with_units_c"))
    end function

    function parse_amount_temperature_unit_with_units_s( &
            string, units) result(fallible_amount_temperature_unit)
        type(varying_string), intent(in) :: string
        class(amount_temperature_unit_t), intent(in) :: units(:)
        type(fallible_amount_temperature_unit_t) :: fallible_amount_temperature_unit

        integer :: i
        type(varying_string) :: unit_strings(size(units))

        do i = 1, size(units)
            if (string == units(i)%to_string()) then
                fallible_amount_temperature_unit = fallible_amount_temperature_unit_t(units(i))
                return
            end if
        end do
        do i = 1, size(units)
            unit_strings(i) = units(i)%to_string()
        end do
        fallible_amount_temperature_unit = fallible_amount_temperature_unit_t(error_list_t(fatal_t( &
                UNKNOWN_UNIT, &
                module_t(MODULE_NAME), &
                procedure_t("parse_amount_temperature_unit_with_units_s"), &
                '"' // string // '", known units: [' // join(unit_strings, ', ') // ']')))
    end function
end module
