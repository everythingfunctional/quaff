module temperature_m
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
            CELSIUS_KELVIN_DIFFERENCE, &
            FAHRENHEIT_RANKINE_DIFFERENCE, &
            RANKINE_PER_KELVIN
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
            temperature_t, &
            fallible_temperature_t, &
            temperature_unit_t, &
            fallible_temperature_unit_t, &
            temperature_simple_unit_t, &
            operator(.unit.), &
            parse_temperature, &
            parse_temperature_unit, &
            sum, &
            DEFAULT_OUTPUT_UNITS, &
            PROVIDED_UNITS, &
            CELSIUS, &
            FAHRENHEIT, &
            KELVIN, &
            RANKINE

    type :: temperature_t
        double precision :: kelvin
    contains
        private
        procedure :: to_units
        generic, public :: operator(.in.) => to_units
        procedure, pass(temperature) :: double_times_temperature
        procedure, pass(temperature) :: integer_times_temperature
        procedure :: temperature_times_double
        procedure :: temperature_times_integer
        generic, public :: operator(*) => &
                double_times_temperature, &
                integer_times_temperature, &
                temperature_times_double, &
                temperature_times_integer
        procedure :: temperature_divided_by_double
        procedure :: temperature_divided_by_integer
        procedure :: temperature_divided_by_temperature
        generic, public :: operator(/) => &
                temperature_divided_by_double, &
                temperature_divided_by_integer, &
                temperature_divided_by_temperature
        procedure :: temperature_plus_temperature
        generic, public :: operator(+) => temperature_plus_temperature
        procedure :: temperature_minus_temperature
        generic, public :: operator(-) => temperature_minus_temperature
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

    type :: fallible_temperature_t
        private
        type(temperature_t) :: temperature_
        type(error_list_t) :: errors_
    contains
        private
        procedure, public :: failed => fallible_temperature_failed
        procedure, public :: temperature => fallible_temperature_temperature
        procedure, public :: errors => fallible_temperature_errors
    end type

    type, abstract :: temperature_unit_t
        double precision :: conversion_factor
        double precision :: difference
    contains
        procedure(unit_to_string_i), deferred :: unit_to_string
        procedure(value_to_string_i), deferred :: value_to_string
        generic :: to_string => unit_to_string, value_to_string
        procedure(parse_as_i), deferred :: parse_as
    end type

    type :: fallible_temperature_unit_t
        private
        class(temperature_unit_t), allocatable :: unit_
        type(error_list_t) :: errors_
    contains
        private
        procedure, public :: failed => fallible_temperature_unit_failed
        procedure, public :: unit => fallible_temperature_unit_unit
        procedure, public :: errors => fallible_temperature_unit_errors
    end type

    type, extends(temperature_unit_t) :: temperature_simple_unit_t
        character(len=20) :: symbol
    contains
        procedure :: unit_to_string => simple_unit_to_string
        procedure :: value_to_string => simple_value_to_string
        procedure :: parse_as => simple_parse_as
    end type

    abstract interface
        elemental function unit_to_string_i(self) result(string)
            import :: temperature_unit_t, varying_string

            implicit none

            class(temperature_unit_t), intent(in) :: self
            type(varying_string) :: string
        end function

        pure function value_to_string_i(self, value_) result(string)
            import :: temperature_unit_t, varying_string

            implicit none

            class(temperature_unit_t), intent(in) :: self
            type(varying_string), intent(in) :: value_
            type(varying_string) :: string
        end function

        function parse_as_i(self, string) result(fallible_temperature)
            import :: temperature_unit_t, fallible_temperature_t, varying_string

            implicit none

            class(temperature_unit_t), intent(in) :: self
            type(varying_string), intent(in) :: string
            type(fallible_temperature_t) :: fallible_temperature
        end function
    end interface

    interface operator(.unit.)
        module procedure from_units
    end interface

    interface fallible_temperature_t
        module procedure fallible_temperature_from_temperature
        module procedure fallible_temperature_from_errors
        module procedure fallible_temperature_from_fallible_temperature
    end interface

    interface fallible_temperature_unit_t
        module procedure fallible_temperature_unit_from_unit
        module procedure fallible_temperature_unit_from_errors
        module procedure fallible_temperature_unit_from_fallible_temperature_unit
    end interface

    interface parse_temperature
        module procedure parse_temperature_c
        module procedure parse_temperature_s
        module procedure parse_temperature_with_units_c
        module procedure parse_temperature_with_units_s
    end interface

    interface parse_temperature_unit
        module procedure parse_temperature_unit_c
        module procedure parse_temperature_unit_s
        module procedure parse_temperature_unit_with_units_c
        module procedure parse_temperature_unit_with_units_s
    end interface

    interface sum
        module procedure sum_temperature
    end interface

    type(temperature_simple_unit_t), parameter :: CELSIUS = &
            temperature_simple_unit_t( &
                    conversion_factor = 1.0d0, &
                    difference = CELSIUS_KELVIN_DIFFERENCE, &
                    symbol = "C")
    type(temperature_simple_unit_t), parameter :: FAHRENHEIT = &
            temperature_simple_unit_t( &
                    conversion_factor = RANKINE_PER_KELVIN, &
                    difference = FAHRENHEIT_RANKINE_DIFFERENCE, &
                    symbol = "F")
    type(temperature_simple_unit_t), parameter :: KELVIN = &
            temperature_simple_unit_t( &
                    conversion_factor = 1.0d0, &
                    difference = 0.0d0, &
                    symbol = "K")
    type(temperature_simple_unit_t), parameter :: RANKINE = &
            temperature_simple_unit_t( &
                    conversion_factor = RANKINE_PER_KELVIN, &
                    difference = 0.0d0, &
                    symbol = "R")

    type(temperature_simple_unit_t) :: DEFAULT_OUTPUT_UNITS = KELVIN

    type(temperature_simple_unit_t), parameter :: PROVIDED_UNITS(*) = &
            [CELSIUS, FAHRENHEIT, KELVIN, RANKINE]

    character(len=*), parameter :: MODULE_NAME = "temperature_m"
contains
    function parse_temperature_c(string) result(fallible_temperature)
        character(len=*), intent(in) :: string
        type(fallible_temperature_t) :: fallible_temperature

        fallible_temperature = fallible_temperature_t( &
                parse_temperature(var_str(string), PROVIDED_UNITS), &
                module_t(MODULE_NAME), &
                procedure_t("parse_temperature_c"))
    end function

    function parse_temperature_s(string) result(fallible_temperature)
        type(varying_string), intent(in) :: string
        type(fallible_temperature_t) :: fallible_temperature

        fallible_temperature = fallible_temperature_t( &
                parse_temperature(string, PROVIDED_UNITS), &
                module_t(MODULE_NAME), &
                procedure_t("parse_temperature_s"))
    end function

    function parse_temperature_with_units_c( &
            string, units) result(fallible_temperature)
        character(len=*), intent(in) :: string
        class(temperature_unit_t), intent(in) :: units(:)
        type(fallible_temperature_t) :: fallible_temperature

        fallible_temperature = fallible_temperature_t( &
                parse_temperature(var_str(string), units), &
                module_t(MODULE_NAME), &
                procedure_t("parse_temperature_with_units_c"))
    end function

    function parse_temperature_with_units_s( &
            string, units) result(fallible_temperature)
        type(varying_string), intent(in) :: string
        class(temperature_unit_t), intent(in) :: units(:)
        type(fallible_temperature_t) :: fallible_temperature

        type(fallible_temperature_t) :: all_attempts(size(units))
        integer :: i

        do i = 1, size(units)
            all_attempts(i) = units(i)%parse_as(string)
            if (.not. all_attempts(i)%failed()) then
                fallible_temperature = all_attempts(i)
                return
            end if
        end do
        fallible_temperature = fallible_temperature_t(error_list_t( &
                all_attempts%errors(), &
                module_t(MODULE_NAME), &
                procedure_t("parse_temperature_with_units_s")))
    end function

    elemental function from_units(value_, units) result(temperature)
        double precision, intent(in) :: value_
        class(temperature_unit_t), intent(in) :: units
        type(temperature_t) :: temperature

        temperature%kelvin = (value_ + units%difference) / units%conversion_factor
    end function

    elemental function to_units(self, units) result(temperature)
        class(temperature_t), intent(in) :: self
        class(temperature_unit_t), intent(in) :: units
        double precision :: temperature

        temperature = self%kelvin * units%conversion_factor - units%difference
    end function

    elemental function double_times_temperature( &
            multiplier, temperature) result(new_temperature)
        double precision, intent(in) :: multiplier
        class(temperature_t), intent(in) :: temperature
        type(temperature_t) :: new_temperature

        new_temperature%kelvin = &
                multiplier * temperature%kelvin
    end function

    elemental function integer_times_temperature( &
            multiplier, temperature) result(new_temperature)
        integer, intent(in) :: multiplier
        class(temperature_t), intent(in) :: temperature
        type(temperature_t) :: new_temperature

        new_temperature%kelvin = &
                dble(multiplier) * temperature%kelvin
    end function

    elemental function temperature_times_double( &
            temperature, multiplier) result(new_temperature)
        class(temperature_t), intent(in) :: temperature
        double precision, intent(in) :: multiplier
        type(temperature_t) :: new_temperature

        new_temperature%kelvin = &
                temperature%kelvin * multiplier
    end function

    elemental function temperature_times_integer( &
            temperature, multiplier) result(new_temperature)
        class(temperature_t), intent(in) :: temperature
        integer, intent(in) :: multiplier
        type(temperature_t) :: new_temperature

        new_temperature%kelvin = &
                temperature%kelvin * dble(multiplier)
    end function

    elemental function temperature_divided_by_double( &
            temperature, divisor) result(new_temperature)
        class(temperature_t), intent(in) :: temperature
        double precision, intent(in) :: divisor
        type(temperature_t) :: new_temperature

        new_temperature%kelvin = &
                temperature%kelvin / divisor
    end function

    elemental function temperature_divided_by_integer( &
            temperature, divisor) result(new_temperature)
        class(temperature_t), intent(in) :: temperature
        integer, intent(in) :: divisor
        type(temperature_t) :: new_temperature

        new_temperature%kelvin = &
                temperature%kelvin / dble(divisor)
    end function

    elemental function temperature_divided_by_temperature( &
            numerator, denomenator) result(ratio)
        class(temperature_t), intent(in) :: numerator
        type(temperature_t), intent(in) :: denomenator
        double precision :: ratio

        ratio = numerator%kelvin / denomenator%kelvin
    end function

    elemental function temperature_plus_temperature( &
            lhs, rhs) result(sum_)
        class(temperature_t), intent(in) :: lhs
        type(temperature_t), intent(in) :: rhs
        type(temperature_t) :: sum_

        sum_%kelvin = lhs%kelvin + rhs%kelvin
    end function

    elemental function temperature_minus_temperature( &
            lhs, rhs) result(difference)
        class(temperature_t), intent(in) :: lhs
        type(temperature_t), intent(in) :: rhs
        type(temperature_t) :: difference

        difference%kelvin = lhs%kelvin - rhs%kelvin
    end function

    pure function sum_temperature(temperatures) result(sum_)
        type(temperature_t), intent(in) :: temperatures(:)
        type(temperature_t) :: sum_

        sum_%kelvin = sum(temperatures%kelvin)
    end function

    elemental function greater_than(lhs, rhs)
        class(temperature_t), intent(in) :: lhs
        type(temperature_t), intent(in) :: rhs
        logical :: greater_than

        greater_than = lhs%kelvin > rhs%kelvin
    end function

    elemental function less_than(lhs, rhs)
        class(temperature_t), intent(in) :: lhs
        type(temperature_t), intent(in) :: rhs
        logical :: less_than

        less_than = lhs%kelvin < rhs%kelvin
    end function

    elemental function greater_than_or_equal(lhs, rhs)
        class(temperature_t), intent(in) :: lhs
        type(temperature_t), intent(in) :: rhs
        logical :: greater_than_or_equal

        greater_than_or_equal = lhs%kelvin >= rhs%kelvin
    end function

    elemental function less_than_or_equal(lhs, rhs)
        class(temperature_t), intent(in) :: lhs
        type(temperature_t), intent(in) :: rhs
        logical :: less_than_or_equal

        less_than_or_equal = lhs%kelvin <= rhs%kelvin
    end function

    elemental function equal_(lhs, rhs)
        class(temperature_t), intent(in) :: lhs
        type(temperature_t), intent(in) :: rhs
        logical :: equal_

        equal_ = lhs%kelvin .safeEq. rhs%kelvin
    end function

    elemental function equal_within_absolute_(lhs, rhs, within)
        class(temperature_t), intent(in) :: lhs
        type(temperature_t), intent(in) :: rhs
        type(temperature_t), intent(in) :: within
        logical :: equal_within_absolute_

        equal_within_absolute_ = equal_within_absolute( &
                lhs%kelvin, rhs%kelvin, within%kelvin)
    end function

    elemental function equal_within_relative_(lhs, rhs, within)
        class(temperature_t), intent(in) :: lhs
        type(temperature_t), intent(in) :: rhs
        double precision, intent(in) :: within
        logical :: equal_within_relative_

        equal_within_relative_ = equal_within_relative( &
                lhs%kelvin, rhs%kelvin, within)
    end function

    elemental function not_equal(lhs, rhs)
        class(temperature_t), intent(in) :: lhs
        type(temperature_t), intent(in) :: rhs
        logical :: not_equal

        not_equal = .not. lhs == rhs
    end function

    elemental function to_string_full_precision(self) result(string)
        class(temperature_t), intent(in) :: self
        type(varying_string) :: string

        string = self%to_string_in(DEFAULT_OUTPUT_UNITS)
    end function

    elemental function to_string_with_precision(self, significant_digits) result(string)
        class(temperature_t), intent(in) :: self
        integer, intent(in) :: significant_digits
        type(varying_string) :: string

        string = self%to_string_in(DEFAULT_OUTPUT_UNITS, significant_digits)
    end function

    elemental function to_string_in_full_precision(self, units) result(string)
        class(temperature_t), intent(in) :: self
        class(temperature_unit_t), intent(in) :: units
        type(varying_string) :: string

        string = units%to_string(to_string(self.in.units))
    end function

    elemental function to_string_in_with_precision( &
            self, units, significant_digits) result(string)
        class(temperature_t), intent(in) :: self
        class(temperature_unit_t), intent(in) :: units
        integer, intent(in) :: significant_digits
        type(varying_string) :: string

        string = units%to_string(to_string(self.in.units, significant_digits))
    end function

    function fallible_temperature_from_temperature( &
            temperature) result(fallible_temperature)
        type(temperature_t), intent(in) :: temperature
        type(fallible_temperature_t) :: fallible_temperature

        fallible_temperature%temperature_ = temperature
    end function

    function fallible_temperature_from_errors( &
            errors) result(fallible_temperature)
        type(error_list_t), intent(in) :: errors
        type(fallible_temperature_t) :: fallible_temperature

        fallible_temperature%errors_ = errors
    end function

    function fallible_temperature_from_fallible_temperature( &
            fallible_temperature, &
            module_, &
            procedure_) &
            result(new_fallible_temperature)
        type(fallible_temperature_t), intent(in) :: fallible_temperature
        type(module_t), intent(in) :: module_
        type(procedure_t), intent(in) :: procedure_
        type(fallible_temperature_t) :: new_fallible_temperature

        if (fallible_temperature%failed()) then
            new_fallible_temperature%errors_ = error_list_t( &
                    fallible_temperature%errors_, module_, procedure_)
        else
            new_fallible_temperature%temperature_ = fallible_temperature%temperature_
        end if
    end function

    elemental function fallible_temperature_failed( &
            self) result(failed)
        class(fallible_temperature_t), intent(in) :: self
        logical :: failed

        failed = self%errors_%has_any()
    end function

    elemental function fallible_temperature_temperature( &
            self) result(temperature)
        class(fallible_temperature_t), intent(in) :: self
        type(temperature_t) :: temperature

        temperature = self%temperature_
    end function

    impure elemental function fallible_temperature_errors( &
            self) result(errors)
        class(fallible_temperature_t), intent(in) :: self
        type(error_list_t) :: errors

        errors = self%errors_
    end function

    function fallible_temperature_unit_from_unit( &
            unit) result(fallible_temperature_unit)
        class(temperature_unit_t), intent(in) :: unit
        type(fallible_temperature_unit_t) :: fallible_temperature_unit

        allocate(fallible_temperature_unit%unit_, source = unit)
    end function

    function fallible_temperature_unit_from_errors( &
            errors) result(fallible_temperature_unit)
        type(error_list_t), intent(in) :: errors
        type(fallible_temperature_unit_t) :: fallible_temperature_unit

        fallible_temperature_unit%errors_ = errors
    end function

    function fallible_temperature_unit_from_fallible_temperature_unit( &
            fallible_temperature_unit, &
            module_, &
            procedure_) &
            result(new_fallible_temperature_unit)
        type(fallible_temperature_unit_t), intent(in) :: fallible_temperature_unit
        type(module_t), intent(in) :: module_
        type(procedure_t), intent(in) :: procedure_
        type(fallible_temperature_unit_t) :: new_fallible_temperature_unit

        if (fallible_temperature_unit%failed()) then
            new_fallible_temperature_unit%errors_ = error_list_t( &
                    fallible_temperature_unit%errors_, module_, procedure_)
        else
            allocate(new_fallible_temperature_unit%unit_, source = &
                    fallible_temperature_unit%unit_)
        end if
    end function

    elemental function fallible_temperature_unit_failed( &
            self) result(failed)
        class(fallible_temperature_unit_t), intent(in) :: self
        logical :: failed

        failed = self%errors_%has_any()
    end function

    function fallible_temperature_unit_unit( &
            self) result(unit)
        class(fallible_temperature_unit_t), intent(in) :: self
        class(temperature_unit_t), allocatable :: unit

        allocate(unit, source = self%unit_)
    end function

    impure elemental function fallible_temperature_unit_errors( &
            self) result(errors)
        class(fallible_temperature_unit_t), intent(in) :: self
        type(error_list_t) :: errors

        errors = self%errors_
    end function

    elemental function simple_unit_to_string(self) result(string)
        class(temperature_simple_unit_t), intent(in) :: self
        type(varying_string) :: string

        string = trim(self%symbol)
    end function

    pure function simple_value_to_string(self, value_) result(string)
        class(temperature_simple_unit_t), intent(in) :: self
        type(varying_string), intent(in) :: value_
        type(varying_string) :: string

        string = value_ // " " // self%to_string()
    end function

    function simple_parse_as(self, string) result(fallible_temperature)
        class(temperature_simple_unit_t), intent(in) :: self
        type(varying_string), intent(in) :: string
        type(fallible_temperature_t) :: fallible_temperature

        type(parse_result_t) :: parse_result

        parse_result = parse_with(the_parser, string)
        if (parse_result%ok()) then
            select type (the_number => parse_result%parsed())
            type is (parsed_rational_t)
                fallible_temperature = fallible_temperature_t(the_number%value_().unit.self)
            end select
        else
            fallible_temperature = fallible_temperature_t(error_list_t(fatal_t( &
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

    function parse_temperature_unit_c(string) result(fallible_temperature_unit)
        character(len=*), intent(in) :: string
        type(fallible_temperature_unit_t) :: fallible_temperature_unit

        fallible_temperature_unit = fallible_temperature_unit_t( &
                parse_temperature_unit(var_str(string), PROVIDED_UNITS), &
                module_t(MODULE_NAME), &
                procedure_t("parse_temperature_unit_c"))
    end function

    function parse_temperature_unit_s(string) result(fallible_temperature_unit)
        type(varying_string), intent(in) :: string
        type(fallible_temperature_unit_t) :: fallible_temperature_unit

        fallible_temperature_unit = fallible_temperature_unit_t( &
                parse_temperature_unit(string, PROVIDED_UNITS), &
                module_t(MODULE_NAME), &
                procedure_t("parse_temperature_unit_s"))
    end function

    function parse_temperature_unit_with_units_c( &
            string, units) result(fallible_temperature_unit)
        character(len=*), intent(in) :: string
        class(temperature_unit_t), intent(in) :: units(:)
        type(fallible_temperature_unit_t) :: fallible_temperature_unit

        fallible_temperature_unit = fallible_temperature_unit_t( &
                parse_temperature_unit(var_str(string), units), &
                module_t(MODULE_NAME), &
                procedure_t("parse_temperature_unit_with_units_c"))
    end function

    function parse_temperature_unit_with_units_s( &
            string, units) result(fallible_temperature_unit)
        type(varying_string), intent(in) :: string
        class(temperature_unit_t), intent(in) :: units(:)
        type(fallible_temperature_unit_t) :: fallible_temperature_unit

        integer :: i
        type(varying_string) :: unit_strings(size(units))

        do i = 1, size(units)
            if (string == units(i)%to_string()) then
                fallible_temperature_unit = fallible_temperature_unit_t(units(i))
                return
            end if
        end do
        do i = 1, size(units)
            unit_strings(i) = units(i)%to_string()
        end do
        fallible_temperature_unit = fallible_temperature_unit_t(error_list_t(fatal_t( &
                UNKNOWN_UNIT, &
                module_t(MODULE_NAME), &
                procedure_t("parse_temperature_unit_with_units_s"), &
                '"' // string // '", known units: [' // join(unit_strings, ', ') // ']')))
    end function
end module
