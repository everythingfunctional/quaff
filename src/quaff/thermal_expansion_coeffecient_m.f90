module thermal_expansion_coeffecient_m
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
            PER_RANKINE_PER_KELVIN
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
            thermal_expansion_coeffecient_t, &
            fallible_thermal_expansion_coeffecient_t, &
            thermal_expansion_coeffecient_unit_t, &
            fallible_thermal_expansion_coeffecient_unit_t, &
            thermal_expansion_coeffecient_simple_unit_t, &
            operator(.unit.), &
            parse_thermal_expansion_coeffecient, &
            parse_thermal_expansion_coeffecient_unit, &
            sum, &
            DEFAULT_OUTPUT_UNITS, &
            PROVIDED_UNITS, &
            PER_KELVIN, &
            PER_RANKINE, &
            PER_FAHRENHEIT

    type :: thermal_expansion_coeffecient_t
        double precision :: per_kelvin
    contains
        private
        procedure :: to_units
        generic, public :: operator(.in.) => to_units
        procedure, pass(thermal_expansion_coeffecient) :: double_times_thermal_expansion_coeffecient
        procedure, pass(thermal_expansion_coeffecient) :: integer_times_thermal_expansion_coeffecient
        procedure :: thermal_expansion_coeffecient_times_double
        procedure :: thermal_expansion_coeffecient_times_integer
        generic, public :: operator(*) => &
                double_times_thermal_expansion_coeffecient, &
                integer_times_thermal_expansion_coeffecient, &
                thermal_expansion_coeffecient_times_double, &
                thermal_expansion_coeffecient_times_integer
        procedure :: thermal_expansion_coeffecient_divided_by_double
        procedure :: thermal_expansion_coeffecient_divided_by_integer
        procedure :: therm_exp_coeff_divided_by_therm_exp_coefft
        generic, public :: operator(/) => &
                thermal_expansion_coeffecient_divided_by_double, &
                thermal_expansion_coeffecient_divided_by_integer, &
                therm_exp_coeff_divided_by_therm_exp_coefft
        procedure :: therm_exp_coeff_plus_therm_exp_coeff
        generic, public :: operator(+) => therm_exp_coeff_plus_therm_exp_coeff
        procedure :: therm_exp_coeff_minus_therm_exp_coeff
        generic, public :: operator(-) => therm_exp_coeff_minus_therm_exp_coeff
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

    type :: fallible_thermal_expansion_coeffecient_t
        private
        type(thermal_expansion_coeffecient_t) :: thermal_expansion_coeffecient_ = thermal_expansion_coeffecient_t(0.0d0)
        type(error_list_t) :: errors_
    contains
        private
        procedure, public :: failed => fallible_thermal_expansion_coeffecient_failed
        procedure, public :: thermal_expansion_coeffecient => fallible_therm_exp_coeff_therm_exp_coeff
        procedure, public :: errors => fallible_thermal_expansion_coeffecient_errors
    end type

    type, abstract :: thermal_expansion_coeffecient_unit_t
        double precision :: conversion_factor
    contains
        procedure(unit_to_string_i), deferred :: unit_to_string
        procedure(value_to_string_i), deferred :: value_to_string
        generic :: to_string => unit_to_string, value_to_string
        procedure(parse_as_i), deferred :: parse_as
    end type

    type :: fallible_thermal_expansion_coeffecient_unit_t
        private
        class(thermal_expansion_coeffecient_unit_t), allocatable :: unit_
        type(error_list_t) :: errors_
    contains
        private
        procedure, public :: failed => fallible_thermal_expansion_coeffecient_unit_failed
        procedure, public :: unit => fallible_thermal_expansion_coeffecient_unit_unit
        procedure, public :: errors => fallible_thermal_expansion_coeffecient_unit_errors
    end type

    type, extends(thermal_expansion_coeffecient_unit_t) :: thermal_expansion_coeffecient_simple_unit_t
        character(len=20) :: symbol
    contains
        procedure :: unit_to_string => simple_unit_to_string
        procedure :: value_to_string => simple_value_to_string
        procedure :: parse_as => simple_parse_as
    end type

    abstract interface
        elemental function unit_to_string_i(self) result(string)
            import :: thermal_expansion_coeffecient_unit_t, varying_string

            implicit none

            class(thermal_expansion_coeffecient_unit_t), intent(in) :: self
            type(varying_string) :: string
        end function

        pure function value_to_string_i(self, value_) result(string)
            import :: thermal_expansion_coeffecient_unit_t, varying_string

            implicit none

            class(thermal_expansion_coeffecient_unit_t), intent(in) :: self
            type(varying_string), intent(in) :: value_
            type(varying_string) :: string
        end function

        function parse_as_i(self, string) result(fallible_thermal_expansion_coeffecient)
            import :: thermal_expansion_coeffecient_unit_t, fallible_thermal_expansion_coeffecient_t, varying_string

            implicit none

            class(thermal_expansion_coeffecient_unit_t), intent(in) :: self
            type(varying_string), intent(in) :: string
            type(fallible_thermal_expansion_coeffecient_t) :: fallible_thermal_expansion_coeffecient
        end function
    end interface

    interface operator(.unit.)
        module procedure from_units
    end interface

    interface fallible_thermal_expansion_coeffecient_t
        module procedure fallible_therm_exp_coeff_from_therm_exp_coeff
        module procedure fallible_thermal_expansion_coeffecient_from_errors
        module procedure fallible_therm_exp_coeff_from_fallible_therm_exp_coeff
    end interface

    interface fallible_thermal_expansion_coeffecient_unit_t
        module procedure fallible_thermal_expansion_coeffecient_unit_from_unit
        module procedure fallible_thermal_expansion_coeffecient_unit_from_errors
        module procedure fallible_therm_exp_co_unit_from_fallible_therm_exp_co_unit
    end interface

    interface parse_thermal_expansion_coeffecient
        module procedure parse_thermal_expansion_coeffecient_c
        module procedure parse_thermal_expansion_coeffecient_s
        module procedure parse_thermal_expansion_coeffecient_with_units_c
        module procedure parse_thermal_expansion_coeffecient_with_units_s
    end interface

    interface parse_thermal_expansion_coeffecient_unit
        module procedure parse_thermal_expansion_coeffecient_unit_c
        module procedure parse_thermal_expansion_coeffecient_unit_s
        module procedure parse_thermal_expansion_coeffecient_unit_with_units_c
        module procedure parse_thermal_expansion_coeffecient_unit_with_units_s
    end interface

    interface sum
        module procedure sum_thermal_expansion_coeffecient
    end interface

    type(thermal_expansion_coeffecient_simple_unit_t), parameter :: PER_KELVIN = &
            thermal_expansion_coeffecient_simple_unit_t( &
                    conversion_factor = 1.0d0, &
                    symbol = "1/K")
    type(thermal_expansion_coeffecient_simple_unit_t), parameter :: PER_RANKINE = &
            thermal_expansion_coeffecient_simple_unit_t( &
                    conversion_factor = PER_RANKINE_PER_KELVIN, &
                    symbol = "1/R")
    type(thermal_expansion_coeffecient_simple_unit_t), parameter :: PER_FAHRENHEIT = &
            thermal_expansion_coeffecient_simple_unit_t( &
                    conversion_factor = PER_RANKINE_PER_KELVIN, &
                    symbol = "1/F")

    type(thermal_expansion_coeffecient_simple_unit_t) :: DEFAULT_OUTPUT_UNITS = PER_KELVIN

    type(thermal_expansion_coeffecient_simple_unit_t), parameter :: PROVIDED_UNITS(*) = &
            [PER_KELVIN, PER_RANKINE, PER_FAHRENHEIT]

    character(len=*), parameter :: MODULE_NAME = "thermal_expansion_coeffecient_m"
contains
    function parse_thermal_expansion_coeffecient_c(string) result(fallible_thermal_expansion_coeffecient)
        character(len=*), intent(in) :: string
        type(fallible_thermal_expansion_coeffecient_t) :: fallible_thermal_expansion_coeffecient

        fallible_thermal_expansion_coeffecient = fallible_thermal_expansion_coeffecient_t( &
                parse_thermal_expansion_coeffecient(var_str(string), PROVIDED_UNITS), &
                module_t(MODULE_NAME), &
                procedure_t("parse_thermal_expansion_coeffecient_c"))
    end function

    function parse_thermal_expansion_coeffecient_s(string) result(fallible_thermal_expansion_coeffecient)
        type(varying_string), intent(in) :: string
        type(fallible_thermal_expansion_coeffecient_t) :: fallible_thermal_expansion_coeffecient

        fallible_thermal_expansion_coeffecient = fallible_thermal_expansion_coeffecient_t( &
                parse_thermal_expansion_coeffecient(string, PROVIDED_UNITS), &
                module_t(MODULE_NAME), &
                procedure_t("parse_thermal_expansion_coeffecient_s"))
    end function

    function parse_thermal_expansion_coeffecient_with_units_c( &
            string, units) result(fallible_thermal_expansion_coeffecient)
        character(len=*), intent(in) :: string
        class(thermal_expansion_coeffecient_unit_t), intent(in) :: units(:)
        type(fallible_thermal_expansion_coeffecient_t) :: fallible_thermal_expansion_coeffecient

        fallible_thermal_expansion_coeffecient = fallible_thermal_expansion_coeffecient_t( &
                parse_thermal_expansion_coeffecient(var_str(string), units), &
                module_t(MODULE_NAME), &
                procedure_t("parse_thermal_expansion_coeffecient_with_units_c"))
    end function

    function parse_thermal_expansion_coeffecient_with_units_s( &
            string, units) result(fallible_thermal_expansion_coeffecient)
        type(varying_string), intent(in) :: string
        class(thermal_expansion_coeffecient_unit_t), intent(in) :: units(:)
        type(fallible_thermal_expansion_coeffecient_t) :: fallible_thermal_expansion_coeffecient

        type(fallible_thermal_expansion_coeffecient_t) :: all_attempts(size(units))
        integer :: i

        do i = 1, size(units)
            all_attempts(i) = units(i)%parse_as(string)
            if (.not. all_attempts(i)%failed()) then
                fallible_thermal_expansion_coeffecient = all_attempts(i)
                return
            end if
        end do
        fallible_thermal_expansion_coeffecient = fallible_thermal_expansion_coeffecient_t(error_list_t( &
                all_attempts%errors(), &
                module_t(MODULE_NAME), &
                procedure_t("parse_thermal_expansion_coeffecient_with_units_s")))
    end function

    elemental function from_units(value_, units) result(thermal_expansion_coeffecient)
        double precision, intent(in) :: value_
        class(thermal_expansion_coeffecient_unit_t), intent(in) :: units
        type(thermal_expansion_coeffecient_t) :: thermal_expansion_coeffecient

        thermal_expansion_coeffecient%per_kelvin = value_ / units%conversion_factor
    end function

    elemental function to_units(self, units) result(thermal_expansion_coeffecient)
        class(thermal_expansion_coeffecient_t), intent(in) :: self
        class(thermal_expansion_coeffecient_unit_t), intent(in) :: units
        double precision :: thermal_expansion_coeffecient

        thermal_expansion_coeffecient = self%per_kelvin * units%conversion_factor
    end function

    elemental function double_times_thermal_expansion_coeffecient( &
            multiplier, thermal_expansion_coeffecient) result(new_thermal_expansion_coeffecient)
        double precision, intent(in) :: multiplier
        class(thermal_expansion_coeffecient_t), intent(in) :: thermal_expansion_coeffecient
        type(thermal_expansion_coeffecient_t) :: new_thermal_expansion_coeffecient

        new_thermal_expansion_coeffecient%per_kelvin = &
                multiplier * thermal_expansion_coeffecient%per_kelvin
    end function

    elemental function integer_times_thermal_expansion_coeffecient( &
            multiplier, thermal_expansion_coeffecient) result(new_thermal_expansion_coeffecient)
        integer, intent(in) :: multiplier
        class(thermal_expansion_coeffecient_t), intent(in) :: thermal_expansion_coeffecient
        type(thermal_expansion_coeffecient_t) :: new_thermal_expansion_coeffecient

        new_thermal_expansion_coeffecient%per_kelvin = &
                dble(multiplier) * thermal_expansion_coeffecient%per_kelvin
    end function

    elemental function thermal_expansion_coeffecient_times_double( &
            thermal_expansion_coeffecient, multiplier) result(new_thermal_expansion_coeffecient)
        class(thermal_expansion_coeffecient_t), intent(in) :: thermal_expansion_coeffecient
        double precision, intent(in) :: multiplier
        type(thermal_expansion_coeffecient_t) :: new_thermal_expansion_coeffecient

        new_thermal_expansion_coeffecient%per_kelvin = &
                thermal_expansion_coeffecient%per_kelvin * multiplier
    end function

    elemental function thermal_expansion_coeffecient_times_integer( &
            thermal_expansion_coeffecient, multiplier) result(new_thermal_expansion_coeffecient)
        class(thermal_expansion_coeffecient_t), intent(in) :: thermal_expansion_coeffecient
        integer, intent(in) :: multiplier
        type(thermal_expansion_coeffecient_t) :: new_thermal_expansion_coeffecient

        new_thermal_expansion_coeffecient%per_kelvin = &
                thermal_expansion_coeffecient%per_kelvin * dble(multiplier)
    end function

    elemental function thermal_expansion_coeffecient_divided_by_double( &
            thermal_expansion_coeffecient, divisor) result(new_thermal_expansion_coeffecient)
        class(thermal_expansion_coeffecient_t), intent(in) :: thermal_expansion_coeffecient
        double precision, intent(in) :: divisor
        type(thermal_expansion_coeffecient_t) :: new_thermal_expansion_coeffecient

        new_thermal_expansion_coeffecient%per_kelvin = &
                thermal_expansion_coeffecient%per_kelvin / divisor
    end function

    elemental function thermal_expansion_coeffecient_divided_by_integer( &
            thermal_expansion_coeffecient, divisor) result(new_thermal_expansion_coeffecient)
        class(thermal_expansion_coeffecient_t), intent(in) :: thermal_expansion_coeffecient
        integer, intent(in) :: divisor
        type(thermal_expansion_coeffecient_t) :: new_thermal_expansion_coeffecient

        new_thermal_expansion_coeffecient%per_kelvin = &
                thermal_expansion_coeffecient%per_kelvin / dble(divisor)
    end function

    elemental function therm_exp_coeff_divided_by_therm_exp_coefft( &
            numerator, denomenator) result(ratio)
        class(thermal_expansion_coeffecient_t), intent(in) :: numerator
        type(thermal_expansion_coeffecient_t), intent(in) :: denomenator
        double precision :: ratio

        ratio = numerator%per_kelvin / denomenator%per_kelvin
    end function

    elemental function therm_exp_coeff_plus_therm_exp_coeff( &
            lhs, rhs) result(sum_)
        class(thermal_expansion_coeffecient_t), intent(in) :: lhs
        type(thermal_expansion_coeffecient_t), intent(in) :: rhs
        type(thermal_expansion_coeffecient_t) :: sum_

        sum_%per_kelvin = lhs%per_kelvin + rhs%per_kelvin
    end function

    elemental function therm_exp_coeff_minus_therm_exp_coeff( &
            lhs, rhs) result(difference)
        class(thermal_expansion_coeffecient_t), intent(in) :: lhs
        type(thermal_expansion_coeffecient_t), intent(in) :: rhs
        type(thermal_expansion_coeffecient_t) :: difference

        difference%per_kelvin = lhs%per_kelvin - rhs%per_kelvin
    end function

    pure function sum_thermal_expansion_coeffecient(thermal_expansion_coeffecients) result(sum_)
        type(thermal_expansion_coeffecient_t), intent(in) :: thermal_expansion_coeffecients(:)
        type(thermal_expansion_coeffecient_t) :: sum_

        sum_%per_kelvin = sum(thermal_expansion_coeffecients%per_kelvin)
    end function

    elemental function greater_than(lhs, rhs)
        class(thermal_expansion_coeffecient_t), intent(in) :: lhs
        type(thermal_expansion_coeffecient_t), intent(in) :: rhs
        logical :: greater_than

        greater_than = lhs%per_kelvin > rhs%per_kelvin
    end function

    elemental function less_than(lhs, rhs)
        class(thermal_expansion_coeffecient_t), intent(in) :: lhs
        type(thermal_expansion_coeffecient_t), intent(in) :: rhs
        logical :: less_than

        less_than = lhs%per_kelvin < rhs%per_kelvin
    end function

    elemental function greater_than_or_equal(lhs, rhs)
        class(thermal_expansion_coeffecient_t), intent(in) :: lhs
        type(thermal_expansion_coeffecient_t), intent(in) :: rhs
        logical :: greater_than_or_equal

        greater_than_or_equal = lhs%per_kelvin >= rhs%per_kelvin
    end function

    elemental function less_than_or_equal(lhs, rhs)
        class(thermal_expansion_coeffecient_t), intent(in) :: lhs
        type(thermal_expansion_coeffecient_t), intent(in) :: rhs
        logical :: less_than_or_equal

        less_than_or_equal = lhs%per_kelvin <= rhs%per_kelvin
    end function

    elemental function equal_(lhs, rhs)
        class(thermal_expansion_coeffecient_t), intent(in) :: lhs
        type(thermal_expansion_coeffecient_t), intent(in) :: rhs
        logical :: equal_

        equal_ = lhs%per_kelvin .safeEq. rhs%per_kelvin
    end function

    elemental function equal_within_absolute_(lhs, rhs, within)
        class(thermal_expansion_coeffecient_t), intent(in) :: lhs
        type(thermal_expansion_coeffecient_t), intent(in) :: rhs
        type(thermal_expansion_coeffecient_t), intent(in) :: within
        logical :: equal_within_absolute_

        equal_within_absolute_ = equal_within_absolute( &
                lhs%per_kelvin, rhs%per_kelvin, within%per_kelvin)
    end function

    elemental function equal_within_relative_(lhs, rhs, within)
        class(thermal_expansion_coeffecient_t), intent(in) :: lhs
        type(thermal_expansion_coeffecient_t), intent(in) :: rhs
        double precision, intent(in) :: within
        logical :: equal_within_relative_

        equal_within_relative_ = equal_within_relative( &
                lhs%per_kelvin, rhs%per_kelvin, within)
    end function

    elemental function not_equal(lhs, rhs)
        class(thermal_expansion_coeffecient_t), intent(in) :: lhs
        type(thermal_expansion_coeffecient_t), intent(in) :: rhs
        logical :: not_equal

        not_equal = .not. lhs == rhs
    end function

    elemental function to_string_full_precision(self) result(string)
        class(thermal_expansion_coeffecient_t), intent(in) :: self
        type(varying_string) :: string

        string = self%to_string_in(DEFAULT_OUTPUT_UNITS)
    end function

    elemental function to_string_with_precision(self, significant_digits) result(string)
        class(thermal_expansion_coeffecient_t), intent(in) :: self
        integer, intent(in) :: significant_digits
        type(varying_string) :: string

        string = self%to_string_in(DEFAULT_OUTPUT_UNITS, significant_digits)
    end function

    elemental function to_string_in_full_precision(self, units) result(string)
        class(thermal_expansion_coeffecient_t), intent(in) :: self
        class(thermal_expansion_coeffecient_unit_t), intent(in) :: units
        type(varying_string) :: string

        string = units%to_string(to_string(self.in.units))
    end function

    elemental function to_string_in_with_precision( &
            self, units, significant_digits) result(string)
        class(thermal_expansion_coeffecient_t), intent(in) :: self
        class(thermal_expansion_coeffecient_unit_t), intent(in) :: units
        integer, intent(in) :: significant_digits
        type(varying_string) :: string

        string = units%to_string(to_string(self.in.units, significant_digits))
    end function

    function fallible_therm_exp_coeff_from_therm_exp_coeff( &
            thermal_expansion_coeffecient) result(fallible_thermal_expansion_coeffecient)
        type(thermal_expansion_coeffecient_t), intent(in) :: thermal_expansion_coeffecient
        type(fallible_thermal_expansion_coeffecient_t) :: fallible_thermal_expansion_coeffecient

        fallible_thermal_expansion_coeffecient%thermal_expansion_coeffecient_ = thermal_expansion_coeffecient
    end function

    function fallible_thermal_expansion_coeffecient_from_errors( &
            errors) result(fallible_thermal_expansion_coeffecient)
        type(error_list_t), intent(in) :: errors
        type(fallible_thermal_expansion_coeffecient_t) :: fallible_thermal_expansion_coeffecient

        fallible_thermal_expansion_coeffecient%errors_ = errors
    end function

    function fallible_therm_exp_coeff_from_fallible_therm_exp_coeff( &
            fallible_thermal_expansion_coeffecient, &
            module_, &
            procedure_) &
            result(new_fallible_thermal_expansion_coeffecient)
        type(fallible_thermal_expansion_coeffecient_t), intent(in) :: fallible_thermal_expansion_coeffecient
        type(module_t), intent(in) :: module_
        type(procedure_t), intent(in) :: procedure_
        type(fallible_thermal_expansion_coeffecient_t) :: new_fallible_thermal_expansion_coeffecient

        if (fallible_thermal_expansion_coeffecient%failed()) then
            new_fallible_thermal_expansion_coeffecient%errors_ = error_list_t( &
                    fallible_thermal_expansion_coeffecient%errors_, module_, procedure_)
        else
            new_fallible_thermal_expansion_coeffecient%thermal_expansion_coeffecient_ = &
              fallible_thermal_expansion_coeffecient%thermal_expansion_coeffecient_
        end if
    end function

    elemental function fallible_thermal_expansion_coeffecient_failed( &
            self) result(failed)
        class(fallible_thermal_expansion_coeffecient_t), intent(in) :: self
        logical :: failed

        failed = self%errors_%has_any()
    end function

    elemental function fallible_therm_exp_coeff_therm_exp_coeff( &
            self) result(thermal_expansion_coeffecient)
        class(fallible_thermal_expansion_coeffecient_t), intent(in) :: self
        type(thermal_expansion_coeffecient_t) :: thermal_expansion_coeffecient

        thermal_expansion_coeffecient = self%thermal_expansion_coeffecient_
    end function

    impure elemental function fallible_thermal_expansion_coeffecient_errors( &
            self) result(errors)
        class(fallible_thermal_expansion_coeffecient_t), intent(in) :: self
        type(error_list_t) :: errors

        errors = self%errors_
    end function

    function fallible_thermal_expansion_coeffecient_unit_from_unit( &
            unit) result(fallible_thermal_expansion_coeffecient_unit)
        class(thermal_expansion_coeffecient_unit_t), intent(in) :: unit
        type(fallible_thermal_expansion_coeffecient_unit_t) :: fallible_thermal_expansion_coeffecient_unit

        allocate(fallible_thermal_expansion_coeffecient_unit%unit_, source = unit)
    end function

    function fallible_thermal_expansion_coeffecient_unit_from_errors( &
            errors) result(fallible_thermal_expansion_coeffecient_unit)
        type(error_list_t), intent(in) :: errors
        type(fallible_thermal_expansion_coeffecient_unit_t) :: fallible_thermal_expansion_coeffecient_unit

        fallible_thermal_expansion_coeffecient_unit%errors_ = errors
    end function

    function fallible_therm_exp_co_unit_from_fallible_therm_exp_co_unit( &
            fallible_thermal_expansion_coeffecient_unit, &
            module_, &
            procedure_) &
            result(new_fallible_thermal_expansion_coeffecient_unit)
        type(fallible_thermal_expansion_coeffecient_unit_t), intent(in) :: fallible_thermal_expansion_coeffecient_unit
        type(module_t), intent(in) :: module_
        type(procedure_t), intent(in) :: procedure_
        type(fallible_thermal_expansion_coeffecient_unit_t) :: new_fallible_thermal_expansion_coeffecient_unit

        if (fallible_thermal_expansion_coeffecient_unit%failed()) then
            new_fallible_thermal_expansion_coeffecient_unit%errors_ = error_list_t( &
                    fallible_thermal_expansion_coeffecient_unit%errors_, module_, procedure_)
        else
            allocate(new_fallible_thermal_expansion_coeffecient_unit%unit_, source = &
                    fallible_thermal_expansion_coeffecient_unit%unit_)
        end if
    end function

    elemental function fallible_thermal_expansion_coeffecient_unit_failed( &
            self) result(failed)
        class(fallible_thermal_expansion_coeffecient_unit_t), intent(in) :: self
        logical :: failed

        failed = self%errors_%has_any()
    end function

    function fallible_thermal_expansion_coeffecient_unit_unit( &
            self) result(unit)
        class(fallible_thermal_expansion_coeffecient_unit_t), intent(in) :: self
        class(thermal_expansion_coeffecient_unit_t), allocatable :: unit

        allocate(unit, source = self%unit_)
    end function

    impure elemental function fallible_thermal_expansion_coeffecient_unit_errors( &
            self) result(errors)
        class(fallible_thermal_expansion_coeffecient_unit_t), intent(in) :: self
        type(error_list_t) :: errors

        errors = self%errors_
    end function

    elemental function simple_unit_to_string(self) result(string)
        class(thermal_expansion_coeffecient_simple_unit_t), intent(in) :: self
        type(varying_string) :: string

        string = trim(self%symbol)
    end function

    pure function simple_value_to_string(self, value_) result(string)
        class(thermal_expansion_coeffecient_simple_unit_t), intent(in) :: self
        type(varying_string), intent(in) :: value_
        type(varying_string) :: string

        string = value_ // " " // self%to_string()
    end function

    function simple_parse_as(self, string) result(fallible_thermal_expansion_coeffecient)
        class(thermal_expansion_coeffecient_simple_unit_t), intent(in) :: self
        type(varying_string), intent(in) :: string
        type(fallible_thermal_expansion_coeffecient_t) :: fallible_thermal_expansion_coeffecient

        type(parse_result_t) :: parse_result

        parse_result = parse_with(the_parser, string)
        if (parse_result%ok) then
            select type (the_number => parse_result%parsed)
            type is (parsed_rational_t)
                fallible_thermal_expansion_coeffecient = fallible_thermal_expansion_coeffecient_t(the_number%value_.unit.self)
            end select
        else
            fallible_thermal_expansion_coeffecient = fallible_thermal_expansion_coeffecient_t(error_list_t(fatal_t( &
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

    function parse_thermal_expansion_coeffecient_unit_c(string) result(fallible_thermal_expansion_coeffecient_unit)
        character(len=*), intent(in) :: string
        type(fallible_thermal_expansion_coeffecient_unit_t) :: fallible_thermal_expansion_coeffecient_unit

        fallible_thermal_expansion_coeffecient_unit = fallible_thermal_expansion_coeffecient_unit_t( &
                parse_thermal_expansion_coeffecient_unit(var_str(string), PROVIDED_UNITS), &
                module_t(MODULE_NAME), &
                procedure_t("parse_thermal_expansion_coeffecient_unit_c"))
    end function

    function parse_thermal_expansion_coeffecient_unit_s(string) result(fallible_thermal_expansion_coeffecient_unit)
        type(varying_string), intent(in) :: string
        type(fallible_thermal_expansion_coeffecient_unit_t) :: fallible_thermal_expansion_coeffecient_unit

        fallible_thermal_expansion_coeffecient_unit = fallible_thermal_expansion_coeffecient_unit_t( &
                parse_thermal_expansion_coeffecient_unit(string, PROVIDED_UNITS), &
                module_t(MODULE_NAME), &
                procedure_t("parse_thermal_expansion_coeffecient_unit_s"))
    end function

    function parse_thermal_expansion_coeffecient_unit_with_units_c( &
            string, units) result(fallible_thermal_expansion_coeffecient_unit)
        character(len=*), intent(in) :: string
        class(thermal_expansion_coeffecient_unit_t), intent(in) :: units(:)
        type(fallible_thermal_expansion_coeffecient_unit_t) :: fallible_thermal_expansion_coeffecient_unit

        fallible_thermal_expansion_coeffecient_unit = fallible_thermal_expansion_coeffecient_unit_t( &
                parse_thermal_expansion_coeffecient_unit(var_str(string), units), &
                module_t(MODULE_NAME), &
                procedure_t("parse_thermal_expansion_coeffecient_unit_with_units_c"))
    end function

    function parse_thermal_expansion_coeffecient_unit_with_units_s( &
            string, units) result(fallible_thermal_expansion_coeffecient_unit)
        type(varying_string), intent(in) :: string
        class(thermal_expansion_coeffecient_unit_t), intent(in) :: units(:)
        type(fallible_thermal_expansion_coeffecient_unit_t) :: fallible_thermal_expansion_coeffecient_unit

        integer :: i
        type(varying_string) :: unit_strings(size(units))

        do i = 1, size(units)
            if (string == units(i)%to_string()) then
                fallible_thermal_expansion_coeffecient_unit = fallible_thermal_expansion_coeffecient_unit_t(units(i))
                return
            end if
        end do
        do i = 1, size(units)
            unit_strings(i) = units(i)%to_string()
        end do
        fallible_thermal_expansion_coeffecient_unit = fallible_thermal_expansion_coeffecient_unit_t(error_list_t(fatal_t( &
                UNKNOWN_UNIT, &
                module_t(MODULE_NAME), &
                procedure_t("parse_thermal_expansion_coeffecient_unit_with_units_s"), &
                '"' // string // '", known units: [' // join(unit_strings, ', ') // ']')))
    end function
end module
