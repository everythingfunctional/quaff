module quaff_thermal_conductivity_m
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
            CAL_PER_SEC_CM_K_PER_WATTS_PER_METER_KELVIN, &
            WATTS_PER_CENTIMETER_KELVIN_PER_WATTS_PER_METER_KELVIN, &
            BTU_PER_HOUR_FEET_RANKINE_PER_WATTS_PER_METER_KELVIN
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
            thermal_conductivity_t, &
            fallible_thermal_conductivity_t, &
            thermal_conductivity_unit_t, &
            fallible_thermal_conductivity_unit_t, &
            thermal_conductivity_simple_unit_t, &
            operator(.unit.), &
            parse_thermal_conductivity, &
            parse_thermal_conductivity_unit, &
            sum, &
            DEFAULT_OUTPUT_UNITS, &
            PROVIDED_UNITS, &
            CALORIES_PER_SECOND_CENTIMETER_KELVIN, &
            WATTS_PER_CENTIMETER_KELVIN, &
            WATTS_PER_METER_KELVIN, &
            BTU_PER_HOUR_FEET_FAHRENHEIT, &
            BTU_PER_HOUR_FEET_RANKINE

    type :: thermal_conductivity_t
        double precision :: watts_per_meter_kelvin
    contains
        private
        procedure :: to_units
        generic, public :: operator(.in.) => to_units
        procedure, pass(thermal_conductivity) :: double_times_thermal_conductivity
        procedure, pass(thermal_conductivity) :: integer_times_thermal_conductivity
        procedure :: thermal_conductivity_times_double
        procedure :: thermal_conductivity_times_integer
        generic, public :: operator(*) => &
                double_times_thermal_conductivity, &
                integer_times_thermal_conductivity, &
                thermal_conductivity_times_double, &
                thermal_conductivity_times_integer
        procedure :: thermal_conductivity_divided_by_double
        procedure :: thermal_conductivity_divided_by_integer
        procedure :: thermal_conductivity_divided_by_thermal_conductivity
        generic, public :: operator(/) => &
                thermal_conductivity_divided_by_double, &
                thermal_conductivity_divided_by_integer, &
                thermal_conductivity_divided_by_thermal_conductivity
        procedure :: thermal_conductivity_plus_thermal_conductivity
        generic, public :: operator(+) => thermal_conductivity_plus_thermal_conductivity
        procedure :: thermal_conductivity_minus_thermal_conductivity
        generic, public :: operator(-) => thermal_conductivity_minus_thermal_conductivity
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

    type :: fallible_thermal_conductivity_t
        private
        type(thermal_conductivity_t) :: thermal_conductivity_ = thermal_conductivity_t(0.0d0)
        type(error_list_t) :: errors_
    contains
        private
        procedure, public :: failed => fallible_thermal_conductivity_failed
        procedure, public :: thermal_conductivity => fallible_thermal_conductivity_thermal_conductivity
        procedure, public :: errors => fallible_thermal_conductivity_errors
    end type

    type, abstract :: thermal_conductivity_unit_t
        double precision :: conversion_factor
    contains
        procedure(unit_to_string_i), deferred :: unit_to_string
        procedure(value_to_string_i), deferred :: value_to_string
        generic :: to_string => unit_to_string, value_to_string
        procedure(parse_as_i), deferred :: parse_as
    end type

    type :: fallible_thermal_conductivity_unit_t
        private
        class(thermal_conductivity_unit_t), allocatable :: unit_
        type(error_list_t) :: errors_
    contains
        private
        procedure, public :: failed => fallible_thermal_conductivity_unit_failed
        procedure, public :: unit => fallible_thermal_conductivity_unit_unit
        procedure, public :: errors => fallible_thermal_conductivity_unit_errors
    end type

    type, extends(thermal_conductivity_unit_t) :: thermal_conductivity_simple_unit_t
        character(len=20) :: symbol
    contains
        procedure :: unit_to_string => simple_unit_to_string
        procedure :: value_to_string => simple_value_to_string
        procedure :: parse_as => simple_parse_as
    end type

    abstract interface
        elemental function unit_to_string_i(self) result(string)
            import :: thermal_conductivity_unit_t, varying_string

            implicit none

            class(thermal_conductivity_unit_t), intent(in) :: self
            type(varying_string) :: string
        end function

        pure function value_to_string_i(self, value_) result(string)
            import :: thermal_conductivity_unit_t, varying_string

            implicit none

            class(thermal_conductivity_unit_t), intent(in) :: self
            type(varying_string), intent(in) :: value_
            type(varying_string) :: string
        end function

        function parse_as_i(self, string) result(fallible_thermal_conductivity)
            import :: thermal_conductivity_unit_t, fallible_thermal_conductivity_t, varying_string

            implicit none

            class(thermal_conductivity_unit_t), intent(in) :: self
            type(varying_string), intent(in) :: string
            type(fallible_thermal_conductivity_t) :: fallible_thermal_conductivity
        end function
    end interface

    interface operator(.unit.)
        module procedure from_units
    end interface

    interface fallible_thermal_conductivity_t
        module procedure fallible_thermal_conductivity_from_thermal_conductivity
        module procedure fallible_thermal_conductivity_from_errors
        module procedure fallible_therm_conduct_from_fallible_therm_conduct
    end interface

    interface fallible_thermal_conductivity_unit_t
        module procedure fallible_thermal_conductivity_unit_from_unit
        module procedure fallible_thermal_conductivity_unit_from_errors
        module procedure fallible_therm_conduct_unit_from_fallible_therm_conduct_unit
    end interface

    interface parse_thermal_conductivity
        module procedure parse_thermal_conductivity_c
        module procedure parse_thermal_conductivity_s
        module procedure parse_thermal_conductivity_with_units_c
        module procedure parse_thermal_conductivity_with_units_s
    end interface

    interface parse_thermal_conductivity_unit
        module procedure parse_thermal_conductivity_unit_c
        module procedure parse_thermal_conductivity_unit_s
        module procedure parse_thermal_conductivity_unit_with_units_c
        module procedure parse_thermal_conductivity_unit_with_units_s
    end interface

    interface sum
        module procedure sum_thermal_conductivity
    end interface

    type(thermal_conductivity_simple_unit_t), parameter :: CALORIES_PER_SECOND_CENTIMETER_KELVIN = &
            thermal_conductivity_simple_unit_t( &
                    conversion_factor = CAL_PER_SEC_CM_K_PER_WATTS_PER_METER_KELVIN, &
                    symbol = "cal/(s cm K)")
    type(thermal_conductivity_simple_unit_t), parameter :: WATTS_PER_CENTIMETER_KELVIN = &
            thermal_conductivity_simple_unit_t( &
                    conversion_factor = WATTS_PER_CENTIMETER_KELVIN_PER_WATTS_PER_METER_KELVIN, &
                    symbol = "W/(cm K)")
    type(thermal_conductivity_simple_unit_t), parameter :: WATTS_PER_METER_KELVIN = &
            thermal_conductivity_simple_unit_t( &
                    conversion_factor = 1.0d0, &
                    symbol = "W/(m K)")
    type(thermal_conductivity_simple_unit_t), parameter :: BTU_PER_HOUR_FEET_FAHRENHEIT = &
            thermal_conductivity_simple_unit_t( &
                    conversion_factor = BTU_PER_HOUR_FEET_RANKINE_PER_WATTS_PER_METER_KELVIN, &
                    symbol = "BTU/(h ft F)")
    type(thermal_conductivity_simple_unit_t), parameter :: BTU_PER_HOUR_FEET_RANKINE = &
            thermal_conductivity_simple_unit_t( &
                    conversion_factor = BTU_PER_HOUR_FEET_RANKINE_PER_WATTS_PER_METER_KELVIN, &
                    symbol = "BTU/(h ft R)")

    type(thermal_conductivity_simple_unit_t) :: DEFAULT_OUTPUT_UNITS = WATTS_PER_METER_KELVIN

    type(thermal_conductivity_simple_unit_t), parameter :: PROVIDED_UNITS(*) = &
            [ CALORIES_PER_SECOND_CENTIMETER_KELVIN &
            , WATTS_PER_CENTIMETER_KELVIN &
            , WATTS_PER_METER_KELVIN &
            , BTU_PER_HOUR_FEET_RANKINE &
            , BTU_PER_HOUR_FEET_FAHRENHEIT &
            ]

    character(len=*), parameter :: MODULE_NAME = "quaff_thermal_conductivity_m"
contains
    function parse_thermal_conductivity_c(string) result(fallible_thermal_conductivity)
        character(len=*), intent(in) :: string
        type(fallible_thermal_conductivity_t) :: fallible_thermal_conductivity

        fallible_thermal_conductivity = fallible_thermal_conductivity_t( &
                parse_thermal_conductivity(var_str(string), PROVIDED_UNITS), &
                module_t(MODULE_NAME), &
                procedure_t("parse_thermal_conductivity_c"))
    end function

    function parse_thermal_conductivity_s(string) result(fallible_thermal_conductivity)
        type(varying_string), intent(in) :: string
        type(fallible_thermal_conductivity_t) :: fallible_thermal_conductivity

        fallible_thermal_conductivity = fallible_thermal_conductivity_t( &
                parse_thermal_conductivity(string, PROVIDED_UNITS), &
                module_t(MODULE_NAME), &
                procedure_t("parse_thermal_conductivity_s"))
    end function

    function parse_thermal_conductivity_with_units_c( &
            string, units) result(fallible_thermal_conductivity)
        character(len=*), intent(in) :: string
        class(thermal_conductivity_unit_t), intent(in) :: units(:)
        type(fallible_thermal_conductivity_t) :: fallible_thermal_conductivity

        fallible_thermal_conductivity = fallible_thermal_conductivity_t( &
                parse_thermal_conductivity(var_str(string), units), &
                module_t(MODULE_NAME), &
                procedure_t("parse_thermal_conductivity_with_units_c"))
    end function

    function parse_thermal_conductivity_with_units_s( &
            string, units) result(fallible_thermal_conductivity)
        type(varying_string), intent(in) :: string
        class(thermal_conductivity_unit_t), intent(in) :: units(:)
        type(fallible_thermal_conductivity_t) :: fallible_thermal_conductivity

        type(fallible_thermal_conductivity_t) :: all_attempts(size(units))
        integer :: i

        do i = 1, size(units)
            all_attempts(i) = units(i)%parse_as(string)
            if (.not. all_attempts(i)%failed()) then
                fallible_thermal_conductivity = all_attempts(i)
                return
            end if
        end do
        fallible_thermal_conductivity = fallible_thermal_conductivity_t(error_list_t( &
                all_attempts%errors(), &
                module_t(MODULE_NAME), &
                procedure_t("parse_thermal_conductivity_with_units_s")))
    end function

    elemental function from_units(value_, units) result(thermal_conductivity)
        double precision, intent(in) :: value_
        class(thermal_conductivity_unit_t), intent(in) :: units
        type(thermal_conductivity_t) :: thermal_conductivity

        thermal_conductivity%watts_per_meter_kelvin = value_ / units%conversion_factor
    end function

    elemental function to_units(self, units) result(thermal_conductivity)
        class(thermal_conductivity_t), intent(in) :: self
        class(thermal_conductivity_unit_t), intent(in) :: units
        double precision :: thermal_conductivity

        thermal_conductivity = self%watts_per_meter_kelvin * units%conversion_factor
    end function

    elemental function double_times_thermal_conductivity( &
            multiplier, thermal_conductivity) result(new_thermal_conductivity)
        double precision, intent(in) :: multiplier
        class(thermal_conductivity_t), intent(in) :: thermal_conductivity
        type(thermal_conductivity_t) :: new_thermal_conductivity

        new_thermal_conductivity%watts_per_meter_kelvin = &
                multiplier * thermal_conductivity%watts_per_meter_kelvin
    end function

    elemental function integer_times_thermal_conductivity( &
            multiplier, thermal_conductivity) result(new_thermal_conductivity)
        integer, intent(in) :: multiplier
        class(thermal_conductivity_t), intent(in) :: thermal_conductivity
        type(thermal_conductivity_t) :: new_thermal_conductivity

        new_thermal_conductivity%watts_per_meter_kelvin = &
                dble(multiplier) * thermal_conductivity%watts_per_meter_kelvin
    end function

    elemental function thermal_conductivity_times_double( &
            thermal_conductivity, multiplier) result(new_thermal_conductivity)
        class(thermal_conductivity_t), intent(in) :: thermal_conductivity
        double precision, intent(in) :: multiplier
        type(thermal_conductivity_t) :: new_thermal_conductivity

        new_thermal_conductivity%watts_per_meter_kelvin = &
                thermal_conductivity%watts_per_meter_kelvin * multiplier
    end function

    elemental function thermal_conductivity_times_integer( &
            thermal_conductivity, multiplier) result(new_thermal_conductivity)
        class(thermal_conductivity_t), intent(in) :: thermal_conductivity
        integer, intent(in) :: multiplier
        type(thermal_conductivity_t) :: new_thermal_conductivity

        new_thermal_conductivity%watts_per_meter_kelvin = &
                thermal_conductivity%watts_per_meter_kelvin * dble(multiplier)
    end function

    elemental function thermal_conductivity_divided_by_double( &
            thermal_conductivity, divisor) result(new_thermal_conductivity)
        class(thermal_conductivity_t), intent(in) :: thermal_conductivity
        double precision, intent(in) :: divisor
        type(thermal_conductivity_t) :: new_thermal_conductivity

        new_thermal_conductivity%watts_per_meter_kelvin = &
                thermal_conductivity%watts_per_meter_kelvin / divisor
    end function

    elemental function thermal_conductivity_divided_by_integer( &
            thermal_conductivity, divisor) result(new_thermal_conductivity)
        class(thermal_conductivity_t), intent(in) :: thermal_conductivity
        integer, intent(in) :: divisor
        type(thermal_conductivity_t) :: new_thermal_conductivity

        new_thermal_conductivity%watts_per_meter_kelvin = &
                thermal_conductivity%watts_per_meter_kelvin / dble(divisor)
    end function

    elemental function thermal_conductivity_divided_by_thermal_conductivity( &
            numerator, denomenator) result(ratio)
        class(thermal_conductivity_t), intent(in) :: numerator
        type(thermal_conductivity_t), intent(in) :: denomenator
        double precision :: ratio

        ratio = numerator%watts_per_meter_kelvin / denomenator%watts_per_meter_kelvin
    end function

    elemental function thermal_conductivity_plus_thermal_conductivity( &
            lhs, rhs) result(sum_)
        class(thermal_conductivity_t), intent(in) :: lhs
        type(thermal_conductivity_t), intent(in) :: rhs
        type(thermal_conductivity_t) :: sum_

        sum_%watts_per_meter_kelvin = lhs%watts_per_meter_kelvin + rhs%watts_per_meter_kelvin
    end function

    elemental function thermal_conductivity_minus_thermal_conductivity( &
            lhs, rhs) result(difference)
        class(thermal_conductivity_t), intent(in) :: lhs
        type(thermal_conductivity_t), intent(in) :: rhs
        type(thermal_conductivity_t) :: difference

        difference%watts_per_meter_kelvin = lhs%watts_per_meter_kelvin - rhs%watts_per_meter_kelvin
    end function

    pure function sum_thermal_conductivity(thermal_conductivitys) result(sum_)
        type(thermal_conductivity_t), intent(in) :: thermal_conductivitys(:)
        type(thermal_conductivity_t) :: sum_

        sum_%watts_per_meter_kelvin = sum(thermal_conductivitys%watts_per_meter_kelvin)
    end function

    elemental function greater_than(lhs, rhs)
        class(thermal_conductivity_t), intent(in) :: lhs
        type(thermal_conductivity_t), intent(in) :: rhs
        logical :: greater_than

        greater_than = lhs%watts_per_meter_kelvin > rhs%watts_per_meter_kelvin
    end function

    elemental function less_than(lhs, rhs)
        class(thermal_conductivity_t), intent(in) :: lhs
        type(thermal_conductivity_t), intent(in) :: rhs
        logical :: less_than

        less_than = lhs%watts_per_meter_kelvin < rhs%watts_per_meter_kelvin
    end function

    elemental function greater_than_or_equal(lhs, rhs)
        class(thermal_conductivity_t), intent(in) :: lhs
        type(thermal_conductivity_t), intent(in) :: rhs
        logical :: greater_than_or_equal

        greater_than_or_equal = lhs%watts_per_meter_kelvin >= rhs%watts_per_meter_kelvin
    end function

    elemental function less_than_or_equal(lhs, rhs)
        class(thermal_conductivity_t), intent(in) :: lhs
        type(thermal_conductivity_t), intent(in) :: rhs
        logical :: less_than_or_equal

        less_than_or_equal = lhs%watts_per_meter_kelvin <= rhs%watts_per_meter_kelvin
    end function

    elemental function equal_(lhs, rhs)
        class(thermal_conductivity_t), intent(in) :: lhs
        type(thermal_conductivity_t), intent(in) :: rhs
        logical :: equal_

        equal_ = lhs%watts_per_meter_kelvin .safeEq. rhs%watts_per_meter_kelvin
    end function

    elemental function equal_within_absolute_(lhs, rhs, within)
        class(thermal_conductivity_t), intent(in) :: lhs
        type(thermal_conductivity_t), intent(in) :: rhs
        type(thermal_conductivity_t), intent(in) :: within
        logical :: equal_within_absolute_

        equal_within_absolute_ = equal_within_absolute( &
                lhs%watts_per_meter_kelvin, rhs%watts_per_meter_kelvin, within%watts_per_meter_kelvin)
    end function

    elemental function equal_within_relative_(lhs, rhs, within)
        class(thermal_conductivity_t), intent(in) :: lhs
        type(thermal_conductivity_t), intent(in) :: rhs
        double precision, intent(in) :: within
        logical :: equal_within_relative_

        equal_within_relative_ = equal_within_relative( &
                lhs%watts_per_meter_kelvin, rhs%watts_per_meter_kelvin, within)
    end function

    elemental function not_equal(lhs, rhs)
        class(thermal_conductivity_t), intent(in) :: lhs
        type(thermal_conductivity_t), intent(in) :: rhs
        logical :: not_equal

        not_equal = .not. lhs == rhs
    end function

    elemental function to_string_full_precision(self) result(string)
        class(thermal_conductivity_t), intent(in) :: self
        type(varying_string) :: string

        string = self%to_string_in(DEFAULT_OUTPUT_UNITS)
    end function

    elemental function to_string_with_precision(self, significant_digits) result(string)
        class(thermal_conductivity_t), intent(in) :: self
        integer, intent(in) :: significant_digits
        type(varying_string) :: string

        string = self%to_string_in(DEFAULT_OUTPUT_UNITS, significant_digits)
    end function

    elemental function to_string_in_full_precision(self, units) result(string)
        class(thermal_conductivity_t), intent(in) :: self
        class(thermal_conductivity_unit_t), intent(in) :: units
        type(varying_string) :: string

        string = units%to_string(to_string(self.in.units))
    end function

    elemental function to_string_in_with_precision( &
            self, units, significant_digits) result(string)
        class(thermal_conductivity_t), intent(in) :: self
        class(thermal_conductivity_unit_t), intent(in) :: units
        integer, intent(in) :: significant_digits
        type(varying_string) :: string

        string = units%to_string(to_string(self.in.units, significant_digits))
    end function

    function fallible_thermal_conductivity_from_thermal_conductivity( &
            thermal_conductivity) result(fallible_thermal_conductivity)
        type(thermal_conductivity_t), intent(in) :: thermal_conductivity
        type(fallible_thermal_conductivity_t) :: fallible_thermal_conductivity

        fallible_thermal_conductivity%thermal_conductivity_ = thermal_conductivity
    end function

    function fallible_thermal_conductivity_from_errors( &
            errors) result(fallible_thermal_conductivity)
        type(error_list_t), intent(in) :: errors
        type(fallible_thermal_conductivity_t) :: fallible_thermal_conductivity

        fallible_thermal_conductivity%errors_ = errors
    end function

    function fallible_therm_conduct_from_fallible_therm_conduct( &
            fallible_thermal_conductivity, &
            module_, &
            procedure_) &
            result(new_fallible_thermal_conductivity)
        type(fallible_thermal_conductivity_t), intent(in) :: fallible_thermal_conductivity
        type(module_t), intent(in) :: module_
        type(procedure_t), intent(in) :: procedure_
        type(fallible_thermal_conductivity_t) :: new_fallible_thermal_conductivity

        if (fallible_thermal_conductivity%failed()) then
            new_fallible_thermal_conductivity%errors_ = error_list_t( &
                    fallible_thermal_conductivity%errors_, module_, procedure_)
        else
            new_fallible_thermal_conductivity%thermal_conductivity_ = fallible_thermal_conductivity%thermal_conductivity_
        end if
    end function

    elemental function fallible_thermal_conductivity_failed( &
            self) result(failed)
        class(fallible_thermal_conductivity_t), intent(in) :: self
        logical :: failed

        failed = self%errors_%has_any()
    end function

    elemental function fallible_thermal_conductivity_thermal_conductivity( &
            self) result(thermal_conductivity)
        class(fallible_thermal_conductivity_t), intent(in) :: self
        type(thermal_conductivity_t) :: thermal_conductivity

        thermal_conductivity = self%thermal_conductivity_
    end function

    impure elemental function fallible_thermal_conductivity_errors( &
            self) result(errors)
        class(fallible_thermal_conductivity_t), intent(in) :: self
        type(error_list_t) :: errors

        errors = self%errors_
    end function

    function fallible_thermal_conductivity_unit_from_unit( &
            unit) result(fallible_thermal_conductivity_unit)
        class(thermal_conductivity_unit_t), intent(in) :: unit
        type(fallible_thermal_conductivity_unit_t) :: fallible_thermal_conductivity_unit

        allocate(fallible_thermal_conductivity_unit%unit_, source = unit)
    end function

    function fallible_thermal_conductivity_unit_from_errors( &
            errors) result(fallible_thermal_conductivity_unit)
        type(error_list_t), intent(in) :: errors
        type(fallible_thermal_conductivity_unit_t) :: fallible_thermal_conductivity_unit

        fallible_thermal_conductivity_unit%errors_ = errors
    end function

    function fallible_therm_conduct_unit_from_fallible_therm_conduct_unit( &
            fallible_thermal_conductivity_unit, &
            module_, &
            procedure_) &
            result(new_fallible_thermal_conductivity_unit)
        type(fallible_thermal_conductivity_unit_t), intent(in) :: fallible_thermal_conductivity_unit
        type(module_t), intent(in) :: module_
        type(procedure_t), intent(in) :: procedure_
        type(fallible_thermal_conductivity_unit_t) :: new_fallible_thermal_conductivity_unit

        if (fallible_thermal_conductivity_unit%failed()) then
            new_fallible_thermal_conductivity_unit%errors_ = error_list_t( &
                    fallible_thermal_conductivity_unit%errors_, module_, procedure_)
        else
            allocate(new_fallible_thermal_conductivity_unit%unit_, source = &
                    fallible_thermal_conductivity_unit%unit_)
        end if
    end function

    elemental function fallible_thermal_conductivity_unit_failed( &
            self) result(failed)
        class(fallible_thermal_conductivity_unit_t), intent(in) :: self
        logical :: failed

        failed = self%errors_%has_any()
    end function

    function fallible_thermal_conductivity_unit_unit( &
            self) result(unit)
        class(fallible_thermal_conductivity_unit_t), intent(in) :: self
        class(thermal_conductivity_unit_t), allocatable :: unit

        allocate(unit, source = self%unit_)
    end function

    impure elemental function fallible_thermal_conductivity_unit_errors( &
            self) result(errors)
        class(fallible_thermal_conductivity_unit_t), intent(in) :: self
        type(error_list_t) :: errors

        errors = self%errors_
    end function

    elemental function simple_unit_to_string(self) result(string)
        class(thermal_conductivity_simple_unit_t), intent(in) :: self
        type(varying_string) :: string

        string = trim(self%symbol)
    end function

    pure function simple_value_to_string(self, value_) result(string)
        class(thermal_conductivity_simple_unit_t), intent(in) :: self
        type(varying_string), intent(in) :: value_
        type(varying_string) :: string

        string = value_ // " " // self%to_string()
    end function

    function simple_parse_as(self, string) result(fallible_thermal_conductivity)
        class(thermal_conductivity_simple_unit_t), intent(in) :: self
        type(varying_string), intent(in) :: string
        type(fallible_thermal_conductivity_t) :: fallible_thermal_conductivity

        type(parse_result_t) :: parse_result

        parse_result = parse_with(the_parser, string)
        if (parse_result%ok) then
            select type (the_number => parse_result%parsed)
            type is (parsed_rational_t)
                fallible_thermal_conductivity = fallible_thermal_conductivity_t(the_number%value_.unit.self)
            end select
        else
            fallible_thermal_conductivity = fallible_thermal_conductivity_t(error_list_t(fatal_t( &
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

    function parse_thermal_conductivity_unit_c(string) result(fallible_thermal_conductivity_unit)
        character(len=*), intent(in) :: string
        type(fallible_thermal_conductivity_unit_t) :: fallible_thermal_conductivity_unit

        fallible_thermal_conductivity_unit = fallible_thermal_conductivity_unit_t( &
                parse_thermal_conductivity_unit(var_str(string), PROVIDED_UNITS), &
                module_t(MODULE_NAME), &
                procedure_t("parse_thermal_conductivity_unit_c"))
    end function

    function parse_thermal_conductivity_unit_s(string) result(fallible_thermal_conductivity_unit)
        type(varying_string), intent(in) :: string
        type(fallible_thermal_conductivity_unit_t) :: fallible_thermal_conductivity_unit

        fallible_thermal_conductivity_unit = fallible_thermal_conductivity_unit_t( &
                parse_thermal_conductivity_unit(string, PROVIDED_UNITS), &
                module_t(MODULE_NAME), &
                procedure_t("parse_thermal_conductivity_unit_s"))
    end function

    function parse_thermal_conductivity_unit_with_units_c( &
            string, units) result(fallible_thermal_conductivity_unit)
        character(len=*), intent(in) :: string
        class(thermal_conductivity_unit_t), intent(in) :: units(:)
        type(fallible_thermal_conductivity_unit_t) :: fallible_thermal_conductivity_unit

        fallible_thermal_conductivity_unit = fallible_thermal_conductivity_unit_t( &
                parse_thermal_conductivity_unit(var_str(string), units), &
                module_t(MODULE_NAME), &
                procedure_t("parse_thermal_conductivity_unit_with_units_c"))
    end function

    function parse_thermal_conductivity_unit_with_units_s( &
            string, units) result(fallible_thermal_conductivity_unit)
        type(varying_string), intent(in) :: string
        class(thermal_conductivity_unit_t), intent(in) :: units(:)
        type(fallible_thermal_conductivity_unit_t) :: fallible_thermal_conductivity_unit

        integer :: i
        type(varying_string) :: unit_strings(size(units))

        do i = 1, size(units)
            if (string == units(i)%to_string()) then
                fallible_thermal_conductivity_unit = fallible_thermal_conductivity_unit_t(units(i))
                return
            end if
        end do
        do i = 1, size(units)
            unit_strings(i) = units(i)%to_string()
        end do
        fallible_thermal_conductivity_unit = fallible_thermal_conductivity_unit_t(error_list_t(fatal_t( &
                UNKNOWN_UNIT, &
                module_t(MODULE_NAME), &
                procedure_t("parse_thermal_conductivity_unit_with_units_s"), &
                '"' // string // '", known units: [' // join(unit_strings, ', ') // ']')))
    end function
end module
