module convective_heat_transfer_m
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
    use quaff_utilities_m, only: &
            operator(.safeEq.), &
            equal_within_absolute, &
            equal_within_relative, &
            parse_space, &
            PARSE_ERROR, &
            UNKNOWN_UNIT
    use quaff_conversion_factors_m, only: &
                    BTU_PER_HR_SQ_FT_RANKINE_PER_WATTS_PER_SQUARE_METER_KELVIN
    use strff, only: join, to_string

    implicit none
    private
    public :: &
            convective_heat_transfer_t, &
            fallible_convective_heat_transfer_t, &
            convective_heat_transfer_unit_t, &
            fallible_convective_heat_transfer_unit_t, &
            convective_heat_transfer_simple_unit_t, &
            operator(.unit.), &
            parse_convective_heat_transfer, &
            parse_convective_heat_transfer_unit, &
            sum, &
            DEFAULT_OUTPUT_UNITS, &
            PROVIDED_UNITS, &
            BTU_PER_HOUR_SQUARE_FEET_RANKINE, &
            BTU_PER_HOUR_SQUARE_FEET_FAHRENHEIT, &
            WATTS_PER_SQUARE_METER_KELVIN

    type :: convective_heat_transfer_t
        double precision :: watts_per_square_meter_kelvin
    contains
        private
        procedure :: to_units
        generic, public :: operator(.in.) => to_units
        procedure, pass(convective_heat_transfer) :: double_times_convective_heat_transfer
        procedure, pass(convective_heat_transfer) :: integer_times_convective_heat_transfer
        procedure :: convective_heat_transfer_times_double
        procedure :: convective_heat_transfer_times_integer
        generic, public :: operator(*) => &
                double_times_convective_heat_transfer, &
                integer_times_convective_heat_transfer, &
                convective_heat_transfer_times_double, &
                convective_heat_transfer_times_integer
        procedure :: convective_heat_transfer_divided_by_double
        procedure :: convective_heat_transfer_divided_by_integer
        procedure :: convective_heat_transfer_divided_by_convective_heat_transfer
        generic, public :: operator(/) => &
                convective_heat_transfer_divided_by_double, &
                convective_heat_transfer_divided_by_integer, &
                convective_heat_transfer_divided_by_convective_heat_transfer
        procedure :: convective_heat_transfer_plus_convective_heat_transfer
        generic, public :: operator(+) => convective_heat_transfer_plus_convective_heat_transfer
        procedure :: convective_heat_transfer_minus_convective_heat_transfer
        generic, public :: operator(-) => convective_heat_transfer_minus_convective_heat_transfer
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

    type :: fallible_convective_heat_transfer_t
        private
        type(convective_heat_transfer_t) :: convective_heat_transfer_
        type(error_list_t) :: errors_
    contains
        private
        procedure, public :: failed => fallible_convective_heat_transfer_failed
        procedure, public :: convective_heat_transfer => fallible_convective_heat_transfer_convective_heat_transfer
        procedure, public :: errors => fallible_convective_heat_transfer_errors
    end type

    type, abstract :: convective_heat_transfer_unit_t
        double precision :: conversion_factor
    contains
        procedure(unit_to_string_i), deferred :: unit_to_string
        procedure(value_to_string_i), deferred :: value_to_string
        generic :: to_string => unit_to_string, value_to_string
        procedure(parse_as_i), deferred :: parse_as
    end type

    type :: fallible_convective_heat_transfer_unit_t
        private
        class(convective_heat_transfer_unit_t), allocatable :: unit_
        type(error_list_t) :: errors_
    contains
        private
        procedure, public :: failed => fallible_convective_heat_transfer_unit_failed
        procedure, public :: unit => fallible_convective_heat_transfer_unit_unit
        procedure, public :: errors => fallible_convective_heat_transfer_unit_errors
    end type

    type, extends(convective_heat_transfer_unit_t) :: convective_heat_transfer_simple_unit_t
        character(len=20) :: symbol
    contains
        procedure :: unit_to_string => simple_unit_to_string
        procedure :: value_to_string => simple_value_to_string
        procedure :: parse_as => simple_parse_as
    end type

    abstract interface
        elemental function unit_to_string_i(self) result(string)
            import :: convective_heat_transfer_unit_t, varying_string

            implicit none

            class(convective_heat_transfer_unit_t), intent(in) :: self
            type(varying_string) :: string
        end function

        pure function value_to_string_i(self, value_) result(string)
            import :: convective_heat_transfer_unit_t, varying_string

            implicit none

            class(convective_heat_transfer_unit_t), intent(in) :: self
            type(varying_string), intent(in) :: value_
            type(varying_string) :: string
        end function

        function parse_as_i(self, string) result(fallible_convective_heat_transfer)
            import :: convective_heat_transfer_unit_t, fallible_convective_heat_transfer_t, varying_string

            implicit none

            class(convective_heat_transfer_unit_t), intent(in) :: self
            type(varying_string), intent(in) :: string
            type(fallible_convective_heat_transfer_t) :: fallible_convective_heat_transfer
        end function
    end interface

    interface operator(.unit.)
        module procedure from_units
    end interface

    interface fallible_convective_heat_transfer_t
        module procedure fallible_convective_heat_transfer_from_convective_heat_transfer
        module procedure fallible_convective_heat_transfer_from_errors
        module procedure fallible_convective_heat_from_fallible_convective_heat
    end interface

    interface fallible_convective_heat_transfer_unit_t
        module procedure fallible_convective_heat_transfer_unit_from_unit
        module procedure fallible_convective_heat_transfer_unit_from_errors
        module procedure fallible_convect_heat_unit_from_fallible_convect_heat_unit
    end interface

    interface parse_convective_heat_transfer
        module procedure parse_convective_heat_transfer_c
        module procedure parse_convective_heat_transfer_s
        module procedure parse_convective_heat_transfer_with_units_c
        module procedure parse_convective_heat_transfer_with_units_s
    end interface

    interface parse_convective_heat_transfer_unit
        module procedure parse_convective_heat_transfer_unit_c
        module procedure parse_convective_heat_transfer_unit_s
        module procedure parse_convective_heat_transfer_unit_with_units_c
        module procedure parse_convective_heat_transfer_unit_with_units_s
    end interface

    interface sum
        module procedure sum_convective_heat_transfer
    end interface

    type(convective_heat_transfer_simple_unit_t), parameter :: BTU_PER_HOUR_SQUARE_FEET_RANKINE = &
            convective_heat_transfer_simple_unit_t( &
                    conversion_factor = BTU_PER_HR_SQ_FT_RANKINE_PER_WATTS_PER_SQUARE_METER_KELVIN, &
                    symbol = "BTU/(hr ft^2 R)")
    type(convective_heat_transfer_simple_unit_t), parameter :: BTU_PER_HOUR_SQUARE_FEET_FAHRENHEIT = &
            convective_heat_transfer_simple_unit_t( &
                    conversion_factor = BTU_PER_HR_SQ_FT_RANKINE_PER_WATTS_PER_SQUARE_METER_KELVIN, &
                    symbol = "BTU/(hr ft^2 F)")
    type(convective_heat_transfer_simple_unit_t), parameter :: WATTS_PER_SQUARE_METER_KELVIN = &
            convective_heat_transfer_simple_unit_t( &
                    conversion_factor = 1.0d0, &
                    symbol = "W/(m^2 K)")

    type(convective_heat_transfer_simple_unit_t) :: DEFAULT_OUTPUT_UNITS = WATTS_PER_SQUARE_METER_KELVIN

    type(convective_heat_transfer_simple_unit_t), parameter :: PROVIDED_UNITS(*) = &
            [BTU_PER_HOUR_SQUARE_FEET_RANKINE &
            , WATTS_PER_SQUARE_METER_KELVIN &
            , BTU_PER_HOUR_SQUARE_FEET_FAHRENHEIT &
            ]

    character(len=*), parameter :: MODULE_NAME = "convective_heat_transfer_m"
contains
    function parse_convective_heat_transfer_c(string) result(fallible_convective_heat_transfer)
        character(len=*), intent(in) :: string
        type(fallible_convective_heat_transfer_t) :: fallible_convective_heat_transfer

        fallible_convective_heat_transfer = fallible_convective_heat_transfer_t( &
                parse_convective_heat_transfer(var_str(string), PROVIDED_UNITS), &
                module_t(MODULE_NAME), &
                procedure_t("parse_convective_heat_transfer_c"))
    end function

    function parse_convective_heat_transfer_s(string) result(fallible_convective_heat_transfer)
        type(varying_string), intent(in) :: string
        type(fallible_convective_heat_transfer_t) :: fallible_convective_heat_transfer

        fallible_convective_heat_transfer = fallible_convective_heat_transfer_t( &
                parse_convective_heat_transfer(string, PROVIDED_UNITS), &
                module_t(MODULE_NAME), &
                procedure_t("parse_convective_heat_transfer_s"))
    end function

    function parse_convective_heat_transfer_with_units_c( &
            string, units) result(fallible_convective_heat_transfer)
        character(len=*), intent(in) :: string
        class(convective_heat_transfer_unit_t), intent(in) :: units(:)
        type(fallible_convective_heat_transfer_t) :: fallible_convective_heat_transfer

        fallible_convective_heat_transfer = fallible_convective_heat_transfer_t( &
                parse_convective_heat_transfer(var_str(string), units), &
                module_t(MODULE_NAME), &
                procedure_t("parse_convective_heat_transfer_with_units_c"))
    end function

    function parse_convective_heat_transfer_with_units_s( &
            string, units) result(fallible_convective_heat_transfer)
        type(varying_string), intent(in) :: string
        class(convective_heat_transfer_unit_t), intent(in) :: units(:)
        type(fallible_convective_heat_transfer_t) :: fallible_convective_heat_transfer

        type(fallible_convective_heat_transfer_t) :: all_attempts(size(units))
        integer :: i

        do i = 1, size(units)
            all_attempts(i) = units(i)%parse_as(string)
            if (.not. all_attempts(i)%failed()) then
                fallible_convective_heat_transfer = all_attempts(i)
                return
            end if
        end do
        fallible_convective_heat_transfer = fallible_convective_heat_transfer_t(error_list_t( &
                all_attempts%errors(), &
                module_t(MODULE_NAME), &
                procedure_t("parse_convective_heat_transfer_with_units_s")))
    end function

    elemental function from_units(value_, units) result(convective_heat_transfer)
        double precision, intent(in) :: value_
        class(convective_heat_transfer_unit_t), intent(in) :: units
        type(convective_heat_transfer_t) :: convective_heat_transfer

        convective_heat_transfer%watts_per_square_meter_kelvin = value_ / units%conversion_factor
    end function

    elemental function to_units(self, units) result(convective_heat_transfer)
        class(convective_heat_transfer_t), intent(in) :: self
        class(convective_heat_transfer_unit_t), intent(in) :: units
        double precision :: convective_heat_transfer

        convective_heat_transfer = self%watts_per_square_meter_kelvin * units%conversion_factor
    end function

    elemental function double_times_convective_heat_transfer( &
            multiplier, convective_heat_transfer) result(new_convective_heat_transfer)
        double precision, intent(in) :: multiplier
        class(convective_heat_transfer_t), intent(in) :: convective_heat_transfer
        type(convective_heat_transfer_t) :: new_convective_heat_transfer

        new_convective_heat_transfer%watts_per_square_meter_kelvin = &
                multiplier * convective_heat_transfer%watts_per_square_meter_kelvin
    end function

    elemental function integer_times_convective_heat_transfer( &
            multiplier, convective_heat_transfer) result(new_convective_heat_transfer)
        integer, intent(in) :: multiplier
        class(convective_heat_transfer_t), intent(in) :: convective_heat_transfer
        type(convective_heat_transfer_t) :: new_convective_heat_transfer

        new_convective_heat_transfer%watts_per_square_meter_kelvin = &
                dble(multiplier) * convective_heat_transfer%watts_per_square_meter_kelvin
    end function

    elemental function convective_heat_transfer_times_double( &
            convective_heat_transfer, multiplier) result(new_convective_heat_transfer)
        class(convective_heat_transfer_t), intent(in) :: convective_heat_transfer
        double precision, intent(in) :: multiplier
        type(convective_heat_transfer_t) :: new_convective_heat_transfer

        new_convective_heat_transfer%watts_per_square_meter_kelvin = &
                convective_heat_transfer%watts_per_square_meter_kelvin * multiplier
    end function

    elemental function convective_heat_transfer_times_integer( &
            convective_heat_transfer, multiplier) result(new_convective_heat_transfer)
        class(convective_heat_transfer_t), intent(in) :: convective_heat_transfer
        integer, intent(in) :: multiplier
        type(convective_heat_transfer_t) :: new_convective_heat_transfer

        new_convective_heat_transfer%watts_per_square_meter_kelvin = &
                convective_heat_transfer%watts_per_square_meter_kelvin * dble(multiplier)
    end function

    elemental function convective_heat_transfer_divided_by_double( &
            convective_heat_transfer, divisor) result(new_convective_heat_transfer)
        class(convective_heat_transfer_t), intent(in) :: convective_heat_transfer
        double precision, intent(in) :: divisor
        type(convective_heat_transfer_t) :: new_convective_heat_transfer

        new_convective_heat_transfer%watts_per_square_meter_kelvin = &
                convective_heat_transfer%watts_per_square_meter_kelvin / divisor
    end function

    elemental function convective_heat_transfer_divided_by_integer( &
            convective_heat_transfer, divisor) result(new_convective_heat_transfer)
        class(convective_heat_transfer_t), intent(in) :: convective_heat_transfer
        integer, intent(in) :: divisor
        type(convective_heat_transfer_t) :: new_convective_heat_transfer

        new_convective_heat_transfer%watts_per_square_meter_kelvin = &
                convective_heat_transfer%watts_per_square_meter_kelvin / dble(divisor)
    end function

    elemental function convective_heat_transfer_divided_by_convective_heat_transfer( &
            numerator, denomenator) result(ratio)
        class(convective_heat_transfer_t), intent(in) :: numerator
        type(convective_heat_transfer_t), intent(in) :: denomenator
        double precision :: ratio

        ratio = numerator%watts_per_square_meter_kelvin / denomenator%watts_per_square_meter_kelvin
    end function

    elemental function convective_heat_transfer_plus_convective_heat_transfer( &
            lhs, rhs) result(sum_)
        class(convective_heat_transfer_t), intent(in) :: lhs
        type(convective_heat_transfer_t), intent(in) :: rhs
        type(convective_heat_transfer_t) :: sum_

        sum_%watts_per_square_meter_kelvin = lhs%watts_per_square_meter_kelvin + rhs%watts_per_square_meter_kelvin
    end function

    elemental function convective_heat_transfer_minus_convective_heat_transfer( &
            lhs, rhs) result(difference)
        class(convective_heat_transfer_t), intent(in) :: lhs
        type(convective_heat_transfer_t), intent(in) :: rhs
        type(convective_heat_transfer_t) :: difference

        difference%watts_per_square_meter_kelvin = lhs%watts_per_square_meter_kelvin - rhs%watts_per_square_meter_kelvin
    end function

    pure function sum_convective_heat_transfer(convective_heat_transfers) result(sum_)
        type(convective_heat_transfer_t), intent(in) :: convective_heat_transfers(:)
        type(convective_heat_transfer_t) :: sum_

        sum_%watts_per_square_meter_kelvin = sum(convective_heat_transfers%watts_per_square_meter_kelvin)
    end function

    elemental function greater_than(lhs, rhs)
        class(convective_heat_transfer_t), intent(in) :: lhs
        type(convective_heat_transfer_t), intent(in) :: rhs
        logical :: greater_than

        greater_than = lhs%watts_per_square_meter_kelvin > rhs%watts_per_square_meter_kelvin
    end function

    elemental function less_than(lhs, rhs)
        class(convective_heat_transfer_t), intent(in) :: lhs
        type(convective_heat_transfer_t), intent(in) :: rhs
        logical :: less_than

        less_than = lhs%watts_per_square_meter_kelvin < rhs%watts_per_square_meter_kelvin
    end function

    elemental function greater_than_or_equal(lhs, rhs)
        class(convective_heat_transfer_t), intent(in) :: lhs
        type(convective_heat_transfer_t), intent(in) :: rhs
        logical :: greater_than_or_equal

        greater_than_or_equal = lhs%watts_per_square_meter_kelvin >= rhs%watts_per_square_meter_kelvin
    end function

    elemental function less_than_or_equal(lhs, rhs)
        class(convective_heat_transfer_t), intent(in) :: lhs
        type(convective_heat_transfer_t), intent(in) :: rhs
        logical :: less_than_or_equal

        less_than_or_equal = lhs%watts_per_square_meter_kelvin <= rhs%watts_per_square_meter_kelvin
    end function

    elemental function equal_(lhs, rhs)
        class(convective_heat_transfer_t), intent(in) :: lhs
        type(convective_heat_transfer_t), intent(in) :: rhs
        logical :: equal_

        equal_ = lhs%watts_per_square_meter_kelvin .safeEq. rhs%watts_per_square_meter_kelvin
    end function

    elemental function equal_within_absolute_(lhs, rhs, within)
        class(convective_heat_transfer_t), intent(in) :: lhs
        type(convective_heat_transfer_t), intent(in) :: rhs
        type(convective_heat_transfer_t), intent(in) :: within
        logical :: equal_within_absolute_

        equal_within_absolute_ = equal_within_absolute( &
                lhs%watts_per_square_meter_kelvin, rhs%watts_per_square_meter_kelvin, within%watts_per_square_meter_kelvin)
    end function

    elemental function equal_within_relative_(lhs, rhs, within)
        class(convective_heat_transfer_t), intent(in) :: lhs
        type(convective_heat_transfer_t), intent(in) :: rhs
        double precision, intent(in) :: within
        logical :: equal_within_relative_

        equal_within_relative_ = equal_within_relative( &
                lhs%watts_per_square_meter_kelvin, rhs%watts_per_square_meter_kelvin, within)
    end function

    elemental function not_equal(lhs, rhs)
        class(convective_heat_transfer_t), intent(in) :: lhs
        type(convective_heat_transfer_t), intent(in) :: rhs
        logical :: not_equal

        not_equal = .not. lhs == rhs
    end function

    elemental function to_string_full_precision(self) result(string)
        class(convective_heat_transfer_t), intent(in) :: self
        type(varying_string) :: string

        string = self%to_string_in(DEFAULT_OUTPUT_UNITS)
    end function

    elemental function to_string_with_precision(self, significant_digits) result(string)
        class(convective_heat_transfer_t), intent(in) :: self
        integer, intent(in) :: significant_digits
        type(varying_string) :: string

        string = self%to_string_in(DEFAULT_OUTPUT_UNITS, significant_digits)
    end function

    elemental function to_string_in_full_precision(self, units) result(string)
        class(convective_heat_transfer_t), intent(in) :: self
        class(convective_heat_transfer_unit_t), intent(in) :: units
        type(varying_string) :: string

        string = units%to_string(to_string(self.in.units))
    end function

    elemental function to_string_in_with_precision( &
            self, units, significant_digits) result(string)
        class(convective_heat_transfer_t), intent(in) :: self
        class(convective_heat_transfer_unit_t), intent(in) :: units
        integer, intent(in) :: significant_digits
        type(varying_string) :: string

        string = units%to_string(to_string(self.in.units, significant_digits))
    end function

    function fallible_convective_heat_transfer_from_convective_heat_transfer( &
            convective_heat_transfer) result(fallible_convective_heat_transfer)
        type(convective_heat_transfer_t), intent(in) :: convective_heat_transfer
        type(fallible_convective_heat_transfer_t) :: fallible_convective_heat_transfer

        fallible_convective_heat_transfer%convective_heat_transfer_ = convective_heat_transfer
    end function

    function fallible_convective_heat_transfer_from_errors( &
            errors) result(fallible_convective_heat_transfer)
        type(error_list_t), intent(in) :: errors
        type(fallible_convective_heat_transfer_t) :: fallible_convective_heat_transfer

        fallible_convective_heat_transfer%errors_ = errors
    end function

    function fallible_convective_heat_from_fallible_convective_heat( &
            fallible_convective_heat_transfer, &
            module_, &
            procedure_) &
            result(new_fallible_convective_heat_transfer)
        type(fallible_convective_heat_transfer_t), intent(in) :: fallible_convective_heat_transfer
        type(module_t), intent(in) :: module_
        type(procedure_t), intent(in) :: procedure_
        type(fallible_convective_heat_transfer_t) :: new_fallible_convective_heat_transfer

        if (fallible_convective_heat_transfer%failed()) then
            new_fallible_convective_heat_transfer%errors_ = error_list_t( &
                    fallible_convective_heat_transfer%errors_, module_, procedure_)
        else
            new_fallible_convective_heat_transfer%convective_heat_transfer_ = &
                    fallible_convective_heat_transfer%convective_heat_transfer_
        end if
    end function

    elemental function fallible_convective_heat_transfer_failed( &
            self) result(failed)
        class(fallible_convective_heat_transfer_t), intent(in) :: self
        logical :: failed

        failed = self%errors_%has_any()
    end function

    elemental function fallible_convective_heat_transfer_convective_heat_transfer( &
            self) result(convective_heat_transfer)
        class(fallible_convective_heat_transfer_t), intent(in) :: self
        type(convective_heat_transfer_t) :: convective_heat_transfer

        convective_heat_transfer = self%convective_heat_transfer_
    end function

    impure elemental function fallible_convective_heat_transfer_errors( &
            self) result(errors)
        class(fallible_convective_heat_transfer_t), intent(in) :: self
        type(error_list_t) :: errors

        errors = self%errors_
    end function

    function fallible_convective_heat_transfer_unit_from_unit( &
            unit) result(fallible_convective_heat_transfer_unit)
        class(convective_heat_transfer_unit_t), intent(in) :: unit
        type(fallible_convective_heat_transfer_unit_t) :: fallible_convective_heat_transfer_unit

        allocate(fallible_convective_heat_transfer_unit%unit_, source = unit)
    end function

    function fallible_convective_heat_transfer_unit_from_errors( &
            errors) result(fallible_convective_heat_transfer_unit)
        type(error_list_t), intent(in) :: errors
        type(fallible_convective_heat_transfer_unit_t) :: fallible_convective_heat_transfer_unit

        fallible_convective_heat_transfer_unit%errors_ = errors
    end function

    function fallible_convect_heat_unit_from_fallible_convect_heat_unit( &
            fallible_convective_heat_transfer_unit, &
            module_, &
            procedure_) &
            result(new_fallible_convective_heat_transfer_unit)
        type(fallible_convective_heat_transfer_unit_t), intent(in) :: fallible_convective_heat_transfer_unit
        type(module_t), intent(in) :: module_
        type(procedure_t), intent(in) :: procedure_
        type(fallible_convective_heat_transfer_unit_t) :: new_fallible_convective_heat_transfer_unit

        if (fallible_convective_heat_transfer_unit%failed()) then
            new_fallible_convective_heat_transfer_unit%errors_ = error_list_t( &
                    fallible_convective_heat_transfer_unit%errors_, module_, procedure_)
        else
            allocate(new_fallible_convective_heat_transfer_unit%unit_, source = &
                    fallible_convective_heat_transfer_unit%unit_)
        end if
    end function

    elemental function fallible_convective_heat_transfer_unit_failed( &
            self) result(failed)
        class(fallible_convective_heat_transfer_unit_t), intent(in) :: self
        logical :: failed

        failed = self%errors_%has_any()
    end function

    function fallible_convective_heat_transfer_unit_unit( &
            self) result(unit)
        class(fallible_convective_heat_transfer_unit_t), intent(in) :: self
        class(convective_heat_transfer_unit_t), allocatable :: unit

        allocate(unit, source = self%unit_)
    end function

    impure elemental function fallible_convective_heat_transfer_unit_errors( &
            self) result(errors)
        class(fallible_convective_heat_transfer_unit_t), intent(in) :: self
        type(error_list_t) :: errors

        errors = self%errors_
    end function

    elemental function simple_unit_to_string(self) result(string)
        class(convective_heat_transfer_simple_unit_t), intent(in) :: self
        type(varying_string) :: string

        string = trim(self%symbol)
    end function

    pure function simple_value_to_string(self, value_) result(string)
        class(convective_heat_transfer_simple_unit_t), intent(in) :: self
        type(varying_string), intent(in) :: value_
        type(varying_string) :: string

        string = value_ // " " // self%to_string()
    end function

    function simple_parse_as(self, string) result(fallible_convective_heat_transfer)
        class(convective_heat_transfer_simple_unit_t), intent(in) :: self
        type(varying_string), intent(in) :: string
        type(fallible_convective_heat_transfer_t) :: fallible_convective_heat_transfer

        type(parse_result_t) :: parse_result

        parse_result = parse_with(the_parser, string)
        if (parse_result%ok()) then
            select type (the_number => parse_result%parsed())
            type is (parsed_rational_t)
                fallible_convective_heat_transfer = fallible_convective_heat_transfer_t(the_number%value_().unit.self)
            end select
        else
            fallible_convective_heat_transfer = fallible_convective_heat_transfer_t(error_list_t(fatal_t( &
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

    function parse_convective_heat_transfer_unit_c(string) result(fallible_convective_heat_transfer_unit)
        character(len=*), intent(in) :: string
        type(fallible_convective_heat_transfer_unit_t) :: fallible_convective_heat_transfer_unit

        fallible_convective_heat_transfer_unit = fallible_convective_heat_transfer_unit_t( &
                parse_convective_heat_transfer_unit(var_str(string), PROVIDED_UNITS), &
                module_t(MODULE_NAME), &
                procedure_t("parse_convective_heat_transfer_unit_c"))
    end function

    function parse_convective_heat_transfer_unit_s(string) result(fallible_convective_heat_transfer_unit)
        type(varying_string), intent(in) :: string
        type(fallible_convective_heat_transfer_unit_t) :: fallible_convective_heat_transfer_unit

        fallible_convective_heat_transfer_unit = fallible_convective_heat_transfer_unit_t( &
                parse_convective_heat_transfer_unit(string, PROVIDED_UNITS), &
                module_t(MODULE_NAME), &
                procedure_t("parse_convective_heat_transfer_unit_s"))
    end function

    function parse_convective_heat_transfer_unit_with_units_c( &
            string, units) result(fallible_convective_heat_transfer_unit)
        character(len=*), intent(in) :: string
        class(convective_heat_transfer_unit_t), intent(in) :: units(:)
        type(fallible_convective_heat_transfer_unit_t) :: fallible_convective_heat_transfer_unit

        fallible_convective_heat_transfer_unit = fallible_convective_heat_transfer_unit_t( &
                parse_convective_heat_transfer_unit(var_str(string), units), &
                module_t(MODULE_NAME), &
                procedure_t("parse_convective_heat_transfer_unit_with_units_c"))
    end function

    function parse_convective_heat_transfer_unit_with_units_s( &
            string, units) result(fallible_convective_heat_transfer_unit)
        type(varying_string), intent(in) :: string
        class(convective_heat_transfer_unit_t), intent(in) :: units(:)
        type(fallible_convective_heat_transfer_unit_t) :: fallible_convective_heat_transfer_unit

        integer :: i
        type(varying_string) :: unit_strings(size(units))

        do i = 1, size(units)
            if (string == units(i)%to_string()) then
                fallible_convective_heat_transfer_unit = fallible_convective_heat_transfer_unit_t(units(i))
                return
            end if
        end do
        do i = 1, size(units)
            unit_strings(i) = units(i)%to_string()
        end do
        fallible_convective_heat_transfer_unit = fallible_convective_heat_transfer_unit_t(error_list_t(fatal_t( &
                UNKNOWN_UNIT, &
                module_t(MODULE_NAME), &
                procedure_t("parse_convective_heat_transfer_unit_with_units_s"), &
                '"' // string // '", known units: [' // join(unit_strings, ', ') // ']')))
    end function
end module
