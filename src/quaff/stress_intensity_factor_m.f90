module quaff_stress_intensity_factor_m
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
            MEGAPASCAL_ROOT_METER_PER_PASCAL_ROOT_METER, &
            KSI_ROOT_INCH_PER_PASCAL_ROOT_METER
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
            stress_intensity_factor_t, &
            fallible_stress_intensity_factor_t, &
            stress_intensity_factor_unit_t, &
            fallible_stress_intensity_factor_unit_t, &
            stress_intensity_factor_simple_unit_t, &
            operator(.unit.), &
            parse_stress_intensity_factor, &
            parse_stress_intensity_factor_unit, &
            sum, &
            abs, &
            DEFAULT_OUTPUT_UNITS, &
            PROVIDED_UNITS, &
            PASCAL_ROOT_METER, &
            MEGAPASCAL_ROOT_METER, &
            KSI_ROOT_INCH

    type :: stress_intensity_factor_t
        double precision :: megapascal_root_meter
    contains
        private
        procedure :: to_units
        generic, public :: operator(.in.) => to_units
        procedure, pass(stress_intensity_factor) :: double_times_stress_intensity_factor
        procedure, pass(stress_intensity_factor) :: integer_times_stress_intensity_factor
        procedure :: stress_intensity_factor_times_double
        procedure :: stress_intensity_factor_times_integer
        generic, public :: operator(*) => &
                double_times_stress_intensity_factor, &
                integer_times_stress_intensity_factor, &
                stress_intensity_factor_times_double, &
                stress_intensity_factor_times_integer
        procedure :: stress_intensity_factor_divided_by_double
        procedure :: stress_intensity_factor_divided_by_integer
        procedure :: stress_intensity_factor_divided_by_stress_intensity_factor
        generic, public :: operator(/) => &
                stress_intensity_factor_divided_by_double, &
                stress_intensity_factor_divided_by_integer, &
                stress_intensity_factor_divided_by_stress_intensity_factor
        procedure :: stress_intensity_factor_plus_stress_intensity_factor
        generic, public :: operator(+) => stress_intensity_factor_plus_stress_intensity_factor
        procedure :: stress_intensity_factor_minus_stress_intensity_factor
        generic, public :: operator(-) => stress_intensity_factor_minus_stress_intensity_factor
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

    type :: fallible_stress_intensity_factor_t
        private
        type(stress_intensity_factor_t) :: stress_intensity_factor_ = stress_intensity_factor_t(0.0d0)
        type(error_list_t) :: errors_
    contains
        private
        procedure, public :: failed => fallible_stress_intensity_factor_failed
        procedure, public :: stress_intensity_factor => fallible_stress_intensity_factor_stress_intensity_factor
        procedure, public :: errors => fallible_stress_intensity_factor_errors
    end type

    type, abstract :: stress_intensity_factor_unit_t
        double precision :: conversion_factor
    contains
        procedure(unit_to_string_i), deferred :: unit_to_string
        procedure(value_to_string_i), deferred :: value_to_string
        generic :: to_string => unit_to_string, value_to_string
        procedure(parse_as_i), deferred :: parse_as
    end type

    type :: fallible_stress_intensity_factor_unit_t
        private
        class(stress_intensity_factor_unit_t), allocatable :: unit_
        type(error_list_t) :: errors_
    contains
        private
        procedure, public :: failed => fallible_stress_intensity_factor_unit_failed
        procedure, public :: unit => fallible_stress_intensity_factor_unit_unit
        procedure, public :: errors => fallible_stress_intensity_factor_unit_errors
    end type

    type, extends(stress_intensity_factor_unit_t) :: stress_intensity_factor_simple_unit_t
        character(len=20) :: symbol
    contains
        procedure :: unit_to_string => simple_unit_to_string
        procedure :: value_to_string => simple_value_to_string
        procedure :: parse_as => simple_parse_as
    end type

    abstract interface
        elemental function unit_to_string_i(self) result(string)
            import :: stress_intensity_factor_unit_t, varying_string

            implicit none

            class(stress_intensity_factor_unit_t), intent(in) :: self
            type(varying_string) :: string
        end function

        pure function value_to_string_i(self, value_) result(string)
            import :: stress_intensity_factor_unit_t, varying_string

            implicit none

            class(stress_intensity_factor_unit_t), intent(in) :: self
            type(varying_string), intent(in) :: value_
            type(varying_string) :: string
        end function

        function parse_as_i(self, string) result(fallible_stress_intensity_factor)
            import :: stress_intensity_factor_unit_t, fallible_stress_intensity_factor_t, varying_string

            implicit none

            class(stress_intensity_factor_unit_t), intent(in) :: self
            type(varying_string), intent(in) :: string
            type(fallible_stress_intensity_factor_t) :: fallible_stress_intensity_factor
        end function
    end interface

    interface operator(.unit.)
        module procedure from_units
    end interface

    interface fallible_stress_intensity_factor_t
        module procedure fallible_stress_intensity_factor_from_stress_intensity_factor
        module procedure fallible_stress_intensity_factor_from_errors
        module procedure fallible_sif_from_fallible_sif
    end interface

    interface fallible_stress_intensity_factor_unit_t
        module procedure fallible_stress_intensity_factor_unit_from_unit
        module procedure fallible_stress_intensity_factor_unit_from_errors
        module procedure fallible_sif_unit_from_fallible_sif_unit
    end interface

    interface parse_stress_intensity_factor
        module procedure parse_stress_intensity_factor_c
        module procedure parse_stress_intensity_factor_s
        module procedure parse_stress_intensity_factor_with_units_c
        module procedure parse_stress_intensity_factor_with_units_s
    end interface

    interface parse_stress_intensity_factor_unit
        module procedure parse_stress_intensity_factor_unit_c
        module procedure parse_stress_intensity_factor_unit_s
        module procedure parse_stress_intensity_factor_unit_with_units_c
        module procedure parse_stress_intensity_factor_unit_with_units_s
    end interface

    interface abs
        module procedure abs_stress_intensity_factor
    end interface

    interface sum
        module procedure sum_stress_intensity_factor
    end interface

    type(stress_intensity_factor_simple_unit_t), parameter :: KSI_ROOT_INCH = &
            stress_intensity_factor_simple_unit_t( &
                    conversion_factor = KSI_ROOT_INCH_PER_PASCAL_ROOT_METER, &
                    symbol = "ksi in^0.5")
    type(stress_intensity_factor_simple_unit_t), parameter :: MEGAPASCAL_ROOT_METER = &
            stress_intensity_factor_simple_unit_t( &
                    conversion_factor = MEGAPASCAL_ROOT_METER_PER_PASCAL_ROOT_METER, &
                    symbol = "MPa m^0.5")
    type(stress_intensity_factor_simple_unit_t), parameter :: PASCAL_ROOT_METER = &
            stress_intensity_factor_simple_unit_t( &
                    conversion_factor = 1.0d0, &
                    symbol = "Pa m^0.5")

    type(stress_intensity_factor_simple_unit_t) :: DEFAULT_OUTPUT_UNITS = MEGAPASCAL_ROOT_METER

    type(stress_intensity_factor_simple_unit_t), parameter :: PROVIDED_UNITS(*) = &
      [KSI_ROOT_INCH, MEGAPASCAL_ROOT_METER, PASCAL_ROOT_METER]

    character(len=*), parameter :: MODULE_NAME = "quaff_stress_intensity_factor_m"
contains
    function parse_stress_intensity_factor_c(string) result(fallible_stress_intensity_factor)
        character(len=*), intent(in) :: string
        type(fallible_stress_intensity_factor_t) :: fallible_stress_intensity_factor

        fallible_stress_intensity_factor = fallible_stress_intensity_factor_t( &
                parse_stress_intensity_factor(var_str(string), PROVIDED_UNITS), &
                module_t(MODULE_NAME), &
                procedure_t("parse_stress_intensity_factor_c"))
    end function

    function parse_stress_intensity_factor_s(string) result(fallible_stress_intensity_factor)
        type(varying_string), intent(in) :: string
        type(fallible_stress_intensity_factor_t) :: fallible_stress_intensity_factor

        fallible_stress_intensity_factor = fallible_stress_intensity_factor_t( &
                parse_stress_intensity_factor(string, PROVIDED_UNITS), &
                module_t(MODULE_NAME), &
                procedure_t("parse_stress_intensity_factor_s"))
    end function

    function parse_stress_intensity_factor_with_units_c( &
            string, units) result(fallible_stress_intensity_factor)
        character(len=*), intent(in) :: string
        class(stress_intensity_factor_unit_t), intent(in) :: units(:)
        type(fallible_stress_intensity_factor_t) :: fallible_stress_intensity_factor

        fallible_stress_intensity_factor = fallible_stress_intensity_factor_t( &
                parse_stress_intensity_factor(var_str(string), units), &
                module_t(MODULE_NAME), &
                procedure_t("parse_stress_intensity_factor_with_units_c"))
    end function

    function parse_stress_intensity_factor_with_units_s( &
            string, units) result(fallible_stress_intensity_factor)
        type(varying_string), intent(in) :: string
        class(stress_intensity_factor_unit_t), intent(in) :: units(:)
        type(fallible_stress_intensity_factor_t) :: fallible_stress_intensity_factor

        integer :: i

        do i = 1, size(units)
            fallible_stress_intensity_factor = units(i)%parse_as(string)
            if (.not. fallible_stress_intensity_factor%failed()) return
        end do
        fallible_stress_intensity_factor = fallible_stress_intensity_factor_t(error_list_t(fatal_t( &
                PARSE_ERROR, &
                module_t(MODULE_NAME), &
                procedure_t("parse_stress_intensity_factor_with_units_s"), &
                "Unable to parse '" // string // "' as a stress_intensity_factor_t. Tried with units: " &
                // join(units%to_string(), ", "))))
    end function

    elemental function from_units(value_, units) result(stress_intensity_factor)
        double precision, intent(in) :: value_
        class(stress_intensity_factor_unit_t), intent(in) :: units
        type(stress_intensity_factor_t) :: stress_intensity_factor

        stress_intensity_factor%megapascal_root_meter = value_ / units%conversion_factor
    end function

    elemental function to_units(self, units) result(stress_intensity_factor)
        class(stress_intensity_factor_t), intent(in) :: self
        class(stress_intensity_factor_unit_t), intent(in) :: units
        double precision :: stress_intensity_factor

        stress_intensity_factor = self%megapascal_root_meter * units%conversion_factor
    end function

    elemental function double_times_stress_intensity_factor( &
            multiplier, stress_intensity_factor) result(new_stress_intensity_factor)
        double precision, intent(in) :: multiplier
        class(stress_intensity_factor_t), intent(in) :: stress_intensity_factor
        type(stress_intensity_factor_t) :: new_stress_intensity_factor

        new_stress_intensity_factor%megapascal_root_meter = &
                multiplier * stress_intensity_factor%megapascal_root_meter
    end function

    elemental function integer_times_stress_intensity_factor( &
            multiplier, stress_intensity_factor) result(new_stress_intensity_factor)
        integer, intent(in) :: multiplier
        class(stress_intensity_factor_t), intent(in) :: stress_intensity_factor
        type(stress_intensity_factor_t) :: new_stress_intensity_factor

        new_stress_intensity_factor%megapascal_root_meter = &
                dble(multiplier) * stress_intensity_factor%megapascal_root_meter
    end function

    elemental function stress_intensity_factor_times_double( &
            stress_intensity_factor, multiplier) result(new_stress_intensity_factor)
        class(stress_intensity_factor_t), intent(in) :: stress_intensity_factor
        double precision, intent(in) :: multiplier
        type(stress_intensity_factor_t) :: new_stress_intensity_factor

        new_stress_intensity_factor%megapascal_root_meter = &
                stress_intensity_factor%megapascal_root_meter * multiplier
    end function

    elemental function stress_intensity_factor_times_integer( &
            stress_intensity_factor, multiplier) result(new_stress_intensity_factor)
        class(stress_intensity_factor_t), intent(in) :: stress_intensity_factor
        integer, intent(in) :: multiplier
        type(stress_intensity_factor_t) :: new_stress_intensity_factor

        new_stress_intensity_factor%megapascal_root_meter = &
                stress_intensity_factor%megapascal_root_meter * dble(multiplier)
    end function

    elemental function stress_intensity_factor_divided_by_double( &
            stress_intensity_factor, divisor) result(new_stress_intensity_factor)
        class(stress_intensity_factor_t), intent(in) :: stress_intensity_factor
        double precision, intent(in) :: divisor
        type(stress_intensity_factor_t) :: new_stress_intensity_factor

        new_stress_intensity_factor%megapascal_root_meter = &
                stress_intensity_factor%megapascal_root_meter / divisor
    end function

    elemental function stress_intensity_factor_divided_by_integer( &
            stress_intensity_factor, divisor) result(new_stress_intensity_factor)
        class(stress_intensity_factor_t), intent(in) :: stress_intensity_factor
        integer, intent(in) :: divisor
        type(stress_intensity_factor_t) :: new_stress_intensity_factor

        new_stress_intensity_factor%megapascal_root_meter = &
                stress_intensity_factor%megapascal_root_meter / dble(divisor)
    end function

    elemental function stress_intensity_factor_divided_by_stress_intensity_factor( &
            numerator, denomenator) result(ratio)
        class(stress_intensity_factor_t), intent(in) :: numerator
        type(stress_intensity_factor_t), intent(in) :: denomenator
        double precision :: ratio

        ratio = numerator%megapascal_root_meter / denomenator%megapascal_root_meter
    end function

    elemental function stress_intensity_factor_plus_stress_intensity_factor( &
            lhs, rhs) result(sum_)
        class(stress_intensity_factor_t), intent(in) :: lhs
        type(stress_intensity_factor_t), intent(in) :: rhs
        type(stress_intensity_factor_t) :: sum_

        sum_%megapascal_root_meter = lhs%megapascal_root_meter + rhs%megapascal_root_meter
    end function

    elemental function stress_intensity_factor_minus_stress_intensity_factor( &
            lhs, rhs) result(difference)
        class(stress_intensity_factor_t), intent(in) :: lhs
        type(stress_intensity_factor_t), intent(in) :: rhs
        type(stress_intensity_factor_t) :: difference

        difference%megapascal_root_meter = lhs%megapascal_root_meter - rhs%megapascal_root_meter
    end function

    pure function abs_stress_intensity_factor(stress_intensity_factor) result(abs_)
        type(stress_intensity_factor_t), intent(in) :: stress_intensity_factor
        type(stress_intensity_factor_t) :: abs_

        abs_%megapascal_root_meter = abs(stress_intensity_factor%megapascal_root_meter)
    end function

    pure function sum_stress_intensity_factor(stress_intensity_factors) result(sum_)
        type(stress_intensity_factor_t), intent(in) :: stress_intensity_factors(:)
        type(stress_intensity_factor_t) :: sum_

        sum_%megapascal_root_meter = sum(stress_intensity_factors%megapascal_root_meter)
    end function

    elemental function greater_than(lhs, rhs)
        class(stress_intensity_factor_t), intent(in) :: lhs
        type(stress_intensity_factor_t), intent(in) :: rhs
        logical :: greater_than

        greater_than = lhs%megapascal_root_meter > rhs%megapascal_root_meter
    end function

    elemental function less_than(lhs, rhs)
        class(stress_intensity_factor_t), intent(in) :: lhs
        type(stress_intensity_factor_t), intent(in) :: rhs
        logical :: less_than

        less_than = lhs%megapascal_root_meter < rhs%megapascal_root_meter
    end function

    elemental function greater_than_or_equal(lhs, rhs)
        class(stress_intensity_factor_t), intent(in) :: lhs
        type(stress_intensity_factor_t), intent(in) :: rhs
        logical :: greater_than_or_equal

        greater_than_or_equal = lhs%megapascal_root_meter >= rhs%megapascal_root_meter
    end function

    elemental function less_than_or_equal(lhs, rhs)
        class(stress_intensity_factor_t), intent(in) :: lhs
        type(stress_intensity_factor_t), intent(in) :: rhs
        logical :: less_than_or_equal

        less_than_or_equal = lhs%megapascal_root_meter <= rhs%megapascal_root_meter
    end function

    elemental function equal_(lhs, rhs)
        class(stress_intensity_factor_t), intent(in) :: lhs
        type(stress_intensity_factor_t), intent(in) :: rhs
        logical :: equal_

        equal_ = lhs%megapascal_root_meter .safeEq. rhs%megapascal_root_meter
    end function

    elemental function equal_within_absolute_(lhs, rhs, within)
        class(stress_intensity_factor_t), intent(in) :: lhs
        type(stress_intensity_factor_t), intent(in) :: rhs
        type(stress_intensity_factor_t), intent(in) :: within
        logical :: equal_within_absolute_

        equal_within_absolute_ = equal_within_absolute( &
                lhs%megapascal_root_meter, rhs%megapascal_root_meter, within%megapascal_root_meter)
    end function

    elemental function equal_within_relative_(lhs, rhs, within)
        class(stress_intensity_factor_t), intent(in) :: lhs
        type(stress_intensity_factor_t), intent(in) :: rhs
        double precision, intent(in) :: within
        logical :: equal_within_relative_

        equal_within_relative_ = equal_within_relative( &
                lhs%megapascal_root_meter, rhs%megapascal_root_meter, within)
    end function

    elemental function not_equal(lhs, rhs)
        class(stress_intensity_factor_t), intent(in) :: lhs
        type(stress_intensity_factor_t), intent(in) :: rhs
        logical :: not_equal

        not_equal = .not. lhs == rhs
    end function

    elemental function to_string_full_precision(self) result(string)
        class(stress_intensity_factor_t), intent(in) :: self
        type(varying_string) :: string

        string = self%to_string_in(DEFAULT_OUTPUT_UNITS)
    end function

    elemental function to_string_with_precision(self, significant_digits) result(string)
        class(stress_intensity_factor_t), intent(in) :: self
        integer, intent(in) :: significant_digits
        type(varying_string) :: string

        string = self%to_string_in(DEFAULT_OUTPUT_UNITS, significant_digits)
    end function

    elemental function to_string_in_full_precision(self, units) result(string)
        class(stress_intensity_factor_t), intent(in) :: self
        class(stress_intensity_factor_unit_t), intent(in) :: units
        type(varying_string) :: string

        string = units%to_string(to_string(self.in.units))
    end function

    elemental function to_string_in_with_precision( &
            self, units, significant_digits) result(string)
        class(stress_intensity_factor_t), intent(in) :: self
        class(stress_intensity_factor_unit_t), intent(in) :: units
        integer, intent(in) :: significant_digits
        type(varying_string) :: string

        string = units%to_string(to_string(self.in.units, significant_digits))
    end function

    function fallible_stress_intensity_factor_from_stress_intensity_factor( &
            stress_intensity_factor) result(fallible_stress_intensity_factor)
        type(stress_intensity_factor_t), intent(in) :: stress_intensity_factor
        type(fallible_stress_intensity_factor_t) :: fallible_stress_intensity_factor

        fallible_stress_intensity_factor%stress_intensity_factor_ = stress_intensity_factor
    end function

    function fallible_stress_intensity_factor_from_errors( &
            errors) result(fallible_stress_intensity_factor)
        type(error_list_t), intent(in) :: errors
        type(fallible_stress_intensity_factor_t) :: fallible_stress_intensity_factor

        fallible_stress_intensity_factor%errors_ = errors
    end function

    function fallible_sif_from_fallible_sif( &
            fallible_stress_intensity_factor, &
            module_, &
            procedure_) &
            result(new_fallible_stress_intensity_factor)
        type(fallible_stress_intensity_factor_t), intent(in) :: fallible_stress_intensity_factor
        type(module_t), intent(in) :: module_
        type(procedure_t), intent(in) :: procedure_
        type(fallible_stress_intensity_factor_t) :: new_fallible_stress_intensity_factor

        if (fallible_stress_intensity_factor%failed()) then
            new_fallible_stress_intensity_factor%errors_ = error_list_t( &
                    fallible_stress_intensity_factor%errors_, module_, procedure_)
        else
            new_fallible_stress_intensity_factor%stress_intensity_factor_ = &
                    fallible_stress_intensity_factor%stress_intensity_factor_
        end if
    end function

    elemental function fallible_stress_intensity_factor_failed( &
            self) result(failed)
        class(fallible_stress_intensity_factor_t), intent(in) :: self
        logical :: failed

        failed = self%errors_%has_any()
    end function

    elemental function fallible_stress_intensity_factor_stress_intensity_factor( &
            self) result(stress_intensity_factor)
        class(fallible_stress_intensity_factor_t), intent(in) :: self
        type(stress_intensity_factor_t) :: stress_intensity_factor

        stress_intensity_factor = self%stress_intensity_factor_
    end function

    impure elemental function fallible_stress_intensity_factor_errors( &
            self) result(errors)
        class(fallible_stress_intensity_factor_t), intent(in) :: self
        type(error_list_t) :: errors

        errors = self%errors_
    end function

    function fallible_stress_intensity_factor_unit_from_unit( &
            unit) result(fallible_stress_intensity_factor_unit)
        class(stress_intensity_factor_unit_t), intent(in) :: unit
        type(fallible_stress_intensity_factor_unit_t) :: fallible_stress_intensity_factor_unit

        allocate(fallible_stress_intensity_factor_unit%unit_, source = unit)
    end function

    function fallible_stress_intensity_factor_unit_from_errors( &
            errors) result(fallible_stress_intensity_factor_unit)
        type(error_list_t), intent(in) :: errors
        type(fallible_stress_intensity_factor_unit_t) :: fallible_stress_intensity_factor_unit

        fallible_stress_intensity_factor_unit%errors_ = errors
    end function

    function fallible_sif_unit_from_fallible_sif_unit( &
            fallible_stress_intensity_factor_unit, &
            module_, &
            procedure_) &
            result(new_fallible_stress_intensity_factor_unit)
        type(fallible_stress_intensity_factor_unit_t), intent(in) :: fallible_stress_intensity_factor_unit
        type(module_t), intent(in) :: module_
        type(procedure_t), intent(in) :: procedure_
        type(fallible_stress_intensity_factor_unit_t) :: new_fallible_stress_intensity_factor_unit

        if (fallible_stress_intensity_factor_unit%failed()) then
            new_fallible_stress_intensity_factor_unit%errors_ = error_list_t( &
                    fallible_stress_intensity_factor_unit%errors_, module_, procedure_)
        else
            allocate(new_fallible_stress_intensity_factor_unit%unit_, source = &
                    fallible_stress_intensity_factor_unit%unit_)
        end if
    end function

    elemental function fallible_stress_intensity_factor_unit_failed( &
            self) result(failed)
        class(fallible_stress_intensity_factor_unit_t), intent(in) :: self
        logical :: failed

        failed = self%errors_%has_any()
    end function

    function fallible_stress_intensity_factor_unit_unit( &
            self) result(unit)
        class(fallible_stress_intensity_factor_unit_t), intent(in) :: self
        class(stress_intensity_factor_unit_t), allocatable :: unit

        allocate(unit, source = self%unit_)
    end function

    impure elemental function fallible_stress_intensity_factor_unit_errors( &
            self) result(errors)
        class(fallible_stress_intensity_factor_unit_t), intent(in) :: self
        type(error_list_t) :: errors

        errors = self%errors_
    end function

    elemental function simple_unit_to_string(self) result(string)
        class(stress_intensity_factor_simple_unit_t), intent(in) :: self
        type(varying_string) :: string

        string = trim(self%symbol)
    end function

    pure function simple_value_to_string(self, value_) result(string)
        class(stress_intensity_factor_simple_unit_t), intent(in) :: self
        type(varying_string), intent(in) :: value_
        type(varying_string) :: string

        string = value_ // " " // self%to_string()
    end function

    function simple_parse_as(self, string) result(fallible_stress_intensity_factor)
        class(stress_intensity_factor_simple_unit_t), intent(in) :: self
        type(varying_string), intent(in) :: string
        type(fallible_stress_intensity_factor_t) :: fallible_stress_intensity_factor

        type(parse_result_t) :: parse_result

        parse_result = parse_with(the_parser, string)
        if (parse_result%ok) then
            select type (the_number => parse_result%parsed)
            type is (parsed_rational_t)
                fallible_stress_intensity_factor = fallible_stress_intensity_factor_t(the_number%value_.unit.self)
            end select
        else
            fallible_stress_intensity_factor = fallible_stress_intensity_factor_t(error_list_t(fatal_t( &
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

    function parse_stress_intensity_factor_unit_c(string) result(fallible_stress_intensity_factor_unit)
        character(len=*), intent(in) :: string
        type(fallible_stress_intensity_factor_unit_t) :: fallible_stress_intensity_factor_unit

        fallible_stress_intensity_factor_unit = fallible_stress_intensity_factor_unit_t( &
                parse_stress_intensity_factor_unit(var_str(string), PROVIDED_UNITS), &
                module_t(MODULE_NAME), &
                procedure_t("parse_stress_intensity_factor_unit_c"))
    end function

    function parse_stress_intensity_factor_unit_s(string) result(fallible_stress_intensity_factor_unit)
        type(varying_string), intent(in) :: string
        type(fallible_stress_intensity_factor_unit_t) :: fallible_stress_intensity_factor_unit

        fallible_stress_intensity_factor_unit = fallible_stress_intensity_factor_unit_t( &
                parse_stress_intensity_factor_unit(string, PROVIDED_UNITS), &
                module_t(MODULE_NAME), &
                procedure_t("parse_stress_intensity_factor_unit_s"))
    end function

    function parse_stress_intensity_factor_unit_with_units_c( &
            string, units) result(fallible_stress_intensity_factor_unit)
        character(len=*), intent(in) :: string
        class(stress_intensity_factor_unit_t), intent(in) :: units(:)
        type(fallible_stress_intensity_factor_unit_t) :: fallible_stress_intensity_factor_unit

        fallible_stress_intensity_factor_unit = fallible_stress_intensity_factor_unit_t( &
                parse_stress_intensity_factor_unit(var_str(string), units), &
                module_t(MODULE_NAME), &
                procedure_t("parse_stress_intensity_factor_unit_with_units_c"))
    end function

    function parse_stress_intensity_factor_unit_with_units_s( &
            string, units) result(fallible_stress_intensity_factor_unit)
        type(varying_string), intent(in) :: string
        class(stress_intensity_factor_unit_t), intent(in) :: units(:)
        type(fallible_stress_intensity_factor_unit_t) :: fallible_stress_intensity_factor_unit

        integer :: i
        type(varying_string) :: unit_strings(size(units))

        do i = 1, size(units)
            if (string == units(i)%to_string()) then
                fallible_stress_intensity_factor_unit = fallible_stress_intensity_factor_unit_t(units(i))
                return
            end if
        end do
        do i = 1, size(units)
            unit_strings(i) = units(i)%to_string()
        end do
        fallible_stress_intensity_factor_unit = fallible_stress_intensity_factor_unit_t(error_list_t(fatal_t( &
                UNKNOWN_UNIT, &
                module_t(MODULE_NAME), &
                procedure_t("parse_stress_intensity_factor_unit_with_units_s"), &
                '"' // string // '", known units: [' // join(unit_strings, ', ') // ']')))
    end function
end module
