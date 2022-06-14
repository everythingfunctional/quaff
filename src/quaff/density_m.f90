module quaff_density_m
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
            GRAMS_PER_CUBIC_METER_PER_KILOGRAMS_PER_CUBIC_METER, &
            POUNDS_PER_CUBIC_FOOT_PER_KILOGRAMS_PER_CUBIC_METER
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
            density_t, &
            fallible_density_t, &
            density_unit_t, &
            fallible_density_unit_t, &
            density_simple_unit_t, &
            operator(.unit.), &
            parse_density, &
            parse_density_unit, &
            sum, &
            abs, &
            DEFAULT_OUTPUT_UNITS, &
            PROVIDED_UNITS, &
            GRAMS_PER_CUBIC_METER, &
            KILOGRAMS_PER_CUBIC_METER, &
            POUNDS_PER_CUBIC_FOOT

    type :: density_t
        double precision :: kilograms_per_cubic_meter
    contains
        private
        procedure :: to_units
        generic, public :: operator(.in.) => to_units
        procedure, pass(density) :: double_times_density
        procedure, pass(density) :: integer_times_density
        procedure :: density_times_double
        procedure :: density_times_integer
        generic, public :: operator(*) => &
                double_times_density, &
                integer_times_density, &
                density_times_double, &
                density_times_integer
        procedure :: density_divided_by_double
        procedure :: density_divided_by_integer
        procedure :: density_divided_by_density
        generic, public :: operator(/) => &
                density_divided_by_double, &
                density_divided_by_integer, &
                density_divided_by_density
        procedure :: density_plus_density
        generic, public :: operator(+) => density_plus_density
        procedure :: density_minus_density
        generic, public :: operator(-) => density_minus_density
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

    type :: fallible_density_t
        private
        type(density_t) :: density_ = density_t(0.0d0)
        type(error_list_t) :: errors_
    contains
        private
        procedure, public :: failed => fallible_density_failed
        procedure, public :: density => fallible_density_density
        procedure, public :: errors => fallible_density_errors
    end type

    type, abstract :: density_unit_t
        double precision :: conversion_factor
    contains
        procedure(unit_to_string_i), deferred :: unit_to_string
        procedure(value_to_string_i), deferred :: value_to_string
        generic :: to_string => unit_to_string, value_to_string
        procedure(parse_as_i), deferred :: parse_as
    end type

    type :: fallible_density_unit_t
        private
        class(density_unit_t), allocatable :: unit_
        type(error_list_t) :: errors_
    contains
        private
        procedure, public :: failed => fallible_density_unit_failed
        procedure, public :: unit => fallible_density_unit_unit
        procedure, public :: errors => fallible_density_unit_errors
    end type

    type, extends(density_unit_t) :: density_simple_unit_t
        character(len=20) :: symbol
    contains
        procedure :: unit_to_string => simple_unit_to_string
        procedure :: value_to_string => simple_value_to_string
        procedure :: parse_as => simple_parse_as
    end type

    abstract interface
        elemental function unit_to_string_i(self) result(string)
            import :: density_unit_t, varying_string

            implicit none

            class(density_unit_t), intent(in) :: self
            type(varying_string) :: string
        end function

        pure function value_to_string_i(self, value_) result(string)
            import :: density_unit_t, varying_string

            implicit none

            class(density_unit_t), intent(in) :: self
            type(varying_string), intent(in) :: value_
            type(varying_string) :: string
        end function

        function parse_as_i(self, string) result(fallible_density)
            import :: density_unit_t, fallible_density_t, varying_string

            implicit none

            class(density_unit_t), intent(in) :: self
            type(varying_string), intent(in) :: string
            type(fallible_density_t) :: fallible_density
        end function
    end interface

    interface operator(.unit.)
        module procedure from_units
    end interface

    interface fallible_density_t
        module procedure fallible_density_from_density
        module procedure fallible_density_from_errors
        module procedure fallible_density_from_fallible_density
    end interface

    interface fallible_density_unit_t
        module procedure fallible_density_unit_from_unit
        module procedure fallible_density_unit_from_errors
        module procedure fallible_density_unit_from_fallible_density_unit
    end interface

    interface parse_density
        module procedure parse_density_c
        module procedure parse_density_s
        module procedure parse_density_with_units_c
        module procedure parse_density_with_units_s
    end interface

    interface parse_density_unit
        module procedure parse_density_unit_c
        module procedure parse_density_unit_s
        module procedure parse_density_unit_with_units_c
        module procedure parse_density_unit_with_units_s
    end interface

    interface abs
        module procedure abs_density
    end interface

    interface sum
        module procedure sum_density
    end interface

    type(density_simple_unit_t), parameter :: GRAMS_PER_CUBIC_METER = &
            density_simple_unit_t( &
                    conversion_factor = GRAMS_PER_CUBIC_METER_PER_KILOGRAMS_PER_CUBIC_METER, &
                    symbol = "g/m^3")
    type(density_simple_unit_t), parameter :: KILOGRAMS_PER_CUBIC_METER = &
            density_simple_unit_t( &
                    conversion_factor = 1.0d0, &
                    symbol = "kg/m^3")
    type(density_simple_unit_t), parameter :: POUNDS_PER_CUBIC_FOOT = &
            density_simple_unit_t( &
                    conversion_factor = POUNDS_PER_CUBIC_FOOT_PER_KILOGRAMS_PER_CUBIC_METER, &
                    symbol = "lbm/ft^3")

    type(density_simple_unit_t) :: DEFAULT_OUTPUT_UNITS = KILOGRAMS_PER_CUBIC_METER

    type(density_simple_unit_t), parameter :: PROVIDED_UNITS(*) = &
            [GRAMS_PER_CUBIC_METER &
            , KILOGRAMS_PER_CUBIC_METER &
            , POUNDS_PER_CUBIC_FOOT &
            ]

    character(len=*), parameter :: MODULE_NAME = "quaff_density_m"
contains
    function parse_density_c(string) result(fallible_density)
        character(len=*), intent(in) :: string
        type(fallible_density_t) :: fallible_density

        fallible_density = fallible_density_t( &
                parse_density(var_str(string), PROVIDED_UNITS), &
                module_t(MODULE_NAME), &
                procedure_t("parse_density_c"))
    end function

    function parse_density_s(string) result(fallible_density)
        type(varying_string), intent(in) :: string
        type(fallible_density_t) :: fallible_density

        fallible_density = fallible_density_t( &
                parse_density(string, PROVIDED_UNITS), &
                module_t(MODULE_NAME), &
                procedure_t("parse_density_s"))
    end function

    function parse_density_with_units_c( &
            string, units) result(fallible_density)
        character(len=*), intent(in) :: string
        class(density_unit_t), intent(in) :: units(:)
        type(fallible_density_t) :: fallible_density

        fallible_density = fallible_density_t( &
                parse_density(var_str(string), units), &
                module_t(MODULE_NAME), &
                procedure_t("parse_density_with_units_c"))
    end function

    function parse_density_with_units_s( &
            string, units) result(fallible_density)
        type(varying_string), intent(in) :: string
        class(density_unit_t), intent(in) :: units(:)
        type(fallible_density_t) :: fallible_density

        integer :: i

        do i = 1, size(units)
            fallible_density = units(i)%parse_as(string)
            if (.not. fallible_density%failed()) return
        end do
        fallible_density = fallible_density_t(error_list_t(fatal_t( &
                PARSE_ERROR, &
                module_t(MODULE_NAME), &
                procedure_t("parse_density_with_units_s"), &
                "Unable to parse '" // string // "' as a density_t. Tried with units: " &
                // join(units%to_string(), ", "))))
    end function

    elemental function from_units(value_, units) result(density)
        double precision, intent(in) :: value_
        class(density_unit_t), intent(in) :: units
        type(density_t) :: density

        density%kilograms_per_cubic_meter = value_ / units%conversion_factor
    end function

    elemental function to_units(self, units) result(density)
        class(density_t), intent(in) :: self
        class(density_unit_t), intent(in) :: units
        double precision :: density

        density = self%kilograms_per_cubic_meter * units%conversion_factor
    end function

    elemental function double_times_density( &
            multiplier, density) result(new_density)
        double precision, intent(in) :: multiplier
        class(density_t), intent(in) :: density
        type(density_t) :: new_density

        new_density%kilograms_per_cubic_meter = &
                multiplier * density%kilograms_per_cubic_meter
    end function

    elemental function integer_times_density( &
            multiplier, density) result(new_density)
        integer, intent(in) :: multiplier
        class(density_t), intent(in) :: density
        type(density_t) :: new_density

        new_density%kilograms_per_cubic_meter = &
                dble(multiplier) * density%kilograms_per_cubic_meter
    end function

    elemental function density_times_double( &
            density, multiplier) result(new_density)
        class(density_t), intent(in) :: density
        double precision, intent(in) :: multiplier
        type(density_t) :: new_density

        new_density%kilograms_per_cubic_meter = &
                density%kilograms_per_cubic_meter * multiplier
    end function

    elemental function density_times_integer( &
            density, multiplier) result(new_density)
        class(density_t), intent(in) :: density
        integer, intent(in) :: multiplier
        type(density_t) :: new_density

        new_density%kilograms_per_cubic_meter = &
                density%kilograms_per_cubic_meter * dble(multiplier)
    end function

    elemental function density_divided_by_double( &
            density, divisor) result(new_density)
        class(density_t), intent(in) :: density
        double precision, intent(in) :: divisor
        type(density_t) :: new_density

        new_density%kilograms_per_cubic_meter = &
                density%kilograms_per_cubic_meter / divisor
    end function

    elemental function density_divided_by_integer( &
            density, divisor) result(new_density)
        class(density_t), intent(in) :: density
        integer, intent(in) :: divisor
        type(density_t) :: new_density

        new_density%kilograms_per_cubic_meter = &
                density%kilograms_per_cubic_meter / dble(divisor)
    end function

    elemental function density_divided_by_density( &
            numerator, denomenator) result(ratio)
        class(density_t), intent(in) :: numerator
        type(density_t), intent(in) :: denomenator
        double precision :: ratio

        ratio = numerator%kilograms_per_cubic_meter / denomenator%kilograms_per_cubic_meter
    end function

    elemental function density_plus_density( &
            lhs, rhs) result(sum_)
        class(density_t), intent(in) :: lhs
        type(density_t), intent(in) :: rhs
        type(density_t) :: sum_

        sum_%kilograms_per_cubic_meter = lhs%kilograms_per_cubic_meter + rhs%kilograms_per_cubic_meter
    end function

    elemental function density_minus_density( &
            lhs, rhs) result(difference)
        class(density_t), intent(in) :: lhs
        type(density_t), intent(in) :: rhs
        type(density_t) :: difference

        difference%kilograms_per_cubic_meter = lhs%kilograms_per_cubic_meter - rhs%kilograms_per_cubic_meter
    end function

    pure function abs_density(density) result(abs_)
        class(density_t), intent(in) :: density
        type(density_t) :: abs_

        abs_%kilograms_per_cubic_meter = abs(density%kilograms_per_cubic_meter)
    end function

    pure function sum_density(densitys) result(sum_)
        type(density_t), intent(in) :: densitys(:)
        type(density_t) :: sum_

        sum_%kilograms_per_cubic_meter = sum(densitys%kilograms_per_cubic_meter)
    end function

    elemental function greater_than(lhs, rhs)
        class(density_t), intent(in) :: lhs
        type(density_t), intent(in) :: rhs
        logical :: greater_than

        greater_than = lhs%kilograms_per_cubic_meter > rhs%kilograms_per_cubic_meter
    end function

    elemental function less_than(lhs, rhs)
        class(density_t), intent(in) :: lhs
        type(density_t), intent(in) :: rhs
        logical :: less_than

        less_than = lhs%kilograms_per_cubic_meter < rhs%kilograms_per_cubic_meter
    end function

    elemental function greater_than_or_equal(lhs, rhs)
        class(density_t), intent(in) :: lhs
        type(density_t), intent(in) :: rhs
        logical :: greater_than_or_equal

        greater_than_or_equal = lhs%kilograms_per_cubic_meter >= rhs%kilograms_per_cubic_meter
    end function

    elemental function less_than_or_equal(lhs, rhs)
        class(density_t), intent(in) :: lhs
        type(density_t), intent(in) :: rhs
        logical :: less_than_or_equal

        less_than_or_equal = lhs%kilograms_per_cubic_meter <= rhs%kilograms_per_cubic_meter
    end function

    elemental function equal_(lhs, rhs)
        class(density_t), intent(in) :: lhs
        type(density_t), intent(in) :: rhs
        logical :: equal_

        equal_ = lhs%kilograms_per_cubic_meter .safeEq. rhs%kilograms_per_cubic_meter
    end function

    elemental function equal_within_absolute_(lhs, rhs, within)
        class(density_t), intent(in) :: lhs
        type(density_t), intent(in) :: rhs
        type(density_t), intent(in) :: within
        logical :: equal_within_absolute_

        equal_within_absolute_ = equal_within_absolute( &
                lhs%kilograms_per_cubic_meter, rhs%kilograms_per_cubic_meter, within%kilograms_per_cubic_meter)
    end function

    elemental function equal_within_relative_(lhs, rhs, within)
        class(density_t), intent(in) :: lhs
        type(density_t), intent(in) :: rhs
        double precision, intent(in) :: within
        logical :: equal_within_relative_

        equal_within_relative_ = equal_within_relative( &
                lhs%kilograms_per_cubic_meter, rhs%kilograms_per_cubic_meter, within)
    end function

    elemental function not_equal(lhs, rhs)
        class(density_t), intent(in) :: lhs
        type(density_t), intent(in) :: rhs
        logical :: not_equal

        not_equal = .not. lhs == rhs
    end function

    elemental function to_string_full_precision(self) result(string)
        class(density_t), intent(in) :: self
        type(varying_string) :: string

        string = self%to_string_in(DEFAULT_OUTPUT_UNITS)
    end function

    elemental function to_string_with_precision(self, significant_digits) result(string)
        class(density_t), intent(in) :: self
        integer, intent(in) :: significant_digits
        type(varying_string) :: string

        string = self%to_string_in(DEFAULT_OUTPUT_UNITS, significant_digits)
    end function

    elemental function to_string_in_full_precision(self, units) result(string)
        class(density_t), intent(in) :: self
        class(density_unit_t), intent(in) :: units
        type(varying_string) :: string

        string = units%to_string(to_string(self.in.units))
    end function

    elemental function to_string_in_with_precision( &
            self, units, significant_digits) result(string)
        class(density_t), intent(in) :: self
        class(density_unit_t), intent(in) :: units
        integer, intent(in) :: significant_digits
        type(varying_string) :: string

        string = units%to_string(to_string(self.in.units, significant_digits))
    end function

    function fallible_density_from_density( &
            density) result(fallible_density)
        type(density_t), intent(in) :: density
        type(fallible_density_t) :: fallible_density

        fallible_density%density_ = density
    end function

    function fallible_density_from_errors( &
            errors) result(fallible_density)
        type(error_list_t), intent(in) :: errors
        type(fallible_density_t) :: fallible_density

        fallible_density%errors_ = errors
    end function

    function fallible_density_from_fallible_density( &
            fallible_density, &
            module_, &
            procedure_) &
            result(new_fallible_density)
        type(fallible_density_t), intent(in) :: fallible_density
        type(module_t), intent(in) :: module_
        type(procedure_t), intent(in) :: procedure_
        type(fallible_density_t) :: new_fallible_density

        if (fallible_density%failed()) then
            new_fallible_density%errors_ = error_list_t( &
                    fallible_density%errors_, module_, procedure_)
        else
            new_fallible_density%density_ = fallible_density%density_
        end if
    end function

    elemental function fallible_density_failed( &
            self) result(failed)
        class(fallible_density_t), intent(in) :: self
        logical :: failed

        failed = self%errors_%has_any()
    end function

    elemental function fallible_density_density( &
            self) result(density)
        class(fallible_density_t), intent(in) :: self
        type(density_t) :: density

        density = self%density_
    end function

    impure elemental function fallible_density_errors( &
            self) result(errors)
        class(fallible_density_t), intent(in) :: self
        type(error_list_t) :: errors

        errors = self%errors_
    end function

    function fallible_density_unit_from_unit( &
            unit) result(fallible_density_unit)
        class(density_unit_t), intent(in) :: unit
        type(fallible_density_unit_t) :: fallible_density_unit

        allocate(fallible_density_unit%unit_, source = unit)
    end function

    function fallible_density_unit_from_errors( &
            errors) result(fallible_density_unit)
        type(error_list_t), intent(in) :: errors
        type(fallible_density_unit_t) :: fallible_density_unit

        fallible_density_unit%errors_ = errors
    end function

    function fallible_density_unit_from_fallible_density_unit( &
            fallible_density_unit, &
            module_, &
            procedure_) &
            result(new_fallible_density_unit)
        type(fallible_density_unit_t), intent(in) :: fallible_density_unit
        type(module_t), intent(in) :: module_
        type(procedure_t), intent(in) :: procedure_
        type(fallible_density_unit_t) :: new_fallible_density_unit

        if (fallible_density_unit%failed()) then
            new_fallible_density_unit%errors_ = error_list_t( &
                    fallible_density_unit%errors_, module_, procedure_)
        else
            allocate(new_fallible_density_unit%unit_, source = &
                    fallible_density_unit%unit_)
        end if
    end function

    elemental function fallible_density_unit_failed( &
            self) result(failed)
        class(fallible_density_unit_t), intent(in) :: self
        logical :: failed

        failed = self%errors_%has_any()
    end function

    function fallible_density_unit_unit( &
            self) result(unit)
        class(fallible_density_unit_t), intent(in) :: self
        class(density_unit_t), allocatable :: unit

        allocate(unit, source = self%unit_)
    end function

    impure elemental function fallible_density_unit_errors( &
            self) result(errors)
        class(fallible_density_unit_t), intent(in) :: self
        type(error_list_t) :: errors

        errors = self%errors_
    end function

    elemental function simple_unit_to_string(self) result(string)
        class(density_simple_unit_t), intent(in) :: self
        type(varying_string) :: string

        string = trim(self%symbol)
    end function

    pure function simple_value_to_string(self, value_) result(string)
        class(density_simple_unit_t), intent(in) :: self
        type(varying_string), intent(in) :: value_
        type(varying_string) :: string

        string = value_ // " " // self%to_string()
    end function

    function simple_parse_as(self, string) result(fallible_density)
        class(density_simple_unit_t), intent(in) :: self
        type(varying_string), intent(in) :: string
        type(fallible_density_t) :: fallible_density

        type(parse_result_t) :: parse_result

        parse_result = parse_with(the_parser, string)
        if (parse_result%ok) then
            select type (the_number => parse_result%parsed)
            type is (parsed_rational_t)
                fallible_density = fallible_density_t(the_number%value_.unit.self)
            end select
        else
            fallible_density = fallible_density_t(error_list_t(fatal_t( &
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

    function parse_density_unit_c(string) result(fallible_density_unit)
        character(len=*), intent(in) :: string
        type(fallible_density_unit_t) :: fallible_density_unit

        fallible_density_unit = fallible_density_unit_t( &
                parse_density_unit(var_str(string), PROVIDED_UNITS), &
                module_t(MODULE_NAME), &
                procedure_t("parse_density_unit_c"))
    end function

    function parse_density_unit_s(string) result(fallible_density_unit)
        type(varying_string), intent(in) :: string
        type(fallible_density_unit_t) :: fallible_density_unit

        fallible_density_unit = fallible_density_unit_t( &
                parse_density_unit(string, PROVIDED_UNITS), &
                module_t(MODULE_NAME), &
                procedure_t("parse_density_unit_s"))
    end function

    function parse_density_unit_with_units_c( &
            string, units) result(fallible_density_unit)
        character(len=*), intent(in) :: string
        class(density_unit_t), intent(in) :: units(:)
        type(fallible_density_unit_t) :: fallible_density_unit

        fallible_density_unit = fallible_density_unit_t( &
                parse_density_unit(var_str(string), units), &
                module_t(MODULE_NAME), &
                procedure_t("parse_density_unit_with_units_c"))
    end function

    function parse_density_unit_with_units_s( &
            string, units) result(fallible_density_unit)
        type(varying_string), intent(in) :: string
        class(density_unit_t), intent(in) :: units(:)
        type(fallible_density_unit_t) :: fallible_density_unit

        integer :: i
        type(varying_string) :: unit_strings(size(units))

        do i = 1, size(units)
            if (string == units(i)%to_string()) then
                fallible_density_unit = fallible_density_unit_t(units(i))
                return
            end if
        end do
        do i = 1, size(units)
            unit_strings(i) = units(i)%to_string()
        end do
        fallible_density_unit = fallible_density_unit_t(error_list_t(fatal_t( &
                UNKNOWN_UNIT, &
                module_t(MODULE_NAME), &
                procedure_t("parse_density_unit_with_units_s"), &
                '"' // string // '", known units: [' // join(unit_strings, ', ') // ']')))
    end function
end module
