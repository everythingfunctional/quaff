module quaff_fracture_toughness_m
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
            fracture_toughness_t, &
            fallible_fracture_toughness_t, &
            fracture_toughness_unit_t, &
            fallible_fracture_toughness_unit_t, &
            fracture_toughness_simple_unit_t, &
            operator(.unit.), &
            parse_fracture_toughness, &
            parse_fracture_toughness_unit, &
            sum, &
            abs, &
            DEFAULT_OUTPUT_UNITS, &
            PROVIDED_UNITS, &
            PASCAL_ROOT_METER, &
            MEGAPASCAL_ROOT_METER, &
            KSI_ROOT_INCH

    type :: fracture_toughness_t
        double precision :: pascal_root_meter
    contains
        private
        procedure :: to_units
        generic, public :: operator(.in.) => to_units
        procedure, pass(fracture_toughness) :: double_times_fracture_toughness
        procedure, pass(fracture_toughness) :: integer_times_fracture_toughness
        procedure :: fracture_toughness_times_double
        procedure :: fracture_toughness_times_integer
        generic, public :: operator(*) => &
                double_times_fracture_toughness, &
                integer_times_fracture_toughness, &
                fracture_toughness_times_double, &
                fracture_toughness_times_integer
        procedure :: fracture_toughness_divided_by_double
        procedure :: fracture_toughness_divided_by_integer
        procedure :: fracture_toughness_divided_by_fracture_toughness
        generic, public :: operator(/) => &
                fracture_toughness_divided_by_double, &
                fracture_toughness_divided_by_integer, &
                fracture_toughness_divided_by_fracture_toughness
        procedure :: fracture_toughness_plus_fracture_toughness
        generic, public :: operator(+) => fracture_toughness_plus_fracture_toughness
        procedure :: negate_fracture_toughness
        procedure :: fracture_toughness_minus_fracture_toughness
        generic, public :: operator(-) => &
                negate_fracture_toughness, &
                fracture_toughness_minus_fracture_toughness
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

    type :: fallible_fracture_toughness_t
        private
        type(fracture_toughness_t) :: fracture_toughness_ = fracture_toughness_t(0.0d0)
        type(error_list_t) :: errors_
    contains
        private
        procedure, public :: failed => fallible_fracture_toughness_failed
        procedure, public :: fracture_toughness => fallible_fracture_toughness_fracture_toughness
        procedure, public :: errors => fallible_fracture_toughness_errors
    end type

    type, abstract :: fracture_toughness_unit_t
        double precision :: conversion_factor
    contains
        procedure(unit_to_string_i), deferred :: unit_to_string
        procedure(value_to_string_i), deferred :: value_to_string
        generic :: to_string => unit_to_string, value_to_string
        procedure(parse_as_i), deferred :: parse_as
    end type

    type :: fallible_fracture_toughness_unit_t
        private
        class(fracture_toughness_unit_t), allocatable :: unit_
        type(error_list_t) :: errors_
    contains
        private
        procedure, public :: failed => fallible_fracture_toughness_unit_failed
        procedure, public :: unit => fallible_fracture_toughness_unit_unit
        procedure, public :: errors => fallible_fracture_toughness_unit_errors
    end type

    type, extends(fracture_toughness_unit_t) :: fracture_toughness_simple_unit_t
        character(len=20) :: symbol
    contains
        procedure :: unit_to_string => simple_unit_to_string
        procedure :: value_to_string => simple_value_to_string
        procedure :: parse_as => simple_parse_as
    end type

    abstract interface
        elemental function unit_to_string_i(self) result(string)
            import :: fracture_toughness_unit_t, varying_string

            implicit none

            class(fracture_toughness_unit_t), intent(in) :: self
            type(varying_string) :: string
        end function

        pure function value_to_string_i(self, value_) result(string)
            import :: fracture_toughness_unit_t, varying_string

            implicit none

            class(fracture_toughness_unit_t), intent(in) :: self
            type(varying_string), intent(in) :: value_
            type(varying_string) :: string
        end function

        function parse_as_i(self, string) result(fallible_fracture_toughness)
            import :: fracture_toughness_unit_t, fallible_fracture_toughness_t, varying_string

            implicit none

            class(fracture_toughness_unit_t), intent(in) :: self
            type(varying_string), intent(in) :: string
            type(fallible_fracture_toughness_t) :: fallible_fracture_toughness
        end function
    end interface

    interface operator(.unit.)
        module procedure from_units
    end interface

    interface fallible_fracture_toughness_t
        module procedure fallible_fracture_toughness_from_fracture_toughness
        module procedure fallible_fracture_toughness_from_errors
        module procedure fallible_fracture_toughness_from_fallible_fracture_toughness
    end interface

    interface fallible_fracture_toughness_unit_t
        module procedure fallible_fracture_toughness_unit_from_unit
        module procedure fallible_fracture_toughness_unit_from_errors
        module procedure fallible_frct_toughness_unit_from_fallible_frct_toughness_unit
    end interface

    interface parse_fracture_toughness
        module procedure parse_fracture_toughness_c
        module procedure parse_fracture_toughness_s
        module procedure parse_fracture_toughness_with_units_c
        module procedure parse_fracture_toughness_with_units_s
    end interface

    interface parse_fracture_toughness_unit
        module procedure parse_fracture_toughness_unit_c
        module procedure parse_fracture_toughness_unit_s
        module procedure parse_fracture_toughness_unit_with_units_c
        module procedure parse_fracture_toughness_unit_with_units_s
    end interface

    interface abs
        module procedure abs_fracture_toughness
    end interface

    interface sum
        module procedure sum_fracture_toughness
    end interface

    type(fracture_toughness_simple_unit_t), parameter :: KSI_ROOT_INCH = &
            fracture_toughness_simple_unit_t( &
                    conversion_factor = KSI_ROOT_INCH_PER_PASCAL_ROOT_METER, &
                    symbol = "ksi in^0.5")
    type(fracture_toughness_simple_unit_t), parameter :: MEGAPASCAL_ROOT_METER = &
            fracture_toughness_simple_unit_t( &
                    conversion_factor = MEGAPASCAL_ROOT_METER_PER_PASCAL_ROOT_METER, &
                    symbol = "MPa m^0.5")
    type(fracture_toughness_simple_unit_t), parameter :: PASCAL_ROOT_METER = &
            fracture_toughness_simple_unit_t( &
                    conversion_factor = 1.0d0, &
                    symbol = "Pa m^0.5")

    type(fracture_toughness_simple_unit_t) :: DEFAULT_OUTPUT_UNITS = PASCAL_ROOT_METER

    type(fracture_toughness_simple_unit_t), parameter :: PROVIDED_UNITS(*) = &
            [KSI_ROOT_INCH, MEGAPASCAL_ROOT_METER, PASCAL_ROOT_METER]

    character(len=*), parameter :: MODULE_NAME = "quaff_fracture_toughness_m"
contains
    function parse_fracture_toughness_c(string) result(fallible_fracture_toughness)
        character(len=*), intent(in) :: string
        type(fallible_fracture_toughness_t) :: fallible_fracture_toughness

        fallible_fracture_toughness = fallible_fracture_toughness_t( &
                parse_fracture_toughness(var_str(string), PROVIDED_UNITS), &
                module_t(MODULE_NAME), &
                procedure_t("parse_fracture_toughness_c"))
    end function

    function parse_fracture_toughness_s(string) result(fallible_fracture_toughness)
        type(varying_string), intent(in) :: string
        type(fallible_fracture_toughness_t) :: fallible_fracture_toughness

        fallible_fracture_toughness = fallible_fracture_toughness_t( &
                parse_fracture_toughness(string, PROVIDED_UNITS), &
                module_t(MODULE_NAME), &
                procedure_t("parse_fracture_toughness_s"))
    end function

    function parse_fracture_toughness_with_units_c( &
            string, units) result(fallible_fracture_toughness)
        character(len=*), intent(in) :: string
        class(fracture_toughness_unit_t), intent(in) :: units(:)
        type(fallible_fracture_toughness_t) :: fallible_fracture_toughness

        fallible_fracture_toughness = fallible_fracture_toughness_t( &
                parse_fracture_toughness(var_str(string), units), &
                module_t(MODULE_NAME), &
                procedure_t("parse_fracture_toughness_with_units_c"))
    end function

    function parse_fracture_toughness_with_units_s( &
            string, units) result(fallible_fracture_toughness)
        type(varying_string), intent(in) :: string
        class(fracture_toughness_unit_t), intent(in) :: units(:)
        type(fallible_fracture_toughness_t) :: fallible_fracture_toughness

        integer :: i

        do i = 1, size(units)
            fallible_fracture_toughness = units(i)%parse_as(string)
            if (.not. fallible_fracture_toughness%failed()) return
        end do
        fallible_fracture_toughness = fallible_fracture_toughness_t(error_list_t(fatal_t( &
                PARSE_ERROR, &
                module_t(MODULE_NAME), &
                procedure_t("parse_fracture_toughness_with_units_s"), &
                "Unable to parse '" // string // "' as a fracture_toughness_t. Tried with units: " &
                // join(units%to_string(), ", "))))
    end function

    elemental function from_units(value_, units) result(fracture_toughness)
        double precision, intent(in) :: value_
        class(fracture_toughness_unit_t), intent(in) :: units
        type(fracture_toughness_t) :: fracture_toughness

        fracture_toughness%pascal_root_meter = value_ / units%conversion_factor
    end function

    elemental function to_units(self, units) result(fracture_toughness)
        class(fracture_toughness_t), intent(in) :: self
        class(fracture_toughness_unit_t), intent(in) :: units
        double precision :: fracture_toughness

        fracture_toughness = self%pascal_root_meter * units%conversion_factor
    end function

    elemental function double_times_fracture_toughness( &
            multiplier, fracture_toughness) result(new_fracture_toughness)
        double precision, intent(in) :: multiplier
        class(fracture_toughness_t), intent(in) :: fracture_toughness
        type(fracture_toughness_t) :: new_fracture_toughness

        new_fracture_toughness%pascal_root_meter = &
                multiplier * fracture_toughness%pascal_root_meter
    end function

    elemental function integer_times_fracture_toughness( &
            multiplier, fracture_toughness) result(new_fracture_toughness)
        integer, intent(in) :: multiplier
        class(fracture_toughness_t), intent(in) :: fracture_toughness
        type(fracture_toughness_t) :: new_fracture_toughness

        new_fracture_toughness%pascal_root_meter = &
                dble(multiplier) * fracture_toughness%pascal_root_meter
    end function

    elemental function fracture_toughness_times_double( &
            fracture_toughness, multiplier) result(new_fracture_toughness)
        class(fracture_toughness_t), intent(in) :: fracture_toughness
        double precision, intent(in) :: multiplier
        type(fracture_toughness_t) :: new_fracture_toughness

        new_fracture_toughness%pascal_root_meter = &
                fracture_toughness%pascal_root_meter * multiplier
    end function

    elemental function fracture_toughness_times_integer( &
            fracture_toughness, multiplier) result(new_fracture_toughness)
        class(fracture_toughness_t), intent(in) :: fracture_toughness
        integer, intent(in) :: multiplier
        type(fracture_toughness_t) :: new_fracture_toughness

        new_fracture_toughness%pascal_root_meter = &
                fracture_toughness%pascal_root_meter * dble(multiplier)
    end function

    elemental function fracture_toughness_divided_by_double( &
            fracture_toughness, divisor) result(new_fracture_toughness)
        class(fracture_toughness_t), intent(in) :: fracture_toughness
        double precision, intent(in) :: divisor
        type(fracture_toughness_t) :: new_fracture_toughness

        new_fracture_toughness%pascal_root_meter = &
                fracture_toughness%pascal_root_meter / divisor
    end function

    elemental function fracture_toughness_divided_by_integer( &
            fracture_toughness, divisor) result(new_fracture_toughness)
        class(fracture_toughness_t), intent(in) :: fracture_toughness
        integer, intent(in) :: divisor
        type(fracture_toughness_t) :: new_fracture_toughness

        new_fracture_toughness%pascal_root_meter = &
                fracture_toughness%pascal_root_meter / dble(divisor)
    end function

    elemental function fracture_toughness_divided_by_fracture_toughness( &
            numerator, denomenator) result(ratio)
        class(fracture_toughness_t), intent(in) :: numerator
        type(fracture_toughness_t), intent(in) :: denomenator
        double precision :: ratio

        ratio = numerator%pascal_root_meter / denomenator%pascal_root_meter
    end function

    elemental function fracture_toughness_plus_fracture_toughness( &
            lhs, rhs) result(sum_)
        class(fracture_toughness_t), intent(in) :: lhs
        type(fracture_toughness_t), intent(in) :: rhs
        type(fracture_toughness_t) :: sum_

        sum_%pascal_root_meter = lhs%pascal_root_meter + rhs%pascal_root_meter
    end function

    elemental function negate_fracture_toughness(self) result(negated)
        class(fracture_toughness_t), intent(in) :: self
        type(fracture_toughness_t) :: negated

        negated%pascal_root_meter = -self%pascal_root_meter
    end function

    elemental function fracture_toughness_minus_fracture_toughness( &
            lhs, rhs) result(difference)
        class(fracture_toughness_t), intent(in) :: lhs
        type(fracture_toughness_t), intent(in) :: rhs
        type(fracture_toughness_t) :: difference

        difference%pascal_root_meter = lhs%pascal_root_meter - rhs%pascal_root_meter
    end function

    pure function abs_fracture_toughness(fracture_toughness) result(abs_)
        type(fracture_toughness_t), intent(in) :: fracture_toughness
        type(fracture_toughness_t) :: abs_

        abs_%pascal_root_meter = abs(fracture_toughness%pascal_root_meter)
    end function

    pure function sum_fracture_toughness(fracture_toughnesss) result(sum_)
        type(fracture_toughness_t), intent(in) :: fracture_toughnesss(:)
        type(fracture_toughness_t) :: sum_

        sum_%pascal_root_meter = sum(fracture_toughnesss%pascal_root_meter)
    end function

    elemental function greater_than(lhs, rhs)
        class(fracture_toughness_t), intent(in) :: lhs
        type(fracture_toughness_t), intent(in) :: rhs
        logical :: greater_than

        greater_than = lhs%pascal_root_meter > rhs%pascal_root_meter
    end function

    elemental function less_than(lhs, rhs)
        class(fracture_toughness_t), intent(in) :: lhs
        type(fracture_toughness_t), intent(in) :: rhs
        logical :: less_than

        less_than = lhs%pascal_root_meter < rhs%pascal_root_meter
    end function

    elemental function greater_than_or_equal(lhs, rhs)
        class(fracture_toughness_t), intent(in) :: lhs
        type(fracture_toughness_t), intent(in) :: rhs
        logical :: greater_than_or_equal

        greater_than_or_equal = lhs%pascal_root_meter >= rhs%pascal_root_meter
    end function

    elemental function less_than_or_equal(lhs, rhs)
        class(fracture_toughness_t), intent(in) :: lhs
        type(fracture_toughness_t), intent(in) :: rhs
        logical :: less_than_or_equal

        less_than_or_equal = lhs%pascal_root_meter <= rhs%pascal_root_meter
    end function

    elemental function equal_(lhs, rhs)
        class(fracture_toughness_t), intent(in) :: lhs
        type(fracture_toughness_t), intent(in) :: rhs
        logical :: equal_

        equal_ = lhs%pascal_root_meter .safeEq. rhs%pascal_root_meter
    end function

    elemental function equal_within_absolute_(lhs, rhs, within)
        class(fracture_toughness_t), intent(in) :: lhs
        type(fracture_toughness_t), intent(in) :: rhs
        type(fracture_toughness_t), intent(in) :: within
        logical :: equal_within_absolute_

        equal_within_absolute_ = equal_within_absolute( &
                lhs%pascal_root_meter, rhs%pascal_root_meter, within%pascal_root_meter)
    end function

    elemental function equal_within_relative_(lhs, rhs, within)
        class(fracture_toughness_t), intent(in) :: lhs
        type(fracture_toughness_t), intent(in) :: rhs
        double precision, intent(in) :: within
        logical :: equal_within_relative_

        equal_within_relative_ = equal_within_relative( &
                lhs%pascal_root_meter, rhs%pascal_root_meter, within)
    end function

    elemental function not_equal(lhs, rhs)
        class(fracture_toughness_t), intent(in) :: lhs
        type(fracture_toughness_t), intent(in) :: rhs
        logical :: not_equal

        not_equal = .not. lhs == rhs
    end function

    elemental function to_string_full_precision(self) result(string)
        class(fracture_toughness_t), intent(in) :: self
        type(varying_string) :: string

        string = self%to_string_in(DEFAULT_OUTPUT_UNITS)
    end function

    elemental function to_string_with_precision(self, significant_digits) result(string)
        class(fracture_toughness_t), intent(in) :: self
        integer, intent(in) :: significant_digits
        type(varying_string) :: string

        string = self%to_string_in(DEFAULT_OUTPUT_UNITS, significant_digits)
    end function

    elemental function to_string_in_full_precision(self, units) result(string)
        class(fracture_toughness_t), intent(in) :: self
        class(fracture_toughness_unit_t), intent(in) :: units
        type(varying_string) :: string

        string = units%to_string(to_string(self.in.units))
    end function

    elemental function to_string_in_with_precision( &
            self, units, significant_digits) result(string)
        class(fracture_toughness_t), intent(in) :: self
        class(fracture_toughness_unit_t), intent(in) :: units
        integer, intent(in) :: significant_digits
        type(varying_string) :: string

        string = units%to_string(to_string(self.in.units, significant_digits))
    end function

    function fallible_fracture_toughness_from_fracture_toughness( &
            fracture_toughness) result(fallible_fracture_toughness)
        type(fracture_toughness_t), intent(in) :: fracture_toughness
        type(fallible_fracture_toughness_t) :: fallible_fracture_toughness

        fallible_fracture_toughness%fracture_toughness_ = fracture_toughness
    end function

    function fallible_fracture_toughness_from_errors( &
            errors) result(fallible_fracture_toughness)
        type(error_list_t), intent(in) :: errors
        type(fallible_fracture_toughness_t) :: fallible_fracture_toughness

        fallible_fracture_toughness%errors_ = errors
    end function

    function fallible_fracture_toughness_from_fallible_fracture_toughness( &
            fallible_fracture_toughness, &
            module_, &
            procedure_) &
            result(new_fallible_fracture_toughness)
        type(fallible_fracture_toughness_t), intent(in) :: fallible_fracture_toughness
        type(module_t), intent(in) :: module_
        type(procedure_t), intent(in) :: procedure_
        type(fallible_fracture_toughness_t) :: new_fallible_fracture_toughness

        if (fallible_fracture_toughness%failed()) then
            new_fallible_fracture_toughness%errors_ = error_list_t( &
                    fallible_fracture_toughness%errors_, module_, procedure_)
        else
            new_fallible_fracture_toughness%fracture_toughness_ = fallible_fracture_toughness%fracture_toughness_
        end if
    end function

    elemental function fallible_fracture_toughness_failed( &
            self) result(failed)
        class(fallible_fracture_toughness_t), intent(in) :: self
        logical :: failed

        failed = self%errors_%has_any()
    end function

    elemental function fallible_fracture_toughness_fracture_toughness( &
            self) result(fracture_toughness)
        class(fallible_fracture_toughness_t), intent(in) :: self
        type(fracture_toughness_t) :: fracture_toughness

        fracture_toughness = self%fracture_toughness_
    end function

    impure elemental function fallible_fracture_toughness_errors( &
            self) result(errors)
        class(fallible_fracture_toughness_t), intent(in) :: self
        type(error_list_t) :: errors

        errors = self%errors_
    end function

    function fallible_fracture_toughness_unit_from_unit( &
            unit) result(fallible_fracture_toughness_unit)
        class(fracture_toughness_unit_t), intent(in) :: unit
        type(fallible_fracture_toughness_unit_t) :: fallible_fracture_toughness_unit

        allocate(fallible_fracture_toughness_unit%unit_, source = unit)
    end function

    function fallible_fracture_toughness_unit_from_errors( &
            errors) result(fallible_fracture_toughness_unit)
        type(error_list_t), intent(in) :: errors
        type(fallible_fracture_toughness_unit_t) :: fallible_fracture_toughness_unit

        fallible_fracture_toughness_unit%errors_ = errors
    end function

    function fallible_frct_toughness_unit_from_fallible_frct_toughness_unit( &
            fallible_fracture_toughness_unit, &
            module_, &
            procedure_) &
            result(new_fallible_fracture_toughness_unit)
        type(fallible_fracture_toughness_unit_t), intent(in) :: fallible_fracture_toughness_unit
        type(module_t), intent(in) :: module_
        type(procedure_t), intent(in) :: procedure_
        type(fallible_fracture_toughness_unit_t) :: new_fallible_fracture_toughness_unit

        if (fallible_fracture_toughness_unit%failed()) then
            new_fallible_fracture_toughness_unit%errors_ = error_list_t( &
                    fallible_fracture_toughness_unit%errors_, module_, procedure_)
        else
            allocate(new_fallible_fracture_toughness_unit%unit_, source = &
                    fallible_fracture_toughness_unit%unit_)
        end if
    end function

    elemental function fallible_fracture_toughness_unit_failed( &
            self) result(failed)
        class(fallible_fracture_toughness_unit_t), intent(in) :: self
        logical :: failed

        failed = self%errors_%has_any()
    end function

    function fallible_fracture_toughness_unit_unit( &
            self) result(unit)
        class(fallible_fracture_toughness_unit_t), intent(in) :: self
        class(fracture_toughness_unit_t), allocatable :: unit

        allocate(unit, source = self%unit_)
    end function

    impure elemental function fallible_fracture_toughness_unit_errors( &
            self) result(errors)
        class(fallible_fracture_toughness_unit_t), intent(in) :: self
        type(error_list_t) :: errors

        errors = self%errors_
    end function

    elemental function simple_unit_to_string(self) result(string)
        class(fracture_toughness_simple_unit_t), intent(in) :: self
        type(varying_string) :: string

        string = trim(self%symbol)
    end function

    pure function simple_value_to_string(self, value_) result(string)
        class(fracture_toughness_simple_unit_t), intent(in) :: self
        type(varying_string), intent(in) :: value_
        type(varying_string) :: string

        string = value_ // " " // self%to_string()
    end function

    function simple_parse_as(self, string) result(fallible_fracture_toughness)
        class(fracture_toughness_simple_unit_t), intent(in) :: self
        type(varying_string), intent(in) :: string
        type(fallible_fracture_toughness_t) :: fallible_fracture_toughness

        type(parse_result_t) :: parse_result

        parse_result = parse_with(the_parser, string)
        if (parse_result%ok) then
            select type (the_number => parse_result%parsed)
            type is (parsed_rational_t)
                fallible_fracture_toughness = fallible_fracture_toughness_t(the_number%value_.unit.self)
            end select
        else
            fallible_fracture_toughness = fallible_fracture_toughness_t(error_list_t(fatal_t( &
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

    function parse_fracture_toughness_unit_c(string) result(fallible_fracture_toughness_unit)
        character(len=*), intent(in) :: string
        type(fallible_fracture_toughness_unit_t) :: fallible_fracture_toughness_unit

        fallible_fracture_toughness_unit = fallible_fracture_toughness_unit_t( &
                parse_fracture_toughness_unit(var_str(string), PROVIDED_UNITS), &
                module_t(MODULE_NAME), &
                procedure_t("parse_fracture_toughness_unit_c"))
    end function

    function parse_fracture_toughness_unit_s(string) result(fallible_fracture_toughness_unit)
        type(varying_string), intent(in) :: string
        type(fallible_fracture_toughness_unit_t) :: fallible_fracture_toughness_unit

        fallible_fracture_toughness_unit = fallible_fracture_toughness_unit_t( &
                parse_fracture_toughness_unit(string, PROVIDED_UNITS), &
                module_t(MODULE_NAME), &
                procedure_t("parse_fracture_toughness_unit_s"))
    end function

    function parse_fracture_toughness_unit_with_units_c( &
            string, units) result(fallible_fracture_toughness_unit)
        character(len=*), intent(in) :: string
        class(fracture_toughness_unit_t), intent(in) :: units(:)
        type(fallible_fracture_toughness_unit_t) :: fallible_fracture_toughness_unit

        fallible_fracture_toughness_unit = fallible_fracture_toughness_unit_t( &
                parse_fracture_toughness_unit(var_str(string), units), &
                module_t(MODULE_NAME), &
                procedure_t("parse_fracture_toughness_unit_with_units_c"))
    end function

    function parse_fracture_toughness_unit_with_units_s( &
            string, units) result(fallible_fracture_toughness_unit)
        type(varying_string), intent(in) :: string
        class(fracture_toughness_unit_t), intent(in) :: units(:)
        type(fallible_fracture_toughness_unit_t) :: fallible_fracture_toughness_unit

        integer :: i
        type(varying_string), allocatable :: unit_strings(:)

        do i = 1, size(units)
            if (string == units(i)%to_string()) then
                fallible_fracture_toughness_unit = fallible_fracture_toughness_unit_t(units(i))
                return
            end if
        end do
        allocate(unit_strings(size(units)))
        do i = 1, size(units)
            unit_strings(i) = units(i)%to_string()
        end do
        fallible_fracture_toughness_unit = fallible_fracture_toughness_unit_t(error_list_t(fatal_t( &
                UNKNOWN_UNIT, &
                module_t(MODULE_NAME), &
                procedure_t("parse_fracture_toughness_unit_with_units_s"), &
                '"' // string // '", known units: [' // join(unit_strings, ', ') // ']')))
    end function
end module
