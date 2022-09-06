module quaff_molar_mass_m
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
    use quaff_conversion_factors_m, only: GRAMS_PER_MOL_PER_KILOGRAMS_PER_MOL
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
            molar_mass_t, &
            fallible_molar_mass_t, &
            molar_mass_unit_t, &
            fallible_molar_mass_unit_t, &
            molar_mass_simple_unit_t, &
            operator(.unit.), &
            parse_molar_mass, &
            parse_molar_mass_unit, &
            sum, &
            abs, &
            DEFAULT_OUTPUT_UNITS, &
            PROVIDED_UNITS, &
            GRAMS_PER_MOL, &
            KILOGRAMS_PER_MOL

    type :: molar_mass_t
        double precision :: kilograms_per_mol
    contains
        private
        procedure :: to_units
        generic, public :: operator(.in.) => to_units
        procedure, pass(molar_mass) :: double_times_molar_mass
        procedure, pass(molar_mass) :: integer_times_molar_mass
        procedure :: molar_mass_times_double
        procedure :: molar_mass_times_integer
        generic, public :: operator(*) => &
                double_times_molar_mass, &
                integer_times_molar_mass, &
                molar_mass_times_double, &
                molar_mass_times_integer
        procedure :: molar_mass_divided_by_double
        procedure :: molar_mass_divided_by_integer
        procedure :: molar_mass_divided_by_molar_mass
        generic, public :: operator(/) => &
                molar_mass_divided_by_double, &
                molar_mass_divided_by_integer, &
                molar_mass_divided_by_molar_mass
        procedure :: molar_mass_plus_molar_mass
        generic, public :: operator(+) => molar_mass_plus_molar_mass
        procedure :: negate_molar_mass
        procedure :: molar_mass_minus_molar_mass
        generic, public :: operator(-) => &
                negate_molar_mass, &
                molar_mass_minus_molar_mass
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

    type :: fallible_molar_mass_t
        private
        type(molar_mass_t) :: molar_mass_ = molar_mass_t(0.0d0)
        type(error_list_t) :: errors_
    contains
        private
        procedure, public :: failed => fallible_molar_mass_failed
        procedure, public :: molar_mass => fallible_molar_mass_molar_mass
        procedure, public :: errors => fallible_molar_mass_errors
    end type

    type, abstract :: molar_mass_unit_t
        double precision :: conversion_factor
    contains
        procedure(unit_to_string_i), deferred :: unit_to_string
        procedure(value_to_string_i), deferred :: value_to_string
        generic :: to_string => unit_to_string, value_to_string
        procedure(parse_as_i), deferred :: parse_as
    end type

    type :: fallible_molar_mass_unit_t
        private
        class(molar_mass_unit_t), allocatable :: unit_
        type(error_list_t) :: errors_
    contains
        private
        procedure, public :: failed => fallible_molar_mass_unit_failed
        procedure, public :: unit => fallible_molar_mass_unit_unit
        procedure, public :: errors => fallible_molar_mass_unit_errors
    end type

    type, extends(molar_mass_unit_t) :: molar_mass_simple_unit_t
        character(len=20) :: symbol
    contains
        procedure :: unit_to_string => simple_unit_to_string
        procedure :: value_to_string => simple_value_to_string
        procedure :: parse_as => simple_parse_as
    end type

    abstract interface
        elemental function unit_to_string_i(self) result(string)
            import :: molar_mass_unit_t, varying_string

            implicit none

            class(molar_mass_unit_t), intent(in) :: self
            type(varying_string) :: string
        end function

        pure function value_to_string_i(self, value_) result(string)
            import :: molar_mass_unit_t, varying_string

            implicit none

            class(molar_mass_unit_t), intent(in) :: self
            type(varying_string), intent(in) :: value_
            type(varying_string) :: string
        end function

        function parse_as_i(self, string) result(fallible_molar_mass)
            import :: molar_mass_unit_t, fallible_molar_mass_t, varying_string

            implicit none

            class(molar_mass_unit_t), intent(in) :: self
            type(varying_string), intent(in) :: string
            type(fallible_molar_mass_t) :: fallible_molar_mass
        end function
    end interface

    interface operator(.unit.)
        module procedure from_units
    end interface

    interface fallible_molar_mass_t
        module procedure fallible_molar_mass_from_molar_mass
        module procedure fallible_molar_mass_from_errors
        module procedure fallible_molar_mass_from_fallible_molar_mass
    end interface

    interface fallible_molar_mass_unit_t
        module procedure fallible_molar_mass_unit_from_unit
        module procedure fallible_molar_mass_unit_from_errors
        module procedure fallible_molar_mass_unit_from_fallible_molar_mass_unit
    end interface

    interface parse_molar_mass
        module procedure parse_molar_mass_c
        module procedure parse_molar_mass_s
        module procedure parse_molar_mass_with_units_c
        module procedure parse_molar_mass_with_units_s
    end interface

    interface parse_molar_mass_unit
        module procedure parse_molar_mass_unit_c
        module procedure parse_molar_mass_unit_s
        module procedure parse_molar_mass_unit_with_units_c
        module procedure parse_molar_mass_unit_with_units_s
    end interface

    interface abs
        module procedure abs_molar_mass
    end interface

    interface sum
        module procedure sum_molar_mass
    end interface

    type(molar_mass_simple_unit_t), parameter :: GRAMS_PER_MOL = &
            molar_mass_simple_unit_t( &
                    conversion_factor = GRAMS_PER_MOL_PER_KILOGRAMS_PER_MOL, &
                    symbol = "g/mol")
    type(molar_mass_simple_unit_t), parameter :: KILOGRAMS_PER_MOL = &
            molar_mass_simple_unit_t( &
                    conversion_factor = 1.0d0, &
                    symbol = "kg/mol")

    type(molar_mass_simple_unit_t) :: DEFAULT_OUTPUT_UNITS = KILOGRAMS_PER_MOL

    type(molar_mass_simple_unit_t), parameter :: PROVIDED_UNITS(*) = &
            [GRAMS_PER_MOL, KILOGRAMS_PER_MOL]

    character(len=*), parameter :: MODULE_NAME = "quaff_molar_mass_m"
contains
    function parse_molar_mass_c(string) result(fallible_molar_mass)
        character(len=*), intent(in) :: string
        type(fallible_molar_mass_t) :: fallible_molar_mass

        fallible_molar_mass = fallible_molar_mass_t( &
                parse_molar_mass(var_str(string), PROVIDED_UNITS), &
                module_t(MODULE_NAME), &
                procedure_t("parse_molar_mass_c"))
    end function

    function parse_molar_mass_s(string) result(fallible_molar_mass)
        type(varying_string), intent(in) :: string
        type(fallible_molar_mass_t) :: fallible_molar_mass

        fallible_molar_mass = fallible_molar_mass_t( &
                parse_molar_mass(string, PROVIDED_UNITS), &
                module_t(MODULE_NAME), &
                procedure_t("parse_molar_mass_s"))
    end function

    function parse_molar_mass_with_units_c( &
            string, units) result(fallible_molar_mass)
        character(len=*), intent(in) :: string
        class(molar_mass_unit_t), intent(in) :: units(:)
        type(fallible_molar_mass_t) :: fallible_molar_mass

        fallible_molar_mass = fallible_molar_mass_t( &
                parse_molar_mass(var_str(string), units), &
                module_t(MODULE_NAME), &
                procedure_t("parse_molar_mass_with_units_c"))
    end function

    function parse_molar_mass_with_units_s( &
            string, units) result(fallible_molar_mass)
        type(varying_string), intent(in) :: string
        class(molar_mass_unit_t), intent(in) :: units(:)
        type(fallible_molar_mass_t) :: fallible_molar_mass

        integer :: i

        do i = 1, size(units)
            fallible_molar_mass = units(i)%parse_as(string)
            if (.not. fallible_molar_mass%failed()) return
        end do
        fallible_molar_mass = fallible_molar_mass_t(error_list_t(fatal_t( &
                PARSE_ERROR, &
                module_t(MODULE_NAME), &
                procedure_t("parse_molar_mass_with_units_s"), &
                "Unable to parse '" // string // "' as a molar_mass_t. Tried with units: " &
                // join(units%to_string(), ", "))))
    end function

    elemental function from_units(value_, units) result(molar_mass)
        double precision, intent(in) :: value_
        class(molar_mass_unit_t), intent(in) :: units
        type(molar_mass_t) :: molar_mass

        molar_mass%kilograms_per_mol = value_ / units%conversion_factor
    end function

    elemental function to_units(self, units) result(molar_mass)
        class(molar_mass_t), intent(in) :: self
        class(molar_mass_unit_t), intent(in) :: units
        double precision :: molar_mass

        molar_mass = self%kilograms_per_mol * units%conversion_factor
    end function

    elemental function double_times_molar_mass( &
            multiplier, molar_mass) result(new_molar_mass)
        double precision, intent(in) :: multiplier
        class(molar_mass_t), intent(in) :: molar_mass
        type(molar_mass_t) :: new_molar_mass

        new_molar_mass%kilograms_per_mol = &
                multiplier * molar_mass%kilograms_per_mol
    end function

    elemental function integer_times_molar_mass( &
            multiplier, molar_mass) result(new_molar_mass)
        integer, intent(in) :: multiplier
        class(molar_mass_t), intent(in) :: molar_mass
        type(molar_mass_t) :: new_molar_mass

        new_molar_mass%kilograms_per_mol = &
                dble(multiplier) * molar_mass%kilograms_per_mol
    end function

    elemental function molar_mass_times_double( &
            molar_mass, multiplier) result(new_molar_mass)
        class(molar_mass_t), intent(in) :: molar_mass
        double precision, intent(in) :: multiplier
        type(molar_mass_t) :: new_molar_mass

        new_molar_mass%kilograms_per_mol = &
                molar_mass%kilograms_per_mol * multiplier
    end function

    elemental function molar_mass_times_integer( &
            molar_mass, multiplier) result(new_molar_mass)
        class(molar_mass_t), intent(in) :: molar_mass
        integer, intent(in) :: multiplier
        type(molar_mass_t) :: new_molar_mass

        new_molar_mass%kilograms_per_mol = &
                molar_mass%kilograms_per_mol * dble(multiplier)
    end function

    elemental function molar_mass_divided_by_double( &
            molar_mass, divisor) result(new_molar_mass)
        class(molar_mass_t), intent(in) :: molar_mass
        double precision, intent(in) :: divisor
        type(molar_mass_t) :: new_molar_mass

        new_molar_mass%kilograms_per_mol = &
                molar_mass%kilograms_per_mol / divisor
    end function

    elemental function molar_mass_divided_by_integer( &
            molar_mass, divisor) result(new_molar_mass)
        class(molar_mass_t), intent(in) :: molar_mass
        integer, intent(in) :: divisor
        type(molar_mass_t) :: new_molar_mass

        new_molar_mass%kilograms_per_mol = &
                molar_mass%kilograms_per_mol / dble(divisor)
    end function

    elemental function molar_mass_divided_by_molar_mass( &
            numerator, denomenator) result(ratio)
        class(molar_mass_t), intent(in) :: numerator
        type(molar_mass_t), intent(in) :: denomenator
        double precision :: ratio

        ratio = numerator%kilograms_per_mol / denomenator%kilograms_per_mol
    end function

    elemental function molar_mass_plus_molar_mass( &
            lhs, rhs) result(sum_)
        class(molar_mass_t), intent(in) :: lhs
        type(molar_mass_t), intent(in) :: rhs
        type(molar_mass_t) :: sum_

        sum_%kilograms_per_mol = lhs%kilograms_per_mol + rhs%kilograms_per_mol
    end function

    elemental function negate_molar_mass(self) result(negated)
        class(molar_mass_t), intent(in) :: self
        type(molar_mass_t) :: negated

        negated%kilograms_per_mol = -self%kilograms_per_mol
    end function

    elemental function molar_mass_minus_molar_mass( &
            lhs, rhs) result(difference)
        class(molar_mass_t), intent(in) :: lhs
        type(molar_mass_t), intent(in) :: rhs
        type(molar_mass_t) :: difference

        difference%kilograms_per_mol = lhs%kilograms_per_mol - rhs%kilograms_per_mol
    end function

    pure function abs_molar_mass(molar_mass) result(abs_)
        type(molar_mass_t), intent(in) :: molar_mass
        type(molar_mass_t) :: abs_

        abs_%kilograms_per_mol = abs(molar_mass%kilograms_per_mol)
    end function

    pure function sum_molar_mass(molar_masss) result(sum_)
        type(molar_mass_t), intent(in) :: molar_masss(:)
        type(molar_mass_t) :: sum_

        sum_%kilograms_per_mol = sum(molar_masss%kilograms_per_mol)
    end function

    elemental function greater_than(lhs, rhs)
        class(molar_mass_t), intent(in) :: lhs
        type(molar_mass_t), intent(in) :: rhs
        logical :: greater_than

        greater_than = lhs%kilograms_per_mol > rhs%kilograms_per_mol
    end function

    elemental function less_than(lhs, rhs)
        class(molar_mass_t), intent(in) :: lhs
        type(molar_mass_t), intent(in) :: rhs
        logical :: less_than

        less_than = lhs%kilograms_per_mol < rhs%kilograms_per_mol
    end function

    elemental function greater_than_or_equal(lhs, rhs)
        class(molar_mass_t), intent(in) :: lhs
        type(molar_mass_t), intent(in) :: rhs
        logical :: greater_than_or_equal

        greater_than_or_equal = lhs%kilograms_per_mol >= rhs%kilograms_per_mol
    end function

    elemental function less_than_or_equal(lhs, rhs)
        class(molar_mass_t), intent(in) :: lhs
        type(molar_mass_t), intent(in) :: rhs
        logical :: less_than_or_equal

        less_than_or_equal = lhs%kilograms_per_mol <= rhs%kilograms_per_mol
    end function

    elemental function equal_(lhs, rhs)
        class(molar_mass_t), intent(in) :: lhs
        type(molar_mass_t), intent(in) :: rhs
        logical :: equal_

        equal_ = lhs%kilograms_per_mol .safeEq. rhs%kilograms_per_mol
    end function

    elemental function equal_within_absolute_(lhs, rhs, within)
        class(molar_mass_t), intent(in) :: lhs
        type(molar_mass_t), intent(in) :: rhs
        type(molar_mass_t), intent(in) :: within
        logical :: equal_within_absolute_

        equal_within_absolute_ = equal_within_absolute( &
                lhs%kilograms_per_mol, rhs%kilograms_per_mol, within%kilograms_per_mol)
    end function

    elemental function equal_within_relative_(lhs, rhs, within)
        class(molar_mass_t), intent(in) :: lhs
        type(molar_mass_t), intent(in) :: rhs
        double precision, intent(in) :: within
        logical :: equal_within_relative_

        equal_within_relative_ = equal_within_relative( &
                lhs%kilograms_per_mol, rhs%kilograms_per_mol, within)
    end function

    elemental function not_equal(lhs, rhs)
        class(molar_mass_t), intent(in) :: lhs
        type(molar_mass_t), intent(in) :: rhs
        logical :: not_equal

        not_equal = .not. lhs == rhs
    end function

    elemental function to_string_full_precision(self) result(string)
        class(molar_mass_t), intent(in) :: self
        type(varying_string) :: string

        string = self%to_string_in(DEFAULT_OUTPUT_UNITS)
    end function

    elemental function to_string_with_precision(self, significant_digits) result(string)
        class(molar_mass_t), intent(in) :: self
        integer, intent(in) :: significant_digits
        type(varying_string) :: string

        string = self%to_string_in(DEFAULT_OUTPUT_UNITS, significant_digits)
    end function

    elemental function to_string_in_full_precision(self, units) result(string)
        class(molar_mass_t), intent(in) :: self
        class(molar_mass_unit_t), intent(in) :: units
        type(varying_string) :: string

        string = units%to_string(to_string(self.in.units))
    end function

    elemental function to_string_in_with_precision( &
            self, units, significant_digits) result(string)
        class(molar_mass_t), intent(in) :: self
        class(molar_mass_unit_t), intent(in) :: units
        integer, intent(in) :: significant_digits
        type(varying_string) :: string

        string = units%to_string(to_string(self.in.units, significant_digits))
    end function

    function fallible_molar_mass_from_molar_mass( &
            molar_mass) result(fallible_molar_mass)
        type(molar_mass_t), intent(in) :: molar_mass
        type(fallible_molar_mass_t) :: fallible_molar_mass

        fallible_molar_mass%molar_mass_ = molar_mass
    end function

    function fallible_molar_mass_from_errors( &
            errors) result(fallible_molar_mass)
        type(error_list_t), intent(in) :: errors
        type(fallible_molar_mass_t) :: fallible_molar_mass

        fallible_molar_mass%errors_ = errors
    end function

    function fallible_molar_mass_from_fallible_molar_mass( &
            fallible_molar_mass, &
            module_, &
            procedure_) &
            result(new_fallible_molar_mass)
        type(fallible_molar_mass_t), intent(in) :: fallible_molar_mass
        type(module_t), intent(in) :: module_
        type(procedure_t), intent(in) :: procedure_
        type(fallible_molar_mass_t) :: new_fallible_molar_mass

        if (fallible_molar_mass%failed()) then
            new_fallible_molar_mass%errors_ = error_list_t( &
                    fallible_molar_mass%errors_, module_, procedure_)
        else
            new_fallible_molar_mass%molar_mass_ = fallible_molar_mass%molar_mass_
        end if
    end function

    elemental function fallible_molar_mass_failed( &
            self) result(failed)
        class(fallible_molar_mass_t), intent(in) :: self
        logical :: failed

        failed = self%errors_%has_any()
    end function

    elemental function fallible_molar_mass_molar_mass( &
            self) result(molar_mass)
        class(fallible_molar_mass_t), intent(in) :: self
        type(molar_mass_t) :: molar_mass

        molar_mass = self%molar_mass_
    end function

    impure elemental function fallible_molar_mass_errors( &
            self) result(errors)
        class(fallible_molar_mass_t), intent(in) :: self
        type(error_list_t) :: errors

        errors = self%errors_
    end function

    function fallible_molar_mass_unit_from_unit( &
            unit) result(fallible_molar_mass_unit)
        class(molar_mass_unit_t), intent(in) :: unit
        type(fallible_molar_mass_unit_t) :: fallible_molar_mass_unit

        allocate(fallible_molar_mass_unit%unit_, source = unit)
    end function

    function fallible_molar_mass_unit_from_errors( &
            errors) result(fallible_molar_mass_unit)
        type(error_list_t), intent(in) :: errors
        type(fallible_molar_mass_unit_t) :: fallible_molar_mass_unit

        fallible_molar_mass_unit%errors_ = errors
    end function

    function fallible_molar_mass_unit_from_fallible_molar_mass_unit( &
            fallible_molar_mass_unit, &
            module_, &
            procedure_) &
            result(new_fallible_molar_mass_unit)
        type(fallible_molar_mass_unit_t), intent(in) :: fallible_molar_mass_unit
        type(module_t), intent(in) :: module_
        type(procedure_t), intent(in) :: procedure_
        type(fallible_molar_mass_unit_t) :: new_fallible_molar_mass_unit

        if (fallible_molar_mass_unit%failed()) then
            new_fallible_molar_mass_unit%errors_ = error_list_t( &
                    fallible_molar_mass_unit%errors_, module_, procedure_)
        else
            allocate(new_fallible_molar_mass_unit%unit_, source = &
                    fallible_molar_mass_unit%unit_)
        end if
    end function

    elemental function fallible_molar_mass_unit_failed( &
            self) result(failed)
        class(fallible_molar_mass_unit_t), intent(in) :: self
        logical :: failed

        failed = self%errors_%has_any()
    end function

    function fallible_molar_mass_unit_unit( &
            self) result(unit)
        class(fallible_molar_mass_unit_t), intent(in) :: self
        class(molar_mass_unit_t), allocatable :: unit

        allocate(unit, source = self%unit_)
    end function

    impure elemental function fallible_molar_mass_unit_errors( &
            self) result(errors)
        class(fallible_molar_mass_unit_t), intent(in) :: self
        type(error_list_t) :: errors

        errors = self%errors_
    end function

    elemental function simple_unit_to_string(self) result(string)
        class(molar_mass_simple_unit_t), intent(in) :: self
        type(varying_string) :: string

        string = trim(self%symbol)
    end function

    pure function simple_value_to_string(self, value_) result(string)
        class(molar_mass_simple_unit_t), intent(in) :: self
        type(varying_string), intent(in) :: value_
        type(varying_string) :: string

        string = value_ // " " // self%to_string()
    end function

    function simple_parse_as(self, string) result(fallible_molar_mass)
        class(molar_mass_simple_unit_t), intent(in) :: self
        type(varying_string), intent(in) :: string
        type(fallible_molar_mass_t) :: fallible_molar_mass

        type(parse_result_t) :: parse_result

        parse_result = parse_with(the_parser, string)
        if (parse_result%ok) then
            select type (the_number => parse_result%parsed)
            type is (parsed_rational_t)
                fallible_molar_mass = fallible_molar_mass_t(the_number%value_.unit.self)
            end select
        else
            fallible_molar_mass = fallible_molar_mass_t(error_list_t(fatal_t( &
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

    function parse_molar_mass_unit_c(string) result(fallible_molar_mass_unit)
        character(len=*), intent(in) :: string
        type(fallible_molar_mass_unit_t) :: fallible_molar_mass_unit

        fallible_molar_mass_unit = fallible_molar_mass_unit_t( &
                parse_molar_mass_unit(var_str(string), PROVIDED_UNITS), &
                module_t(MODULE_NAME), &
                procedure_t("parse_molar_mass_unit_c"))
    end function

    function parse_molar_mass_unit_s(string) result(fallible_molar_mass_unit)
        type(varying_string), intent(in) :: string
        type(fallible_molar_mass_unit_t) :: fallible_molar_mass_unit

        fallible_molar_mass_unit = fallible_molar_mass_unit_t( &
                parse_molar_mass_unit(string, PROVIDED_UNITS), &
                module_t(MODULE_NAME), &
                procedure_t("parse_molar_mass_unit_s"))
    end function

    function parse_molar_mass_unit_with_units_c( &
            string, units) result(fallible_molar_mass_unit)
        character(len=*), intent(in) :: string
        class(molar_mass_unit_t), intent(in) :: units(:)
        type(fallible_molar_mass_unit_t) :: fallible_molar_mass_unit

        fallible_molar_mass_unit = fallible_molar_mass_unit_t( &
                parse_molar_mass_unit(var_str(string), units), &
                module_t(MODULE_NAME), &
                procedure_t("parse_molar_mass_unit_with_units_c"))
    end function

    function parse_molar_mass_unit_with_units_s( &
            string, units) result(fallible_molar_mass_unit)
        type(varying_string), intent(in) :: string
        class(molar_mass_unit_t), intent(in) :: units(:)
        type(fallible_molar_mass_unit_t) :: fallible_molar_mass_unit

        integer :: i
        type(varying_string), allocatable :: unit_strings(:)

        do i = 1, size(units)
            if (string == units(i)%to_string()) then
                fallible_molar_mass_unit = fallible_molar_mass_unit_t(units(i))
                return
            end if
        end do
        allocate(unit_strings(size(units)))
        do i = 1, size(units)
            unit_strings(i) = units(i)%to_string()
        end do
        fallible_molar_mass_unit = fallible_molar_mass_unit_t(error_list_t(fatal_t( &
                UNKNOWN_UNIT, &
                module_t(MODULE_NAME), &
                procedure_t("parse_molar_mass_unit_with_units_s"), &
                '"' // string // '", known units: [' // join(unit_strings, ', ') // ']')))
    end function
end module
