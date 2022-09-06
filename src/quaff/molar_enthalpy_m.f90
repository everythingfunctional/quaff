module quaff_molar_enthalpy_m
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
    use quaff_conversion_factors_m, only: KILOJOULES_PER_MOL_PER_JOULES_PER_MOL
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
            molar_enthalpy_t, &
            fallible_molar_enthalpy_t, &
            molar_enthalpy_unit_t, &
            fallible_molar_enthalpy_unit_t, &
            molar_enthalpy_simple_unit_t, &
            operator(.unit.), &
            parse_molar_enthalpy, &
            parse_molar_enthalpy_unit, &
            sum, &
            abs, &
            DEFAULT_OUTPUT_UNITS, &
            PROVIDED_UNITS, &
            JOULES_PER_MOL, &
            KILOJOULES_PER_MOL

    type :: molar_enthalpy_t
        double precision :: joules_per_mol
    contains
        private
        procedure :: to_units
        generic, public :: operator(.in.) => to_units
        procedure, pass(molar_enthalpy) :: double_times_molar_enthalpy
        procedure, pass(molar_enthalpy) :: integer_times_molar_enthalpy
        procedure :: molar_enthalpy_times_double
        procedure :: molar_enthalpy_times_integer
        generic, public :: operator(*) => &
                double_times_molar_enthalpy, &
                integer_times_molar_enthalpy, &
                molar_enthalpy_times_double, &
                molar_enthalpy_times_integer
        procedure :: molar_enthalpy_divided_by_double
        procedure :: molar_enthalpy_divided_by_integer
        procedure :: molar_enthalpy_divided_by_molar_enthalpy
        generic, public :: operator(/) => &
                molar_enthalpy_divided_by_double, &
                molar_enthalpy_divided_by_integer, &
                molar_enthalpy_divided_by_molar_enthalpy
        procedure :: molar_enthalpy_plus_molar_enthalpy
        generic, public :: operator(+) => molar_enthalpy_plus_molar_enthalpy
        procedure :: negate_molar_enthalpy
        procedure :: molar_enthalpy_minus_molar_enthalpy
        generic, public :: operator(-) => &
                negate_molar_enthalpy, &
                molar_enthalpy_minus_molar_enthalpy
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

    type :: fallible_molar_enthalpy_t
        private
        type(molar_enthalpy_t) :: molar_enthalpy_ = molar_enthalpy_t(0.0d0)
        type(error_list_t) :: errors_
    contains
        private
        procedure, public :: failed => fallible_molar_enthalpy_failed
        procedure, public :: molar_enthalpy => fallible_molar_enthalpy_molar_enthalpy
        procedure, public :: errors => fallible_molar_enthalpy_errors
    end type

    type, abstract :: molar_enthalpy_unit_t
        double precision :: conversion_factor
    contains
        procedure(unit_to_string_i), deferred :: unit_to_string
        procedure(value_to_string_i), deferred :: value_to_string
        generic :: to_string => unit_to_string, value_to_string
        procedure(parse_as_i), deferred :: parse_as
    end type

    type :: fallible_molar_enthalpy_unit_t
        private
        class(molar_enthalpy_unit_t), allocatable :: unit_
        type(error_list_t) :: errors_
    contains
        private
        procedure, public :: failed => fallible_molar_enthalpy_unit_failed
        procedure, public :: unit => fallible_molar_enthalpy_unit_unit
        procedure, public :: errors => fallible_molar_enthalpy_unit_errors
    end type

    type, extends(molar_enthalpy_unit_t) :: molar_enthalpy_simple_unit_t
        character(len=20) :: symbol
    contains
        procedure :: unit_to_string => simple_unit_to_string
        procedure :: value_to_string => simple_value_to_string
        procedure :: parse_as => simple_parse_as
    end type

    abstract interface
        elemental function unit_to_string_i(self) result(string)
            import :: molar_enthalpy_unit_t, varying_string

            implicit none

            class(molar_enthalpy_unit_t), intent(in) :: self
            type(varying_string) :: string
        end function

        pure function value_to_string_i(self, value_) result(string)
            import :: molar_enthalpy_unit_t, varying_string

            implicit none

            class(molar_enthalpy_unit_t), intent(in) :: self
            type(varying_string), intent(in) :: value_
            type(varying_string) :: string
        end function

        function parse_as_i(self, string) result(fallible_molar_enthalpy)
            import :: molar_enthalpy_unit_t, fallible_molar_enthalpy_t, varying_string

            implicit none

            class(molar_enthalpy_unit_t), intent(in) :: self
            type(varying_string), intent(in) :: string
            type(fallible_molar_enthalpy_t) :: fallible_molar_enthalpy
        end function
    end interface

    interface operator(.unit.)
        module procedure from_units
    end interface

    interface fallible_molar_enthalpy_t
        module procedure fallible_molar_enthalpy_from_molar_enthalpy
        module procedure fallible_molar_enthalpy_from_errors
        module procedure fallible_molar_enthalpy_from_fallible_molar_enthalpy
    end interface

    interface fallible_molar_enthalpy_unit_t
        module procedure fallible_molar_enthalpy_unit_from_unit
        module procedure fallible_molar_enthalpy_unit_from_errors
        module procedure fallible_unit_from_fallible_unit
    end interface

    interface parse_molar_enthalpy
        module procedure parse_molar_enthalpy_c
        module procedure parse_molar_enthalpy_s
        module procedure parse_molar_enthalpy_with_units_c
        module procedure parse_molar_enthalpy_with_units_s
    end interface

    interface parse_molar_enthalpy_unit
        module procedure parse_molar_enthalpy_unit_c
        module procedure parse_molar_enthalpy_unit_s
        module procedure parse_molar_enthalpy_unit_with_units_c
        module procedure parse_molar_enthalpy_unit_with_units_s
    end interface

    interface abs
        module procedure abs_molar_enthalpy
    end interface
    interface sum
        module procedure sum_molar_enthalpy
    end interface

    type(molar_enthalpy_simple_unit_t), parameter :: JOULES_PER_MOL = &
            molar_enthalpy_simple_unit_t( &
                    conversion_factor = 1.0d0, &
                    symbol = "J/mol")
    type(molar_enthalpy_simple_unit_t), parameter :: KILOJOULES_PER_MOL = &
            molar_enthalpy_simple_unit_t( &
                    conversion_factor = KILOJOULES_PER_MOL_PER_JOULES_PER_MOL, &
                    symbol = "kJ/mol")

    type(molar_enthalpy_simple_unit_t) :: DEFAULT_OUTPUT_UNITS = JOULES_PER_MOL

    type(molar_enthalpy_simple_unit_t), parameter :: PROVIDED_UNITS(*) = &
            [JOULES_PER_MOL, KILOJOULES_PER_MOL]

    character(len=*), parameter :: MODULE_NAME = "quaff_molar_enthalpy_m"
contains
    function parse_molar_enthalpy_c(string) result(fallible_molar_enthalpy)
        character(len=*), intent(in) :: string
        type(fallible_molar_enthalpy_t) :: fallible_molar_enthalpy

        fallible_molar_enthalpy = fallible_molar_enthalpy_t( &
                parse_molar_enthalpy(var_str(string), PROVIDED_UNITS), &
                module_t(MODULE_NAME), &
                procedure_t("parse_molar_enthalpy_c"))
    end function

    function parse_molar_enthalpy_s(string) result(fallible_molar_enthalpy)
        type(varying_string), intent(in) :: string
        type(fallible_molar_enthalpy_t) :: fallible_molar_enthalpy

        fallible_molar_enthalpy = fallible_molar_enthalpy_t( &
                parse_molar_enthalpy(string, PROVIDED_UNITS), &
                module_t(MODULE_NAME), &
                procedure_t("parse_molar_enthalpy_s"))
    end function

    function parse_molar_enthalpy_with_units_c( &
            string, units) result(fallible_molar_enthalpy)
        character(len=*), intent(in) :: string
        class(molar_enthalpy_unit_t), intent(in) :: units(:)
        type(fallible_molar_enthalpy_t) :: fallible_molar_enthalpy

        fallible_molar_enthalpy = fallible_molar_enthalpy_t( &
                parse_molar_enthalpy(var_str(string), units), &
                module_t(MODULE_NAME), &
                procedure_t("parse_molar_enthalpy_with_units_c"))
    end function

    function parse_molar_enthalpy_with_units_s( &
            string, units) result(fallible_molar_enthalpy)
        type(varying_string), intent(in) :: string
        class(molar_enthalpy_unit_t), intent(in) :: units(:)
        type(fallible_molar_enthalpy_t) :: fallible_molar_enthalpy

        integer :: i

        do i = 1, size(units)
            fallible_molar_enthalpy = units(i)%parse_as(string)
            if (.not. fallible_molar_enthalpy%failed()) return
        end do
        fallible_molar_enthalpy = fallible_molar_enthalpy_t(error_list_t(fatal_t( &
                PARSE_ERROR, &
                module_t(MODULE_NAME), &
                procedure_t("parse_molar_enthalpy_with_units_s"), &
                "Unable to parse '" // string // "' as a molar_enthalpy_t. Tried with units: " &
                // join(units%to_string(), ", "))))
    end function

    elemental function from_units(value_, units) result(molar_enthalpy)
        double precision, intent(in) :: value_
        class(molar_enthalpy_unit_t), intent(in) :: units
        type(molar_enthalpy_t) :: molar_enthalpy

        molar_enthalpy%joules_per_mol = value_ / units%conversion_factor
    end function

    elemental function to_units(self, units) result(molar_enthalpy)
        class(molar_enthalpy_t), intent(in) :: self
        class(molar_enthalpy_unit_t), intent(in) :: units
        double precision :: molar_enthalpy

        molar_enthalpy = self%joules_per_mol * units%conversion_factor
    end function

    elemental function double_times_molar_enthalpy( &
            multiplier, molar_enthalpy) result(new_molar_enthalpy)
        double precision, intent(in) :: multiplier
        class(molar_enthalpy_t), intent(in) :: molar_enthalpy
        type(molar_enthalpy_t) :: new_molar_enthalpy

        new_molar_enthalpy%joules_per_mol = &
                multiplier * molar_enthalpy%joules_per_mol
    end function

    elemental function integer_times_molar_enthalpy( &
            multiplier, molar_enthalpy) result(new_molar_enthalpy)
        integer, intent(in) :: multiplier
        class(molar_enthalpy_t), intent(in) :: molar_enthalpy
        type(molar_enthalpy_t) :: new_molar_enthalpy

        new_molar_enthalpy%joules_per_mol = &
                dble(multiplier) * molar_enthalpy%joules_per_mol
    end function

    elemental function molar_enthalpy_times_double( &
            molar_enthalpy, multiplier) result(new_molar_enthalpy)
        class(molar_enthalpy_t), intent(in) :: molar_enthalpy
        double precision, intent(in) :: multiplier
        type(molar_enthalpy_t) :: new_molar_enthalpy

        new_molar_enthalpy%joules_per_mol = &
                molar_enthalpy%joules_per_mol * multiplier
    end function

    elemental function molar_enthalpy_times_integer( &
            molar_enthalpy, multiplier) result(new_molar_enthalpy)
        class(molar_enthalpy_t), intent(in) :: molar_enthalpy
        integer, intent(in) :: multiplier
        type(molar_enthalpy_t) :: new_molar_enthalpy

        new_molar_enthalpy%joules_per_mol = &
                molar_enthalpy%joules_per_mol * dble(multiplier)
    end function

    elemental function molar_enthalpy_divided_by_double( &
            molar_enthalpy, divisor) result(new_molar_enthalpy)
        class(molar_enthalpy_t), intent(in) :: molar_enthalpy
        double precision, intent(in) :: divisor
        type(molar_enthalpy_t) :: new_molar_enthalpy

        new_molar_enthalpy%joules_per_mol = &
                molar_enthalpy%joules_per_mol / divisor
    end function

    elemental function molar_enthalpy_divided_by_integer( &
            molar_enthalpy, divisor) result(new_molar_enthalpy)
        class(molar_enthalpy_t), intent(in) :: molar_enthalpy
        integer, intent(in) :: divisor
        type(molar_enthalpy_t) :: new_molar_enthalpy

        new_molar_enthalpy%joules_per_mol = &
                molar_enthalpy%joules_per_mol / dble(divisor)
    end function

    elemental function molar_enthalpy_divided_by_molar_enthalpy( &
            numerator, denomenator) result(ratio)
        class(molar_enthalpy_t), intent(in) :: numerator
        type(molar_enthalpy_t), intent(in) :: denomenator
        double precision :: ratio

        ratio = numerator%joules_per_mol / denomenator%joules_per_mol
    end function

    elemental function molar_enthalpy_plus_molar_enthalpy( &
            lhs, rhs) result(sum_)
        class(molar_enthalpy_t), intent(in) :: lhs
        type(molar_enthalpy_t), intent(in) :: rhs
        type(molar_enthalpy_t) :: sum_

        sum_%joules_per_mol = lhs%joules_per_mol + rhs%joules_per_mol
    end function

    elemental function negate_molar_enthalpy(self) result(negated)
        class(molar_enthalpy_t), intent(in) :: self
        type(molar_enthalpy_t) :: negated

        negated%joules_per_mol = -self%joules_per_mol
    end function

    elemental function molar_enthalpy_minus_molar_enthalpy( &
            lhs, rhs) result(difference)
        class(molar_enthalpy_t), intent(in) :: lhs
        type(molar_enthalpy_t), intent(in) :: rhs
        type(molar_enthalpy_t) :: difference

        difference%joules_per_mol = lhs%joules_per_mol - rhs%joules_per_mol
    end function

    pure function abs_molar_enthalpy(molar_enthalpy) result(abs_)
        class(molar_enthalpy_t), intent(in) :: molar_enthalpy
        type(molar_enthalpy_t) :: abs_

        abs_%joules_per_mol = abs(molar_enthalpy%joules_per_mol)
    end function

    pure function sum_molar_enthalpy(molar_enthalpys) result(sum_)
        type(molar_enthalpy_t), intent(in) :: molar_enthalpys(:)
        type(molar_enthalpy_t) :: sum_

        sum_%joules_per_mol = sum(molar_enthalpys%joules_per_mol)
    end function

    elemental function greater_than(lhs, rhs)
        class(molar_enthalpy_t), intent(in) :: lhs
        type(molar_enthalpy_t), intent(in) :: rhs
        logical :: greater_than

        greater_than = lhs%joules_per_mol > rhs%joules_per_mol
    end function

    elemental function less_than(lhs, rhs)
        class(molar_enthalpy_t), intent(in) :: lhs
        type(molar_enthalpy_t), intent(in) :: rhs
        logical :: less_than

        less_than = lhs%joules_per_mol < rhs%joules_per_mol
    end function

    elemental function greater_than_or_equal(lhs, rhs)
        class(molar_enthalpy_t), intent(in) :: lhs
        type(molar_enthalpy_t), intent(in) :: rhs
        logical :: greater_than_or_equal

        greater_than_or_equal = lhs%joules_per_mol >= rhs%joules_per_mol
    end function

    elemental function less_than_or_equal(lhs, rhs)
        class(molar_enthalpy_t), intent(in) :: lhs
        type(molar_enthalpy_t), intent(in) :: rhs
        logical :: less_than_or_equal

        less_than_or_equal = lhs%joules_per_mol <= rhs%joules_per_mol
    end function

    elemental function equal_(lhs, rhs)
        class(molar_enthalpy_t), intent(in) :: lhs
        type(molar_enthalpy_t), intent(in) :: rhs
        logical :: equal_

        equal_ = lhs%joules_per_mol .safeEq. rhs%joules_per_mol
    end function

    elemental function equal_within_absolute_(lhs, rhs, within)
        class(molar_enthalpy_t), intent(in) :: lhs
        type(molar_enthalpy_t), intent(in) :: rhs
        type(molar_enthalpy_t), intent(in) :: within
        logical :: equal_within_absolute_

        equal_within_absolute_ = equal_within_absolute( &
                lhs%joules_per_mol, rhs%joules_per_mol, within%joules_per_mol)
    end function

    elemental function equal_within_relative_(lhs, rhs, within)
        class(molar_enthalpy_t), intent(in) :: lhs
        type(molar_enthalpy_t), intent(in) :: rhs
        double precision, intent(in) :: within
        logical :: equal_within_relative_

        equal_within_relative_ = equal_within_relative( &
                lhs%joules_per_mol, rhs%joules_per_mol, within)
    end function

    elemental function not_equal(lhs, rhs)
        class(molar_enthalpy_t), intent(in) :: lhs
        type(molar_enthalpy_t), intent(in) :: rhs
        logical :: not_equal

        not_equal = .not. lhs == rhs
    end function

    elemental function to_string_full_precision(self) result(string)
        class(molar_enthalpy_t), intent(in) :: self
        type(varying_string) :: string

        string = self%to_string_in(DEFAULT_OUTPUT_UNITS)
    end function

    elemental function to_string_with_precision(self, significant_digits) result(string)
        class(molar_enthalpy_t), intent(in) :: self
        integer, intent(in) :: significant_digits
        type(varying_string) :: string

        string = self%to_string_in(DEFAULT_OUTPUT_UNITS, significant_digits)
    end function

    elemental function to_string_in_full_precision(self, units) result(string)
        class(molar_enthalpy_t), intent(in) :: self
        class(molar_enthalpy_unit_t), intent(in) :: units
        type(varying_string) :: string

        string = units%to_string(to_string(self.in.units))
    end function

    elemental function to_string_in_with_precision( &
            self, units, significant_digits) result(string)
        class(molar_enthalpy_t), intent(in) :: self
        class(molar_enthalpy_unit_t), intent(in) :: units
        integer, intent(in) :: significant_digits
        type(varying_string) :: string

        string = units%to_string(to_string(self.in.units, significant_digits))
    end function

    function fallible_molar_enthalpy_from_molar_enthalpy( &
            molar_enthalpy) result(fallible_molar_enthalpy)
        type(molar_enthalpy_t), intent(in) :: molar_enthalpy
        type(fallible_molar_enthalpy_t) :: fallible_molar_enthalpy

        fallible_molar_enthalpy%molar_enthalpy_ = molar_enthalpy
    end function

    function fallible_molar_enthalpy_from_errors( &
            errors) result(fallible_molar_enthalpy)
        type(error_list_t), intent(in) :: errors
        type(fallible_molar_enthalpy_t) :: fallible_molar_enthalpy

        fallible_molar_enthalpy%errors_ = errors
    end function

    function fallible_molar_enthalpy_from_fallible_molar_enthalpy( &
            fallible_molar_enthalpy, &
            module_, &
            procedure_) &
            result(new_fallible_molar_enthalpy)
        type(fallible_molar_enthalpy_t), intent(in) :: fallible_molar_enthalpy
        type(module_t), intent(in) :: module_
        type(procedure_t), intent(in) :: procedure_
        type(fallible_molar_enthalpy_t) :: new_fallible_molar_enthalpy

        if (fallible_molar_enthalpy%failed()) then
            new_fallible_molar_enthalpy%errors_ = error_list_t( &
                    fallible_molar_enthalpy%errors_, module_, procedure_)
        else
            new_fallible_molar_enthalpy%molar_enthalpy_ = fallible_molar_enthalpy%molar_enthalpy_
        end if
    end function

    elemental function fallible_molar_enthalpy_failed( &
            self) result(failed)
        class(fallible_molar_enthalpy_t), intent(in) :: self
        logical :: failed

        failed = self%errors_%has_any()
    end function

    elemental function fallible_molar_enthalpy_molar_enthalpy( &
            self) result(molar_enthalpy)
        class(fallible_molar_enthalpy_t), intent(in) :: self
        type(molar_enthalpy_t) :: molar_enthalpy

        molar_enthalpy = self%molar_enthalpy_
    end function

    impure elemental function fallible_molar_enthalpy_errors( &
            self) result(errors)
        class(fallible_molar_enthalpy_t), intent(in) :: self
        type(error_list_t) :: errors

        errors = self%errors_
    end function

    function fallible_molar_enthalpy_unit_from_unit( &
            unit) result(fallible_molar_enthalpy_unit)
        class(molar_enthalpy_unit_t), intent(in) :: unit
        type(fallible_molar_enthalpy_unit_t) :: fallible_molar_enthalpy_unit

        allocate(fallible_molar_enthalpy_unit%unit_, source = unit)
    end function

    function fallible_molar_enthalpy_unit_from_errors( &
            errors) result(fallible_molar_enthalpy_unit)
        type(error_list_t), intent(in) :: errors
        type(fallible_molar_enthalpy_unit_t) :: fallible_molar_enthalpy_unit

        fallible_molar_enthalpy_unit%errors_ = errors
    end function

    function fallible_unit_from_fallible_unit( &
            fallible_molar_enthalpy_unit, &
            module_, &
            procedure_) &
            result(new_fallible_molar_enthalpy_unit)
        type(fallible_molar_enthalpy_unit_t), intent(in) :: fallible_molar_enthalpy_unit
        type(module_t), intent(in) :: module_
        type(procedure_t), intent(in) :: procedure_
        type(fallible_molar_enthalpy_unit_t) :: new_fallible_molar_enthalpy_unit

        if (fallible_molar_enthalpy_unit%failed()) then
            new_fallible_molar_enthalpy_unit%errors_ = error_list_t( &
                    fallible_molar_enthalpy_unit%errors_, module_, procedure_)
        else
            allocate(new_fallible_molar_enthalpy_unit%unit_, source = &
                    fallible_molar_enthalpy_unit%unit_)
        end if
    end function

    elemental function fallible_molar_enthalpy_unit_failed( &
            self) result(failed)
        class(fallible_molar_enthalpy_unit_t), intent(in) :: self
        logical :: failed

        failed = self%errors_%has_any()
    end function

    function fallible_molar_enthalpy_unit_unit( &
            self) result(unit)
        class(fallible_molar_enthalpy_unit_t), intent(in) :: self
        class(molar_enthalpy_unit_t), allocatable :: unit

        allocate(unit, source = self%unit_)
    end function

    impure elemental function fallible_molar_enthalpy_unit_errors( &
            self) result(errors)
        class(fallible_molar_enthalpy_unit_t), intent(in) :: self
        type(error_list_t) :: errors

        errors = self%errors_
    end function

    elemental function simple_unit_to_string(self) result(string)
        class(molar_enthalpy_simple_unit_t), intent(in) :: self
        type(varying_string) :: string

        string = trim(self%symbol)
    end function

    pure function simple_value_to_string(self, value_) result(string)
        class(molar_enthalpy_simple_unit_t), intent(in) :: self
        type(varying_string), intent(in) :: value_
        type(varying_string) :: string

        string = value_ // " " // self%to_string()
    end function

    function simple_parse_as(self, string) result(fallible_molar_enthalpy)
        class(molar_enthalpy_simple_unit_t), intent(in) :: self
        type(varying_string), intent(in) :: string
        type(fallible_molar_enthalpy_t) :: fallible_molar_enthalpy

        type(parse_result_t) :: parse_result

        parse_result = parse_with(the_parser, string)
        if (parse_result%ok) then
            select type (the_number => parse_result%parsed)
            type is (parsed_rational_t)
                fallible_molar_enthalpy = fallible_molar_enthalpy_t(the_number%value_.unit.self)
            end select
        else
            fallible_molar_enthalpy = fallible_molar_enthalpy_t(error_list_t(fatal_t( &
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

    function parse_molar_enthalpy_unit_c(string) result(fallible_molar_enthalpy_unit)
        character(len=*), intent(in) :: string
        type(fallible_molar_enthalpy_unit_t) :: fallible_molar_enthalpy_unit

        fallible_molar_enthalpy_unit = fallible_molar_enthalpy_unit_t( &
                parse_molar_enthalpy_unit(var_str(string), PROVIDED_UNITS), &
                module_t(MODULE_NAME), &
                procedure_t("parse_molar_enthalpy_unit_c"))
    end function

    function parse_molar_enthalpy_unit_s(string) result(fallible_molar_enthalpy_unit)
        type(varying_string), intent(in) :: string
        type(fallible_molar_enthalpy_unit_t) :: fallible_molar_enthalpy_unit

        fallible_molar_enthalpy_unit = fallible_molar_enthalpy_unit_t( &
                parse_molar_enthalpy_unit(string, PROVIDED_UNITS), &
                module_t(MODULE_NAME), &
                procedure_t("parse_molar_enthalpy_unit_s"))
    end function

    function parse_molar_enthalpy_unit_with_units_c( &
            string, units) result(fallible_molar_enthalpy_unit)
        character(len=*), intent(in) :: string
        class(molar_enthalpy_unit_t), intent(in) :: units(:)
        type(fallible_molar_enthalpy_unit_t) :: fallible_molar_enthalpy_unit

        fallible_molar_enthalpy_unit = fallible_molar_enthalpy_unit_t( &
                parse_molar_enthalpy_unit(var_str(string), units), &
                module_t(MODULE_NAME), &
                procedure_t("parse_molar_enthalpy_unit_with_units_c"))
    end function

    function parse_molar_enthalpy_unit_with_units_s( &
            string, units) result(fallible_molar_enthalpy_unit)
        type(varying_string), intent(in) :: string
        class(molar_enthalpy_unit_t), intent(in) :: units(:)
        type(fallible_molar_enthalpy_unit_t) :: fallible_molar_enthalpy_unit

        integer :: i
        type(varying_string), allocatable :: unit_strings(:)

        do i = 1, size(units)
            if (string == units(i)%to_string()) then
                fallible_molar_enthalpy_unit = fallible_molar_enthalpy_unit_t(units(i))
                return
            end if
        end do
        allocate(unit_strings(size(units)))
        do i = 1, size(units)
            unit_strings(i) = units(i)%to_string()
        end do
        fallible_molar_enthalpy_unit = fallible_molar_enthalpy_unit_t(error_list_t(fatal_t( &
                UNKNOWN_UNIT, &
                module_t(MODULE_NAME), &
                procedure_t("parse_molar_enthalpy_unit_with_units_s"), &
                '"' // string // '", known units: [' // join(unit_strings, ', ') // ']')))
    end function
end module
