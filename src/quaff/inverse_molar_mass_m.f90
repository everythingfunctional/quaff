module quaff_inverse_molar_mass_m
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
    use quaff_conversion_factors_m, only: MOLS_PER_GRAM_PER_MOLS_PER_KILOGRAM
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
            inverse_molar_mass_t, &
            fallible_inverse_molar_mass_t, &
            inverse_molar_mass_unit_t, &
            fallible_inverse_molar_mass_unit_t, &
            inverse_molar_mass_simple_unit_t, &
            operator(.unit.), &
            abs, &
            parse_inverse_molar_mass, &
            parse_inverse_molar_mass_unit, &
            sum, &
            DEFAULT_OUTPUT_UNITS, &
            PROVIDED_UNITS, &
            MOLS_PER_GRAM, &
            MOLS_PER_KILOGRAM

    type :: inverse_molar_mass_t
        double precision :: mols_per_kilogram
    contains
        private
        procedure :: to_units
        generic, public :: operator(.in.) => to_units
        procedure, pass(inverse_molar_mass) :: double_times_inverse_molar_mass
        procedure, pass(inverse_molar_mass) :: integer_times_inverse_molar_mass
        procedure :: inverse_molar_mass_times_double
        procedure :: inverse_molar_mass_times_integer
        generic, public :: operator(*) => &
                double_times_inverse_molar_mass, &
                integer_times_inverse_molar_mass, &
                inverse_molar_mass_times_double, &
                inverse_molar_mass_times_integer
        procedure :: inverse_molar_mass_divided_by_double
        procedure :: inverse_molar_mass_divided_by_integer
        procedure :: inverse_molar_mass_divided_by_inverse_molar_mass
        generic, public :: operator(/) => &
                inverse_molar_mass_divided_by_double, &
                inverse_molar_mass_divided_by_integer, &
                inverse_molar_mass_divided_by_inverse_molar_mass
        procedure :: inverse_molar_mass_plus_inverse_molar_mass
        generic, public :: operator(+) => inverse_molar_mass_plus_inverse_molar_mass
        procedure :: negate_inverse_molar_mass
        procedure :: inverse_molar_mass_minus_inverse_molar_mass
        generic, public :: operator(-) => &
                negate_inverse_molar_mass, &
                inverse_molar_mass_minus_inverse_molar_mass
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

    type :: fallible_inverse_molar_mass_t
        private
        type(inverse_molar_mass_t) :: inverse_molar_mass_ = inverse_molar_mass_t(0.0d0)
        type(error_list_t) :: errors_
    contains
        private
        procedure, public :: failed => fallible_inverse_molar_mass_failed
        procedure, public :: inverse_molar_mass => fallible_inverse_molar_mass_inverse_molar_mass
        procedure, public :: errors => fallible_inverse_molar_mass_errors
    end type

    type, abstract :: inverse_molar_mass_unit_t
        double precision :: conversion_factor
    contains
        procedure(unit_to_string_i), deferred :: unit_to_string
        procedure(value_to_string_i), deferred :: value_to_string
        generic :: to_string => unit_to_string, value_to_string
        procedure(parse_as_i), deferred :: parse_as
    end type

    type :: fallible_inverse_molar_mass_unit_t
        private
        class(inverse_molar_mass_unit_t), allocatable :: unit_
        type(error_list_t) :: errors_
    contains
        private
        procedure, public :: failed => fallible_inverse_molar_mass_unit_failed
        procedure, public :: unit => fallible_inverse_molar_mass_unit_unit
        procedure, public :: errors => fallible_inverse_molar_mass_unit_errors
    end type

    type, extends(inverse_molar_mass_unit_t) :: inverse_molar_mass_simple_unit_t
        character(len=20) :: symbol
    contains
        procedure :: unit_to_string => simple_unit_to_string
        procedure :: value_to_string => simple_value_to_string
        procedure :: parse_as => simple_parse_as
    end type

    abstract interface
        elemental function unit_to_string_i(self) result(string)
            import :: inverse_molar_mass_unit_t, varying_string

            implicit none

            class(inverse_molar_mass_unit_t), intent(in) :: self
            type(varying_string) :: string
        end function

        pure function value_to_string_i(self, value_) result(string)
            import :: inverse_molar_mass_unit_t, varying_string

            implicit none

            class(inverse_molar_mass_unit_t), intent(in) :: self
            type(varying_string), intent(in) :: value_
            type(varying_string) :: string
        end function

        function parse_as_i(self, string) result(fallible_inverse_molar_mass)
            import :: inverse_molar_mass_unit_t, fallible_inverse_molar_mass_t, varying_string

            implicit none

            class(inverse_molar_mass_unit_t), intent(in) :: self
            type(varying_string), intent(in) :: string
            type(fallible_inverse_molar_mass_t) :: fallible_inverse_molar_mass
        end function
    end interface

    interface operator(.unit.)
        module procedure from_units
    end interface

    interface fallible_inverse_molar_mass_t
        module procedure fallible_inverse_molar_mass_from_inverse_molar_mass
        module procedure fallible_inverse_molar_mass_from_errors
        module procedure fallible_inverse_molar_mass_from_fallible_inverse_molar_mass
    end interface

    interface fallible_inverse_molar_mass_unit_t
        module procedure fallible_inverse_molar_mass_unit_from_unit
        module procedure fallible_inverse_molar_mass_unit_from_errors
        module procedure fallible_inv_mol_mass_unit_from_fallible_inv_mol_mass_unit
    end interface

    interface parse_inverse_molar_mass
        module procedure parse_inverse_molar_mass_c
        module procedure parse_inverse_molar_mass_s
        module procedure parse_inverse_molar_mass_with_units_c
        module procedure parse_inverse_molar_mass_with_units_s
    end interface

    interface parse_inverse_molar_mass_unit
        module procedure parse_inverse_molar_mass_unit_c
        module procedure parse_inverse_molar_mass_unit_s
        module procedure parse_inverse_molar_mass_unit_with_units_c
        module procedure parse_inverse_molar_mass_unit_with_units_s
    end interface

    interface abs
        module procedure abs_inverse_molar_mass
    end interface

    interface sum
        module procedure sum_inverse_molar_mass
    end interface

    type(inverse_molar_mass_simple_unit_t), parameter :: MOLS_PER_GRAM = &
            inverse_molar_mass_simple_unit_t( &
                    conversion_factor = MOLS_PER_GRAM_PER_MOLS_PER_KILOGRAM, &
                    symbol = "mol/g")
    type(inverse_molar_mass_simple_unit_t), parameter :: MOLS_PER_KILOGRAM = &
            inverse_molar_mass_simple_unit_t( &
                    conversion_factor = 1.0d0, &
                    symbol = "mol/kg")

    type(inverse_molar_mass_simple_unit_t) :: DEFAULT_OUTPUT_UNITS = MOLS_PER_KILOGRAM

    type(inverse_molar_mass_simple_unit_t), parameter :: PROVIDED_UNITS(*) = &
            [MOLS_PER_GRAM, MOLS_PER_KILOGRAM]

    character(len=*), parameter :: MODULE_NAME = "quaff_inverse_molar_mass_m"
contains
    function parse_inverse_molar_mass_c(string) result(fallible_inverse_molar_mass)
        character(len=*), intent(in) :: string
        type(fallible_inverse_molar_mass_t) :: fallible_inverse_molar_mass

        fallible_inverse_molar_mass = fallible_inverse_molar_mass_t( &
                parse_inverse_molar_mass(var_str(string), PROVIDED_UNITS), &
                module_t(MODULE_NAME), &
                procedure_t("parse_inverse_molar_mass_c"))
    end function

    function parse_inverse_molar_mass_s(string) result(fallible_inverse_molar_mass)
        type(varying_string), intent(in) :: string
        type(fallible_inverse_molar_mass_t) :: fallible_inverse_molar_mass

        fallible_inverse_molar_mass = fallible_inverse_molar_mass_t( &
                parse_inverse_molar_mass(string, PROVIDED_UNITS), &
                module_t(MODULE_NAME), &
                procedure_t("parse_inverse_molar_mass_s"))
    end function

    function parse_inverse_molar_mass_with_units_c( &
            string, units) result(fallible_inverse_molar_mass)
        character(len=*), intent(in) :: string
        class(inverse_molar_mass_unit_t), intent(in) :: units(:)
        type(fallible_inverse_molar_mass_t) :: fallible_inverse_molar_mass

        fallible_inverse_molar_mass = fallible_inverse_molar_mass_t( &
                parse_inverse_molar_mass(var_str(string), units), &
                module_t(MODULE_NAME), &
                procedure_t("parse_inverse_molar_mass_with_units_c"))
    end function

    function parse_inverse_molar_mass_with_units_s( &
            string, units) result(fallible_inverse_molar_mass)
        type(varying_string), intent(in) :: string
        class(inverse_molar_mass_unit_t), intent(in) :: units(:)
        type(fallible_inverse_molar_mass_t) :: fallible_inverse_molar_mass

        integer :: i

        do i = 1, size(units)
            fallible_inverse_molar_mass = units(i)%parse_as(string)
            if (.not. fallible_inverse_molar_mass%failed()) return
        end do
        fallible_inverse_molar_mass = fallible_inverse_molar_mass_t(error_list_t(fatal_t( &
                PARSE_ERROR, &
                module_t(MODULE_NAME), &
                procedure_t("parse_inverse_molar_mass_with_units_s"), &
                "Unable to parse '" // string // "' as a inverse_molar_mass_t. Tried with units: " &
                // join(units%to_string(), ", "))))
    end function

    elemental function from_units(value_, units) result(inverse_molar_mass)
        double precision, intent(in) :: value_
        class(inverse_molar_mass_unit_t), intent(in) :: units
        type(inverse_molar_mass_t) :: inverse_molar_mass

        inverse_molar_mass%mols_per_kilogram = value_ / units%conversion_factor
    end function

    elemental function to_units(self, units) result(inverse_molar_mass)
        class(inverse_molar_mass_t), intent(in) :: self
        class(inverse_molar_mass_unit_t), intent(in) :: units
        double precision :: inverse_molar_mass

        inverse_molar_mass = self%mols_per_kilogram * units%conversion_factor
    end function

    elemental function double_times_inverse_molar_mass( &
            multiplier, inverse_molar_mass) result(new_inverse_molar_mass)
        double precision, intent(in) :: multiplier
        class(inverse_molar_mass_t), intent(in) :: inverse_molar_mass
        type(inverse_molar_mass_t) :: new_inverse_molar_mass

        new_inverse_molar_mass%mols_per_kilogram = &
                multiplier * inverse_molar_mass%mols_per_kilogram
    end function

    elemental function integer_times_inverse_molar_mass( &
            multiplier, inverse_molar_mass) result(new_inverse_molar_mass)
        integer, intent(in) :: multiplier
        class(inverse_molar_mass_t), intent(in) :: inverse_molar_mass
        type(inverse_molar_mass_t) :: new_inverse_molar_mass

        new_inverse_molar_mass%mols_per_kilogram = &
                dble(multiplier) * inverse_molar_mass%mols_per_kilogram
    end function

    elemental function inverse_molar_mass_times_double( &
            inverse_molar_mass, multiplier) result(new_inverse_molar_mass)
        class(inverse_molar_mass_t), intent(in) :: inverse_molar_mass
        double precision, intent(in) :: multiplier
        type(inverse_molar_mass_t) :: new_inverse_molar_mass

        new_inverse_molar_mass%mols_per_kilogram = &
                inverse_molar_mass%mols_per_kilogram * multiplier
    end function

    elemental function inverse_molar_mass_times_integer( &
            inverse_molar_mass, multiplier) result(new_inverse_molar_mass)
        class(inverse_molar_mass_t), intent(in) :: inverse_molar_mass
        integer, intent(in) :: multiplier
        type(inverse_molar_mass_t) :: new_inverse_molar_mass

        new_inverse_molar_mass%mols_per_kilogram = &
                inverse_molar_mass%mols_per_kilogram * dble(multiplier)
    end function

    elemental function inverse_molar_mass_divided_by_double( &
            inverse_molar_mass, divisor) result(new_inverse_molar_mass)
        class(inverse_molar_mass_t), intent(in) :: inverse_molar_mass
        double precision, intent(in) :: divisor
        type(inverse_molar_mass_t) :: new_inverse_molar_mass

        new_inverse_molar_mass%mols_per_kilogram = &
                inverse_molar_mass%mols_per_kilogram / divisor
    end function

    elemental function inverse_molar_mass_divided_by_integer( &
            inverse_molar_mass, divisor) result(new_inverse_molar_mass)
        class(inverse_molar_mass_t), intent(in) :: inverse_molar_mass
        integer, intent(in) :: divisor
        type(inverse_molar_mass_t) :: new_inverse_molar_mass

        new_inverse_molar_mass%mols_per_kilogram = &
                inverse_molar_mass%mols_per_kilogram / dble(divisor)
    end function

    elemental function inverse_molar_mass_divided_by_inverse_molar_mass( &
            numerator, denomenator) result(ratio)
        class(inverse_molar_mass_t), intent(in) :: numerator
        type(inverse_molar_mass_t), intent(in) :: denomenator
        double precision :: ratio

        ratio = numerator%mols_per_kilogram / denomenator%mols_per_kilogram
    end function

    elemental function inverse_molar_mass_plus_inverse_molar_mass( &
            lhs, rhs) result(sum_)
        class(inverse_molar_mass_t), intent(in) :: lhs
        type(inverse_molar_mass_t), intent(in) :: rhs
        type(inverse_molar_mass_t) :: sum_

        sum_%mols_per_kilogram = lhs%mols_per_kilogram + rhs%mols_per_kilogram
    end function

    elemental function negate_inverse_molar_mass(self) result(negated)
        class(inverse_molar_mass_t), intent(in) :: self
        type(inverse_molar_mass_t) :: negated

        negated%mols_per_kilogram = -self%mols_per_kilogram
    end function

    elemental function inverse_molar_mass_minus_inverse_molar_mass( &
            lhs, rhs) result(difference)
        class(inverse_molar_mass_t), intent(in) :: lhs
        type(inverse_molar_mass_t), intent(in) :: rhs
        type(inverse_molar_mass_t) :: difference

        difference%mols_per_kilogram = lhs%mols_per_kilogram - rhs%mols_per_kilogram
    end function

    elemental function abs_inverse_molar_mass(inverse_molar_mass) result(abs_)
        type(inverse_molar_mass_t), intent(in) :: inverse_molar_mass
        type(inverse_molar_mass_t) :: abs_

        abs_%mols_per_kilogram = abs(inverse_molar_mass%mols_per_kilogram)
    end function

    pure function sum_inverse_molar_mass(inverse_molar_masss) result(sum_)
        type(inverse_molar_mass_t), intent(in) :: inverse_molar_masss(:)
        type(inverse_molar_mass_t) :: sum_

        sum_%mols_per_kilogram = sum(inverse_molar_masss%mols_per_kilogram)
    end function

    elemental function greater_than(lhs, rhs)
        class(inverse_molar_mass_t), intent(in) :: lhs
        type(inverse_molar_mass_t), intent(in) :: rhs
        logical :: greater_than

        greater_than = lhs%mols_per_kilogram > rhs%mols_per_kilogram
    end function

    elemental function less_than(lhs, rhs)
        class(inverse_molar_mass_t), intent(in) :: lhs
        type(inverse_molar_mass_t), intent(in) :: rhs
        logical :: less_than

        less_than = lhs%mols_per_kilogram < rhs%mols_per_kilogram
    end function

    elemental function greater_than_or_equal(lhs, rhs)
        class(inverse_molar_mass_t), intent(in) :: lhs
        type(inverse_molar_mass_t), intent(in) :: rhs
        logical :: greater_than_or_equal

        greater_than_or_equal = lhs%mols_per_kilogram >= rhs%mols_per_kilogram
    end function

    elemental function less_than_or_equal(lhs, rhs)
        class(inverse_molar_mass_t), intent(in) :: lhs
        type(inverse_molar_mass_t), intent(in) :: rhs
        logical :: less_than_or_equal

        less_than_or_equal = lhs%mols_per_kilogram <= rhs%mols_per_kilogram
    end function

    elemental function equal_(lhs, rhs)
        class(inverse_molar_mass_t), intent(in) :: lhs
        type(inverse_molar_mass_t), intent(in) :: rhs
        logical :: equal_

        equal_ = lhs%mols_per_kilogram .safeEq. rhs%mols_per_kilogram
    end function

    elemental function equal_within_absolute_(lhs, rhs, within)
        class(inverse_molar_mass_t), intent(in) :: lhs
        type(inverse_molar_mass_t), intent(in) :: rhs
        type(inverse_molar_mass_t), intent(in) :: within
        logical :: equal_within_absolute_

        equal_within_absolute_ = equal_within_absolute( &
                lhs%mols_per_kilogram, rhs%mols_per_kilogram, within%mols_per_kilogram)
    end function

    elemental function equal_within_relative_(lhs, rhs, within)
        class(inverse_molar_mass_t), intent(in) :: lhs
        type(inverse_molar_mass_t), intent(in) :: rhs
        double precision, intent(in) :: within
        logical :: equal_within_relative_

        equal_within_relative_ = equal_within_relative( &
                lhs%mols_per_kilogram, rhs%mols_per_kilogram, within)
    end function

    elemental function not_equal(lhs, rhs)
        class(inverse_molar_mass_t), intent(in) :: lhs
        type(inverse_molar_mass_t), intent(in) :: rhs
        logical :: not_equal

        not_equal = .not. lhs == rhs
    end function

    elemental function to_string_full_precision(self) result(string)
        class(inverse_molar_mass_t), intent(in) :: self
        type(varying_string) :: string

        string = self%to_string_in(DEFAULT_OUTPUT_UNITS)
    end function

    elemental function to_string_with_precision(self, significant_digits) result(string)
        class(inverse_molar_mass_t), intent(in) :: self
        integer, intent(in) :: significant_digits
        type(varying_string) :: string

        string = self%to_string_in(DEFAULT_OUTPUT_UNITS, significant_digits)
    end function

    elemental function to_string_in_full_precision(self, units) result(string)
        class(inverse_molar_mass_t), intent(in) :: self
        class(inverse_molar_mass_unit_t), intent(in) :: units
        type(varying_string) :: string

        string = units%to_string(to_string(self.in.units))
    end function

    elemental function to_string_in_with_precision( &
            self, units, significant_digits) result(string)
        class(inverse_molar_mass_t), intent(in) :: self
        class(inverse_molar_mass_unit_t), intent(in) :: units
        integer, intent(in) :: significant_digits
        type(varying_string) :: string

        string = units%to_string(to_string(self.in.units, significant_digits))
    end function

    function fallible_inverse_molar_mass_from_inverse_molar_mass( &
            inverse_molar_mass) result(fallible_inverse_molar_mass)
        type(inverse_molar_mass_t), intent(in) :: inverse_molar_mass
        type(fallible_inverse_molar_mass_t) :: fallible_inverse_molar_mass

        fallible_inverse_molar_mass%inverse_molar_mass_ = inverse_molar_mass
    end function

    function fallible_inverse_molar_mass_from_errors( &
            errors) result(fallible_inverse_molar_mass)
        type(error_list_t), intent(in) :: errors
        type(fallible_inverse_molar_mass_t) :: fallible_inverse_molar_mass

        fallible_inverse_molar_mass%errors_ = errors
    end function

    function fallible_inverse_molar_mass_from_fallible_inverse_molar_mass( &
            fallible_inverse_molar_mass, &
            module_, &
            procedure_) &
            result(new_fallible_inverse_molar_mass)
        type(fallible_inverse_molar_mass_t), intent(in) :: fallible_inverse_molar_mass
        type(module_t), intent(in) :: module_
        type(procedure_t), intent(in) :: procedure_
        type(fallible_inverse_molar_mass_t) :: new_fallible_inverse_molar_mass

        if (fallible_inverse_molar_mass%failed()) then
            new_fallible_inverse_molar_mass%errors_ = error_list_t( &
                    fallible_inverse_molar_mass%errors_, module_, procedure_)
        else
            new_fallible_inverse_molar_mass%inverse_molar_mass_ = fallible_inverse_molar_mass%inverse_molar_mass_
        end if
    end function

    elemental function fallible_inverse_molar_mass_failed( &
            self) result(failed)
        class(fallible_inverse_molar_mass_t), intent(in) :: self
        logical :: failed

        failed = self%errors_%has_any()
    end function

    elemental function fallible_inverse_molar_mass_inverse_molar_mass( &
            self) result(inverse_molar_mass)
        class(fallible_inverse_molar_mass_t), intent(in) :: self
        type(inverse_molar_mass_t) :: inverse_molar_mass

        inverse_molar_mass = self%inverse_molar_mass_
    end function

    impure elemental function fallible_inverse_molar_mass_errors( &
            self) result(errors)
        class(fallible_inverse_molar_mass_t), intent(in) :: self
        type(error_list_t) :: errors

        errors = self%errors_
    end function

    function fallible_inverse_molar_mass_unit_from_unit( &
            unit) result(fallible_inverse_molar_mass_unit)
        class(inverse_molar_mass_unit_t), intent(in) :: unit
        type(fallible_inverse_molar_mass_unit_t) :: fallible_inverse_molar_mass_unit

        allocate(fallible_inverse_molar_mass_unit%unit_, source = unit)
    end function

    function fallible_inverse_molar_mass_unit_from_errors( &
            errors) result(fallible_inverse_molar_mass_unit)
        type(error_list_t), intent(in) :: errors
        type(fallible_inverse_molar_mass_unit_t) :: fallible_inverse_molar_mass_unit

        fallible_inverse_molar_mass_unit%errors_ = errors
    end function

    function fallible_inv_mol_mass_unit_from_fallible_inv_mol_mass_unit( &
            fallible_inverse_molar_mass_unit, &
            module_, &
            procedure_) &
            result(new_fallible_inverse_molar_mass_unit)
        type(fallible_inverse_molar_mass_unit_t), intent(in) :: fallible_inverse_molar_mass_unit
        type(module_t), intent(in) :: module_
        type(procedure_t), intent(in) :: procedure_
        type(fallible_inverse_molar_mass_unit_t) :: new_fallible_inverse_molar_mass_unit

        if (fallible_inverse_molar_mass_unit%failed()) then
            new_fallible_inverse_molar_mass_unit%errors_ = error_list_t( &
                    fallible_inverse_molar_mass_unit%errors_, module_, procedure_)
        else
            allocate(new_fallible_inverse_molar_mass_unit%unit_, source = &
                    fallible_inverse_molar_mass_unit%unit_)
        end if
    end function

    elemental function fallible_inverse_molar_mass_unit_failed( &
            self) result(failed)
        class(fallible_inverse_molar_mass_unit_t), intent(in) :: self
        logical :: failed

        failed = self%errors_%has_any()
    end function

    function fallible_inverse_molar_mass_unit_unit( &
            self) result(unit)
        class(fallible_inverse_molar_mass_unit_t), intent(in) :: self
        class(inverse_molar_mass_unit_t), allocatable :: unit

        allocate(unit, source = self%unit_)
    end function

    impure elemental function fallible_inverse_molar_mass_unit_errors( &
            self) result(errors)
        class(fallible_inverse_molar_mass_unit_t), intent(in) :: self
        type(error_list_t) :: errors

        errors = self%errors_
    end function

    elemental function simple_unit_to_string(self) result(string)
        class(inverse_molar_mass_simple_unit_t), intent(in) :: self
        type(varying_string) :: string

        string = trim(self%symbol)
    end function

    pure function simple_value_to_string(self, value_) result(string)
        class(inverse_molar_mass_simple_unit_t), intent(in) :: self
        type(varying_string), intent(in) :: value_
        type(varying_string) :: string

        string = value_ // " " // self%to_string()
    end function

    function simple_parse_as(self, string) result(fallible_inverse_molar_mass)
        class(inverse_molar_mass_simple_unit_t), intent(in) :: self
        type(varying_string), intent(in) :: string
        type(fallible_inverse_molar_mass_t) :: fallible_inverse_molar_mass

        type(parse_result_t) :: parse_result

        parse_result = parse_with(the_parser, string)
        if (parse_result%ok) then
            select type (the_number => parse_result%parsed)
            type is (parsed_rational_t)
                fallible_inverse_molar_mass = fallible_inverse_molar_mass_t(the_number%value_.unit.self)
            end select
        else
            fallible_inverse_molar_mass = fallible_inverse_molar_mass_t(error_list_t(fatal_t( &
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

    function parse_inverse_molar_mass_unit_c(string) result(fallible_inverse_molar_mass_unit)
        character(len=*), intent(in) :: string
        type(fallible_inverse_molar_mass_unit_t) :: fallible_inverse_molar_mass_unit

        fallible_inverse_molar_mass_unit = fallible_inverse_molar_mass_unit_t( &
                parse_inverse_molar_mass_unit(var_str(string), PROVIDED_UNITS), &
                module_t(MODULE_NAME), &
                procedure_t("parse_inverse_molar_mass_unit_c"))
    end function

    function parse_inverse_molar_mass_unit_s(string) result(fallible_inverse_molar_mass_unit)
        type(varying_string), intent(in) :: string
        type(fallible_inverse_molar_mass_unit_t) :: fallible_inverse_molar_mass_unit

        fallible_inverse_molar_mass_unit = fallible_inverse_molar_mass_unit_t( &
                parse_inverse_molar_mass_unit(string, PROVIDED_UNITS), &
                module_t(MODULE_NAME), &
                procedure_t("parse_inverse_molar_mass_unit_s"))
    end function

    function parse_inverse_molar_mass_unit_with_units_c( &
            string, units) result(fallible_inverse_molar_mass_unit)
        character(len=*), intent(in) :: string
        class(inverse_molar_mass_unit_t), intent(in) :: units(:)
        type(fallible_inverse_molar_mass_unit_t) :: fallible_inverse_molar_mass_unit

        fallible_inverse_molar_mass_unit = fallible_inverse_molar_mass_unit_t( &
                parse_inverse_molar_mass_unit(var_str(string), units), &
                module_t(MODULE_NAME), &
                procedure_t("parse_inverse_molar_mass_unit_with_units_c"))
    end function

    function parse_inverse_molar_mass_unit_with_units_s( &
            string, units) result(fallible_inverse_molar_mass_unit)
        type(varying_string), intent(in) :: string
        class(inverse_molar_mass_unit_t), intent(in) :: units(:)
        type(fallible_inverse_molar_mass_unit_t) :: fallible_inverse_molar_mass_unit

        integer :: i
        type(varying_string), allocatable :: unit_strings(:)

        do i = 1, size(units)
            if (string == units(i)%to_string()) then
                fallible_inverse_molar_mass_unit = fallible_inverse_molar_mass_unit_t(units(i))
                return
            end if
        end do
        allocate(unit_strings(size(units)))
        do i = 1, size(units)
            unit_strings(i) = units(i)%to_string()
        end do
        fallible_inverse_molar_mass_unit = fallible_inverse_molar_mass_unit_t(error_list_t(fatal_t( &
                UNKNOWN_UNIT, &
                module_t(MODULE_NAME), &
                procedure_t("parse_inverse_molar_mass_unit_with_units_s"), &
                '"' // string // '", known units: [' // join(unit_strings, ', ') // ']')))
    end function
end module
