module energy_per_temperature_amount_m
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
            KILOJOULES_PER_KELVIN_MOL_PER_JOULES_PER_KELVIN_MOL
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
            energy_per_temperature_amount_t, &
            fallible_energy_per_temperature_amount_t, &
            energy_per_temperature_amount_unit_t, &
            fallible_energy_per_temperature_amount_unit_t, &
            energy_per_temperature_amount_simple_unit_t, &
            operator(.unit.), &
            parse_energy_per_temperature_amount, &
            parse_energy_per_temperature_amount_unit, &
            sum, &
            DEFAULT_OUTPUT_UNITS, &
            PROVIDED_UNITS, &
            JOULES_PER_KELVIN_MOL, &
            KILOJOULES_PER_KELVIN_MOL

    type :: energy_per_temperature_amount_t
        double precision :: joules_per_kelvin_mol
    contains
        private
        procedure :: to_units
        generic, public :: operator(.in.) => to_units
        procedure, pass(energy_per_temperature_amount) :: double_times_energy_per_temperature_amount
        procedure, pass(energy_per_temperature_amount) :: integer_times_energy_per_temperature_amount
        procedure :: energy_per_temperature_amount_times_double
        procedure :: energy_per_temperature_amount_times_integer
        generic, public :: operator(*) => &
                double_times_energy_per_temperature_amount, &
                integer_times_energy_per_temperature_amount, &
                energy_per_temperature_amount_times_double, &
                energy_per_temperature_amount_times_integer
        procedure :: energy_per_temperature_amount_divided_by_double
        procedure :: energy_per_temperature_amount_divided_by_integer
        procedure :: energy_per_temp_amt_divided_by_energy_per_temp_amt
        generic, public :: operator(/) => &
                energy_per_temperature_amount_divided_by_double, &
                energy_per_temperature_amount_divided_by_integer, &
                energy_per_temp_amt_divided_by_energy_per_temp_amt
        procedure :: energy_per_temp_amt_plus_energy_per_temp_amt
        generic, public :: operator(+) => energy_per_temp_amt_plus_energy_per_temp_amt
        procedure :: energy_per_temp_amt_minus_energy_per_temp_amt
        generic, public :: operator(-) => energy_per_temp_amt_minus_energy_per_temp_amt
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

    type :: fallible_energy_per_temperature_amount_t
        private
        type(energy_per_temperature_amount_t) :: energy_per_temperature_amount_
        type(error_list_t) :: errors_
    contains
        private
        procedure, public :: failed => fallible_energy_per_temperature_amount_failed
        procedure, public :: energy_per_temperature_amount => fallible_energy_per_temp_amt_energy_per_temp_amt
        procedure, public :: errors => fallible_energy_per_temperature_amount_errors
    end type

    type, abstract :: energy_per_temperature_amount_unit_t
        double precision :: conversion_factor
    contains
        procedure(unit_to_string_i), deferred :: unit_to_string
        procedure(value_to_string_i), deferred :: value_to_string
        generic :: to_string => unit_to_string, value_to_string
        procedure(parse_as_i), deferred :: parse_as
    end type

    type :: fallible_energy_per_temperature_amount_unit_t
        private
        class(energy_per_temperature_amount_unit_t), allocatable :: unit_
        type(error_list_t) :: errors_
    contains
        private
        procedure, public :: failed => fallible_energy_per_temperature_amount_unit_failed
        procedure, public :: unit => fallible_energy_per_temperature_amount_unit_unit
        procedure, public :: errors => fallible_energy_per_temperature_amount_unit_errors
    end type

    type, extends(energy_per_temperature_amount_unit_t) :: energy_per_temperature_amount_simple_unit_t
        character(len=20) :: symbol
    contains
        procedure :: unit_to_string => simple_unit_to_string
        procedure :: value_to_string => simple_value_to_string
        procedure :: parse_as => simple_parse_as
    end type

    abstract interface
        elemental function unit_to_string_i(self) result(string)
            import :: energy_per_temperature_amount_unit_t, varying_string

            implicit none

            class(energy_per_temperature_amount_unit_t), intent(in) :: self
            type(varying_string) :: string
        end function

        pure function value_to_string_i(self, value_) result(string)
            import :: energy_per_temperature_amount_unit_t, varying_string

            implicit none

            class(energy_per_temperature_amount_unit_t), intent(in) :: self
            type(varying_string), intent(in) :: value_
            type(varying_string) :: string
        end function

        function parse_as_i(self, string) result(fallible_energy_per_temperature_amount)
            import :: energy_per_temperature_amount_unit_t, fallible_energy_per_temperature_amount_t, varying_string

            implicit none

            class(energy_per_temperature_amount_unit_t), intent(in) :: self
            type(varying_string), intent(in) :: string
            type(fallible_energy_per_temperature_amount_t) :: fallible_energy_per_temperature_amount
        end function
    end interface

    interface operator(.unit.)
        module procedure from_units
    end interface

    interface fallible_energy_per_temperature_amount_t
        module procedure fallible_energy_per_temp_amt_from_energy_per_temp_amt
        module procedure fallible_energy_per_temperature_amount_from_errors
        module procedure fallible_energy_per_temp_amt_from_fallible_energy_per_temp_amt
    end interface

    interface fallible_energy_per_temperature_amount_unit_t
        module procedure fallible_energy_per_temperature_amount_unit_from_unit
        module procedure fallible_energy_per_temperature_amount_unit_from_errors
        module procedure fal_nrgy_per_temp_amt_unit_from_fal_nrgy_per_temp_amt_unit
    end interface

    interface parse_energy_per_temperature_amount
        module procedure parse_energy_per_temperature_amount_c
        module procedure parse_energy_per_temperature_amount_s
        module procedure parse_energy_per_temperature_amount_with_units_c
        module procedure parse_energy_per_temperature_amount_with_units_s
    end interface

    interface parse_energy_per_temperature_amount_unit
        module procedure parse_energy_per_temperature_amount_unit_c
        module procedure parse_energy_per_temperature_amount_unit_s
        module procedure parse_energy_per_temperature_amount_unit_with_units_c
        module procedure parse_energy_per_temperature_amount_unit_with_units_s
    end interface

    interface sum
        module procedure sum_energy_per_temperature_amount
    end interface

    type(energy_per_temperature_amount_simple_unit_t), parameter :: JOULES_PER_KELVIN_MOL = &
            energy_per_temperature_amount_simple_unit_t( &
                    conversion_factor = 1.0d0, &
                    symbol = "J/(K mol)")
    type(energy_per_temperature_amount_simple_unit_t), parameter :: KILOJOULES_PER_KELVIN_MOL = &
            energy_per_temperature_amount_simple_unit_t( &
                    conversion_factor = KILOJOULES_PER_KELVIN_MOL_PER_JOULES_PER_KELVIN_MOL, &
                    symbol = "kJ/(K mol)")

    type(energy_per_temperature_amount_simple_unit_t) :: DEFAULT_OUTPUT_UNITS = JOULES_PER_KELVIN_MOL

    type(energy_per_temperature_amount_simple_unit_t), parameter :: PROVIDED_UNITS(*) = &
            [JOULES_PER_KELVIN_MOL, KILOJOULES_PER_KELVIN_MOL]

    character(len=*), parameter :: MODULE_NAME = "energy_per_temperature_amount_m"
contains
    function parse_energy_per_temperature_amount_c(string) result(fallible_energy_per_temperature_amount)
        character(len=*), intent(in) :: string
        type(fallible_energy_per_temperature_amount_t) :: fallible_energy_per_temperature_amount

        fallible_energy_per_temperature_amount = fallible_energy_per_temperature_amount_t( &
                parse_energy_per_temperature_amount(var_str(string), PROVIDED_UNITS), &
                module_t(MODULE_NAME), &
                procedure_t("parse_energy_per_temperature_amount_c"))
    end function

    function parse_energy_per_temperature_amount_s(string) result(fallible_energy_per_temperature_amount)
        type(varying_string), intent(in) :: string
        type(fallible_energy_per_temperature_amount_t) :: fallible_energy_per_temperature_amount

        fallible_energy_per_temperature_amount = fallible_energy_per_temperature_amount_t( &
                parse_energy_per_temperature_amount(string, PROVIDED_UNITS), &
                module_t(MODULE_NAME), &
                procedure_t("parse_energy_per_temperature_amount_s"))
    end function

    function parse_energy_per_temperature_amount_with_units_c( &
            string, units) result(fallible_energy_per_temperature_amount)
        character(len=*), intent(in) :: string
        class(energy_per_temperature_amount_unit_t), intent(in) :: units(:)
        type(fallible_energy_per_temperature_amount_t) :: fallible_energy_per_temperature_amount

        fallible_energy_per_temperature_amount = fallible_energy_per_temperature_amount_t( &
                parse_energy_per_temperature_amount(var_str(string), units), &
                module_t(MODULE_NAME), &
                procedure_t("parse_energy_per_temperature_amount_with_units_c"))
    end function

    function parse_energy_per_temperature_amount_with_units_s( &
            string, units) result(fallible_energy_per_temperature_amount)
        type(varying_string), intent(in) :: string
        class(energy_per_temperature_amount_unit_t), intent(in) :: units(:)
        type(fallible_energy_per_temperature_amount_t) :: fallible_energy_per_temperature_amount

        type(fallible_energy_per_temperature_amount_t) :: all_attempts(size(units))
        integer :: i

        do i = 1, size(units)
            all_attempts(i) = units(i)%parse_as(string)
            if (.not. all_attempts(i)%failed()) then
                fallible_energy_per_temperature_amount = all_attempts(i)
                return
            end if
        end do
        fallible_energy_per_temperature_amount = fallible_energy_per_temperature_amount_t(error_list_t( &
                all_attempts%errors(), &
                module_t(MODULE_NAME), &
                procedure_t("parse_energy_per_temperature_amount_with_units_s")))
    end function

    elemental function from_units(value_, units) result(energy_per_temperature_amount)
        double precision, intent(in) :: value_
        class(energy_per_temperature_amount_unit_t), intent(in) :: units
        type(energy_per_temperature_amount_t) :: energy_per_temperature_amount

        energy_per_temperature_amount%joules_per_kelvin_mol = value_ / units%conversion_factor
    end function

    elemental function to_units(self, units) result(energy_per_temperature_amount)
        class(energy_per_temperature_amount_t), intent(in) :: self
        class(energy_per_temperature_amount_unit_t), intent(in) :: units
        double precision :: energy_per_temperature_amount

        energy_per_temperature_amount = self%joules_per_kelvin_mol * units%conversion_factor
    end function

    elemental function double_times_energy_per_temperature_amount( &
            multiplier, energy_per_temperature_amount) result(new_energy_per_temperature_amount)
        double precision, intent(in) :: multiplier
        class(energy_per_temperature_amount_t), intent(in) :: energy_per_temperature_amount
        type(energy_per_temperature_amount_t) :: new_energy_per_temperature_amount

        new_energy_per_temperature_amount%joules_per_kelvin_mol = &
                multiplier * energy_per_temperature_amount%joules_per_kelvin_mol
    end function

    elemental function integer_times_energy_per_temperature_amount( &
            multiplier, energy_per_temperature_amount) result(new_energy_per_temperature_amount)
        integer, intent(in) :: multiplier
        class(energy_per_temperature_amount_t), intent(in) :: energy_per_temperature_amount
        type(energy_per_temperature_amount_t) :: new_energy_per_temperature_amount

        new_energy_per_temperature_amount%joules_per_kelvin_mol = &
                dble(multiplier) * energy_per_temperature_amount%joules_per_kelvin_mol
    end function

    elemental function energy_per_temperature_amount_times_double( &
            energy_per_temperature_amount, multiplier) result(new_energy_per_temperature_amount)
        class(energy_per_temperature_amount_t), intent(in) :: energy_per_temperature_amount
        double precision, intent(in) :: multiplier
        type(energy_per_temperature_amount_t) :: new_energy_per_temperature_amount

        new_energy_per_temperature_amount%joules_per_kelvin_mol = &
                energy_per_temperature_amount%joules_per_kelvin_mol * multiplier
    end function

    elemental function energy_per_temperature_amount_times_integer( &
            energy_per_temperature_amount, multiplier) result(new_energy_per_temperature_amount)
        class(energy_per_temperature_amount_t), intent(in) :: energy_per_temperature_amount
        integer, intent(in) :: multiplier
        type(energy_per_temperature_amount_t) :: new_energy_per_temperature_amount

        new_energy_per_temperature_amount%joules_per_kelvin_mol = &
                energy_per_temperature_amount%joules_per_kelvin_mol * dble(multiplier)
    end function

    elemental function energy_per_temperature_amount_divided_by_double( &
            energy_per_temperature_amount, divisor) result(new_energy_per_temperature_amount)
        class(energy_per_temperature_amount_t), intent(in) :: energy_per_temperature_amount
        double precision, intent(in) :: divisor
        type(energy_per_temperature_amount_t) :: new_energy_per_temperature_amount

        new_energy_per_temperature_amount%joules_per_kelvin_mol = &
                energy_per_temperature_amount%joules_per_kelvin_mol / divisor
    end function

    elemental function energy_per_temperature_amount_divided_by_integer( &
            energy_per_temperature_amount, divisor) result(new_energy_per_temperature_amount)
        class(energy_per_temperature_amount_t), intent(in) :: energy_per_temperature_amount
        integer, intent(in) :: divisor
        type(energy_per_temperature_amount_t) :: new_energy_per_temperature_amount

        new_energy_per_temperature_amount%joules_per_kelvin_mol = &
                energy_per_temperature_amount%joules_per_kelvin_mol / dble(divisor)
    end function

    elemental function energy_per_temp_amt_divided_by_energy_per_temp_amt( &
            numerator, denomenator) result(ratio)
        class(energy_per_temperature_amount_t), intent(in) :: numerator
        type(energy_per_temperature_amount_t), intent(in) :: denomenator
        double precision :: ratio

        ratio = numerator%joules_per_kelvin_mol / denomenator%joules_per_kelvin_mol
    end function

    elemental function energy_per_temp_amt_plus_energy_per_temp_amt( &
            lhs, rhs) result(sum_)
        class(energy_per_temperature_amount_t), intent(in) :: lhs
        type(energy_per_temperature_amount_t), intent(in) :: rhs
        type(energy_per_temperature_amount_t) :: sum_

        sum_%joules_per_kelvin_mol = lhs%joules_per_kelvin_mol + rhs%joules_per_kelvin_mol
    end function

    elemental function energy_per_temp_amt_minus_energy_per_temp_amt( &
            lhs, rhs) result(difference)
        class(energy_per_temperature_amount_t), intent(in) :: lhs
        type(energy_per_temperature_amount_t), intent(in) :: rhs
        type(energy_per_temperature_amount_t) :: difference

        difference%joules_per_kelvin_mol = lhs%joules_per_kelvin_mol - rhs%joules_per_kelvin_mol
    end function

    pure function sum_energy_per_temperature_amount(energy_per_temperature_amounts) result(sum_)
        type(energy_per_temperature_amount_t), intent(in) :: energy_per_temperature_amounts(:)
        type(energy_per_temperature_amount_t) :: sum_

        sum_%joules_per_kelvin_mol = sum(energy_per_temperature_amounts%joules_per_kelvin_mol)
    end function

    elemental function greater_than(lhs, rhs)
        class(energy_per_temperature_amount_t), intent(in) :: lhs
        type(energy_per_temperature_amount_t), intent(in) :: rhs
        logical :: greater_than

        greater_than = lhs%joules_per_kelvin_mol > rhs%joules_per_kelvin_mol
    end function

    elemental function less_than(lhs, rhs)
        class(energy_per_temperature_amount_t), intent(in) :: lhs
        type(energy_per_temperature_amount_t), intent(in) :: rhs
        logical :: less_than

        less_than = lhs%joules_per_kelvin_mol < rhs%joules_per_kelvin_mol
    end function

    elemental function greater_than_or_equal(lhs, rhs)
        class(energy_per_temperature_amount_t), intent(in) :: lhs
        type(energy_per_temperature_amount_t), intent(in) :: rhs
        logical :: greater_than_or_equal

        greater_than_or_equal = lhs%joules_per_kelvin_mol >= rhs%joules_per_kelvin_mol
    end function

    elemental function less_than_or_equal(lhs, rhs)
        class(energy_per_temperature_amount_t), intent(in) :: lhs
        type(energy_per_temperature_amount_t), intent(in) :: rhs
        logical :: less_than_or_equal

        less_than_or_equal = lhs%joules_per_kelvin_mol <= rhs%joules_per_kelvin_mol
    end function

    elemental function equal_(lhs, rhs)
        class(energy_per_temperature_amount_t), intent(in) :: lhs
        type(energy_per_temperature_amount_t), intent(in) :: rhs
        logical :: equal_

        equal_ = lhs%joules_per_kelvin_mol .safeEq. rhs%joules_per_kelvin_mol
    end function

    elemental function equal_within_absolute_(lhs, rhs, within)
        class(energy_per_temperature_amount_t), intent(in) :: lhs
        type(energy_per_temperature_amount_t), intent(in) :: rhs
        type(energy_per_temperature_amount_t), intent(in) :: within
        logical :: equal_within_absolute_

        equal_within_absolute_ = equal_within_absolute( &
                lhs%joules_per_kelvin_mol, rhs%joules_per_kelvin_mol, within%joules_per_kelvin_mol)
    end function

    elemental function equal_within_relative_(lhs, rhs, within)
        class(energy_per_temperature_amount_t), intent(in) :: lhs
        type(energy_per_temperature_amount_t), intent(in) :: rhs
        double precision, intent(in) :: within
        logical :: equal_within_relative_

        equal_within_relative_ = equal_within_relative( &
                lhs%joules_per_kelvin_mol, rhs%joules_per_kelvin_mol, within)
    end function

    elemental function not_equal(lhs, rhs)
        class(energy_per_temperature_amount_t), intent(in) :: lhs
        type(energy_per_temperature_amount_t), intent(in) :: rhs
        logical :: not_equal

        not_equal = .not. lhs == rhs
    end function

    elemental function to_string_full_precision(self) result(string)
        class(energy_per_temperature_amount_t), intent(in) :: self
        type(varying_string) :: string

        string = self%to_string_in(DEFAULT_OUTPUT_UNITS)
    end function

    elemental function to_string_with_precision(self, significant_digits) result(string)
        class(energy_per_temperature_amount_t), intent(in) :: self
        integer, intent(in) :: significant_digits
        type(varying_string) :: string

        string = self%to_string_in(DEFAULT_OUTPUT_UNITS, significant_digits)
    end function

    elemental function to_string_in_full_precision(self, units) result(string)
        class(energy_per_temperature_amount_t), intent(in) :: self
        class(energy_per_temperature_amount_unit_t), intent(in) :: units
        type(varying_string) :: string

        string = units%to_string(to_string(self.in.units))
    end function

    elemental function to_string_in_with_precision( &
            self, units, significant_digits) result(string)
        class(energy_per_temperature_amount_t), intent(in) :: self
        class(energy_per_temperature_amount_unit_t), intent(in) :: units
        integer, intent(in) :: significant_digits
        type(varying_string) :: string

        string = units%to_string(to_string(self.in.units, significant_digits))
    end function

    function fallible_energy_per_temp_amt_from_energy_per_temp_amt( &
            energy_per_temperature_amount) result(fallible_energy_per_temperature_amount)
        type(energy_per_temperature_amount_t), intent(in) :: energy_per_temperature_amount
        type(fallible_energy_per_temperature_amount_t) :: fallible_energy_per_temperature_amount

        fallible_energy_per_temperature_amount%energy_per_temperature_amount_ = energy_per_temperature_amount
    end function

    function fallible_energy_per_temperature_amount_from_errors( &
            errors) result(fallible_energy_per_temperature_amount)
        type(error_list_t), intent(in) :: errors
        type(fallible_energy_per_temperature_amount_t) :: fallible_energy_per_temperature_amount

        fallible_energy_per_temperature_amount%errors_ = errors
    end function

    function fallible_energy_per_temp_amt_from_fallible_energy_per_temp_amt( &
            fallible_energy_per_temperature_amount, &
            module_, &
            procedure_) &
            result(new_fallible_energy_per_temperature_amount)
        type(fallible_energy_per_temperature_amount_t), intent(in) :: fallible_energy_per_temperature_amount
        type(module_t), intent(in) :: module_
        type(procedure_t), intent(in) :: procedure_
        type(fallible_energy_per_temperature_amount_t) :: new_fallible_energy_per_temperature_amount

        if (fallible_energy_per_temperature_amount%failed()) then
            new_fallible_energy_per_temperature_amount%errors_ = error_list_t( &
                    fallible_energy_per_temperature_amount%errors_, module_, procedure_)
        else
            new_fallible_energy_per_temperature_amount%energy_per_temperature_amount_ = &
                    fallible_energy_per_temperature_amount%energy_per_temperature_amount_
        end if
    end function

    elemental function fallible_energy_per_temperature_amount_failed( &
            self) result(failed)
        class(fallible_energy_per_temperature_amount_t), intent(in) :: self
        logical :: failed

        failed = self%errors_%has_any()
    end function

    elemental function fallible_energy_per_temp_amt_energy_per_temp_amt( &
            self) result(energy_per_temperature_amount)
        class(fallible_energy_per_temperature_amount_t), intent(in) :: self
        type(energy_per_temperature_amount_t) :: energy_per_temperature_amount

        energy_per_temperature_amount = self%energy_per_temperature_amount_
    end function

    impure elemental function fallible_energy_per_temperature_amount_errors( &
            self) result(errors)
        class(fallible_energy_per_temperature_amount_t), intent(in) :: self
        type(error_list_t) :: errors

        errors = self%errors_
    end function

    function fallible_energy_per_temperature_amount_unit_from_unit( &
            unit) result(fallible_energy_per_temperature_amount_unit)
        class(energy_per_temperature_amount_unit_t), intent(in) :: unit
        type(fallible_energy_per_temperature_amount_unit_t) :: fallible_energy_per_temperature_amount_unit

        allocate(fallible_energy_per_temperature_amount_unit%unit_, source = unit)
    end function

    function fallible_energy_per_temperature_amount_unit_from_errors( &
            errors) result(fallible_energy_per_temperature_amount_unit)
        type(error_list_t), intent(in) :: errors
        type(fallible_energy_per_temperature_amount_unit_t) :: fallible_energy_per_temperature_amount_unit

        fallible_energy_per_temperature_amount_unit%errors_ = errors
    end function

    function fal_nrgy_per_temp_amt_unit_from_fal_nrgy_per_temp_amt_unit( &
            fallible_energy_per_temperature_amount_unit, &
            module_, &
            procedure_) &
            result(new_fallible_energy_per_temperature_amount_unit)
        type(fallible_energy_per_temperature_amount_unit_t), intent(in) :: fallible_energy_per_temperature_amount_unit
        type(module_t), intent(in) :: module_
        type(procedure_t), intent(in) :: procedure_
        type(fallible_energy_per_temperature_amount_unit_t) :: new_fallible_energy_per_temperature_amount_unit

        if (fallible_energy_per_temperature_amount_unit%failed()) then
            new_fallible_energy_per_temperature_amount_unit%errors_ = error_list_t( &
                    fallible_energy_per_temperature_amount_unit%errors_, module_, procedure_)
        else
            allocate(new_fallible_energy_per_temperature_amount_unit%unit_, source = &
                    fallible_energy_per_temperature_amount_unit%unit_)
        end if
    end function

    elemental function fallible_energy_per_temperature_amount_unit_failed( &
            self) result(failed)
        class(fallible_energy_per_temperature_amount_unit_t), intent(in) :: self
        logical :: failed

        failed = self%errors_%has_any()
    end function

    function fallible_energy_per_temperature_amount_unit_unit( &
            self) result(unit)
        class(fallible_energy_per_temperature_amount_unit_t), intent(in) :: self
        class(energy_per_temperature_amount_unit_t), allocatable :: unit

        allocate(unit, source = self%unit_)
    end function

    impure elemental function fallible_energy_per_temperature_amount_unit_errors( &
            self) result(errors)
        class(fallible_energy_per_temperature_amount_unit_t), intent(in) :: self
        type(error_list_t) :: errors

        errors = self%errors_
    end function

    elemental function simple_unit_to_string(self) result(string)
        class(energy_per_temperature_amount_simple_unit_t), intent(in) :: self
        type(varying_string) :: string

        string = trim(self%symbol)
    end function

    pure function simple_value_to_string(self, value_) result(string)
        class(energy_per_temperature_amount_simple_unit_t), intent(in) :: self
        type(varying_string), intent(in) :: value_
        type(varying_string) :: string

        string = value_ // " " // self%to_string()
    end function

    function simple_parse_as(self, string) result(fallible_energy_per_temperature_amount)
        class(energy_per_temperature_amount_simple_unit_t), intent(in) :: self
        type(varying_string), intent(in) :: string
        type(fallible_energy_per_temperature_amount_t) :: fallible_energy_per_temperature_amount

        type(parse_result_t) :: parse_result

        parse_result = parse_with(the_parser, string)
        if (parse_result%ok) then
            select type (the_number => parse_result%parsed)
            type is (parsed_rational_t)
                fallible_energy_per_temperature_amount = fallible_energy_per_temperature_amount_t(the_number%value_.unit.self)
            end select
        else
            fallible_energy_per_temperature_amount = fallible_energy_per_temperature_amount_t(error_list_t(fatal_t( &
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

    function parse_energy_per_temperature_amount_unit_c(string) result(fallible_energy_per_temperature_amount_unit)
        character(len=*), intent(in) :: string
        type(fallible_energy_per_temperature_amount_unit_t) :: fallible_energy_per_temperature_amount_unit

        fallible_energy_per_temperature_amount_unit = fallible_energy_per_temperature_amount_unit_t( &
                parse_energy_per_temperature_amount_unit(var_str(string), PROVIDED_UNITS), &
                module_t(MODULE_NAME), &
                procedure_t("parse_energy_per_temperature_amount_unit_c"))
    end function

    function parse_energy_per_temperature_amount_unit_s(string) result(fallible_energy_per_temperature_amount_unit)
        type(varying_string), intent(in) :: string
        type(fallible_energy_per_temperature_amount_unit_t) :: fallible_energy_per_temperature_amount_unit

        fallible_energy_per_temperature_amount_unit = fallible_energy_per_temperature_amount_unit_t( &
                parse_energy_per_temperature_amount_unit(string, PROVIDED_UNITS), &
                module_t(MODULE_NAME), &
                procedure_t("parse_energy_per_temperature_amount_unit_s"))
    end function

    function parse_energy_per_temperature_amount_unit_with_units_c( &
            string, units) result(fallible_energy_per_temperature_amount_unit)
        character(len=*), intent(in) :: string
        class(energy_per_temperature_amount_unit_t), intent(in) :: units(:)
        type(fallible_energy_per_temperature_amount_unit_t) :: fallible_energy_per_temperature_amount_unit

        fallible_energy_per_temperature_amount_unit = fallible_energy_per_temperature_amount_unit_t( &
                parse_energy_per_temperature_amount_unit(var_str(string), units), &
                module_t(MODULE_NAME), &
                procedure_t("parse_energy_per_temperature_amount_unit_with_units_c"))
    end function

    function parse_energy_per_temperature_amount_unit_with_units_s( &
            string, units) result(fallible_energy_per_temperature_amount_unit)
        type(varying_string), intent(in) :: string
        class(energy_per_temperature_amount_unit_t), intent(in) :: units(:)
        type(fallible_energy_per_temperature_amount_unit_t) :: fallible_energy_per_temperature_amount_unit

        integer :: i
        type(varying_string) :: unit_strings(size(units))

        do i = 1, size(units)
            if (string == units(i)%to_string()) then
                fallible_energy_per_temperature_amount_unit = fallible_energy_per_temperature_amount_unit_t(units(i))
                return
            end if
        end do
        do i = 1, size(units)
            unit_strings(i) = units(i)%to_string()
        end do
        fallible_energy_per_temperature_amount_unit = fallible_energy_per_temperature_amount_unit_t(error_list_t(fatal_t( &
                UNKNOWN_UNIT, &
                module_t(MODULE_NAME), &
                procedure_t("parse_energy_per_temperature_amount_unit_with_units_s"), &
                '"' // string // '", known units: [' // join(unit_strings, ', ') // ']')))
    end function
end module
