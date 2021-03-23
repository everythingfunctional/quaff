module mass_m
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
            GRAMS_PER_KILOGRAM, &
            POUNDS_PER_KILOGRAM, &
            TONS_PER_KILOGRAM
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
            mass_t, &
            fallible_mass_t, &
            mass_unit_t, &
            fallible_mass_unit_t, &
            mass_simple_unit_t, &
            operator(.unit.), &
            parse_mass, &
            parse_mass_unit, &
            sum, &
            DEFAULT_OUTPUT_UNITS, &
            PROVIDED_UNITS, &
            GRAMS, &
            KILOGRAMS, &
            POUNDS_MASS, &
            TONS

    type :: mass_t
        double precision :: kilograms
    contains
        private
        procedure :: to_units
        generic, public :: operator(.in.) => to_units
        procedure, pass(mass) :: double_times_mass
        procedure, pass(mass) :: integer_times_mass
        procedure :: mass_times_double
        procedure :: mass_times_integer
        generic, public :: operator(*) => &
                double_times_mass, &
                integer_times_mass, &
                mass_times_double, &
                mass_times_integer
        procedure :: mass_divided_by_double
        procedure :: mass_divided_by_integer
        procedure :: mass_divided_by_mass
        generic, public :: operator(/) => &
                mass_divided_by_double, &
                mass_divided_by_integer, &
                mass_divided_by_mass
        procedure :: mass_plus_mass
        generic, public :: operator(+) => mass_plus_mass
        procedure :: mass_minus_mass
        generic, public :: operator(-) => mass_minus_mass
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

    type :: fallible_mass_t
        private
        type(mass_t) :: mass_
        type(error_list_t) :: errors_
    contains
        private
        procedure, public :: failed => fallible_mass_failed
        procedure, public :: mass => fallible_mass_mass
        procedure, public :: errors => fallible_mass_errors
    end type

    type, abstract :: mass_unit_t
        double precision :: conversion_factor
    contains
        procedure(unit_to_string_i), deferred :: unit_to_string
        procedure(value_to_string_i), deferred :: value_to_string
        generic :: to_string => unit_to_string, value_to_string
        procedure(parse_as_i), deferred :: parse_as
    end type

    type :: fallible_mass_unit_t
        private
        class(mass_unit_t), allocatable :: unit_
        type(error_list_t) :: errors_
    contains
        private
        procedure, public :: failed => fallible_mass_unit_failed
        procedure, public :: unit => fallible_mass_unit_unit
        procedure, public :: errors => fallible_mass_unit_errors
    end type

    type, extends(mass_unit_t) :: mass_simple_unit_t
        character(len=20) :: symbol
    contains
        procedure :: unit_to_string => simple_unit_to_string
        procedure :: value_to_string => simple_value_to_string
        procedure :: parse_as => simple_parse_as
    end type

    abstract interface
        elemental function unit_to_string_i(self) result(string)
            import :: mass_unit_t, varying_string

            implicit none

            class(mass_unit_t), intent(in) :: self
            type(varying_string) :: string
        end function

        pure function value_to_string_i(self, value_) result(string)
            import :: mass_unit_t, varying_string

            implicit none

            class(mass_unit_t), intent(in) :: self
            type(varying_string), intent(in) :: value_
            type(varying_string) :: string
        end function

        function parse_as_i(self, string) result(fallible_mass)
            import :: mass_unit_t, fallible_mass_t, varying_string

            implicit none

            class(mass_unit_t), intent(in) :: self
            type(varying_string), intent(in) :: string
            type(fallible_mass_t) :: fallible_mass
        end function
    end interface

    interface operator(.unit.)
        module procedure from_units
    end interface

    interface fallible_mass_t
        module procedure fallible_mass_from_mass
        module procedure fallible_mass_from_errors
        module procedure fallible_mass_from_fallible_mass
    end interface

    interface fallible_mass_unit_t
        module procedure fallible_mass_unit_from_unit
        module procedure fallible_mass_unit_from_errors
        module procedure fallible_mass_unit_from_fallible_mass_unit
    end interface

    interface parse_mass
        module procedure parse_mass_c
        module procedure parse_mass_s
        module procedure parse_mass_with_units_c
        module procedure parse_mass_with_units_s
    end interface

    interface parse_mass_unit
        module procedure parse_mass_unit_c
        module procedure parse_mass_unit_s
        module procedure parse_mass_unit_with_units_c
        module procedure parse_mass_unit_with_units_s
    end interface

    interface sum
        module procedure sum_mass
    end interface

    type(mass_simple_unit_t), parameter :: GRAMS = &
            mass_simple_unit_t( &
                    conversion_factor = GRAMS_PER_KILOGRAM, &
                    symbol = "g")
    type(mass_simple_unit_t), parameter :: KILOGRAMS = &
            mass_simple_unit_t( &
                    conversion_factor = 1.0d0, &
                    symbol = "kg")
    type(mass_simple_unit_t), parameter :: POUNDS_MASS = &
            mass_simple_unit_t( &
                    conversion_factor = POUNDS_PER_KILOGRAM, &
                    symbol = "lbm")
    type(mass_simple_unit_t), parameter :: TONS = &
            mass_simple_unit_t( &
                    conversion_factor = TONS_PER_KILOGRAM, &
                    symbol = "t")

    type(mass_simple_unit_t) :: DEFAULT_OUTPUT_UNITS = KILOGRAMS

    type(mass_simple_unit_t), parameter :: PROVIDED_UNITS(*) = &
            [GRAMS, KILOGRAMS, POUNDS_MASS, TONS]

    character(len=*), parameter :: MODULE_NAME = "mass_m"
contains
    function parse_mass_c(string) result(fallible_mass)
        character(len=*), intent(in) :: string
        type(fallible_mass_t) :: fallible_mass

        fallible_mass = fallible_mass_t( &
                parse_mass(var_str(string), PROVIDED_UNITS), &
                module_t(MODULE_NAME), &
                procedure_t("parse_mass_c"))
    end function

    function parse_mass_s(string) result(fallible_mass)
        type(varying_string), intent(in) :: string
        type(fallible_mass_t) :: fallible_mass

        fallible_mass = fallible_mass_t( &
                parse_mass(string, PROVIDED_UNITS), &
                module_t(MODULE_NAME), &
                procedure_t("parse_mass_s"))
    end function

    function parse_mass_with_units_c( &
            string, units) result(fallible_mass)
        character(len=*), intent(in) :: string
        class(mass_unit_t), intent(in) :: units(:)
        type(fallible_mass_t) :: fallible_mass

        fallible_mass = fallible_mass_t( &
                parse_mass(var_str(string), units), &
                module_t(MODULE_NAME), &
                procedure_t("parse_mass_with_units_c"))
    end function

    function parse_mass_with_units_s( &
            string, units) result(fallible_mass)
        type(varying_string), intent(in) :: string
        class(mass_unit_t), intent(in) :: units(:)
        type(fallible_mass_t) :: fallible_mass

        type(fallible_mass_t) :: all_attempts(size(units))
        integer :: i

        do i = 1, size(units)
            all_attempts(i) = units(i)%parse_as(string)
            if (.not. all_attempts(i)%failed()) then
                fallible_mass = all_attempts(i)
                return
            end if
        end do
        fallible_mass = fallible_mass_t(error_list_t( &
                all_attempts%errors(), &
                module_t(MODULE_NAME), &
                procedure_t("parse_mass_with_units_s")))
    end function

    elemental function from_units(value_, units) result(mass)
        double precision, intent(in) :: value_
        class(mass_unit_t), intent(in) :: units
        type(mass_t) :: mass

        mass%kilograms = value_ / units%conversion_factor
    end function

    elemental function to_units(self, units) result(mass)
        class(mass_t), intent(in) :: self
        class(mass_unit_t), intent(in) :: units
        double precision :: mass

        mass = self%kilograms * units%conversion_factor
    end function

    elemental function double_times_mass( &
            multiplier, mass) result(new_mass)
        double precision, intent(in) :: multiplier
        class(mass_t), intent(in) :: mass
        type(mass_t) :: new_mass

        new_mass%kilograms = &
                multiplier * mass%kilograms
    end function

    elemental function integer_times_mass( &
            multiplier, mass) result(new_mass)
        integer, intent(in) :: multiplier
        class(mass_t), intent(in) :: mass
        type(mass_t) :: new_mass

        new_mass%kilograms = &
                dble(multiplier) * mass%kilograms
    end function

    elemental function mass_times_double( &
            mass, multiplier) result(new_mass)
        class(mass_t), intent(in) :: mass
        double precision, intent(in) :: multiplier
        type(mass_t) :: new_mass

        new_mass%kilograms = &
                mass%kilograms * multiplier
    end function

    elemental function mass_times_integer( &
            mass, multiplier) result(new_mass)
        class(mass_t), intent(in) :: mass
        integer, intent(in) :: multiplier
        type(mass_t) :: new_mass

        new_mass%kilograms = &
                mass%kilograms * dble(multiplier)
    end function

    elemental function mass_divided_by_double( &
            mass, divisor) result(new_mass)
        class(mass_t), intent(in) :: mass
        double precision, intent(in) :: divisor
        type(mass_t) :: new_mass

        new_mass%kilograms = &
                mass%kilograms / divisor
    end function

    elemental function mass_divided_by_integer( &
            mass, divisor) result(new_mass)
        class(mass_t), intent(in) :: mass
        integer, intent(in) :: divisor
        type(mass_t) :: new_mass

        new_mass%kilograms = &
                mass%kilograms / dble(divisor)
    end function

    elemental function mass_divided_by_mass( &
            numerator, denomenator) result(ratio)
        class(mass_t), intent(in) :: numerator
        type(mass_t), intent(in) :: denomenator
        double precision :: ratio

        ratio = numerator%kilograms / denomenator%kilograms
    end function

    elemental function mass_plus_mass( &
            lhs, rhs) result(sum_)
        class(mass_t), intent(in) :: lhs
        type(mass_t), intent(in) :: rhs
        type(mass_t) :: sum_

        sum_%kilograms = lhs%kilograms + rhs%kilograms
    end function

    elemental function mass_minus_mass( &
            lhs, rhs) result(difference)
        class(mass_t), intent(in) :: lhs
        type(mass_t), intent(in) :: rhs
        type(mass_t) :: difference

        difference%kilograms = lhs%kilograms - rhs%kilograms
    end function

    pure function sum_mass(masss) result(sum_)
        type(mass_t), intent(in) :: masss(:)
        type(mass_t) :: sum_

        sum_%kilograms = sum(masss%kilograms)
    end function

    elemental function greater_than(lhs, rhs)
        class(mass_t), intent(in) :: lhs
        type(mass_t), intent(in) :: rhs
        logical :: greater_than

        greater_than = lhs%kilograms > rhs%kilograms
    end function

    elemental function less_than(lhs, rhs)
        class(mass_t), intent(in) :: lhs
        type(mass_t), intent(in) :: rhs
        logical :: less_than

        less_than = lhs%kilograms < rhs%kilograms
    end function

    elemental function greater_than_or_equal(lhs, rhs)
        class(mass_t), intent(in) :: lhs
        type(mass_t), intent(in) :: rhs
        logical :: greater_than_or_equal

        greater_than_or_equal = lhs%kilograms >= rhs%kilograms
    end function

    elemental function less_than_or_equal(lhs, rhs)
        class(mass_t), intent(in) :: lhs
        type(mass_t), intent(in) :: rhs
        logical :: less_than_or_equal

        less_than_or_equal = lhs%kilograms <= rhs%kilograms
    end function

    elemental function equal_(lhs, rhs)
        class(mass_t), intent(in) :: lhs
        type(mass_t), intent(in) :: rhs
        logical :: equal_

        equal_ = lhs%kilograms .safeEq. rhs%kilograms
    end function

    elemental function equal_within_absolute_(lhs, rhs, within)
        class(mass_t), intent(in) :: lhs
        type(mass_t), intent(in) :: rhs
        type(mass_t), intent(in) :: within
        logical :: equal_within_absolute_

        equal_within_absolute_ = equal_within_absolute( &
                lhs%kilograms, rhs%kilograms, within%kilograms)
    end function

    elemental function equal_within_relative_(lhs, rhs, within)
        class(mass_t), intent(in) :: lhs
        type(mass_t), intent(in) :: rhs
        double precision, intent(in) :: within
        logical :: equal_within_relative_

        equal_within_relative_ = equal_within_relative( &
                lhs%kilograms, rhs%kilograms, within)
    end function

    elemental function not_equal(lhs, rhs)
        class(mass_t), intent(in) :: lhs
        type(mass_t), intent(in) :: rhs
        logical :: not_equal

        not_equal = .not. lhs == rhs
    end function

    elemental function to_string_full_precision(self) result(string)
        class(mass_t), intent(in) :: self
        type(varying_string) :: string

        string = self%to_string_in(DEFAULT_OUTPUT_UNITS)
    end function

    elemental function to_string_with_precision(self, significant_digits) result(string)
        class(mass_t), intent(in) :: self
        integer, intent(in) :: significant_digits
        type(varying_string) :: string

        string = self%to_string_in(DEFAULT_OUTPUT_UNITS, significant_digits)
    end function

    elemental function to_string_in_full_precision(self, units) result(string)
        class(mass_t), intent(in) :: self
        class(mass_unit_t), intent(in) :: units
        type(varying_string) :: string

        string = units%to_string(to_string(self.in.units))
    end function

    elemental function to_string_in_with_precision( &
            self, units, significant_digits) result(string)
        class(mass_t), intent(in) :: self
        class(mass_unit_t), intent(in) :: units
        integer, intent(in) :: significant_digits
        type(varying_string) :: string

        string = units%to_string(to_string(self.in.units, significant_digits))
    end function

    function fallible_mass_from_mass( &
            mass) result(fallible_mass)
        type(mass_t), intent(in) :: mass
        type(fallible_mass_t) :: fallible_mass

        fallible_mass%mass_ = mass
    end function

    function fallible_mass_from_errors( &
            errors) result(fallible_mass)
        type(error_list_t), intent(in) :: errors
        type(fallible_mass_t) :: fallible_mass

        fallible_mass%errors_ = errors
    end function

    function fallible_mass_from_fallible_mass( &
            fallible_mass, &
            module_, &
            procedure_) &
            result(new_fallible_mass)
        type(fallible_mass_t), intent(in) :: fallible_mass
        type(module_t), intent(in) :: module_
        type(procedure_t), intent(in) :: procedure_
        type(fallible_mass_t) :: new_fallible_mass

        if (fallible_mass%failed()) then
            new_fallible_mass%errors_ = error_list_t( &
                    fallible_mass%errors_, module_, procedure_)
        else
            new_fallible_mass%mass_ = fallible_mass%mass_
        end if
    end function

    elemental function fallible_mass_failed( &
            self) result(failed)
        class(fallible_mass_t), intent(in) :: self
        logical :: failed

        failed = self%errors_%has_any()
    end function

    elemental function fallible_mass_mass( &
            self) result(mass)
        class(fallible_mass_t), intent(in) :: self
        type(mass_t) :: mass

        mass = self%mass_
    end function

    impure elemental function fallible_mass_errors( &
            self) result(errors)
        class(fallible_mass_t), intent(in) :: self
        type(error_list_t) :: errors

        errors = self%errors_
    end function

    function fallible_mass_unit_from_unit( &
            unit) result(fallible_mass_unit)
        class(mass_unit_t), intent(in) :: unit
        type(fallible_mass_unit_t) :: fallible_mass_unit

        allocate(fallible_mass_unit%unit_, source = unit)
    end function

    function fallible_mass_unit_from_errors( &
            errors) result(fallible_mass_unit)
        type(error_list_t), intent(in) :: errors
        type(fallible_mass_unit_t) :: fallible_mass_unit

        fallible_mass_unit%errors_ = errors
    end function

    function fallible_mass_unit_from_fallible_mass_unit( &
            fallible_mass_unit, &
            module_, &
            procedure_) &
            result(new_fallible_mass_unit)
        type(fallible_mass_unit_t), intent(in) :: fallible_mass_unit
        type(module_t), intent(in) :: module_
        type(procedure_t), intent(in) :: procedure_
        type(fallible_mass_unit_t) :: new_fallible_mass_unit

        if (fallible_mass_unit%failed()) then
            new_fallible_mass_unit%errors_ = error_list_t( &
                    fallible_mass_unit%errors_, module_, procedure_)
        else
            allocate(new_fallible_mass_unit%unit_, source = &
                    fallible_mass_unit%unit_)
        end if
    end function

    elemental function fallible_mass_unit_failed( &
            self) result(failed)
        class(fallible_mass_unit_t), intent(in) :: self
        logical :: failed

        failed = self%errors_%has_any()
    end function

    function fallible_mass_unit_unit( &
            self) result(unit)
        class(fallible_mass_unit_t), intent(in) :: self
        class(mass_unit_t), allocatable :: unit

        allocate(unit, source = self%unit_)
    end function

    impure elemental function fallible_mass_unit_errors( &
            self) result(errors)
        class(fallible_mass_unit_t), intent(in) :: self
        type(error_list_t) :: errors

        errors = self%errors_
    end function

    elemental function simple_unit_to_string(self) result(string)
        class(mass_simple_unit_t), intent(in) :: self
        type(varying_string) :: string

        string = trim(self%symbol)
    end function

    pure function simple_value_to_string(self, value_) result(string)
        class(mass_simple_unit_t), intent(in) :: self
        type(varying_string), intent(in) :: value_
        type(varying_string) :: string

        string = value_ // " " // self%to_string()
    end function

    function simple_parse_as(self, string) result(fallible_mass)
        class(mass_simple_unit_t), intent(in) :: self
        type(varying_string), intent(in) :: string
        type(fallible_mass_t) :: fallible_mass

        type(parse_result_t) :: parse_result

        parse_result = parse_with(the_parser, string)
        if (parse_result%ok()) then
            select type (the_number => parse_result%parsed())
            type is (parsed_rational_t)
                fallible_mass = fallible_mass_t(the_number%value_().unit.self)
            end select
        else
            fallible_mass = fallible_mass_t(error_list_t(fatal_t( &
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

    function parse_mass_unit_c(string) result(fallible_mass_unit)
        character(len=*), intent(in) :: string
        type(fallible_mass_unit_t) :: fallible_mass_unit

        fallible_mass_unit = fallible_mass_unit_t( &
                parse_mass_unit(var_str(string), PROVIDED_UNITS), &
                module_t(MODULE_NAME), &
                procedure_t("parse_mass_unit_c"))
    end function

    function parse_mass_unit_s(string) result(fallible_mass_unit)
        type(varying_string), intent(in) :: string
        type(fallible_mass_unit_t) :: fallible_mass_unit

        fallible_mass_unit = fallible_mass_unit_t( &
                parse_mass_unit(string, PROVIDED_UNITS), &
                module_t(MODULE_NAME), &
                procedure_t("parse_mass_unit_s"))
    end function

    function parse_mass_unit_with_units_c( &
            string, units) result(fallible_mass_unit)
        character(len=*), intent(in) :: string
        class(mass_unit_t), intent(in) :: units(:)
        type(fallible_mass_unit_t) :: fallible_mass_unit

        fallible_mass_unit = fallible_mass_unit_t( &
                parse_mass_unit(var_str(string), units), &
                module_t(MODULE_NAME), &
                procedure_t("parse_mass_unit_with_units_c"))
    end function

    function parse_mass_unit_with_units_s( &
            string, units) result(fallible_mass_unit)
        type(varying_string), intent(in) :: string
        class(mass_unit_t), intent(in) :: units(:)
        type(fallible_mass_unit_t) :: fallible_mass_unit

        integer :: i
        type(varying_string) :: unit_strings(size(units))

        do i = 1, size(units)
            if (string == units(i)%to_string()) then
                fallible_mass_unit = fallible_mass_unit_t(units(i))
                return
            end if
        end do
        do i = 1, size(units)
            unit_strings(i) = units(i)%to_string()
        end do
        fallible_mass_unit = fallible_mass_unit_t(error_list_t(fatal_t( &
                UNKNOWN_UNIT, &
                module_t(MODULE_NAME), &
                procedure_t("parse_mass_unit_with_units_s"), &
                '"' // string // '", known units: [' // join(unit_strings, ', ') // ']')))
    end function
end module
