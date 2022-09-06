module quaff_amount_m
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
    use quaff_conversion_factors_m, only: AVOGADROS_NUMBER, KILO_PER_BASE
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
            amount_t, &
            fallible_amount_t, &
            amount_unit_t, &
            fallible_amount_unit_t, &
            amount_simple_unit_t, &
            operator(.unit.), &
            abs, &
            parse_amount, &
            parse_amount_unit, &
            sum, &
            DEFAULT_OUTPUT_UNITS, &
            PROVIDED_UNITS, &
            KILOMOLS, &
            MOLS, &
            PARTICLES

    type :: amount_t
        double precision :: mols
    contains
        private
        procedure :: to_units
        generic, public :: operator(.in.) => to_units
        procedure, pass(amount) :: double_times_amount
        procedure, pass(amount) :: integer_times_amount
        procedure :: amount_times_double
        procedure :: amount_times_integer
        generic, public :: operator(*) => &
                double_times_amount, &
                integer_times_amount, &
                amount_times_double, &
                amount_times_integer
        procedure :: amount_divided_by_double
        procedure :: amount_divided_by_integer
        procedure :: amount_divided_by_amount
        generic, public :: operator(/) => &
                amount_divided_by_double, &
                amount_divided_by_integer, &
                amount_divided_by_amount
        procedure :: amount_plus_amount
        generic, public :: operator(+) => amount_plus_amount
        procedure :: negate_amount
        procedure :: amount_minus_amount
        generic, public :: operator(-) => &
                negate_amount, &
                amount_minus_amount
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

    type :: fallible_amount_t
        private
        type(amount_t) :: amount_ = amount_t(0.0d0)
        type(error_list_t) :: errors_
    contains
        private
        procedure, public :: failed => fallible_amount_failed
        procedure, public :: amount => fallible_amount_amount
        procedure, public :: errors => fallible_amount_errors
    end type

    type, abstract :: amount_unit_t
        double precision :: conversion_factor
    contains
        procedure(unit_to_string_i), deferred :: unit_to_string
        procedure(value_to_string_i), deferred :: value_to_string
        generic :: to_string => unit_to_string, value_to_string
        procedure(parse_as_i), deferred :: parse_as
    end type

    type :: fallible_amount_unit_t
        private
        class(amount_unit_t), allocatable :: unit_
        type(error_list_t) :: errors_
    contains
        private
        procedure, public :: failed => fallible_amount_unit_failed
        procedure, public :: unit => fallible_amount_unit_unit
        procedure, public :: errors => fallible_amount_unit_errors
    end type

    type, extends(amount_unit_t) :: amount_simple_unit_t
        character(len=20) :: symbol
    contains
        procedure :: unit_to_string => simple_unit_to_string
        procedure :: value_to_string => simple_value_to_string
        procedure :: parse_as => simple_parse_as
    end type

    abstract interface
        elemental function unit_to_string_i(self) result(string)
            import :: amount_unit_t, varying_string

            implicit none

            class(amount_unit_t), intent(in) :: self
            type(varying_string) :: string
        end function

        pure function value_to_string_i(self, value_) result(string)
            import :: amount_unit_t, varying_string

            implicit none

            class(amount_unit_t), intent(in) :: self
            type(varying_string), intent(in) :: value_
            type(varying_string) :: string
        end function

        function parse_as_i(self, string) result(fallible_amount)
            import :: amount_unit_t, fallible_amount_t, varying_string

            implicit none

            class(amount_unit_t), intent(in) :: self
            type(varying_string), intent(in) :: string
            type(fallible_amount_t) :: fallible_amount
        end function
    end interface

    interface operator(.unit.)
        module procedure from_units
    end interface

    interface fallible_amount_t
        module procedure fallible_amount_from_amount
        module procedure fallible_amount_from_errors
        module procedure fallible_amount_from_fallible_amount
    end interface

    interface fallible_amount_unit_t
        module procedure fallible_amount_unit_from_unit
        module procedure fallible_amount_unit_from_errors
        module procedure fallible_amount_unit_from_fallible_amount_unit
    end interface

    interface parse_amount
        module procedure parse_amount_c
        module procedure parse_amount_s
        module procedure parse_amount_with_units_c
        module procedure parse_amount_with_units_s
    end interface

    interface parse_amount_unit
        module procedure parse_amount_unit_c
        module procedure parse_amount_unit_s
        module procedure parse_amount_unit_with_units_c
        module procedure parse_amount_unit_with_units_s
    end interface

    interface abs
        module procedure abs_amount
    end interface

    interface sum
        module procedure sum_amount
    end interface

    type(amount_simple_unit_t), parameter :: KILOMOLS = &
            amount_simple_unit_t( &
                    conversion_factor = KILO_PER_BASE, &
                    symbol = "kmol")
    type(amount_simple_unit_t), parameter :: MOLS = &
            amount_simple_unit_t( &
                    conversion_factor = 1.0d0, &
                    symbol = "mol")
    type(amount_simple_unit_t), parameter :: PARTICLES = &
            amount_simple_unit_t( &
                    conversion_factor = AVOGADROS_NUMBER, &
                    symbol = "particles")

    type(amount_simple_unit_t) :: DEFAULT_OUTPUT_UNITS = MOLS

    type(amount_simple_unit_t), parameter :: PROVIDED_UNITS(*) = &
            [KILOMOLS, MOLS, PARTICLES]

    character(len=*), parameter :: MODULE_NAME = "quaff_amount_m"
contains
    function parse_amount_c(string) result(fallible_amount)
        character(len=*), intent(in) :: string
        type(fallible_amount_t) :: fallible_amount

        fallible_amount = fallible_amount_t( &
                parse_amount(var_str(string), PROVIDED_UNITS), &
                module_t(MODULE_NAME), &
                procedure_t("parse_amount_c"))
    end function

    function parse_amount_s(string) result(fallible_amount)
        type(varying_string), intent(in) :: string
        type(fallible_amount_t) :: fallible_amount

        fallible_amount = fallible_amount_t( &
                parse_amount(string, PROVIDED_UNITS), &
                module_t(MODULE_NAME), &
                procedure_t("parse_amount_s"))
    end function

    function parse_amount_with_units_c( &
            string, units) result(fallible_amount)
        character(len=*), intent(in) :: string
        class(amount_unit_t), intent(in) :: units(:)
        type(fallible_amount_t) :: fallible_amount

        fallible_amount = fallible_amount_t( &
                parse_amount(var_str(string), units), &
                module_t(MODULE_NAME), &
                procedure_t("parse_amount_with_units_c"))
    end function

    function parse_amount_with_units_s( &
            string, units) result(fallible_amount)
        type(varying_string), intent(in) :: string
        class(amount_unit_t), intent(in) :: units(:)
        type(fallible_amount_t) :: fallible_amount

        integer :: i

        do i = 1, size(units)
            fallible_amount = units(i)%parse_as(string)
            if (.not. fallible_amount%failed()) return
        end do
        fallible_amount = fallible_amount_t(error_list_t(fatal_t( &
                PARSE_ERROR, &
                module_t(MODULE_NAME), &
                procedure_t("parse_amount_with_units_s"), &
                "Unable to parse '" // string // "' as a amount_t. Tried with units: " &
                // join(units%to_string(), ", "))))
    end function

    elemental function from_units(value_, units) result(amount)
        double precision, intent(in) :: value_
        class(amount_unit_t), intent(in) :: units
        type(amount_t) :: amount

        amount%mols = value_ / units%conversion_factor
    end function

    elemental function to_units(self, units) result(amount)
        class(amount_t), intent(in) :: self
        class(amount_unit_t), intent(in) :: units
        double precision :: amount

        amount = self%mols * units%conversion_factor
    end function

    elemental function double_times_amount( &
            multiplier, amount) result(new_amount)
        double precision, intent(in) :: multiplier
        class(amount_t), intent(in) :: amount
        type(amount_t) :: new_amount

        new_amount%mols = &
                multiplier * amount%mols
    end function

    elemental function integer_times_amount( &
            multiplier, amount) result(new_amount)
        integer, intent(in) :: multiplier
        class(amount_t), intent(in) :: amount
        type(amount_t) :: new_amount

        new_amount%mols = &
                dble(multiplier) * amount%mols
    end function

    elemental function amount_times_double( &
            amount, multiplier) result(new_amount)
        class(amount_t), intent(in) :: amount
        double precision, intent(in) :: multiplier
        type(amount_t) :: new_amount

        new_amount%mols = &
                amount%mols * multiplier
    end function

    elemental function amount_times_integer( &
            amount, multiplier) result(new_amount)
        class(amount_t), intent(in) :: amount
        integer, intent(in) :: multiplier
        type(amount_t) :: new_amount

        new_amount%mols = &
                amount%mols * dble(multiplier)
    end function

    elemental function amount_divided_by_double( &
            amount, divisor) result(new_amount)
        class(amount_t), intent(in) :: amount
        double precision, intent(in) :: divisor
        type(amount_t) :: new_amount

        new_amount%mols = &
                amount%mols / divisor
    end function

    elemental function amount_divided_by_integer( &
            amount, divisor) result(new_amount)
        class(amount_t), intent(in) :: amount
        integer, intent(in) :: divisor
        type(amount_t) :: new_amount

        new_amount%mols = &
                amount%mols / dble(divisor)
    end function

    elemental function amount_divided_by_amount( &
            numerator, denomenator) result(ratio)
        class(amount_t), intent(in) :: numerator
        type(amount_t), intent(in) :: denomenator
        double precision :: ratio

        ratio = numerator%mols / denomenator%mols
    end function

    elemental function amount_plus_amount( &
            lhs, rhs) result(sum_)
        class(amount_t), intent(in) :: lhs
        type(amount_t), intent(in) :: rhs
        type(amount_t) :: sum_

        sum_%mols = lhs%mols + rhs%mols
    end function

    elemental function negate_amount(self) result(negated)
        class(amount_t), intent(in) :: self
        type(amount_t) :: negated

        negated%mols = -self%mols
    end function

    elemental function amount_minus_amount( &
            lhs, rhs) result(difference)
        class(amount_t), intent(in) :: lhs
        type(amount_t), intent(in) :: rhs
        type(amount_t) :: difference

        difference%mols = lhs%mols - rhs%mols
    end function

    elemental function abs_amount(amount) result(abs_)
        type(amount_t), intent(in) :: amount
        type(amount_t) :: abs_

        abs_%mols = abs(amount%mols)
    end function

    pure function sum_amount(amounts) result(sum_)
        type(amount_t), intent(in) :: amounts(:)
        type(amount_t) :: sum_

        sum_%mols = sum(amounts%mols)
    end function

    elemental function greater_than(lhs, rhs)
        class(amount_t), intent(in) :: lhs
        type(amount_t), intent(in) :: rhs
        logical :: greater_than

        greater_than = lhs%mols > rhs%mols
    end function

    elemental function less_than(lhs, rhs)
        class(amount_t), intent(in) :: lhs
        type(amount_t), intent(in) :: rhs
        logical :: less_than

        less_than = lhs%mols < rhs%mols
    end function

    elemental function greater_than_or_equal(lhs, rhs)
        class(amount_t), intent(in) :: lhs
        type(amount_t), intent(in) :: rhs
        logical :: greater_than_or_equal

        greater_than_or_equal = lhs%mols >= rhs%mols
    end function

    elemental function less_than_or_equal(lhs, rhs)
        class(amount_t), intent(in) :: lhs
        type(amount_t), intent(in) :: rhs
        logical :: less_than_or_equal

        less_than_or_equal = lhs%mols <= rhs%mols
    end function

    elemental function equal_(lhs, rhs)
        class(amount_t), intent(in) :: lhs
        type(amount_t), intent(in) :: rhs
        logical :: equal_

        equal_ = lhs%mols .safeEq. rhs%mols
    end function

    elemental function equal_within_absolute_(lhs, rhs, within)
        class(amount_t), intent(in) :: lhs
        type(amount_t), intent(in) :: rhs
        type(amount_t), intent(in) :: within
        logical :: equal_within_absolute_

        equal_within_absolute_ = equal_within_absolute( &
                lhs%mols, rhs%mols, within%mols)
    end function

    elemental function equal_within_relative_(lhs, rhs, within)
        class(amount_t), intent(in) :: lhs
        type(amount_t), intent(in) :: rhs
        double precision, intent(in) :: within
        logical :: equal_within_relative_

        equal_within_relative_ = equal_within_relative( &
                lhs%mols, rhs%mols, within)
    end function

    elemental function not_equal(lhs, rhs)
        class(amount_t), intent(in) :: lhs
        type(amount_t), intent(in) :: rhs
        logical :: not_equal

        not_equal = .not. lhs == rhs
    end function

    elemental function to_string_full_precision(self) result(string)
        class(amount_t), intent(in) :: self
        type(varying_string) :: string

        string = self%to_string_in(DEFAULT_OUTPUT_UNITS)
    end function

    elemental function to_string_with_precision(self, significant_digits) result(string)
        class(amount_t), intent(in) :: self
        integer, intent(in) :: significant_digits
        type(varying_string) :: string

        string = self%to_string_in(DEFAULT_OUTPUT_UNITS, significant_digits)
    end function

    elemental function to_string_in_full_precision(self, units) result(string)
        class(amount_t), intent(in) :: self
        class(amount_unit_t), intent(in) :: units
        type(varying_string) :: string

        string = units%to_string(to_string(self.in.units))
    end function

    elemental function to_string_in_with_precision( &
            self, units, significant_digits) result(string)
        class(amount_t), intent(in) :: self
        class(amount_unit_t), intent(in) :: units
        integer, intent(in) :: significant_digits
        type(varying_string) :: string

        string = units%to_string(to_string(self.in.units, significant_digits))
    end function

    function fallible_amount_from_amount( &
            amount) result(fallible_amount)
        type(amount_t), intent(in) :: amount
        type(fallible_amount_t) :: fallible_amount

        fallible_amount%amount_ = amount
    end function

    function fallible_amount_from_errors( &
            errors) result(fallible_amount)
        type(error_list_t), intent(in) :: errors
        type(fallible_amount_t) :: fallible_amount

        fallible_amount%errors_ = errors
    end function

    function fallible_amount_from_fallible_amount( &
            fallible_amount, &
            module_, &
            procedure_) &
            result(new_fallible_amount)
        type(fallible_amount_t), intent(in) :: fallible_amount
        type(module_t), intent(in) :: module_
        type(procedure_t), intent(in) :: procedure_
        type(fallible_amount_t) :: new_fallible_amount

        if (fallible_amount%failed()) then
            new_fallible_amount%errors_ = error_list_t( &
                    fallible_amount%errors_, module_, procedure_)
        else
            new_fallible_amount%amount_ = fallible_amount%amount_
        end if
    end function

    elemental function fallible_amount_failed( &
            self) result(failed)
        class(fallible_amount_t), intent(in) :: self
        logical :: failed

        failed = self%errors_%has_any()
    end function

    elemental function fallible_amount_amount( &
            self) result(amount)
        class(fallible_amount_t), intent(in) :: self
        type(amount_t) :: amount

        amount = self%amount_
    end function

    impure elemental function fallible_amount_errors( &
            self) result(errors)
        class(fallible_amount_t), intent(in) :: self
        type(error_list_t) :: errors

        errors = self%errors_
    end function

    function fallible_amount_unit_from_unit( &
            unit) result(fallible_amount_unit)
        class(amount_unit_t), intent(in) :: unit
        type(fallible_amount_unit_t) :: fallible_amount_unit

        allocate(fallible_amount_unit%unit_, source = unit)
    end function

    function fallible_amount_unit_from_errors( &
            errors) result(fallible_amount_unit)
        type(error_list_t), intent(in) :: errors
        type(fallible_amount_unit_t) :: fallible_amount_unit

        fallible_amount_unit%errors_ = errors
    end function

    function fallible_amount_unit_from_fallible_amount_unit( &
            fallible_amount_unit, &
            module_, &
            procedure_) &
            result(new_fallible_amount_unit)
        type(fallible_amount_unit_t), intent(in) :: fallible_amount_unit
        type(module_t), intent(in) :: module_
        type(procedure_t), intent(in) :: procedure_
        type(fallible_amount_unit_t) :: new_fallible_amount_unit

        if (fallible_amount_unit%failed()) then
            new_fallible_amount_unit%errors_ = error_list_t( &
                    fallible_amount_unit%errors_, module_, procedure_)
        else
            allocate(new_fallible_amount_unit%unit_, source = &
                    fallible_amount_unit%unit_)
        end if
    end function

    elemental function fallible_amount_unit_failed( &
            self) result(failed)
        class(fallible_amount_unit_t), intent(in) :: self
        logical :: failed

        failed = self%errors_%has_any()
    end function

    function fallible_amount_unit_unit( &
            self) result(unit)
        class(fallible_amount_unit_t), intent(in) :: self
        class(amount_unit_t), allocatable :: unit

        allocate(unit, source = self%unit_)
    end function

    impure elemental function fallible_amount_unit_errors( &
            self) result(errors)
        class(fallible_amount_unit_t), intent(in) :: self
        type(error_list_t) :: errors

        errors = self%errors_
    end function

    elemental function simple_unit_to_string(self) result(string)
        class(amount_simple_unit_t), intent(in) :: self
        type(varying_string) :: string

        string = trim(self%symbol)
    end function

    pure function simple_value_to_string(self, value_) result(string)
        class(amount_simple_unit_t), intent(in) :: self
        type(varying_string), intent(in) :: value_
        type(varying_string) :: string

        string = value_ // " " // self%to_string()
    end function

    function simple_parse_as(self, string) result(fallible_amount)
        class(amount_simple_unit_t), intent(in) :: self
        type(varying_string), intent(in) :: string
        type(fallible_amount_t) :: fallible_amount

        type(parse_result_t) :: parse_result

        parse_result = parse_with(the_parser, string)
        if (parse_result%ok) then
            select type (the_number => parse_result%parsed)
            type is (parsed_rational_t)
                fallible_amount = fallible_amount_t(the_number%value_.unit.self)
            end select
        else
            fallible_amount = fallible_amount_t(error_list_t(fatal_t( &
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

    function parse_amount_unit_c(string) result(fallible_amount_unit)
        character(len=*), intent(in) :: string
        type(fallible_amount_unit_t) :: fallible_amount_unit

        fallible_amount_unit = fallible_amount_unit_t( &
                parse_amount_unit(var_str(string), PROVIDED_UNITS), &
                module_t(MODULE_NAME), &
                procedure_t("parse_amount_unit_c"))
    end function

    function parse_amount_unit_s(string) result(fallible_amount_unit)
        type(varying_string), intent(in) :: string
        type(fallible_amount_unit_t) :: fallible_amount_unit

        fallible_amount_unit = fallible_amount_unit_t( &
                parse_amount_unit(string, PROVIDED_UNITS), &
                module_t(MODULE_NAME), &
                procedure_t("parse_amount_unit_s"))
    end function

    function parse_amount_unit_with_units_c( &
            string, units) result(fallible_amount_unit)
        character(len=*), intent(in) :: string
        class(amount_unit_t), intent(in) :: units(:)
        type(fallible_amount_unit_t) :: fallible_amount_unit

        fallible_amount_unit = fallible_amount_unit_t( &
                parse_amount_unit(var_str(string), units), &
                module_t(MODULE_NAME), &
                procedure_t("parse_amount_unit_with_units_c"))
    end function

    function parse_amount_unit_with_units_s( &
            string, units) result(fallible_amount_unit)
        type(varying_string), intent(in) :: string
        class(amount_unit_t), intent(in) :: units(:)
        type(fallible_amount_unit_t) :: fallible_amount_unit

        integer :: i
        type(varying_string), allocatable :: unit_strings(:)

        do i = 1, size(units)
            if (string == units(i)%to_string()) then
                fallible_amount_unit = fallible_amount_unit_t(units(i))
                return
            end if
        end do
        allocate(unit_strings(size(units)))
        do i = 1, size(units)
            unit_strings(i) = units(i)%to_string()
        end do
        fallible_amount_unit = fallible_amount_unit_t(error_list_t(fatal_t( &
                UNKNOWN_UNIT, &
                module_t(MODULE_NAME), &
                procedure_t("parse_amount_unit_with_units_s"), &
                '"' // string // '", known units: [' // join(unit_strings, ', ') // ']')))
    end function
end module
