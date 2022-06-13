module quaff_force_m
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
            DYNES_PER_NEWTON, &
            KILOPONDS_PER_NEWTON, &
            MILLINEWTONS_PER_NEWTON, &
            POUNDS_PER_NEWTON
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
            force_t, &
            fallible_force_t, &
            force_unit_t, &
            fallible_force_unit_t, &
            force_simple_unit_t, &
            operator(.unit.), &
            parse_force, &
            parse_force_unit, &
            sum, &
            abs, &
            DEFAULT_OUTPUT_UNITS, &
            PROVIDED_UNITS, &
            DYNES, &
            KILOPONDS, &
            MILLINEWTONS, &
            NEWTONS, &
            POUNDS_FORCE

    type :: force_t
        double precision :: newtons
    contains
        private
        procedure :: to_units
        generic, public :: operator(.in.) => to_units
        procedure, pass(force) :: double_times_force
        procedure, pass(force) :: integer_times_force
        procedure :: force_times_double
        procedure :: force_times_integer
        generic, public :: operator(*) => &
                double_times_force, &
                integer_times_force, &
                force_times_double, &
                force_times_integer
        procedure :: force_divided_by_double
        procedure :: force_divided_by_integer
        procedure :: force_divided_by_force
        generic, public :: operator(/) => &
                force_divided_by_double, &
                force_divided_by_integer, &
                force_divided_by_force
        procedure :: force_plus_force
        generic, public :: operator(+) => force_plus_force
        procedure :: force_minus_force
        generic, public :: operator(-) => force_minus_force
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

    type :: fallible_force_t
        private
        type(force_t) :: force_ = force_t(0.0d0)
        type(error_list_t) :: errors_
    contains
        private
        procedure, public :: failed => fallible_force_failed
        procedure, public :: force => fallible_force_force
        procedure, public :: errors => fallible_force_errors
    end type

    type, abstract :: force_unit_t
        double precision :: conversion_factor
    contains
        procedure(unit_to_string_i), deferred :: unit_to_string
        procedure(value_to_string_i), deferred :: value_to_string
        generic :: to_string => unit_to_string, value_to_string
        procedure(parse_as_i), deferred :: parse_as
    end type

    type :: fallible_force_unit_t
        private
        class(force_unit_t), allocatable :: unit_
        type(error_list_t) :: errors_
    contains
        private
        procedure, public :: failed => fallible_force_unit_failed
        procedure, public :: unit => fallible_force_unit_unit
        procedure, public :: errors => fallible_force_unit_errors
    end type

    type, extends(force_unit_t) :: force_simple_unit_t
        character(len=20) :: symbol
    contains
        procedure :: unit_to_string => simple_unit_to_string
        procedure :: value_to_string => simple_value_to_string
        procedure :: parse_as => simple_parse_as
    end type

    abstract interface
        elemental function unit_to_string_i(self) result(string)
            import :: force_unit_t, varying_string

            implicit none

            class(force_unit_t), intent(in) :: self
            type(varying_string) :: string
        end function

        pure function value_to_string_i(self, value_) result(string)
            import :: force_unit_t, varying_string

            implicit none

            class(force_unit_t), intent(in) :: self
            type(varying_string), intent(in) :: value_
            type(varying_string) :: string
        end function

        function parse_as_i(self, string) result(fallible_force)
            import :: force_unit_t, fallible_force_t, varying_string

            implicit none

            class(force_unit_t), intent(in) :: self
            type(varying_string), intent(in) :: string
            type(fallible_force_t) :: fallible_force
        end function
    end interface

    interface operator(.unit.)
        module procedure from_units
    end interface

    interface fallible_force_t
        module procedure fallible_force_from_force
        module procedure fallible_force_from_errors
        module procedure fallible_force_from_fallible_force
    end interface

    interface fallible_force_unit_t
        module procedure fallible_force_unit_from_unit
        module procedure fallible_force_unit_from_errors
        module procedure fallible_force_unit_from_fallible_force_unit
    end interface

    interface parse_force
        module procedure parse_force_c
        module procedure parse_force_s
        module procedure parse_force_with_units_c
        module procedure parse_force_with_units_s
    end interface

    interface parse_force_unit
        module procedure parse_force_unit_c
        module procedure parse_force_unit_s
        module procedure parse_force_unit_with_units_c
        module procedure parse_force_unit_with_units_s
    end interface

    interface abs
        module procedure abs_force
    end interface

    interface sum
        module procedure sum_force
    end interface

    type(force_simple_unit_t), parameter :: DYNES = &
            force_simple_unit_t( &
                    conversion_factor = DYNES_PER_NEWTON, &
                    symbol = "dyn")
    type(force_simple_unit_t), parameter :: KILOPONDS = &
            force_simple_unit_t( &
                    conversion_factor = KILOPONDS_PER_NEWTON, &
                    symbol = "kp")
    type(force_simple_unit_t), parameter :: MILLINEWTONS = &
            force_simple_unit_t( &
                    conversion_factor = MILLINEWTONS_PER_NEWTON, &
                    symbol = "mN")
    type(force_simple_unit_t), parameter :: NEWTONS = &
            force_simple_unit_t( &
                    conversion_factor = 1.0d0, &
                    symbol = "N")
    type(force_simple_unit_t), parameter :: POUNDS_FORCE = &
            force_simple_unit_t( &
                    conversion_factor = POUNDS_PER_NEWTON, &
                    symbol = "lbf")

    type(force_simple_unit_t) :: DEFAULT_OUTPUT_UNITS = NEWTONS

    type(force_simple_unit_t), parameter :: PROVIDED_UNITS(*) = &
            [DYNES, KILOPONDS, MILLINEWTONS, NEWTONS, POUNDS_FORCE]

    character(len=*), parameter :: MODULE_NAME = "quaff_force_m"
contains
    function parse_force_c(string) result(fallible_force)
        character(len=*), intent(in) :: string
        type(fallible_force_t) :: fallible_force

        fallible_force = fallible_force_t( &
                parse_force(var_str(string), PROVIDED_UNITS), &
                module_t(MODULE_NAME), &
                procedure_t("parse_force_c"))
    end function

    function parse_force_s(string) result(fallible_force)
        type(varying_string), intent(in) :: string
        type(fallible_force_t) :: fallible_force

        fallible_force = fallible_force_t( &
                parse_force(string, PROVIDED_UNITS), &
                module_t(MODULE_NAME), &
                procedure_t("parse_force_s"))
    end function

    function parse_force_with_units_c( &
            string, units) result(fallible_force)
        character(len=*), intent(in) :: string
        class(force_unit_t), intent(in) :: units(:)
        type(fallible_force_t) :: fallible_force

        fallible_force = fallible_force_t( &
                parse_force(var_str(string), units), &
                module_t(MODULE_NAME), &
                procedure_t("parse_force_with_units_c"))
    end function

    function parse_force_with_units_s( &
            string, units) result(fallible_force)
        type(varying_string), intent(in) :: string
        class(force_unit_t), intent(in) :: units(:)
        type(fallible_force_t) :: fallible_force

        integer :: i

        do i = 1, size(units)
            fallible_force = units(i)%parse_as(string)
            if (.not. fallible_force%failed()) return
        end do
        fallible_force = fallible_force_t(error_list_t(fatal_t( &
                PARSE_ERROR, &
                module_t(MODULE_NAME), &
                procedure_t("parse_force_with_units_s"), &
                "Unable to parse '" // string // "' as a force_t. Tried with units: " &
                // join(units%to_string(), ", "))))
    end function

    elemental function from_units(value_, units) result(force)
        double precision, intent(in) :: value_
        class(force_unit_t), intent(in) :: units
        type(force_t) :: force

        force%newtons = value_ / units%conversion_factor
    end function

    elemental function to_units(self, units) result(force)
        class(force_t), intent(in) :: self
        class(force_unit_t), intent(in) :: units
        double precision :: force

        force = self%newtons * units%conversion_factor
    end function

    elemental function double_times_force( &
            multiplier, force) result(new_force)
        double precision, intent(in) :: multiplier
        class(force_t), intent(in) :: force
        type(force_t) :: new_force

        new_force%newtons = &
                multiplier * force%newtons
    end function

    elemental function integer_times_force( &
            multiplier, force) result(new_force)
        integer, intent(in) :: multiplier
        class(force_t), intent(in) :: force
        type(force_t) :: new_force

        new_force%newtons = &
                dble(multiplier) * force%newtons
    end function

    elemental function force_times_double( &
            force, multiplier) result(new_force)
        class(force_t), intent(in) :: force
        double precision, intent(in) :: multiplier
        type(force_t) :: new_force

        new_force%newtons = &
                force%newtons * multiplier
    end function

    elemental function force_times_integer( &
            force, multiplier) result(new_force)
        class(force_t), intent(in) :: force
        integer, intent(in) :: multiplier
        type(force_t) :: new_force

        new_force%newtons = &
                force%newtons * dble(multiplier)
    end function

    elemental function force_divided_by_double( &
            force, divisor) result(new_force)
        class(force_t), intent(in) :: force
        double precision, intent(in) :: divisor
        type(force_t) :: new_force

        new_force%newtons = &
                force%newtons / divisor
    end function

    elemental function force_divided_by_integer( &
            force, divisor) result(new_force)
        class(force_t), intent(in) :: force
        integer, intent(in) :: divisor
        type(force_t) :: new_force

        new_force%newtons = &
                force%newtons / dble(divisor)
    end function

    elemental function force_divided_by_force( &
            numerator, denomenator) result(ratio)
        class(force_t), intent(in) :: numerator
        type(force_t), intent(in) :: denomenator
        double precision :: ratio

        ratio = numerator%newtons / denomenator%newtons
    end function

    elemental function force_plus_force( &
            lhs, rhs) result(sum_)
        class(force_t), intent(in) :: lhs
        type(force_t), intent(in) :: rhs
        type(force_t) :: sum_

        sum_%newtons = lhs%newtons + rhs%newtons
    end function

    elemental function force_minus_force( &
            lhs, rhs) result(difference)
        class(force_t), intent(in) :: lhs
        type(force_t), intent(in) :: rhs
        type(force_t) :: difference

        difference%newtons = lhs%newtons - rhs%newtons
    end function

    pure function abs_force(force) result(abs_)
        type(force_t), intent(in) :: force
        type(force_t) :: abs_

        abs_%newtons = abs(force%newtons)
    end function

    pure function sum_force(forces) result(sum_)
        type(force_t), intent(in) :: forces(:)
        type(force_t) :: sum_

        sum_%newtons = sum(forces%newtons)
    end function

    elemental function greater_than(lhs, rhs)
        class(force_t), intent(in) :: lhs
        type(force_t), intent(in) :: rhs
        logical :: greater_than

        greater_than = lhs%newtons > rhs%newtons
    end function

    elemental function less_than(lhs, rhs)
        class(force_t), intent(in) :: lhs
        type(force_t), intent(in) :: rhs
        logical :: less_than

        less_than = lhs%newtons < rhs%newtons
    end function

    elemental function greater_than_or_equal(lhs, rhs)
        class(force_t), intent(in) :: lhs
        type(force_t), intent(in) :: rhs
        logical :: greater_than_or_equal

        greater_than_or_equal = lhs%newtons >= rhs%newtons
    end function

    elemental function less_than_or_equal(lhs, rhs)
        class(force_t), intent(in) :: lhs
        type(force_t), intent(in) :: rhs
        logical :: less_than_or_equal

        less_than_or_equal = lhs%newtons <= rhs%newtons
    end function

    elemental function equal_(lhs, rhs)
        class(force_t), intent(in) :: lhs
        type(force_t), intent(in) :: rhs
        logical :: equal_

        equal_ = lhs%newtons .safeEq. rhs%newtons
    end function

    elemental function equal_within_absolute_(lhs, rhs, within)
        class(force_t), intent(in) :: lhs
        type(force_t), intent(in) :: rhs
        type(force_t), intent(in) :: within
        logical :: equal_within_absolute_

        equal_within_absolute_ = equal_within_absolute( &
                lhs%newtons, rhs%newtons, within%newtons)
    end function

    elemental function equal_within_relative_(lhs, rhs, within)
        class(force_t), intent(in) :: lhs
        type(force_t), intent(in) :: rhs
        double precision, intent(in) :: within
        logical :: equal_within_relative_

        equal_within_relative_ = equal_within_relative( &
                lhs%newtons, rhs%newtons, within)
    end function

    elemental function not_equal(lhs, rhs)
        class(force_t), intent(in) :: lhs
        type(force_t), intent(in) :: rhs
        logical :: not_equal

        not_equal = .not. lhs == rhs
    end function

    elemental function to_string_full_precision(self) result(string)
        class(force_t), intent(in) :: self
        type(varying_string) :: string

        string = self%to_string_in(DEFAULT_OUTPUT_UNITS)
    end function

    elemental function to_string_with_precision(self, significant_digits) result(string)
        class(force_t), intent(in) :: self
        integer, intent(in) :: significant_digits
        type(varying_string) :: string

        string = self%to_string_in(DEFAULT_OUTPUT_UNITS, significant_digits)
    end function

    elemental function to_string_in_full_precision(self, units) result(string)
        class(force_t), intent(in) :: self
        class(force_unit_t), intent(in) :: units
        type(varying_string) :: string

        string = units%to_string(to_string(self.in.units))
    end function

    elemental function to_string_in_with_precision( &
            self, units, significant_digits) result(string)
        class(force_t), intent(in) :: self
        class(force_unit_t), intent(in) :: units
        integer, intent(in) :: significant_digits
        type(varying_string) :: string

        string = units%to_string(to_string(self.in.units, significant_digits))
    end function

    function fallible_force_from_force( &
            force) result(fallible_force)
        type(force_t), intent(in) :: force
        type(fallible_force_t) :: fallible_force

        fallible_force%force_ = force
    end function

    function fallible_force_from_errors( &
            errors) result(fallible_force)
        type(error_list_t), intent(in) :: errors
        type(fallible_force_t) :: fallible_force

        fallible_force%errors_ = errors
    end function

    function fallible_force_from_fallible_force( &
            fallible_force, &
            module_, &
            procedure_) &
            result(new_fallible_force)
        type(fallible_force_t), intent(in) :: fallible_force
        type(module_t), intent(in) :: module_
        type(procedure_t), intent(in) :: procedure_
        type(fallible_force_t) :: new_fallible_force

        if (fallible_force%failed()) then
            new_fallible_force%errors_ = error_list_t( &
                    fallible_force%errors_, module_, procedure_)
        else
            new_fallible_force%force_ = fallible_force%force_
        end if
    end function

    elemental function fallible_force_failed( &
            self) result(failed)
        class(fallible_force_t), intent(in) :: self
        logical :: failed

        failed = self%errors_%has_any()
    end function

    elemental function fallible_force_force( &
            self) result(force)
        class(fallible_force_t), intent(in) :: self
        type(force_t) :: force

        force = self%force_
    end function

    impure elemental function fallible_force_errors( &
            self) result(errors)
        class(fallible_force_t), intent(in) :: self
        type(error_list_t) :: errors

        errors = self%errors_
    end function

    function fallible_force_unit_from_unit( &
            unit) result(fallible_force_unit)
        class(force_unit_t), intent(in) :: unit
        type(fallible_force_unit_t) :: fallible_force_unit

        allocate(fallible_force_unit%unit_, source = unit)
    end function

    function fallible_force_unit_from_errors( &
            errors) result(fallible_force_unit)
        type(error_list_t), intent(in) :: errors
        type(fallible_force_unit_t) :: fallible_force_unit

        fallible_force_unit%errors_ = errors
    end function

    function fallible_force_unit_from_fallible_force_unit( &
            fallible_force_unit, &
            module_, &
            procedure_) &
            result(new_fallible_force_unit)
        type(fallible_force_unit_t), intent(in) :: fallible_force_unit
        type(module_t), intent(in) :: module_
        type(procedure_t), intent(in) :: procedure_
        type(fallible_force_unit_t) :: new_fallible_force_unit

        if (fallible_force_unit%failed()) then
            new_fallible_force_unit%errors_ = error_list_t( &
                    fallible_force_unit%errors_, module_, procedure_)
        else
            allocate(new_fallible_force_unit%unit_, source = &
                    fallible_force_unit%unit_)
        end if
    end function

    elemental function fallible_force_unit_failed( &
            self) result(failed)
        class(fallible_force_unit_t), intent(in) :: self
        logical :: failed

        failed = self%errors_%has_any()
    end function

    function fallible_force_unit_unit( &
            self) result(unit)
        class(fallible_force_unit_t), intent(in) :: self
        class(force_unit_t), allocatable :: unit

        allocate(unit, source = self%unit_)
    end function

    impure elemental function fallible_force_unit_errors( &
            self) result(errors)
        class(fallible_force_unit_t), intent(in) :: self
        type(error_list_t) :: errors

        errors = self%errors_
    end function

    elemental function simple_unit_to_string(self) result(string)
        class(force_simple_unit_t), intent(in) :: self
        type(varying_string) :: string

        string = trim(self%symbol)
    end function

    pure function simple_value_to_string(self, value_) result(string)
        class(force_simple_unit_t), intent(in) :: self
        type(varying_string), intent(in) :: value_
        type(varying_string) :: string

        string = value_ // " " // self%to_string()
    end function

    function simple_parse_as(self, string) result(fallible_force)
        class(force_simple_unit_t), intent(in) :: self
        type(varying_string), intent(in) :: string
        type(fallible_force_t) :: fallible_force

        type(parse_result_t) :: parse_result

        parse_result = parse_with(the_parser, string)
        if (parse_result%ok) then
            select type (the_number => parse_result%parsed)
            type is (parsed_rational_t)
                fallible_force = fallible_force_t(the_number%value_.unit.self)
            end select
        else
            fallible_force = fallible_force_t(error_list_t(fatal_t( &
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

    function parse_force_unit_c(string) result(fallible_force_unit)
        character(len=*), intent(in) :: string
        type(fallible_force_unit_t) :: fallible_force_unit

        fallible_force_unit = fallible_force_unit_t( &
                parse_force_unit(var_str(string), PROVIDED_UNITS), &
                module_t(MODULE_NAME), &
                procedure_t("parse_force_unit_c"))
    end function

    function parse_force_unit_s(string) result(fallible_force_unit)
        type(varying_string), intent(in) :: string
        type(fallible_force_unit_t) :: fallible_force_unit

        fallible_force_unit = fallible_force_unit_t( &
                parse_force_unit(string, PROVIDED_UNITS), &
                module_t(MODULE_NAME), &
                procedure_t("parse_force_unit_s"))
    end function

    function parse_force_unit_with_units_c( &
            string, units) result(fallible_force_unit)
        character(len=*), intent(in) :: string
        class(force_unit_t), intent(in) :: units(:)
        type(fallible_force_unit_t) :: fallible_force_unit

        fallible_force_unit = fallible_force_unit_t( &
                parse_force_unit(var_str(string), units), &
                module_t(MODULE_NAME), &
                procedure_t("parse_force_unit_with_units_c"))
    end function

    function parse_force_unit_with_units_s( &
            string, units) result(fallible_force_unit)
        type(varying_string), intent(in) :: string
        class(force_unit_t), intent(in) :: units(:)
        type(fallible_force_unit_t) :: fallible_force_unit

        integer :: i
        type(varying_string) :: unit_strings(size(units))

        do i = 1, size(units)
            if (string == units(i)%to_string()) then
                fallible_force_unit = fallible_force_unit_t(units(i))
                return
            end if
        end do
        do i = 1, size(units)
            unit_strings(i) = units(i)%to_string()
        end do
        fallible_force_unit = fallible_force_unit_t(error_list_t(fatal_t( &
                UNKNOWN_UNIT, &
                module_t(MODULE_NAME), &
                procedure_t("parse_force_unit_with_units_s"), &
                '"' // string // '", known units: [' // join(unit_strings, ', ') // ']')))
    end function
end module
