module quaff_fluence_m
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
            PARTICLES_PER_SQUARE_CENTIMETER_PER_PARTICLES_PER_SQUARE_METER
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
            fluence_t, &
            fallible_fluence_t, &
            fluence_unit_t, &
            fallible_fluence_unit_t, &
            fluence_simple_unit_t, &
            operator(.unit.), &
            parse_fluence, &
            parse_fluence_unit, &
            sum, &
            abs, &
            DEFAULT_OUTPUT_UNITS, &
            PROVIDED_UNITS, &
            PARTICLES_PER_SQUARE_METER, &
            PARTICLES_PER_SQUARE_CENTIMETER

    type :: fluence_t
        double precision :: particles_per_square_meter
    contains
        private
        procedure :: to_units
        generic, public :: operator(.in.) => to_units
        procedure, pass(fluence) :: double_times_fluence
        procedure, pass(fluence) :: integer_times_fluence
        procedure :: fluence_times_double
        procedure :: fluence_times_integer
        generic, public :: operator(*) => &
                double_times_fluence, &
                integer_times_fluence, &
                fluence_times_double, &
                fluence_times_integer
        procedure :: fluence_divided_by_double
        procedure :: fluence_divided_by_integer
        procedure :: fluence_divided_by_fluence
        generic, public :: operator(/) => &
                fluence_divided_by_double, &
                fluence_divided_by_integer, &
                fluence_divided_by_fluence
        procedure :: fluence_plus_fluence
        generic, public :: operator(+) => fluence_plus_fluence
        procedure :: negate_fluence
        procedure :: fluence_minus_fluence
        generic, public :: operator(-) => &
                negate_fluence, &
                fluence_minus_fluence
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

    type :: fallible_fluence_t
        private
        type(fluence_t) :: fluence_ = fluence_t(0.0d0)
        type(error_list_t) :: errors_
    contains
        private
        procedure, public :: failed => fallible_fluence_failed
        procedure, public :: fluence => fallible_fluence_fluence
        procedure, public :: errors => fallible_fluence_errors
    end type

    type, abstract :: fluence_unit_t
        double precision :: conversion_factor
    contains
        procedure(unit_to_string_i), deferred :: unit_to_string
        procedure(value_to_string_i), deferred :: value_to_string
        generic :: to_string => unit_to_string, value_to_string
        procedure(parse_as_i), deferred :: parse_as
    end type

    type :: fallible_fluence_unit_t
        private
        class(fluence_unit_t), allocatable :: unit_
        type(error_list_t) :: errors_
    contains
        private
        procedure, public :: failed => fallible_fluence_unit_failed
        procedure, public :: unit => fallible_fluence_unit_unit
        procedure, public :: errors => fallible_fluence_unit_errors
    end type

    type, extends(fluence_unit_t) :: fluence_simple_unit_t
        character(len=20) :: symbol
    contains
        procedure :: unit_to_string => simple_unit_to_string
        procedure :: value_to_string => simple_value_to_string
        procedure :: parse_as => simple_parse_as
    end type

    abstract interface
        elemental function unit_to_string_i(self) result(string)
            import :: fluence_unit_t, varying_string

            implicit none

            class(fluence_unit_t), intent(in) :: self
            type(varying_string) :: string
        end function

        pure function value_to_string_i(self, value_) result(string)
            import :: fluence_unit_t, varying_string

            implicit none

            class(fluence_unit_t), intent(in) :: self
            type(varying_string), intent(in) :: value_
            type(varying_string) :: string
        end function

        function parse_as_i(self, string) result(fallible_fluence)
            import :: fluence_unit_t, fallible_fluence_t, varying_string

            implicit none

            class(fluence_unit_t), intent(in) :: self
            type(varying_string), intent(in) :: string
            type(fallible_fluence_t) :: fallible_fluence
        end function
    end interface

    interface operator(.unit.)
        module procedure from_units
    end interface

    interface fallible_fluence_t
        module procedure fallible_fluence_from_fluence
        module procedure fallible_fluence_from_errors
        module procedure fallible_fluence_from_fallible_fluence
    end interface

    interface fallible_fluence_unit_t
        module procedure fallible_fluence_unit_from_unit
        module procedure fallible_fluence_unit_from_errors
        module procedure fallible_fluence_unit_from_fallible_fluence_unit
    end interface

    interface parse_fluence
        module procedure parse_fluence_c
        module procedure parse_fluence_s
        module procedure parse_fluence_with_units_c
        module procedure parse_fluence_with_units_s
    end interface

    interface parse_fluence_unit
        module procedure parse_fluence_unit_c
        module procedure parse_fluence_unit_s
        module procedure parse_fluence_unit_with_units_c
        module procedure parse_fluence_unit_with_units_s
    end interface

    interface abs
        module procedure abs_fluence
    end interface
    interface sum
        module procedure sum_fluence
    end interface

    type(fluence_simple_unit_t), parameter :: PARTICLES_PER_SQUARE_CENTIMETER = &
            fluence_simple_unit_t( &
                    conversion_factor = PARTICLES_PER_SQUARE_CENTIMETER_PER_PARTICLES_PER_SQUARE_METER, &
                    symbol = "particles/cm^2")
    type(fluence_simple_unit_t), parameter :: PARTICLES_PER_SQUARE_METER = &
            fluence_simple_unit_t( &
                    conversion_factor = 1.0d0, &
                    symbol = "particles/m^2")

    type(fluence_simple_unit_t) :: DEFAULT_OUTPUT_UNITS = PARTICLES_PER_SQUARE_METER

    type(fluence_simple_unit_t), parameter :: PROVIDED_UNITS(*) = &
            [PARTICLES_PER_SQUARE_CENTIMETER, PARTICLES_PER_SQUARE_METER]

    character(len=*), parameter :: MODULE_NAME = "quaff_fluence_m"
contains
    function parse_fluence_c(string) result(fallible_fluence)
        character(len=*), intent(in) :: string
        type(fallible_fluence_t) :: fallible_fluence

        fallible_fluence = fallible_fluence_t( &
                parse_fluence(var_str(string), PROVIDED_UNITS), &
                module_t(MODULE_NAME), &
                procedure_t("parse_fluence_c"))
    end function

    function parse_fluence_s(string) result(fallible_fluence)
        type(varying_string), intent(in) :: string
        type(fallible_fluence_t) :: fallible_fluence

        fallible_fluence = fallible_fluence_t( &
                parse_fluence(string, PROVIDED_UNITS), &
                module_t(MODULE_NAME), &
                procedure_t("parse_fluence_s"))
    end function

    function parse_fluence_with_units_c( &
            string, units) result(fallible_fluence)
        character(len=*), intent(in) :: string
        class(fluence_unit_t), intent(in) :: units(:)
        type(fallible_fluence_t) :: fallible_fluence

        fallible_fluence = fallible_fluence_t( &
                parse_fluence(var_str(string), units), &
                module_t(MODULE_NAME), &
                procedure_t("parse_fluence_with_units_c"))
    end function

    function parse_fluence_with_units_s( &
            string, units) result(fallible_fluence)
        type(varying_string), intent(in) :: string
        class(fluence_unit_t), intent(in) :: units(:)
        type(fallible_fluence_t) :: fallible_fluence

        integer :: i

        do i = 1, size(units)
            fallible_fluence = units(i)%parse_as(string)
            if (.not. fallible_fluence%failed()) return
        end do
        fallible_fluence = fallible_fluence_t(error_list_t(fatal_t( &
                PARSE_ERROR, &
                module_t(MODULE_NAME), &
                procedure_t("parse_fluence_with_units_s"), &
                "Unable to parse '" // string // "' as a fluence_t. Tried with units: " &
                // join(units%to_string(), ", "))))
    end function

    elemental function from_units(value_, units) result(fluence)
        double precision, intent(in) :: value_
        class(fluence_unit_t), intent(in) :: units
        type(fluence_t) :: fluence

        fluence%particles_per_square_meter = value_ / units%conversion_factor
    end function

    elemental function to_units(self, units) result(fluence)
        class(fluence_t), intent(in) :: self
        class(fluence_unit_t), intent(in) :: units
        double precision :: fluence

        fluence = self%particles_per_square_meter * units%conversion_factor
    end function

    elemental function double_times_fluence( &
            multiplier, fluence) result(new_fluence)
        double precision, intent(in) :: multiplier
        class(fluence_t), intent(in) :: fluence
        type(fluence_t) :: new_fluence

        new_fluence%particles_per_square_meter = &
                multiplier * fluence%particles_per_square_meter
    end function

    elemental function integer_times_fluence( &
            multiplier, fluence) result(new_fluence)
        integer, intent(in) :: multiplier
        class(fluence_t), intent(in) :: fluence
        type(fluence_t) :: new_fluence

        new_fluence%particles_per_square_meter = &
                dble(multiplier) * fluence%particles_per_square_meter
    end function

    elemental function fluence_times_double( &
            fluence, multiplier) result(new_fluence)
        class(fluence_t), intent(in) :: fluence
        double precision, intent(in) :: multiplier
        type(fluence_t) :: new_fluence

        new_fluence%particles_per_square_meter = &
                fluence%particles_per_square_meter * multiplier
    end function

    elemental function fluence_times_integer( &
            fluence, multiplier) result(new_fluence)
        class(fluence_t), intent(in) :: fluence
        integer, intent(in) :: multiplier
        type(fluence_t) :: new_fluence

        new_fluence%particles_per_square_meter = &
                fluence%particles_per_square_meter * dble(multiplier)
    end function

    elemental function fluence_divided_by_double( &
            fluence, divisor) result(new_fluence)
        class(fluence_t), intent(in) :: fluence
        double precision, intent(in) :: divisor
        type(fluence_t) :: new_fluence

        new_fluence%particles_per_square_meter = &
                fluence%particles_per_square_meter / divisor
    end function

    elemental function fluence_divided_by_integer( &
            fluence, divisor) result(new_fluence)
        class(fluence_t), intent(in) :: fluence
        integer, intent(in) :: divisor
        type(fluence_t) :: new_fluence

        new_fluence%particles_per_square_meter = &
                fluence%particles_per_square_meter / dble(divisor)
    end function

    elemental function fluence_divided_by_fluence( &
            numerator, denomenator) result(ratio)
        class(fluence_t), intent(in) :: numerator
        type(fluence_t), intent(in) :: denomenator
        double precision :: ratio

        ratio = numerator%particles_per_square_meter / denomenator%particles_per_square_meter
    end function

    elemental function fluence_plus_fluence( &
            lhs, rhs) result(sum_)
        class(fluence_t), intent(in) :: lhs
        type(fluence_t), intent(in) :: rhs
        type(fluence_t) :: sum_

        sum_%particles_per_square_meter = lhs%particles_per_square_meter + rhs%particles_per_square_meter
    end function

    elemental function negate_fluence(self) result(negated)
        class(fluence_t), intent(in) :: self
        type(fluence_t) :: negated

        negated%particles_per_square_meter = -self%particles_per_square_meter
    end function

    elemental function fluence_minus_fluence( &
            lhs, rhs) result(difference)
        class(fluence_t), intent(in) :: lhs
        type(fluence_t), intent(in) :: rhs
        type(fluence_t) :: difference

        difference%particles_per_square_meter = lhs%particles_per_square_meter - rhs%particles_per_square_meter
    end function

    pure function abs_fluence(fluence) result(abs_)
        type(fluence_t), intent(in) :: fluence
        type(fluence_t) :: abs_

        abs_%particles_per_square_meter = abs(fluence%particles_per_square_meter)
    end function

    pure function sum_fluence(fluences) result(sum_)
        type(fluence_t), intent(in) :: fluences(:)
        type(fluence_t) :: sum_

        sum_%particles_per_square_meter = sum(fluences%particles_per_square_meter)
    end function

    elemental function greater_than(lhs, rhs)
        class(fluence_t), intent(in) :: lhs
        type(fluence_t), intent(in) :: rhs
        logical :: greater_than

        greater_than = lhs%particles_per_square_meter > rhs%particles_per_square_meter
    end function

    elemental function less_than(lhs, rhs)
        class(fluence_t), intent(in) :: lhs
        type(fluence_t), intent(in) :: rhs
        logical :: less_than

        less_than = lhs%particles_per_square_meter < rhs%particles_per_square_meter
    end function

    elemental function greater_than_or_equal(lhs, rhs)
        class(fluence_t), intent(in) :: lhs
        type(fluence_t), intent(in) :: rhs
        logical :: greater_than_or_equal

        greater_than_or_equal = lhs%particles_per_square_meter >= rhs%particles_per_square_meter
    end function

    elemental function less_than_or_equal(lhs, rhs)
        class(fluence_t), intent(in) :: lhs
        type(fluence_t), intent(in) :: rhs
        logical :: less_than_or_equal

        less_than_or_equal = lhs%particles_per_square_meter <= rhs%particles_per_square_meter
    end function

    elemental function equal_(lhs, rhs)
        class(fluence_t), intent(in) :: lhs
        type(fluence_t), intent(in) :: rhs
        logical :: equal_

        equal_ = lhs%particles_per_square_meter .safeEq. rhs%particles_per_square_meter
    end function

    elemental function equal_within_absolute_(lhs, rhs, within)
        class(fluence_t), intent(in) :: lhs
        type(fluence_t), intent(in) :: rhs
        type(fluence_t), intent(in) :: within
        logical :: equal_within_absolute_

        equal_within_absolute_ = equal_within_absolute( &
                lhs%particles_per_square_meter, rhs%particles_per_square_meter, within%particles_per_square_meter)
    end function

    elemental function equal_within_relative_(lhs, rhs, within)
        class(fluence_t), intent(in) :: lhs
        type(fluence_t), intent(in) :: rhs
        double precision, intent(in) :: within
        logical :: equal_within_relative_

        equal_within_relative_ = equal_within_relative( &
                lhs%particles_per_square_meter, rhs%particles_per_square_meter, within)
    end function

    elemental function not_equal(lhs, rhs)
        class(fluence_t), intent(in) :: lhs
        type(fluence_t), intent(in) :: rhs
        logical :: not_equal

        not_equal = .not. lhs == rhs
    end function

    elemental function to_string_full_precision(self) result(string)
        class(fluence_t), intent(in) :: self
        type(varying_string) :: string

        string = self%to_string_in(DEFAULT_OUTPUT_UNITS)
    end function

    elemental function to_string_with_precision(self, significant_digits) result(string)
        class(fluence_t), intent(in) :: self
        integer, intent(in) :: significant_digits
        type(varying_string) :: string

        string = self%to_string_in(DEFAULT_OUTPUT_UNITS, significant_digits)
    end function

    elemental function to_string_in_full_precision(self, units) result(string)
        class(fluence_t), intent(in) :: self
        class(fluence_unit_t), intent(in) :: units
        type(varying_string) :: string

        string = units%to_string(to_string(self.in.units))
    end function

    elemental function to_string_in_with_precision( &
            self, units, significant_digits) result(string)
        class(fluence_t), intent(in) :: self
        class(fluence_unit_t), intent(in) :: units
        integer, intent(in) :: significant_digits
        type(varying_string) :: string

        string = units%to_string(to_string(self.in.units, significant_digits))
    end function

    function fallible_fluence_from_fluence( &
            fluence) result(fallible_fluence)
        type(fluence_t), intent(in) :: fluence
        type(fallible_fluence_t) :: fallible_fluence

        fallible_fluence%fluence_ = fluence
    end function

    function fallible_fluence_from_errors( &
            errors) result(fallible_fluence)
        type(error_list_t), intent(in) :: errors
        type(fallible_fluence_t) :: fallible_fluence

        fallible_fluence%errors_ = errors
    end function

    function fallible_fluence_from_fallible_fluence( &
            fallible_fluence, &
            module_, &
            procedure_) &
            result(new_fallible_fluence)
        type(fallible_fluence_t), intent(in) :: fallible_fluence
        type(module_t), intent(in) :: module_
        type(procedure_t), intent(in) :: procedure_
        type(fallible_fluence_t) :: new_fallible_fluence

        if (fallible_fluence%failed()) then
            new_fallible_fluence%errors_ = error_list_t( &
                    fallible_fluence%errors_, module_, procedure_)
        else
            new_fallible_fluence%fluence_ = fallible_fluence%fluence_
        end if
    end function

    elemental function fallible_fluence_failed( &
            self) result(failed)
        class(fallible_fluence_t), intent(in) :: self
        logical :: failed

        failed = self%errors_%has_any()
    end function

    elemental function fallible_fluence_fluence( &
            self) result(fluence)
        class(fallible_fluence_t), intent(in) :: self
        type(fluence_t) :: fluence

        fluence = self%fluence_
    end function

    impure elemental function fallible_fluence_errors( &
            self) result(errors)
        class(fallible_fluence_t), intent(in) :: self
        type(error_list_t) :: errors

        errors = self%errors_
    end function

    function fallible_fluence_unit_from_unit( &
            unit) result(fallible_fluence_unit)
        class(fluence_unit_t), intent(in) :: unit
        type(fallible_fluence_unit_t) :: fallible_fluence_unit

        allocate(fallible_fluence_unit%unit_, source = unit)
    end function

    function fallible_fluence_unit_from_errors( &
            errors) result(fallible_fluence_unit)
        type(error_list_t), intent(in) :: errors
        type(fallible_fluence_unit_t) :: fallible_fluence_unit

        fallible_fluence_unit%errors_ = errors
    end function

    function fallible_fluence_unit_from_fallible_fluence_unit( &
            fallible_fluence_unit, &
            module_, &
            procedure_) &
            result(new_fallible_fluence_unit)
        type(fallible_fluence_unit_t), intent(in) :: fallible_fluence_unit
        type(module_t), intent(in) :: module_
        type(procedure_t), intent(in) :: procedure_
        type(fallible_fluence_unit_t) :: new_fallible_fluence_unit

        if (fallible_fluence_unit%failed()) then
            new_fallible_fluence_unit%errors_ = error_list_t( &
                    fallible_fluence_unit%errors_, module_, procedure_)
        else
            allocate(new_fallible_fluence_unit%unit_, source = &
                    fallible_fluence_unit%unit_)
        end if
    end function

    elemental function fallible_fluence_unit_failed( &
            self) result(failed)
        class(fallible_fluence_unit_t), intent(in) :: self
        logical :: failed

        failed = self%errors_%has_any()
    end function

    function fallible_fluence_unit_unit( &
            self) result(unit)
        class(fallible_fluence_unit_t), intent(in) :: self
        class(fluence_unit_t), allocatable :: unit

        allocate(unit, source = self%unit_)
    end function

    impure elemental function fallible_fluence_unit_errors( &
            self) result(errors)
        class(fallible_fluence_unit_t), intent(in) :: self
        type(error_list_t) :: errors

        errors = self%errors_
    end function

    elemental function simple_unit_to_string(self) result(string)
        class(fluence_simple_unit_t), intent(in) :: self
        type(varying_string) :: string

        string = trim(self%symbol)
    end function

    pure function simple_value_to_string(self, value_) result(string)
        class(fluence_simple_unit_t), intent(in) :: self
        type(varying_string), intent(in) :: value_
        type(varying_string) :: string

        string = value_ // " " // self%to_string()
    end function

    function simple_parse_as(self, string) result(fallible_fluence)
        class(fluence_simple_unit_t), intent(in) :: self
        type(varying_string), intent(in) :: string
        type(fallible_fluence_t) :: fallible_fluence

        type(parse_result_t) :: parse_result

        parse_result = parse_with(the_parser, string)
        if (parse_result%ok) then
            select type (the_number => parse_result%parsed)
            type is (parsed_rational_t)
                fallible_fluence = fallible_fluence_t(the_number%value_.unit.self)
            end select
        else
            fallible_fluence = fallible_fluence_t(error_list_t(fatal_t( &
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

    function parse_fluence_unit_c(string) result(fallible_fluence_unit)
        character(len=*), intent(in) :: string
        type(fallible_fluence_unit_t) :: fallible_fluence_unit

        fallible_fluence_unit = fallible_fluence_unit_t( &
                parse_fluence_unit(var_str(string), PROVIDED_UNITS), &
                module_t(MODULE_NAME), &
                procedure_t("parse_fluence_unit_c"))
    end function

    function parse_fluence_unit_s(string) result(fallible_fluence_unit)
        type(varying_string), intent(in) :: string
        type(fallible_fluence_unit_t) :: fallible_fluence_unit

        fallible_fluence_unit = fallible_fluence_unit_t( &
                parse_fluence_unit(string, PROVIDED_UNITS), &
                module_t(MODULE_NAME), &
                procedure_t("parse_fluence_unit_s"))
    end function

    function parse_fluence_unit_with_units_c( &
            string, units) result(fallible_fluence_unit)
        character(len=*), intent(in) :: string
        class(fluence_unit_t), intent(in) :: units(:)
        type(fallible_fluence_unit_t) :: fallible_fluence_unit

        fallible_fluence_unit = fallible_fluence_unit_t( &
                parse_fluence_unit(var_str(string), units), &
                module_t(MODULE_NAME), &
                procedure_t("parse_fluence_unit_with_units_c"))
    end function

    function parse_fluence_unit_with_units_s( &
            string, units) result(fallible_fluence_unit)
        type(varying_string), intent(in) :: string
        class(fluence_unit_t), intent(in) :: units(:)
        type(fallible_fluence_unit_t) :: fallible_fluence_unit

        integer :: i
        type(varying_string), allocatable :: unit_strings(:)

        do i = 1, size(units)
            if (string == units(i)%to_string()) then
                fallible_fluence_unit = fallible_fluence_unit_t(units(i))
                return
            end if
        end do
        allocate(unit_strings(size(units)))
        do i = 1, size(units)
            unit_strings(i) = units(i)%to_string()
        end do
        fallible_fluence_unit = fallible_fluence_unit_t(error_list_t(fatal_t( &
                UNKNOWN_UNIT, &
                module_t(MODULE_NAME), &
                procedure_t("parse_fluence_unit_with_units_s"), &
                '"' // string // '", known units: [' // join(unit_strings, ', ') // ']')))
    end function
end module
