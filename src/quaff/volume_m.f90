module quaff_volume_m
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
            CUBIC_CENTIMETERS_PER_CUBIC_METER, &
            CUBIC_MILLIMETERS_PER_CUBIC_METER, &
            LITERS_PER_CUBIC_METER
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
            volume_t, &
            fallible_volume_t, &
            volume_unit_t, &
            fallible_volume_unit_t, &
            volume_simple_unit_t, &
            operator(.unit.), &
            parse_volume, &
            parse_volume_unit, &
            sum, &
            abs, &
            DEFAULT_OUTPUT_UNITS, &
            PROVIDED_UNITS, &
            CUBIC_CENTIMETERS, &
            CUBIC_METERS, &
            CUBIC_MILLIMETERS, &
            LITERS

    type :: volume_t
        double precision :: cubic_meters
    contains
        private
        procedure :: to_units
        generic, public :: operator(.in.) => to_units
        procedure, pass(volume) :: double_times_volume
        procedure, pass(volume) :: integer_times_volume
        procedure :: volume_times_double
        procedure :: volume_times_integer
        generic, public :: operator(*) => &
                double_times_volume, &
                integer_times_volume, &
                volume_times_double, &
                volume_times_integer
        procedure :: volume_divided_by_double
        procedure :: volume_divided_by_integer
        procedure :: volume_divided_by_volume
        generic, public :: operator(/) => &
                volume_divided_by_double, &
                volume_divided_by_integer, &
                volume_divided_by_volume
        procedure :: volume_plus_volume
        generic, public :: operator(+) => volume_plus_volume
        procedure :: negate_volume
        procedure :: volume_minus_volume
        generic, public :: operator(-) => &
                negate_volume, &
                volume_minus_volume
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

    type :: fallible_volume_t
        private
        type(volume_t) :: volume_ = volume_t(0.0d0)
        type(error_list_t) :: errors_
    contains
        private
        procedure, public :: failed => fallible_volume_failed
        procedure, public :: volume => fallible_volume_volume
        procedure, public :: errors => fallible_volume_errors
    end type

    type, abstract :: volume_unit_t
        double precision :: conversion_factor
    contains
        procedure(unit_to_string_i), deferred :: unit_to_string
        procedure(value_to_string_i), deferred :: value_to_string
        generic :: to_string => unit_to_string, value_to_string
        procedure(parse_as_i), deferred :: parse_as
    end type

    type :: fallible_volume_unit_t
        private
        class(volume_unit_t), allocatable :: unit_
        type(error_list_t) :: errors_
    contains
        private
        procedure, public :: failed => fallible_volume_unit_failed
        procedure, public :: unit => fallible_volume_unit_unit
        procedure, public :: errors => fallible_volume_unit_errors
    end type

    type, extends(volume_unit_t) :: volume_simple_unit_t
        character(len=20) :: symbol
    contains
        procedure :: unit_to_string => simple_unit_to_string
        procedure :: value_to_string => simple_value_to_string
        procedure :: parse_as => simple_parse_as
    end type

    abstract interface
        elemental function unit_to_string_i(self) result(string)
            import :: volume_unit_t, varying_string

            implicit none

            class(volume_unit_t), intent(in) :: self
            type(varying_string) :: string
        end function

        pure function value_to_string_i(self, value_) result(string)
            import :: volume_unit_t, varying_string

            implicit none

            class(volume_unit_t), intent(in) :: self
            type(varying_string), intent(in) :: value_
            type(varying_string) :: string
        end function

        function parse_as_i(self, string) result(fallible_volume)
            import :: volume_unit_t, fallible_volume_t, varying_string

            implicit none

            class(volume_unit_t), intent(in) :: self
            type(varying_string), intent(in) :: string
            type(fallible_volume_t) :: fallible_volume
        end function
    end interface

    interface operator(.unit.)
        module procedure from_units
    end interface

    interface fallible_volume_t
        module procedure fallible_volume_from_volume
        module procedure fallible_volume_from_errors
        module procedure fallible_volume_from_fallible_volume
    end interface

    interface fallible_volume_unit_t
        module procedure fallible_volume_unit_from_unit
        module procedure fallible_volume_unit_from_errors
        module procedure fallible_volume_unit_from_fallible_volume_unit
    end interface

    interface parse_volume
        module procedure parse_volume_c
        module procedure parse_volume_s
        module procedure parse_volume_with_units_c
        module procedure parse_volume_with_units_s
    end interface

    interface parse_volume_unit
        module procedure parse_volume_unit_c
        module procedure parse_volume_unit_s
        module procedure parse_volume_unit_with_units_c
        module procedure parse_volume_unit_with_units_s
    end interface

    interface abs
        module procedure abs_volume
    end interface

    interface sum
        module procedure sum_volume
    end interface

    type(volume_simple_unit_t), parameter :: CUBIC_CENTIMETERS = &
            volume_simple_unit_t( &
                    conversion_factor = CUBIC_CENTIMETERS_PER_CUBIC_METER, &
                    symbol = "cm^3")
    type(volume_simple_unit_t), parameter :: CUBIC_METERS = &
            volume_simple_unit_t( &
                    conversion_factor = 1.0d0, &
                    symbol = "m^3")
    type(volume_simple_unit_t), parameter :: CUBIC_MILLIMETERS = &
            volume_simple_unit_t( &
                    conversion_factor = CUBIC_MILLIMETERS_PER_CUBIC_METER, &
                    symbol = "mm^3")
    type(volume_simple_unit_t), parameter :: LITERS = &
            volume_simple_unit_t( &
                    conversion_factor = LITERS_PER_CUBIC_METER, &
                    symbol = "L")

    type(volume_simple_unit_t) :: DEFAULT_OUTPUT_UNITS = CUBIC_METERS

    type(volume_simple_unit_t), parameter :: PROVIDED_UNITS(*) = &
            [CUBIC_CENTIMETERS, CUBIC_METERS, CUBIC_MILLIMETERS, LITERS]

    character(len=*), parameter :: MODULE_NAME = "quaff_volume_m"
contains
    function parse_volume_c(string) result(fallible_volume)
        character(len=*), intent(in) :: string
        type(fallible_volume_t) :: fallible_volume

        fallible_volume = fallible_volume_t( &
                parse_volume(var_str(string), PROVIDED_UNITS), &
                module_t(MODULE_NAME), &
                procedure_t("parse_volume_c"))
    end function

    function parse_volume_s(string) result(fallible_volume)
        type(varying_string), intent(in) :: string
        type(fallible_volume_t) :: fallible_volume

        fallible_volume = fallible_volume_t( &
                parse_volume(string, PROVIDED_UNITS), &
                module_t(MODULE_NAME), &
                procedure_t("parse_volume_s"))
    end function

    function parse_volume_with_units_c( &
            string, units) result(fallible_volume)
        character(len=*), intent(in) :: string
        class(volume_unit_t), intent(in) :: units(:)
        type(fallible_volume_t) :: fallible_volume

        fallible_volume = fallible_volume_t( &
                parse_volume(var_str(string), units), &
                module_t(MODULE_NAME), &
                procedure_t("parse_volume_with_units_c"))
    end function

    function parse_volume_with_units_s( &
            string, units) result(fallible_volume)
        type(varying_string), intent(in) :: string
        class(volume_unit_t), intent(in) :: units(:)
        type(fallible_volume_t) :: fallible_volume

        integer :: i

        do i = 1, size(units)
            fallible_volume = units(i)%parse_as(string)
            if (.not. fallible_volume%failed()) return
        end do
        fallible_volume = fallible_volume_t(error_list_t(fatal_t( &
                PARSE_ERROR, &
                module_t(MODULE_NAME), &
                procedure_t("parse_volume_with_units_s"), &
                "Unable to parse '" // string // "' as a volume_t. Tried with units: " &
                // join(units%to_string(), ", "))))
    end function

    elemental function from_units(value_, units) result(volume)
        double precision, intent(in) :: value_
        class(volume_unit_t), intent(in) :: units
        type(volume_t) :: volume

        volume%cubic_meters = value_ / units%conversion_factor
    end function

    elemental function to_units(self, units) result(volume)
        class(volume_t), intent(in) :: self
        class(volume_unit_t), intent(in) :: units
        double precision :: volume

        volume = self%cubic_meters * units%conversion_factor
    end function

    elemental function double_times_volume( &
            multiplier, volume) result(new_volume)
        double precision, intent(in) :: multiplier
        class(volume_t), intent(in) :: volume
        type(volume_t) :: new_volume

        new_volume%cubic_meters = &
                multiplier * volume%cubic_meters
    end function

    elemental function integer_times_volume( &
            multiplier, volume) result(new_volume)
        integer, intent(in) :: multiplier
        class(volume_t), intent(in) :: volume
        type(volume_t) :: new_volume

        new_volume%cubic_meters = &
                dble(multiplier) * volume%cubic_meters
    end function

    elemental function volume_times_double( &
            volume, multiplier) result(new_volume)
        class(volume_t), intent(in) :: volume
        double precision, intent(in) :: multiplier
        type(volume_t) :: new_volume

        new_volume%cubic_meters = &
                volume%cubic_meters * multiplier
    end function

    elemental function volume_times_integer( &
            volume, multiplier) result(new_volume)
        class(volume_t), intent(in) :: volume
        integer, intent(in) :: multiplier
        type(volume_t) :: new_volume

        new_volume%cubic_meters = &
                volume%cubic_meters * dble(multiplier)
    end function

    elemental function volume_divided_by_double( &
            volume, divisor) result(new_volume)
        class(volume_t), intent(in) :: volume
        double precision, intent(in) :: divisor
        type(volume_t) :: new_volume

        new_volume%cubic_meters = &
                volume%cubic_meters / divisor
    end function

    elemental function volume_divided_by_integer( &
            volume, divisor) result(new_volume)
        class(volume_t), intent(in) :: volume
        integer, intent(in) :: divisor
        type(volume_t) :: new_volume

        new_volume%cubic_meters = &
                volume%cubic_meters / dble(divisor)
    end function

    elemental function volume_divided_by_volume( &
            numerator, denomenator) result(ratio)
        class(volume_t), intent(in) :: numerator
        type(volume_t), intent(in) :: denomenator
        double precision :: ratio

        ratio = numerator%cubic_meters / denomenator%cubic_meters
    end function

    elemental function volume_plus_volume( &
            lhs, rhs) result(sum_)
        class(volume_t), intent(in) :: lhs
        type(volume_t), intent(in) :: rhs
        type(volume_t) :: sum_

        sum_%cubic_meters = lhs%cubic_meters + rhs%cubic_meters
    end function

    elemental function negate_volume(self) result(negated)
        class(volume_t), intent(in) :: self
        type(volume_t) :: negated

        negated%cubic_meters = -self%cubic_meters
    end function

    elemental function volume_minus_volume( &
            lhs, rhs) result(difference)
        class(volume_t), intent(in) :: lhs
        type(volume_t), intent(in) :: rhs
        type(volume_t) :: difference

        difference%cubic_meters = lhs%cubic_meters - rhs%cubic_meters
    end function

    pure function abs_volume(volume) result(abs_)
        type(volume_t), intent(in) :: volume
        type(volume_t) :: abs_

        abs_%cubic_meters = abs(volume%cubic_meters)
    end function

    pure function sum_volume(volumes) result(sum_)
        type(volume_t), intent(in) :: volumes(:)
        type(volume_t) :: sum_

        sum_%cubic_meters = sum(volumes%cubic_meters)
    end function

    elemental function greater_than(lhs, rhs)
        class(volume_t), intent(in) :: lhs
        type(volume_t), intent(in) :: rhs
        logical :: greater_than

        greater_than = lhs%cubic_meters > rhs%cubic_meters
    end function

    elemental function less_than(lhs, rhs)
        class(volume_t), intent(in) :: lhs
        type(volume_t), intent(in) :: rhs
        logical :: less_than

        less_than = lhs%cubic_meters < rhs%cubic_meters
    end function

    elemental function greater_than_or_equal(lhs, rhs)
        class(volume_t), intent(in) :: lhs
        type(volume_t), intent(in) :: rhs
        logical :: greater_than_or_equal

        greater_than_or_equal = lhs%cubic_meters >= rhs%cubic_meters
    end function

    elemental function less_than_or_equal(lhs, rhs)
        class(volume_t), intent(in) :: lhs
        type(volume_t), intent(in) :: rhs
        logical :: less_than_or_equal

        less_than_or_equal = lhs%cubic_meters <= rhs%cubic_meters
    end function

    elemental function equal_(lhs, rhs)
        class(volume_t), intent(in) :: lhs
        type(volume_t), intent(in) :: rhs
        logical :: equal_

        equal_ = lhs%cubic_meters .safeEq. rhs%cubic_meters
    end function

    elemental function equal_within_absolute_(lhs, rhs, within)
        class(volume_t), intent(in) :: lhs
        type(volume_t), intent(in) :: rhs
        type(volume_t), intent(in) :: within
        logical :: equal_within_absolute_

        equal_within_absolute_ = equal_within_absolute( &
                lhs%cubic_meters, rhs%cubic_meters, within%cubic_meters)
    end function

    elemental function equal_within_relative_(lhs, rhs, within)
        class(volume_t), intent(in) :: lhs
        type(volume_t), intent(in) :: rhs
        double precision, intent(in) :: within
        logical :: equal_within_relative_

        equal_within_relative_ = equal_within_relative( &
                lhs%cubic_meters, rhs%cubic_meters, within)
    end function

    elemental function not_equal(lhs, rhs)
        class(volume_t), intent(in) :: lhs
        type(volume_t), intent(in) :: rhs
        logical :: not_equal

        not_equal = .not. lhs == rhs
    end function

    elemental function to_string_full_precision(self) result(string)
        class(volume_t), intent(in) :: self
        type(varying_string) :: string

        string = self%to_string_in(DEFAULT_OUTPUT_UNITS)
    end function

    elemental function to_string_with_precision(self, significant_digits) result(string)
        class(volume_t), intent(in) :: self
        integer, intent(in) :: significant_digits
        type(varying_string) :: string

        string = self%to_string_in(DEFAULT_OUTPUT_UNITS, significant_digits)
    end function

    elemental function to_string_in_full_precision(self, units) result(string)
        class(volume_t), intent(in) :: self
        class(volume_unit_t), intent(in) :: units
        type(varying_string) :: string

        string = units%to_string(to_string(self.in.units))
    end function

    elemental function to_string_in_with_precision( &
            self, units, significant_digits) result(string)
        class(volume_t), intent(in) :: self
        class(volume_unit_t), intent(in) :: units
        integer, intent(in) :: significant_digits
        type(varying_string) :: string

        string = units%to_string(to_string(self.in.units, significant_digits))
    end function

    function fallible_volume_from_volume( &
            volume) result(fallible_volume)
        type(volume_t), intent(in) :: volume
        type(fallible_volume_t) :: fallible_volume

        fallible_volume%volume_ = volume
    end function

    function fallible_volume_from_errors( &
            errors) result(fallible_volume)
        type(error_list_t), intent(in) :: errors
        type(fallible_volume_t) :: fallible_volume

        fallible_volume%errors_ = errors
    end function

    function fallible_volume_from_fallible_volume( &
            fallible_volume, &
            module_, &
            procedure_) &
            result(new_fallible_volume)
        type(fallible_volume_t), intent(in) :: fallible_volume
        type(module_t), intent(in) :: module_
        type(procedure_t), intent(in) :: procedure_
        type(fallible_volume_t) :: new_fallible_volume

        if (fallible_volume%failed()) then
            new_fallible_volume%errors_ = error_list_t( &
                    fallible_volume%errors_, module_, procedure_)
        else
            new_fallible_volume%volume_ = fallible_volume%volume_
        end if
    end function

    elemental function fallible_volume_failed( &
            self) result(failed)
        class(fallible_volume_t), intent(in) :: self
        logical :: failed

        failed = self%errors_%has_any()
    end function

    elemental function fallible_volume_volume( &
            self) result(volume)
        class(fallible_volume_t), intent(in) :: self
        type(volume_t) :: volume

        volume = self%volume_
    end function

    impure elemental function fallible_volume_errors( &
            self) result(errors)
        class(fallible_volume_t), intent(in) :: self
        type(error_list_t) :: errors

        errors = self%errors_
    end function

    function fallible_volume_unit_from_unit( &
            unit) result(fallible_volume_unit)
        class(volume_unit_t), intent(in) :: unit
        type(fallible_volume_unit_t) :: fallible_volume_unit

        allocate(fallible_volume_unit%unit_, source = unit)
    end function

    function fallible_volume_unit_from_errors( &
            errors) result(fallible_volume_unit)
        type(error_list_t), intent(in) :: errors
        type(fallible_volume_unit_t) :: fallible_volume_unit

        fallible_volume_unit%errors_ = errors
    end function

    function fallible_volume_unit_from_fallible_volume_unit( &
            fallible_volume_unit, &
            module_, &
            procedure_) &
            result(new_fallible_volume_unit)
        type(fallible_volume_unit_t), intent(in) :: fallible_volume_unit
        type(module_t), intent(in) :: module_
        type(procedure_t), intent(in) :: procedure_
        type(fallible_volume_unit_t) :: new_fallible_volume_unit

        if (fallible_volume_unit%failed()) then
            new_fallible_volume_unit%errors_ = error_list_t( &
                    fallible_volume_unit%errors_, module_, procedure_)
        else
            allocate(new_fallible_volume_unit%unit_, source = &
                    fallible_volume_unit%unit_)
        end if
    end function

    elemental function fallible_volume_unit_failed( &
            self) result(failed)
        class(fallible_volume_unit_t), intent(in) :: self
        logical :: failed

        failed = self%errors_%has_any()
    end function

    function fallible_volume_unit_unit( &
            self) result(unit)
        class(fallible_volume_unit_t), intent(in) :: self
        class(volume_unit_t), allocatable :: unit

        allocate(unit, source = self%unit_)
    end function

    impure elemental function fallible_volume_unit_errors( &
            self) result(errors)
        class(fallible_volume_unit_t), intent(in) :: self
        type(error_list_t) :: errors

        errors = self%errors_
    end function

    elemental function simple_unit_to_string(self) result(string)
        class(volume_simple_unit_t), intent(in) :: self
        type(varying_string) :: string

        string = trim(self%symbol)
    end function

    pure function simple_value_to_string(self, value_) result(string)
        class(volume_simple_unit_t), intent(in) :: self
        type(varying_string), intent(in) :: value_
        type(varying_string) :: string

        string = value_ // " " // self%to_string()
    end function

    function simple_parse_as(self, string) result(fallible_volume)
        class(volume_simple_unit_t), intent(in) :: self
        type(varying_string), intent(in) :: string
        type(fallible_volume_t) :: fallible_volume

        type(parse_result_t) :: parse_result

        parse_result = parse_with(the_parser, string)
        if (parse_result%ok) then
            select type (the_number => parse_result%parsed)
            type is (parsed_rational_t)
                fallible_volume = fallible_volume_t(the_number%value_.unit.self)
            end select
        else
            fallible_volume = fallible_volume_t(error_list_t(fatal_t( &
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

    function parse_volume_unit_c(string) result(fallible_volume_unit)
        character(len=*), intent(in) :: string
        type(fallible_volume_unit_t) :: fallible_volume_unit

        fallible_volume_unit = fallible_volume_unit_t( &
                parse_volume_unit(var_str(string), PROVIDED_UNITS), &
                module_t(MODULE_NAME), &
                procedure_t("parse_volume_unit_c"))
    end function

    function parse_volume_unit_s(string) result(fallible_volume_unit)
        type(varying_string), intent(in) :: string
        type(fallible_volume_unit_t) :: fallible_volume_unit

        fallible_volume_unit = fallible_volume_unit_t( &
                parse_volume_unit(string, PROVIDED_UNITS), &
                module_t(MODULE_NAME), &
                procedure_t("parse_volume_unit_s"))
    end function

    function parse_volume_unit_with_units_c( &
            string, units) result(fallible_volume_unit)
        character(len=*), intent(in) :: string
        class(volume_unit_t), intent(in) :: units(:)
        type(fallible_volume_unit_t) :: fallible_volume_unit

        fallible_volume_unit = fallible_volume_unit_t( &
                parse_volume_unit(var_str(string), units), &
                module_t(MODULE_NAME), &
                procedure_t("parse_volume_unit_with_units_c"))
    end function

    function parse_volume_unit_with_units_s( &
            string, units) result(fallible_volume_unit)
        type(varying_string), intent(in) :: string
        class(volume_unit_t), intent(in) :: units(:)
        type(fallible_volume_unit_t) :: fallible_volume_unit

        integer :: i
        type(varying_string), allocatable :: unit_strings(:)

        do i = 1, size(units)
            if (string == units(i)%to_string()) then
                fallible_volume_unit = fallible_volume_unit_t(units(i))
                return
            end if
        end do
        allocate(unit_strings(size(units)))
        do i = 1, size(units)
            unit_strings(i) = units(i)%to_string()
        end do
        fallible_volume_unit = fallible_volume_unit_t(error_list_t(fatal_t( &
                UNKNOWN_UNIT, &
                module_t(MODULE_NAME), &
                procedure_t("parse_volume_unit_with_units_s"), &
                '"' // string // '", known units: [' // join(unit_strings, ', ') // ']')))
    end function
end module
