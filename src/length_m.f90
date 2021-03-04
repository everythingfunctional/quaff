module length_m
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
            CENTIMETERS_PER_METER, &
            FEET_PER_METER, &
            INCHES_PER_METER, &
            MICROINCHES_PER_METER, &
            MICROMETERS_PER_METER
    use quaff_utilities_m, only: &
            operator(.safeEq.), &
            equal_within_absolute_ => equal_within_absolute, &
            equal_within_relative_ => equal_within_relative, &
            parse_space, &
            PARSE_ERROR, &
            UNKNOWN_UNIT
    use strff, only: join, to_string

    implicit none
    private
    public :: &
            length_t, &
            fallible_length_t, &
            length_unit_t, &
            fallible_length_unit_t, &
            length_simple_unit_t, &
            operator(.unit.), &
            parse_length, &
            parse_length_unit, &
            sum

    type :: length_t
        double precision :: meters
    contains
        private
        procedure :: to_units
        generic, public :: operator(.in.) => to_units
        procedure, pass(length) :: double_times_length
        procedure, pass(length) :: integer_times_length
        procedure :: length_times_double
        procedure :: length_times_integer
        generic, public :: operator(*) => &
                double_times_length, &
                integer_times_length, &
                length_times_double, &
                length_times_integer
        procedure :: length_divided_by_double
        procedure :: length_divided_by_integer
        procedure :: length_divided_by_length
        generic, public :: operator(/) => &
                length_divided_by_double, &
                length_divided_by_integer, &
                length_divided_by_length
        procedure :: length_plus_length
        generic, public :: operator(+) => length_plus_length
        procedure :: length_minus_length
        generic, public :: operator(-) => length_minus_length
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
        procedure :: equal_within_absolute
        procedure :: equal_within_relative
        generic, public :: equal => &
                equal_, equal_within_absolute, equal_within_relative
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

    type :: fallible_length_t
        private
        type(length_t) :: length_
        type(error_list_t) :: errors_
    contains
        private
        procedure, public :: failed => fallible_length_failed
        procedure, public :: length => fallible_length_length
        procedure, public :: errors => fallible_length_errors
    end type

    type, abstract :: length_unit_t
        double precision :: conversion_factor
    contains
        procedure(unit_to_string_i), deferred :: unit_to_string
        procedure(value_to_string_i), deferred :: value_to_string
        generic :: to_string => unit_to_string, value_to_string
        procedure(parse_as_i), deferred :: parse_as
    end type

    type :: fallible_length_unit_t
        private
        class(length_unit_t), allocatable :: unit_
        type(error_list_t) :: errors_
    contains
        private
        procedure, public :: failed => fallible_length_unit_failed
        procedure, public :: unit => fallible_length_unit_unit
        procedure, public :: errors => fallible_length_unit_errors
    end type

    type, extends(length_unit_t) :: length_simple_unit_t
        character(len=20) :: symbol
    contains
        procedure :: unit_to_string => simple_unit_to_string
        procedure :: value_to_string => simple_value_to_string
        procedure :: parse_as => simple_parse_as
    end type

    abstract interface
        elemental function unit_to_string_i(self) result(string)
            import :: length_unit_t, varying_string

            implicit none

            class(length_unit_t), intent(in) :: self
            type(varying_string) :: string
        end function

        pure function value_to_string_i(self, value_) result(string)
            import :: length_unit_t, varying_string

            implicit none

            class(length_unit_t), intent(in) :: self
            type(varying_string), intent(in) :: value_
            type(varying_string) :: string
        end function

        function parse_as_i(self, string) result(fallible_length)
            import :: length_unit_t, fallible_length_t, varying_string

            implicit none

            class(length_unit_t), intent(in) :: self
            type(varying_string), intent(in) :: string
            type(fallible_length_t) :: fallible_length
        end function
    end interface

    interface operator(.unit.)
        module procedure from_units
    end interface

    interface fallible_length_t
        module procedure fallible_length_from_length
        module procedure fallible_length_from_errors
        module procedure fallible_length_from_fallible_length
    end interface

    interface fallible_length_unit_t
        module procedure fallible_length_unit_from_unit
        module procedure fallible_length_unit_from_errors
        module procedure fallible_length_unit_from_fallible_length_unit
    end interface

    interface parse_length
        module procedure parse_length_c
        module procedure parse_length_s
        module procedure parse_length_with_units_c
        module procedure parse_length_with_units_s
    end interface

    interface parse_length_unit
        module procedure parse_length_unit_c
        module procedure parse_length_unit_s
        module procedure parse_length_unit_with_units_c
        module procedure parse_length_unit_with_units_s
    end interface

    interface sum
        module procedure sum_length
    end interface

    type(length_simple_unit_t), parameter, public :: CENTIMETERS = &
            length_simple_unit_t( &
                    conversion_factor = CENTIMETERS_PER_METER, &
                    symbol = "cm")
    type(length_simple_unit_t), parameter, public :: FEET = &
            length_simple_unit_t( &
                    conversion_factor = FEET_PER_METER, &
                    symbol = "ft")
    type(length_simple_unit_t), parameter, public :: INCHES = &
            length_simple_unit_t( &
                    conversion_factor = INCHES_PER_METER, &
                    symbol = "in")
    type(length_simple_unit_t), parameter, public :: METERS = &
            length_simple_unit_t( &
                    conversion_factor = 1.0d0, &
                    symbol = "m")
    type(length_simple_unit_t), parameter, public :: MICROINCHES = &
            length_simple_unit_t( &
                    conversion_factor = MICROINCHES_PER_METER, &
                    symbol = "uin")
    type(length_simple_unit_t), parameter, public :: MICROMETERS = &
            length_simple_unit_t( &
                    conversion_factor = MICROMETERS_PER_METER, &
                    symbol = "um")

    type(length_simple_unit_t), public :: DEFAULT_OUTPUT_UNITS = METERS

    type(length_simple_unit_t), parameter, public :: PROVIDED_UNITS(*) = &
            [CENTIMETERS, FEET, INCHES, METERS, MICROINCHES, MICROMETERS]

    character(len=*), parameter :: MODULE_NAME = "length_m"
contains
    function parse_length_c(string) result(fallible_length)
        character(len=*), intent(in) :: string
        type(fallible_length_t) :: fallible_length

        fallible_length = fallible_length_t( &
                parse_length(var_str(string), PROVIDED_UNITS), &
                module_t(MODULE_NAME), &
                procedure_t("parse_length_c"))
    end function

    function parse_length_s(string) result(fallible_length)
        type(varying_string), intent(in) :: string
        type(fallible_length_t) :: fallible_length

        fallible_length = fallible_length_t( &
                parse_length(string, PROVIDED_UNITS), &
                module_t(MODULE_NAME), &
                procedure_t("parse_length_s"))
    end function

    function parse_length_with_units_c( &
            string, units) result(fallible_length)
        character(len=*), intent(in) :: string
        class(length_unit_t), intent(in) :: units(:)
        type(fallible_length_t) :: fallible_length

        fallible_length = fallible_length_t( &
                parse_length(var_str(string), units), &
                module_t(MODULE_NAME), &
                procedure_t("parse_length_with_units_c"))
    end function

    function parse_length_with_units_s( &
            string, units) result(fallible_length)
        type(varying_string), intent(in) :: string
        class(length_unit_t), intent(in) :: units(:)
        type(fallible_length_t) :: fallible_length

        type(fallible_length_t) :: all_attempts(size(units))
        integer :: i

        do i = 1, size(units)
            all_attempts(i) = units(i)%parse_as(string)
            if (.not. all_attempts(i)%failed()) then
                fallible_length = all_attempts(i)
                return
            end if
        end do
        fallible_length = fallible_length_t(error_list_t( &
                all_attempts%errors(), &
                module_t(MODULE_NAME), &
                procedure_t("parse_length_with_units_s")))
    end function

    elemental function from_units(value_, units) result(length)
        double precision, intent(in) :: value_
        class(length_unit_t), intent(in) :: units
        type(length_t) :: length

        length%meters = value_ / units%conversion_factor
    end function

    elemental function to_units(self, units) result(length)
        class(length_t), intent(in) :: self
        class(length_unit_t), intent(in) :: units
        double precision :: length

        length = self%meters * units%conversion_factor
    end function

    elemental function double_times_length( &
            multiplier, length) result(new_length)
        double precision, intent(in) :: multiplier
        class(length_t), intent(in) :: length
        type(length_t) :: new_length

        new_length%meters = &
                multiplier * length%meters
    end function

    elemental function integer_times_length( &
            multiplier, length) result(new_length)
        integer, intent(in) :: multiplier
        class(length_t), intent(in) :: length
        type(length_t) :: new_length

        new_length%meters = &
                dble(multiplier) * length%meters
    end function

    elemental function length_times_double( &
            length, multiplier) result(new_length)
        class(length_t), intent(in) :: length
        double precision, intent(in) :: multiplier
        type(length_t) :: new_length

        new_length%meters = &
                length%meters * multiplier
    end function

    elemental function length_times_integer( &
            length, multiplier) result(new_length)
        class(length_t), intent(in) :: length
        integer, intent(in) :: multiplier
        type(length_t) :: new_length

        new_length%meters = &
                length%meters * dble(multiplier)
    end function

    elemental function length_divided_by_double( &
            length, divisor) result(new_length)
        class(length_t), intent(in) :: length
        double precision, intent(in) :: divisor
        type(length_t) :: new_length

        new_length%meters = &
                length%meters / divisor
    end function

    elemental function length_divided_by_integer( &
            length, divisor) result(new_length)
        class(length_t), intent(in) :: length
        integer, intent(in) :: divisor
        type(length_t) :: new_length

        new_length%meters = &
                length%meters / dble(divisor)
    end function

    elemental function length_divided_by_length( &
            numerator, denomenator) result(ratio)
        class(length_t), intent(in) :: numerator
        type(length_t), intent(in) :: denomenator
        double precision :: ratio

        ratio = numerator%meters / denomenator%meters
    end function

    elemental function length_plus_length( &
            lhs, rhs) result(sum_)
        class(length_t), intent(in) :: lhs
        type(length_t), intent(in) :: rhs
        type(length_t) :: sum_

        sum_%meters = lhs%meters + rhs%meters
    end function

    elemental function length_minus_length( &
            lhs, rhs) result(difference)
        class(length_t), intent(in) :: lhs
        type(length_t), intent(in) :: rhs
        type(length_t) :: difference

        difference%meters = lhs%meters - rhs%meters
    end function

    pure function sum_length(lengths) result(sum_)
        type(length_t), intent(in) :: lengths(:)
        type(length_t) :: sum_

        sum_%meters = sum(lengths%meters)
    end function

    elemental function greater_than(lhs, rhs)
        class(length_t), intent(in) :: lhs
        type(length_t), intent(in) :: rhs
        logical :: greater_than

        greater_than = lhs%meters > rhs%meters
    end function

    elemental function less_than(lhs, rhs)
        class(length_t), intent(in) :: lhs
        type(length_t), intent(in) :: rhs
        logical :: less_than

        less_than = lhs%meters < rhs%meters
    end function

    elemental function greater_than_or_equal(lhs, rhs)
        class(length_t), intent(in) :: lhs
        type(length_t), intent(in) :: rhs
        logical :: greater_than_or_equal

        greater_than_or_equal = lhs%meters >= rhs%meters
    end function

    elemental function less_than_or_equal(lhs, rhs)
        class(length_t), intent(in) :: lhs
        type(length_t), intent(in) :: rhs
        logical :: less_than_or_equal

        less_than_or_equal = lhs%meters <= rhs%meters
    end function

    elemental function equal_(lhs, rhs)
        class(length_t), intent(in) :: lhs
        type(length_t), intent(in) :: rhs
        logical :: equal_

        equal_ = lhs%meters .safeEq. rhs%meters
    end function

    elemental function equal_within_absolute(lhs, rhs, within)
        class(length_t), intent(in) :: lhs
        type(length_t), intent(in) :: rhs
        type(length_t), intent(in) :: within
        logical :: equal_within_absolute

        equal_within_absolute = equal_within_absolute_( &
                lhs%meters, rhs%meters, within%meters)
    end function

    elemental function equal_within_relative(lhs, rhs, within)
        class(length_t), intent(in) :: lhs
        type(length_t), intent(in) :: rhs
        double precision, intent(in) :: within
        logical :: equal_within_relative

        equal_within_relative = equal_within_relative_( &
                lhs%meters, rhs%meters, within)
    end function

    elemental function not_equal(lhs, rhs)
        class(length_t), intent(in) :: lhs
        type(length_t), intent(in) :: rhs
        logical :: not_equal

        not_equal = .not. lhs == rhs
    end function

    elemental function to_string_full_precision(self) result(string)
        class(length_t), intent(in) :: self
        type(varying_string) :: string

        string = self%to_string_in(DEFAULT_OUTPUT_UNITS)
    end function

    elemental function to_string_with_precision(self, significant_digits) result(string)
        class(length_t), intent(in) :: self
        integer, intent(in) :: significant_digits
        type(varying_string) :: string

        string = self%to_string_in(DEFAULT_OUTPUT_UNITS, significant_digits)
    end function

    elemental function to_string_in_full_precision(self, units) result(string)
        class(length_t), intent(in) :: self
        class(length_unit_t), intent(in) :: units
        type(varying_string) :: string

        string = units%to_string(to_string(self.in.units))
    end function

    elemental function to_string_in_with_precision( &
            self, units, significant_digits) result(string)
        class(length_t), intent(in) :: self
        class(length_unit_t), intent(in) :: units
        integer, intent(in) :: significant_digits
        type(varying_string) :: string

        string = units%to_string(to_string(self.in.units, significant_digits))
    end function

    function fallible_length_from_length( &
            length) result(fallible_length)
        type(length_t), intent(in) :: length
        type(fallible_length_t) :: fallible_length

        fallible_length%length_ = length
    end function

    function fallible_length_from_errors( &
            errors) result(fallible_length)
        type(error_list_t), intent(in) :: errors
        type(fallible_length_t) :: fallible_length

        fallible_length%errors_ = errors
    end function

    function fallible_length_from_fallible_length( &
            fallible_length, &
            module_, &
            procedure_) &
            result(new_fallible_length)
        type(fallible_length_t), intent(in) :: fallible_length
        type(module_t), intent(in) :: module_
        type(procedure_t), intent(in) :: procedure_
        type(fallible_length_t) :: new_fallible_length

        if (fallible_length%failed()) then
            new_fallible_length%errors_ = error_list_t( &
                    fallible_length%errors_, module_, procedure_)
        else
            new_fallible_length%length_ = fallible_length%length_
        end if
    end function

    elemental function fallible_length_failed( &
            self) result(failed)
        class(fallible_length_t), intent(in) :: self
        logical :: failed

        failed = self%errors_%has_any()
    end function

    elemental function fallible_length_length( &
            self) result(length)
        class(fallible_length_t), intent(in) :: self
        type(length_t) :: length

        length = self%length_
    end function

    impure elemental function fallible_length_errors( &
            self) result(errors)
        class(fallible_length_t), intent(in) :: self
        type(error_list_t) :: errors

        errors = self%errors_
    end function

    function fallible_length_unit_from_unit( &
            unit) result(fallible_length_unit)
        class(length_unit_t), intent(in) :: unit
        type(fallible_length_unit_t) :: fallible_length_unit

        allocate(fallible_length_unit%unit_, source = unit)
    end function

    function fallible_length_unit_from_errors( &
            errors) result(fallible_length_unit)
        type(error_list_t), intent(in) :: errors
        type(fallible_length_unit_t) :: fallible_length_unit

        fallible_length_unit%errors_ = errors
    end function

    function fallible_length_unit_from_fallible_length_unit( &
            fallible_length_unit, &
            module_, &
            procedure_) &
            result(new_fallible_length_unit)
        type(fallible_length_unit_t), intent(in) :: fallible_length_unit
        type(module_t), intent(in) :: module_
        type(procedure_t), intent(in) :: procedure_
        type(fallible_length_unit_t) :: new_fallible_length_unit

        if (fallible_length_unit%failed()) then
            new_fallible_length_unit%errors_ = error_list_t( &
                    fallible_length_unit%errors_, module_, procedure_)
        else
            allocate(new_fallible_length_unit%unit_, source = &
                    fallible_length_unit%unit_)
        end if
    end function

    elemental function fallible_length_unit_failed( &
            self) result(failed)
        class(fallible_length_unit_t), intent(in) :: self
        logical :: failed

        failed = self%errors_%has_any()
    end function

    function fallible_length_unit_unit( &
            self) result(unit)
        class(fallible_length_unit_t), intent(in) :: self
        class(length_unit_t), allocatable :: unit

        allocate(unit, source = self%unit_)
    end function

    impure elemental function fallible_length_unit_errors( &
            self) result(errors)
        class(fallible_length_unit_t), intent(in) :: self
        type(error_list_t) :: errors

        errors = self%errors_
    end function

    elemental function simple_unit_to_string(self) result(string)
        class(length_simple_unit_t), intent(in) :: self
        type(varying_string) :: string

        string = trim(self%symbol)
    end function

    pure function simple_value_to_string(self, value_) result(string)
        class(length_simple_unit_t), intent(in) :: self
        type(varying_string), intent(in) :: value_
        type(varying_string) :: string

        string = value_ // " " // self%to_string()
    end function

    function simple_parse_as(self, string) result(fallible_length)
        class(length_simple_unit_t), intent(in) :: self
        type(varying_string), intent(in) :: string
        type(fallible_length_t) :: fallible_length

        type(parse_result_t) :: parse_result

        parse_result = parse_with(the_parser, string)
        if (parse_result%ok()) then
            select type (the_number => parse_result%parsed())
            type is (parsed_rational_t)
                fallible_length = fallible_length_t(the_number%value_().unit.self)
            end select
        else
            fallible_length = fallible_length_t(error_list_t(fatal_t( &
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

    function parse_length_unit_c(string) result(fallible_length_unit)
        character(len=*), intent(in) :: string
        type(fallible_length_unit_t) :: fallible_length_unit

        fallible_length_unit = fallible_length_unit_t( &
                parse_length_unit(var_str(string), PROVIDED_UNITS), &
                module_t(MODULE_NAME), &
                procedure_t("parse_length_unit_c"))
    end function

    function parse_length_unit_s(string) result(fallible_length_unit)
        type(varying_string), intent(in) :: string
        type(fallible_length_unit_t) :: fallible_length_unit

        fallible_length_unit = fallible_length_unit_t( &
                parse_length_unit(string, PROVIDED_UNITS), &
                module_t(MODULE_NAME), &
                procedure_t("parse_length_unit_s"))
    end function

    function parse_length_unit_with_units_c( &
            string, units) result(fallible_length_unit)
        character(len=*), intent(in) :: string
        class(length_unit_t), intent(in) :: units(:)
        type(fallible_length_unit_t) :: fallible_length_unit

        fallible_length_unit = fallible_length_unit_t( &
                parse_length_unit(var_str(string), units), &
                module_t(MODULE_NAME), &
                procedure_t("parse_length_unit_with_units_c"))
    end function

    function parse_length_unit_with_units_s( &
            string, units) result(fallible_length_unit)
        type(varying_string), intent(in) :: string
        class(length_unit_t), intent(in) :: units(:)
        type(fallible_length_unit_t) :: fallible_length_unit

        integer :: i
        type(varying_string) :: unit_strings(size(units))

        do i = 1, size(units)
            if (string == units(i)%to_string()) then
                fallible_length_unit = fallible_length_unit_t(units(i))
                return
            end if
        end do
        do i = 1, size(units)
            unit_strings(i) = units(i)%to_string()
        end do
        fallible_length_unit = fallible_length_unit_t(error_list_t(fatal_t( &
                UNKNOWN_UNIT, &
                module_t(MODULE_NAME), &
                procedure_t("parse_length_unit_with_units_s"), &
                '"' // string // '", known units: [' // join(unit_strings, ', ') // ']')))
    end function
end module
