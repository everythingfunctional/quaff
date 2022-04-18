module area_m
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
            parse_rational, &
            parse_string, &
            parse_with, &
            then_drop
    use quaff_conversion_factors_m, only: &
            SQUARE_CENTIMETERS_PER_SQUARE_METER, &
            SQUARE_FEET_PER_SQUARE_METER, &
            SQUARE_INCHES_PER_SQUARE_METER
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
            area_t, &
            fallible_area_t, &
            area_unit_t, &
            fallible_area_unit_t, &
            area_simple_unit_t, &
            operator(.unit.), &
            parse_area, &
            parse_area_unit, &
            sum, &
            DEFAULT_OUTPUT_UNITS, &
            PROVIDED_UNITS, &
            SQUARE_CENTIMETERS, &
            SQUARE_FEET, &
            SQUARE_INCHES, &
            SQUARE_METERS

    type :: area_t
        double precision :: square_meters
    contains
        private
        procedure :: to_units
        generic, public :: operator(.in.) => to_units
        procedure, pass(area) :: double_times_area
        procedure, pass(area) :: integer_times_area
        procedure :: area_times_double
        procedure :: area_times_integer
        generic, public :: operator(*) => &
                double_times_area, &
                integer_times_area, &
                area_times_double, &
                area_times_integer
        procedure :: area_divided_by_double
        procedure :: area_divided_by_integer
        procedure :: area_divided_by_area
        generic, public :: operator(/) => &
                area_divided_by_double, &
                area_divided_by_integer, &
                area_divided_by_area
        procedure :: area_plus_area
        generic, public :: operator(+) => area_plus_area
        procedure :: area_minus_area
        generic, public :: operator(-) => area_minus_area
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

    type :: fallible_area_t
        private
        type(area_t) :: area_ = area_t(0.0d0)
        type(error_list_t) :: errors_
    contains
        private
        procedure, public :: failed => fallible_area_failed
        procedure, public :: area => fallible_area_area
        procedure, public :: errors => fallible_area_errors
    end type

    type, abstract :: area_unit_t
        double precision :: conversion_factor
    contains
        procedure(unit_to_string_i), deferred :: unit_to_string
        procedure(value_to_string_i), deferred :: value_to_string
        generic :: to_string => unit_to_string, value_to_string
        procedure(parse_as_i), deferred :: parse_as
    end type

    type :: fallible_area_unit_t
        private
        class(area_unit_t), allocatable :: unit_
        type(error_list_t) :: errors_
    contains
        private
        procedure, public :: failed => fallible_area_unit_failed
        procedure, public :: unit => fallible_area_unit_unit
        procedure, public :: errors => fallible_area_unit_errors
    end type

    type, extends(area_unit_t) :: area_simple_unit_t
        character(len=20) :: symbol
    contains
        procedure :: unit_to_string => simple_unit_to_string
        procedure :: value_to_string => simple_value_to_string
        procedure :: parse_as => simple_parse_as
    end type

    abstract interface
        elemental function unit_to_string_i(self) result(string)
            import :: area_unit_t, varying_string

            implicit none

            class(area_unit_t), intent(in) :: self
            type(varying_string) :: string
        end function

        pure function value_to_string_i(self, value_) result(string)
            import :: area_unit_t, varying_string

            implicit none

            class(area_unit_t), intent(in) :: self
            type(varying_string), intent(in) :: value_
            type(varying_string) :: string
        end function

        function parse_as_i(self, string) result(fallible_area)
            import :: area_unit_t, fallible_area_t, varying_string

            implicit none

            class(area_unit_t), intent(in) :: self
            type(varying_string), intent(in) :: string
            type(fallible_area_t) :: fallible_area
        end function
    end interface

    interface operator(.unit.)
        module procedure from_units
    end interface

    interface fallible_area_t
        module procedure fallible_area_from_area
        module procedure fallible_area_from_errors
        module procedure fallible_area_from_fallible_area
    end interface

    interface fallible_area_unit_t
        module procedure fallible_area_unit_from_unit
        module procedure fallible_area_unit_from_errors
        module procedure fallible_area_unit_from_fallible_area_unit
    end interface

    interface parse_area
        module procedure parse_area_c
        module procedure parse_area_s
        module procedure parse_area_with_units_c
        module procedure parse_area_with_units_s
    end interface

    interface parse_area_unit
        module procedure parse_area_unit_c
        module procedure parse_area_unit_s
        module procedure parse_area_unit_with_units_c
        module procedure parse_area_unit_with_units_s
    end interface

    interface sum
        module procedure sum_area
    end interface

    type(area_simple_unit_t), parameter :: SQUARE_CENTIMETERS = &
            area_simple_unit_t( &
                    conversion_factor = SQUARE_CENTIMETERS_PER_SQUARE_METER, &
                    symbol = "cm^2")
    type(area_simple_unit_t), parameter :: SQUARE_FEET = &
            area_simple_unit_t( &
                    conversion_factor = SQUARE_FEET_PER_SQUARE_METER, &
                    symbol = "ft^2")
    type(area_simple_unit_t), parameter :: SQUARE_INCHES = &
            area_simple_unit_t( &
                    conversion_factor = SQUARE_INCHES_PER_SQUARE_METER, &
                    symbol = "in^2")
    type(area_simple_unit_t), parameter :: SQUARE_METERS = &
            area_simple_unit_t( &
                    conversion_factor = 1.0d0, &
                    symbol = "m^2")

    type(area_simple_unit_t) :: DEFAULT_OUTPUT_UNITS = SQUARE_METERS

    type(area_simple_unit_t), parameter :: PROVIDED_UNITS(*) = &
            [ SQUARE_CENTIMETERS &
            , SQUARE_FEET &
            , SQUARE_INCHES &
            , SQUARE_METERS &
            ]

    character(len=*), parameter :: MODULE_NAME = "area_m"
contains
    function parse_area_c(string) result(fallible_area)
        character(len=*), intent(in) :: string
        type(fallible_area_t) :: fallible_area

        fallible_area = fallible_area_t( &
                parse_area(var_str(string), PROVIDED_UNITS), &
                module_t(MODULE_NAME), &
                procedure_t("parse_area_c"))
    end function

    function parse_area_s(string) result(fallible_area)
        type(varying_string), intent(in) :: string
        type(fallible_area_t) :: fallible_area

        fallible_area = fallible_area_t( &
                parse_area(string, PROVIDED_UNITS), &
                module_t(MODULE_NAME), &
                procedure_t("parse_area_s"))
    end function

    function parse_area_with_units_c( &
            string, units) result(fallible_area)
        character(len=*), intent(in) :: string
        class(area_unit_t), intent(in) :: units(:)
        type(fallible_area_t) :: fallible_area

        fallible_area = fallible_area_t( &
                parse_area(var_str(string), units), &
                module_t(MODULE_NAME), &
                procedure_t("parse_area_with_units_c"))
    end function

    function parse_area_with_units_s( &
            string, units) result(fallible_area)
        type(varying_string), intent(in) :: string
        class(area_unit_t), intent(in) :: units(:)
        type(fallible_area_t) :: fallible_area

        type(fallible_area_t) :: all_attempts(size(units))
        integer :: i

        do i = 1, size(units)
            all_attempts(i) = units(i)%parse_as(string)
            if (.not. all_attempts(i)%failed()) then
                fallible_area = all_attempts(i)
                return
            end if
        end do
        fallible_area = fallible_area_t(error_list_t( &
                all_attempts%errors(), &
                module_t(MODULE_NAME), &
                procedure_t("parse_area_with_units_s")))
    end function

    elemental function from_units(value_, units) result(area)
        double precision, intent(in) :: value_
        class(area_unit_t), intent(in) :: units
        type(area_t) :: area

        area%square_meters = value_ / units%conversion_factor
    end function

    elemental function to_units(self, units) result(area)
        class(area_t), intent(in) :: self
        class(area_unit_t), intent(in) :: units
        double precision :: area

        area = self%square_meters * units%conversion_factor
    end function

    elemental function double_times_area( &
            multiplier, area) result(new_area)
        double precision, intent(in) :: multiplier
        class(area_t), intent(in) :: area
        type(area_t) :: new_area

        new_area%square_meters = &
                multiplier * area%square_meters
    end function

    elemental function integer_times_area( &
            multiplier, area) result(new_area)
        integer, intent(in) :: multiplier
        class(area_t), intent(in) :: area
        type(area_t) :: new_area

        new_area%square_meters = &
                dble(multiplier) * area%square_meters
    end function

    elemental function area_times_double( &
            area, multiplier) result(new_area)
        class(area_t), intent(in) :: area
        double precision, intent(in) :: multiplier
        type(area_t) :: new_area

        new_area%square_meters = &
                area%square_meters * multiplier
    end function

    elemental function area_times_integer( &
            area, multiplier) result(new_area)
        class(area_t), intent(in) :: area
        integer, intent(in) :: multiplier
        type(area_t) :: new_area

        new_area%square_meters = &
                area%square_meters * dble(multiplier)
    end function

    elemental function area_divided_by_double( &
            area, divisor) result(new_area)
        class(area_t), intent(in) :: area
        double precision, intent(in) :: divisor
        type(area_t) :: new_area

        new_area%square_meters = &
                area%square_meters / divisor
    end function

    elemental function area_divided_by_integer( &
            area, divisor) result(new_area)
        class(area_t), intent(in) :: area
        integer, intent(in) :: divisor
        type(area_t) :: new_area

        new_area%square_meters = &
                area%square_meters / dble(divisor)
    end function

    elemental function area_divided_by_area( &
            numerator, denomenator) result(ratio)
        class(area_t), intent(in) :: numerator
        type(area_t), intent(in) :: denomenator
        double precision :: ratio

        ratio = numerator%square_meters / denomenator%square_meters
    end function

    elemental function area_plus_area( &
            lhs, rhs) result(sum_)
        class(area_t), intent(in) :: lhs
        type(area_t), intent(in) :: rhs
        type(area_t) :: sum_

        sum_%square_meters = lhs%square_meters + rhs%square_meters
    end function

    elemental function area_minus_area( &
            lhs, rhs) result(difference)
        class(area_t), intent(in) :: lhs
        type(area_t), intent(in) :: rhs
        type(area_t) :: difference

        difference%square_meters = lhs%square_meters - rhs%square_meters
    end function

    pure function sum_area(areas) result(sum_)
        type(area_t), intent(in) :: areas(:)
        type(area_t) :: sum_

        sum_%square_meters = sum(areas%square_meters)
    end function

    elemental function greater_than(lhs, rhs)
        class(area_t), intent(in) :: lhs
        type(area_t), intent(in) :: rhs
        logical :: greater_than

        greater_than = lhs%square_meters > rhs%square_meters
    end function

    elemental function less_than(lhs, rhs)
        class(area_t), intent(in) :: lhs
        type(area_t), intent(in) :: rhs
        logical :: less_than

        less_than = lhs%square_meters < rhs%square_meters
    end function

    elemental function greater_than_or_equal(lhs, rhs)
        class(area_t), intent(in) :: lhs
        type(area_t), intent(in) :: rhs
        logical :: greater_than_or_equal

        greater_than_or_equal = lhs%square_meters >= rhs%square_meters
    end function

    elemental function less_than_or_equal(lhs, rhs)
        class(area_t), intent(in) :: lhs
        type(area_t), intent(in) :: rhs
        logical :: less_than_or_equal

        less_than_or_equal = lhs%square_meters <= rhs%square_meters
    end function

    elemental function equal_(lhs, rhs)
        class(area_t), intent(in) :: lhs
        type(area_t), intent(in) :: rhs
        logical :: equal_

        equal_ = lhs%square_meters .safeEq. rhs%square_meters
    end function

    elemental function equal_within_absolute_(lhs, rhs, within)
        class(area_t), intent(in) :: lhs
        type(area_t), intent(in) :: rhs
        type(area_t), intent(in) :: within
        logical :: equal_within_absolute_

        equal_within_absolute_ = equal_within_absolute( &
                lhs%square_meters, rhs%square_meters, within%square_meters)
    end function

    elemental function equal_within_relative_(lhs, rhs, within)
        class(area_t), intent(in) :: lhs
        type(area_t), intent(in) :: rhs
        double precision, intent(in) :: within
        logical :: equal_within_relative_

        equal_within_relative_ = equal_within_relative( &
                lhs%square_meters, rhs%square_meters, within)
    end function

    elemental function not_equal(lhs, rhs)
        class(area_t), intent(in) :: lhs
        type(area_t), intent(in) :: rhs
        logical :: not_equal

        not_equal = .not. lhs == rhs
    end function

    elemental function to_string_full_precision(self) result(string)
        class(area_t), intent(in) :: self
        type(varying_string) :: string

        string = self%to_string_in(DEFAULT_OUTPUT_UNITS)
    end function

    elemental function to_string_with_precision(self, significant_digits) result(string)
        class(area_t), intent(in) :: self
        integer, intent(in) :: significant_digits
        type(varying_string) :: string

        string = self%to_string_in(DEFAULT_OUTPUT_UNITS, significant_digits)
    end function

    elemental function to_string_in_full_precision(self, units) result(string)
        class(area_t), intent(in) :: self
        class(area_unit_t), intent(in) :: units
        type(varying_string) :: string

        string = units%to_string(to_string(self.in.units))
    end function

    elemental function to_string_in_with_precision( &
            self, units, significant_digits) result(string)
        class(area_t), intent(in) :: self
        class(area_unit_t), intent(in) :: units
        integer, intent(in) :: significant_digits
        type(varying_string) :: string

        string = units%to_string(to_string(self.in.units, significant_digits))
    end function

    function fallible_area_from_area( &
            area) result(fallible_area)
        type(area_t), intent(in) :: area
        type(fallible_area_t) :: fallible_area

        fallible_area%area_ = area
    end function

    function fallible_area_from_errors( &
            errors) result(fallible_area)
        type(error_list_t), intent(in) :: errors
        type(fallible_area_t) :: fallible_area

        fallible_area%errors_ = errors
    end function

    function fallible_area_from_fallible_area( &
            fallible_area, &
            module_, &
            procedure_) &
            result(new_fallible_area)
        type(fallible_area_t), intent(in) :: fallible_area
        type(module_t), intent(in) :: module_
        type(procedure_t), intent(in) :: procedure_
        type(fallible_area_t) :: new_fallible_area

        if (fallible_area%failed()) then
            new_fallible_area%errors_ = error_list_t( &
                    fallible_area%errors_, module_, procedure_)
        else
            new_fallible_area%area_ = fallible_area%area_
        end if
    end function

    elemental function fallible_area_failed( &
            self) result(failed)
        class(fallible_area_t), intent(in) :: self
        logical :: failed

        failed = self%errors_%has_any()
    end function

    elemental function fallible_area_area( &
            self) result(area)
        class(fallible_area_t), intent(in) :: self
        type(area_t) :: area

        area = self%area_
    end function

    impure elemental function fallible_area_errors( &
            self) result(errors)
        class(fallible_area_t), intent(in) :: self
        type(error_list_t) :: errors

        errors = self%errors_
    end function

    function fallible_area_unit_from_unit( &
            unit) result(fallible_area_unit)
        class(area_unit_t), intent(in) :: unit
        type(fallible_area_unit_t) :: fallible_area_unit

        allocate(fallible_area_unit%unit_, source = unit)
    end function

    function fallible_area_unit_from_errors( &
            errors) result(fallible_area_unit)
        type(error_list_t), intent(in) :: errors
        type(fallible_area_unit_t) :: fallible_area_unit

        fallible_area_unit%errors_ = errors
    end function

    function fallible_area_unit_from_fallible_area_unit( &
            fallible_area_unit, &
            module_, &
            procedure_) &
            result(new_fallible_area_unit)
        type(fallible_area_unit_t), intent(in) :: fallible_area_unit
        type(module_t), intent(in) :: module_
        type(procedure_t), intent(in) :: procedure_
        type(fallible_area_unit_t) :: new_fallible_area_unit

        if (fallible_area_unit%failed()) then
            new_fallible_area_unit%errors_ = error_list_t( &
                    fallible_area_unit%errors_, module_, procedure_)
        else
            allocate(new_fallible_area_unit%unit_, source = &
                    fallible_area_unit%unit_)
        end if
    end function

    elemental function fallible_area_unit_failed( &
            self) result(failed)
        class(fallible_area_unit_t), intent(in) :: self
        logical :: failed

        failed = self%errors_%has_any()
    end function

    function fallible_area_unit_unit( &
            self) result(unit)
        class(fallible_area_unit_t), intent(in) :: self
        class(area_unit_t), allocatable :: unit

        allocate(unit, source = self%unit_)
    end function

    impure elemental function fallible_area_unit_errors( &
            self) result(errors)
        class(fallible_area_unit_t), intent(in) :: self
        type(error_list_t) :: errors

        errors = self%errors_
    end function

    elemental function simple_unit_to_string(self) result(string)
        class(area_simple_unit_t), intent(in) :: self
        type(varying_string) :: string

        string = trim(self%symbol)
    end function

    pure function simple_value_to_string(self, value_) result(string)
        class(area_simple_unit_t), intent(in) :: self
        type(varying_string), intent(in) :: value_
        type(varying_string) :: string

        string = value_ // " " // self%to_string()
    end function

    function simple_parse_as(self, string) result(fallible_area)
        class(area_simple_unit_t), intent(in) :: self
        type(varying_string), intent(in) :: string
        type(fallible_area_t) :: fallible_area

        type(parse_result_t) :: parse_result

        parse_result = parse_with(the_parser, string)
        if (parse_result%ok) then
            select type (the_number => parse_result%parsed)
            type is (parsed_rational_t)
                fallible_area = fallible_area_t(the_number%value_.unit.self)
            end select
        else
            fallible_area = fallible_area_t(error_list_t(fatal_t( &
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

    function parse_area_unit_c(string) result(fallible_area_unit)
        character(len=*), intent(in) :: string
        type(fallible_area_unit_t) :: fallible_area_unit

        fallible_area_unit = fallible_area_unit_t( &
                parse_area_unit(var_str(string), PROVIDED_UNITS), &
                module_t(MODULE_NAME), &
                procedure_t("parse_area_unit_c"))
    end function

    function parse_area_unit_s(string) result(fallible_area_unit)
        type(varying_string), intent(in) :: string
        type(fallible_area_unit_t) :: fallible_area_unit

        fallible_area_unit = fallible_area_unit_t( &
                parse_area_unit(string, PROVIDED_UNITS), &
                module_t(MODULE_NAME), &
                procedure_t("parse_area_unit_s"))
    end function

    function parse_area_unit_with_units_c( &
            string, units) result(fallible_area_unit)
        character(len=*), intent(in) :: string
        class(area_unit_t), intent(in) :: units(:)
        type(fallible_area_unit_t) :: fallible_area_unit

        fallible_area_unit = fallible_area_unit_t( &
                parse_area_unit(var_str(string), units), &
                module_t(MODULE_NAME), &
                procedure_t("parse_area_unit_with_units_c"))
    end function

    function parse_area_unit_with_units_s( &
            string, units) result(fallible_area_unit)
        type(varying_string), intent(in) :: string
        class(area_unit_t), intent(in) :: units(:)
        type(fallible_area_unit_t) :: fallible_area_unit

        integer :: i
        type(varying_string) :: unit_strings(size(units))

        do i = 1, size(units)
            if (string == units(i)%to_string()) then
                fallible_area_unit = fallible_area_unit_t(units(i))
                return
            end if
        end do
        do i = 1, size(units)
            unit_strings(i) = units(i)%to_string()
        end do
        fallible_area_unit = fallible_area_unit_t(error_list_t(fatal_t( &
                UNKNOWN_UNIT, &
                module_t(MODULE_NAME), &
                procedure_t("parse_area_unit_with_units_s"), &
                '"' // string // '", known units: [' // join(unit_strings, ', ') // ']')))
    end function
end module
