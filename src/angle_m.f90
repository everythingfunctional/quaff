module angle_m
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
    use quaff_conversion_factors_m, only: DEGREES_PER_RADIAN
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
            angle_t, &
            fallible_angle_t, &
            angle_unit_t, &
            fallible_angle_unit_t, &
            angle_simple_unit_t, &
            operator(.unit.), &
            parse_angle, &
            parse_angle_unit, &
            sum, &
            sin, &
            cos, &
            tan, &
            asin_, &
            acos_, &
            atan_, &
            atan2_, &
            DEFAULT_OUTPUT_UNITS, &
            PROVIDED_UNITS, &
            DEGREES, &
            RADIANS

    type :: angle_t
        double precision :: radians
    contains
        private
        procedure :: to_units
        generic, public :: operator(.in.) => to_units
        procedure, pass(angle) :: double_times_angle
        procedure, pass(angle) :: integer_times_angle
        procedure :: angle_times_double
        procedure :: angle_times_integer
        generic, public :: operator(*) => &
                double_times_angle, &
                integer_times_angle, &
                angle_times_double, &
                angle_times_integer
        procedure :: angle_divided_by_double
        procedure :: angle_divided_by_integer
        procedure :: angle_divided_by_angle
        generic, public :: operator(/) => &
                angle_divided_by_double, &
                angle_divided_by_integer, &
                angle_divided_by_angle
        procedure :: angle_plus_angle
        generic, public :: operator(+) => angle_plus_angle
        procedure :: angle_minus_angle
        generic, public :: operator(-) => angle_minus_angle
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

    type :: fallible_angle_t
        private
        type(angle_t) :: angle_
        type(error_list_t) :: errors_
    contains
        private
        procedure, public :: failed => fallible_angle_failed
        procedure, public :: angle => fallible_angle_angle
        procedure, public :: errors => fallible_angle_errors
    end type

    type, abstract :: angle_unit_t
        double precision :: conversion_factor
    contains
        procedure(unit_to_string_i), deferred :: unit_to_string
        procedure(value_to_string_i), deferred :: value_to_string
        generic :: to_string => unit_to_string, value_to_string
        procedure(parse_as_i), deferred :: parse_as
    end type

    type :: fallible_angle_unit_t
        private
        class(angle_unit_t), allocatable :: unit_
        type(error_list_t) :: errors_
    contains
        private
        procedure, public :: failed => fallible_angle_unit_failed
        procedure, public :: unit => fallible_angle_unit_unit
        procedure, public :: errors => fallible_angle_unit_errors
    end type

    type, extends(angle_unit_t) :: angle_simple_unit_t
        character(len=20) :: symbol
    contains
        procedure :: unit_to_string => simple_unit_to_string
        procedure :: value_to_string => simple_value_to_string
        procedure :: parse_as => simple_parse_as
    end type

    abstract interface
        elemental function unit_to_string_i(self) result(string)
            import :: angle_unit_t, varying_string

            implicit none

            class(angle_unit_t), intent(in) :: self
            type(varying_string) :: string
        end function

        pure function value_to_string_i(self, value_) result(string)
            import :: angle_unit_t, varying_string

            implicit none

            class(angle_unit_t), intent(in) :: self
            type(varying_string), intent(in) :: value_
            type(varying_string) :: string
        end function

        function parse_as_i(self, string) result(fallible_angle)
            import :: angle_unit_t, fallible_angle_t, varying_string

            implicit none

            class(angle_unit_t), intent(in) :: self
            type(varying_string), intent(in) :: string
            type(fallible_angle_t) :: fallible_angle
        end function
    end interface

    interface operator(.unit.)
        module procedure from_units
    end interface

    interface fallible_angle_t
        module procedure fallible_angle_from_angle
        module procedure fallible_angle_from_errors
        module procedure fallible_angle_from_fallible_angle
    end interface

    interface fallible_angle_unit_t
        module procedure fallible_angle_unit_from_unit
        module procedure fallible_angle_unit_from_errors
        module procedure fallible_angle_unit_from_fallible_angle_unit
    end interface

    interface parse_angle
        module procedure parse_angle_c
        module procedure parse_angle_s
        module procedure parse_angle_with_units_c
        module procedure parse_angle_with_units_s
    end interface

    interface parse_angle_unit
        module procedure parse_angle_unit_c
        module procedure parse_angle_unit_s
        module procedure parse_angle_unit_with_units_c
        module procedure parse_angle_unit_with_units_s
    end interface

    interface sum
        module procedure sum_angle
    end interface

    interface sin
        module procedure sin_
    end interface

    interface cos
        module procedure cos_
    end interface

    interface tan
        module procedure tan_
    end interface

    type(angle_simple_unit_t), parameter :: DEGREES = &
            angle_simple_unit_t( &
                    conversion_factor = DEGREES_PER_RADIAN, &
                    symbol = "deg")
    type(angle_simple_unit_t), parameter :: RADIANS = &
            angle_simple_unit_t( &
                    conversion_factor = 1.0d0, &
                    symbol = "rad")

    type(angle_simple_unit_t) :: DEFAULT_OUTPUT_UNITS = RADIANS

    type(angle_simple_unit_t), parameter :: PROVIDED_UNITS(*) = &
            [DEGREES, RADIANS]

    character(len=*), parameter :: MODULE_NAME = "angle_m"
contains
    function parse_angle_c(string) result(fallible_angle)
        character(len=*), intent(in) :: string
        type(fallible_angle_t) :: fallible_angle

        fallible_angle = fallible_angle_t( &
                parse_angle(var_str(string), PROVIDED_UNITS), &
                module_t(MODULE_NAME), &
                procedure_t("parse_angle_c"))
    end function

    function parse_angle_s(string) result(fallible_angle)
        type(varying_string), intent(in) :: string
        type(fallible_angle_t) :: fallible_angle

        fallible_angle = fallible_angle_t( &
                parse_angle(string, PROVIDED_UNITS), &
                module_t(MODULE_NAME), &
                procedure_t("parse_angle_s"))
    end function

    function parse_angle_with_units_c( &
            string, units) result(fallible_angle)
        character(len=*), intent(in) :: string
        class(angle_unit_t), intent(in) :: units(:)
        type(fallible_angle_t) :: fallible_angle

        fallible_angle = fallible_angle_t( &
                parse_angle(var_str(string), units), &
                module_t(MODULE_NAME), &
                procedure_t("parse_angle_with_units_c"))
    end function

    function parse_angle_with_units_s( &
            string, units) result(fallible_angle)
        type(varying_string), intent(in) :: string
        class(angle_unit_t), intent(in) :: units(:)
        type(fallible_angle_t) :: fallible_angle

        type(fallible_angle_t) :: all_attempts(size(units))
        integer :: i

        do i = 1, size(units)
            all_attempts(i) = units(i)%parse_as(string)
            if (.not. all_attempts(i)%failed()) then
                fallible_angle = all_attempts(i)
                return
            end if
        end do
        fallible_angle = fallible_angle_t(error_list_t( &
                all_attempts%errors(), &
                module_t(MODULE_NAME), &
                procedure_t("parse_angle_with_units_s")))
    end function

    elemental function from_units(value_, units) result(angle)
        double precision, intent(in) :: value_
        class(angle_unit_t), intent(in) :: units
        type(angle_t) :: angle

        angle%radians = value_ / units%conversion_factor
    end function

    elemental function to_units(self, units) result(angle)
        class(angle_t), intent(in) :: self
        class(angle_unit_t), intent(in) :: units
        double precision :: angle

        angle = self%radians * units%conversion_factor
    end function

    elemental function double_times_angle( &
            multiplier, angle) result(new_angle)
        double precision, intent(in) :: multiplier
        class(angle_t), intent(in) :: angle
        type(angle_t) :: new_angle

        new_angle%radians = &
                multiplier * angle%radians
    end function

    elemental function integer_times_angle( &
            multiplier, angle) result(new_angle)
        integer, intent(in) :: multiplier
        class(angle_t), intent(in) :: angle
        type(angle_t) :: new_angle

        new_angle%radians = &
                dble(multiplier) * angle%radians
    end function

    elemental function angle_times_double( &
            angle, multiplier) result(new_angle)
        class(angle_t), intent(in) :: angle
        double precision, intent(in) :: multiplier
        type(angle_t) :: new_angle

        new_angle%radians = &
                angle%radians * multiplier
    end function

    elemental function angle_times_integer( &
            angle, multiplier) result(new_angle)
        class(angle_t), intent(in) :: angle
        integer, intent(in) :: multiplier
        type(angle_t) :: new_angle

        new_angle%radians = &
                angle%radians * dble(multiplier)
    end function

    elemental function angle_divided_by_double( &
            angle, divisor) result(new_angle)
        class(angle_t), intent(in) :: angle
        double precision, intent(in) :: divisor
        type(angle_t) :: new_angle

        new_angle%radians = &
                angle%radians / divisor
    end function

    elemental function angle_divided_by_integer( &
            angle, divisor) result(new_angle)
        class(angle_t), intent(in) :: angle
        integer, intent(in) :: divisor
        type(angle_t) :: new_angle

        new_angle%radians = &
                angle%radians / dble(divisor)
    end function

    elemental function angle_divided_by_angle( &
            numerator, denomenator) result(ratio)
        class(angle_t), intent(in) :: numerator
        type(angle_t), intent(in) :: denomenator
        double precision :: ratio

        ratio = numerator%radians / denomenator%radians
    end function

    elemental function angle_plus_angle( &
            lhs, rhs) result(sum_)
        class(angle_t), intent(in) :: lhs
        type(angle_t), intent(in) :: rhs
        type(angle_t) :: sum_

        sum_%radians = lhs%radians + rhs%radians
    end function

    elemental function angle_minus_angle( &
            lhs, rhs) result(difference)
        class(angle_t), intent(in) :: lhs
        type(angle_t), intent(in) :: rhs
        type(angle_t) :: difference

        difference%radians = lhs%radians - rhs%radians
    end function

    pure function sum_angle(angles) result(sum_)
        type(angle_t), intent(in) :: angles(:)
        type(angle_t) :: sum_

        sum_%radians = sum(angles%radians)
    end function

    elemental function greater_than(lhs, rhs)
        class(angle_t), intent(in) :: lhs
        type(angle_t), intent(in) :: rhs
        logical :: greater_than

        greater_than = lhs%radians > rhs%radians
    end function

    elemental function less_than(lhs, rhs)
        class(angle_t), intent(in) :: lhs
        type(angle_t), intent(in) :: rhs
        logical :: less_than

        less_than = lhs%radians < rhs%radians
    end function

    elemental function greater_than_or_equal(lhs, rhs)
        class(angle_t), intent(in) :: lhs
        type(angle_t), intent(in) :: rhs
        logical :: greater_than_or_equal

        greater_than_or_equal = lhs%radians >= rhs%radians
    end function

    elemental function less_than_or_equal(lhs, rhs)
        class(angle_t), intent(in) :: lhs
        type(angle_t), intent(in) :: rhs
        logical :: less_than_or_equal

        less_than_or_equal = lhs%radians <= rhs%radians
    end function

    elemental function equal_(lhs, rhs)
        class(angle_t), intent(in) :: lhs
        type(angle_t), intent(in) :: rhs
        logical :: equal_

        equal_ = lhs%radians .safeEq. rhs%radians
    end function

    elemental function equal_within_absolute_(lhs, rhs, within)
        class(angle_t), intent(in) :: lhs
        type(angle_t), intent(in) :: rhs
        type(angle_t), intent(in) :: within
        logical :: equal_within_absolute_

        equal_within_absolute_ = equal_within_absolute( &
                lhs%radians, rhs%radians, within%radians)
    end function

    elemental function equal_within_relative_(lhs, rhs, within)
        class(angle_t), intent(in) :: lhs
        type(angle_t), intent(in) :: rhs
        double precision, intent(in) :: within
        logical :: equal_within_relative_

        equal_within_relative_ = equal_within_relative( &
                lhs%radians, rhs%radians, within)
    end function

    elemental function not_equal(lhs, rhs)
        class(angle_t), intent(in) :: lhs
        type(angle_t), intent(in) :: rhs
        logical :: not_equal

        not_equal = .not. lhs == rhs
    end function

    elemental function to_string_full_precision(self) result(string)
        class(angle_t), intent(in) :: self
        type(varying_string) :: string

        string = self%to_string_in(DEFAULT_OUTPUT_UNITS)
    end function

    elemental function to_string_with_precision(self, significant_digits) result(string)
        class(angle_t), intent(in) :: self
        integer, intent(in) :: significant_digits
        type(varying_string) :: string

        string = self%to_string_in(DEFAULT_OUTPUT_UNITS, significant_digits)
    end function

    elemental function to_string_in_full_precision(self, units) result(string)
        class(angle_t), intent(in) :: self
        class(angle_unit_t), intent(in) :: units
        type(varying_string) :: string

        string = units%to_string(to_string(self.in.units))
    end function

    elemental function to_string_in_with_precision( &
            self, units, significant_digits) result(string)
        class(angle_t), intent(in) :: self
        class(angle_unit_t), intent(in) :: units
        integer, intent(in) :: significant_digits
        type(varying_string) :: string

        string = units%to_string(to_string(self.in.units, significant_digits))
    end function

    function fallible_angle_from_angle( &
            angle) result(fallible_angle)
        type(angle_t), intent(in) :: angle
        type(fallible_angle_t) :: fallible_angle

        fallible_angle%angle_ = angle
    end function

    function fallible_angle_from_errors( &
            errors) result(fallible_angle)
        type(error_list_t), intent(in) :: errors
        type(fallible_angle_t) :: fallible_angle

        fallible_angle%errors_ = errors
    end function

    function fallible_angle_from_fallible_angle( &
            fallible_angle, &
            module_, &
            procedure_) &
            result(new_fallible_angle)
        type(fallible_angle_t), intent(in) :: fallible_angle
        type(module_t), intent(in) :: module_
        type(procedure_t), intent(in) :: procedure_
        type(fallible_angle_t) :: new_fallible_angle

        if (fallible_angle%failed()) then
            new_fallible_angle%errors_ = error_list_t( &
                    fallible_angle%errors_, module_, procedure_)
        else
            new_fallible_angle%angle_ = fallible_angle%angle_
        end if
    end function

    elemental function fallible_angle_failed( &
            self) result(failed)
        class(fallible_angle_t), intent(in) :: self
        logical :: failed

        failed = self%errors_%has_any()
    end function

    elemental function fallible_angle_angle( &
            self) result(angle)
        class(fallible_angle_t), intent(in) :: self
        type(angle_t) :: angle

        angle = self%angle_
    end function

    impure elemental function fallible_angle_errors( &
            self) result(errors)
        class(fallible_angle_t), intent(in) :: self
        type(error_list_t) :: errors

        errors = self%errors_
    end function

    function fallible_angle_unit_from_unit( &
            unit) result(fallible_angle_unit)
        class(angle_unit_t), intent(in) :: unit
        type(fallible_angle_unit_t) :: fallible_angle_unit

        allocate(fallible_angle_unit%unit_, source = unit)
    end function

    function fallible_angle_unit_from_errors( &
            errors) result(fallible_angle_unit)
        type(error_list_t), intent(in) :: errors
        type(fallible_angle_unit_t) :: fallible_angle_unit

        fallible_angle_unit%errors_ = errors
    end function

    function fallible_angle_unit_from_fallible_angle_unit( &
            fallible_angle_unit, &
            module_, &
            procedure_) &
            result(new_fallible_angle_unit)
        type(fallible_angle_unit_t), intent(in) :: fallible_angle_unit
        type(module_t), intent(in) :: module_
        type(procedure_t), intent(in) :: procedure_
        type(fallible_angle_unit_t) :: new_fallible_angle_unit

        if (fallible_angle_unit%failed()) then
            new_fallible_angle_unit%errors_ = error_list_t( &
                    fallible_angle_unit%errors_, module_, procedure_)
        else
            allocate(new_fallible_angle_unit%unit_, source = &
                    fallible_angle_unit%unit_)
        end if
    end function

    elemental function fallible_angle_unit_failed( &
            self) result(failed)
        class(fallible_angle_unit_t), intent(in) :: self
        logical :: failed

        failed = self%errors_%has_any()
    end function

    function fallible_angle_unit_unit( &
            self) result(unit)
        class(fallible_angle_unit_t), intent(in) :: self
        class(angle_unit_t), allocatable :: unit

        allocate(unit, source = self%unit_)
    end function

    impure elemental function fallible_angle_unit_errors( &
            self) result(errors)
        class(fallible_angle_unit_t), intent(in) :: self
        type(error_list_t) :: errors

        errors = self%errors_
    end function

    elemental function simple_unit_to_string(self) result(string)
        class(angle_simple_unit_t), intent(in) :: self
        type(varying_string) :: string

        string = trim(self%symbol)
    end function

    pure function simple_value_to_string(self, value_) result(string)
        class(angle_simple_unit_t), intent(in) :: self
        type(varying_string), intent(in) :: value_
        type(varying_string) :: string

        string = value_ // " " // self%to_string()
    end function

    function simple_parse_as(self, string) result(fallible_angle)
        class(angle_simple_unit_t), intent(in) :: self
        type(varying_string), intent(in) :: string
        type(fallible_angle_t) :: fallible_angle

        type(parse_result_t) :: parse_result

        parse_result = parse_with(the_parser, string)
        if (parse_result%ok()) then
            select type (the_number => parse_result%parsed())
            type is (parsed_rational_t)
                fallible_angle = fallible_angle_t(the_number%value_().unit.self)
            end select
        else
            fallible_angle = fallible_angle_t(error_list_t(fatal_t( &
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

    function parse_angle_unit_c(string) result(fallible_angle_unit)
        character(len=*), intent(in) :: string
        type(fallible_angle_unit_t) :: fallible_angle_unit

        fallible_angle_unit = fallible_angle_unit_t( &
                parse_angle_unit(var_str(string), PROVIDED_UNITS), &
                module_t(MODULE_NAME), &
                procedure_t("parse_angle_unit_c"))
    end function

    function parse_angle_unit_s(string) result(fallible_angle_unit)
        type(varying_string), intent(in) :: string
        type(fallible_angle_unit_t) :: fallible_angle_unit

        fallible_angle_unit = fallible_angle_unit_t( &
                parse_angle_unit(string, PROVIDED_UNITS), &
                module_t(MODULE_NAME), &
                procedure_t("parse_angle_unit_s"))
    end function

    function parse_angle_unit_with_units_c( &
            string, units) result(fallible_angle_unit)
        character(len=*), intent(in) :: string
        class(angle_unit_t), intent(in) :: units(:)
        type(fallible_angle_unit_t) :: fallible_angle_unit

        fallible_angle_unit = fallible_angle_unit_t( &
                parse_angle_unit(var_str(string), units), &
                module_t(MODULE_NAME), &
                procedure_t("parse_angle_unit_with_units_c"))
    end function

    function parse_angle_unit_with_units_s( &
            string, units) result(fallible_angle_unit)
        type(varying_string), intent(in) :: string
        class(angle_unit_t), intent(in) :: units(:)
        type(fallible_angle_unit_t) :: fallible_angle_unit

        integer :: i
        type(varying_string) :: unit_strings(size(units))

        do i = 1, size(units)
            if (string == units(i)%to_string()) then
                fallible_angle_unit = fallible_angle_unit_t(units(i))
                return
            end if
        end do
        do i = 1, size(units)
            unit_strings(i) = units(i)%to_string()
        end do
        fallible_angle_unit = fallible_angle_unit_t(error_list_t(fatal_t( &
                UNKNOWN_UNIT, &
                module_t(MODULE_NAME), &
                procedure_t("parse_angle_unit_with_units_s"), &
                '"' // string // '", known units: [' // join(unit_strings, ', ') // ']')))
    end function

    elemental function sin_(angle)
        type(angle_t), intent(in) :: angle
        double precision :: sin_

        sin_ = sin(angle%radians)
    end function

    elemental function cos_(angle)
        type(angle_t), intent(in) :: angle
        double precision :: cos_

        cos_ = cos(angle%radians)
    end function

    elemental function tan_(angle)
        type(angle_t), intent(in) :: angle
        double precision :: tan_

        tan_ = tan(angle%radians)
    end function

    elemental function asin_(number) result(angle)
        double precision, intent(in) :: number
        type(angle_t) :: angle

        angle%radians = asin(number)
    end function

    elemental function acos_(number) result(angle)
        double precision, intent(in) :: number
        type(angle_t) :: angle

        angle%radians = acos(number)
    end function

    elemental function atan_(number) result(angle)
        double precision, intent(in) :: number
        type(angle_t) :: angle

        angle%radians = atan(number)
    end function

    elemental function atan2_(y, x) result(angle)
        double precision, intent(in) :: y
        double precision, intent(in) :: x
        type(angle_t) :: angle

        angle%radians = atan2(y, x)
    end function
end module
