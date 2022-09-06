module quaff_pressure_m
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
            ATMOSPHERES_PER_PASCAL, &
            BAR_PER_PASCAL, &
            DYNES_PER_SQUARE_CENTIMETER_PER_PASCAL, &
            KILOPASCALS_PER_PASCAL, &
            KILOPONDS_PER_SQUARE_CENTIMETER_PER_PASCAL, &
            MEGAPASCALS_PER_PASCAL, &
            POUNDS_PER_SQUARE_INCH_PER_PASCAL, &
            KILOPOUNDS_PER_SQUARE_INCH_PER_PASCAL
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
            pressure_t, &
            fallible_pressure_t, &
            pressure_unit_t, &
            fallible_pressure_unit_t, &
            pressure_simple_unit_t, &
            operator(.unit.), &
            parse_pressure, &
            parse_pressure_unit, &
            abs, &
            sum, &
            DEFAULT_OUTPUT_UNITS, &
            PROVIDED_UNITS, &
            ATMOSPHERES, &
            BAR, &
            DYNES_PER_SQUARE_CENTIMETER, &
            KILOPASCALS, &
            KILOPONDS_PER_SQUARE_CENTIMETER, &
            MEGAPASCALS, &
            PASCALS, &
            POUNDS_PER_SQUARE_INCH, &
            KILOPOUNDS_PER_SQUARE_INCH

    type :: pressure_t
        double precision :: pascals
    contains
        private
        procedure :: to_units
        generic, public :: operator(.in.) => to_units
        procedure, pass(pressure) :: double_times_pressure
        procedure, pass(pressure) :: integer_times_pressure
        procedure :: pressure_times_double
        procedure :: pressure_times_integer
        generic, public :: operator(*) => &
                double_times_pressure, &
                integer_times_pressure, &
                pressure_times_double, &
                pressure_times_integer
        procedure :: pressure_divided_by_double
        procedure :: pressure_divided_by_integer
        procedure :: pressure_divided_by_pressure
        generic, public :: operator(/) => &
                pressure_divided_by_double, &
                pressure_divided_by_integer, &
                pressure_divided_by_pressure
        procedure :: pressure_plus_pressure
        generic, public :: operator(+) => pressure_plus_pressure
        procedure :: negate_pressure
        procedure :: pressure_minus_pressure
        generic, public :: operator(-) => &
                negate_pressure, &
                pressure_minus_pressure
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

    type :: fallible_pressure_t
        private
        type(pressure_t) :: pressure_ = pressure_t(0.0d0)
        type(error_list_t) :: errors_
    contains
        private
        procedure, public :: failed => fallible_pressure_failed
        procedure, public :: pressure => fallible_pressure_pressure
        procedure, public :: errors => fallible_pressure_errors
    end type

    type, abstract :: pressure_unit_t
        double precision :: conversion_factor
    contains
        procedure(unit_to_string_i), deferred :: unit_to_string
        procedure(value_to_string_i), deferred :: value_to_string
        generic :: to_string => unit_to_string, value_to_string
        procedure(parse_as_i), deferred :: parse_as
    end type

    type :: fallible_pressure_unit_t
        private
        class(pressure_unit_t), allocatable :: unit_
        type(error_list_t) :: errors_
    contains
        private
        procedure, public :: failed => fallible_pressure_unit_failed
        procedure, public :: unit => fallible_pressure_unit_unit
        procedure, public :: errors => fallible_pressure_unit_errors
    end type

    type, extends(pressure_unit_t) :: pressure_simple_unit_t
        character(len=20) :: symbol
    contains
        procedure :: unit_to_string => simple_unit_to_string
        procedure :: value_to_string => simple_value_to_string
        procedure :: parse_as => simple_parse_as
    end type

    abstract interface
        elemental function unit_to_string_i(self) result(string)
            import :: pressure_unit_t, varying_string

            implicit none

            class(pressure_unit_t), intent(in) :: self
            type(varying_string) :: string
        end function

        pure function value_to_string_i(self, value_) result(string)
            import :: pressure_unit_t, varying_string

            implicit none

            class(pressure_unit_t), intent(in) :: self
            type(varying_string), intent(in) :: value_
            type(varying_string) :: string
        end function

        function parse_as_i(self, string) result(fallible_pressure)
            import :: pressure_unit_t, fallible_pressure_t, varying_string

            implicit none

            class(pressure_unit_t), intent(in) :: self
            type(varying_string), intent(in) :: string
            type(fallible_pressure_t) :: fallible_pressure
        end function
    end interface

    interface operator(.unit.)
        module procedure from_units
    end interface

    interface fallible_pressure_t
        module procedure fallible_pressure_from_pressure
        module procedure fallible_pressure_from_errors
        module procedure fallible_pressure_from_fallible_pressure
    end interface

    interface fallible_pressure_unit_t
        module procedure fallible_pressure_unit_from_unit
        module procedure fallible_pressure_unit_from_errors
        module procedure fallible_pressure_unit_from_fallible_pressure_unit
    end interface

    interface parse_pressure
        module procedure parse_pressure_c
        module procedure parse_pressure_s
        module procedure parse_pressure_with_units_c
        module procedure parse_pressure_with_units_s
    end interface

    interface parse_pressure_unit
        module procedure parse_pressure_unit_c
        module procedure parse_pressure_unit_s
        module procedure parse_pressure_unit_with_units_c
        module procedure parse_pressure_unit_with_units_s
    end interface

    interface abs
        module procedure abs_pressure
    end interface

    interface sum
        module procedure sum_pressure
    end interface

    type(pressure_simple_unit_t), parameter :: ATMOSPHERES = &
            pressure_simple_unit_t( &
                    conversion_factor = ATMOSPHERES_PER_PASCAL, &
                    symbol = "atm")
    type(pressure_simple_unit_t), parameter :: BAR = &
            pressure_simple_unit_t( &
                    conversion_factor = BAR_PER_PASCAL, &
                    symbol = "bar")
    type(pressure_simple_unit_t), parameter :: DYNES_PER_SQUARE_CENTIMETER = &
            pressure_simple_unit_t( &
                    conversion_factor = DYNES_PER_SQUARE_CENTIMETER_PER_PASCAL, &
                    symbol = "dyn/cm^2")
    type(pressure_simple_unit_t), parameter :: KILOPASCALS = &
            pressure_simple_unit_t( &
                    conversion_factor = KILOPASCALS_PER_PASCAL, &
                    symbol = "kPa")
    type(pressure_simple_unit_t), parameter :: KILOPONDS_PER_SQUARE_CENTIMETER = &
            pressure_simple_unit_t( &
                    conversion_factor = KILOPONDS_PER_SQUARE_CENTIMETER_PER_PASCAL, &
                    symbol = "kp/cm^2")
    type(pressure_simple_unit_t), parameter :: MEGAPASCALS = &
            pressure_simple_unit_t( &
                    conversion_factor = MEGAPASCALS_PER_PASCAL, &
                    symbol = "MPa")
    type(pressure_simple_unit_t), parameter :: PASCALS = &
            pressure_simple_unit_t( &
                    conversion_factor = 1.0d0, &
                    symbol = "Pa")
    type(pressure_simple_unit_t), parameter :: POUNDS_PER_SQUARE_INCH = &
            pressure_simple_unit_t( &
                    conversion_factor = POUNDS_PER_SQUARE_INCH_PER_PASCAL, &
                    symbol = "psi")
    type(pressure_simple_unit_t), parameter :: KILOPOUNDS_PER_SQUARE_INCH = &
            pressure_simple_unit_t( &
                    conversion_factor = KILOPOUNDS_PER_SQUARE_INCH_PER_PASCAL, &
                    symbol = "ksi")

    type(pressure_simple_unit_t) :: DEFAULT_OUTPUT_UNITS = PASCALS

    type(pressure_simple_unit_t), parameter :: PROVIDED_UNITS(*) = &
            [ ATMOSPHERES &
            , BAR &
            , DYNES_PER_SQUARE_CENTIMETER &
            , KILOPASCALS &
            , KILOPONDS_PER_SQUARE_CENTIMETER &
            , MEGAPASCALS &
            , PASCALS &
            , POUNDS_PER_SQUARE_INCH &
            , KILOPOUNDS_PER_SQUARE_INCH &
            ]

    character(len=*), parameter :: MODULE_NAME = "quaff_pressure_m"
contains
    function parse_pressure_c(string) result(fallible_pressure)
        character(len=*), intent(in) :: string
        type(fallible_pressure_t) :: fallible_pressure

        fallible_pressure = fallible_pressure_t( &
                parse_pressure(var_str(string), PROVIDED_UNITS), &
                module_t(MODULE_NAME), &
                procedure_t("parse_pressure_c"))
    end function

    function parse_pressure_s(string) result(fallible_pressure)
        type(varying_string), intent(in) :: string
        type(fallible_pressure_t) :: fallible_pressure

        fallible_pressure = fallible_pressure_t( &
                parse_pressure(string, PROVIDED_UNITS), &
                module_t(MODULE_NAME), &
                procedure_t("parse_pressure_s"))
    end function

    function parse_pressure_with_units_c( &
            string, units) result(fallible_pressure)
        character(len=*), intent(in) :: string
        class(pressure_unit_t), intent(in) :: units(:)
        type(fallible_pressure_t) :: fallible_pressure

        fallible_pressure = fallible_pressure_t( &
                parse_pressure(var_str(string), units), &
                module_t(MODULE_NAME), &
                procedure_t("parse_pressure_with_units_c"))
    end function

    function parse_pressure_with_units_s( &
            string, units) result(fallible_pressure)
        type(varying_string), intent(in) :: string
        class(pressure_unit_t), intent(in) :: units(:)
        type(fallible_pressure_t) :: fallible_pressure

        integer :: i

        do i = 1, size(units)
            fallible_pressure = units(i)%parse_as(string)
            if (.not. fallible_pressure%failed()) return
        end do
        fallible_pressure = fallible_pressure_t(error_list_t(fatal_t( &
                PARSE_ERROR, &
                module_t(MODULE_NAME), &
                procedure_t("parse_pressure_with_units_s"), &
                "Unable to parse '" // string // "' as a pressure_t. Tried with units: " &
                // join(units%to_string(), ", "))))
    end function

    elemental function from_units(value_, units) result(pressure)
        double precision, intent(in) :: value_
        class(pressure_unit_t), intent(in) :: units
        type(pressure_t) :: pressure

        pressure%pascals = value_ / units%conversion_factor
    end function

    elemental function to_units(self, units) result(pressure)
        class(pressure_t), intent(in) :: self
        class(pressure_unit_t), intent(in) :: units
        double precision :: pressure

        pressure = self%pascals * units%conversion_factor
    end function

    elemental function double_times_pressure( &
            multiplier, pressure) result(new_pressure)
        double precision, intent(in) :: multiplier
        class(pressure_t), intent(in) :: pressure
        type(pressure_t) :: new_pressure

        new_pressure%pascals = &
                multiplier * pressure%pascals
    end function

    elemental function integer_times_pressure( &
            multiplier, pressure) result(new_pressure)
        integer, intent(in) :: multiplier
        class(pressure_t), intent(in) :: pressure
        type(pressure_t) :: new_pressure

        new_pressure%pascals = &
                dble(multiplier) * pressure%pascals
    end function

    elemental function pressure_times_double( &
            pressure, multiplier) result(new_pressure)
        class(pressure_t), intent(in) :: pressure
        double precision, intent(in) :: multiplier
        type(pressure_t) :: new_pressure

        new_pressure%pascals = &
                pressure%pascals * multiplier
    end function

    elemental function pressure_times_integer( &
            pressure, multiplier) result(new_pressure)
        class(pressure_t), intent(in) :: pressure
        integer, intent(in) :: multiplier
        type(pressure_t) :: new_pressure

        new_pressure%pascals = &
                pressure%pascals * dble(multiplier)
    end function

    elemental function pressure_divided_by_double( &
            pressure, divisor) result(new_pressure)
        class(pressure_t), intent(in) :: pressure
        double precision, intent(in) :: divisor
        type(pressure_t) :: new_pressure

        new_pressure%pascals = &
                pressure%pascals / divisor
    end function

    elemental function pressure_divided_by_integer( &
            pressure, divisor) result(new_pressure)
        class(pressure_t), intent(in) :: pressure
        integer, intent(in) :: divisor
        type(pressure_t) :: new_pressure

        new_pressure%pascals = &
                pressure%pascals / dble(divisor)
    end function

    elemental function pressure_divided_by_pressure( &
            numerator, denomenator) result(ratio)
        class(pressure_t), intent(in) :: numerator
        type(pressure_t), intent(in) :: denomenator
        double precision :: ratio

        ratio = numerator%pascals / denomenator%pascals
    end function

    elemental function pressure_plus_pressure( &
            lhs, rhs) result(sum_)
        class(pressure_t), intent(in) :: lhs
        type(pressure_t), intent(in) :: rhs
        type(pressure_t) :: sum_

        sum_%pascals = lhs%pascals + rhs%pascals
    end function

    elemental function negate_pressure(self) result(negated)
        class(pressure_t), intent(in) :: self
        type(pressure_t) :: negated

        negated%pascals = -self%pascals
    end function

    elemental function pressure_minus_pressure( &
            lhs, rhs) result(difference)
        class(pressure_t), intent(in) :: lhs
        type(pressure_t), intent(in) :: rhs
        type(pressure_t) :: difference

        difference%pascals = lhs%pascals - rhs%pascals
    end function

    elemental function abs_pressure(pressure) result(abs_)
        type(pressure_t), intent(in) :: pressure
        type(pressure_t) :: abs_

        abs_%pascals = abs(pressure%pascals)
    end function

    pure function sum_pressure(pressures) result(sum_)
        type(pressure_t), intent(in) :: pressures(:)
        type(pressure_t) :: sum_

        sum_%pascals = sum(pressures%pascals)
    end function

    elemental function greater_than(lhs, rhs)
        class(pressure_t), intent(in) :: lhs
        type(pressure_t), intent(in) :: rhs
        logical :: greater_than

        greater_than = lhs%pascals > rhs%pascals
    end function

    elemental function less_than(lhs, rhs)
        class(pressure_t), intent(in) :: lhs
        type(pressure_t), intent(in) :: rhs
        logical :: less_than

        less_than = lhs%pascals < rhs%pascals
    end function

    elemental function greater_than_or_equal(lhs, rhs)
        class(pressure_t), intent(in) :: lhs
        type(pressure_t), intent(in) :: rhs
        logical :: greater_than_or_equal

        greater_than_or_equal = lhs%pascals >= rhs%pascals
    end function

    elemental function less_than_or_equal(lhs, rhs)
        class(pressure_t), intent(in) :: lhs
        type(pressure_t), intent(in) :: rhs
        logical :: less_than_or_equal

        less_than_or_equal = lhs%pascals <= rhs%pascals
    end function

    elemental function equal_(lhs, rhs)
        class(pressure_t), intent(in) :: lhs
        type(pressure_t), intent(in) :: rhs
        logical :: equal_

        equal_ = lhs%pascals .safeEq. rhs%pascals
    end function

    elemental function equal_within_absolute_(lhs, rhs, within)
        class(pressure_t), intent(in) :: lhs
        type(pressure_t), intent(in) :: rhs
        type(pressure_t), intent(in) :: within
        logical :: equal_within_absolute_

        equal_within_absolute_ = equal_within_absolute( &
                lhs%pascals, rhs%pascals, within%pascals)
    end function

    elemental function equal_within_relative_(lhs, rhs, within)
        class(pressure_t), intent(in) :: lhs
        type(pressure_t), intent(in) :: rhs
        double precision, intent(in) :: within
        logical :: equal_within_relative_

        equal_within_relative_ = equal_within_relative( &
                lhs%pascals, rhs%pascals, within)
    end function

    elemental function not_equal(lhs, rhs)
        class(pressure_t), intent(in) :: lhs
        type(pressure_t), intent(in) :: rhs
        logical :: not_equal

        not_equal = .not. lhs == rhs
    end function

    elemental function to_string_full_precision(self) result(string)
        class(pressure_t), intent(in) :: self
        type(varying_string) :: string

        string = self%to_string_in(DEFAULT_OUTPUT_UNITS)
    end function

    elemental function to_string_with_precision(self, significant_digits) result(string)
        class(pressure_t), intent(in) :: self
        integer, intent(in) :: significant_digits
        type(varying_string) :: string

        string = self%to_string_in(DEFAULT_OUTPUT_UNITS, significant_digits)
    end function

    elemental function to_string_in_full_precision(self, units) result(string)
        class(pressure_t), intent(in) :: self
        class(pressure_unit_t), intent(in) :: units
        type(varying_string) :: string

        string = units%to_string(to_string(self.in.units))
    end function

    elemental function to_string_in_with_precision( &
            self, units, significant_digits) result(string)
        class(pressure_t), intent(in) :: self
        class(pressure_unit_t), intent(in) :: units
        integer, intent(in) :: significant_digits
        type(varying_string) :: string

        string = units%to_string(to_string(self.in.units, significant_digits))
    end function

    function fallible_pressure_from_pressure( &
            pressure) result(fallible_pressure)
        type(pressure_t), intent(in) :: pressure
        type(fallible_pressure_t) :: fallible_pressure

        fallible_pressure%pressure_ = pressure
    end function

    function fallible_pressure_from_errors( &
            errors) result(fallible_pressure)
        type(error_list_t), intent(in) :: errors
        type(fallible_pressure_t) :: fallible_pressure

        fallible_pressure%errors_ = errors
    end function

    function fallible_pressure_from_fallible_pressure( &
            fallible_pressure, &
            module_, &
            procedure_) &
            result(new_fallible_pressure)
        type(fallible_pressure_t), intent(in) :: fallible_pressure
        type(module_t), intent(in) :: module_
        type(procedure_t), intent(in) :: procedure_
        type(fallible_pressure_t) :: new_fallible_pressure

        if (fallible_pressure%failed()) then
            new_fallible_pressure%errors_ = error_list_t( &
                    fallible_pressure%errors_, module_, procedure_)
        else
            new_fallible_pressure%pressure_ = fallible_pressure%pressure_
        end if
    end function

    elemental function fallible_pressure_failed( &
            self) result(failed)
        class(fallible_pressure_t), intent(in) :: self
        logical :: failed

        failed = self%errors_%has_any()
    end function

    elemental function fallible_pressure_pressure( &
            self) result(pressure)
        class(fallible_pressure_t), intent(in) :: self
        type(pressure_t) :: pressure

        pressure = self%pressure_
    end function

    impure elemental function fallible_pressure_errors( &
            self) result(errors)
        class(fallible_pressure_t), intent(in) :: self
        type(error_list_t) :: errors

        errors = self%errors_
    end function

    function fallible_pressure_unit_from_unit( &
            unit) result(fallible_pressure_unit)
        class(pressure_unit_t), intent(in) :: unit
        type(fallible_pressure_unit_t) :: fallible_pressure_unit

        allocate(fallible_pressure_unit%unit_, source = unit)
    end function

    function fallible_pressure_unit_from_errors( &
            errors) result(fallible_pressure_unit)
        type(error_list_t), intent(in) :: errors
        type(fallible_pressure_unit_t) :: fallible_pressure_unit

        fallible_pressure_unit%errors_ = errors
    end function

    function fallible_pressure_unit_from_fallible_pressure_unit( &
            fallible_pressure_unit, &
            module_, &
            procedure_) &
            result(new_fallible_pressure_unit)
        type(fallible_pressure_unit_t), intent(in) :: fallible_pressure_unit
        type(module_t), intent(in) :: module_
        type(procedure_t), intent(in) :: procedure_
        type(fallible_pressure_unit_t) :: new_fallible_pressure_unit

        if (fallible_pressure_unit%failed()) then
            new_fallible_pressure_unit%errors_ = error_list_t( &
                    fallible_pressure_unit%errors_, module_, procedure_)
        else
            allocate(new_fallible_pressure_unit%unit_, source = &
                    fallible_pressure_unit%unit_)
        end if
    end function

    elemental function fallible_pressure_unit_failed( &
            self) result(failed)
        class(fallible_pressure_unit_t), intent(in) :: self
        logical :: failed

        failed = self%errors_%has_any()
    end function

    function fallible_pressure_unit_unit( &
            self) result(unit)
        class(fallible_pressure_unit_t), intent(in) :: self
        class(pressure_unit_t), allocatable :: unit

        allocate(unit, source = self%unit_)
    end function

    impure elemental function fallible_pressure_unit_errors( &
            self) result(errors)
        class(fallible_pressure_unit_t), intent(in) :: self
        type(error_list_t) :: errors

        errors = self%errors_
    end function

    elemental function simple_unit_to_string(self) result(string)
        class(pressure_simple_unit_t), intent(in) :: self
        type(varying_string) :: string

        string = trim(self%symbol)
    end function

    pure function simple_value_to_string(self, value_) result(string)
        class(pressure_simple_unit_t), intent(in) :: self
        type(varying_string), intent(in) :: value_
        type(varying_string) :: string

        string = value_ // " " // self%to_string()
    end function

    function simple_parse_as(self, string) result(fallible_pressure)
        class(pressure_simple_unit_t), intent(in) :: self
        type(varying_string), intent(in) :: string
        type(fallible_pressure_t) :: fallible_pressure

        type(parse_result_t) :: parse_result

        parse_result = parse_with(the_parser, string)
        if (parse_result%ok) then
            select type (the_number => parse_result%parsed)
            type is (parsed_rational_t)
                fallible_pressure = fallible_pressure_t(the_number%value_.unit.self)
            end select
        else
            fallible_pressure = fallible_pressure_t(error_list_t(fatal_t( &
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

    function parse_pressure_unit_c(string) result(fallible_pressure_unit)
        character(len=*), intent(in) :: string
        type(fallible_pressure_unit_t) :: fallible_pressure_unit

        fallible_pressure_unit = fallible_pressure_unit_t( &
                parse_pressure_unit(var_str(string), PROVIDED_UNITS), &
                module_t(MODULE_NAME), &
                procedure_t("parse_pressure_unit_c"))
    end function

    function parse_pressure_unit_s(string) result(fallible_pressure_unit)
        type(varying_string), intent(in) :: string
        type(fallible_pressure_unit_t) :: fallible_pressure_unit

        fallible_pressure_unit = fallible_pressure_unit_t( &
                parse_pressure_unit(string, PROVIDED_UNITS), &
                module_t(MODULE_NAME), &
                procedure_t("parse_pressure_unit_s"))
    end function

    function parse_pressure_unit_with_units_c( &
            string, units) result(fallible_pressure_unit)
        character(len=*), intent(in) :: string
        class(pressure_unit_t), intent(in) :: units(:)
        type(fallible_pressure_unit_t) :: fallible_pressure_unit

        fallible_pressure_unit = fallible_pressure_unit_t( &
                parse_pressure_unit(var_str(string), units), &
                module_t(MODULE_NAME), &
                procedure_t("parse_pressure_unit_with_units_c"))
    end function

    function parse_pressure_unit_with_units_s( &
            string, units) result(fallible_pressure_unit)
        type(varying_string), intent(in) :: string
        class(pressure_unit_t), intent(in) :: units(:)
        type(fallible_pressure_unit_t) :: fallible_pressure_unit

        integer :: i
        type(varying_string), allocatable :: unit_strings(:)

        do i = 1, size(units)
            if (string == units(i)%to_string()) then
                fallible_pressure_unit = fallible_pressure_unit_t(units(i))
                return
            end if
        end do
        allocate(unit_strings(size(units)))
        do i = 1, size(units)
            unit_strings(i) = units(i)%to_string()
        end do
        fallible_pressure_unit = fallible_pressure_unit_t(error_list_t(fatal_t( &
                UNKNOWN_UNIT, &
                module_t(MODULE_NAME), &
                procedure_t("parse_pressure_unit_with_units_s"), &
                '"' // string // '", known units: [' // join(unit_strings, ', ') // ']')))
    end function
end module
