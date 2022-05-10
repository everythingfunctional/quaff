module quaff_utilities_m
    use erloff, only: message_type_t
    use parff, only: parser_output_t, state_t, parse_char

    implicit none
    private
    public :: &
            effectively_zero, &
            equal_within_absolute, &
            equal_within_relative, &
            operator(.safeEq.), &
            parse_space, &
            PARSE_ERROR, &
            UNKNOWN_UNIT

    interface operator(.safeEq.)
        module procedure safe_eq
    end interface

    double precision, parameter :: MACHINE_EPSILON = epsilon(1.0d0)

    type(message_type_t), parameter :: PARSE_ERROR = &
            message_type_t("Parse Error")
    type(message_type_t), parameter :: UNKNOWN_UNIT = &
            message_type_t("Unknown Unit")
contains
    elemental function effectively_zero(a)
        double precision, intent(in) :: a
        logical :: effectively_zero

        effectively_zero = abs(a) < MACHINE_EPSILON
    end function

    pure function equal_within_absolute(a, b, tolerance)
        double precision, intent(in) :: a
        double precision, intent(in) :: b
        double precision, intent(in) :: tolerance
        logical :: equal_within_absolute

        equal_within_absolute = abs(a - b) < tolerance
    end function

    pure function equal_within_relative(a, b, tolerance)
        double precision, intent(in) :: a
        double precision, intent(in) :: b
        double precision, intent(in) :: tolerance
        logical :: equal_within_relative

        if (effectively_zero(a) .and. effectively_zero(b)) then
            equal_within_relative = .true.
        else
            equal_within_relative = &
                    (abs(a - b) / max(abs(a), abs(b))) < tolerance
        end if
    end function

    function parse_space(state_) result(result_)
        type(state_t), intent(in) :: state_
        type(parser_output_t) :: result_

        result_ = parse_char(" ", state_)
    end function

    elemental function safe_eq(a, b)
        double precision, intent(in) :: a
        double precision, intent(in) :: b
        logical :: safe_eq

        safe_eq = equal_within_relative(a, b, MACHINE_EPSILON)
    end function
end module
