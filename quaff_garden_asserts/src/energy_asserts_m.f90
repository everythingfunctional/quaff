module energy_asserts_m
    use iso_varying_string, only: varying_string, operator(//), var_str
    use quaff, only: energy_t
    use strff, only: to_string
    use garden, only: &
            result_t, &
            fail, &
            make_equals_failure_message, &
            make_equals_success_message, &
            make_within_failure_message, &
            make_within_success_message, &
            succeed, &
            with_user_message

    implicit none
    private
    public :: &
            assert_equals, &
            assert_equals_within_absolute, &
            assert_equals_within_relative

    interface assert_equals
        module procedure assert_equals_basic
        module procedure assert_equals_with_message_c
        module procedure assert_equals_with_message_s
        module procedure assert_equals_with_messages_cc
        module procedure assert_equals_with_messages_cs
        module procedure assert_equals_with_messages_sc
        module procedure assert_equals_with_messages_ss
    end interface

    interface assert_equals_within_absolute
        module procedure assert_equals_within_absolute_basic
        module procedure assert_equals_within_absolute_with_message_c
        module procedure assert_equals_within_absolute_with_message_s
        module procedure assert_equals_within_absolute_with_messages_cc
        module procedure assert_equals_within_absolute_with_messages_cs
        module procedure assert_equals_within_absolute_with_messages_sc
        module procedure assert_equals_within_absolute_with_messages_ss
    end interface

    interface assert_equals_within_relative
        module procedure assert_equals_within_relative_basic
        module procedure assert_equals_within_relative_with_message_c
        module procedure assert_equals_within_relative_with_message_s
        module procedure assert_equals_within_relative_with_messages_cc
        module procedure assert_equals_within_relative_with_messages_cs
        module procedure assert_equals_within_relative_with_messages_sc
        module procedure assert_equals_within_relative_with_messages_ss
    end interface
contains
    pure function assert_equals_basic(expected, actual) result(result_)
        type(energy_t), intent(in) :: expected
        type(energy_t), intent(in) :: actual
        type(result_t) :: result_

        result_ = assert_equals(expected, actual, var_str(""), var_str(""))
    end function

    pure function assert_equals_with_message_c( &
            expected, actual, message) result(result_)
        type(energy_t), intent(in) :: expected
        type(energy_t), intent(in) :: actual
        character(len=*), intent(in) :: message
        type(result_t) :: result_

        result_ = assert_equals( &
                expected, actual, var_str(message), var_str(message))
    end function

    pure function assert_equals_with_message_s( &
            expected, actual, message) result(result_)
        type(energy_t), intent(in) :: expected
        type(energy_t), intent(in) :: actual
        type(varying_string), intent(in) :: message
        type(result_t) :: result_

        result_ = assert_equals( &
                expected, actual, message, message)
    end function

    pure function assert_equals_with_messages_cc( &
            expected, actual, success_message, failure_message) result(result_)
        type(energy_t), intent(in) :: expected
        type(energy_t), intent(in) :: actual
        character(len=*), intent(in) :: success_message
        character(len=*), intent(in) :: failure_message
        type(result_t) :: result_

        result_ = assert_equals( &
                expected, &
                actual, &
                var_str(success_message), &
                var_str(failure_message))
    end function

    pure function assert_equals_with_messages_cs( &
            expected, actual, success_message, failure_message) result(result_)
        type(energy_t), intent(in) :: expected
        type(energy_t), intent(in) :: actual
        character(len=*), intent(in) :: success_message
        type(varying_string), intent(in) :: failure_message
        type(result_t) :: result_

        result_ = assert_equals( &
                expected, &
                actual, &
                var_str(success_message), &
                failure_message)
    end function

    pure function assert_equals_with_messages_sc( &
            expected, actual, success_message, failure_message) result(result_)
        type(energy_t), intent(in) :: expected
        type(energy_t), intent(in) :: actual
        type(varying_string), intent(in) :: success_message
        character(len=*), intent(in) :: failure_message
        type(result_t) :: result_

        result_ = assert_equals( &
                expected, &
                actual, &
                success_message, &
                var_str(failure_message))
    end function

    pure function assert_equals_with_messages_ss( &
            expected, &
            actual, &
            success_message, &
            failure_message) &
            result(result_)
        type(energy_t), intent(in) :: expected
        type(energy_t), intent(in) :: actual
        type(varying_string), intent(in) :: success_message
        type(varying_string), intent(in) :: failure_message
        type(result_t) :: result_

        if (expected == actual) then
            result_ = succeed(with_user_message( &
                    make_equals_success_message( &
                            expected%to_string()), &
                    success_message))
        else
            result_ = fail(with_user_message( &
                    make_equals_failure_message( &
                            expected%to_string(), &
                            actual%to_string()), &
                    failure_message))
        end if
    end function

    pure function assert_equals_within_absolute_basic( &
            expected, actual, tolerance) result(result_)
        type(energy_t), intent(in) :: expected
        type(energy_t), intent(in) :: actual
        type(energy_t), intent(in) :: tolerance
        type(result_t) :: result_

        result_ = assert_equals_within_absolute( &
                expected, actual, tolerance, var_str(""), var_str(""))
    end function

    pure function assert_equals_within_absolute_with_message_c( &
            expected, actual, tolerance, message) result(result_)
        type(energy_t), intent(in) :: expected
        type(energy_t), intent(in) :: actual
        type(energy_t), intent(in) :: tolerance
        character(len=*), intent(in) :: message
        type(result_t) :: result_

        result_ = assert_equals_within_absolute( &
                expected, actual, tolerance, var_str(message), var_str(message))
    end function

    pure function assert_equals_within_absolute_with_message_s( &
            expected, actual, tolerance, message) result(result_)
        type(energy_t), intent(in) :: expected
        type(energy_t), intent(in) :: actual
        type(energy_t), intent(in) :: tolerance
        type(varying_string), intent(in) :: message
        type(result_t) :: result_

        result_ = assert_equals_within_absolute( &
                expected, actual, tolerance, message, message)
    end function

    pure function assert_equals_within_absolute_with_messages_cc( &
            expected, &
            actual, &
            tolerance, &
            success_message, &
            failure_message) &
            result(result_)
        type(energy_t), intent(in) :: expected
        type(energy_t), intent(in) :: actual
        type(energy_t), intent(in) :: tolerance
        character(len=*), intent(in) :: success_message
        character(len=*), intent(in) :: failure_message
        type(result_t) :: result_

        result_ = assert_equals_within_absolute( &
                expected, &
                actual, &
                tolerance, &
                var_str(success_message), &
                var_str(failure_message))
    end function

    pure function assert_equals_within_absolute_with_messages_cs( &
            expected, &
            actual, &
            tolerance, &
            success_message, &
            failure_message) &
            result(result_)
        type(energy_t), intent(in) :: expected
        type(energy_t), intent(in) :: actual
        type(energy_t), intent(in) :: tolerance
        character(len=*), intent(in) :: success_message
        type(varying_string), intent(in) :: failure_message
        type(result_t) :: result_

        result_ = assert_equals_within_absolute( &
                expected, &
                actual, &
                tolerance, &
                var_str(success_message), &
                failure_message)
    end function

    pure function assert_equals_within_absolute_with_messages_sc( &
            expected, &
            actual, &
            tolerance, &
            success_message, &
            failure_message) &
            result(result_)
        type(energy_t), intent(in) :: expected
        type(energy_t), intent(in) :: actual
        type(energy_t), intent(in) :: tolerance
        type(varying_string), intent(in) :: success_message
        character(len=*), intent(in) :: failure_message
        type(result_t) :: result_

        result_ = assert_equals_within_absolute( &
                expected, &
                actual, &
                tolerance, &
                success_message, &
                var_str(failure_message))
    end function

    pure function assert_equals_within_absolute_with_messages_ss( &
            expected, &
            actual, &
            tolerance, &
            success_message, &
            failure_message) &
            result(result_)
        type(energy_t), intent(in) :: expected
        type(energy_t), intent(in) :: actual
        type(energy_t), intent(in) :: tolerance
        type(varying_string), intent(in) :: success_message
        type(varying_string), intent(in) :: failure_message
        type(result_t) :: result_

        if (expected%equal(actual, within = tolerance)) then
            result_ =  succeed(with_user_message( &
                    make_within_success_message( &
                            expected%to_string(), &
                            actual%to_string(), &
                            tolerance%to_string()), &
                    success_message))
        else
            result_ = fail(with_user_message( &
                    make_within_failure_message( &
                            expected%to_string(), &
                            actual%to_string(), &
                            tolerance%to_string()), &
                    failure_message))
        end if
    end function

    pure function assert_equals_within_relative_basic( &
            expected, actual, tolerance) result(result_)
        type(energy_t), intent(in) :: expected
        type(energy_t), intent(in) :: actual
        double precision, intent(in) :: tolerance
        type(result_t) :: result_

        result_ = assert_equals_within_relative( &
                expected, actual, tolerance, var_str(""), var_str(""))
    end function

    pure function assert_equals_within_relative_with_message_c( &
            expected, actual, tolerance, message) result(result_)
        type(energy_t), intent(in) :: expected
        type(energy_t), intent(in) :: actual
        double precision, intent(in) :: tolerance
        character(len=*), intent(in) :: message
        type(result_t) :: result_

        result_ = assert_equals_within_relative( &
                expected, actual, tolerance, var_str(message), var_str(message))
    end function

    pure function assert_equals_within_relative_with_message_s( &
            expected, actual, tolerance, message) result(result_)
        type(energy_t), intent(in) :: expected
        type(energy_t), intent(in) :: actual
        double precision, intent(in) :: tolerance
        type(varying_string), intent(in) :: message
        type(result_t) :: result_

        result_ = assert_equals_within_relative( &
                expected, actual, tolerance, message, message)
    end function

    pure function assert_equals_within_relative_with_messages_cc( &
            expected, &
            actual, &
            tolerance, &
            success_message, &
            failure_message) &
            result(result_)
        type(energy_t), intent(in) :: expected
        type(energy_t), intent(in) :: actual
        double precision, intent(in) :: tolerance
        character(len=*), intent(in) :: success_message
        character(len=*), intent(in) :: failure_message
        type(result_t) :: result_

        result_ = assert_equals_within_relative( &
                expected, &
                actual, &
                tolerance, &
                var_str(success_message), &
                var_str(failure_message))
    end function

    pure function assert_equals_within_relative_with_messages_cs( &
            expected, &
            actual, &
            tolerance, &
            success_message, &
            failure_message) &
            result(result_)
        type(energy_t), intent(in) :: expected
        type(energy_t), intent(in) :: actual
        double precision, intent(in) :: tolerance
        character(len=*), intent(in) :: success_message
        type(varying_string), intent(in) :: failure_message
        type(result_t) :: result_

        result_ = assert_equals_within_relative( &
                expected, &
                actual, &
                tolerance, &
                var_str(success_message), &
                failure_message)
    end function

    pure function assert_equals_within_relative_with_messages_sc( &
            expected, &
            actual, &
            tolerance, &
            success_message, &
            failure_message) &
            result(result_)
        type(energy_t), intent(in) :: expected
        type(energy_t), intent(in) :: actual
        double precision, intent(in) :: tolerance
        type(varying_string), intent(in) :: success_message
        character(len=*), intent(in) :: failure_message
        type(result_t) :: result_

        result_ = assert_equals_within_relative( &
                expected, &
                actual, &
                tolerance, &
                success_message, &
                var_str(failure_message))
    end function

    pure function assert_equals_within_relative_with_messages_ss( &
            expected, &
            actual, &
            tolerance, &
            success_message, &
            failure_message) &
            result(result_)
        type(energy_t), intent(in) :: expected
        type(energy_t), intent(in) :: actual
        double precision, intent(in) :: tolerance
        type(varying_string), intent(in) :: success_message
        type(varying_string), intent(in) :: failure_message
        type(result_t) :: result_

        if (expected%equal(actual, within = tolerance)) then
            result_ =  succeed(with_user_message( &
                    make_within_success_message( &
                            expected%to_string(), &
                            actual%to_string(), &
                            to_string(tolerance * 100.0d0) // "%"), &
                    success_message))
        else
            result_ = fail(with_user_message( &
                    make_within_failure_message( &
                            expected%to_string(), &
                            actual%to_string(), &
                            to_string(tolerance * 100.0d0) // "%"), &
                    failure_message))
        end if
    end function
end module
