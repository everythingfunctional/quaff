module Length_asserts_m
    use iso_varying_string, only: varying_string, operator(//), var_str
    use quaff, only: Length_t
    use strff, only: to_string
    use vegetables, only: &
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

    interface assertEquals
        module procedure assertEqualsBasic
        module procedure assertEqualsWithMessageC
        module procedure assertEqualsWithMessageS
        module procedure assertEqualsWithMessagesCC
        module procedure assertEqualsWithMessagesCS
        module procedure assertEqualsWithMessagesSC
        module procedure assertEqualsWithMessagesSS
    end interface

    interface assertEqualsWithinAbsolute
        module procedure assertEqualsWithinAbsoluteBasic
        module procedure assertEqualsWithinAbsoluteWithMessageC
        module procedure assertEqualsWithinAbsoluteWithMessageS
        module procedure assertEqualsWithinAbsoluteWithMessagesCC
        module procedure assertEqualsWithinAbsoluteWithMessagesCS
        module procedure assertEqualsWithinAbsoluteWithMessagesSC
        module procedure assertEqualsWithinAbsoluteWithMessagesSS
    end interface

    interface assertEqualsWithinRelative
        module procedure assertEqualsWithinRelativeBasic
        module procedure assertEqualsWithinRelativeWithMessageC
        module procedure assertEqualsWithinRelativeWithMessageS
        module procedure assertEqualsWithinRelativeWithMessagesCC
        module procedure assertEqualsWithinRelativeWithMessagesCS
        module procedure assertEqualsWithinRelativeWithMessagesSC
        module procedure assertEqualsWithinRelativeWithMessagesSS
    end interface

    public :: assertEquals, assertEqualsWithinAbsolute, assertEqualsWithinRelative
contains
    pure function assertEqualsBasic(expected, actual) result(result__)
        class(Length_t), intent(in) :: expected
        class(Length_t), intent(in) :: actual
        type(result_t) :: result__

        result__ = assertEquals(expected, actual, var_str(""), var_str(""))
    end function assertEqualsBasic

    pure function assertEqualsWithMessageC( &
            expected, actual, message) result(result__)
        class(Length_t), intent(in) :: expected
        class(Length_t), intent(in) :: actual
        character(len=*), intent(in) :: message
        type(result_t) :: result__

        result__ = assertEquals( &
                expected, actual, var_str(message), var_str(message))
    end function assertEqualsWithMessageC

    pure function assertEqualsWithMessageS( &
            expected, actual, message) result(result__)
        class(Length_t), intent(in) :: expected
        class(Length_t), intent(in) :: actual
        type(varying_string), intent(in) :: message
        type(result_t) :: result__

        result__ = assertEquals( &
                expected, actual, message, message)
    end function assertEqualsWithMessageS

    pure function assertEqualsWithMessagesCC( &
            expected, actual, success_message, failure_message) result(result__)
        class(Length_t), intent(in) :: expected
        class(Length_t), intent(in) :: actual
        character(len=*), intent(in) :: success_message
        character(len=*), intent(in) :: failure_message
        type(result_t) :: result__

        result__ = assertEquals( &
                expected, &
                actual, &
                var_str(success_message), &
                var_str(failure_message))
    end function assertEqualsWithMessagesCC

    pure function assertEqualsWithMessagesCS( &
            expected, actual, success_message, failure_message) result(result__)
        class(Length_t), intent(in) :: expected
        class(Length_t), intent(in) :: actual
        character(len=*), intent(in) :: success_message
        type(varying_string), intent(in) :: failure_message
        type(result_t) :: result__

        result__ = assertEquals( &
                expected, &
                actual, &
                var_str(success_message), &
                failure_message)
    end function assertEqualsWithMessagesCS

    pure function assertEqualsWithMessagesSC( &
            expected, actual, success_message, failure_message) result(result__)
        class(Length_t), intent(in) :: expected
        class(Length_t), intent(in) :: actual
        type(varying_string), intent(in) :: success_message
        character(len=*), intent(in) :: failure_message
        type(result_t) :: result__

        result__ = assertEquals( &
                expected, &
                actual, &
                success_message, &
                var_str(failure_message))
    end function assertEqualsWithMessagesSC

    pure function assertEqualsWithMessagesSS( &
            expected, &
            actual, &
            success_message, &
            failure_message) &
            result(result__)
        class(Length_t), intent(in) :: expected
        class(Length_t), intent(in) :: actual
        type(varying_string), intent(in) :: success_message
        type(varying_string), intent(in) :: failure_message
        type(result_t) :: result__

        if (expected == actual) then
            result__ = succeed(with_user_message( &
                    make_equals_success_message( &
                            expected%toString()), &
                    success_message))
        else
            result__ = fail(with_user_message( &
                    make_equals_failure_message( &
                            expected%toString(), &
                            actual%toString()), &
                    failure_message))
        end if
    end function assertEqualsWithMessagesSS

    pure function assertEqualsWithinAbsoluteBasic( &
            expected, actual, tolerance) result(result__)
        class(Length_t), intent(in) :: expected
        class(Length_t), intent(in) :: actual
        class(Length_t), intent(in) :: tolerance
        type(result_t) :: result__

        result__ = assertEqualsWithinAbsolute( &
                expected, actual, tolerance, var_str(""), var_str(""))
    end function assertEqualsWithinAbsoluteBasic

    pure function assertEqualsWithinAbsoluteWithMessageC( &
            expected, actual, tolerance, message) result(result__)
        class(Length_t), intent(in) :: expected
        class(Length_t), intent(in) :: actual
        class(Length_t), intent(in) :: tolerance
        character(len=*), intent(in) :: message
        type(result_t) :: result__

        result__ = assertEqualsWithinAbsolute( &
                expected, actual, tolerance, var_str(message), var_str(message))
    end function assertEqualsWithinAbsoluteWithMessageC

    pure function assertEqualsWithinAbsoluteWithMessageS( &
            expected, actual, tolerance, message) result(result__)
        class(Length_t), intent(in) :: expected
        class(Length_t), intent(in) :: actual
        class(Length_t), intent(in) :: tolerance
        type(varying_string), intent(in) :: message
        type(result_t) :: result__

        result__ = assertEqualsWithinAbsolute( &
                expected, actual, tolerance, message, message)
    end function assertEqualsWithinAbsoluteWithMessageS

    pure function assertEqualsWithinAbsoluteWithMessagesCC( &
            expected, &
            actual, &
            tolerance, &
            success_message, &
            failure_message) &
            result(result__)
        class(Length_t), intent(in) :: expected
        class(Length_t), intent(in) :: actual
        class(Length_t), intent(in) :: tolerance
        character(len=*), intent(in) :: success_message
        character(len=*), intent(in) :: failure_message
        type(result_t) :: result__

        result__ = assertEqualsWithinAbsolute( &
                expected, &
                actual, &
                tolerance, &
                var_str(success_message), &
                var_str(failure_message))
    end function assertEqualsWithinAbsoluteWithMessagesCC

    pure function assertEqualsWithinAbsoluteWithMessagesCS( &
            expected, &
            actual, &
            tolerance, &
            success_message, &
            failure_message) &
            result(result__)
        class(Length_t), intent(in) :: expected
        class(Length_t), intent(in) :: actual
        class(Length_t), intent(in) :: tolerance
        character(len=*), intent(in) :: success_message
        type(varying_string), intent(in) :: failure_message
        type(result_t) :: result__

        result__ = assertEqualsWithinAbsolute( &
                expected, &
                actual, &
                tolerance, &
                var_str(success_message), &
                failure_message)
    end function assertEqualsWithinAbsoluteWithMessagesCS

    pure function assertEqualsWithinAbsoluteWithMessagesSC( &
            expected, &
            actual, &
            tolerance, &
            success_message, &
            failure_message) &
            result(result__)
        class(Length_t), intent(in) :: expected
        class(Length_t), intent(in) :: actual
        class(Length_t), intent(in) :: tolerance
        type(varying_string), intent(in) :: success_message
        character(len=*), intent(in) :: failure_message
        type(result_t) :: result__

        result__ = assertEqualsWithinAbsolute( &
                expected, &
                actual, &
                tolerance, &
                success_message, &
                var_str(failure_message))
    end function assertEqualsWithinAbsoluteWithMessagesSC

    pure function assertEqualsWithinAbsoluteWithMessagesSS( &
            expected, &
            actual, &
            tolerance, &
            success_message, &
            failure_message) &
            result(result__)
        class(Length_t), intent(in) :: expected
        class(Length_t), intent(in) :: actual
        class(Length_t), intent(in) :: tolerance
        type(varying_string), intent(in) :: success_message
        type(varying_string), intent(in) :: failure_message
        type(result_t) :: result__

        if (expected%equal(actual, within = tolerance)) then
            result__ =  succeed(with_user_message( &
                    make_within_success_message( &
                            expected%toString(), &
                            actual%toString(), &
                            tolerance%toString()), &
                    success_message))
        else
            result__ = fail(with_user_message( &
                    make_within_failure_message( &
                            expected%toString(), &
                            actual%toString(), &
                            tolerance%toString()), &
                    failure_message))
        end if
    end function assertEqualsWithinAbsoluteWithMessagesSS

    pure function assertEqualsWithinRelativeBasic( &
            expected, actual, tolerance) result(result__)
        class(Length_t), intent(in) :: expected
        class(Length_t), intent(in) :: actual
        double precision, intent(in) :: tolerance
        type(result_t) :: result__

        result__ = assertEqualsWithinRelative( &
                expected, actual, tolerance, var_str(""), var_str(""))
    end function assertEqualsWithinRelativeBasic

    pure function assertEqualsWithinRelativeWithMessageC( &
            expected, actual, tolerance, message) result(result__)
        class(Length_t), intent(in) :: expected
        class(Length_t), intent(in) :: actual
        double precision, intent(in) :: tolerance
        character(len=*), intent(in) :: message
        type(result_t) :: result__

        result__ = assertEqualsWithinRelative( &
                expected, actual, tolerance, var_str(message), var_str(message))
    end function assertEqualsWithinRelativeWithMessageC

    pure function assertEqualsWithinRelativeWithMessageS( &
            expected, actual, tolerance, message) result(result__)
        class(Length_t), intent(in) :: expected
        class(Length_t), intent(in) :: actual
        double precision, intent(in) :: tolerance
        type(varying_string), intent(in) :: message
        type(result_t) :: result__

        result__ = assertEqualsWithinRelative( &
                expected, actual, tolerance, message, message)
    end function assertEqualsWithinRelativeWithMessageS

    pure function assertEqualsWithinRelativeWithMessagesCC( &
            expected, &
            actual, &
            tolerance, &
            success_message, &
            failure_message) &
            result(result__)
        class(Length_t), intent(in) :: expected
        class(Length_t), intent(in) :: actual
        double precision, intent(in) :: tolerance
        character(len=*), intent(in) :: success_message
        character(len=*), intent(in) :: failure_message
        type(result_t) :: result__

        result__ = assertEqualsWithinRelative( &
                expected, &
                actual, &
                tolerance, &
                var_str(success_message), &
                var_str(failure_message))
    end function assertEqualsWithinRelativeWithMessagesCC

    pure function assertEqualsWithinRelativeWithMessagesCS( &
            expected, &
            actual, &
            tolerance, &
            success_message, &
            failure_message) &
            result(result__)
        class(Length_t), intent(in) :: expected
        class(Length_t), intent(in) :: actual
        double precision, intent(in) :: tolerance
        character(len=*), intent(in) :: success_message
        type(varying_string), intent(in) :: failure_message
        type(result_t) :: result__

        result__ = assertEqualsWithinRelative( &
                expected, &
                actual, &
                tolerance, &
                var_str(success_message), &
                failure_message)
    end function assertEqualsWithinRelativeWithMessagesCS

    pure function assertEqualsWithinRelativeWithMessagesSC( &
            expected, &
            actual, &
            tolerance, &
            success_message, &
            failure_message) &
            result(result__)
        class(Length_t), intent(in) :: expected
        class(Length_t), intent(in) :: actual
        double precision, intent(in) :: tolerance
        type(varying_string), intent(in) :: success_message
        character(len=*), intent(in) :: failure_message
        type(result_t) :: result__

        result__ = assertEqualsWithinRelative( &
                expected, &
                actual, &
                tolerance, &
                success_message, &
                var_str(failure_message))
    end function assertEqualsWithinRelativeWithMessagesSC

    pure function assertEqualsWithinRelativeWithMessagesSS( &
            expected, &
            actual, &
            tolerance, &
            success_message, &
            failure_message) &
            result(result__)
        class(Length_t), intent(in) :: expected
        class(Length_t), intent(in) :: actual
        double precision, intent(in) :: tolerance
        type(varying_string), intent(in) :: success_message
        type(varying_string), intent(in) :: failure_message
        type(result_t) :: result__

        if (expected%equal(actual, within = tolerance)) then
            result__ =  succeed(with_user_message( &
                    make_within_success_message( &
                            expected%toString(), &
                            actual%toString(), &
                            to_string(tolerance * 100.0d0) // "%"), &
                    success_message))
        else
            result__ = fail(with_user_message( &
                    make_within_failure_message( &
                            expected%toString(), &
                            actual%toString(), &
                            to_string(tolerance * 100.0d0) // "%"), &
                    failure_message))
        end if
    end function assertEqualsWithinRelativeWithMessagesSS
end module Length_asserts_m
