module Temperature_asserts_m
    use iso_varying_string, only: VARYING_STRING, operator(//), var_str
    use Temperature_m, only: Temperature_t
    use strff, only: toString
    use Vegetables_m, only: &
            Result_t, &
            fail, &
            makeEqualsFailureMessage, &
            makeEqualsSuccessMessage, &
            makeWithinFailureMessage, &
            makeWithinSuccessMessage, &
            succeed, &
            withUserMessage

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
    function assertEqualsBasic(expected, actual) result(result__)
        class(Temperature_t), intent(in) :: expected
        class(Temperature_t), intent(in) :: actual
        type(Result_t) :: result__

        result__ = assertEquals(expected, actual, var_str(""), var_str(""))
    end function assertEqualsBasic

    function assertEqualsWithMessageC( &
            expected, actual, message) result(result__)
        class(Temperature_t), intent(in) :: expected
        class(Temperature_t), intent(in) :: actual
        character(len=*), intent(in) :: message
        type(Result_t) :: result__

        result__ = assertEquals( &
                expected, actual, var_str(message), var_str(message))
    end function assertEqualsWithMessageC

    function assertEqualsWithMessageS( &
            expected, actual, message) result(result__)
        class(Temperature_t), intent(in) :: expected
        class(Temperature_t), intent(in) :: actual
        type(VARYING_STRING), intent(in) :: message
        type(Result_t) :: result__

        result__ = assertEquals( &
                expected, actual, message, message)
    end function assertEqualsWithMessageS

    function assertEqualsWithMessagesCC( &
            expected, actual, success_message, failure_message) result(result__)
        class(Temperature_t), intent(in) :: expected
        class(Temperature_t), intent(in) :: actual
        character(len=*), intent(in) :: success_message
        character(len=*), intent(in) :: failure_message
        type(Result_t) :: result__

        result__ = assertEquals( &
                expected, &
                actual, &
                var_str(success_message), &
                var_str(failure_message))
    end function assertEqualsWithMessagesCC

    function assertEqualsWithMessagesCS( &
            expected, actual, success_message, failure_message) result(result__)
        class(Temperature_t), intent(in) :: expected
        class(Temperature_t), intent(in) :: actual
        character(len=*), intent(in) :: success_message
        type(VARYING_STRING), intent(in) :: failure_message
        type(Result_t) :: result__

        result__ = assertEquals( &
                expected, &
                actual, &
                var_str(success_message), &
                failure_message)
    end function assertEqualsWithMessagesCS

    function assertEqualsWithMessagesSC( &
            expected, actual, success_message, failure_message) result(result__)
        class(Temperature_t), intent(in) :: expected
        class(Temperature_t), intent(in) :: actual
        type(VARYING_STRING), intent(in) :: success_message
        character(len=*), intent(in) :: failure_message
        type(Result_t) :: result__

        result__ = assertEquals( &
                expected, &
                actual, &
                success_message, &
                var_str(failure_message))
    end function assertEqualsWithMessagesSC

    function assertEqualsWithMessagesSS( &
            expected, &
            actual, &
            success_message, &
            failure_message) &
            result(result__)
        class(Temperature_t), intent(in) :: expected
        class(Temperature_t), intent(in) :: actual
        type(VARYING_STRING), intent(in) :: success_message
        type(VARYING_STRING), intent(in) :: failure_message
        type(Result_t) :: result__

        if (expected == actual) then
            result__ = succeed(withUserMessage( &
                    makeEqualsSuccessMessage( &
                            expected%toString()), &
                    success_message))
        else
            result__ = fail(withUserMessage( &
                    makeEqualsFailureMessage( &
                            expected%toString(), &
                            actual%toString()), &
                    failure_message))
        end if
    end function assertEqualsWithMessagesSS

    function assertEqualsWithinAbsoluteBasic( &
            expected, actual, tolerance) result(result__)
        class(Temperature_t), intent(in) :: expected
        class(Temperature_t), intent(in) :: actual
        class(Temperature_t), intent(in) :: tolerance
        type(Result_t) :: result__

        result__ = assertEqualsWithinAbsolute( &
                expected, actual, tolerance, var_str(""), var_str(""))
    end function assertEqualsWithinAbsoluteBasic

    function assertEqualsWithinAbsoluteWithMessageC( &
            expected, actual, tolerance, message) result(result__)
        class(Temperature_t), intent(in) :: expected
        class(Temperature_t), intent(in) :: actual
        class(Temperature_t), intent(in) :: tolerance
        character(len=*), intent(in) :: message
        type(Result_t) :: result__

        result__ = assertEqualsWithinAbsolute( &
                expected, actual, tolerance, var_str(message), var_str(message))
    end function assertEqualsWithinAbsoluteWithMessageC

    function assertEqualsWithinAbsoluteWithMessageS( &
            expected, actual, tolerance, message) result(result__)
        class(Temperature_t), intent(in) :: expected
        class(Temperature_t), intent(in) :: actual
        class(Temperature_t), intent(in) :: tolerance
        type(VARYING_STRING), intent(in) :: message
        type(Result_t) :: result__

        result__ = assertEqualsWithinAbsolute( &
                expected, actual, tolerance, message, message)
    end function assertEqualsWithinAbsoluteWithMessageS

    function assertEqualsWithinAbsoluteWithMessagesCC( &
            expected, &
            actual, &
            tolerance, &
            success_message, &
            failure_message) &
            result(result__)
        class(Temperature_t), intent(in) :: expected
        class(Temperature_t), intent(in) :: actual
        class(Temperature_t), intent(in) :: tolerance
        character(len=*), intent(in) :: success_message
        character(len=*), intent(in) :: failure_message
        type(Result_t) :: result__

        result__ = assertEqualsWithinAbsolute( &
                expected, &
                actual, &
                tolerance, &
                var_str(success_message), &
                var_str(failure_message))
    end function assertEqualsWithinAbsoluteWithMessagesCC

    function assertEqualsWithinAbsoluteWithMessagesCS( &
            expected, &
            actual, &
            tolerance, &
            success_message, &
            failure_message) &
            result(result__)
        class(Temperature_t), intent(in) :: expected
        class(Temperature_t), intent(in) :: actual
        class(Temperature_t), intent(in) :: tolerance
        character(len=*), intent(in) :: success_message
        type(VARYING_STRING), intent(in) :: failure_message
        type(Result_t) :: result__

        result__ = assertEqualsWithinAbsolute( &
                expected, &
                actual, &
                tolerance, &
                var_str(success_message), &
                failure_message)
    end function assertEqualsWithinAbsoluteWithMessagesCS

    function assertEqualsWithinAbsoluteWithMessagesSC( &
            expected, &
            actual, &
            tolerance, &
            success_message, &
            failure_message) &
            result(result__)
        class(Temperature_t), intent(in) :: expected
        class(Temperature_t), intent(in) :: actual
        class(Temperature_t), intent(in) :: tolerance
        type(VARYING_STRING), intent(in) :: success_message
        character(len=*), intent(in) :: failure_message
        type(Result_t) :: result__

        result__ = assertEqualsWithinAbsolute( &
                expected, &
                actual, &
                tolerance, &
                success_message, &
                var_str(failure_message))
    end function assertEqualsWithinAbsoluteWithMessagesSC

    function assertEqualsWithinAbsoluteWithMessagesSS( &
            expected, &
            actual, &
            tolerance, &
            success_message, &
            failure_message) &
            result(result__)
        class(Temperature_t), intent(in) :: expected
        class(Temperature_t), intent(in) :: actual
        class(Temperature_t), intent(in) :: tolerance
        type(VARYING_STRING), intent(in) :: success_message
        type(VARYING_STRING), intent(in) :: failure_message
        type(Result_t) :: result__

        if (expected%equal(actual, within = tolerance)) then
            result__ =  succeed(withUserMessage( &
                    makeWithinSuccessMessage( &
                            expected%toString(), &
                            actual%toString(), &
                            tolerance%toString()), &
                    success_message))
        else
            result__ = fail(withUserMessage( &
                    makeWithinFailureMessage( &
                            expected%toString(), &
                            actual%toString(), &
                            tolerance%toString()), &
                    failure_message))
        end if
    end function assertEqualsWithinAbsoluteWithMessagesSS

    function assertEqualsWithinRelativeBasic( &
            expected, actual, tolerance) result(result__)
        class(Temperature_t), intent(in) :: expected
        class(Temperature_t), intent(in) :: actual
        double precision, intent(in) :: tolerance
        type(Result_t) :: result__

        result__ = assertEqualsWithinRelative( &
                expected, actual, tolerance, var_str(""), var_str(""))
    end function assertEqualsWithinRelativeBasic

    function assertEqualsWithinRelativeWithMessageC( &
            expected, actual, tolerance, message) result(result__)
        class(Temperature_t), intent(in) :: expected
        class(Temperature_t), intent(in) :: actual
        double precision, intent(in) :: tolerance
        character(len=*), intent(in) :: message
        type(Result_t) :: result__

        result__ = assertEqualsWithinRelative( &
                expected, actual, tolerance, var_str(message), var_str(message))
    end function assertEqualsWithinRelativeWithMessageC

    function assertEqualsWithinRelativeWithMessageS( &
            expected, actual, tolerance, message) result(result__)
        class(Temperature_t), intent(in) :: expected
        class(Temperature_t), intent(in) :: actual
        double precision, intent(in) :: tolerance
        type(VARYING_STRING), intent(in) :: message
        type(Result_t) :: result__

        result__ = assertEqualsWithinRelative( &
                expected, actual, tolerance, message, message)
    end function assertEqualsWithinRelativeWithMessageS

    function assertEqualsWithinRelativeWithMessagesCC( &
            expected, &
            actual, &
            tolerance, &
            success_message, &
            failure_message) &
            result(result__)
        class(Temperature_t), intent(in) :: expected
        class(Temperature_t), intent(in) :: actual
        double precision, intent(in) :: tolerance
        character(len=*), intent(in) :: success_message
        character(len=*), intent(in) :: failure_message
        type(Result_t) :: result__

        result__ = assertEqualsWithinRelative( &
                expected, &
                actual, &
                tolerance, &
                var_str(success_message), &
                var_str(failure_message))
    end function assertEqualsWithinRelativeWithMessagesCC

    function assertEqualsWithinRelativeWithMessagesCS( &
            expected, &
            actual, &
            tolerance, &
            success_message, &
            failure_message) &
            result(result__)
        class(Temperature_t), intent(in) :: expected
        class(Temperature_t), intent(in) :: actual
        double precision, intent(in) :: tolerance
        character(len=*), intent(in) :: success_message
        type(VARYING_STRING), intent(in) :: failure_message
        type(Result_t) :: result__

        result__ = assertEqualsWithinRelative( &
                expected, &
                actual, &
                tolerance, &
                var_str(success_message), &
                failure_message)
    end function assertEqualsWithinRelativeWithMessagesCS

    function assertEqualsWithinRelativeWithMessagesSC( &
            expected, &
            actual, &
            tolerance, &
            success_message, &
            failure_message) &
            result(result__)
        class(Temperature_t), intent(in) :: expected
        class(Temperature_t), intent(in) :: actual
        double precision, intent(in) :: tolerance
        type(VARYING_STRING), intent(in) :: success_message
        character(len=*), intent(in) :: failure_message
        type(Result_t) :: result__

        result__ = assertEqualsWithinRelative( &
                expected, &
                actual, &
                tolerance, &
                success_message, &
                var_str(failure_message))
    end function assertEqualsWithinRelativeWithMessagesSC

    function assertEqualsWithinRelativeWithMessagesSS( &
            expected, &
            actual, &
            tolerance, &
            success_message, &
            failure_message) &
            result(result__)
        class(Temperature_t), intent(in) :: expected
        class(Temperature_t), intent(in) :: actual
        double precision, intent(in) :: tolerance
        type(VARYING_STRING), intent(in) :: success_message
        type(VARYING_STRING), intent(in) :: failure_message
        type(Result_t) :: result__

        if (expected%equal(actual, within = tolerance)) then
            result__ =  succeed(withUserMessage( &
                    makeWithinSuccessMessage( &
                            expected%toString(), &
                            actual%toString(), &
                            toString(tolerance * 100.0d0) // "%"), &
                    success_message))
        else
            result__ = fail(withUserMessage( &
                    makeWithinFailureMessage( &
                            expected%toString(), &
                            actual%toString(), &
                            toString(tolerance * 100.0d0) // "%"), &
                    failure_message))
        end if
    end function assertEqualsWithinRelativeWithMessagesSS
end module Temperature_asserts_m
