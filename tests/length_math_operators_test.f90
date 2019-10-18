module length_math_operators_test
    implicit none
    private

    public :: &
            test_addition_subtraction_operators, &
            test_multiplication_division_operator
contains
    function test_addition_subtraction_operators() result(tests)
        use DoublePrecisionGenerator_m, only: DOUBLE_PRECISION_GENERATOR
        use DoublePrecisionPairGenerator_m, only: DOUBLE_PRECISION_PAIR_GENERATOR
        use Vegetables_m, only: TestItem_t, Describe, It

        type(TestItem_t) :: tests

        type(TestItem_t) :: individual_tests(3)

        individual_tests(1) = It( &
                "Adding zero returns the original length", &
                DOUBLE_PRECISION_GENERATOR, &
                checkAddZero)
        individual_tests(2) = It( &
                "Subtracting zero returns the original length", &
                DOUBLE_PRECISION_GENERATOR, &
                checkSubtractZero)
        individual_tests(3) = It( &
                "Adding and subtracting the same length returns the original length", &
                DOUBLE_PRECISION_PAIR_GENERATOR, &
                checkAddSubtract)
        tests = Describe("Length_t (+/-) operators", individual_tests)
    end function test_addition_subtraction_operators

    function test_multiplication_division_operator() result(tests)
        use DoublePrecisionGenerator_m, only: DOUBLE_PRECISION_GENERATOR
        use NonZeroDoublePrecisionGenerator_m, only: &
                NON_ZERO_DOUBLE_PRECISION_GENERATOR
        use NonZeroDoublePrecisionPairGenerator_m, only: &
                NON_ZERO_DOUBLE_PRECISION_PAIR_GENERATOR
        use Vegetables_m, only: TestItem_t, Describe, It

        type(TestItem_t) :: tests

        type(TestItem_t) :: individual_tests(6)

        individual_tests(1) = It( &
                "A length multiplied by 1 equals itself", &
                DOUBLE_PRECISION_GENERATOR, &
                checkMultiplyByOne)
        individual_tests(2) = It( &
                "A length multiplied by 0 is 0", &
                DOUBLE_PRECISION_GENERATOR, &
                checkMultiplyByZero)
        individual_tests(3) = It( &
                "A length divided by 1 equals itself", &
                DOUBLE_PRECISION_GENERATOR, &
                checkDivideByOne)
        individual_tests(4) = It( &
                "A length divided by itself equals 1", &
                NON_ZERO_DOUBLE_PRECISION_GENERATOR, &
                checkDivideBySelf)
        individual_tests(5) = It( &
                "Multiplying and dividing by the same number returns the original length", &
                NON_ZERO_DOUBLE_PRECISION_PAIR_GENERATOR, &
                checkMultiplyDivide)
        individual_tests(6) = It( &
                "Dividing and multiplying by the same number returns the original length", &
                NON_ZERO_DOUBLE_PRECISION_PAIR_GENERATOR, &
                checkDivideMultiply)
        tests = Describe("Length_t (* & / ) operators", individual_tests)
    end function test_multiplication_division_operator

    function checkAddZero(input) result(result_)
        use Length_m, only: &
                Length_t, operator(.unit.), METERS
        use Length_asserts_m, only: assertEquals
        use Vegetables_m, only: DoublePrecisionInput_t, Input_t, Result_t, fail

        class(Input_t), intent(in) :: input
        type(Result_t) :: result_

        type(Length_t) :: length
        type(Length_t) :: zero

        select type(input)
        type is (DoublePrecisionInput_t)
            length = input%value_.unit.METERS
            zero = 0.0d0.unit.METERS
            result_ = assertEquals(length, length + zero)
        class default
            result_ = fail("Expected a DoublePrecisionInput_t")
        end select
    end function checkAddZero

    function checkSubtractZero(input) result(result_)
        use Length_m, only: &
                Length_t, operator(.unit.), METERS
        use Length_asserts_m, only: assertEquals
        use Vegetables_m, only: DoublePrecisionInput_t, Input_t, Result_t, fail

        class(Input_t), intent(in) :: input
        type(Result_t) :: result_

        type(Length_t) :: length
        type(Length_t) :: zero

        select type(input)
        type is (DoublePrecisionInput_t)
            length = input%value_.unit.METERS
            zero = 0.0d0.unit.METERS
            result_ = assertEquals(length, length - zero)
        class default
            result_ = fail("Expected a DoublePrecisionInput_t")
        end select
    end function checkSubtractZero

    function checkAddSubtract(input) result(result_)
        use DoublePrecisionPairGenerator_m, only: DoublePrecisionPairInput_t
        use iso_varying_string, only: operator(//)
        use Length_m, only: &
                Length_t, operator(.unit.), METERS
        use Length_asserts_m, only: assertEqualsWithinRelative
        use Vegetables_m, only: DoublePrecisionInput_t, Input_t, Result_t, fail

        class(Input_t), intent(in) :: input
        type(Result_t) :: result_

        type(Length_t) :: length1
        type(Length_t) :: length2

        select type(input)
        type is (DoublePrecisionPairInput_t)
            length1 = input%first.unit.METERS
            length2 = input%second.unit.METERS
            result_ = assertEqualsWithinRelative( &
                    length1, &
                    (length1 + length2) - length2, &
                    1.0d-12, &
                    "length1 = " // length1%toString() // ", length2 = " // length2%toString())
        class default
            result_ = fail("Expected a DoublePrecisionPairInput_t")
        end select
    end function checkAddSubtract

    function checkMultiplyByOne(input) result(result_)
        use Length_m, only: &
                Length_t, operator(.unit.), METERS
        use Length_asserts_m, only: assertEquals
        use Vegetables_m, only: DoublePrecisionInput_t, Input_t, Result_t, fail

        class(Input_t), intent(in) :: input
        type(Result_t) :: result_

        type(Length_t) :: length

        select type(input)
        type is (DoublePrecisionInput_t)
            length = input%value_.unit.METERS
            result_ = assertEquals(length, length * 1.0d0)
        class default
            result_ = fail("Expected a DoublePrecisionInput_t")
        end select
    end function checkMultiplyByOne

    function checkMultiplyByZero(input) result(result_)
        use Length_m, only: &
                Length_t, operator(.unit.), METERS
        use Length_asserts_m, only: assertEquals
        use Vegetables_m, only: DoublePrecisionInput_t, Input_t, Result_t, fail

        class(Input_t), intent(in) :: input
        type(Result_t) :: result_

        type(Length_t) :: length
        type(Length_t) :: zero

        select type(input)
        type is (DoublePrecisionInput_t)
            length = input%value_.unit.METERS
            zero = 0.0d0.unit.METERS
            result_ = assertEquals(zero, length * 0.0d0)
        class default
            result_ = fail("Expected a DoublePrecisionInput_t")
        end select
    end function checkMultiplyByZero

    function checkDivideByOne(input) result(result_)
        use Length_m, only: &
                Length_t, operator(.unit.), METERS
        use Length_asserts_m, only: assertEquals
        use Vegetables_m, only: DoublePrecisionInput_t, Input_t, Result_t, fail

        class(Input_t), intent(in) :: input
        type(Result_t) :: result_

        type(Length_t) :: length

        select type(input)
        type is (DoublePrecisionInput_t)
            length = input%value_.unit.METERS
            result_ = assertEquals(length, length / 1.0d0)
        class default
            result_ = fail("Expected a DoublePrecisionInput_t")
        end select
    end function checkDivideByOne

    function checkDivideBySelf(input) result(result_)
        use Length_m, only: &
                Length_t, operator(.unit.), METERS
        use Vegetables_m, only: &
                DoublePrecisionInput_t, Input_t, Result_t, assertEquals, fail

        class(Input_t), intent(in) :: input
        type(Result_t) :: result_

        type(Length_t) :: length

        select type(input)
        type is (DoublePrecisionInput_t)
            length = input%value_.unit.METERS
            result_ = assertEquals(1.0d0, length / length)
        class default
            result_ = fail("Expected a DoublePrecisionInput_t")
        end select
    end function checkDivideBySelf

    function checkMultiplyDivide(input) result(result_)
        use DoublePrecisionPairGenerator_m, only: DoublePrecisionPairInput_t
        use Length_m, only: &
                Length_t, operator(.unit.), METERS
        use Length_asserts_m, only: assertEquals
        use Vegetables_m, only: Input_t, Result_t, fail

        class(Input_t), intent(in) :: input
        type(Result_t) :: result_

        type(Length_t) :: length

        select type (input)
        type is (DoublePrecisionPairInput_t)
            length = input%first.unit.METERS
            result_ = assertEquals(length, length * input%second / input%second)
        class default
            result_ = fail("Expected a DoublePrecisionPairInput_t")
        end select
    end function checkMultiplyDivide

    function checkDivideMultiply(input) result(result_)
        use DoublePrecisionPairGenerator_m, only: DoublePrecisionPairInput_t
        use Length_m, only: &
                Length_t, operator(.unit.), METERS
        use Length_asserts_m, only: assertEquals
        use Vegetables_m, only: Input_t, Result_t, fail

        class(Input_t), intent(in) :: input
        type(Result_t) :: result_

        type(Length_t) :: length

        select type (input)
        type is (DoublePrecisionPairInput_t)
            length = input%first.unit.METERS
            result_ = assertEquals(length, length / input%second * input%second)
        class default
            result_ = fail("Expected a DoublePrecisionPairInput_t")
        end select
    end function checkDivideMultiply
end module length_math_operators_test
