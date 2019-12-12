module acceleration_math_operators_test
    use DoublePrecisionGenerator_m, only: DOUBLE_PRECISION_GENERATOR
    use DoublePrecisionPairGenerator_m, only: &
            DoublePrecisionPairInput_t, DOUBLE_PRECISION_PAIR_GENERATOR
    use iso_varying_string, only: operator(//)
    use NonZeroDoublePrecisionGenerator_m, only: &
            NON_ZERO_DOUBLE_PRECISION_GENERATOR
    use NonZeroDoublePrecisionPairGenerator_m, only: &
            NON_ZERO_DOUBLE_PRECISION_PAIR_GENERATOR
    use quaff, only: &
            Acceleration_t, operator(.unit.), METERS_PER_SQUARE_SECOND
    use quaff_asserts_m, only: &
            assertEquals, assertEqualsWithinRelative
    use Vegetables_m, only: &
            DoublePrecisionInput_t, &
            Input_t, &
            Result_t, &
            TestItem_t, &
            assertEquals, &
            Describe, &
            fail, &
            It

    implicit none
    private

    public :: &
            test_addition_subtraction_operators, &
            test_multiplication_division_operator
contains
    function test_addition_subtraction_operators() result(tests)
        type(TestItem_t) :: tests

        type(TestItem_t) :: individual_tests(3)

        individual_tests(1) = It( &
                "Adding zero returns the original acceleration", &
                DOUBLE_PRECISION_GENERATOR, &
                checkAddZero)
        individual_tests(2) = It( &
                "Subtracting zero returns the original acceleration", &
                DOUBLE_PRECISION_GENERATOR, &
                checkSubtractZero)
        individual_tests(3) = It( &
                "Adding and subtracting the same acceleration returns the original acceleration", &
                DOUBLE_PRECISION_PAIR_GENERATOR, &
                checkAddSubtract)
        tests = Describe("Acceleration_t (+/-) operators", individual_tests)
    end function test_addition_subtraction_operators

    function test_multiplication_division_operator() result(tests)
        type(TestItem_t) :: tests

        type(TestItem_t) :: individual_tests(6)

        individual_tests(1) = It( &
                "A acceleration multiplied by 1 equals itself", &
                DOUBLE_PRECISION_GENERATOR, &
                checkMultiplyByOne)
        individual_tests(2) = It( &
                "A acceleration multiplied by 0 is 0", &
                DOUBLE_PRECISION_GENERATOR, &
                checkMultiplyByZero)
        individual_tests(3) = It( &
                "A acceleration divided by 1 equals itself", &
                DOUBLE_PRECISION_GENERATOR, &
                checkDivideByOne)
        individual_tests(4) = It( &
                "A acceleration divided by itself equals 1", &
                NON_ZERO_DOUBLE_PRECISION_GENERATOR, &
                checkDivideBySelf)
        individual_tests(5) = It( &
                "Multiplying and dividing by the same number returns the original acceleration", &
                NON_ZERO_DOUBLE_PRECISION_PAIR_GENERATOR, &
                checkMultiplyDivide)
        individual_tests(6) = It( &
                "Dividing and multiplying by the same number returns the original acceleration", &
                NON_ZERO_DOUBLE_PRECISION_PAIR_GENERATOR, &
                checkDivideMultiply)
        tests = Describe("Acceleration_t (* & / ) operators", individual_tests)
    end function test_multiplication_division_operator

    pure function checkAddZero(input) result(result_)
        class(Input_t), intent(in) :: input
        type(Result_t) :: result_

        type(Acceleration_t) :: acceleration
        type(Acceleration_t) :: zero

        select type(input)
        type is (DoublePrecisionInput_t)
            acceleration = input%value_.unit.METERS_PER_SQUARE_SECOND
            zero = 0.0d0.unit.METERS_PER_SQUARE_SECOND
            result_ = assertEquals(acceleration, acceleration + zero)
        class default
            result_ = fail("Expected a DoublePrecisionInput_t")
        end select
    end function checkAddZero

    pure function checkSubtractZero(input) result(result_)
        class(Input_t), intent(in) :: input
        type(Result_t) :: result_

        type(Acceleration_t) :: acceleration
        type(Acceleration_t) :: zero

        select type(input)
        type is (DoublePrecisionInput_t)
            acceleration = input%value_.unit.METERS_PER_SQUARE_SECOND
            zero = 0.0d0.unit.METERS_PER_SQUARE_SECOND
            result_ = assertEquals(acceleration, acceleration - zero)
        class default
            result_ = fail("Expected a DoublePrecisionInput_t")
        end select
    end function checkSubtractZero

    pure function checkAddSubtract(input) result(result_)
        class(Input_t), intent(in) :: input
        type(Result_t) :: result_

        type(Acceleration_t) :: acceleration1
        type(Acceleration_t) :: acceleration2

        select type(input)
        type is (DoublePrecisionPairInput_t)
            acceleration1 = input%first.unit.METERS_PER_SQUARE_SECOND
            acceleration2 = input%second.unit.METERS_PER_SQUARE_SECOND
            result_ = assertEqualsWithinRelative( &
                    acceleration1, &
                    (acceleration1 + acceleration2) - acceleration2, &
                    1.0d-8, &
                    "acceleration1 = " // acceleration1%toString() // ", acceleration2 = " // acceleration2%toString())
        class default
            result_ = fail("Expected a DoublePrecisionPairInput_t")
        end select
    end function checkAddSubtract

    pure function checkMultiplyByOne(input) result(result_)
        class(Input_t), intent(in) :: input
        type(Result_t) :: result_

        type(Acceleration_t) :: acceleration

        select type(input)
        type is (DoublePrecisionInput_t)
            acceleration = input%value_.unit.METERS_PER_SQUARE_SECOND
            result_ = assertEquals(acceleration, acceleration * 1.0d0)
        class default
            result_ = fail("Expected a DoublePrecisionInput_t")
        end select
    end function checkMultiplyByOne

    pure function checkMultiplyByZero(input) result(result_)
        class(Input_t), intent(in) :: input
        type(Result_t) :: result_

        type(Acceleration_t) :: acceleration
        type(Acceleration_t) :: zero

        select type(input)
        type is (DoublePrecisionInput_t)
            acceleration = input%value_.unit.METERS_PER_SQUARE_SECOND
            zero = 0.0d0.unit.METERS_PER_SQUARE_SECOND
            result_ = assertEquals(zero, acceleration * 0.0d0)
        class default
            result_ = fail("Expected a DoublePrecisionInput_t")
        end select
    end function checkMultiplyByZero

    pure function checkDivideByOne(input) result(result_)
        class(Input_t), intent(in) :: input
        type(Result_t) :: result_

        type(Acceleration_t) :: acceleration

        select type(input)
        type is (DoublePrecisionInput_t)
            acceleration = input%value_.unit.METERS_PER_SQUARE_SECOND
            result_ = assertEquals(acceleration, acceleration / 1.0d0)
        class default
            result_ = fail("Expected a DoublePrecisionInput_t")
        end select
    end function checkDivideByOne

    pure function checkDivideBySelf(input) result(result_)
        class(Input_t), intent(in) :: input
        type(Result_t) :: result_

        type(Acceleration_t) :: acceleration

        select type(input)
        type is (DoublePrecisionInput_t)
            acceleration = input%value_.unit.METERS_PER_SQUARE_SECOND
            result_ = assertEquals(1.0d0, acceleration / acceleration)
        class default
            result_ = fail("Expected a DoublePrecisionInput_t")
        end select
    end function checkDivideBySelf

    pure function checkMultiplyDivide(input) result(result_)
        class(Input_t), intent(in) :: input
        type(Result_t) :: result_

        type(Acceleration_t) :: acceleration

        select type (input)
        type is (DoublePrecisionPairInput_t)
            acceleration = input%first.unit.METERS_PER_SQUARE_SECOND
            result_ = assertEquals(acceleration, acceleration * input%second / input%second)
        class default
            result_ = fail("Expected a DoublePrecisionPairInput_t")
        end select
    end function checkMultiplyDivide

    pure function checkDivideMultiply(input) result(result_)
        class(Input_t), intent(in) :: input
        type(Result_t) :: result_

        type(Acceleration_t) :: acceleration

        select type (input)
        type is (DoublePrecisionPairInput_t)
            acceleration = input%first.unit.METERS_PER_SQUARE_SECOND
            result_ = assertEquals(acceleration, acceleration / input%second * input%second)
        class default
            result_ = fail("Expected a DoublePrecisionPairInput_t")
        end select
    end function checkDivideMultiply
end module acceleration_math_operators_test
