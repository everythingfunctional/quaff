module volume_math_operators_test
    use DoublePrecisionGenerator_m, only: DOUBLE_PRECISION_GENERATOR
    use DoublePrecisionPairGenerator_m, only: &
            DoublePrecisionPairInput_t, DOUBLE_PRECISION_PAIR_GENERATOR
    use iso_varying_string, only: operator(//)
    use NonZeroDoublePrecisionGenerator_m, only: &
            NON_ZERO_DOUBLE_PRECISION_GENERATOR
    use NonZeroDoublePrecisionPairGenerator_m, only: &
            NON_ZERO_DOUBLE_PRECISION_PAIR_GENERATOR
    use Volume_m, only: &
            Volume_t, operator(.unit.), CUBIC_METERS
    use Volume_asserts_m, only: &
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
                "Adding zero returns the original volume", &
                DOUBLE_PRECISION_GENERATOR, &
                checkAddZero)
        individual_tests(2) = It( &
                "Subtracting zero returns the original volume", &
                DOUBLE_PRECISION_GENERATOR, &
                checkSubtractZero)
        individual_tests(3) = It( &
                "Adding and subtracting the same volume returns the original volume", &
                DOUBLE_PRECISION_PAIR_GENERATOR, &
                checkAddSubtract)
        tests = Describe("Volume_t (+/-) operators", individual_tests)
    end function test_addition_subtraction_operators

    function test_multiplication_division_operator() result(tests)
        type(TestItem_t) :: tests

        type(TestItem_t) :: individual_tests(6)

        individual_tests(1) = It( &
                "A volume multiplied by 1 equals itself", &
                DOUBLE_PRECISION_GENERATOR, &
                checkMultiplyByOne)
        individual_tests(2) = It( &
                "A volume multiplied by 0 is 0", &
                DOUBLE_PRECISION_GENERATOR, &
                checkMultiplyByZero)
        individual_tests(3) = It( &
                "A volume divided by 1 equals itself", &
                DOUBLE_PRECISION_GENERATOR, &
                checkDivideByOne)
        individual_tests(4) = It( &
                "A volume divided by itself equals 1", &
                NON_ZERO_DOUBLE_PRECISION_GENERATOR, &
                checkDivideBySelf)
        individual_tests(5) = It( &
                "Multiplying and dividing by the same number returns the original volume", &
                NON_ZERO_DOUBLE_PRECISION_PAIR_GENERATOR, &
                checkMultiplyDivide)
        individual_tests(6) = It( &
                "Dividing and multiplying by the same number returns the original volume", &
                NON_ZERO_DOUBLE_PRECISION_PAIR_GENERATOR, &
                checkDivideMultiply)
        tests = Describe("Volume_t (* & / ) operators", individual_tests)
    end function test_multiplication_division_operator

    pure function checkAddZero(input) result(result_)
        class(Input_t), intent(in) :: input
        type(Result_t) :: result_

        type(Volume_t) :: volume
        type(Volume_t) :: zero

        select type(input)
        type is (DoublePrecisionInput_t)
            volume = input%value_.unit.CUBIC_METERS
            zero = 0.0d0.unit.CUBIC_METERS
            result_ = assertEquals(volume, volume + zero)
        class default
            result_ = fail("Expected a DoublePrecisionInput_t")
        end select
    end function checkAddZero

    pure function checkSubtractZero(input) result(result_)
        class(Input_t), intent(in) :: input
        type(Result_t) :: result_

        type(Volume_t) :: volume
        type(Volume_t) :: zero

        select type(input)
        type is (DoublePrecisionInput_t)
            volume = input%value_.unit.CUBIC_METERS
            zero = 0.0d0.unit.CUBIC_METERS
            result_ = assertEquals(volume, volume - zero)
        class default
            result_ = fail("Expected a DoublePrecisionInput_t")
        end select
    end function checkSubtractZero

    pure function checkAddSubtract(input) result(result_)
        class(Input_t), intent(in) :: input
        type(Result_t) :: result_

        type(Volume_t) :: volume1
        type(Volume_t) :: volume2

        select type(input)
        type is (DoublePrecisionPairInput_t)
            volume1 = input%first.unit.CUBIC_METERS
            volume2 = input%second.unit.CUBIC_METERS
            result_ = assertEqualsWithinRelative( &
                    volume1, &
                    (volume1 + volume2) - volume2, &
                    1.0d-8, &
                    "volume1 = " // volume1%toString() // ", volume2 = " // volume2%toString())
        class default
            result_ = fail("Expected a DoublePrecisionPairInput_t")
        end select
    end function checkAddSubtract

    pure function checkMultiplyByOne(input) result(result_)
        class(Input_t), intent(in) :: input
        type(Result_t) :: result_

        type(Volume_t) :: volume

        select type(input)
        type is (DoublePrecisionInput_t)
            volume = input%value_.unit.CUBIC_METERS
            result_ = assertEquals(volume, volume * 1.0d0)
        class default
            result_ = fail("Expected a DoublePrecisionInput_t")
        end select
    end function checkMultiplyByOne

    pure function checkMultiplyByZero(input) result(result_)
        class(Input_t), intent(in) :: input
        type(Result_t) :: result_

        type(Volume_t) :: volume
        type(Volume_t) :: zero

        select type(input)
        type is (DoublePrecisionInput_t)
            volume = input%value_.unit.CUBIC_METERS
            zero = 0.0d0.unit.CUBIC_METERS
            result_ = assertEquals(zero, volume * 0.0d0)
        class default
            result_ = fail("Expected a DoublePrecisionInput_t")
        end select
    end function checkMultiplyByZero

    pure function checkDivideByOne(input) result(result_)
        class(Input_t), intent(in) :: input
        type(Result_t) :: result_

        type(Volume_t) :: volume

        select type(input)
        type is (DoublePrecisionInput_t)
            volume = input%value_.unit.CUBIC_METERS
            result_ = assertEquals(volume, volume / 1.0d0)
        class default
            result_ = fail("Expected a DoublePrecisionInput_t")
        end select
    end function checkDivideByOne

    pure function checkDivideBySelf(input) result(result_)
        class(Input_t), intent(in) :: input
        type(Result_t) :: result_

        type(Volume_t) :: volume

        select type(input)
        type is (DoublePrecisionInput_t)
            volume = input%value_.unit.CUBIC_METERS
            result_ = assertEquals(1.0d0, volume / volume)
        class default
            result_ = fail("Expected a DoublePrecisionInput_t")
        end select
    end function checkDivideBySelf

    pure function checkMultiplyDivide(input) result(result_)
        class(Input_t), intent(in) :: input
        type(Result_t) :: result_

        type(Volume_t) :: volume

        select type (input)
        type is (DoublePrecisionPairInput_t)
            volume = input%first.unit.CUBIC_METERS
            result_ = assertEquals(volume, volume * input%second / input%second)
        class default
            result_ = fail("Expected a DoublePrecisionPairInput_t")
        end select
    end function checkMultiplyDivide

    pure function checkDivideMultiply(input) result(result_)
        class(Input_t), intent(in) :: input
        type(Result_t) :: result_

        type(Volume_t) :: volume

        select type (input)
        type is (DoublePrecisionPairInput_t)
            volume = input%first.unit.CUBIC_METERS
            result_ = assertEquals(volume, volume / input%second * input%second)
        class default
            result_ = fail("Expected a DoublePrecisionPairInput_t")
        end select
    end function checkDivideMultiply
end module volume_math_operators_test
