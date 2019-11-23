module temperature_math_operators_test
    use DoublePrecisionGenerator_m, only: DOUBLE_PRECISION_GENERATOR
    use DoublePrecisionPairGenerator_m, only: &
            DoublePrecisionPairInput_t, DOUBLE_PRECISION_PAIR_GENERATOR
    use iso_varying_string, only: operator(//)
    use NonZeroDoublePrecisionGenerator_m, only: &
            NON_ZERO_DOUBLE_PRECISION_GENERATOR
    use NonZeroDoublePrecisionPairGenerator_m, only: &
            NON_ZERO_DOUBLE_PRECISION_PAIR_GENERATOR
    use Temperature_m, only: &
            Temperature_t, operator(.unit.), KELVIN
    use Temperature_asserts_m, only: &
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
                "Adding zero returns the original temperature", &
                DOUBLE_PRECISION_GENERATOR, &
                checkAddZero)
        individual_tests(2) = It( &
                "Subtracting zero returns the original temperature", &
                DOUBLE_PRECISION_GENERATOR, &
                checkSubtractZero)
        individual_tests(3) = It( &
                "Adding and subtracting the same temperature returns the original temperature", &
                DOUBLE_PRECISION_PAIR_GENERATOR, &
                checkAddSubtract)
        tests = Describe("Temperature_t (+/-) operators", individual_tests)
    end function test_addition_subtraction_operators

    function test_multiplication_division_operator() result(tests)
        type(TestItem_t) :: tests

        type(TestItem_t) :: individual_tests(6)

        individual_tests(1) = It( &
                "A temperature multiplied by 1 equals itself", &
                DOUBLE_PRECISION_GENERATOR, &
                checkMultiplyByOne)
        individual_tests(2) = It( &
                "A temperature multiplied by 0 is 0", &
                DOUBLE_PRECISION_GENERATOR, &
                checkMultiplyByZero)
        individual_tests(3) = It( &
                "A temperature divided by 1 equals itself", &
                DOUBLE_PRECISION_GENERATOR, &
                checkDivideByOne)
        individual_tests(4) = It( &
                "A temperature divided by itself equals 1", &
                NON_ZERO_DOUBLE_PRECISION_GENERATOR, &
                checkDivideBySelf)
        individual_tests(5) = It( &
                "Multiplying and dividing by the same number returns the original temperature", &
                NON_ZERO_DOUBLE_PRECISION_PAIR_GENERATOR, &
                checkMultiplyDivide)
        individual_tests(6) = It( &
                "Dividing and multiplying by the same number returns the original temperature", &
                NON_ZERO_DOUBLE_PRECISION_PAIR_GENERATOR, &
                checkDivideMultiply)
        tests = Describe("Temperature_t (* & / ) operators", individual_tests)
    end function test_multiplication_division_operator

    pure function checkAddZero(input) result(result_)
        class(Input_t), intent(in) :: input
        type(Result_t) :: result_

        type(Temperature_t) :: temperature
        type(Temperature_t) :: zero

        select type(input)
        type is (DoublePrecisionInput_t)
            temperature = input%value_.unit.KELVIN
            zero = 0.0d0.unit.KELVIN
            result_ = assertEquals(temperature, temperature + zero)
        class default
            result_ = fail("Expected a DoublePrecisionInput_t")
        end select
    end function checkAddZero

    pure function checkSubtractZero(input) result(result_)
        class(Input_t), intent(in) :: input
        type(Result_t) :: result_

        type(Temperature_t) :: temperature
        type(Temperature_t) :: zero

        select type(input)
        type is (DoublePrecisionInput_t)
            temperature = input%value_.unit.KELVIN
            zero = 0.0d0.unit.KELVIN
            result_ = assertEquals(temperature, temperature - zero)
        class default
            result_ = fail("Expected a DoublePrecisionInput_t")
        end select
    end function checkSubtractZero

    pure function checkAddSubtract(input) result(result_)
        class(Input_t), intent(in) :: input
        type(Result_t) :: result_

        type(Temperature_t) :: temperature1
        type(Temperature_t) :: temperature2

        select type(input)
        type is (DoublePrecisionPairInput_t)
            temperature1 = input%first.unit.KELVIN
            temperature2 = input%second.unit.KELVIN
            result_ = assertEqualsWithinRelative( &
                    temperature1, &
                    (temperature1 + temperature2) - temperature2, &
                    1.0d-8, &
                    "temperature1 = " // temperature1%toString() // ", temperature2 = " // temperature2%toString())
        class default
            result_ = fail("Expected a DoublePrecisionPairInput_t")
        end select
    end function checkAddSubtract

    pure function checkMultiplyByOne(input) result(result_)
        class(Input_t), intent(in) :: input
        type(Result_t) :: result_

        type(Temperature_t) :: temperature

        select type(input)
        type is (DoublePrecisionInput_t)
            temperature = input%value_.unit.KELVIN
            result_ = assertEquals(temperature, temperature * 1.0d0)
        class default
            result_ = fail("Expected a DoublePrecisionInput_t")
        end select
    end function checkMultiplyByOne

    pure function checkMultiplyByZero(input) result(result_)
        class(Input_t), intent(in) :: input
        type(Result_t) :: result_

        type(Temperature_t) :: temperature
        type(Temperature_t) :: zero

        select type(input)
        type is (DoublePrecisionInput_t)
            temperature = input%value_.unit.KELVIN
            zero = 0.0d0.unit.KELVIN
            result_ = assertEquals(zero, temperature * 0.0d0)
        class default
            result_ = fail("Expected a DoublePrecisionInput_t")
        end select
    end function checkMultiplyByZero

    pure function checkDivideByOne(input) result(result_)
        class(Input_t), intent(in) :: input
        type(Result_t) :: result_

        type(Temperature_t) :: temperature

        select type(input)
        type is (DoublePrecisionInput_t)
            temperature = input%value_.unit.KELVIN
            result_ = assertEquals(temperature, temperature / 1.0d0)
        class default
            result_ = fail("Expected a DoublePrecisionInput_t")
        end select
    end function checkDivideByOne

    pure function checkDivideBySelf(input) result(result_)
        class(Input_t), intent(in) :: input
        type(Result_t) :: result_

        type(Temperature_t) :: temperature

        select type(input)
        type is (DoublePrecisionInput_t)
            temperature = input%value_.unit.KELVIN
            result_ = assertEquals(1.0d0, temperature / temperature)
        class default
            result_ = fail("Expected a DoublePrecisionInput_t")
        end select
    end function checkDivideBySelf

    pure function checkMultiplyDivide(input) result(result_)
        class(Input_t), intent(in) :: input
        type(Result_t) :: result_

        type(Temperature_t) :: temperature

        select type (input)
        type is (DoublePrecisionPairInput_t)
            temperature = input%first.unit.KELVIN
            result_ = assertEquals(temperature, temperature * input%second / input%second)
        class default
            result_ = fail("Expected a DoublePrecisionPairInput_t")
        end select
    end function checkMultiplyDivide

    pure function checkDivideMultiply(input) result(result_)
        class(Input_t), intent(in) :: input
        type(Result_t) :: result_

        type(Temperature_t) :: temperature

        select type (input)
        type is (DoublePrecisionPairInput_t)
            temperature = input%first.unit.KELVIN
            result_ = assertEquals(temperature, temperature / input%second * input%second)
        class default
            result_ = fail("Expected a DoublePrecisionPairInput_t")
        end select
    end function checkDivideMultiply
end module temperature_math_operators_test
