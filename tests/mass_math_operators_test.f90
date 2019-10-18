module mass_math_operators_test
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
                "Adding zero returns the original mass", &
                DOUBLE_PRECISION_GENERATOR, &
                checkAddZero)
        individual_tests(2) = It( &
                "Subtracting zero returns the original mass", &
                DOUBLE_PRECISION_GENERATOR, &
                checkSubtractZero)
        individual_tests(3) = It( &
                "Adding and subtracting the same mass returns the original mass", &
                DOUBLE_PRECISION_PAIR_GENERATOR, &
                checkAddSubtract)
        tests = Describe("Mass_t (+/-) operators", individual_tests)
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
                "A mass multiplied by 1 equals itself", &
                DOUBLE_PRECISION_GENERATOR, &
                checkMultiplyByOne)
        individual_tests(2) = It( &
                "A mass multiplied by 0 is 0", &
                DOUBLE_PRECISION_GENERATOR, &
                checkMultiplyByZero)
        individual_tests(3) = It( &
                "A mass divided by 1 equals itself", &
                DOUBLE_PRECISION_GENERATOR, &
                checkDivideByOne)
        individual_tests(4) = It( &
                "A mass divided by itself equals 1", &
                NON_ZERO_DOUBLE_PRECISION_GENERATOR, &
                checkDivideBySelf)
        individual_tests(5) = It( &
                "Multiplying and dividing by the same number returns the original mass", &
                NON_ZERO_DOUBLE_PRECISION_PAIR_GENERATOR, &
                checkMultiplyDivide)
        individual_tests(6) = It( &
                "Dividing and multiplying by the same number returns the original mass", &
                NON_ZERO_DOUBLE_PRECISION_PAIR_GENERATOR, &
                checkDivideMultiply)
        tests = Describe("Mass_t (* & / ) operators", individual_tests)
    end function test_multiplication_division_operator

    function checkAddZero(input) result(result_)
        use Mass_m, only: Mass_t, operator(.unit.), KILOGRAMS
        use Mass_asserts_m, only: assertEquals
        use Vegetables_m, only: DoublePrecisionInput_t, Input_t, Result_t, fail

        class(Input_t), intent(in) :: input
        type(Result_t) :: result_

        type(Mass_t) :: mass
        type(Mass_t) :: zero

        select type(input)
        type is (DoublePrecisionInput_t)
            mass = input%value_.unit.KILOGRAMS
            zero = 0.0d0.unit.KILOGRAMS
            result_ = assertEquals(mass, mass + zero)
        class default
            result_ = fail("Expected a DoublePrecisionInput_t")
        end select
    end function checkAddZero

    function checkSubtractZero(input) result(result_)
        use Mass_m, only: Mass_t, operator(.unit.), KILOGRAMS
        use Mass_asserts_m, only: assertEquals
        use Vegetables_m, only: DoublePrecisionInput_t, Input_t, Result_t, fail

        class(Input_t), intent(in) :: input
        type(Result_t) :: result_

        type(Mass_t) :: mass
        type(Mass_t) :: zero

        select type(input)
        type is (DoublePrecisionInput_t)
            mass = input%value_.unit.KILOGRAMS
            zero = 0.0d0.unit.KILOGRAMS
            result_ = assertEquals(mass, mass - zero)
        class default
            result_ = fail("Expected a DoublePrecisionInput_t")
        end select
    end function checkSubtractZero

    function checkAddSubtract(input) result(result_)
        use DoublePrecisionPairGenerator_m, only: DoublePrecisionPairInput_t
        use iso_varying_string, only: operator(//)
        use Mass_m, only: Mass_t, operator(.unit.), KILOGRAMS
        use Mass_asserts_m, only: assertEqualsWithinRelative
        use Vegetables_m, only: DoublePrecisionInput_t, Input_t, Result_t, fail

        class(Input_t), intent(in) :: input
        type(Result_t) :: result_

        type(Mass_t) :: mass1
        type(Mass_t) :: mass2

        select type(input)
        type is (DoublePrecisionPairInput_t)
            mass1 = input%first.unit.KILOGRAMS
            mass2 = input%second.unit.KILOGRAMS
            result_ = assertEqualsWithinRelative( &
                    mass1, &
                    (mass1 + mass2) - mass2, &
                    1.0d-12, &
                    "mass1 = " // mass1%toString() // ", mass2 = " // mass2%toString())
        class default
            result_ = fail("Expected a DoublePrecisionPairInput_t")
        end select
    end function checkAddSubtract

    function checkMultiplyByOne(input) result(result_)
        use Mass_m, only: Mass_t, operator(.unit.), KILOGRAMS
        use Mass_asserts_m, only: assertEquals
        use Vegetables_m, only: DoublePrecisionInput_t, Input_t, Result_t, fail

        class(Input_t), intent(in) :: input
        type(Result_t) :: result_

        type(Mass_t) :: mass

        select type(input)
        type is (DoublePrecisionInput_t)
            mass = input%value_.unit.KILOGRAMS
            result_ = assertEquals(mass, mass * 1.0d0)
        class default
            result_ = fail("Expected a DoublePrecisionInput_t")
        end select
    end function checkMultiplyByOne

    function checkMultiplyByZero(input) result(result_)
        use Mass_m, only: Mass_t, operator(.unit.), KILOGRAMS
        use Mass_asserts_m, only: assertEquals
        use Vegetables_m, only: DoublePrecisionInput_t, Input_t, Result_t, fail

        class(Input_t), intent(in) :: input
        type(Result_t) :: result_

        type(Mass_t) :: mass
        type(Mass_t) :: zero

        select type(input)
        type is (DoublePrecisionInput_t)
            mass = input%value_.unit.KILOGRAMS
            zero = 0.0d0.unit.KILOGRAMS
            result_ = assertEquals(zero, mass * 0.0d0)
        class default
            result_ = fail("Expected a DoublePrecisionInput_t")
        end select
    end function checkMultiplyByZero

    function checkDivideByOne(input) result(result_)
        use Mass_m, only: Mass_t, operator(.unit.), KILOGRAMS
        use Mass_asserts_m, only: assertEquals
        use Vegetables_m, only: DoublePrecisionInput_t, Input_t, Result_t, fail

        class(Input_t), intent(in) :: input
        type(Result_t) :: result_

        type(Mass_t) :: mass

        select type(input)
        type is (DoublePrecisionInput_t)
            mass = input%value_.unit.KILOGRAMS
            result_ = assertEquals(mass, mass / 1.0d0)
        class default
            result_ = fail("Expected a DoublePrecisionInput_t")
        end select
    end function checkDivideByOne

    function checkDivideBySelf(input) result(result_)
        use Mass_m, only: Mass_t, operator(.unit.), KILOGRAMS
        use Vegetables_m, only: &
                DoublePrecisionInput_t, Input_t, Result_t, assertEquals, fail

        class(Input_t), intent(in) :: input
        type(Result_t) :: result_

        type(Mass_t) :: mass

        select type(input)
        type is (DoublePrecisionInput_t)
            mass = input%value_.unit.KILOGRAMS
            result_ = assertEquals(1.0d0, mass / mass)
        class default
            result_ = fail("Expected a DoublePrecisionInput_t")
        end select
    end function checkDivideBySelf

    function checkMultiplyDivide(input) result(result_)
        use DoublePrecisionPairGenerator_m, only: DoublePrecisionPairInput_t
        use Mass_m, only: Mass_t, operator(.unit.), KILOGRAMS
        use Mass_asserts_m, only: assertEquals
        use Vegetables_m, only: Input_t, Result_t, fail

        class(Input_t), intent(in) :: input
        type(Result_t) :: result_

        type(Mass_t) :: mass

        select type (input)
        type is (DoublePrecisionPairInput_t)
            mass = input%first.unit.KILOGRAMS
            result_ = assertEquals(mass, mass * input%second / input%second)
        class default
            result_ = fail("Expected a DoublePrecisionPairInput_t")
        end select
    end function checkMultiplyDivide

    function checkDivideMultiply(input) result(result_)
        use DoublePrecisionPairGenerator_m, only: DoublePrecisionPairInput_t
        use Mass_m, only: Mass_t, operator(.unit.), KILOGRAMS
        use Mass_asserts_m, only: assertEquals
        use Vegetables_m, only: Input_t, Result_t, fail

        class(Input_t), intent(in) :: input
        type(Result_t) :: result_

        type(Mass_t) :: mass

        select type (input)
        type is (DoublePrecisionPairInput_t)
            mass = input%first.unit.KILOGRAMS
            result_ = assertEquals(mass, mass / input%second * input%second)
        class default
            result_ = fail("Expected a DoublePrecisionPairInput_t")
        end select
    end function checkDivideMultiply
end module mass_math_operators_test
