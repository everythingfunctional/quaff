module area_math_operators_test
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
                "Adding zero returns the original area", &
                DOUBLE_PRECISION_GENERATOR, &
                checkAddZero)
        individual_tests(2) = It( &
                "Subtracting zero returns the original area", &
                DOUBLE_PRECISION_GENERATOR, &
                checkSubtractZero)
        individual_tests(3) = It( &
                "Adding and subtracting the same area returns the original area", &
                DOUBLE_PRECISION_PAIR_GENERATOR, &
                checkAddSubtract)
        tests = Describe("Area_t (+/-) operators", individual_tests)
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
                "A area multiplied by 1 equals itself", &
                DOUBLE_PRECISION_GENERATOR, &
                checkMultiplyByOne)
        individual_tests(2) = It( &
                "A area multiplied by 0 is 0", &
                DOUBLE_PRECISION_GENERATOR, &
                checkMultiplyByZero)
        individual_tests(3) = It( &
                "A area divided by 1 equals itself", &
                DOUBLE_PRECISION_GENERATOR, &
                checkDivideByOne)
        individual_tests(4) = It( &
                "A area divided by itself equals 1", &
                NON_ZERO_DOUBLE_PRECISION_GENERATOR, &
                checkDivideBySelf)
        individual_tests(5) = It( &
                "Multiplying and dividing by the same number returns the original area", &
                NON_ZERO_DOUBLE_PRECISION_PAIR_GENERATOR, &
                checkMultiplyDivide)
        individual_tests(6) = It( &
                "Dividing and multiplying by the same number returns the original area", &
                NON_ZERO_DOUBLE_PRECISION_PAIR_GENERATOR, &
                checkDivideMultiply)
        tests = Describe("Area_t (* & / ) operators", individual_tests)
    end function test_multiplication_division_operator

    function checkAddZero(input) result(result_)
        use Area_m, only: &
                Area_t, operator(.unit.), SQUARE_METERS
        use Area_asserts_m, only: assertEquals
        use Vegetables_m, only: DoublePrecisionInput_t, Input_t, Result_t, fail

        class(Input_t), intent(in) :: input
        type(Result_t) :: result_

        type(Area_t) :: area
        type(Area_t) :: zero

        select type(input)
        type is (DoublePrecisionInput_t)
            area = input%value_.unit.SQUARE_METERS
            zero = 0.0d0.unit.SQUARE_METERS
            result_ = assertEquals(area, area + zero)
        class default
            result_ = fail("Expected a DoublePrecisionInput_t")
        end select
    end function checkAddZero

    function checkSubtractZero(input) result(result_)
        use Area_m, only: &
                Area_t, operator(.unit.), SQUARE_METERS
        use Area_asserts_m, only: assertEquals
        use Vegetables_m, only: DoublePrecisionInput_t, Input_t, Result_t, fail

        class(Input_t), intent(in) :: input
        type(Result_t) :: result_

        type(Area_t) :: area
        type(Area_t) :: zero

        select type(input)
        type is (DoublePrecisionInput_t)
            area = input%value_.unit.SQUARE_METERS
            zero = 0.0d0.unit.SQUARE_METERS
            result_ = assertEquals(area, area - zero)
        class default
            result_ = fail("Expected a DoublePrecisionInput_t")
        end select
    end function checkSubtractZero

    function checkAddSubtract(input) result(result_)
        use DoublePrecisionPairGenerator_m, only: DoublePrecisionPairInput_t
        use iso_varying_string, only: operator(//)
        use Area_m, only: &
                Area_t, operator(.unit.), SQUARE_METERS
        use Area_asserts_m, only: assertEqualsWithinRelative
        use Vegetables_m, only: DoublePrecisionInput_t, Input_t, Result_t, fail

        class(Input_t), intent(in) :: input
        type(Result_t) :: result_

        type(Area_t) :: area1
        type(Area_t) :: area2

        select type(input)
        type is (DoublePrecisionPairInput_t)
            area1 = input%first.unit.SQUARE_METERS
            area2 = input%second.unit.SQUARE_METERS
            result_ = assertEqualsWithinRelative( &
                    area1, &
                    (area1 + area2) - area2, &
                    1.0d-12, &
                    "area1 = " // area1%toString() // ", area2 = " // area2%toString())
        class default
            result_ = fail("Expected a DoublePrecisionPairInput_t")
        end select
    end function checkAddSubtract

    function checkMultiplyByOne(input) result(result_)
        use Area_m, only: &
                Area_t, operator(.unit.), SQUARE_METERS
        use Area_asserts_m, only: assertEquals
        use Vegetables_m, only: DoublePrecisionInput_t, Input_t, Result_t, fail

        class(Input_t), intent(in) :: input
        type(Result_t) :: result_

        type(Area_t) :: area

        select type(input)
        type is (DoublePrecisionInput_t)
            area = input%value_.unit.SQUARE_METERS
            result_ = assertEquals(area, area * 1.0d0)
        class default
            result_ = fail("Expected a DoublePrecisionInput_t")
        end select
    end function checkMultiplyByOne

    function checkMultiplyByZero(input) result(result_)
        use Area_m, only: &
                Area_t, operator(.unit.), SQUARE_METERS
        use Area_asserts_m, only: assertEquals
        use Vegetables_m, only: DoublePrecisionInput_t, Input_t, Result_t, fail

        class(Input_t), intent(in) :: input
        type(Result_t) :: result_

        type(Area_t) :: area
        type(Area_t) :: zero

        select type(input)
        type is (DoublePrecisionInput_t)
            area = input%value_.unit.SQUARE_METERS
            zero = 0.0d0.unit.SQUARE_METERS
            result_ = assertEquals(zero, area * 0.0d0)
        class default
            result_ = fail("Expected a DoublePrecisionInput_t")
        end select
    end function checkMultiplyByZero

    function checkDivideByOne(input) result(result_)
        use Area_m, only: &
                Area_t, operator(.unit.), SQUARE_METERS
        use Area_asserts_m, only: assertEquals
        use Vegetables_m, only: DoublePrecisionInput_t, Input_t, Result_t, fail

        class(Input_t), intent(in) :: input
        type(Result_t) :: result_

        type(Area_t) :: area

        select type(input)
        type is (DoublePrecisionInput_t)
            area = input%value_.unit.SQUARE_METERS
            result_ = assertEquals(area, area / 1.0d0)
        class default
            result_ = fail("Expected a DoublePrecisionInput_t")
        end select
    end function checkDivideByOne

    function checkDivideBySelf(input) result(result_)
        use Area_m, only: &
                Area_t, operator(.unit.), SQUARE_METERS
        use Vegetables_m, only: &
                DoublePrecisionInput_t, Input_t, Result_t, assertEquals, fail

        class(Input_t), intent(in) :: input
        type(Result_t) :: result_

        type(Area_t) :: area

        select type(input)
        type is (DoublePrecisionInput_t)
            area = input%value_.unit.SQUARE_METERS
            result_ = assertEquals(1.0d0, area / area)
        class default
            result_ = fail("Expected a DoublePrecisionInput_t")
        end select
    end function checkDivideBySelf

    function checkMultiplyDivide(input) result(result_)
        use DoublePrecisionPairGenerator_m, only: DoublePrecisionPairInput_t
        use Area_m, only: &
                Area_t, operator(.unit.), SQUARE_METERS
        use Area_asserts_m, only: assertEquals
        use Vegetables_m, only: Input_t, Result_t, fail

        class(Input_t), intent(in) :: input
        type(Result_t) :: result_

        type(Area_t) :: area

        select type (input)
        type is (DoublePrecisionPairInput_t)
            area = input%first.unit.SQUARE_METERS
            result_ = assertEquals(area, area * input%second / input%second)
        class default
            result_ = fail("Expected a DoublePrecisionPairInput_t")
        end select
    end function checkMultiplyDivide

    function checkDivideMultiply(input) result(result_)
        use DoublePrecisionPairGenerator_m, only: DoublePrecisionPairInput_t
        use Area_m, only: &
                Area_t, operator(.unit.), SQUARE_METERS
        use Area_asserts_m, only: assertEquals
        use Vegetables_m, only: Input_t, Result_t, fail

        class(Input_t), intent(in) :: input
        type(Result_t) :: result_

        type(Area_t) :: area

        select type (input)
        type is (DoublePrecisionPairInput_t)
            area = input%first.unit.SQUARE_METERS
            result_ = assertEquals(area, area / input%second * input%second)
        class default
            result_ = fail("Expected a DoublePrecisionPairInput_t")
        end select
    end function checkDivideMultiply
end module area_math_operators_test
