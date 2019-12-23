module molar_mass_math_ops_test
    use DoublePrecisionGenerator_m, only: DOUBLE_PRECISION_GENERATOR
    use DoublePrecisionPairGenerator_m, only: &
            DoublePrecisionPairInput_t, DOUBLE_PRECISION_PAIR_GENERATOR
    use iso_varying_string, only: operator(//)
    use NonZeroDoublePrecisionGenerator_m, only: &
            NON_ZERO_DOUBLE_PRECISION_GENERATOR
    use NonZeroDoublePrecisionPairGenerator_m, only: &
            NON_ZERO_DOUBLE_PRECISION_PAIR_GENERATOR
    use quaff, only: &
            MolarMass_t, operator(.unit.), KILOGRAMS_PER_MOL
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
                "Adding zero returns the original molar_mass", &
                DOUBLE_PRECISION_GENERATOR, &
                checkAddZero)
        individual_tests(2) = It( &
                "Subtracting zero returns the original molar_mass", &
                DOUBLE_PRECISION_GENERATOR, &
                checkSubtractZero)
        individual_tests(3) = It( &
                "Adding and subtracting the same molar_mass returns the original molar_mass", &
                DOUBLE_PRECISION_PAIR_GENERATOR, &
                checkAddSubtract)
        tests = Describe("MolarMass_t (+/-) operators", individual_tests)
    end function test_addition_subtraction_operators

    function test_multiplication_division_operator() result(tests)
        type(TestItem_t) :: tests

        type(TestItem_t) :: individual_tests(6)

        individual_tests(1) = It( &
                "A molar_mass multiplied by 1 equals itself", &
                DOUBLE_PRECISION_GENERATOR, &
                checkMultiplyByOne)
        individual_tests(2) = It( &
                "A molar_mass multiplied by 0 is 0", &
                DOUBLE_PRECISION_GENERATOR, &
                checkMultiplyByZero)
        individual_tests(3) = It( &
                "A molar_mass divided by 1 equals itself", &
                DOUBLE_PRECISION_GENERATOR, &
                checkDivideByOne)
        individual_tests(4) = It( &
                "A molar_mass divided by itself equals 1", &
                NON_ZERO_DOUBLE_PRECISION_GENERATOR, &
                checkDivideBySelf)
        individual_tests(5) = It( &
                "Multiplying and dividing by the same number returns the original molar_mass", &
                NON_ZERO_DOUBLE_PRECISION_PAIR_GENERATOR, &
                checkMultiplyDivide)
        individual_tests(6) = It( &
                "Dividing and multiplying by the same number returns the original molar_mass", &
                NON_ZERO_DOUBLE_PRECISION_PAIR_GENERATOR, &
                checkDivideMultiply)
        tests = Describe("MolarMass_t (* & / ) operators", individual_tests)
    end function test_multiplication_division_operator

    pure function checkAddZero(input) result(result_)
        class(Input_t), intent(in) :: input
        type(Result_t) :: result_

        type(MolarMass_t) :: molar_mass
        type(MolarMass_t) :: zero

        select type(input)
        type is (DoublePrecisionInput_t)
            molar_mass = input%value_.unit.KILOGRAMS_PER_MOL
            zero = 0.0d0.unit.KILOGRAMS_PER_MOL
            result_ = assertEquals(molar_mass, molar_mass + zero)
        class default
            result_ = fail("Expected a DoublePrecisionInput_t")
        end select
    end function checkAddZero

    pure function checkSubtractZero(input) result(result_)
        class(Input_t), intent(in) :: input
        type(Result_t) :: result_

        type(MolarMass_t) :: molar_mass
        type(MolarMass_t) :: zero

        select type(input)
        type is (DoublePrecisionInput_t)
            molar_mass = input%value_.unit.KILOGRAMS_PER_MOL
            zero = 0.0d0.unit.KILOGRAMS_PER_MOL
            result_ = assertEquals(molar_mass, molar_mass - zero)
        class default
            result_ = fail("Expected a DoublePrecisionInput_t")
        end select
    end function checkSubtractZero

    pure function checkAddSubtract(input) result(result_)
        class(Input_t), intent(in) :: input
        type(Result_t) :: result_

        type(MolarMass_t) :: molar_mass1
        type(MolarMass_t) :: molar_mass2

        select type(input)
        type is (DoublePrecisionPairInput_t)
            molar_mass1 = input%first.unit.KILOGRAMS_PER_MOL
            molar_mass2 = input%second.unit.KILOGRAMS_PER_MOL
            result_ = assertEqualsWithinRelative( &
                    molar_mass1, &
                    (molar_mass1 + molar_mass2) - molar_mass2, &
                    1.0d-8, &
                    "molar_mass1 = " // molar_mass1%toString() &
                    // ", molar_mass2 = " // molar_mass2%toString())
        class default
            result_ = fail("Expected a DoublePrecisionPairInput_t")
        end select
    end function checkAddSubtract

    pure function checkMultiplyByOne(input) result(result_)
        class(Input_t), intent(in) :: input
        type(Result_t) :: result_

        type(MolarMass_t) :: molar_mass

        select type(input)
        type is (DoublePrecisionInput_t)
            molar_mass = input%value_.unit.KILOGRAMS_PER_MOL
            result_ = assertEquals(molar_mass, molar_mass * 1.0d0)
        class default
            result_ = fail("Expected a DoublePrecisionInput_t")
        end select
    end function checkMultiplyByOne

    pure function checkMultiplyByZero(input) result(result_)
        class(Input_t), intent(in) :: input
        type(Result_t) :: result_

        type(MolarMass_t) :: molar_mass
        type(MolarMass_t) :: zero

        select type(input)
        type is (DoublePrecisionInput_t)
            molar_mass = input%value_.unit.KILOGRAMS_PER_MOL
            zero = 0.0d0.unit.KILOGRAMS_PER_MOL
            result_ = assertEquals(zero, molar_mass * 0.0d0)
        class default
            result_ = fail("Expected a DoublePrecisionInput_t")
        end select
    end function checkMultiplyByZero

    pure function checkDivideByOne(input) result(result_)
        class(Input_t), intent(in) :: input
        type(Result_t) :: result_

        type(MolarMass_t) :: molar_mass

        select type(input)
        type is (DoublePrecisionInput_t)
            molar_mass = input%value_.unit.KILOGRAMS_PER_MOL
            result_ = assertEquals(molar_mass, molar_mass / 1.0d0)
        class default
            result_ = fail("Expected a DoublePrecisionInput_t")
        end select
    end function checkDivideByOne

    pure function checkDivideBySelf(input) result(result_)
        class(Input_t), intent(in) :: input
        type(Result_t) :: result_

        type(MolarMass_t) :: molar_mass

        select type(input)
        type is (DoublePrecisionInput_t)
            molar_mass = input%value_.unit.KILOGRAMS_PER_MOL
            result_ = assertEquals(1.0d0, molar_mass / molar_mass)
        class default
            result_ = fail("Expected a DoublePrecisionInput_t")
        end select
    end function checkDivideBySelf

    pure function checkMultiplyDivide(input) result(result_)
        class(Input_t), intent(in) :: input
        type(Result_t) :: result_

        type(MolarMass_t) :: molar_mass

        select type (input)
        type is (DoublePrecisionPairInput_t)
            molar_mass = input%first.unit.KILOGRAMS_PER_MOL
            result_ = assertEquals(molar_mass, molar_mass * input%second / input%second)
        class default
            result_ = fail("Expected a DoublePrecisionPairInput_t")
        end select
    end function checkMultiplyDivide

    pure function checkDivideMultiply(input) result(result_)
        class(Input_t), intent(in) :: input
        type(Result_t) :: result_

        type(MolarMass_t) :: molar_mass

        select type (input)
        type is (DoublePrecisionPairInput_t)
            molar_mass = input%first.unit.KILOGRAMS_PER_MOL
            result_ = assertEquals(molar_mass, molar_mass / input%second * input%second)
        class default
            result_ = fail("Expected a DoublePrecisionPairInput_t")
        end select
    end function checkDivideMultiply
end module molar_mass_math_ops_test
