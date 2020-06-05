module mass_logic_ops_test
    use DoublePrecisionGenerator_m, only: DOUBLE_PRECISION_GENERATOR
    use iso_varying_string, only: operator(//)
    use quaff, only: &
            Mass_t, operator(.unit.), KILOGRAMS
    use Vegetables_m, only: &
            DoublePrecisionInput_t, &
            Input_t, &
            Result_t, &
            TestItem_t, &
            assertNot, &
            assertThat, &
            Describe, &
            fail, &
            It

    implicit none
    private

    public :: &
            test_equal_operator, &
            test_not_equal_operator, &
            test_equal_within, &
            test_greater_than_or_equal_operator, &
            test_less_than_or_equal_operator, &
            test_greater_than_operator, &
            test_less_than_operator
contains
    function test_equal_operator() result(tests)
        type(TestItem_t) :: tests

        type(TestItem_t) :: individual_tests(2)

        individual_tests(1) = it( &
                "is equal for the same mass", &
                DOUBLE_PRECISION_GENERATOR, &
                checkEqualWithSameNumber)
        individual_tests(2) = it( &
                "is not equal for different masss", &
                DOUBLE_PRECISION_GENERATOR, &
                checkEqualWithDifferentNumbers)
        tests = describe("Mass_t with operator(==)", individual_tests)
    end function test_equal_operator

    function test_not_equal_operator() result(tests)
        type(TestItem_t) :: tests

        type(TestItem_t) :: individual_tests(2)

        individual_tests(1) = it( &
                "is equal for the same mass", &
                DOUBLE_PRECISION_GENERATOR, &
                checkNotEqualWithSameNumber)
        individual_tests(2) = it( &
                "is not equal for different masss", &
                DOUBLE_PRECISION_GENERATOR, &
                checkNotEqualWithDifferentNumbers)
        tests = describe("Mass_t with operator(/=)", individual_tests)
    end function test_not_equal_operator

    function test_equal_within() result(tests)
        type(TestItem_t) :: tests

        type(TestItem_t) :: individual_tests(3)

        individual_tests(1) = it( &
                "is equal for the same mass even for tiny tolerance", &
                DOUBLE_PRECISION_GENERATOR, &
                checkEqualWithinWithSameNumber)
        individual_tests(2) = it( &
                "is equal for sufficiently close values", &
                DOUBLE_PRECISION_GENERATOR, &
                checkEqualWithinWithCloseNumbers)
        individual_tests(3) = it( &
                "is not equal for sufficiently different values", &
                DOUBLE_PRECISION_GENERATOR, &
                checkEqualWithinWithDifferentNumbers)
        tests = describe("Mass_t%equal with tolerance", individual_tests)
    end function test_equal_within

    function test_greater_than_or_equal_operator() result(tests)
        type(TestItem_t) ::  tests

        type(TestItem_t) :: individual_tests(3)

        individual_tests(1) = it( &
                "is true if the lhs is greater than the rhs",&
                DOUBLE_PRECISION_GENERATOR,&
                checkGreaterThanOrEqualWithGreaterNumbers)
        individual_tests(2) = it( &
                "is true if the lhs is equal to the rhs", &
                DOUBLE_PRECISION_GENERATOR, &
                checkGreaterThanOrEqualWithSameNumbers)
        individual_tests(3) = it( &
                "is false if the lhs is less than the rhs", &
                DOUBLE_PRECISION_GENERATOR, &
                checkGreaterThanOrEqualWithLesserNumbers)
        tests = describe("Mass_t operator (>=)", &
                individual_tests)
    end function test_greater_than_or_equal_operator

    function test_less_than_or_equal_operator() result(tests)
        type(TestItem_t) :: tests

        type(TestItem_t) :: individual_tests(3)

        individual_tests(1) = it( &
                "is true if the lhs is less than the rhs",&
                DOUBLE_PRECISION_GENERATOR,&
                checklessThanOrEqualWithLesserNumbers)
        individual_tests(2) = it( &
                "is true if the lhs is equal to the rhs", &
                DOUBLE_PRECISION_GENERATOR, &
                checkLessThanOrEqualWithSameNumbers)
        individual_tests(3) = it( &
                "is false if the lhs is greater than the rhs", &
                DOUBLE_PRECISION_GENERATOR, &
                checkLessThanOrEqualWithGreaterNumbers)
        tests = describe("Mass_t operator (<=)", &
                individual_tests)
    end function test_less_than_or_equal_operator

    function test_greater_than_operator() result(tests)
        type(TestItem_t) ::  tests

        type(TestItem_t) :: individual_tests(3)

        individual_tests(1) = it( &
                "is true if the lhs is greater than the rhs",&
                DOUBLE_PRECISION_GENERATOR,&
                checkGreaterThanWithGreaterNumbers)
        individual_tests(2) = it( &
                "is false if the lhs is equal to the rhs", &
                DOUBLE_PRECISION_GENERATOR, &
                checkGreaterThanWithSameNumbers)
        individual_tests(3) = it( &
                "is false if the lhs is less than the rhs", &
                DOUBLE_PRECISION_GENERATOR, &
                checkGreaterThanWithLesserNumbers)
        tests = describe("Mass_t operator (>)", &
                individual_tests)
    end function test_greater_than_operator

    function test_less_than_operator() result(tests)
        type(TestItem_t) ::  tests

        type(TestItem_t) :: individual_tests(3)

        individual_tests(1) = it( &
                "is true if the lhs is less than the rhs",&
                DOUBLE_PRECISION_GENERATOR,&
                checkLessThanWithLesserNumbers)
        individual_tests(2) = it( &
                "is false if the lhs is equal to the rhs", &
                DOUBLE_PRECISION_GENERATOR, &
                checkLessThanWithSameNumbers)
        individual_tests(3) = it( &
                "is false if the lhs is less than the rhs", &
                DOUBLE_PRECISION_GENERATOR, &
                checkLessThanWithGreaterNumbers)
        tests = describe("Mass_t operator (<)", &
                individual_tests)
    end function test_less_than_operator

    pure function checkEqualWithSameNumber(number) result(result_)
        class(Input_t), intent(in) :: number
        type(Result_t) :: result_

        type(Mass_t) :: the_mass

        select type (number)
        type is (DoublePrecisionInput_t)
            the_mass = number%value_.unit.KILOGRAMS
            result_ = assertThat( &
                    the_mass == the_mass, &
                    the_mass%toString() // " == " // the_mass%toString())
        class default
            result_ = fail("Expected to get a DoublePrecisionInput_t")
        end select
    end function checkEqualWithSameNumber

    pure function checkEqualWithDifferentNumbers(number) result(result_)
        class(Input_t), intent(in) :: number
        type(Result_t) :: result_

        type(Mass_t) :: mass1
        type(Mass_t) :: mass2

        select type (number)
        type is (DoublePrecisionInput_t)
            mass1 = number%value_.unit.KILOGRAMS
            mass2 = (1.0d0 + number%value_).unit.KILOGRAMS
            result_ = assertNot( &
                    mass1 == mass2, &
                    mass1%toString() // " == " // mass2%toString())
        class default
            result_ = fail("Expected to get a DoublePrecisionInput_t")
        end select
    end function checkEqualWithDifferentNumbers

    pure function checkNotEqualWithSameNumber(number) result(result_)
        class(Input_t), intent(in) :: number
        type(Result_t) :: result_

        type(Mass_t) :: the_mass

        select type (number)
        type is (DoublePrecisionInput_t)
            the_mass = number%value_.unit.KILOGRAMS
            result_ = assertNot( &
                    the_mass /= the_mass, &
                    the_mass%toString() // " /= " // the_mass%toString())
        class default
            result_ = fail("Expected to get a DoublePrecisionInput_t")
        end select
    end function checkNotEqualWithSameNumber

    pure function checkNotEqualWithDifferentNumbers(number) result(result_)
        class(Input_t), intent(in) :: number
        type(Result_t) :: result_

        type(Mass_t) :: mass1
        type(Mass_t) :: mass2

        select type (number)
        type is (DoublePrecisionInput_t)
            mass1 = number%value_.unit.KILOGRAMS
            mass2 = (1.0d0 + number%value_).unit.KILOGRAMS
            result_ = assertThat( &
                    mass1 /= mass2, &
                    mass1%toString() // " /= " // mass2%toString())
        class default
            result_ = fail("Expected to get a DoublePrecisionInput_t")
        end select
    end function checkNotEqualWithDifferentNumbers

    pure function checkEqualWithinWithSameNumber(number) result(result_)
        class(Input_t), intent(in) :: number
        type(Result_t) :: result_

        type(Mass_t) :: the_mass
        type(Mass_t) :: tolerance

        select type (number)
        type is (DoublePrecisionInput_t)
            the_mass = number%value_.unit.KILOGRAMS
            tolerance = TINY(1.0d0).unit.KILOGRAMS
            result_ = assertThat( &
                    the_mass%equal(the_mass, within = tolerance), &
                    "(" // the_mass%toString() // ")%equal(" &
                        // the_mass%toString() // ", within = " &
                        // tolerance%toString() // ")")
        class default
            result_ = fail("Expected to get a DoublePrecisionInput_t")
        end select
    end function checkEqualWithinWithSameNumber

    pure function checkEqualWithinWithCloseNumbers(number) result(result_)
        class(Input_t), intent(in) :: number
        type(Result_t) :: result_

        type(Mass_t) :: mass1
        type(Mass_t) :: mass2
        type(Mass_t) :: tolerance

        select type (number)
        type is (DoublePrecisionInput_t)
            mass1 = number%value_.unit.KILOGRAMS
            mass2 = (number%value_ + 0.05d0).unit.KILOGRAMS
            tolerance = 0.1d0.unit.KILOGRAMS
            result_ = assertThat( &
                    mass1%equal(mass2, within = tolerance), &
                    "(" // mass1%toString() // ")%equal(" &
                        // mass2%toString() // ", within = " &
                        // tolerance%toString() // ")")
        class default
            result_ = fail("Expected to get a DoublePrecisionInput_t")
        end select
    end function checkEqualWithinWithCloseNumbers

    pure function checkEqualWithinWithDifferentNumbers(number) result(result_)
        class(Input_t), intent(in) :: number
        type(Result_t) :: result_

        type(Mass_t) :: mass1
        type(Mass_t) :: mass2
        type(Mass_t) :: tolerance

        select type (number)
        type is (DoublePrecisionInput_t)
            mass1 = number%value_.unit.KILOGRAMS
            mass2 = (number%value_ + 0.2d0).unit.KILOGRAMS
            tolerance = 0.1d0.unit.KILOGRAMS
            result_ = assertNot( &
                    mass1%equal(mass2, within = tolerance), &
                    "(" // mass1%toString() // ")%equal(" &
                    // mass2%toString() // ", within = " &
                    // tolerance%toString() // ")")
        class default
            result_ = fail("Expected to get a DoublePrecisionInput_t")
        end select
    end function checkEqualWithinWithDifferentNumbers

    pure function checkGreaterThanOrEqualWithGreaterNumbers(number) result(result_)
        class(Input_t), intent(in) :: number
        type(Result_t) :: result_

        type(Mass_t) :: mass1
        type(Mass_t) :: mass2

        select type (number)
        type is (DoublePrecisionInput_t)
            mass1 = number%value_.unit. KILOGRAMS
            mass2 = (number%value_ - 1.0d0).unit.KILOGRAMS
            result_ = assertThat( &
                    mass1 >= mass2, &
                    mass1%toString() // " >= " // mass2%toString())
        class default
            result_ = fail("Expected to get a DoublePrecisionInput_t")
        end select
    end function checkGreaterThanOrEqualWithGreaterNumbers

    pure function checkGreaterThanOrEqualWithSameNumbers(number) result(result_)
        class(Input_t), intent(in) :: number
        type(Result_t) :: result_

        type(Mass_t) :: mass1
        type(Mass_t) :: mass2

        select type (number)
        type is (DoublePrecisionInput_t)
            mass1 = number%value_.unit.KILOGRAMS
            mass2 = number%value_.unit.KILOGRAMS
            result_ = assertThat( &
                    mass1 >= mass2, &
                    mass1%toString() // " >= " // mass2%toString())
        class default
            result_ = fail("Expected to get a DoublePrecisionInput_t")
        end select
    end function checkGreaterThanOrEqualWithSameNumbers

    pure function checkGreaterThanOrEqualWithLesserNumbers(number) result(result_)
        class(Input_t), intent(in) :: number
        type(Result_t) :: result_

        type(Mass_t) :: mass1
        type(Mass_t) :: mass2

        select type (number)
        type is (DoublePrecisionInput_t)
            mass1 = number%value_.unit.KILOGRAMS
            mass2 = (1.0d0 + number%value_).unit.KILOGRAMS
            result_ = assertNot( &
                    mass1 >= mass2, &
                    mass1%toString() // " >= " // mass2%toString())
        class default
            result_ = fail("Expected to get a DoublePrecisionInput_t")
        end select
    end function checkGreaterThanOrEqualWithLesserNumbers

    pure function checklessThanOrEqualWithLesserNumbers(number) result(result_)
        class(Input_t), intent(in) :: number
        type(Result_t) :: result_

        type(Mass_t) :: mass1
        type(Mass_t) :: mass2

        select type (number)
        type is (DoublePrecisionInput_t)
            mass1 = number%value_.unit.KILOGRAMS
            mass2 = (1.0d0 + number%value_).unit.KILOGRAMS
            result_ = assertThat( &
                    mass1 <= mass2, &
                    mass1%toString() // " <= " // mass2%toString())
        class default
            result_ = fail("Expected to get a DoublePrecisionInput_t")
        end select
    end function checklessThanOrEqualWithLesserNumbers

    pure function checkLessThanOrEqualWithSameNumbers(number) result(result_)
        class(Input_t), intent(in) :: number
        type(Result_t) :: result_

        type(Mass_t) :: mass1
        type(Mass_t) :: mass2

        select type (number)
        type is (DoublePrecisionInput_t)
            mass1 = number%value_.unit.KILOGRAMS
            mass2 = number%value_.unit.KILOGRAMS
            result_ = assertThat( &
                    mass1 <= mass2, &
                    mass1%toString() // " <= " // mass2%toString())
        class default
            result_ = fail("Expected to get a DoublePrecisionInput_t")
        end select
    end function checkLessThanOrEqualWithSameNumbers

    pure function checkLessThanOrEqualWithGreaterNumbers(number) result(result_)
        class(Input_t), intent(in) :: number
        type(Result_t) :: result_

        type(Mass_t) :: mass1
        type(Mass_t) :: mass2

        select type (number)
        type is (DoublePrecisionInput_t)
            mass1 = number%value_.unit.KILOGRAMS
            mass2 = (number%value_ - 1.0d0).unit.KILOGRAMS
            result_ = assertNot( &
                    mass1 <= mass2, &
                    mass1%toString() // " <= " // mass2%toString())
        class default
            result_ = fail("Expected to get a DoublePrecisionInput_t")
        end select
    end function checkLessThanOrEqualWithGreaterNumbers

    pure function checkGreaterThanWithGreaterNumbers(number) result(result_)
        class(Input_t), intent(in) :: number
        type(Result_t) :: result_

        type(Mass_t) :: mass1
        type(Mass_t) :: mass2

        select type (number)
        type is (DoublePrecisionInput_t)
            mass1 = number%value_.unit.KILOGRAMS
            mass2 = (number%value_ - 1.0d0).unit.KILOGRAMS
            result_ = assertThat( &
                    mass1 > mass2, &
                    mass1%toString() // " > " // mass2%toString())
        class default
            result_ = fail("Expected to get a DoublePrecisionInput_t")
        end select
    end function checkGreaterThanWithGreaterNumbers

    pure function checkGreaterThanWithSameNumbers(number) result(result_)
        class(Input_t), intent(in) :: number
        type(Result_t) :: result_

        type(Mass_t) :: mass1
        type(Mass_t) :: mass2

        select type (number)
        type is (DoublePrecisionInput_t)
            mass1 = number%value_.unit.KILOGRAMS
            mass2 = number%value_.unit.KILOGRAMS
            result_ = assertNot( &
                    mass1 > mass2, &
                    mass1%toString() // " > " // mass2%toString())
        class default
            result_ = fail("Expected to get a DoublePrecisionInput_t")
        end select
    end function checkGreaterThanWithSameNumbers

    pure function checkGreaterThanWithLesserNumbers(number) result(result_)
        class(Input_t), intent(in) :: number
        type(Result_t) :: result_

        type(Mass_t) :: mass1
        type(Mass_t) :: mass2

        select type (number)
        type is (DoublePrecisionInput_t)
            mass1 = number%value_.unit.KILOGRAMS
            mass2 = (1.0d0 + number%value_).unit.KILOGRAMS
            result_ = assertNot( &
                    mass1 > mass2, &
                    mass1%toString() // " > " // mass2%toString())
        class default
            result_ = fail("Expected to get a DoublePrecisionInput_t")
        end select
    end function checkGreaterThanWithLesserNumbers

    pure function checkLessThanWithLesserNumbers(number) result(result_)
        class(Input_t), intent(in) :: number
        type(Result_t) :: result_

        type(Mass_t) :: mass1
        type(Mass_t) :: mass2

        select type (number)
        type is (DoublePrecisionInput_t)
            mass1 = number%value_.unit.KILOGRAMS
            mass2 = (1.0d0 + number%value_).unit.KILOGRAMS
            result_ = assertThat( &
                    mass1 < mass2, &
                    mass1%toString() // " < " // mass2%toString())
        class default
            result_ = fail("Expected to get a DoublePrecisionInput_t")
        end select
    end function checkLessThanWithLesserNumbers

    pure function checkLessThanWithSameNumbers(number) result(result_)
        class(Input_t), intent(in) :: number
        type(Result_t) :: result_

        type(Mass_t) :: mass1
        type(Mass_t) :: mass2

        select type (number)
        type is (DoublePrecisionInput_t)
            mass1 = number%value_.unit.KILOGRAMS
            mass2 = number%value_.unit.KILOGRAMS
            result_ = assertNot( &
                    mass1 < mass2, &
                    mass1%toString() // " < " // mass2%toString())
        class default
            result_ = fail("Expected to get a DoublePrecisionInput_t")
        end select
    end function checkLessThanWithSameNumbers

    pure function checkLessThanWithGreaterNumbers(number) result(result_)
        class(Input_t), intent(in) :: number
        type(Result_t) :: result_

        type(Mass_t) :: mass1
        type(Mass_t) :: mass2

        select type (number)
        type is (DoublePrecisionInput_t)
            mass1 = number%value_.unit.KILOGRAMS
            mass2 = (number%value_ - 1.0d0).unit.KILOGRAMS
            result_ = assertNot( &
                    mass1 < mass2, &
                    mass1%toString() // " < " // mass2%toString())
        class default
            result_ = fail("Expected to get a DoublePrecisionInput_t")
        end select
    end function checkLessThanWithGreaterNumbers
end module mass_logic_ops_test
