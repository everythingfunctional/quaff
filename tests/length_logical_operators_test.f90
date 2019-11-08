module length_logical_operators_test
    use DoublePrecisionGenerator_m, only: DOUBLE_PRECISION_GENERATOR
    use iso_varying_string, only: operator(//)
    use Length_m, only: &
            Length_t, operator(.unit.), METERS
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
                "is equal for the same length", &
                DOUBLE_PRECISION_GENERATOR, &
                checkEqualWithSameNumber)
        individual_tests(2) = it( &
                "is not equal for different lengths", &
                DOUBLE_PRECISION_GENERATOR, &
                checkEqualWithDifferentNumbers)
        tests = describe("Length_t with operator(==)", individual_tests)
    end function test_equal_operator

    function test_not_equal_operator() result(tests)
        type(TestItem_t) :: tests

        type(TestItem_t) :: individual_tests(2)

        individual_tests(1) = it( &
                "is equal for the same length", &
                DOUBLE_PRECISION_GENERATOR, &
                checkNotEqualWithSameNumber)
        individual_tests(2) = it( &
                "is not equal for different lengths", &
                DOUBLE_PRECISION_GENERATOR, &
                checkNotEqualWithDifferentNumbers)
        tests = describe("Length_t with operator(/=)", individual_tests)
    end function test_not_equal_operator

    function test_equal_within() result(tests)
        type(TestItem_t) :: tests

        type(TestItem_t) :: individual_tests(3)

        individual_tests(1) = it( &
                "is equal for the same length even for tiny tolerance", &
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
        tests = describe("Length_t%equal with tolerance", individual_tests)
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
        tests = describe("Length_t operator (>=)", &
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
        tests = describe("Length_t operator (<=)", &
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
        tests = describe("Length_t operator (>)", &
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
        tests = describe("Length_t operator (<)", &
                individual_tests)
    end function test_less_than_operator

    function checkEqualWithSameNumber(number) result(result_)
        class(Input_t), intent(in) :: number
        type(Result_t) :: result_

        type(Length_t) :: the_length

        select type (number)
        type is (DoublePrecisionInput_t)
            the_length = number%value_.unit.METERS
            result_ = assertThat( &
                    the_length == the_length, &
                    the_length%toString() // " == " // the_length%toString())
        class default
            result_ = fail("Expected to get a DoublePrecisionInput_t")
        end select
    end function checkEqualWithSameNumber

    function checkEqualWithDifferentNumbers(number) result(result_)
        class(Input_t), intent(in) :: number
        type(Result_t) :: result_

        type(Length_t) :: length1
        type(Length_t) :: length2

        select type (number)
        type is (DoublePrecisionInput_t)
            length1 = number%value_.unit.METERS
            length2 = (1.0d0 + number%value_).unit.METERS
            result_ = assertNot( &
                    length1 == length2, &
                    length1%toString() // " == " // length2%toString())
        class default
            result_ = fail("Expected to get a DoublePrecisionInput_t")
        end select
    end function checkEqualWithDifferentNumbers

    function checkNotEqualWithSameNumber(number) result(result_)
        class(Input_t), intent(in) :: number
        type(Result_t) :: result_

        type(Length_t) :: the_length

        select type (number)
        type is (DoublePrecisionInput_t)
            the_length = number%value_.unit.METERS
            result_ = assertNot( &
                    the_length /= the_length, &
                    the_length%toString() // " /= " // the_length%toString())
        class default
            result_ = fail("Expected to get a DoublePrecisionInput_t")
        end select
    end function checkNotEqualWithSameNumber

    function checkNotEqualWithDifferentNumbers(number) result(result_)
        class(Input_t), intent(in) :: number
        type(Result_t) :: result_

        type(Length_t) :: length1
        type(Length_t) :: length2

        select type (number)
        type is (DoublePrecisionInput_t)
            length1 = number%value_.unit.METERS
            length2 = (1.0d0 + number%value_).unit.METERS
            result_ = assertThat( &
                    length1 /= length2, &
                    length1%toString() // " /= " // length2%toString())
        class default
            result_ = fail("Expected to get a DoublePrecisionInput_t")
        end select
    end function checkNotEqualWithDifferentNumbers

    function checkEqualWithinWithSameNumber(number) result(result_)
        class(Input_t), intent(in) :: number
        type(Result_t) :: result_

        type(Length_t) :: the_length
        type(Length_t) :: tolerance

        select type (number)
        type is (DoublePrecisionInput_t)
            the_length = number%value_.unit.METERS
            tolerance = TINY(1.0d0).unit.METERS
            result_ = assertThat( &
                    the_length%equal(the_length, within = tolerance), &
                    "(" // the_length%toString() // ")%equal(" &
                        // the_length%toString() // ", within = " &
                        // tolerance%toString() // ")")
        class default
            result_ = fail("Expected to get a DoublePrecisionInput_t")
        end select
    end function checkEqualWithinWithSameNumber

    function checkEqualWithinWithCloseNumbers(number) result(result_)
        class(Input_t), intent(in) :: number
        type(Result_t) :: result_

        type(Length_t) :: length1
        type(Length_t) :: length2
        type(Length_t) :: tolerance

        select type (number)
        type is (DoublePrecisionInput_t)
            length1 = number%value_.unit.METERS
            length2 = (number%value_ + 0.05d0).unit.METERS
            tolerance = 0.1d0.unit.METERS
            result_ = assertThat( &
                    length1%equal(length2, within = tolerance), &
                    "(" // length1%toString() // ")%equal(" &
                        // length2%toString() // ", within = " &
                        // tolerance%toString() // ")")
        class default
            result_ = fail("Expected to get a DoublePrecisionInput_t")
        end select
    end function checkEqualWithinWithCloseNumbers

    function checkEqualWithinWithDifferentNumbers(number) result(result_)
        class(Input_t), intent(in) :: number
        type(Result_t) :: result_

        type(Length_t) :: length1
        type(Length_t) :: length2
        type(Length_t) :: tolerance

        select type (number)
        type is (DoublePrecisionInput_t)
            length1 = number%value_.unit.METERS
            length2 = (number%value_ + 0.2d0).unit.METERS
            tolerance = 0.1d0.unit.METERS
            result_ = assertNot( &
                    length1%equal(length2, within = tolerance), &
                    "(" // length1%toString() // ")%equal(" &
                    // length2%toString() // ", within = " &
                    // tolerance%toString() // ")")
        class default
            result_ = fail("Expected to get a DoublePrecisionInput_t")
        end select
    end function checkEqualWithinWithDifferentNumbers

    function checkGreaterThanOrEqualWithGreaterNumbers(number) result(result_)
        class(Input_t), intent(in) :: number
        type(Result_t) :: result_

        type(Length_t) :: length1
        type(Length_t) :: length2

        select type (number)
        type is (DoublePrecisionInput_t)
            length1 = number%value_.unit. METERS
            length2 = (number%value_ - 1.0d0).unit.METERS
            result_ = assertThat( &
                    length1 >= length2, &
                    length1%toString() // " >= " // length2%toString())
        class default
            result_ = fail("Expected to get a DoublePrecisionInput_t")
        end select
    end function checkGreaterThanOrEqualWithGreaterNumbers

    function checkGreaterThanOrEqualWithSameNumbers(number) result(result_)
        class(Input_t), intent(in) :: number
        type(Result_t) :: result_

        type(Length_t) :: length1
        type(Length_t) :: length2

        select type (number)
        type is (DoublePrecisionInput_t)
            length1 = number%value_.unit.METERS
            length2 = number%value_.unit.METERS
            result_ = assertThat( &
                    length1 >= length2, &
                    length1%toString() // " >= " // length2%toString())
        class default
            result_ = fail("Expected to get a DoublePrecisionInput_t")
        end select
    end function checkGreaterThanOrEqualWithSameNumbers

    function checkGreaterThanOrEqualWithLesserNumbers(number) result(result_)
        class(Input_t), intent(in) :: number
        type(Result_t) :: result_

        type(Length_t) :: length1
        type(Length_t) :: length2

        select type (number)
        type is (DoublePrecisionInput_t)
            length1 = number%value_.unit.METERS
            length2 = (1.0d0 + number%value_).unit.METERS
            result_ = assertNot( &
                    length1 >= length2, &
                    length1%toString() // " >= " // length2%toString())
        class default
            result_ = fail("Expected to get a DoublePrecisionInput_t")
        end select
    end function checkGreaterThanOrEqualWithLesserNumbers

    function checklessThanOrEqualWithLesserNumbers(number) result(result_)
        class(Input_t), intent(in) :: number
        type(Result_t) :: result_

        type(Length_t) :: length1
        type(Length_t) :: length2

        select type (number)
        type is (DoublePrecisionInput_t)
            length1 = number%value_.unit.METERS
            length2 = (1.0d0 + number%value_).unit.METERS
            result_ = assertThat( &
                    length1 <= length2, &
                    length1%toString() // " <= " // length2%toString())
        class default
            result_ = fail("Expected to get a DoublePrecisionInput_t")
        end select
    end function checklessThanOrEqualWithLesserNumbers

    function checkLessThanOrEqualWithSameNumbers(number) result(result_)
        class(Input_t), intent(in) :: number
        type(Result_t) :: result_

        type(Length_t) :: length1
        type(Length_t) :: length2

        select type (number)
        type is (DoublePrecisionInput_t)
            length1 = number%value_.unit.METERS
            length2 = number%value_.unit.METERS
            result_ = assertThat( &
                    length1 <= length2, &
                    length1%toString() // " <= " // length2%toString())
        class default
            result_ = fail("Expected to get a DoublePrecisionInput_t")
        end select
    end function checkLessThanOrEqualWithSameNumbers

    function checkLessThanOrEqualWithGreaterNumbers(number) result(result_)
        class(Input_t), intent(in) :: number
        type(Result_t) :: result_

        type(Length_t) :: length1
        type(Length_t) :: length2

        select type (number)
        type is (DoublePrecisionInput_t)
            length1 = number%value_.unit.METERS
            length2 = (number%value_ - 1.0d0).unit.METERS
            result_ = assertNot( &
                    length1 <= length2, &
                    length1%toString() // " <= " // length2%toString())
        class default
            result_ = fail("Expected to get a DoublePrecisionInput_t")
        end select
    end function checkLessThanOrEqualWithGreaterNumbers

    function checkGreaterThanWithGreaterNumbers(number) result(result_)
        class(Input_t), intent(in) :: number
        type(Result_t) :: result_

        type(Length_t) :: length1
        type(Length_t) :: length2

        select type (number)
        type is (DoublePrecisionInput_t)
            length1 = number%value_.unit.METERS
            length2 = (number%value_ - 1.0d0).unit.METERS
            result_ = assertThat( &
                    length1 > length2, &
                    length1%toString() // " > " // length2%toString())
        class default
            result_ = fail("Expected to get a DoublePrecisionInput_t")
        end select
    end function checkGreaterThanWithGreaterNumbers

    function checkGreaterThanWithSameNumbers(number) result(result_)
        class(Input_t), intent(in) :: number
        type(Result_t) :: result_

        type(Length_t) :: length1
        type(Length_t) :: length2

        select type (number)
        type is (DoublePrecisionInput_t)
            length1 = number%value_.unit.METERS
            length2 = number%value_.unit.METERS
            result_ = assertNot( &
                    length1 > length2, &
                    length1%toString() // " > " // length2%toString())
        class default
            result_ = fail("Expected to get a DoublePrecisionInput_t")
        end select
    end function checkGreaterThanWithSameNumbers

    function checkGreaterThanWithLesserNumbers(number) result(result_)
        class(Input_t), intent(in) :: number
        type(Result_t) :: result_

        type(Length_t) :: length1
        type(Length_t) :: length2

        select type (number)
        type is (DoublePrecisionInput_t)
            length1 = number%value_.unit.METERS
            length2 = (1.0d0 + number%value_).unit.METERS
            result_ = assertNot( &
                    length1 > length2, &
                    length1%toString() // " > " // length2%toString())
        class default
            result_ = fail("Expected to get a DoublePrecisionInput_t")
        end select
    end function checkGreaterThanWithLesserNumbers

    function checkLessThanWithLesserNumbers(number) result(result_)
        class(Input_t), intent(in) :: number
        type(Result_t) :: result_

        type(Length_t) :: length1
        type(Length_t) :: length2

        select type (number)
        type is (DoublePrecisionInput_t)
            length1 = number%value_.unit.METERS
            length2 = (1.0d0 + number%value_).unit.METERS
            result_ = assertThat( &
                    length1 < length2, &
                    length1%toString() // " < " // length2%toString())
        class default
            result_ = fail("Expected to get a DoublePrecisionInput_t")
        end select
    end function checkLessThanWithLesserNumbers

    function checkLessThanWithSameNumbers(number) result(result_)
        class(Input_t), intent(in) :: number
        type(Result_t) :: result_

        type(Length_t) :: length1
        type(Length_t) :: length2

        select type (number)
        type is (DoublePrecisionInput_t)
            length1 = number%value_.unit.METERS
            length2 = number%value_.unit.METERS
            result_ = assertNot( &
                    length1 < length2, &
                    length1%toString() // " < " // length2%toString())
        class default
            result_ = fail("Expected to get a DoublePrecisionInput_t")
        end select
    end function checkLessThanWithSameNumbers

    function checkLessThanWithGreaterNumbers(number) result(result_)
        class(Input_t), intent(in) :: number
        type(Result_t) :: result_

        type(Length_t) :: length1
        type(Length_t) :: length2

        select type (number)
        type is (DoublePrecisionInput_t)
            length1 = number%value_.unit.METERS
            length2 = (number%value_ - 1.0d0).unit.METERS
            result_ = assertNot( &
                    length1 < length2, &
                    length1%toString() // " < " // length2%toString())
        class default
            result_ = fail("Expected to get a DoublePrecisionInput_t")
        end select
    end function checkLessThanWithGreaterNumbers
end module length_logical_operators_test
