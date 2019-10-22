module volume_logical_operators_test
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
        use DoublePrecisionGenerator_m, only: DOUBLE_PRECISION_GENERATOR
        use Vegetables_m, only: TestItem_t, Describe, It

        type(TestItem_t) :: tests

        type(TestItem_t) :: individual_tests(2)

        individual_tests(1) = it( &
                "is equal for the same volume", &
                DOUBLE_PRECISION_GENERATOR, &
                checkEqualWithSameNumber)
        individual_tests(2) = it( &
                "is not equal for different volumes", &
                DOUBLE_PRECISION_GENERATOR, &
                checkEqualWithDifferentNumbers)
        tests = describe("Volume_t with operator(==)", individual_tests)
    end function test_equal_operator

    function test_not_equal_operator() result(tests)
        use DoublePrecisionGenerator_m, only: DOUBLE_PRECISION_GENERATOR
        use Vegetables_m, only: TestItem_t, Describe, It

        type(TestItem_t) :: tests

        type(TestItem_t) :: individual_tests(2)

        individual_tests(1) = it( &
                "is equal for the same volume", &
                DOUBLE_PRECISION_GENERATOR, &
                checkNotEqualWithSameNumber)
        individual_tests(2) = it( &
                "is not equal for different volumes", &
                DOUBLE_PRECISION_GENERATOR, &
                checkNotEqualWithDifferentNumbers)
        tests = describe("Volume_t with operator(/=)", individual_tests)
    end function test_not_equal_operator

    function test_equal_within() result(tests)
        use DoublePrecisionGenerator_m, only: DOUBLE_PRECISION_GENERATOR
        use Vegetables_m, only: TestItem_t, Describe, It

        type(TestItem_t) :: tests

        type(TestItem_t) :: individual_tests(3)

        individual_tests(1) = it( &
                "is equal for the same volume even for tiny tolerance", &
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
        tests = describe("Volume_t%equal with tolerance", individual_tests)
    end function test_equal_within

    function test_greater_than_or_equal_operator() result(tests)
        use DoublePrecisionGenerator_m, only: DOUBLE_PRECISION_GENERATOR
        use Vegetables_m, only: TestItem_t, Describe, It

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
        tests = describe("Volume_t operator (>=)", &
                individual_tests)
    end function test_greater_than_or_equal_operator

    function test_less_than_or_equal_operator() result(tests)
        use DoublePrecisionGenerator_m, only: DOUBLE_PRECISION_GENERATOR
        use Vegetables_m, only: TestItem_t, Describe, It

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
        tests = describe("Volume_t operator (<=)", &
                individual_tests)
    end function test_less_than_or_equal_operator

    function test_greater_than_operator() result(tests)
        use DoublePrecisionGenerator_m, only: DOUBLE_PRECISION_GENERATOR
        use Vegetables_m, only: TestItem_t, Describe, It

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
        tests = describe("Volume_t operator (>)", &
                individual_tests)
    end function test_greater_than_operator

    function test_less_than_operator() result(tests)
        use DoublePrecisionGenerator_m, only: DOUBLE_PRECISION_GENERATOR
        use Vegetables_m, only: TestItem_t, Describe, It

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
        tests = describe("Volume_t operator (<)", &
                individual_tests)
    end function test_less_than_operator

    function checkEqualWithSameNumber(number) result(result_)
        use iso_varying_string, only: operator(//)
        use Volume_m, only: &
                Volume_t, operator(.unit.), CUBIC_METERS
        use Vegetables_m, only: &
                DoublePrecisionInput_t, Input_t, Result_t, assertThat, fail

        class(Input_t), intent(in) :: number
        type(Result_t) :: result_

        type(Volume_t) :: the_volume

        select type (number)
        type is (DoublePrecisionInput_t)
            the_volume = number%value_.unit.CUBIC_METERS
            result_ = assertThat( &
                    the_volume == the_volume, &
                    the_volume%toString() // " == " // the_volume%toString())
        class default
            result_ = fail("Expected to get a DoublePrecisionInput_t")
        end select
    end function checkEqualWithSameNumber

    function checkEqualWithDifferentNumbers(number) result(result_)
        use iso_varying_string, only: operator(//)
        use Volume_m, only: &
                Volume_t, operator(.unit.), CUBIC_METERS
        use Vegetables_m, only: &
                DoublePrecisionInput_t, Input_t, Result_t, assertNot, fail

        class(Input_t), intent(in) :: number
        type(Result_t) :: result_

        type(Volume_t) :: volume1
        type(Volume_t) :: volume2

        select type (number)
        type is (DoublePrecisionInput_t)
            volume1 = number%value_.unit.CUBIC_METERS
            volume2 = (1.0d0 + number%value_).unit.CUBIC_METERS
            result_ = assertNot( &
                    volume1 == volume2, &
                    volume1%toString() // " == " // volume2%toString())
        class default
            result_ = fail("Expected to get a DoublePrecisionInput_t")
        end select
    end function checkEqualWithDifferentNumbers

    function checkNotEqualWithSameNumber(number) result(result_)
        use iso_varying_string, only: operator(//)
        use Volume_m, only: &
                Volume_t, operator(.unit.), CUBIC_METERS
        use Vegetables_m, only: &
                DoublePrecisionInput_t, Input_t, Result_t, assertNot, fail

        class(Input_t), intent(in) :: number
        type(Result_t) :: result_

        type(Volume_t) :: the_volume

        select type (number)
        type is (DoublePrecisionInput_t)
            the_volume = number%value_.unit.CUBIC_METERS
            result_ = assertNot( &
                    the_volume /= the_volume, &
                    the_volume%toString() // " /= " // the_volume%toString())
        class default
            result_ = fail("Expected to get a DoublePrecisionInput_t")
        end select
    end function checkNotEqualWithSameNumber

    function checkNotEqualWithDifferentNumbers(number) result(result_)
        use iso_varying_string, only: operator(//)
        use Volume_m, only: &
                Volume_t, operator(.unit.), CUBIC_METERS
        use Vegetables_m, only: &
                DoublePrecisionInput_t, Input_t, Result_t, assertThat, fail

        class(Input_t), intent(in) :: number
        type(Result_t) :: result_

        type(Volume_t) :: volume1
        type(Volume_t) :: volume2

        select type (number)
        type is (DoublePrecisionInput_t)
            volume1 = number%value_.unit.CUBIC_METERS
            volume2 = (1.0d0 + number%value_).unit.CUBIC_METERS
            result_ = assertThat( &
                    volume1 /= volume2, &
                    volume1%toString() // " /= " // volume2%toString())
        class default
            result_ = fail("Expected to get a DoublePrecisionInput_t")
        end select
    end function checkNotEqualWithDifferentNumbers

    function checkEqualWithinWithSameNumber(number) result(result_)
        use iso_varying_string, only: operator(//)
        use Volume_m, only: &
                Volume_t, operator(.unit.), CUBIC_METERS
        use Vegetables_m, only: &
                DoublePrecisionInput_t, Input_t, Result_t, assertThat, fail

        class(Input_t), intent(in) :: number
        type(Result_t) :: result_

        type(Volume_t) :: the_volume
        type(Volume_t) :: tolerance

        select type (number)
        type is (DoublePrecisionInput_t)
            the_volume = number%value_.unit.CUBIC_METERS
            tolerance = TINY(1.0d0).unit.CUBIC_METERS
            result_ = assertThat( &
                    the_volume%equal(the_volume, within = tolerance), &
                    "(" // the_volume%toString() // ")%equal(" &
                        // the_volume%toString() // ", within = " &
                        // tolerance%toString() // ")")
        class default
            result_ = fail("Expected to get a DoublePrecisionInput_t")
        end select
    end function checkEqualWithinWithSameNumber

    function checkEqualWithinWithCloseNumbers(number) result(result_)
        use iso_varying_string, only: operator(//)
        use Volume_m, only: &
                Volume_t, operator(.unit.), CUBIC_METERS
        use Vegetables_m, only: &
                DoublePrecisionInput_t, Input_t, Result_t, assertThat, fail

        class(Input_t), intent(in) :: number
        type(Result_t) :: result_

        type(Volume_t) :: volume1
        type(Volume_t) :: volume2
        type(Volume_t) :: tolerance

        select type (number)
        type is (DoublePrecisionInput_t)
            volume1 = number%value_.unit.CUBIC_METERS
            volume2 = (number%value_ + 0.05d0).unit.CUBIC_METERS
            tolerance = 0.1d0.unit.CUBIC_METERS
            result_ = assertThat( &
                    volume1%equal(volume2, within = tolerance), &
                    "(" // volume1%toString() // ")%equal(" &
                        // volume2%toString() // ", within = " &
                        // tolerance%toString() // ")")
        class default
            result_ = fail("Expected to get a DoublePrecisionInput_t")
        end select
    end function checkEqualWithinWithCloseNumbers

    function checkEqualWithinWithDifferentNumbers(number) result(result_)
        use iso_varying_string, only: operator(//)
        use Volume_m, only: &
                Volume_t, operator(.unit.), CUBIC_METERS
        use Vegetables_m, only: &
                DoublePrecisionInput_t, Input_t, Result_t, assertNot, fail

        class(Input_t), intent(in) :: number
        type(Result_t) :: result_

        type(Volume_t) :: volume1
        type(Volume_t) :: volume2
        type(Volume_t) :: tolerance

        select type (number)
        type is (DoublePrecisionInput_t)
            volume1 = number%value_.unit.CUBIC_METERS
            volume2 = (number%value_ + 0.2d0).unit.CUBIC_METERS
            tolerance = 0.1d0.unit.CUBIC_METERS
            result_ = assertNot( &
                    volume1%equal(volume2, within = tolerance), &
                    "(" // volume1%toString() // ")%equal(" &
                    // volume2%toString() // ", within = " &
                    // tolerance%toString() // ")")
        class default
            result_ = fail("Expected to get a DoublePrecisionInput_t")
        end select
    end function checkEqualWithinWithDifferentNumbers

    function checkGreaterThanOrEqualWithGreaterNumbers(number) result(result_)
        use iso_varying_string, only: operator(//)
        use Volume_m, only: &
                Volume_t, operator(.unit.), CUBIC_METERS
        use Vegetables_m, only: &
                DoublePrecisionInput_t, Input_t, Result_t, assertThat, fail

        class(Input_t), intent(in) :: number
        type(Result_t) :: result_

        type(Volume_t) :: volume1
        type(Volume_t) :: volume2

        select type (number)
        type is (DoublePrecisionInput_t)
            volume1 = number%value_.unit. CUBIC_METERS
            volume2 = (number%value_ - 1.0d0).unit.CUBIC_METERS
            result_ = assertThat( &
                    volume1 >= volume2, &
                    volume1%toString() // " >= " // volume2%toString())
        class default
            result_ = fail("Expected to get a DoublePrecisionInput_t")
        end select
    end function checkGreaterThanOrEqualWithGreaterNumbers

    function checkGreaterThanOrEqualWithSameNumbers(number) result(result_)
        use iso_varying_string, only: operator(//)
        use Volume_m, only: &
                Volume_t, operator(.unit.), CUBIC_METERS
        use Vegetables_m, only: &
                DoublePrecisionInput_t, Input_t, Result_t, assertThat, fail

        class(Input_t), intent(in) :: number
        type(Result_t) :: result_

        type(Volume_t) :: volume1
        type(Volume_t) :: volume2

        select type (number)
        type is (DoublePrecisionInput_t)
            volume1 = number%value_.unit.CUBIC_METERS
            volume2 = number%value_.unit.CUBIC_METERS
            result_ = assertThat( &
                    volume1 >= volume2, &
                    volume1%toString() // " >= " // volume2%toString())
        class default
            result_ = fail("Expected to get a DoublePrecisionInput_t")
        end select
    end function checkGreaterThanOrEqualWithSameNumbers

    function checkGreaterThanOrEqualWithLesserNumbers(number) result(result_)
        use iso_varying_string, only: operator(//)
        use Volume_m, only: &
                Volume_t, operator(.unit.), CUBIC_METERS
        use Vegetables_m, only: &
                DoublePrecisionInput_t, Input_t, Result_t, assertNot, fail

        class(Input_t), intent(in) :: number
        type(Result_t) :: result_

        type(Volume_t) :: volume1
        type(Volume_t) :: volume2

        select type (number)
        type is (DoublePrecisionInput_t)
            volume1 = number%value_.unit.CUBIC_METERS
            volume2 = (1.0d0 + number%value_).unit.CUBIC_METERS
            result_ = assertNot( &
                    volume1 >= volume2, &
                    volume1%toString() // " >= " // volume2%toString())
        class default
            result_ = fail("Expected to get a DoublePrecisionInput_t")
        end select
    end function checkGreaterThanOrEqualWithLesserNumbers

    function checklessThanOrEqualWithLesserNumbers(number) result(result_)
        use iso_varying_string, only: operator(//)
        use Volume_m, only: &
                Volume_t, operator(.unit.), CUBIC_METERS
        use Vegetables_m, only: &
                DoublePrecisionInput_t, Input_t, Result_t, assertThat, fail

        class(Input_t), intent(in) :: number
        type(Result_t) :: result_

        type(Volume_t) :: volume1
        type(Volume_t) :: volume2

        select type (number)
        type is (DoublePrecisionInput_t)
            volume1 = number%value_.unit.CUBIC_METERS
            volume2 = (1.0d0 + number%value_).unit.CUBIC_METERS
            result_ = assertThat( &
                    volume1 <= volume2, &
                    volume1%toString() // " <= " // volume2%toString())
        class default
            result_ = fail("Expected to get a DoublePrecisionInput_t")
        end select
    end function checklessThanOrEqualWithLesserNumbers

    function checkLessThanOrEqualWithSameNumbers(number) result(result_)
        use iso_varying_string, only: operator(//)
        use Volume_m, only: &
                Volume_t, operator(.unit.), CUBIC_METERS
        use Vegetables_m, only: &
                DoublePrecisionInput_t, Input_t, Result_t, assertThat, fail

        class(Input_t), intent(in) :: number
        type(Result_t) :: result_

        type(Volume_t) :: volume1
        type(Volume_t) :: volume2

        select type (number)
        type is (DoublePrecisionInput_t)
            volume1 = number%value_.unit.CUBIC_METERS
            volume2 = number%value_.unit.CUBIC_METERS
            result_ = assertThat( &
                    volume1 <= volume2, &
                    volume1%toString() // " <= " // volume2%toString())
        class default
            result_ = fail("Expected to get a DoublePrecisionInput_t")
        end select
    end function checkLessThanOrEqualWithSameNumbers

    function checkLessThanOrEqualWithGreaterNumbers(number) result(result_)
        use iso_varying_string, only: operator(//)
        use Volume_m, only: &
                Volume_t, operator(.unit.), CUBIC_METERS
        use Vegetables_m, only: &
                DoublePrecisionInput_t, Input_t, Result_t, assertNot, fail

        class(Input_t), intent(in) :: number
        type(Result_t) :: result_

        type(Volume_t) :: volume1
        type(Volume_t) :: volume2

        select type (number)
        type is (DoublePrecisionInput_t)
            volume1 = number%value_.unit.CUBIC_METERS
            volume2 = (number%value_ - 1.0d0).unit.CUBIC_METERS
            result_ = assertNot( &
                    volume1 <= volume2, &
                    volume1%toString() // " <= " // volume2%toString())
        class default
            result_ = fail("Expected to get a DoublePrecisionInput_t")
        end select
    end function checkLessThanOrEqualWithGreaterNumbers

    function checkGreaterThanWithGreaterNumbers(number) result(result_)
        use iso_varying_string, only: operator(//)
        use Volume_m, only: &
                Volume_t, operator(.unit.), CUBIC_METERS
        use Vegetables_m, only: &
                DoublePrecisionInput_t, Input_t, Result_t, assertThat, fail

        class(Input_t), intent(in) :: number
        type(Result_t) :: result_

        type(Volume_t) :: volume1
        type(Volume_t) :: volume2

        select type (number)
        type is (DoublePrecisionInput_t)
            volume1 = number%value_.unit.CUBIC_METERS
            volume2 = (number%value_ - 1.0d0).unit.CUBIC_METERS
            result_ = assertThat( &
                    volume1 > volume2, &
                    volume1%toString() // " > " // volume2%toString())
        class default
            result_ = fail("Expected to get a DoublePrecisionInput_t")
        end select
    end function checkGreaterThanWithGreaterNumbers

    function checkGreaterThanWithSameNumbers(number) result(result_)
        use iso_varying_string, only: operator(//)
        use Volume_m, only: &
                Volume_t, operator(.unit.), CUBIC_METERS
        use Vegetables_m, only: &
                DoublePrecisionInput_t, Input_t, Result_t, assertNot, fail

        class(Input_t), intent(in) :: number
        type(Result_t) :: result_

        type(Volume_t) :: volume1
        type(Volume_t) :: volume2

        select type (number)
        type is (DoublePrecisionInput_t)
            volume1 = number%value_.unit.CUBIC_METERS
            volume2 = number%value_.unit.CUBIC_METERS
            result_ = assertNot( &
                    volume1 > volume2, &
                    volume1%toString() // " > " // volume2%toString())
        class default
            result_ = fail("Expected to get a DoublePrecisionInput_t")
        end select
    end function checkGreaterThanWithSameNumbers

    function checkGreaterThanWithLesserNumbers(number) result(result_)
        use iso_varying_string, only: operator(//)
        use Volume_m, only: &
                Volume_t, operator(.unit.), CUBIC_METERS
        use Vegetables_m, only: &
                DoublePrecisionInput_t, Input_t, Result_t, assertNot, fail

        class(Input_t), intent(in) :: number
        type(Result_t) :: result_

        type(Volume_t) :: volume1
        type(Volume_t) :: volume2

        select type (number)
        type is (DoublePrecisionInput_t)
            volume1 = number%value_.unit.CUBIC_METERS
            volume2 = (1.0d0 + number%value_).unit.CUBIC_METERS
            result_ = assertNot( &
                    volume1 > volume2, &
                    volume1%toString() // " > " // volume2%toString())
        class default
            result_ = fail("Expected to get a DoublePrecisionInput_t")
        end select
    end function checkGreaterThanWithLesserNumbers

    function checkLessThanWithLesserNumbers(number) result(result_)
        use iso_varying_string, only: operator(//)
        use Volume_m, only: &
                Volume_t, operator(.unit.), CUBIC_METERS
        use Vegetables_m, only: &
                DoublePrecisionInput_t, Input_t, Result_t, assertThat, fail

        class(Input_t), intent(in) :: number
        type(Result_t) :: result_

        type(Volume_t) :: volume1
        type(Volume_t) :: volume2

        select type (number)
        type is (DoublePrecisionInput_t)
            volume1 = number%value_.unit.CUBIC_METERS
            volume2 = (1.0d0 + number%value_).unit.CUBIC_METERS
            result_ = assertThat( &
                    volume1 < volume2, &
                    volume1%toString() // " < " // volume2%toString())
        class default
            result_ = fail("Expected to get a DoublePrecisionInput_t")
        end select
    end function checkLessThanWithLesserNumbers

    function checkLessThanWithSameNumbers(number) result(result_)
        use iso_varying_string, only: operator(//)
        use Volume_m, only: &
                Volume_t, operator(.unit.), CUBIC_METERS
        use Vegetables_m, only: &
                DoublePrecisionInput_t, Input_t, Result_t, assertNot, fail

        class(Input_t), intent(in) :: number
        type(Result_t) :: result_

        type(Volume_t) :: volume1
        type(Volume_t) :: volume2

        select type (number)
        type is (DoublePrecisionInput_t)
            volume1 = number%value_.unit.CUBIC_METERS
            volume2 = number%value_.unit.CUBIC_METERS
            result_ = assertNot( &
                    volume1 < volume2, &
                    volume1%toString() // " < " // volume2%toString())
        class default
            result_ = fail("Expected to get a DoublePrecisionInput_t")
        end select
    end function checkLessThanWithSameNumbers

    function checkLessThanWithGreaterNumbers(number) result(result_)
        use iso_varying_string, only: operator(//)
        use Volume_m, only: &
                Volume_t, operator(.unit.), CUBIC_METERS
        use Vegetables_m, only: &
                DoublePrecisionInput_t, Input_t, Result_t, assertNot, fail

        class(Input_t), intent(in) :: number
        type(Result_t) :: result_

        type(Volume_t) :: volume1
        type(Volume_t) :: volume2

        select type (number)
        type is (DoublePrecisionInput_t)
            volume1 = number%value_.unit.CUBIC_METERS
            volume2 = (number%value_ - 1.0d0).unit.CUBIC_METERS
            result_ = assertNot( &
                    volume1 < volume2, &
                    volume1%toString() // " < " // volume2%toString())
        class default
            result_ = fail("Expected to get a DoublePrecisionInput_t")
        end select
    end function checkLessThanWithGreaterNumbers
end module volume_logical_operators_test
