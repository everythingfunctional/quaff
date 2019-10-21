module angle_type_test
    implicit none
    private

    public :: test_angle
contains
    function test_angle() result(tests)
        use Angle_m, only: PROVIDED_UNITS
        use Units_examples_m, only: UnitsExamples_t, makeUnitsExamples
        use Vegetables_m, only: TestItem_t, describe, it

        type(TestItem_t) :: tests

        type(UnitsExamples_t) :: examples
        type(TestItem_t) :: individual_tests(6)

        examples = makeUnitsExamples(PROVIDED_UNITS)
        individual_tests(1) = it( &
                "gets the same value given the same units", &
                examples%units, &
                checkRoundTrip)
        individual_tests(2) = it( &
                "the conversion factors between 2 units are inverses", &
                examples%pairs, &
                checkConversionFactorsInverse)
        individual_tests(3) = it( &
                "can be converted to and from a string", &
                examples%units, &
                checkToAndFromString)
        individual_tests(4) = it( &
                "Trying to parse a bad string is an error", &
                checkBadString)
        individual_tests(5) = it( &
                "Trying to parse with an unknown unit is an error", &
                checkBadUnit)
        individual_tests(6) = it( &
                "Trying to parse a bad number is an error", &
                checkBadNumber)
        tests = describe("Angle_t", individual_tests)
    end function test_angle

    function checkRoundTrip(units) result(result_)
        use Check_round_trip_in_m, only: checkRoundTripIn
        use Angle_m, only: AngleUnit_t
        use Units_examples_m, only: UnitsInput_t
        use Vegetables_m, only: Input_t, Result_t, fail

        class(Input_t), intent(in) :: units
        type(Result_t) :: result_

        select type (units)
        type is (UnitsInput_t)
            select type (the_unit => units%unit)
            type is (AngleUnit_t)
                result_ = checkRoundTripIn(the_unit)
            class default
                result_ = fail("Expected to get an AngleUnit_t")
            end select
        class default
            result_ = fail("Expected to get an UnitsInput_t")
        end select
    end function checkRoundTrip

    function checkConversionFactorsInverse(pair) result(result_)
        use Check_conversion_factor_m, only: checkConversionFactorsAreInverse
        use Angle_m, only: AngleUnit_t
        use Units_examples_m, only: UnitsPairInput_t
        use Vegetables_m, only: Input_t, Result_t, fail

        class(Input_t), intent(in) :: pair
        type(Result_t) :: result_

        select type (pair)
        type is (UnitsPairInput_t)
            select type (first => pair%first)
            type is (AngleUnit_t)
                select type (second => pair%second)
                type is (AngleUnit_t)
                    result_ = checkConversionFactorsAreInverse(first, second)
                class default
                    result_ = fail("Expected second in pair to be AngleUnit_t")
                end select
            class default
                result_ = fail("Expected first in pair to be AngleUnit_t")
            end select
        class default
            result_ = fail("Expected to get a UnitsPairInput_t")
        end select
    end function checkConversionFactorsInverse

    function checkToAndFromString(units) result(result_)
        use Error_list_m, only: ErrorList_t, size
        use Angle_m, only: &
                Angle_t, &
                AngleUnit_t, &
                operator(.unit.), &
                angleFromString
        use Angle_asserts_m, only: assertEquals
        use Units_examples_m, only: UnitsInput_t
        use Vegetables_m, only: Input_t, Result_t, assertEquals, fail

        class(Input_t), intent(in) :: units
        type(Result_t) :: result_

        type(ErrorList_t) :: errors
        type(Angle_t) :: original_angle
        type(Angle_t) :: new_angle

        select type (units)
        type is (UnitsInput_t)
            select type (the_unit => units%unit)
            type is (AngleUnit_t)
                original_angle = 3.0d0.unit.the_unit
                new_angle = angleFromString( &
                        original_angle%toStringIn(the_unit), errors)
                result_ = &
                        assertEquals( &
                                original_angle, &
                                new_angle, &
                                the_unit%toString()) &
                        .and.assertEquals(0, size(errors))
            class default
                result_ = fail("Expected to get an AngleUnit_t")
            end select
        class default
            result_ = fail("Expected to get an UnitsInput_t")
        end select
    end function checkToAndFromString

    function checkBadString() result(result_)
        use Error_list_m, only: ErrorList_t
        use Angle_m, only: Angle_t, angleFromString
        use Miscellaneous_m, only: PARSE_ERROR
        use Vegetables_m, only: Result_t, assertThat

        type(Result_t) :: result_

        type(ErrorList_t) :: errors
        type(Angle_t) :: length

        length = angleFromString("bad", errors)
        result_ = assertThat(errors.hasType.PARSE_ERROR, errors%toString())
    end function checkBadString

    function checkBadUnit() result(result_)
        use Error_list_m, only: ErrorList_t
        use Angle_m, only: &
                Angle_t, angleFromString, RADIANS
        use Miscellaneous_m, only: UNKNOWN_UNIT
        use Vegetables_m, only: Result_t, assertThat

        type(Result_t) :: result_

        type(ErrorList_t) :: errors
        type(Angle_t) :: length

        length = angleFromString("1.0 bad", [RADIANS], errors)
        result_ = assertThat(errors.hasType.UNKNOWN_UNIT, errors%toString())
    end function checkBadUnit

    function checkBadNumber() result(result_)
        use Error_list_m, only: ErrorList_t
        use Angle_m, only: Angle_t, angleFromString
        use Miscellaneous_m, only: PARSE_ERROR
        use Vegetables_m, only: Result_t, assertThat

        type(Result_t) :: result_

        type(ErrorList_t) :: errors
        type(Angle_t) :: length

        length = angleFromString("bad rad", errors)
        result_ = assertThat(errors.hasType.PARSE_ERROR, errors%toString())
    end function checkBadNumber
end module angle_type_test
