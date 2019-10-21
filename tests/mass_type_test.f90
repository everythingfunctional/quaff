module mass_type_test
    implicit none
    private

    public :: test_mass
contains
    function test_mass() result(tests)
        use Mass_m, only: PROVIDED_UNITS
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
        tests = describe("Mass_t", individual_tests)
    end function test_mass

    function checkRoundTrip(units) result(result_)
        use Check_round_trip_in_m, only: checkRoundTripIn
        use Mass_m, only: MassUnit_t
        use Units_examples_m, only: UnitsInput_t
        use Vegetables_m, only: Input_t, Result_t, fail

        class(Input_t), intent(in) :: units
        type(Result_t) :: result_

        select type (units)
        type is (UnitsInput_t)
            select type (the_unit => units%unit)
            type is (MassUnit_t)
                result_ = checkRoundTripIn(the_unit)
            class default
                result_ = fail("Expected to get an MassUnit_t")
            end select
        class default
            result_ = fail("Expected to get an UnitsInput_t")
        end select
    end function checkRoundTrip

    function checkConversionFactorsInverse(pair) result(result_)
        use Check_conversion_factor_m, only: checkConversionFactorsAreInverse
        use Mass_m, only: MassUnit_t
        use Units_examples_m, only: UnitsPairInput_t
        use Vegetables_m, only: Input_t, Result_t, fail

        class(Input_t), intent(in) :: pair
        type(Result_t) :: result_

        select type (pair)
        type is (UnitsPairInput_t)
            select type (first => pair%first)
            type is (MassUnit_t)
                select type (second => pair%second)
                type is (MassUnit_t)
                    result_ = checkConversionFactorsAreInverse(first, second)
                class default
                    result_ = fail("Expected second in pair to be MassUnit_t")
                end select
            class default
                result_ = fail("Expected first in pair to be MassUnit_t")
            end select
        class default
            result_ = fail("Expected to get a UnitsPairInput_t")
        end select
    end function checkConversionFactorsInverse

    function checkToAndFromString(units) result(result_)
        use Error_list_m, only: ErrorList_t, size
        use Mass_m, only: &
                Mass_t, &
                MassUnit_t, &
                operator(.unit.), &
                massFromString
        use Mass_asserts_m, only: assertEquals
        use Units_examples_m, only: UnitsInput_t
        use Vegetables_m, only: Input_t, Result_t, assertEquals, fail

        class(Input_t), intent(in) :: units
        type(Result_t) :: result_

        type(ErrorList_t) :: errors
        type(Mass_t) :: original_mass
        type(Mass_t) :: new_mass

        select type (units)
        type is (UnitsInput_t)
            select type (the_unit => units%unit)
            type is (MassUnit_t)
                original_mass = 3.0d0.unit.the_unit
                new_mass = massFromString( &
                        original_mass%toStringIn(the_unit), errors)
                result_ = &
                        assertEquals( &
                                original_mass, &
                                new_mass, &
                                the_unit%toString()) &
                        .and.assertEquals(0, size(errors))
            class default
                result_ = fail("Expected to get an MassUnit_t")
            end select
        class default
            result_ = fail("Expected to get an UnitsInput_t")
        end select
    end function checkToAndFromString

    function checkBadString() result(result_)
        use Error_list_m, only: ErrorList_t
        use Mass_m, only: Mass_t, massFromString
        use Miscellaneous_m, only: PARSE_ERROR
        use Vegetables_m, only: Result_t, assertThat

        type(Result_t) :: result_

        type(ErrorList_t) :: errors
        type(Mass_t) :: length

        length = massFromString("bad", errors)
        result_ = assertThat(errors.hasType.PARSE_ERROR, errors%toString())
    end function checkBadString

    function checkBadUnit() result(result_)
        use Error_list_m, only: ErrorList_t
        use Mass_m, only: &
                Mass_t, massFromString, KILOGRAMS
        use Miscellaneous_m, only: UNKNOWN_UNIT
        use Vegetables_m, only: Result_t, assertThat

        type(Result_t) :: result_

        type(ErrorList_t) :: errors
        type(Mass_t) :: length

        length = massFromString("1.0 bad", [KILOGRAMS], errors)
        result_ = assertThat(errors.hasType.UNKNOWN_UNIT, errors%toString())
    end function checkBadUnit

    function checkBadNumber() result(result_)
        use Error_list_m, only: ErrorList_t
        use Mass_m, only: Mass_t, massFromString
        use Miscellaneous_m, only: PARSE_ERROR
        use Vegetables_m, only: Result_t, assertThat

        type(Result_t) :: result_

        type(ErrorList_t) :: errors
        type(Mass_t) :: length

        length = massFromString("bad kg", errors)
        result_ = assertThat(errors.hasType.PARSE_ERROR, errors%toString())
    end function checkBadNumber
end module mass_type_test
