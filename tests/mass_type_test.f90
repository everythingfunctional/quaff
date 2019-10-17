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
        type(TestItem_t) :: individual_tests(2)

        examples = makeUnitsExamples(PROVIDED_UNITS)
        individual_tests(1) = it( &
                "gets the same value given the same units", &
                examples%units, &
                checkRoundTrip)
        individual_tests(2) = it( &
                "the conversion factors between 2 units are inverses", &
                examples%pairs, &
                checkConversionFactorsInverse)
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
end module mass_type_test
