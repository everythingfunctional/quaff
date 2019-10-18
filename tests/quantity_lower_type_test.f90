module quantity_lower_type_test
    implicit none
    private

    public :: test_quantity_lower
contains
    function test_quantity_lower() result(tests)
        use Quantity_module_m, only: PROVIDED_UNITS
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
        tests = describe("QuantityCamel_t", individual_tests)
    end function test_quantity_lower

    function checkRoundTrip(units) result(result_)
        use Check_round_trip_in_m, only: checkRoundTripIn
        use Quantity_module_m, only: QuantityCamelUnit_t
        use Units_examples_m, only: UnitsInput_t
        use Vegetables_m, only: Input_t, Result_t, fail

        class(Input_t), intent(in) :: units
        type(Result_t) :: result_

        select type (units)
        type is (UnitsInput_t)
            select type (the_unit => units%unit)
            type is (QuantityCamelUnit_t)
                result_ = checkRoundTripIn(the_unit)
            class default
                result_ = fail("Expected to get an QuantityCamelUnit_t")
            end select
        class default
            result_ = fail("Expected to get an UnitsInput_t")
        end select
    end function checkRoundTrip

    function checkConversionFactorsInverse(pair) result(result_)
        use Check_conversion_factor_m, only: checkConversionFactorsAreInverse
        use Quantity_module_m, only: QuantityCamelUnit_t
        use Units_examples_m, only: UnitsPairInput_t
        use Vegetables_m, only: Input_t, Result_t, fail

        class(Input_t), intent(in) :: pair
        type(Result_t) :: result_

        select type (pair)
        type is (UnitsPairInput_t)
            select type (first => pair%first)
            type is (QuantityCamelUnit_t)
                select type (second => pair%second)
                type is (QuantityCamelUnit_t)
                    result_ = checkConversionFactorsAreInverse(first, second)
                class default
                    result_ = fail("Expected second in pair to be QuantityCamelUnit_t")
                end select
            class default
                result_ = fail("Expected first in pair to be QuantityCamelUnit_t")
            end select
        class default
            result_ = fail("Expected to get a UnitsPairInput_t")
        end select
    end function checkConversionFactorsInverse

    function checkToAndFromString(units) result(result_)
        use Error_list_m, only: ErrorList_t, size
        use Quantity_module_m, only: &
                QuantityCamel_t, &
                QuantityCamelUnit_t, &
                operator(.unit.), &
                quantitySnakeFromString
        use Quantity_module_asserts_m, only: assertEquals
        use Units_examples_m, only: UnitsInput_t
        use Vegetables_m, only: Input_t, Result_t, assertEquals, fail

        class(Input_t), intent(in) :: units
        type(Result_t) :: result_

        type(ErrorList_t) :: errors
        type(QuantityCamel_t) :: original_quantity_lower
        type(QuantityCamel_t) :: new_quantity_lower

        select type (units)
        type is (UnitsInput_t)
            select type (the_unit => units%unit)
            type is (QuantityCamelUnit_t)
                original_quantity_lower = 3.0d0.unit.the_unit
                new_quantity_lower = quantitySnakeFromString( &
                        original_quantity_lower%toStringIn(the_unit), errors)
                result_ = &
                        assertEquals( &
                                original_quantity_lower, &
                                new_quantity_lower, &
                                the_unit%toString()) &
                        .and.assertEquals(0, size(errors))
            class default
                result_ = fail("Expected to get an QuantityCamelUnit_t")
            end select
        class default
            result_ = fail("Expected to get an UnitsInput_t")
        end select
    end function checkToAndFromString

    function checkBadString() result(result_)
        use Error_list_m, only: ErrorList_t
        use Quantity_module_m, only: QuantityCamel_t, quantitySnakeFromString
        use Miscellaneous_m, only: PARSE_ERROR
        use Vegetables_m, only: Result_t, assertThat

        type(Result_t) :: result_

        type(ErrorList_t) :: errors
        type(QuantityCamel_t) :: length

        length = quantitySnakeFromString("bad", errors)
        result_ = assertThat(errors.hasType.PARSE_ERROR, errors%toString())
    end function checkBadString

    function checkBadUnit() result(result_)
        use Error_list_m, only: ErrorList_t
        use Quantity_module_m, only: &
                QuantityCamel_t, quantitySnakeFromString, UNITS1_CAPITAL
        use Miscellaneous_m, only: UNKNOWN_UNIT
        use Vegetables_m, only: Result_t, assertThat

        type(Result_t) :: result_

        type(ErrorList_t) :: errors
        type(QuantityCamel_t) :: length

        length = quantitySnakeFromString("1.0 bad", [UNITS1_CAPITAL], errors)
        result_ = assertThat(errors.hasType.UNKNOWN_UNIT, errors%toString())
    end function checkBadUnit

    function checkBadNumber() result(result_)
        use Error_list_m, only: ErrorList_t
        use Quantity_module_m, only: QuantityCamel_t, quantitySnakeFromString
        use Miscellaneous_m, only: PARSE_ERROR
        use Vegetables_m, only: Result_t, assertThat

        type(Result_t) :: result_

        type(ErrorList_t) :: errors
        type(QuantityCamel_t) :: length

        length = quantitySnakeFromString("bad m", errors)
        result_ = assertThat(errors.hasType.PARSE_ERROR, errors%toString())
    end function checkBadNumber
end module quantity_lower_type_test
