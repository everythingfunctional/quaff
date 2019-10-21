module temperature_type_test
    implicit none
    private

    public :: test_temperature
contains
    function test_temperature() result(tests)
        use Temperature_m, only: PROVIDED_UNITS
        use Units_examples_m, only: UnitsExamples_t, makeUnitsExamples
        use Vegetables_m, only: TestItem_t, describe, it

        type(TestItem_t) :: tests

        type(UnitsExamples_t) :: examples
        type(TestItem_t) :: individual_tests(5)

        examples = makeUnitsExamples(PROVIDED_UNITS)
        individual_tests(1) = it( &
                "gets the same value given the same units", &
                examples%units, &
                checkRoundTrip)
        individual_tests(2) = it( &
                "can be converted to and from a string", &
                examples%units, &
                checkToAndFromString)
        individual_tests(3) = it( &
                "Trying to parse a bad string is an error", &
                checkBadString)
        individual_tests(4) = it( &
                "Trying to parse with an unknown unit is an error", &
                checkBadUnit)
        individual_tests(5) = it( &
                "Trying to parse a bad number is an error", &
                checkBadNumber)
        tests = describe("Temperature_t", individual_tests)
    end function test_temperature

    function checkRoundTrip(units) result(result_)
        use Check_round_trip_in_m, only: checkRoundTripIn
        use Temperature_m, only: TemperatureUnit_t
        use Units_examples_m, only: UnitsInput_t
        use Vegetables_m, only: Input_t, Result_t, fail

        class(Input_t), intent(in) :: units
        type(Result_t) :: result_

        select type (units)
        type is (UnitsInput_t)
            select type (the_unit => units%unit)
            type is (TemperatureUnit_t)
                result_ = checkRoundTripIn(the_unit)
            class default
                result_ = fail("Expected to get an TemperatureUnit_t")
            end select
        class default
            result_ = fail("Expected to get an UnitsInput_t")
        end select
    end function checkRoundTrip

    function checkToAndFromString(units) result(result_)
        use Error_list_m, only: ErrorList_t, size
        use Temperature_m, only: &
                Temperature_t, &
                TemperatureUnit_t, &
                operator(.unit.), &
                temperatureFromString
        use Temperature_asserts_m, only: assertEquals
        use Units_examples_m, only: UnitsInput_t
        use Vegetables_m, only: Input_t, Result_t, assertEquals, fail

        class(Input_t), intent(in) :: units
        type(Result_t) :: result_

        type(ErrorList_t) :: errors
        type(Temperature_t) :: original_temperature
        type(Temperature_t) :: new_temperature

        select type (units)
        type is (UnitsInput_t)
            select type (the_unit => units%unit)
            type is (TemperatureUnit_t)
                original_temperature = 3.0d0.unit.the_unit
                new_temperature = temperatureFromString( &
                        original_temperature%toStringIn(the_unit), errors)
                result_ = &
                        assertEquals( &
                                original_temperature, &
                                new_temperature, &
                                the_unit%toString()) &
                        .and.assertEquals(0, size(errors))
            class default
                result_ = fail("Expected to get an TemperatureUnit_t")
            end select
        class default
            result_ = fail("Expected to get an UnitsInput_t")
        end select
    end function checkToAndFromString

    function checkBadString() result(result_)
        use Error_list_m, only: ErrorList_t
        use Temperature_m, only: Temperature_t, temperatureFromString
        use Miscellaneous_m, only: PARSE_ERROR
        use Vegetables_m, only: Result_t, assertThat

        type(Result_t) :: result_

        type(ErrorList_t) :: errors
        type(Temperature_t) :: length

        length = temperatureFromString("bad", errors)
        result_ = assertThat(errors.hasType.PARSE_ERROR, errors%toString())
    end function checkBadString

    function checkBadUnit() result(result_)
        use Error_list_m, only: ErrorList_t
        use Temperature_m, only: &
                Temperature_t, temperatureFromString, KELVIN
        use Miscellaneous_m, only: UNKNOWN_UNIT
        use Vegetables_m, only: Result_t, assertThat

        type(Result_t) :: result_

        type(ErrorList_t) :: errors
        type(Temperature_t) :: length

        length = temperatureFromString("1.0 bad", [KELVIN], errors)
        result_ = assertThat(errors.hasType.UNKNOWN_UNIT, errors%toString())
    end function checkBadUnit

    function checkBadNumber() result(result_)
        use Error_list_m, only: ErrorList_t
        use Temperature_m, only: Temperature_t, temperatureFromString
        use Miscellaneous_m, only: PARSE_ERROR
        use Vegetables_m, only: Result_t, assertThat

        type(Result_t) :: result_

        type(ErrorList_t) :: errors
        type(Temperature_t) :: length

        length = temperatureFromString("bad K", errors)
        result_ = assertThat(errors.hasType.PARSE_ERROR, errors%toString())
    end function checkBadNumber
end module temperature_type_test
