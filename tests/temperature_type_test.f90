module temperature_type_test
    use Temperature_m, only: TemperatureUnit_t
    use Vegetables_m, only: Example_t, Input_t

    implicit none
    private

    type, public, extends(Input_t) :: UnitsInput_t
        class(TemperatureUnit_t), allocatable :: unit
    end type UnitsInput_t

    type, public, extends(Input_t) :: UnitsPairInput_t
        class(TemperatureUnit_t), allocatable :: first
        class(TemperatureUnit_t), allocatable :: second
    end type UnitsPairInput_t

    type, public :: UnitsExamples_t
        type(Example_t), allocatable :: units(:)
        type(Example_t), allocatable :: pairs(:)
    end type UnitsExamples_t

    public :: test_temperature
contains
    function test_temperature() result(tests)
        use Temperature_m, only: PROVIDED_UNITS
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
        use Vegetables_m, only: Input_t, Result_t, fail

        class(Input_t), intent(in) :: units
        type(Result_t) :: result_

        select type (units)
        type is (UnitsInput_t)
            result_ = checkRoundTripIn(units%unit)
        class default
            result_ = fail("Expected to get an UnitsInput_t")
        end select
    end function checkRoundTrip

    function checkToAndFromString(units) result(result_)
        use Vegetables_m, only: Input_t, Result_t, fail

        class(Input_t), intent(in) :: units
        type(Result_t) :: result_

        select type (units)
        type is (UnitsInput_t)
            result_ = checkStringTrip(units%unit)
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

    function makeUnitsExamples(units) result(examples)
        use Temperature_m, only: TemperatureUnit_t
        use Vegetables_m, only: Example

        type(TemperatureUnit_t), intent(in) :: units(:)
        type(UnitsExamples_t) :: examples

        integer :: i
        integer :: j
        integer :: num_pairs
        integer :: num_units
        type(UnitsPairInput_t) :: pair
        integer :: pair_index
        type(UnitsInput_t) :: input

        num_units = size(units)
        allocate(examples%units(num_units))
        do i = 1, num_units
            allocate(input%unit, source = units(i))
            examples%units(i) = Example(input)
            deallocate(input%unit)
        end do

        num_pairs = combinations(num_units)
        allocate(examples%pairs(num_pairs))
        pair_index = 1
        do i = 1, num_units - 1
            allocate(pair%first, source = units(i))
            do j = i + 1, num_units
                allocate(pair%second, source = units(j))
                examples%pairs(pair_index) = Example(pair)
                pair_index = pair_index + 1
                deallocate(pair%second)
            end do
            deallocate(pair%first)
        end do
    contains
        recursive function combinations(num_items) result(num_combinations)
            integer, intent(in) :: num_items
            integer :: num_combinations

            if (num_items <= 1) then
                num_combinations = 0
            else
                num_combinations = num_items - 1 + combinations(num_items - 1)
            end if
        end function combinations
    end function makeUnitsExamples

    function checkRoundTripIn(units) result(result_)
        use DoublePrecisionGenerator_m, only: DOUBLE_PRECISION_GENERATOR
        use iso_varying_string, only: char
        use Temperature_m, only: TemperatureUnit_t
        use Vegetables_m, only: &
                Result_t, TestItem_t, TestResultItem_t, assertThat, It

        type(TemperatureUnit_t), intent(in) :: units
        type(Result_t) :: result_

        type(TestItem_t) :: the_test
        type(TestResultItem_t) :: the_result

        the_test = It(units%toString(), DOUBLE_PRECISION_GENERATOR, checkRoundTrip_)
        the_result = the_test%run()
        result_ = assertThat(the_result%passed(), the_result%verboseDescription(.false.))
    contains
        function checkRoundTrip_(input) result(result__)
            use Temperature_m, only: Temperature_t, operator(.unit.)
            use Vegetables_m, only: &
                    DoublePrecisionInput_t, &
                    Input_t, &
                    Result_t, &
                    assertEqualsWithinRelative, &
                    fail

            class(Input_t), intent(in) :: input
            type(Result_t) :: result__

            type(Temperature_t) :: intermediate

            select type (input)
            type is (DoublePrecisionInput_t)
                intermediate = input%value_.unit.units
                result__ = assertEqualsWithinRelative( &
                        input%value_, &
                        intermediate.in.units, &
                        1.0d-12)
            class default
                result__ = fail("Expected to get a DoublePrecisionInput_t")
            end select
        end function checkRoundTrip_
    end function checkRoundTripIn

    function checkStringTrip(units) result(result_)
        use DoublePrecisionGenerator_m, only: DOUBLE_PRECISION_GENERATOR
        use iso_varying_string, only: char
        use Temperature_m, only: TemperatureUnit_t
        use Vegetables_m, only: &
                Result_t, TestItem_t, TestResultItem_t, assertThat, It

        type(TemperatureUnit_t), intent(in) :: units
        type(Result_t) :: result_

        type(TestItem_t) :: the_test
        type(TestResultItem_t) :: the_result

        the_test = It(units%toString(), DOUBLE_PRECISION_GENERATOR, doCheck)
        the_result = the_test%run()
        result_ = assertThat(the_result%passed(), the_result%verboseDescription(.false.))
    contains
        function doCheck(input) result(result__)
            use Error_list_m, only: ErrorList_t, size
            use Temperature_m, only: &
                    Temperature_t, &
                    operator(.unit.), &
                    temperatureFromString
            use Temperature_asserts_m, only: assertEquals
            use Vegetables_m, only: &
                    DoublePrecisionInput_t, Input_t, Result_t, assertNot, fail

            class(Input_t), intent(in) :: input
            type(Result_t) :: result__

            type(ErrorList_t) :: errors
            type(Temperature_t) :: original_temperature
            type(Temperature_t) :: new_temperature

            select type (input)
            type is (DoublePrecisionInput_t)
                original_temperature = input%value_.unit.units
                new_temperature = temperatureFromString( &
                        original_temperature%toStringIn(units), errors)
                result__ = &
                        assertEquals( &
                                original_temperature, &
                                new_temperature) &
                        .and.assertNot(errors%hasAny(), errors%toString())
            class default
                result__ = fail("Expected to get a DoublePrecisionInput_t")
            end select
        end function doCheck
    end function checkStringTrip
end module temperature_type_test
