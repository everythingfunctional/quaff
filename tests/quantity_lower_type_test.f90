module quantity_lower_type_test
    use Quantity_module_m, only: QuantityCamelUnit_t
    use Vegetables_m, only: Example_t, Input_t

    implicit none
    private

    type, public, extends(Input_t) :: UnitsInput_t
        class(QuantityCamelUnit_t), allocatable :: unit
    end type UnitsInput_t

    type, public, extends(Input_t) :: UnitsPairInput_t
        class(QuantityCamelUnit_t), allocatable :: first
        class(QuantityCamelUnit_t), allocatable :: second
    end type UnitsPairInput_t

    type, public :: UnitsExamples_t
        type(Example_t), allocatable :: units(:)
        type(Example_t), allocatable :: pairs(:)
    end type UnitsExamples_t

    public :: test_quantity_lower
contains
    function test_quantity_lower() result(tests)
        use Quantity_module_m, only: PROVIDED_UNITS
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

    function checkConversionFactorsInverse(pair) result(result_)
        use Vegetables_m, only: Input_t, Result_t, fail

        class(Input_t), intent(in) :: pair
        type(Result_t) :: result_

        select type (pair)
        type is (UnitsPairInput_t)
            result_ = checkConversionFactorsAreInverse(pair%first, pair%second)
        class default
            result_ = fail("Expected to get a UnitsPairInput_t")
        end select
    end function checkConversionFactorsInverse

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
        use erloff, only: ErrorList_t
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
        use erloff, only: ErrorList_t
        use Quantity_module_m, only: &
                QuantityCamel_t, quantitySnakeFromString, UNITS_CAPITAL
        use Miscellaneous_m, only: UNKNOWN_UNIT
        use Vegetables_m, only: Result_t, assertThat

        type(Result_t) :: result_

        type(ErrorList_t) :: errors
        type(QuantityCamel_t) :: length

        length = quantitySnakeFromString("1.0 bad", [UNITS_CAPITAL], errors)
        result_ = assertThat(errors.hasType.UNKNOWN_UNIT, errors%toString())
    end function checkBadUnit

    function checkBadNumber() result(result_)
        use erloff, only: ErrorList_t
        use Quantity_module_m, only: QuantityCamel_t, quantitySnakeFromString
        use Miscellaneous_m, only: PARSE_ERROR
        use Vegetables_m, only: Result_t, assertThat

        type(Result_t) :: result_

        type(ErrorList_t) :: errors
        type(QuantityCamel_t) :: length

        length = quantitySnakeFromString("bad unit_sym", errors)
        result_ = assertThat(errors.hasType.PARSE_ERROR, errors%toString())
    end function checkBadNumber

    function makeUnitsExamples(units) result(examples)
        use Quantity_module_m, only: QuantityCamelUnit_t
        use Vegetables_m, only: Example

        type(QuantityCamelUnit_t), intent(in) :: units(:)
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
        use Quantity_module_m, only: QuantityCamelUnit_t
        use Vegetables_m, only: &
                Result_t, TestItem_t, TestResultItem_t, assertThat, It

        type(QuantityCamelUnit_t), intent(in) :: units
        type(Result_t) :: result_

        type(TestItem_t) :: the_test
        type(TestResultItem_t) :: the_result

        the_test = It(units%toString(), DOUBLE_PRECISION_GENERATOR, checkRoundTrip_)
        the_result = the_test%run()
        result_ = assertThat(the_result%passed(), the_result%verboseDescription(.false.))
    contains
        function checkRoundTrip_(input) result(result__)
            use Quantity_module_m, only: QuantityCamel_t, operator(.unit.)
            use Vegetables_m, only: &
                    DoublePrecisionInput_t, &
                    Input_t, &
                    Result_t, &
                    assertEqualsWithinRelative, &
                    fail

            class(Input_t), intent(in) :: input
            type(Result_t) :: result__

            type(QuantityCamel_t) :: intermediate

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

    function checkConversionFactorsAreInverse( &
            from, to) result(result_)
        use Quantity_module_m, only: &
                QuantityCamel_t, QuantityCamelUnit_t, operator(.unit.)
        use iso_varying_string, only: operator(//)
        use Vegetables_m, only: Result_t, assertEqualsWithinRelative

        type(QuantityCamelUnit_t), intent(in) :: to
        type(QuantityCamelUnit_t), intent(in) :: from
        type(Result_t) :: result_

        double precision :: factor1
        double precision :: factor2

        factor1 = (1.0d0.unit.from).in.to
        factor2 = (1.0d0.unit.to).in.from
        result_ = assertEqualsWithinRelative( &
                factor1, &
                1.0d0 / factor2, &
                1.0d-12, &
                from%toString() // " to " // to%toString())
    end function checkConversionFactorsAreInverse

    function checkStringTrip(units) result(result_)
        use DoublePrecisionGenerator_m, only: DOUBLE_PRECISION_GENERATOR
        use iso_varying_string, only: char
        use Quantity_module_m, only: QuantityCamelUnit_t
        use Vegetables_m, only: &
                Result_t, TestItem_t, TestResultItem_t, assertThat, It

        type(QuantityCamelUnit_t), intent(in) :: units
        type(Result_t) :: result_

        type(TestItem_t) :: the_test
        type(TestResultItem_t) :: the_result

        the_test = It(units%toString(), DOUBLE_PRECISION_GENERATOR, doCheck)
        the_result = the_test%run()
        result_ = assertThat(the_result%passed(), the_result%verboseDescription(.false.))
    contains
        function doCheck(input) result(result__)
            use erloff, only: ErrorList_t, size
            use Quantity_module_m, only: &
                    QuantityCamel_t, &
                    operator(.unit.), &
                    quantitySnakeFromString
            use Quantity_module_asserts_m, only: assertEquals
            use Vegetables_m, only: &
                    DoublePrecisionInput_t, Input_t, Result_t, assertNot, fail

            class(Input_t), intent(in) :: input
            type(Result_t) :: result__

            type(ErrorList_t) :: errors
            type(QuantityCamel_t) :: original_quantity_lower
            type(QuantityCamel_t) :: new_quantity_lower

            select type (input)
            type is (DoublePrecisionInput_t)
                original_quantity_lower = input%value_.unit.units
                new_quantity_lower = quantitySnakeFromString( &
                        original_quantity_lower%toStringIn(units), errors)
                result__ = &
                        assertEquals( &
                                original_quantity_lower, &
                                new_quantity_lower) &
                        .and.assertNot(errors%hasAny(), errors%toString())
            class default
                result__ = fail("Expected to get a DoublePrecisionInput_t")
            end select
        end function doCheck
    end function checkStringTrip
end module quantity_lower_type_test
