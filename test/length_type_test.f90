module length_type_test
    use DoublePrecisionGenerator_m, only: DOUBLE_PRECISION_GENERATOR
    use erloff, only: error_list_t
    use iso_varying_string, only: operator(//)
    use quaff, only: &
            Length_t, &
            LengthUnit_t, &
            operator(.unit.), &
            fromString, &
            sum, &
            PROVIDED_LENGTH_UNITS, &
            METERS
    use quaff_asserts_m, only: assert_equals
    use quaff_Utilities_m, only: PARSE_ERROR
    use vegetables, only: &
            double_precision_input_t, &
            example_t, &
            input_t, &
            result_t, &
            test_item_t, &
            test_result_item_t, &
            assert_equals_within_relative, &
            assert_not, &
            assert_that, &
            Describe, &
            fail, &
            It

    implicit none
    private

    type, public, extends(input_t) :: UnitsInput_t
        class(LengthUnit_t), allocatable :: unit
    end type UnitsInput_t

    type, public, extends(input_t) :: UnitsPairInput_t
        class(LengthUnit_t), allocatable :: first
        class(LengthUnit_t), allocatable :: second
    end type UnitsPairInput_t

    type, public :: UnitsExamples_t
        type(example_t), allocatable :: units(:)
        type(example_t), allocatable :: pairs(:)
    end type UnitsExamples_t

    public :: test_length
contains
    function test_length() result(tests)
        type(test_item_t) :: tests

        type(UnitsExamples_t) :: examples
        type(test_item_t) :: individual_tests(7)

        examples = makeUnitsExamples(PROVIDED_LENGTH_UNITS)
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
        individual_tests(7) = it("Can be summed", checkSum)
        tests = describe("Length_t", individual_tests)
    end function test_length

    function checkRoundTrip(units) result(result_)
        class(input_t), intent(in) :: units
        type(result_t) :: result_

        select type (units)
        type is (UnitsInput_t)
            result_ = checkRoundTripIn(units%unit)
        class default
            result_ = fail("Expected to get an UnitsInput_t")
        end select
    end function checkRoundTrip

    pure function checkConversionFactorsInverse(pair) result(result_)
        class(input_t), intent(in) :: pair
        type(result_t) :: result_

        select type (pair)
        type is (UnitsPairInput_t)
            result_ = checkConversionFactorsAreInverse(pair%first, pair%second)
        class default
            result_ = fail("Expected to get a UnitsPairInput_t")
        end select
    end function checkConversionFactorsInverse

    function checkToAndFromString(units) result(result_)
        class(input_t), intent(in) :: units
        type(result_t) :: result_

        select type (units)
        type is (UnitsInput_t)
            result_ = checkStringTrip(units%unit)
        class default
            result_ = fail("Expected to get an UnitsInput_t")
        end select
    end function checkToAndFromString

    function checkBadString() result(result_)
        type(result_t) :: result_

        type(error_list_t) :: errors
        type(Length_t) :: length

        call fromString("bad", errors, length)
        result_ = assert_that(errors.hasType.PARSE_ERROR, errors%to_string())
    end function checkBadString

    function checkBadUnit() result(result_)
        type(result_t) :: result_

        type(error_list_t) :: errors
        type(Length_t) :: length

        call fromString( &
                "1.0 bad", [METERS], errors, length)
        result_ = assert_that(errors.hasType.PARSE_ERROR, errors%to_string())
    end function checkBadUnit

    function checkBadNumber() result(result_)
        type(result_t) :: result_

        type(error_list_t) :: errors
        type(Length_t) :: length

        call fromString("bad m", errors, length)
        result_ = assert_that(errors.hasType.PARSE_ERROR, errors%to_string())
    end function checkBadNumber

    pure function checkSum() result(result_)
        type(result_t) :: result_

        double precision, parameter :: numbers(*) = [1.0d0, 2.0d0, 3.0d0]

        result_ = assert_equals( &
                sum(numbers).unit.METERS, &
                sum(numbers.unit.METERS))
    end function checkSum

    function makeUnitsExamples(units) result(examples)
        class(LengthUnit_t), intent(in) :: units(:)
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
            examples%units(i) = example_t(input)
            deallocate(input%unit)
        end do

        num_pairs = combinations(num_units)
        allocate(examples%pairs(num_pairs))
        pair_index = 1
        do i = 1, num_units - 1
            allocate(pair%first, source = units(i))
            do j = i + 1, num_units
                allocate(pair%second, source = units(j))
                examples%pairs(pair_index) = example_t(pair)
                pair_index = pair_index + 1
                deallocate(pair%second)
            end do
            deallocate(pair%first)
        end do
    contains
        pure recursive function combinations(num_items) result(num_combinations)
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
        class(LengthUnit_t), intent(in) :: units
        type(result_t) :: result_

        type(test_item_t) :: the_test
        type(test_result_item_t) :: the_result

        the_test = It(units%toString(), DOUBLE_PRECISION_GENERATOR, checkRoundTrip_)
        the_result = the_test%run()
        result_ = assert_that(the_result%passed(), the_result%verbose_description(.false.))
    contains
        pure function checkRoundTrip_(input) result(result__)
            class(input_t), intent(in) :: input
            type(result_t) :: result__

            type(Length_t) :: intermediate

            select type (input)
            type is (double_precision_input_t)
                intermediate = input%input().unit.units
                result__ = assert_equals_within_relative( &
                        input%input(), &
                        intermediate.in.units, &
                        1.0d-12)
            class default
                result__ = fail("Expected to get a double_precision_input_t")
            end select
        end function checkRoundTrip_
    end function checkRoundTripIn

    pure function checkConversionFactorsAreInverse( &
            from, to) result(result_)
        class(LengthUnit_t), intent(in) :: to
        class(LengthUnit_t), intent(in) :: from
        type(result_t) :: result_

        double precision :: factor1
        double precision :: factor2

        factor1 = (1.0d0.unit.from).in.to
        factor2 = (1.0d0.unit.to).in.from
        result_ = assert_equals_within_relative( &
                factor1, &
                1.0d0 / factor2, &
                1.0d-12, &
                from%toString() // " to " // to%toString())
    end function checkConversionFactorsAreInverse

    function checkStringTrip(units) result(result_)
        class(LengthUnit_t), intent(in) :: units
        type(result_t) :: result_

        type(test_item_t) :: the_test
        type(test_result_item_t) :: the_result

        the_test = It(units%toString(), DOUBLE_PRECISION_GENERATOR, doCheck)
        the_result = the_test%run()
        result_ = assert_that(the_result%passed(), the_result%verbose_description(.false.))
    contains
        function doCheck(input) result(result__)
            class(input_t), intent(in) :: input
            type(result_t) :: result__

            type(error_list_t) :: errors
            type(Length_t) :: original_length
            type(Length_t) :: new_length

            select type (input)
            type is (double_precision_input_t)
                original_length = input%input().unit.units
                call fromString( &
                        original_length%toStringIn(units), &
                        errors, &
                        new_length)
                result__ = &
                        assert_equals( &
                                original_length, &
                                new_length) &
                        .and.assert_not(errors%has_any(), errors%to_string())
            class default
                result__ = fail("Expected to get a double_precision_input_t")
            end select
        end function doCheck
    end function checkStringTrip
end module length_type_test
