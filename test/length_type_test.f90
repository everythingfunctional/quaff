module length_type_test
    use double_precision_generator_m, only: DOUBLE_PRECISION_GENERATOR
    use erloff, only: error_list_t
    use iso_varying_string, only: operator(//)
    use quaff, only: &
            length_t, &
            fallible_length_t, &
            length_unit_t, &
            operator(.unit.), &
            parse_length, &
            sum, &
            PROVIDED_LENGTH_UNITS, &
            METERS
    use quaff_asserts_m, only: assert_equals
    use quaff_utilities_m, only: PARSE_ERROR
    use length_utilities_m, only: &
            units_input_t, units_pair_input_t, make_units_examples
    use units_examples_m, only: units_examples_t
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
    public :: test_length
contains
    function test_length() result(tests)
        type(test_item_t) :: tests

        type(units_examples_t) :: examples
        type(test_item_t) :: individual_tests(7)

        examples = make_units_examples(PROVIDED_LENGTH_UNITS)
        individual_tests(1) = it( &
                "gets the same value given the same units", &
                examples%units(), &
                checkRoundTrip)
        individual_tests(2) = it( &
                "the conversion factors between 2 units are inverses", &
                examples%pairs(), &
                checkConversionFactorsInverse)
        individual_tests(3) = it( &
                "can be converted to and from a string", &
                examples%units(), &
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
        tests = describe("length_t", individual_tests)
    end function test_length

    function checkRoundTrip(units) result(result_)
        class(input_t), intent(in) :: units
        type(result_t) :: result_

        select type (units)
        type is (units_input_t)
            result_ = checkRoundTripIn(units%unit())
        class default
            result_ = fail("Expected to get an units_input_t")
        end select
    end function checkRoundTrip

    function checkConversionFactorsInverse(pair) result(result_)
        class(input_t), intent(in) :: pair
        type(result_t) :: result_

        select type (pair)
        type is (units_pair_input_t)
            result_ = checkConversionFactorsAreInverse(pair%first(), pair%second_())
        class default
            result_ = fail("Expected to get a units_pair_input_t")
        end select
    end function checkConversionFactorsInverse

    function checkToAndFromString(units) result(result_)
        class(input_t), intent(in) :: units
        type(result_t) :: result_

        select type (units)
        type is (units_input_t)
            result_ = checkStringTrip(units%unit())
        class default
            result_ = fail("Expected to get an units_input_t")
        end select
    end function checkToAndFromString

    function checkBadString() result(result_)
        type(result_t) :: result_

        type(error_list_t) :: errors
        type(fallible_length_t) :: maybe_length

        maybe_length = parse_length("bad")
        errors = maybe_length%errors()
        result_ = assert_that(errors.hasType.PARSE_ERROR, errors%to_string())
    end function checkBadString

    function checkBadUnit() result(result_)
        type(result_t) :: result_

        type(error_list_t) :: errors
        type(fallible_length_t) :: maybe_length

        maybe_length = parse_length("1.0 bad", [METERS])
        errors = maybe_length%errors()
        result_ = assert_that(errors.hasType.PARSE_ERROR, errors%to_string())
    end function checkBadUnit

    function checkBadNumber() result(result_)
        type(result_t) :: result_

        type(error_list_t) :: errors
        type(fallible_length_t) :: maybe_length

        maybe_length = parse_length("bad m")
        errors = maybe_length%errors()
        result_ = assert_that(errors.hasType.PARSE_ERROR, errors%to_string())
    end function checkBadNumber

    pure function checkSum() result(result_)
        type(result_t) :: result_

        double precision, parameter :: numbers(*) = [1.0d0, 2.0d0, 3.0d0]

        result_ = assert_equals( &
                sum(numbers).unit.METERS, &
                sum(numbers.unit.METERS))
    end function checkSum

    function checkRoundTripIn(units) result(result_)
        class(length_unit_t), intent(in) :: units
        type(result_t) :: result_

        type(test_item_t) :: the_test
        type(test_result_item_t) :: the_result

        the_test = It(units%to_string(), DOUBLE_PRECISION_GENERATOR, checkRoundTrip_)
        the_result = the_test%run()
        result_ = assert_that(the_result%passed(), the_result%verbose_description(.false.))
    contains
        pure function checkRoundTrip_(input) result(result__)
            class(input_t), intent(in) :: input
            type(result_t) :: result__

            type(length_t) :: intermediate

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
        class(length_unit_t), intent(in) :: to
        class(length_unit_t), intent(in) :: from
        type(result_t) :: result_

        double precision :: factor1
        double precision :: factor2

        factor1 = (1.0d0.unit.from).in.to
        factor2 = (1.0d0.unit.to).in.from
        result_ = assert_equals_within_relative( &
                factor1, &
                1.0d0 / factor2, &
                1.0d-12, &
                from%to_string() // " to " // to%to_string())
    end function checkConversionFactorsAreInverse

    function checkStringTrip(units) result(result_)
        class(length_unit_t), intent(in) :: units
        type(result_t) :: result_

        type(test_item_t) :: the_test
        type(test_result_item_t) :: the_result

        the_test = It(units%to_string(), DOUBLE_PRECISION_GENERATOR, doCheck)
        the_result = the_test%run()
        result_ = assert_that(the_result%passed(), the_result%verbose_description(.false.))
    contains
        function doCheck(input) result(result__)
            class(input_t), intent(in) :: input
            type(result_t) :: result__

            type(error_list_t) :: errors
            type(length_t) :: original_length
            type(fallible_length_t) :: maybe_length
            type(length_t) :: new_length

            select type (input)
            type is (double_precision_input_t)
                original_length = input%input().unit.units
                maybe_length = parse_length( &
                        original_length%to_string_in(units))
                new_length = maybe_length%length()
                errors = maybe_length%errors()
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
