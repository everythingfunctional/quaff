module amount_temperature_rate_test
    use double_precision_generator_m, only: DOUBLE_PRECISION_GENERATOR
    use double_precision_pair_generator_m, only: DOUBLE_PRECISION_PAIR_GENERATOR
    use double_precision_pair_input_m, only: double_precision_pair_input_t
    use erloff, only: error_list_t
    use iso_varying_string, only: operator(//)
    use non_zero_double_precision_generator_m, only: &
            NON_ZERO_DOUBLE_PRECISION_GENERATOR
    use non_zero_double_precision_pair_generator_m, only: &
            NON_ZERO_DOUBLE_PRECISION_PAIR_GENERATOR
    use quaff, only: &
            amount_temperature_rate_t, &
            fallible_amount_temperature_rate_t, &
            amount_temperature_rate_unit_t, &
            operator(.unit.), &
            parse_amount_temperature_rate, &
            sum, &
            PROVIDED_AMOUNT_TEMPERATURE_RATE_UNITS, &
            MOLS_KELVIN_PER_SECOND
    use quaff_asserts_m, only: assert_equals, assert_equals_within_relative
    use quaff_utilities_m, only: PARSE_ERROR
    use amount_temperature_rate_utilities_m, only: &
            units_input_t, units_pair_input_t, make_units_examples
    use units_examples_m, only: units_examples_t
    use veggies, only: &
            double_precision_input_t, &
            input_t, &
            result_t, &
            test_item_t, &
            test_result_item_t, &
            assert_equals, &
            assert_equals_within_relative, &
            assert_not, &
            assert_that, &
            describe, &
            fail, &
            it

    implicit none
    private
    public :: test_amount_temperature_rate
contains
    function test_amount_temperature_rate() result(tests)
        type(test_item_t) :: tests

        type(units_examples_t) :: examples

        examples = make_units_examples(PROVIDED_AMOUNT_TEMPERATURE_RATE_UNITS)
        tests = describe( &
                "amount_temperature_rate_t", &
                [ it( &
                        "returns the same value given the same units", &
                        examples%units(), &
                        check_round_trip) &
                , it( &
                        "the conversion factors between two units are inverses", &
                        examples%pairs(), &
                        check_conversion_factors_inverse) &
                , it( &
                        "preserves its value converting to and from a string", &
                        examples%units(), &
                        check_to_and_from_string) &
                , it( &
                        "returns an error trying to parse a bad string", &
                        check_bad_string) &
                , it( &
                        "returns an error trying to parse an unknown unit", &
                        check_bad_unit) &
                , it( &
                        "returns an error trying to parse a bad number", &
                        check_bad_number) &
                , it("arrays can be summed", check_sum) &
                , it( &
                        "adding zero returns the original amount_temperature_rate", &
                        DOUBLE_PRECISION_GENERATOR, &
                        check_add_zero) &
                , it( &
                        "subtracting zero returns the original amount_temperature_rate", &
                        DOUBLE_PRECISION_GENERATOR, &
                        check_subtract_zero) &
                , it( &
                        "adding and subtracting the same amount_temperature_rate returns the original amount_temperature_rate", &
                        DOUBLE_PRECISION_PAIR_GENERATOR, &
                        check_add_subtract) &
                , it( &
                        "multiplying by one returns the original amount_temperature_rate", &
                        DOUBLE_PRECISION_GENERATOR, &
                        check_multiply_by_one) &
                , it( &
                        "multiplying by zero returns zero amount_temperature_rate", &
                        DOUBLE_PRECISION_GENERATOR, &
                        check_multiply_by_zero) &
                , it( &
                        "dividing by one returns the original amount_temperature_rate", &
                        DOUBLE_PRECISION_GENERATOR, &
                        check_divide_by_one) &
                , it( &
                        "dividing by itself returns one", &
                        NON_ZERO_DOUBLE_PRECISION_GENERATOR, &
                        check_divide_by_self) &
                , it( &
                        "multiplying and dividing by the same number returns the original amount_temperature_rate", &
                        NON_ZERO_DOUBLE_PRECISION_PAIR_GENERATOR, &
                        check_multiply_divide) &
                , describe( &
                        "operator(==)", &
                        [ it( &
                                "is true for the same amount_temperature_rate", &
                                DOUBLE_PRECISION_GENERATOR, &
                                check_equal_with_same_number) &
                        , it( &
                                "is false for different amount_temperature_rates", &
                                DOUBLE_PRECISION_GENERATOR, &
                                check_equal_with_different_numbers) &
                        ]) &
                , describe( &
                        "operator(/=)", &
                        [ it( &
                                "is false for the same amount_temperature_rate", &
                                DOUBLE_PRECISION_GENERATOR, &
                                check_not_equal_with_same_number) &
                        , it( &
                                "is true for different amount_temperature_rates", &
                                DOUBLE_PRECISION_GENERATOR, &
                                check_not_equal_with_different_numbers) &
                        ]) &
                , describe( &
                        "%equal(amount_temperature_rate, within)", &
                        [ it( &
                                "is true for the same amount_temperature_rate even for tiny tolerance", &
                                DOUBLE_PRECISION_GENERATOR, &
                                check_equal_within_with_same_number) &
                        , it( &
                                "is true for sufficiently close values", &
                                DOUBLE_PRECISION_GENERATOR, &
                                check_equal_within_with_close_numbers) &
                        , it( &
                                "is false for sufficiently different numbers", &
                                DOUBLE_PRECISION_GENERATOR, &
                                check_equal_within_with_different_numbers) &
                        ]) &
                , describe( &
                        "operator(>=)", &
                        [ it( &
                                "is true if the lhs is greater than the rhs", &
                                DOUBLE_PRECISION_GENERATOR, &
                                check_greater_than_or_equal_with_greater_number) &
                        , it( &
                                "is true if the lhs is equal to the rhs", &
                                DOUBLE_PRECISION_GENERATOR, &
                                check_greater_than_or_equal_with_same_numbers) &
                        , it( &
                                "is false if the lhs is less than the rhs", &
                                DOUBLE_PRECISION_GENERATOR, &
                                check_greater_than_or_equal_with_lesser_number) &
                        ]) &
                , describe( &
                        "operator(<=)", &
                        [ it( &
                                "is true if the lhs is less than the rhs", &
                                DOUBLE_PRECISION_GENERATOR, &
                                check_less_than_or_equal_with_less_number) &
                        , it( &
                                "is true if the lhs is equal to the rhs", &
                                DOUBLE_PRECISION_GENERATOR, &
                                check_less_than_or_equal_with_same_numbers) &
                        , it( &
                                "is false if the lhs is greater than the rhs", &
                                DOUBLE_PRECISION_GENERATOR, &
                                check_less_than_or_equal_with_greater_number) &
                        ]) &
                , describe( &
                        "operator(>)", &
                        [ it( &
                                "is true if the lhs is greater than the rhs", &
                                DOUBLE_PRECISION_GENERATOR, &
                                check_greater_than_with_greater_number) &
                        , it( &
                                "is false if the lhs is equal to the rhs", &
                                DOUBLE_PRECISION_GENERATOR, &
                                check_greater_than_with_same_numbers) &
                        , it( &
                                "is false if the lhs is less than the rhs", &
                                DOUBLE_PRECISION_GENERATOR, &
                                check_greater_than_with_lesser_number) &
                        ]) &
                , describe( &
                        "operator(<)", &
                        [ it( &
                                "is true if the lhs is less than the rhs", &
                                DOUBLE_PRECISION_GENERATOR, &
                                check_less_than_with_less_number) &
                        , it( &
                                "is false if the lhs is equal to the rhs", &
                                DOUBLE_PRECISION_GENERATOR, &
                                check_less_than_with_same_numbers) &
                        , it( &
                                "is false if the lhs is greater than the rhs", &
                                DOUBLE_PRECISION_GENERATOR, &
                                check_less_than_with_greater_number) &
                        ]) &
                ])
    end function

    function check_round_trip(input) result(result_)
        class(input_t), intent(in) :: input
        type(result_t) :: result_

        select type (input)
        type is (units_input_t)
            result_ = check_round_trip_in(input%unit())
        class default
            result_ = fail("Expected to get a units_input_t")
        end select
    end function

    function check_round_trip_in(units) result(result_)
        class(amount_temperature_rate_unit_t), intent(in) :: units
        type(result_t) :: result_

        type(test_item_t) :: the_test
        type(test_result_item_t) :: the_result

        the_test = it(units%to_string(), DOUBLE_PRECISION_GENERATOR, check_round_trip_)
        the_result = the_test%run()
        result_ = assert_that(the_result%passed(), the_result%verbose_description(.false.))
    contains
        pure function check_round_trip_(input) result(result__)
            class(input_t), intent(in) :: input
            type(result_t) :: result__

            type(amount_temperature_rate_t) :: intermediate

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
        end function
    end function

    function check_conversion_factors_inverse(input) result(result_)
        class(input_t), intent(in) :: input
        type(result_t) :: result_

        select type (input)
        type is (units_pair_input_t)
            result_ = check_conversion_factors_are_inverse(input%first(), input%second_())
        class default
            result_ = fail("Expected to get a units_pair_input_t")
        end select
    end function

    pure function check_conversion_factors_are_inverse( &
            from, to) result(result_)
        class(amount_temperature_rate_unit_t), intent(in) :: to
        class(amount_temperature_rate_unit_t), intent(in) :: from
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
    end function

    function check_to_and_from_string(input) result(result_)
        class(input_t), intent(in) :: input
        type(result_t) :: result_

        select type (input)
        type is (units_input_t)
            result_ = check_string_trip(input%unit())
        class default
            result_ = fail("Expected to get an units_input_t")
        end select
    end function

    function check_string_trip(units) result(result_)
        class(amount_temperature_rate_unit_t), intent(in) :: units
        type(result_t) :: result_

        type(test_item_t) :: the_test
        type(test_result_item_t) :: the_result

        the_test = it(units%to_string(), DOUBLE_PRECISION_GENERATOR, do_check)
        the_result = the_test%run()
        result_ = assert_that(the_result%passed(), the_result%verbose_description(.false.))
    contains
        function do_check(input) result(result__)
            class(input_t), intent(in) :: input
            type(result_t) :: result__

            type(error_list_t) :: errors
            type(amount_temperature_rate_t) :: original_amount_temperature_rate
            type(fallible_amount_temperature_rate_t) :: maybe_amount_temperature_rate
            type(amount_temperature_rate_t) :: new_amount_temperature_rate

            select type (input)
            type is (double_precision_input_t)
                original_amount_temperature_rate = input%input().unit.units
                maybe_amount_temperature_rate = parse_amount_temperature_rate( &
                        original_amount_temperature_rate%to_string_in(units))
                new_amount_temperature_rate = maybe_amount_temperature_rate%amount_temperature_rate()
                errors = maybe_amount_temperature_rate%errors()
                result__ = &
                        assert_equals( &
                                original_amount_temperature_rate, &
                                new_amount_temperature_rate) &
                        .and.assert_not(errors%has_any(), errors%to_string())
            class default
                result__ = fail("Expected to get a double_precision_input_t")
            end select
        end function
    end function

    function check_bad_string() result(result_)
        type(result_t) :: result_

        type(error_list_t) :: errors
        type(fallible_amount_temperature_rate_t) :: maybe_amount_temperature_rate

        maybe_amount_temperature_rate = parse_amount_temperature_rate("bad")
        errors = maybe_amount_temperature_rate%errors()
        result_ = assert_that(errors.hasType.PARSE_ERROR, errors%to_string())
    end function

    function check_bad_unit() result(result_)
        type(result_t) :: result_

        type(error_list_t) :: errors
        type(fallible_amount_temperature_rate_t) :: maybe_amount_temperature_rate

        maybe_amount_temperature_rate = parse_amount_temperature_rate("1.0 (mol K)/sbad", [MOLS_KELVIN_PER_SECOND])
        errors = maybe_amount_temperature_rate%errors()
        result_ = assert_that(errors.hasType.PARSE_ERROR, errors%to_string())
    end function

    function check_bad_number() result(result_)
        type(result_t) :: result_

        type(error_list_t) :: errors
        type(fallible_amount_temperature_rate_t) :: maybe_amount_temperature_rate

        maybe_amount_temperature_rate = parse_amount_temperature_rate("bad (mol K)/s")
        errors = maybe_amount_temperature_rate%errors()
        result_ = assert_that(errors.hasType.PARSE_ERROR, errors%to_string())
    end function

    pure function check_sum() result(result_)
        type(result_t) :: result_

        double precision, parameter :: numbers(*) = [1.0d0, 2.0d0, 3.0d0]

        result_ = assert_equals( &
                sum(numbers).unit.MOLS_KELVIN_PER_SECOND, &
                sum(numbers.unit.MOLS_KELVIN_PER_SECOND))
    end function

    pure function check_add_zero(input) result(result_)
        class(input_t), intent(in) :: input
        type(result_t) :: result_

        type(amount_temperature_rate_t) :: amount_temperature_rate
        type(amount_temperature_rate_t) :: zero

        select type(input)
        type is (double_precision_input_t)
            amount_temperature_rate = input%input().unit.MOLS_KELVIN_PER_SECOND
            zero = 0.0d0.unit.MOLS_KELVIN_PER_SECOND
            result_ = assert_equals(amount_temperature_rate, amount_temperature_rate + zero)
        class default
            result_ = fail("Expected a double_precision_input_t")
        end select
    end function

    pure function check_subtract_zero(input) result(result_)
        class(input_t), intent(in) :: input
        type(result_t) :: result_

        type(amount_temperature_rate_t) :: amount_temperature_rate
        type(amount_temperature_rate_t) :: zero

        select type(input)
        type is (double_precision_input_t)
            amount_temperature_rate = input%input().unit.MOLS_KELVIN_PER_SECOND
            zero = 0.0d0.unit.MOLS_KELVIN_PER_SECOND
            result_ = assert_equals(amount_temperature_rate, amount_temperature_rate - zero)
        class default
            result_ = fail("Expected a double_precision_input_t")
        end select
    end function

    pure function check_add_subtract(input) result(result_)
        class(input_t), intent(in) :: input
        type(result_t) :: result_

        type(amount_temperature_rate_t) :: amount_temperature_rate1
        type(amount_temperature_rate_t) :: amount_temperature_rate2

        select type(input)
        type is (double_precision_pair_input_t)
            amount_temperature_rate1 = input%first().unit.MOLS_KELVIN_PER_SECOND
            amount_temperature_rate2 = input%second_().unit.MOLS_KELVIN_PER_SECOND
            result_ = assert_equals_within_relative( &
                    amount_temperature_rate1, &
                    (amount_temperature_rate1 + amount_temperature_rate2) - amount_temperature_rate2, &
                    1.0d-8, &
                    "amount_temperature_rate1 = " // amount_temperature_rate1%to_string() &
                    // ", amount_temperature_rate2 = " // amount_temperature_rate2%to_string())
        class default
            result_ = fail("Expected a double_precision_pair_input_t")
        end select
    end function

    pure function check_multiply_by_one(input) result(result_)
        class(input_t), intent(in) :: input
        type(result_t) :: result_

        type(amount_temperature_rate_t) :: amount_temperature_rate

        select type(input)
        type is (double_precision_input_t)
            amount_temperature_rate = input%input().unit.MOLS_KELVIN_PER_SECOND
            result_ = assert_equals(amount_temperature_rate, amount_temperature_rate * 1.0d0)
        class default
            result_ = fail("Expected a double_precision_input_t")
        end select
    end function

    pure function check_multiply_by_zero(input) result(result_)
        class(input_t), intent(in) :: input
        type(result_t) :: result_

        type(amount_temperature_rate_t) :: amount_temperature_rate
        type(amount_temperature_rate_t) :: zero

        select type(input)
        type is (double_precision_input_t)
            amount_temperature_rate = input%input().unit.MOLS_KELVIN_PER_SECOND
            zero = 0.0d0.unit.MOLS_KELVIN_PER_SECOND
            result_ = assert_equals(zero, amount_temperature_rate * 0.0d0)
        class default
            result_ = fail("Expected a double_precision_input_t")
        end select
    end function

    pure function check_divide_by_one(input) result(result_)
        class(input_t), intent(in) :: input
        type(result_t) :: result_

        type(amount_temperature_rate_t) :: amount_temperature_rate

        select type(input)
        type is (double_precision_input_t)
            amount_temperature_rate = input%input().unit.MOLS_KELVIN_PER_SECOND
            result_ = assert_equals(amount_temperature_rate, amount_temperature_rate / 1.0d0)
        class default
            result_ = fail("Expected a double_precision_input_t")
        end select
    end function

    pure function check_divide_by_self(input) result(result_)
        class(input_t), intent(in) :: input
        type(result_t) :: result_

        type(amount_temperature_rate_t) :: amount_temperature_rate

        select type(input)
        type is (double_precision_input_t)
            amount_temperature_rate = input%input().unit.MOLS_KELVIN_PER_SECOND
            result_ = assert_equals(1.0d0, amount_temperature_rate / amount_temperature_rate)
        class default
            result_ = fail("Expected a double_precision_input_t")
        end select
    end function

    pure function check_multiply_divide(input) result(result_)
        class(input_t), intent(in) :: input
        type(result_t) :: result_

        type(amount_temperature_rate_t) :: amount_temperature_rate

        select type (input)
        type is (double_precision_pair_input_t)
            amount_temperature_rate = input%first().unit.MOLS_KELVIN_PER_SECOND
            result_ = assert_equals(amount_temperature_rate, (amount_temperature_rate * input%second_()) / input%second_())
        class default
            result_ = fail("Expected a double_precision_pair_input_t")
        end select
    end function

    pure function check_equal_with_same_number(input) result(result_)
        class(input_t), intent(in) :: input
        type(result_t) :: result_

        type(amount_temperature_rate_t) :: the_amount_temperature_rate

        select type (input)
        type is (double_precision_input_t)
            the_amount_temperature_rate = input%input().unit.MOLS_KELVIN_PER_SECOND
            result_ = assert_that( &
                    the_amount_temperature_rate == the_amount_temperature_rate, &
                    the_amount_temperature_rate%to_string() // " == " // the_amount_temperature_rate%to_string())
        class default
            result_ = fail("Expected to get a double_precision_input_t")
        end select
    end function

    pure function check_equal_with_different_numbers(input) result(result_)
        class(input_t), intent(in) :: input
        type(result_t) :: result_

        type(amount_temperature_rate_t) :: amount_temperature_rate1
        type(amount_temperature_rate_t) :: amount_temperature_rate2

        select type (input)
        type is (double_precision_input_t)
            amount_temperature_rate1 = input%input().unit.MOLS_KELVIN_PER_SECOND
            amount_temperature_rate2 = (input%input() + 1.0d0).unit.MOLS_KELVIN_PER_SECOND
            result_ = assert_not( &
                    amount_temperature_rate1 == amount_temperature_rate2, &
                    amount_temperature_rate1%to_string() // " == " // amount_temperature_rate2%to_string())
        class default
            result_ = fail("Expected to get a double_precision_input_t")
        end select
    end function

    pure function check_not_equal_with_same_number(input) result(result_)
        class(input_t), intent(in) :: input
        type(result_t) :: result_

        type(amount_temperature_rate_t) :: the_amount_temperature_rate

        select type (input)
        type is (double_precision_input_t)
            the_amount_temperature_rate = input%input().unit.MOLS_KELVIN_PER_SECOND
            result_ = assert_not( &
                    the_amount_temperature_rate /= the_amount_temperature_rate, &
                    the_amount_temperature_rate%to_string() // " /= " // the_amount_temperature_rate%to_string())
        class default
            result_ = fail("Expected to get a double_precision_input_t")
        end select
    end function

    pure function check_not_equal_with_different_numbers(input) result(result_)
        class(input_t), intent(in) :: input
        type(result_t) :: result_

        type(amount_temperature_rate_t) :: amount_temperature_rate1
        type(amount_temperature_rate_t) :: amount_temperature_rate2

        select type (input)
        type is (double_precision_input_t)
            amount_temperature_rate1 = input%input().unit.MOLS_KELVIN_PER_SECOND
            amount_temperature_rate2 = (input%input() + 1.0d0).unit.MOLS_KELVIN_PER_SECOND
            result_ = assert_that( &
                    amount_temperature_rate1 /= amount_temperature_rate2, &
                    amount_temperature_rate1%to_string() // " /= " // amount_temperature_rate2%to_string())
        class default
            result_ = fail("Expected to get a double_precision_input_t")
        end select
    end function

    pure function check_equal_within_with_same_number(input) result(result_)
        class(input_t), intent(in) :: input
        type(result_t) :: result_

        type(amount_temperature_rate_t) :: the_amount_temperature_rate
        type(amount_temperature_rate_t) :: tolerance

        select type (input)
        type is (double_precision_input_t)
            the_amount_temperature_rate = input%input().unit.MOLS_KELVIN_PER_SECOND
            tolerance = tiny(1.0d0).unit.MOLS_KELVIN_PER_SECOND
            result_ = assert_that( &
                    the_amount_temperature_rate%equal(the_amount_temperature_rate, within = tolerance), &
                    "(" // the_amount_temperature_rate%to_string() // ")%equal(" &
                        // the_amount_temperature_rate%to_string() // ", within = " &
                        // tolerance%to_string() // ")")
        class default
            result_ = fail("Expected to get a double_precision_input_t")
        end select
    end function

    pure function check_equal_within_with_close_numbers(input) result(result_)
        class(input_t), intent(in) :: input
        type(result_t) :: result_

        type(amount_temperature_rate_t) :: amount_temperature_rate1
        type(amount_temperature_rate_t) :: amount_temperature_rate2
        type(amount_temperature_rate_t) :: tolerance

        select type (input)
        type is (double_precision_input_t)
            amount_temperature_rate1 = input%input().unit.MOLS_KELVIN_PER_SECOND
            amount_temperature_rate2 = (input%input() + 0.05d0).unit.MOLS_KELVIN_PER_SECOND
            tolerance = 0.1d0.unit.MOLS_KELVIN_PER_SECOND
            result_ = assert_that( &
                    amount_temperature_rate1%equal(amount_temperature_rate2, within = tolerance), &
                    "(" // amount_temperature_rate1%to_string() // ")%equal(" &
                        // amount_temperature_rate2%to_string() // ", within = " &
                        // tolerance%to_string() // ")")
        class default
            result_ = fail("Expected to get a double_precision_input_t")
        end select
    end function

    pure function check_equal_within_with_different_numbers(input) result(result_)
        class(input_t), intent(in) :: input
        type(result_t) :: result_

        type(amount_temperature_rate_t) :: amount_temperature_rate1
        type(amount_temperature_rate_t) :: amount_temperature_rate2
        type(amount_temperature_rate_t) :: tolerance

        select type (input)
        type is (double_precision_input_t)
            amount_temperature_rate1 = input%input().unit.MOLS_KELVIN_PER_SECOND
            amount_temperature_rate2 = (input%input() + 0.2d0).unit.MOLS_KELVIN_PER_SECOND
            tolerance = 0.1d0.unit.MOLS_KELVIN_PER_SECOND
            result_ = assert_not( &
                    amount_temperature_rate1%equal(amount_temperature_rate2, within = tolerance), &
                    "(" // amount_temperature_rate1%to_string() // ")%equal(" &
                    // amount_temperature_rate2%to_string() // ", within = " &
                    // tolerance%to_string() // ")")
        class default
            result_ = fail("Expected to get a double_precision_input_t")
        end select
    end function

    pure function check_greater_than_or_equal_with_greater_number(input) result(result_)
        class(input_t), intent(in) :: input
        type(result_t) :: result_

        type(amount_temperature_rate_t) :: amount_temperature_rate1
        type(amount_temperature_rate_t) :: amount_temperature_rate2

        select type (input)
        type is (double_precision_input_t)
            amount_temperature_rate1 = input%input().unit. MOLS_KELVIN_PER_SECOND
            amount_temperature_rate2 = (input%input() - 1.0d0).unit.MOLS_KELVIN_PER_SECOND
            result_ = assert_that( &
                    amount_temperature_rate1 >= amount_temperature_rate2, &
                    amount_temperature_rate1%to_string() // " >= " // amount_temperature_rate2%to_string())
        class default
            result_ = fail("Expected to get a double_precision_input_t")
        end select
    end function

    pure function check_greater_than_or_equal_with_same_numbers(input) result(result_)
        class(input_t), intent(in) :: input
        type(result_t) :: result_

        type(amount_temperature_rate_t) :: amount_temperature_rate1
        type(amount_temperature_rate_t) :: amount_temperature_rate2

        select type (input)
        type is (double_precision_input_t)
            amount_temperature_rate1 = input%input().unit.MOLS_KELVIN_PER_SECOND
            amount_temperature_rate2 = input%input().unit.MOLS_KELVIN_PER_SECOND
            result_ = assert_that( &
                    amount_temperature_rate1 >= amount_temperature_rate2, &
                    amount_temperature_rate1%to_string() // " >= " // amount_temperature_rate2%to_string())
        class default
            result_ = fail("Expected to get a double_precision_input_t")
        end select
    end function

    pure function check_greater_than_or_equal_with_lesser_number(input) result(result_)
        class(input_t), intent(in) :: input
        type(result_t) :: result_

        type(amount_temperature_rate_t) :: amount_temperature_rate1
        type(amount_temperature_rate_t) :: amount_temperature_rate2

        select type (input)
        type is (double_precision_input_t)
            amount_temperature_rate1 = input%input().unit.MOLS_KELVIN_PER_SECOND
            amount_temperature_rate2 = (input%input() + 1.0d0).unit.MOLS_KELVIN_PER_SECOND
            result_ = assert_not( &
                    amount_temperature_rate1 >= amount_temperature_rate2, &
                    amount_temperature_rate1%to_string() // " >= " // amount_temperature_rate2%to_string())
        class default
            result_ = fail("Expected to get a double_precision_input_t")
        end select
    end function

    pure function check_less_than_or_equal_with_less_number(input) result(result_)
        class(input_t), intent(in) :: input
        type(result_t) :: result_

        type(amount_temperature_rate_t) :: amount_temperature_rate1
        type(amount_temperature_rate_t) :: amount_temperature_rate2

        select type (input)
        type is (double_precision_input_t)
            amount_temperature_rate1 = input%input().unit. MOLS_KELVIN_PER_SECOND
            amount_temperature_rate2 = (input%input() + 1.0d0).unit.MOLS_KELVIN_PER_SECOND
            result_ = assert_that( &
                    amount_temperature_rate1 <= amount_temperature_rate2, &
                    amount_temperature_rate1%to_string() // " <= " // amount_temperature_rate2%to_string())
        class default
            result_ = fail("Expected to get a double_precision_input_t")
        end select
    end function

    pure function check_less_than_or_equal_with_same_numbers(input) result(result_)
        class(input_t), intent(in) :: input
        type(result_t) :: result_

        type(amount_temperature_rate_t) :: amount_temperature_rate1
        type(amount_temperature_rate_t) :: amount_temperature_rate2

        select type (input)
        type is (double_precision_input_t)
            amount_temperature_rate1 = input%input().unit.MOLS_KELVIN_PER_SECOND
            amount_temperature_rate2 = input%input().unit.MOLS_KELVIN_PER_SECOND
            result_ = assert_that( &
                    amount_temperature_rate1 <= amount_temperature_rate2, &
                    amount_temperature_rate1%to_string() // " <= " // amount_temperature_rate2%to_string())
        class default
            result_ = fail("Expected to get a double_precision_input_t")
        end select
    end function

    pure function check_less_than_or_equal_with_greater_number(input) result(result_)
        class(input_t), intent(in) :: input
        type(result_t) :: result_

        type(amount_temperature_rate_t) :: amount_temperature_rate1
        type(amount_temperature_rate_t) :: amount_temperature_rate2

        select type (input)
        type is (double_precision_input_t)
            amount_temperature_rate1 = input%input().unit.MOLS_KELVIN_PER_SECOND
            amount_temperature_rate2 = (input%input() - 1.0d0).unit.MOLS_KELVIN_PER_SECOND
            result_ = assert_not( &
                    amount_temperature_rate1 <= amount_temperature_rate2, &
                    amount_temperature_rate1%to_string() // " <= " // amount_temperature_rate2%to_string())
        class default
            result_ = fail("Expected to get a double_precision_input_t")
        end select
    end function

    pure function check_greater_than_with_greater_number(input) result(result_)
        class(input_t), intent(in) :: input
        type(result_t) :: result_

        type(amount_temperature_rate_t) :: amount_temperature_rate1
        type(amount_temperature_rate_t) :: amount_temperature_rate2

        select type (input)
        type is (double_precision_input_t)
            amount_temperature_rate1 = input%input().unit. MOLS_KELVIN_PER_SECOND
            amount_temperature_rate2 = (input%input() - 1.0d0).unit.MOLS_KELVIN_PER_SECOND
            result_ = assert_that( &
                    amount_temperature_rate1 > amount_temperature_rate2, &
                    amount_temperature_rate1%to_string() // " > " // amount_temperature_rate2%to_string())
        class default
            result_ = fail("Expected to get a double_precision_input_t")
        end select
    end function

    pure function check_greater_than_with_same_numbers(input) result(result_)
        class(input_t), intent(in) :: input
        type(result_t) :: result_

        type(amount_temperature_rate_t) :: amount_temperature_rate1
        type(amount_temperature_rate_t) :: amount_temperature_rate2

        select type (input)
        type is (double_precision_input_t)
            amount_temperature_rate1 = input%input().unit.MOLS_KELVIN_PER_SECOND
            amount_temperature_rate2 = input%input().unit.MOLS_KELVIN_PER_SECOND
            result_ = assert_not( &
                    amount_temperature_rate1 > amount_temperature_rate2, &
                    amount_temperature_rate1%to_string() // " > " // amount_temperature_rate2%to_string())
        class default
            result_ = fail("Expected to get a double_precision_input_t")
        end select
    end function

    pure function check_greater_than_with_lesser_number(input) result(result_)
        class(input_t), intent(in) :: input
        type(result_t) :: result_

        type(amount_temperature_rate_t) :: amount_temperature_rate1
        type(amount_temperature_rate_t) :: amount_temperature_rate2

        select type (input)
        type is (double_precision_input_t)
            amount_temperature_rate1 = input%input().unit.MOLS_KELVIN_PER_SECOND
            amount_temperature_rate2 = (input%input() + 1.0d0).unit.MOLS_KELVIN_PER_SECOND
            result_ = assert_not( &
                    amount_temperature_rate1 > amount_temperature_rate2, &
                    amount_temperature_rate1%to_string() // " > " // amount_temperature_rate2%to_string())
        class default
            result_ = fail("Expected to get a double_precision_input_t")
        end select
    end function

    pure function check_less_than_with_less_number(input) result(result_)
        class(input_t), intent(in) :: input
        type(result_t) :: result_

        type(amount_temperature_rate_t) :: amount_temperature_rate1
        type(amount_temperature_rate_t) :: amount_temperature_rate2

        select type (input)
        type is (double_precision_input_t)
            amount_temperature_rate1 = input%input().unit. MOLS_KELVIN_PER_SECOND
            amount_temperature_rate2 = (input%input() + 1.0d0).unit.MOLS_KELVIN_PER_SECOND
            result_ = assert_that( &
                    amount_temperature_rate1 < amount_temperature_rate2, &
                    amount_temperature_rate1%to_string() // " < " // amount_temperature_rate2%to_string())
        class default
            result_ = fail("Expected to get a double_precision_input_t")
        end select
    end function

    pure function check_less_than_with_same_numbers(input) result(result_)
        class(input_t), intent(in) :: input
        type(result_t) :: result_

        type(amount_temperature_rate_t) :: amount_temperature_rate1
        type(amount_temperature_rate_t) :: amount_temperature_rate2

        select type (input)
        type is (double_precision_input_t)
            amount_temperature_rate1 = input%input().unit.MOLS_KELVIN_PER_SECOND
            amount_temperature_rate2 = input%input().unit.MOLS_KELVIN_PER_SECOND
            result_ = assert_not( &
                    amount_temperature_rate1 < amount_temperature_rate2, &
                    amount_temperature_rate1%to_string() // " <=" // amount_temperature_rate2%to_string())
        class default
            result_ = fail("Expected to get a double_precision_input_t")
        end select
    end function

    pure function check_less_than_with_greater_number(input) result(result_)
        class(input_t), intent(in) :: input
        type(result_t) :: result_

        type(amount_temperature_rate_t) :: amount_temperature_rate1
        type(amount_temperature_rate_t) :: amount_temperature_rate2

        select type (input)
        type is (double_precision_input_t)
            amount_temperature_rate1 = input%input().unit.MOLS_KELVIN_PER_SECOND
            amount_temperature_rate2 = (input%input() - 1.0d0).unit.MOLS_KELVIN_PER_SECOND
            result_ = assert_not( &
                    amount_temperature_rate1 < amount_temperature_rate2, &
                    amount_temperature_rate1%to_string() // " < " // amount_temperature_rate2%to_string())
        class default
            result_ = fail("Expected to get a double_precision_input_t")
        end select
    end function
end module
