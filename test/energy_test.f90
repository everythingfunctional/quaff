module energy_test
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
            energy_t, &
            fallible_energy_t, &
            energy_unit_t, &
            operator(.unit.), &
            parse_energy, &
            sum, &
            abs, &
            PROVIDED_ENERGY_UNITS, &
            JOULES
    use quaff_asserts_m, only: assert_equals, assert_equals_within_relative
    use quaff_utilities_m, only: PARSE_ERROR
    use energy_utilities_m, only: &
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
    public :: test_energy
contains
    function test_energy() result(tests)
        type(test_item_t) :: tests

        type(units_examples_t) :: examples

        examples = make_units_examples(PROVIDED_ENERGY_UNITS)
        tests = describe( &
                "energy_t", &
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
                , it("can take the absolute value", check_abs) &
                , it("can be negated", DOUBLE_PRECISION_GENERATOR, check_negation) &
                , it( &
                        "adding zero returns the original energy", &
                        DOUBLE_PRECISION_GENERATOR, &
                        check_add_zero) &
                , it( &
                        "subtracting zero returns the original energy", &
                        DOUBLE_PRECISION_GENERATOR, &
                        check_subtract_zero) &
                , it( &
                        "adding and subtracting the same energy returns the original energy", &
                        DOUBLE_PRECISION_PAIR_GENERATOR, &
                        check_add_subtract) &
                , it( &
                        "multiplying by one returns the original energy", &
                        DOUBLE_PRECISION_GENERATOR, &
                        check_multiply_by_one) &
                , it( &
                        "multiplying by zero returns zero energy", &
                        DOUBLE_PRECISION_GENERATOR, &
                        check_multiply_by_zero) &
                , it( &
                        "dividing by one returns the original energy", &
                        DOUBLE_PRECISION_GENERATOR, &
                        check_divide_by_one) &
                , it( &
                        "dividing by itself returns one", &
                        NON_ZERO_DOUBLE_PRECISION_GENERATOR, &
                        check_divide_by_self) &
                , it( &
                        "multiplying and dividing by the same number returns the original energy", &
                        NON_ZERO_DOUBLE_PRECISION_PAIR_GENERATOR, &
                        check_multiply_divide) &
                , describe( &
                        "operator(==)", &
                        [ it( &
                                "is true for the same energy", &
                                DOUBLE_PRECISION_GENERATOR, &
                                check_equal_with_same_number) &
                        , it( &
                                "is false for different energys", &
                                DOUBLE_PRECISION_GENERATOR, &
                                check_equal_with_different_numbers) &
                        ]) &
                , describe( &
                        "operator(/=)", &
                        [ it( &
                                "is false for the same energy", &
                                DOUBLE_PRECISION_GENERATOR, &
                                check_not_equal_with_same_number) &
                        , it( &
                                "is true for different energys", &
                                DOUBLE_PRECISION_GENERATOR, &
                                check_not_equal_with_different_numbers) &
                        ]) &
                , describe( &
                        "%equal(energy, within)", &
                        [ it( &
                                "is true for the same energy even for tiny tolerance", &
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
        class(energy_unit_t), intent(in) :: units
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

            type(energy_t) :: intermediate

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
        class(energy_unit_t), intent(in) :: to
        class(energy_unit_t), intent(in) :: from
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
        class(energy_unit_t), intent(in) :: units
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
            type(energy_t) :: original_energy
            type(fallible_energy_t) :: maybe_energy
            type(energy_t) :: new_energy

            select type (input)
            type is (double_precision_input_t)
                original_energy = input%input().unit.units
                maybe_energy = parse_energy( &
                        original_energy%to_string_in(units))
                new_energy = maybe_energy%energy()
                errors = maybe_energy%errors()
                result__ = &
                        assert_equals( &
                                original_energy, &
                                new_energy) &
                        .and.assert_not(errors%has_any(), errors%to_string())
            class default
                result__ = fail("Expected to get a double_precision_input_t")
            end select
        end function
    end function

    function check_bad_string() result(result_)
        type(result_t) :: result_

        type(error_list_t) :: errors
        type(fallible_energy_t) :: maybe_energy

        maybe_energy = parse_energy("bad")
        errors = maybe_energy%errors()
        result_ = assert_that(errors.hasType.PARSE_ERROR, errors%to_string())
    end function

    function check_bad_unit() result(result_)
        type(result_t) :: result_

        type(error_list_t) :: errors
        type(fallible_energy_t) :: maybe_energy

        maybe_energy = parse_energy("1.0 Jbad", [JOULES])
        errors = maybe_energy%errors()
        result_ = assert_that(errors.hasType.PARSE_ERROR, errors%to_string())
    end function

    function check_bad_number() result(result_)
        type(result_t) :: result_

        type(error_list_t) :: errors
        type(fallible_energy_t) :: maybe_energy

        maybe_energy = parse_energy("bad J")
        errors = maybe_energy%errors()
        result_ = assert_that(errors.hasType.PARSE_ERROR, errors%to_string())
    end function

    pure function check_sum() result(result_)
        type(result_t) :: result_

        double precision, parameter :: numbers(*) = [1.0d0, 2.0d0, 3.0d0]

        result_ = assert_equals( &
                sum(numbers).unit.JOULES, &
                sum(numbers.unit.JOULES))
    end function

    pure function check_abs() result(result_)
        type(result_t) :: result_

        result_ = &
                assert_equals( &
                        abs(1.0d0).unit.JOULES, &
                        abs(1.0d0.unit.JOULES)) &
                .and.assert_equals( &
                        abs(-1.0d0).unit.JOULES, &
                        abs((-1.0d0).unit.JOULES))
    end function

    pure function check_negation(input) result(result_)
        class(input_t), intent(in) :: input
        type(result_t) :: result_

        select type(input)
        type is (double_precision_input_t)
            result_ = assert_equals( &
                    (-input%input()).unit.JOULES, &
                    -(input%input().unit.JOULES))
        class default
            result_ = fail("Expected a double_precision_input_t")
        end select
    end function

    pure function check_add_zero(input) result(result_)
        class(input_t), intent(in) :: input
        type(result_t) :: result_

        type(energy_t) :: energy
        type(energy_t) :: zero

        select type(input)
        type is (double_precision_input_t)
            energy = input%input().unit.JOULES
            zero = 0.0d0.unit.JOULES
            result_ = assert_equals(energy, energy + zero)
        class default
            result_ = fail("Expected a double_precision_input_t")
        end select
    end function

    pure function check_subtract_zero(input) result(result_)
        class(input_t), intent(in) :: input
        type(result_t) :: result_

        type(energy_t) :: energy
        type(energy_t) :: zero

        select type(input)
        type is (double_precision_input_t)
            energy = input%input().unit.JOULES
            zero = 0.0d0.unit.JOULES
            result_ = assert_equals(energy, energy - zero)
        class default
            result_ = fail("Expected a double_precision_input_t")
        end select
    end function

    pure function check_add_subtract(input) result(result_)
        class(input_t), intent(in) :: input
        type(result_t) :: result_

        type(energy_t) :: energy1
        type(energy_t) :: energy2

        select type(input)
        type is (double_precision_pair_input_t)
            energy1 = input%first().unit.JOULES
            energy2 = input%second_().unit.JOULES
            result_ = assert_equals_within_relative( &
                    energy1, &
                    (energy1 + energy2) - energy2, &
                    1.0d-8, &
                    "energy1 = " // energy1%to_string() &
                    // ", energy2 = " // energy2%to_string())
        class default
            result_ = fail("Expected a double_precision_pair_input_t")
        end select
    end function

    pure function check_multiply_by_one(input) result(result_)
        class(input_t), intent(in) :: input
        type(result_t) :: result_

        type(energy_t) :: energy

        select type(input)
        type is (double_precision_input_t)
            energy = input%input().unit.JOULES
            result_ = assert_equals(energy, energy * 1.0d0)
        class default
            result_ = fail("Expected a double_precision_input_t")
        end select
    end function

    pure function check_multiply_by_zero(input) result(result_)
        class(input_t), intent(in) :: input
        type(result_t) :: result_

        type(energy_t) :: energy
        type(energy_t) :: zero

        select type(input)
        type is (double_precision_input_t)
            energy = input%input().unit.JOULES
            zero = 0.0d0.unit.JOULES
            result_ = assert_equals(zero, energy * 0.0d0)
        class default
            result_ = fail("Expected a double_precision_input_t")
        end select
    end function

    pure function check_divide_by_one(input) result(result_)
        class(input_t), intent(in) :: input
        type(result_t) :: result_

        type(energy_t) :: energy

        select type(input)
        type is (double_precision_input_t)
            energy = input%input().unit.JOULES
            result_ = assert_equals(energy, energy / 1.0d0)
        class default
            result_ = fail("Expected a double_precision_input_t")
        end select
    end function

    pure function check_divide_by_self(input) result(result_)
        class(input_t), intent(in) :: input
        type(result_t) :: result_

        type(energy_t) :: energy

        select type(input)
        type is (double_precision_input_t)
            energy = input%input().unit.JOULES
            result_ = assert_equals(1.0d0, energy / energy)
        class default
            result_ = fail("Expected a double_precision_input_t")
        end select
    end function

    pure function check_multiply_divide(input) result(result_)
        class(input_t), intent(in) :: input
        type(result_t) :: result_

        type(energy_t) :: energy

        select type (input)
        type is (double_precision_pair_input_t)
            energy = input%first().unit.JOULES
            result_ = assert_equals(energy, (energy * input%second_()) / input%second_())
        class default
            result_ = fail("Expected a double_precision_pair_input_t")
        end select
    end function

    pure function check_equal_with_same_number(input) result(result_)
        class(input_t), intent(in) :: input
        type(result_t) :: result_

        type(energy_t) :: the_energy

        select type (input)
        type is (double_precision_input_t)
            the_energy = input%input().unit.JOULES
            result_ = assert_that( &
                    the_energy == the_energy, &
                    the_energy%to_string() // " == " // the_energy%to_string())
        class default
            result_ = fail("Expected to get a double_precision_input_t")
        end select
    end function

    pure function check_equal_with_different_numbers(input) result(result_)
        class(input_t), intent(in) :: input
        type(result_t) :: result_

        type(energy_t) :: energy1
        type(energy_t) :: energy2

        select type (input)
        type is (double_precision_input_t)
            energy1 = input%input().unit.JOULES
            energy2 = (input%input() + 1.0d0).unit.JOULES
            result_ = assert_not( &
                    energy1 == energy2, &
                    energy1%to_string() // " == " // energy2%to_string())
        class default
            result_ = fail("Expected to get a double_precision_input_t")
        end select
    end function

    pure function check_not_equal_with_same_number(input) result(result_)
        class(input_t), intent(in) :: input
        type(result_t) :: result_

        type(energy_t) :: the_energy

        select type (input)
        type is (double_precision_input_t)
            the_energy = input%input().unit.JOULES
            result_ = assert_not( &
                    the_energy /= the_energy, &
                    the_energy%to_string() // " /= " // the_energy%to_string())
        class default
            result_ = fail("Expected to get a double_precision_input_t")
        end select
    end function

    pure function check_not_equal_with_different_numbers(input) result(result_)
        class(input_t), intent(in) :: input
        type(result_t) :: result_

        type(energy_t) :: energy1
        type(energy_t) :: energy2

        select type (input)
        type is (double_precision_input_t)
            energy1 = input%input().unit.JOULES
            energy2 = (input%input() + 1.0d0).unit.JOULES
            result_ = assert_that( &
                    energy1 /= energy2, &
                    energy1%to_string() // " /= " // energy2%to_string())
        class default
            result_ = fail("Expected to get a double_precision_input_t")
        end select
    end function

    pure function check_equal_within_with_same_number(input) result(result_)
        class(input_t), intent(in) :: input
        type(result_t) :: result_

        type(energy_t) :: the_energy
        type(energy_t) :: tolerance

        select type (input)
        type is (double_precision_input_t)
            the_energy = input%input().unit.JOULES
            tolerance = tiny(1.0d0).unit.JOULES
            result_ = assert_that( &
                    the_energy%equal(the_energy, within = tolerance), &
                    "(" // the_energy%to_string() // ")%equal(" &
                        // the_energy%to_string() // ", within = " &
                        // tolerance%to_string() // ")")
        class default
            result_ = fail("Expected to get a double_precision_input_t")
        end select
    end function

    pure function check_equal_within_with_close_numbers(input) result(result_)
        class(input_t), intent(in) :: input
        type(result_t) :: result_

        type(energy_t) :: energy1
        type(energy_t) :: energy2
        type(energy_t) :: tolerance

        select type (input)
        type is (double_precision_input_t)
            energy1 = input%input().unit.JOULES
            energy2 = (input%input() + 0.05d0).unit.JOULES
            tolerance = 0.1d0.unit.JOULES
            result_ = assert_that( &
                    energy1%equal(energy2, within = tolerance), &
                    "(" // energy1%to_string() // ")%equal(" &
                        // energy2%to_string() // ", within = " &
                        // tolerance%to_string() // ")")
        class default
            result_ = fail("Expected to get a double_precision_input_t")
        end select
    end function

    pure function check_equal_within_with_different_numbers(input) result(result_)
        class(input_t), intent(in) :: input
        type(result_t) :: result_

        type(energy_t) :: energy1
        type(energy_t) :: energy2
        type(energy_t) :: tolerance

        select type (input)
        type is (double_precision_input_t)
            energy1 = input%input().unit.JOULES
            energy2 = (input%input() + 0.2d0).unit.JOULES
            tolerance = 0.1d0.unit.JOULES
            result_ = assert_not( &
                    energy1%equal(energy2, within = tolerance), &
                    "(" // energy1%to_string() // ")%equal(" &
                    // energy2%to_string() // ", within = " &
                    // tolerance%to_string() // ")")
        class default
            result_ = fail("Expected to get a double_precision_input_t")
        end select
    end function

    pure function check_greater_than_or_equal_with_greater_number(input) result(result_)
        class(input_t), intent(in) :: input
        type(result_t) :: result_

        type(energy_t) :: energy1
        type(energy_t) :: energy2

        select type (input)
        type is (double_precision_input_t)
            energy1 = input%input().unit. JOULES
            energy2 = (input%input() - 1.0d0).unit.JOULES
            result_ = assert_that( &
                    energy1 >= energy2, &
                    energy1%to_string() // " >= " // energy2%to_string())
        class default
            result_ = fail("Expected to get a double_precision_input_t")
        end select
    end function

    pure function check_greater_than_or_equal_with_same_numbers(input) result(result_)
        class(input_t), intent(in) :: input
        type(result_t) :: result_

        type(energy_t) :: energy1
        type(energy_t) :: energy2

        select type (input)
        type is (double_precision_input_t)
            energy1 = input%input().unit.JOULES
            energy2 = input%input().unit.JOULES
            result_ = assert_that( &
                    energy1 >= energy2, &
                    energy1%to_string() // " >= " // energy2%to_string())
        class default
            result_ = fail("Expected to get a double_precision_input_t")
        end select
    end function

    pure function check_greater_than_or_equal_with_lesser_number(input) result(result_)
        class(input_t), intent(in) :: input
        type(result_t) :: result_

        type(energy_t) :: energy1
        type(energy_t) :: energy2

        select type (input)
        type is (double_precision_input_t)
            energy1 = input%input().unit.JOULES
            energy2 = (input%input() + 1.0d0).unit.JOULES
            result_ = assert_not( &
                    energy1 >= energy2, &
                    energy1%to_string() // " >= " // energy2%to_string())
        class default
            result_ = fail("Expected to get a double_precision_input_t")
        end select
    end function

    pure function check_less_than_or_equal_with_less_number(input) result(result_)
        class(input_t), intent(in) :: input
        type(result_t) :: result_

        type(energy_t) :: energy1
        type(energy_t) :: energy2

        select type (input)
        type is (double_precision_input_t)
            energy1 = input%input().unit. JOULES
            energy2 = (input%input() + 1.0d0).unit.JOULES
            result_ = assert_that( &
                    energy1 <= energy2, &
                    energy1%to_string() // " <= " // energy2%to_string())
        class default
            result_ = fail("Expected to get a double_precision_input_t")
        end select
    end function

    pure function check_less_than_or_equal_with_same_numbers(input) result(result_)
        class(input_t), intent(in) :: input
        type(result_t) :: result_

        type(energy_t) :: energy1
        type(energy_t) :: energy2

        select type (input)
        type is (double_precision_input_t)
            energy1 = input%input().unit.JOULES
            energy2 = input%input().unit.JOULES
            result_ = assert_that( &
                    energy1 <= energy2, &
                    energy1%to_string() // " <= " // energy2%to_string())
        class default
            result_ = fail("Expected to get a double_precision_input_t")
        end select
    end function

    pure function check_less_than_or_equal_with_greater_number(input) result(result_)
        class(input_t), intent(in) :: input
        type(result_t) :: result_

        type(energy_t) :: energy1
        type(energy_t) :: energy2

        select type (input)
        type is (double_precision_input_t)
            energy1 = input%input().unit.JOULES
            energy2 = (input%input() - 1.0d0).unit.JOULES
            result_ = assert_not( &
                    energy1 <= energy2, &
                    energy1%to_string() // " <= " // energy2%to_string())
        class default
            result_ = fail("Expected to get a double_precision_input_t")
        end select
    end function

    pure function check_greater_than_with_greater_number(input) result(result_)
        class(input_t), intent(in) :: input
        type(result_t) :: result_

        type(energy_t) :: energy1
        type(energy_t) :: energy2

        select type (input)
        type is (double_precision_input_t)
            energy1 = input%input().unit. JOULES
            energy2 = (input%input() - 1.0d0).unit.JOULES
            result_ = assert_that( &
                    energy1 > energy2, &
                    energy1%to_string() // " > " // energy2%to_string())
        class default
            result_ = fail("Expected to get a double_precision_input_t")
        end select
    end function

    pure function check_greater_than_with_same_numbers(input) result(result_)
        class(input_t), intent(in) :: input
        type(result_t) :: result_

        type(energy_t) :: energy1
        type(energy_t) :: energy2

        select type (input)
        type is (double_precision_input_t)
            energy1 = input%input().unit.JOULES
            energy2 = input%input().unit.JOULES
            result_ = assert_not( &
                    energy1 > energy2, &
                    energy1%to_string() // " > " // energy2%to_string())
        class default
            result_ = fail("Expected to get a double_precision_input_t")
        end select
    end function

    pure function check_greater_than_with_lesser_number(input) result(result_)
        class(input_t), intent(in) :: input
        type(result_t) :: result_

        type(energy_t) :: energy1
        type(energy_t) :: energy2

        select type (input)
        type is (double_precision_input_t)
            energy1 = input%input().unit.JOULES
            energy2 = (input%input() + 1.0d0).unit.JOULES
            result_ = assert_not( &
                    energy1 > energy2, &
                    energy1%to_string() // " > " // energy2%to_string())
        class default
            result_ = fail("Expected to get a double_precision_input_t")
        end select
    end function

    pure function check_less_than_with_less_number(input) result(result_)
        class(input_t), intent(in) :: input
        type(result_t) :: result_

        type(energy_t) :: energy1
        type(energy_t) :: energy2

        select type (input)
        type is (double_precision_input_t)
            energy1 = input%input().unit. JOULES
            energy2 = (input%input() + 1.0d0).unit.JOULES
            result_ = assert_that( &
                    energy1 < energy2, &
                    energy1%to_string() // " < " // energy2%to_string())
        class default
            result_ = fail("Expected to get a double_precision_input_t")
        end select
    end function

    pure function check_less_than_with_same_numbers(input) result(result_)
        class(input_t), intent(in) :: input
        type(result_t) :: result_

        type(energy_t) :: energy1
        type(energy_t) :: energy2

        select type (input)
        type is (double_precision_input_t)
            energy1 = input%input().unit.JOULES
            energy2 = input%input().unit.JOULES
            result_ = assert_not( &
                    energy1 < energy2, &
                    energy1%to_string() // " <=" // energy2%to_string())
        class default
            result_ = fail("Expected to get a double_precision_input_t")
        end select
    end function

    pure function check_less_than_with_greater_number(input) result(result_)
        class(input_t), intent(in) :: input
        type(result_t) :: result_

        type(energy_t) :: energy1
        type(energy_t) :: energy2

        select type (input)
        type is (double_precision_input_t)
            energy1 = input%input().unit.JOULES
            energy2 = (input%input() - 1.0d0).unit.JOULES
            result_ = assert_not( &
                    energy1 < energy2, &
                    energy1%to_string() // " < " // energy2%to_string())
        class default
            result_ = fail("Expected to get a double_precision_input_t")
        end select
    end function
end module
