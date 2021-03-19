module angle_test
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
            angle_t, &
            fallible_angle_t, &
            angle_unit_t, &
            operator(.unit.), &
            parse_angle, &
            sum, &
            PROVIDED_ANGLE_UNITS, &
            RADIANS
    use quaff_asserts_m, only: assert_equals, assert_equals_within_relative
    use quaff_utilities_m, only: PARSE_ERROR
    use angle_utilities_m, only: &
            units_input_t, units_pair_input_t, make_units_examples
    use units_examples_m, only: units_examples_t
    use vegetables, only: &
            double_precision_input_t, &
            example_t, &
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
    public :: test_angle
contains
    function test_angle() result(tests)
        type(test_item_t) :: tests

        type(units_examples_t) :: examples

        examples = make_units_examples(PROVIDED_ANGLE_UNITS)
        tests = describe( &
                "angle_t", &
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
                        "adding zero returns the original angle", &
                        DOUBLE_PRECISION_GENERATOR, &
                        check_add_zero) &
                , it( &
                        "subtracting zero returns the original angle", &
                        DOUBLE_PRECISION_GENERATOR, &
                        check_subtract_zero) &
                , it( &
                        "adding and subtracting the same angle returns the original angle", &
                        DOUBLE_PRECISION_PAIR_GENERATOR, &
                        check_add_subtract) &
                , it( &
                        "multiplying by one returns the original angle", &
                        DOUBLE_PRECISION_GENERATOR, &
                        check_multiply_by_one) &
                , it( &
                        "multiplying by zero returns zero angle", &
                        DOUBLE_PRECISION_GENERATOR, &
                        check_multiply_by_zero) &
                , it( &
                        "dividing by one returns the original angle", &
                        DOUBLE_PRECISION_GENERATOR, &
                        check_divide_by_one) &
                , it( &
                        "dividing by itself returns one", &
                        NON_ZERO_DOUBLE_PRECISION_GENERATOR, &
                        check_divide_by_self) &
                , it( &
                        "multiplying and dividing by the same number returns the original angle", &
                        NON_ZERO_DOUBLE_PRECISION_PAIR_GENERATOR, &
                        check_multiply_divide) &
                , describe( &
                        "operator(==)", &
                        [ it( &
                                "is true for the same angle", &
                                DOUBLE_PRECISION_GENERATOR, &
                                check_equal_with_same_number) &
                        , it( &
                                "is false for different angles", &
                                DOUBLE_PRECISION_GENERATOR, &
                                check_equal_with_different_numbers) &
                        ]) &
                , describe( &
                        "operator(/=)", &
                        [ it( &
                                "is false for the same angle", &
                                DOUBLE_PRECISION_GENERATOR, &
                                check_not_equal_with_same_number) &
                        , it( &
                                "is true for different angles", &
                                DOUBLE_PRECISION_GENERATOR, &
                                check_not_equal_with_different_numbers) &
                        ]) &
                , describe( &
                        "%equal(angle, within)", &
                        [ it( &
                                "is true for the same angle even for tiny tolerance", &
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
        class(angle_unit_t), intent(in) :: units
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

            type(angle_t) :: intermediate

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
        class(angle_unit_t), intent(in) :: to
        class(angle_unit_t), intent(in) :: from
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
        class(angle_unit_t), intent(in) :: units
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
            type(angle_t) :: original_angle
            type(fallible_angle_t) :: maybe_angle
            type(angle_t) :: new_angle

            select type (input)
            type is (double_precision_input_t)
                original_angle = input%input().unit.units
                maybe_angle = parse_angle( &
                        original_angle%to_string_in(units))
                new_angle = maybe_angle%angle()
                errors = maybe_angle%errors()
                result__ = &
                        assert_equals( &
                                original_angle, &
                                new_angle) &
                        .and.assert_not(errors%has_any(), errors%to_string())
            class default
                result__ = fail("Expected to get a double_precision_input_t")
            end select
        end function
    end function

    function check_bad_string() result(result_)
        type(result_t) :: result_

        type(error_list_t) :: errors
        type(fallible_angle_t) :: maybe_angle

        maybe_angle = parse_angle("bad")
        errors = maybe_angle%errors()
        result_ = assert_that(errors.hasType.PARSE_ERROR, errors%to_string())
    end function

    function check_bad_unit() result(result_)
        type(result_t) :: result_

        type(error_list_t) :: errors
        type(fallible_angle_t) :: maybe_angle

        maybe_angle = parse_angle("1.0 bad", [RADIANS])
        errors = maybe_angle%errors()
        result_ = assert_that(errors.hasType.PARSE_ERROR, errors%to_string())
    end function

    function check_bad_number() result(result_)
        type(result_t) :: result_

        type(error_list_t) :: errors
        type(fallible_angle_t) :: maybe_angle

        maybe_angle = parse_angle("bad rad")
        errors = maybe_angle%errors()
        result_ = assert_that(errors.hasType.PARSE_ERROR, errors%to_string())
    end function

    pure function check_sum() result(result_)
        type(result_t) :: result_

        double precision, parameter :: numbers(*) = [1.0d0, 2.0d0, 3.0d0]

        result_ = assert_equals( &
                sum(numbers).unit.RADIANS, &
                sum(numbers.unit.RADIANS))
    end function

    pure function check_add_zero(input) result(result_)
        class(input_t), intent(in) :: input
        type(result_t) :: result_

        type(angle_t) :: angle
        type(angle_t) :: zero

        select type(input)
        type is (double_precision_input_t)
            angle = input%input().unit.RADIANS
            zero = 0.0d0.unit.RADIANS
            result_ = assert_equals(angle, angle + zero)
        class default
            result_ = fail("Expected a double_precision_input_t")
        end select
    end function

    pure function check_subtract_zero(input) result(result_)
        class(input_t), intent(in) :: input
        type(result_t) :: result_

        type(angle_t) :: angle
        type(angle_t) :: zero

        select type(input)
        type is (double_precision_input_t)
            angle = input%input().unit.RADIANS
            zero = 0.0d0.unit.RADIANS
            result_ = assert_equals(angle, angle - zero)
        class default
            result_ = fail("Expected a double_precision_input_t")
        end select
    end function

    pure function check_add_subtract(input) result(result_)
        class(input_t), intent(in) :: input
        type(result_t) :: result_

        type(angle_t) :: angle1
        type(angle_t) :: angle2

        select type(input)
        type is (double_precision_pair_input_t)
            angle1 = input%first().unit.RADIANS
            angle2 = input%second_().unit.RADIANS
            result_ = assert_equals_within_relative( &
                    angle1, &
                    (angle1 + angle2) - angle2, &
                    1.0d-8, &
                    "angle1 = " // angle1%to_string() &
                    // ", angle2 = " // angle2%to_string())
        class default
            result_ = fail("Expected a double_precision_pair_input_t")
        end select
    end function

    pure function check_multiply_by_one(input) result(result_)
        class(input_t), intent(in) :: input
        type(result_t) :: result_

        type(angle_t) :: angle

        select type(input)
        type is (double_precision_input_t)
            angle = input%input().unit.RADIANS
            result_ = assert_equals(angle, angle * 1.0d0)
        class default
            result_ = fail("Expected a double_precision_input_t")
        end select
    end function

    pure function check_multiply_by_zero(input) result(result_)
        class(input_t), intent(in) :: input
        type(result_t) :: result_

        type(angle_t) :: angle
        type(angle_t) :: zero

        select type(input)
        type is (double_precision_input_t)
            angle = input%input().unit.RADIANS
            zero = 0.0d0.unit.RADIANS
            result_ = assert_equals(zero, angle * 0.0d0)
        class default
            result_ = fail("Expected a double_precision_input_t")
        end select
    end function

    pure function check_divide_by_one(input) result(result_)
        class(input_t), intent(in) :: input
        type(result_t) :: result_

        type(angle_t) :: angle

        select type(input)
        type is (double_precision_input_t)
            angle = input%input().unit.RADIANS
            result_ = assert_equals(angle, angle / 1.0d0)
        class default
            result_ = fail("Expected a double_precision_input_t")
        end select
    end function

    pure function check_divide_by_self(input) result(result_)
        class(input_t), intent(in) :: input
        type(result_t) :: result_

        type(angle_t) :: angle

        select type(input)
        type is (double_precision_input_t)
            angle = input%input().unit.RADIANS
            result_ = assert_equals(1.0d0, angle / angle)
        class default
            result_ = fail("Expected a double_precision_input_t")
        end select
    end function

    pure function check_multiply_divide(input) result(result_)
        class(input_t), intent(in) :: input
        type(result_t) :: result_

        type(angle_t) :: angle

        select type (input)
        type is (double_precision_pair_input_t)
            angle = input%first().unit.RADIANS
            result_ = assert_equals(angle, (angle * input%second_()) / input%second_())
        class default
            result_ = fail("Expected a double_precision_pair_input_t")
        end select
    end function

    pure function check_equal_with_same_number(input) result(result_)
        class(input_t), intent(in) :: input
        type(result_t) :: result_

        type(angle_t) :: the_angle

        select type (input)
        type is (double_precision_input_t)
            the_angle = input%input().unit.RADIANS
            result_ = assert_that( &
                    the_angle == the_angle, &
                    the_angle%to_string() // " == " // the_angle%to_string())
        class default
            result_ = fail("Expected to get a double_precision_input_t")
        end select
    end function

    pure function check_equal_with_different_numbers(input) result(result_)
        class(input_t), intent(in) :: input
        type(result_t) :: result_

        type(angle_t) :: angle1
        type(angle_t) :: angle2

        select type (input)
        type is (double_precision_input_t)
            angle1 = input%input().unit.RADIANS
            angle2 = (input%input() + 1.0d0).unit.RADIANS
            result_ = assert_not( &
                    angle1 == angle2, &
                    angle1%to_string() // " == " // angle2%to_string())
        class default
            result_ = fail("Expected to get a double_precision_input_t")
        end select
    end function

    pure function check_not_equal_with_same_number(input) result(result_)
        class(input_t), intent(in) :: input
        type(result_t) :: result_

        type(angle_t) :: the_angle

        select type (input)
        type is (double_precision_input_t)
            the_angle = input%input().unit.RADIANS
            result_ = assert_not( &
                    the_angle /= the_angle, &
                    the_angle%to_string() // " /= " // the_angle%to_string())
        class default
            result_ = fail("Expected to get a double_precision_input_t")
        end select
    end function

    pure function check_not_equal_with_different_numbers(input) result(result_)
        class(input_t), intent(in) :: input
        type(result_t) :: result_

        type(angle_t) :: angle1
        type(angle_t) :: angle2

        select type (input)
        type is (double_precision_input_t)
            angle1 = input%input().unit.RADIANS
            angle2 = (input%input() + 1.0d0).unit.RADIANS
            result_ = assert_that( &
                    angle1 /= angle2, &
                    angle1%to_string() // " /= " // angle2%to_string())
        class default
            result_ = fail("Expected to get a double_precision_input_t")
        end select
    end function

    pure function check_equal_within_with_same_number(input) result(result_)
        class(input_t), intent(in) :: input
        type(result_t) :: result_

        type(angle_t) :: the_angle
        type(angle_t) :: tolerance

        select type (input)
        type is (double_precision_input_t)
            the_angle = input%input().unit.RADIANS
            tolerance = tiny(1.0d0).unit.RADIANS
            result_ = assert_that( &
                    the_angle%equal(the_angle, within = tolerance), &
                    "(" // the_angle%to_string() // ")%equal(" &
                        // the_angle%to_string() // ", within = " &
                        // tolerance%to_string() // ")")
        class default
            result_ = fail("Expected to get a double_precision_input_t")
        end select
    end function

    pure function check_equal_within_with_close_numbers(input) result(result_)
        class(input_t), intent(in) :: input
        type(result_t) :: result_

        type(angle_t) :: angle1
        type(angle_t) :: angle2
        type(angle_t) :: tolerance

        select type (input)
        type is (double_precision_input_t)
            angle1 = input%input().unit.RADIANS
            angle2 = (input%input() + 0.05d0).unit.RADIANS
            tolerance = 0.1d0.unit.RADIANS
            result_ = assert_that( &
                    angle1%equal(angle2, within = tolerance), &
                    "(" // angle1%to_string() // ")%equal(" &
                        // angle2%to_string() // ", within = " &
                        // tolerance%to_string() // ")")
        class default
            result_ = fail("Expected to get a double_precision_input_t")
        end select
    end function

    pure function check_equal_within_with_different_numbers(input) result(result_)
        class(input_t), intent(in) :: input
        type(result_t) :: result_

        type(angle_t) :: angle1
        type(angle_t) :: angle2
        type(angle_t) :: tolerance

        select type (input)
        type is (double_precision_input_t)
            angle1 = input%input().unit.RADIANS
            angle2 = (input%input() + 0.2d0).unit.RADIANS
            tolerance = 0.1d0.unit.RADIANS
            result_ = assert_not( &
                    angle1%equal(angle2, within = tolerance), &
                    "(" // angle1%to_string() // ")%equal(" &
                    // angle2%to_string() // ", within = " &
                    // tolerance%to_string() // ")")
        class default
            result_ = fail("Expected to get a double_precision_input_t")
        end select
    end function

    pure function check_greater_than_or_equal_with_greater_number(input) result(result_)
        class(input_t), intent(in) :: input
        type(result_t) :: result_

        type(angle_t) :: angle1
        type(angle_t) :: angle2

        select type (input)
        type is (double_precision_input_t)
            angle1 = input%input().unit. RADIANS
            angle2 = (input%input() - 1.0d0).unit.RADIANS
            result_ = assert_that( &
                    angle1 >= angle2, &
                    angle1%to_string() // " >= " // angle2%to_string())
        class default
            result_ = fail("Expected to get a double_precision_input_t")
        end select
    end function

    pure function check_greater_than_or_equal_with_same_numbers(input) result(result_)
        class(input_t), intent(in) :: input
        type(result_t) :: result_

        type(angle_t) :: angle1
        type(angle_t) :: angle2

        select type (input)
        type is (double_precision_input_t)
            angle1 = input%input().unit.RADIANS
            angle2 = input%input().unit.RADIANS
            result_ = assert_that( &
                    angle1 >= angle2, &
                    angle1%to_string() // " >= " // angle2%to_string())
        class default
            result_ = fail("Expected to get a double_precision_input_t")
        end select
    end function

    pure function check_greater_than_or_equal_with_lesser_number(input) result(result_)
        class(input_t), intent(in) :: input
        type(result_t) :: result_

        type(angle_t) :: angle1
        type(angle_t) :: angle2

        select type (input)
        type is (double_precision_input_t)
            angle1 = input%input().unit.RADIANS
            angle2 = (input%input() + 1.0d0).unit.RADIANS
            result_ = assert_not( &
                    angle1 >= angle2, &
                    angle1%to_string() // " >= " // angle2%to_string())
        class default
            result_ = fail("Expected to get a double_precision_input_t")
        end select
    end function

    pure function check_less_than_or_equal_with_less_number(input) result(result_)
        class(input_t), intent(in) :: input
        type(result_t) :: result_

        type(angle_t) :: angle1
        type(angle_t) :: angle2

        select type (input)
        type is (double_precision_input_t)
            angle1 = input%input().unit. RADIANS
            angle2 = (input%input() + 1.0d0).unit.RADIANS
            result_ = assert_that( &
                    angle1 <= angle2, &
                    angle1%to_string() // " <= " // angle2%to_string())
        class default
            result_ = fail("Expected to get a double_precision_input_t")
        end select
    end function

    pure function check_less_than_or_equal_with_same_numbers(input) result(result_)
        class(input_t), intent(in) :: input
        type(result_t) :: result_

        type(angle_t) :: angle1
        type(angle_t) :: angle2

        select type (input)
        type is (double_precision_input_t)
            angle1 = input%input().unit.RADIANS
            angle2 = input%input().unit.RADIANS
            result_ = assert_that( &
                    angle1 <= angle2, &
                    angle1%to_string() // " <= " // angle2%to_string())
        class default
            result_ = fail("Expected to get a double_precision_input_t")
        end select
    end function

    pure function check_less_than_or_equal_with_greater_number(input) result(result_)
        class(input_t), intent(in) :: input
        type(result_t) :: result_

        type(angle_t) :: angle1
        type(angle_t) :: angle2

        select type (input)
        type is (double_precision_input_t)
            angle1 = input%input().unit.RADIANS
            angle2 = (input%input() - 1.0d0).unit.RADIANS
            result_ = assert_not( &
                    angle1 <= angle2, &
                    angle1%to_string() // " <= " // angle2%to_string())
        class default
            result_ = fail("Expected to get a double_precision_input_t")
        end select
    end function

    pure function check_greater_than_with_greater_number(input) result(result_)
        class(input_t), intent(in) :: input
        type(result_t) :: result_

        type(angle_t) :: angle1
        type(angle_t) :: angle2

        select type (input)
        type is (double_precision_input_t)
            angle1 = input%input().unit. RADIANS
            angle2 = (input%input() - 1.0d0).unit.RADIANS
            result_ = assert_that( &
                    angle1 > angle2, &
                    angle1%to_string() // " > " // angle2%to_string())
        class default
            result_ = fail("Expected to get a double_precision_input_t")
        end select
    end function

    pure function check_greater_than_with_same_numbers(input) result(result_)
        class(input_t), intent(in) :: input
        type(result_t) :: result_

        type(angle_t) :: angle1
        type(angle_t) :: angle2

        select type (input)
        type is (double_precision_input_t)
            angle1 = input%input().unit.RADIANS
            angle2 = input%input().unit.RADIANS
            result_ = assert_not( &
                    angle1 > angle2, &
                    angle1%to_string() // " > " // angle2%to_string())
        class default
            result_ = fail("Expected to get a double_precision_input_t")
        end select
    end function

    pure function check_greater_than_with_lesser_number(input) result(result_)
        class(input_t), intent(in) :: input
        type(result_t) :: result_

        type(angle_t) :: angle1
        type(angle_t) :: angle2

        select type (input)
        type is (double_precision_input_t)
            angle1 = input%input().unit.RADIANS
            angle2 = (input%input() + 1.0d0).unit.RADIANS
            result_ = assert_not( &
                    angle1 > angle2, &
                    angle1%to_string() // " > " // angle2%to_string())
        class default
            result_ = fail("Expected to get a double_precision_input_t")
        end select
    end function

    pure function check_less_than_with_less_number(input) result(result_)
        class(input_t), intent(in) :: input
        type(result_t) :: result_

        type(angle_t) :: angle1
        type(angle_t) :: angle2

        select type (input)
        type is (double_precision_input_t)
            angle1 = input%input().unit. RADIANS
            angle2 = (input%input() + 1.0d0).unit.RADIANS
            result_ = assert_that( &
                    angle1 < angle2, &
                    angle1%to_string() // " < " // angle2%to_string())
        class default
            result_ = fail("Expected to get a double_precision_input_t")
        end select
    end function

    pure function check_less_than_with_same_numbers(input) result(result_)
        class(input_t), intent(in) :: input
        type(result_t) :: result_

        type(angle_t) :: angle1
        type(angle_t) :: angle2

        select type (input)
        type is (double_precision_input_t)
            angle1 = input%input().unit.RADIANS
            angle2 = input%input().unit.RADIANS
            result_ = assert_not( &
                    angle1 < angle2, &
                    angle1%to_string() // " <=" // angle2%to_string())
        class default
            result_ = fail("Expected to get a double_precision_input_t")
        end select
    end function

    pure function check_less_than_with_greater_number(input) result(result_)
        class(input_t), intent(in) :: input
        type(result_t) :: result_

        type(angle_t) :: angle1
        type(angle_t) :: angle2

        select type (input)
        type is (double_precision_input_t)
            angle1 = input%input().unit.RADIANS
            angle2 = (input%input() - 1.0d0).unit.RADIANS
            result_ = assert_not( &
                    angle1 < angle2, &
                    angle1%to_string() // " < " // angle2%to_string())
        class default
            result_ = fail("Expected to get a double_precision_input_t")
        end select
    end function
end module
