! Generated by make_vegetable_driver. DO NOT EDIT
program main
    implicit none

    call run()
contains
    subroutine run()
        use length_logic_ops_test, only: &
                length_logic_ops_equal_operator => test_equal_operator, &
                length_logic_ops_not_equal_operator => test_not_equal_operator, &
                length_logic_ops_equal_within => test_equal_within, &
                length_logic_ops_greater_than_or_equal_operator => test_greater_than_or_equal_operator, &
                length_logic_ops_less_than_or_equal_operator => test_less_than_or_equal_operator, &
                length_logic_ops_greater_than_operator => test_greater_than_operator, &
                length_logic_ops_less_than_operator => test_less_than_operator
        use length_math_ops_test, only: &
                length_math_ops_addition_subtraction_operators => test_addition_subtraction_operators, &
                length_math_ops_multiplication_division_operator => test_multiplication_division_operator
        use length_test, only: &
                length_length => test_length
        use vegetables, only: test_item_t, test_that, run_tests

        type(test_item_t) :: tests
        type(test_item_t) :: individual_tests(10)

        individual_tests(1) = length_logic_ops_equal_operator()
        individual_tests(2) = length_logic_ops_not_equal_operator()
        individual_tests(3) = length_logic_ops_equal_within()
        individual_tests(4) = length_logic_ops_greater_than_or_equal_operator()
        individual_tests(5) = length_logic_ops_less_than_or_equal_operator()
        individual_tests(6) = length_logic_ops_greater_than_operator()
        individual_tests(7) = length_logic_ops_less_than_operator()
        individual_tests(8) = length_math_ops_addition_subtraction_operators()
        individual_tests(9) = length_math_ops_multiplication_division_operator()
        individual_tests(10) = length_length()
        tests = test_that(individual_tests)

        call run_tests(tests)
    end subroutine
end program
