program test
    implicit none

    call run()
contains
    subroutine run()
        use length_logic_ops_test, only: &
            length_logic_ops_less_than_operator => test_less_than_operator, &
            length_logic_ops_greater_than_operator => test_greater_than_operator, &
            length_logic_ops_less_than_or_equal_operator => test_less_than_or_equal_operator, &
            length_logic_ops_greater_than_or_equal_operator => test_greater_than_or_equal_operator, &
            length_logic_ops_equal_within => test_equal_within, &
            length_logic_ops_not_equal_operator => test_not_equal_operator, &
            length_logic_ops_equal_operator => test_equal_operator
        use length_math_ops_test, only: &
            length_math_ops_multiplication_division_operator => test_multiplication_division_operator, &
            length_math_ops_addition_subtraction_operators => test_addition_subtraction_operators
        use length_type_test, only: &
            length_type_length => test_length
        use iso_varying_string
        use Vegetables_m, only: TestItem_t, testThat, runTests

        type(TestItem_t) :: tests
        type(TestItem_t) :: individual_tests(10)

        individual_tests(1) = length_logic_ops_less_than_operator()
        individual_tests(2) = length_logic_ops_greater_than_operator()
        individual_tests(3) = length_logic_ops_less_than_or_equal_operator()
        individual_tests(4) = length_logic_ops_greater_than_or_equal_operator()
        individual_tests(5) = length_logic_ops_equal_within()
        individual_tests(6) = length_logic_ops_not_equal_operator()
        individual_tests(7) = length_logic_ops_equal_operator()
        individual_tests(8) = length_math_ops_multiplication_division_operator()
        individual_tests(9) = length_math_ops_addition_subtraction_operators()
        individual_tests(10) = length_type_length()
        tests = testThat(individual_tests)

        call runTests(tests)
    end subroutine run
end program test
