! Generated by make_vegetable_driver. DO NOT EDIT
program main
    implicit none

    call run()
contains
    subroutine run()
        use length_test, only: &
                length_length => test_length
        use vegetables, only: test_item_t, test_that, run_tests

        type(test_item_t) :: tests
        type(test_item_t) :: individual_tests(1)

        individual_tests(1) = length_length()
        tests = test_that(individual_tests)

        call run_tests(tests)
    end subroutine
end program
