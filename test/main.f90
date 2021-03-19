! Generated by make_vegetable_driver. DO NOT EDIT
program main
    implicit none

    call run()
contains
    subroutine run()
        use acceleration_test, only: &
                acceleration_acceleration => test_acceleration
        use amount_test, only: &
                amount_amount => test_amount
        use angle_test, only: &
                angle_angle => test_angle
        use area_test, only: &
                area_area => test_area
        use burnup_test, only: &
                burnup_burnup => test_burnup
        use length_test, only: &
                length_length => test_length
        use temperature_test, only: &
                temperature_temperature => test_temperature
        use time_test, only: &
                time_time => test_time
        use vegetables, only: test_item_t, test_that, run_tests

        type(test_item_t) :: tests
        type(test_item_t) :: individual_tests(8)

        individual_tests(1) = acceleration_acceleration()
        individual_tests(2) = amount_amount()
        individual_tests(3) = angle_angle()
        individual_tests(4) = area_area()
        individual_tests(5) = burnup_burnup()
        individual_tests(6) = length_length()
        individual_tests(7) = temperature_temperature()
        individual_tests(8) = time_time()
        tests = test_that(individual_tests)

        call run_tests(tests)
    end subroutine
end program
