module quaff_asserts_m
    use angle_asserts_m, only: &
            assert_equals, &
            assert_equals_within_absolute, &
            assert_equals_within_relative
    use length_asserts_m, only: &
            assert_equals, &
            assert_equals_within_absolute, &
            assert_equals_within_relative
    use temperature_asserts_m, only: &
            assert_equals, &
            assert_equals_within_absolute, &
            assert_equals_within_relative
    use time_asserts_m, only: &
            assert_equals, &
            assert_equals_within_absolute, &
            assert_equals_within_relative
end module
