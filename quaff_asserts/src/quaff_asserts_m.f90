module quaff_asserts_m
    use length_asserts_m, only: &
            assert_equals, &
            assert_equals_within_absolute, &
            assert_equals_within_relative
    use time_asserts_m, only: &
            assert_equals, &
            assert_equals_within_absolute, &
            assert_equals_within_relative
end module
