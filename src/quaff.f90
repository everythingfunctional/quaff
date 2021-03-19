module quaff
    use length_m, only: &
            length_t, &
            fallible_length_t, &
            length_unit_t, &
            fallible_length_unit_t, &
            operator(.unit.), &
            parse_length, &
            parse_length_unit, &
            sum, &
            DEFAULT_LENGTH_OUTPUT_UNITS => DEFAULT_OUTPUT_UNITS, &
            PROVIDED_LENGTH_UNITS => PROVIDED_UNITS, &
            CENTIMETERS, &
            FEET, &
            INCHES, &
            METERS, &
            MICROINCHES, &
            MICROMETERS
    use time_m, only: &
            time_t, &
            fallible_time_t, &
            time_unit_t, &
            fallible_time_unit_t, &
            operator(.unit.), &
            parse_time, &
            parse_time_unit, &
            sum, &
            DEFAULT_TIME_OUTPUT_UNITS => DEFAULT_OUTPUT_UNITS, &
            PROVIDED_TIME_UNITS => PROVIDED_UNITS, &
            DAYS, &
            HOURS, &
            MINUTES, &
            SECONDS
end module
