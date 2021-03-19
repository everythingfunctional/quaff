module quaff
    use acceleration_m, only: &
            acceleration_t, &
            fallible_acceleration_t, &
            acceleration_unit_t, &
            fallible_acceleration_unit_t, &
            operator(.unit.), &
            parse_acceleration, &
            parse_acceleration_unit, &
            sum, &
            DEFAULT_ACCELERATION_OUTPUT_UNITS => DEFAULT_OUTPUT_UNITS, &
            PROVIDED_ACCELERATION_UNITS => PROVIDED_UNITS, &
            CENTIMETERS_PER_SQUARE_SECOND, &
            FEET_PER_SQUARE_SECOND, &
            METERS_PER_SQUARE_SECOND
    use angle_m, only: &
            angle_t, &
            fallible_angle_t, &
            angle_unit_t, &
            fallible_angle_unit_t, &
            operator(.unit.), &
            parse_angle, &
            parse_angle_unit, &
            sum, &
            sin, &
            cos, &
            tan, &
            asin_, &
            acos_, &
            atan_, &
            atan2_, &
            DEFAULT_ANGLE_OUTPUT_UNITS => DEFAULT_OUTPUT_UNITS, &
            PROVIDED_ANGLE_UNITS => PROVIDED_UNITS, &
            DEGREES, &
            RADIANS
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
    use temperature_m, only: &
            temperature_t, &
            fallible_temperature_t, &
            temperature_unit_t, &
            fallible_temperature_unit_t, &
            operator(.unit.), &
            parse_temperature, &
            parse_temperature_unit, &
            sum, &
            DEFAULT_TEMPERATURE_OUTPUT_UNITS => DEFAULT_OUTPUT_UNITS, &
            PROVIDED_TEMPERATURE_UNITS => PROVIDED_UNITS, &
            CELSIUS, &
            FAHRENHEIT, &
            KELVIN, &
            RANKINE
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
