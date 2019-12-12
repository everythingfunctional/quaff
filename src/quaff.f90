module quaff
    use Amount_m, only: &
            Amount_t, &
            AmountUnit_t, &
            operator(.unit.), &
            fromString, &
            DEFAULT_AMOUNT_OUTPUT_UNITS => DEFAULT_OUTPUT_UNITS, &
            PROVIDED_AMOUNT_UNITS => PROVIDED_UNITS, &
            MOLS, &
            PARTICLES
    use Angle_m, only: &
            Angle_t, &
            AngleUnit_t, &
            operator(.unit.), &
            fromString, &
            DEFAULT_ANGLE_OUTPUT_UNITS => DEFAULT_OUTPUT_UNITS, &
            PROVIDED_ANGLE_UNITS => PROVIDED_UNITS, &
            DEGREES, &
            RADIANS
    use Area_m, only: &
            Area_t, &
            AreaUnit_t, &
            operator(.unit.), &
            fromString, &
            DEFAULT_AREA_OUTPUT_UNITS => DEFAULT_OUTPUT_UNITS, &
            PROVIDED_AREA_UNITS => PROVIDED_UNITS, &
            SQUARE_CENTIMETERS, &
            SQUARE_METERS
    use Density_m, only: &
            Density_t, &
            DensityUnit_t, &
            operator(.unit.), &
            fromString, &
            DEFAULT_DENSITY_OUTPUT_UNITS => DEFAULT_OUTPUT_UNITS, &
            PROVIDED_DENSITY_UNITS => PROVIDED_UNITS, &
            GRAMS_PER_CUBIC_METER, &
            KILOGRAMS_PER_CUBIC_METER
    use Interquantity_operators_m, only: operator(*), operator(/)
    use Length_m, only: &
            Length_t, &
            LengthUnit_t, &
            operator(.unit.), &
            fromString, &
            DEFAULT_LENGTH_OUTPUT_UNITS => DEFAULT_OUTPUT_UNITS, &
            PROVIDED_LENGTH_UNITS => PROVIDED_UNITS, &
            CENTIMETERS, &
            METERS
    use Mass_m, only: &
            Mass_t, &
            MassUnit_t, &
            operator(.unit.), &
            fromString, &
            DEFAULT_MASS_OUTPUT_UNITS => DEFAULT_OUTPUT_UNITS, &
            PROVIDED_MASS_UNITS => PROVIDED_UNITS, &
            GRAMS, &
            KILOGRAMS
    use Quantity_module_m, only: &
            QuantityCamel_t, &
            QuantityCamelUnit_t, &
            operator(.unit.), &
            fromString, &
            DEFAULT_QUANTITY_CAPITAL_OUTPUT_UNITS => DEFAULT_OUTPUT_UNITS, &
            PROVIDED_QUANTITY_CAPITAL_UNITS => PROVIDED_UNITS, &
            UNITS_CAPITAL, &
            UNITS_CAPITAL2
    use Speed_m, only: &
            Speed_t, &
            SpeedUnit_t, &
            operator(.unit.), &
            fromString, &
            DEFAULT_SPEED_OUTPUT_UNITS => DEFAULT_OUTPUT_UNITS, &
            PROVIDED_SPEED_UNITS => PROVIDED_UNITS, &
            CENTIMETERS_PER_SECOND, &
            METERS_PER_SECOND
    use Temperature_m, only: &
            Temperature_t, &
            TemperatureUnit_t, &
            operator(.unit.), &
            fromString, &
            DEFAULT_TEMPERATURE_OUTPUT_UNITS => DEFAULT_OUTPUT_UNITS, &
            PROVIDED_TEMPERATURE_UNITS => PROVIDED_UNITS, &
            CELSIUS, &
            FAHRENHEIT, &
            KELVIN, &
            RANKINE
    use Time_m, only: &
            Time_t, &
            TimeUnit_t, &
            operator(.unit.), &
            fromString, &
            DEFAULT_TIME_OUTPUT_UNITS => DEFAULT_OUTPUT_UNITS, &
            PROVIDED_TIME_UNITS => PROVIDED_UNITS, &
            HOURS, &
            SECONDS
    use Volume_m, only: &
            Volume_t, &
            VolumeUnit_t, &
            operator(.unit.), &
            fromString, &
            DEFAULT_VOLUME_OUTPUT_UNITS => DEFAULT_OUTPUT_UNITS, &
            PROVIDED_VOLUME_UNITS => PROVIDED_UNITS, &
            CUBIC_CENTIMETERS, &
            CUBIC_METERS
end module quaff
