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
    use amount_m, only: &
            amount_t, &
            fallible_amount_t, &
            amount_unit_t, &
            fallible_amount_unit_t, &
            operator(.unit.), &
            parse_amount, &
            parse_amount_unit, &
            sum, &
            DEFAULT_AMOUNT_OUTPUT_UNITS => DEFAULT_OUTPUT_UNITS, &
            PROVIDED_AMOUNT_UNITS => PROVIDED_UNITS, &
            MOLS, &
            PARTICLES
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
    use area_m, only: &
            area_t, &
            fallible_area_t, &
            area_unit_t, &
            fallible_area_unit_t, &
            operator(.unit.), &
            parse_area, &
            parse_area_unit, &
            sum, &
            DEFAULT_AREA_OUTPUT_UNITS => DEFAULT_OUTPUT_UNITS, &
            PROVIDED_AREA_UNITS => PROVIDED_UNITS, &
            SQUARE_CENTIMETERS, &
            SQUARE_FEET, &
            SQUARE_INCHES, &
            SQUARE_METERS
    use burnup_m, only: &
            burnup_t, &
            fallible_burnup_t, &
            burnup_unit_t, &
            fallible_burnup_unit_t, &
            operator(.unit.), &
            parse_burnup, &
            parse_burnup_unit, &
            sum, &
            DEFAULT_BURNUP_OUTPUT_UNITS => DEFAULT_OUTPUT_UNITS, &
            PROVIDED_BURNUP_UNITS => PROVIDED_UNITS, &
            MEGAWATT_DAYS_PER_TON, &
            WATT_SECONDS_PER_KILOGRAM
    use density_m, only: &
            density_t, &
            fallible_density_t, &
            density_unit_t, &
            fallible_density_unit_t, &
            operator(.unit.), &
            parse_density, &
            parse_density_unit, &
            sum, &
            DEFAULT_DENSITY_OUTPUT_UNITS => DEFAULT_OUTPUT_UNITS, &
            PROVIDED_DENSITY_UNITS => PROVIDED_UNITS, &
            GRAMS_PER_CUBIC_METER, &
            KILOGRAMS_PER_CUBIC_METER
    use dynamic_viscosity_m, only: &
            dynamic_viscosity_t, &
            fallible_dynamic_viscosity_t, &
            dynamic_viscosity_unit_t, &
            fallible_dynamic_viscosity_unit_t, &
            operator(.unit.), &
            parse_dynamic_viscosity, &
            parse_dynamic_viscosity_unit, &
            sum, &
            DEFAULT_DYNAMIC_VISCOSITY_OUTPUT_UNITS => DEFAULT_OUTPUT_UNITS, &
            PROVIDED_DYNAMIC_VISCOSITY_UNITS => PROVIDED_UNITS, &
            MEGAPASCAL_SECONDS, &
            PASCAL_SECONDS
    use energy_m, only: &
            energy_t, &
            fallible_energy_t, &
            energy_unit_t, &
            fallible_energy_unit_t, &
            operator(.unit.), &
            parse_energy, &
            parse_energy_unit, &
            sum, &
            DEFAULT_ENERGY_OUTPUT_UNITS => DEFAULT_OUTPUT_UNITS, &
            PROVIDED_ENERGY_UNITS => PROVIDED_UNITS, &
            BTU, &
            CALORIES, &
            JOULES, &
            KILOJOULES, &
            MEGABTU, &
            MEGAWATT_DAYS
    use energy_per_amount_m, only: &
            energy_per_amount_t, &
            fallible_energy_per_amount_t, &
            energy_per_amount_unit_t, &
            fallible_energy_per_amount_unit_t, &
            operator(.unit.), &
            parse_energy_per_amount, &
            parse_energy_per_amount_unit, &
            sum, &
            DEFAULT_ENERGY_PER_AMOUNT_OUTPUT_UNITS => DEFAULT_OUTPUT_UNITS, &
            PROVIDED_ENERGY_PER_AMOUNT_UNITS => PROVIDED_UNITS, &
            JOULES_PER_MOL, &
            KILOJOULES_PER_MOL
    use energy_per_temperature_amount_m, only: &
            energy_per_temperature_amount_t, &
            fallible_energy_per_temperature_amount_t, &
            energy_per_temperature_amount_unit_t, &
            fallible_energy_per_temperature_amount_unit_t, &
            operator(.unit.), &
            parse_energy_per_temperature_amount, &
            parse_energy_per_temperature_amount_unit, &
            sum, &
            DEFAULT_ENERGY_PER_AMOUNT_TEMPERATURE_OUTPUT_UNITS => DEFAULT_OUTPUT_UNITS, &
            PROVIDED_ENERGY_PER_TEMPERATURE_AMOUNT_UNITS => PROVIDED_UNITS, &
            JOULES_PER_KELVIN_MOL, &
            KILOJOULES_PER_KELVIN_MOL
    use enthalpy_m, only: &
            enthalpy_t, &
            fallible_enthalpy_t, &
            enthalpy_unit_t, &
            fallible_enthalpy_unit_t, &
            operator(.unit.), &
            parse_enthalpy, &
            parse_enthalpy_unit, &
            sum, &
            DEFAULT_ENTHALPY_OUTPUT_UNITS => DEFAULT_OUTPUT_UNITS, &
            PROVIDED_ENTHALPY_UNITS => PROVIDED_UNITS, &
            JOULES_PER_KILOGRAM, &
            KILOJOULES_PER_KILOGRAM
    use force_m, only: &
            force_t, &
            fallible_force_t, &
            force_unit_t, &
            fallible_force_unit_t, &
            operator(.unit.), &
            parse_force, &
            parse_force_unit, &
            sum, &
            DEFAULT_FORCE_OUTPUT_UNITS => DEFAULT_OUTPUT_UNITS, &
            PROVIDED_FORCE_UNITS => PROVIDED_UNITS, &
            DYNES, &
            KILOPONDS, &
            MILLINEWTONS, &
            NEWTONS, &
            POUNDS_FORCE
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
    use mass_m, only: &
            mass_t, &
            fallible_mass_t, &
            mass_unit_t, &
            fallible_mass_unit_t, &
            operator(.unit.), &
            parse_mass, &
            parse_mass_unit, &
            sum, &
            DEFAULT_MASS_OUTPUT_UNITS => DEFAULT_OUTPUT_UNITS, &
            PROVIDED_MASS_UNITS => PROVIDED_UNITS, &
            GRAMS, &
            KILOGRAMS, &
            POUNDS_MASS, &
            TONS
    use molar_mass_m, only: &
            molar_mass_t, &
            fallible_molar_mass_t, &
            molar_mass_unit_t, &
            fallible_molar_mass_unit_t, &
            operator(.unit.), &
            parse_molar_mass, &
            parse_molar_mass_unit, &
            sum, &
            DEFAULT_MOLAR_MASS_OUTPUT_UNITS => DEFAULT_OUTPUT_UNITS, &
            PROVIDED_MOLAR_MASS_UNITS => PROVIDED_UNITS, &
            GRAMS_PER_MOL, &
            KILOGRAMS_PER_MOL
    use power_m, only: &
            power_t, &
            fallible_power_t, &
            power_unit_t, &
            fallible_power_unit_t, &
            operator(.unit.), &
            parse_power, &
            parse_power_unit, &
            sum, &
            DEFAULT_POWER_OUTPUT_UNITS => DEFAULT_OUTPUT_UNITS, &
            PROVIDED_POWER_UNITS => PROVIDED_UNITS, &
            BTU_PER_HOUR, &
            CALORIES_PER_SECOND, &
            MEGABTU_PER_HOUR, &
            MEGAWATTS, &
            WATTS
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
