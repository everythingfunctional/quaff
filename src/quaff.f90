module quaff
    use quaff_acceleration_m, only: &
            acceleration_t, &
            fallible_acceleration_t, &
            acceleration_unit_t, &
            fallible_acceleration_unit_t, &
            operator(.unit.), &
            abs, &
            parse_acceleration, &
            parse_acceleration_unit, &
            sum, &
            DEFAULT_ACCELERATION_OUTPUT_UNITS => DEFAULT_OUTPUT_UNITS, &
            PROVIDED_ACCELERATION_UNITS => PROVIDED_UNITS, &
            CENTIMETERS_PER_SQUARE_SECOND, &
            FEET_PER_SQUARE_SECOND, &
            METERS_PER_SQUARE_SECOND
    use quaff_amount_m, only: &
            amount_t, &
            fallible_amount_t, &
            amount_unit_t, &
            fallible_amount_unit_t, &
            operator(.unit.), &
            abs, &
            parse_amount, &
            parse_amount_unit, &
            sum, &
            DEFAULT_AMOUNT_OUTPUT_UNITS => DEFAULT_OUTPUT_UNITS, &
            PROVIDED_AMOUNT_UNITS => PROVIDED_UNITS, &
            KILOMOLS, &
            MOLS, &
            PARTICLES
    use quaff_amount_rate_m, only: &
            amount_rate_t, &
            fallible_amount_rate_t, &
            amount_rate_unit_t, &
            fallible_amount_rate_unit_t, &
            operator(.unit.), &
            parse_amount_rate, &
            parse_amount_rate_unit, &
            sum, &
            abs, &
            DEFAULT_AMOUNT_RATE_OUTPUT_UNITS => DEFAULT_OUTPUT_UNITS, &
            PROVIDED_AMOUNT_RATE_UNITS => PROVIDED_UNITS, &
            KILOMOLS_PER_SECOND, &
            MOLS_PER_SECOND
    use quaff_amount_temperature_m, only: &
            amount_temperature_t, &
            fallible_amount_temperature_t, &
            amount_temperature_unit_t, &
            fallible_amount_temperature_unit_t, &
            operator(.unit.), &
            parse_amount_temperature, &
            parse_amount_temperature_unit, &
            sum, &
            abs, &
            DEFAULT_AMOUNT_TEMPERATURE_OUTPUT_UNITS => DEFAULT_OUTPUT_UNITS, &
            PROVIDED_AMOUNT_TEMPERATURE_UNITS => PROVIDED_UNITS, &
            KILOMOLS_KELVIN, &
            MOLS_KELVIN
    use quaff_amount_temperature_rate_m, only: &
            amount_temperature_rate_t, &
            fallible_amount_temperature_rate_t, &
            amount_temperature_rate_unit_t, &
            fallible_amount_temperature_rate_unit_t, &
            operator(.unit.), &
            parse_amount_temperature_rate, &
            parse_amount_temperature_rate_unit, &
            sum, &
            abs, &
            DEFAULT_AMOUNT_TEMPERATURE_RATE_OUTPUT_UNITS => DEFAULT_OUTPUT_UNITS, &
            PROVIDED_AMOUNT_TEMPERATURE_RATE_UNITS => PROVIDED_UNITS, &
            KILOMOLS_KELVIN_PER_SECOND, &
            MOLS_KELVIN_PER_SECOND
    use quaff_angle_m, only: &
            angle_t, &
            fallible_angle_t, &
            angle_unit_t, &
            fallible_angle_unit_t, &
            operator(.unit.), &
            parse_angle, &
            parse_angle_unit, &
            sum, &
            abs, &
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
    use quaff_area_m, only: &
            area_t, &
            fallible_area_t, &
            area_unit_t, &
            fallible_area_unit_t, &
            operator(.unit.), &
            parse_area, &
            parse_area_unit, &
            sum, &
            abs, &
            DEFAULT_AREA_OUTPUT_UNITS => DEFAULT_OUTPUT_UNITS, &
            PROVIDED_AREA_UNITS => PROVIDED_UNITS, &
            SQUARE_CENTIMETERS, &
            SQUARE_FEET, &
            SQUARE_INCHES, &
            SQUARE_METERS
    use quaff_burnup_m, only: &
            burnup_t, &
            fallible_burnup_t, &
            burnup_unit_t, &
            fallible_burnup_unit_t, &
            operator(.unit.), &
            parse_burnup, &
            parse_burnup_unit, &
            sum, &
            abs, &
            DEFAULT_BURNUP_OUTPUT_UNITS => DEFAULT_OUTPUT_UNITS, &
            PROVIDED_BURNUP_UNITS => PROVIDED_UNITS, &
            MEGAWATT_DAYS_PER_TON, &
            WATT_SECONDS_PER_KILOGRAM
    use quaff_convective_heat_transfer_m, only: &
            convective_heat_transfer_t, &
            fallible_convective_heat_transfer_t, &
            convective_heat_transfer_unit_t, &
            fallible_convective_heat_transfer_unit_t, &
            operator(.unit.), &
            parse_convective_heat_transfer, &
            parse_convective_heat_transfer_unit, &
            sum, &
            abs, &
            DEFAULT_CONVECTIVE_HEAT_TRANSFER_UNITS => DEFAULT_OUTPUT_UNITS, &
            PROVIDED_CONVECTIVE_HEAT_TRANSFER_UNITS => PROVIDED_UNITS, &
            BTU_PER_HOUR_SQUARE_FEET_RANKINE, &
            BTU_PER_HOUR_SQUARE_FEET_FAHRENHEIT, &
            WATTS_PER_SQUARE_METER_KELVIN
    use quaff_delta_temperature_m, only: &
            delta_temperature_t, &
            fallible_delta_temperature_t, &
            delta_temperature_unit_t, &
            fallible_delta_temperature_unit_t, &
            operator(.unit.), &
            parse_delta_temperature, &
            parse_delta_temperature_unit, &
            sum, &
            abs, &
            DEFAULT_DELTA_TEMPERATURE_OUTPUT_UNITS => DEFAULT_OUTPUT_UNITS, &
            PROVIDED_DELTA_TEMPERATURE_UNITS =>PROVIDED_UNITS, &
            DELTA_CELSIUS, &
            DELTA_FAHRENHEIT, &
            DELTA_KELVIN, &
            DELTA_RANKINE
    use quaff_density_m, only: &
            density_t, &
            fallible_density_t, &
            density_unit_t, &
            fallible_density_unit_t, &
            operator(.unit.), &
            parse_density, &
            parse_density_unit, &
            sum, &
            abs, &
            DEFAULT_DENSITY_OUTPUT_UNITS => DEFAULT_OUTPUT_UNITS, &
            PROVIDED_DENSITY_UNITS => PROVIDED_UNITS, &
            GRAMS_PER_CUBIC_METER, &
            KILOGRAMS_PER_CUBIC_METER, &
            POUNDS_PER_CUBIC_FOOT
    use quaff_dynamic_viscosity_m, only: &
            dynamic_viscosity_t, &
            fallible_dynamic_viscosity_t, &
            dynamic_viscosity_unit_t, &
            fallible_dynamic_viscosity_unit_t, &
            operator(.unit.), &
            parse_dynamic_viscosity, &
            parse_dynamic_viscosity_unit, &
            sum, &
            abs, &
            DEFAULT_DYNAMIC_VISCOSITY_OUTPUT_UNITS => DEFAULT_OUTPUT_UNITS, &
            PROVIDED_DYNAMIC_VISCOSITY_UNITS => PROVIDED_UNITS, &
            MEGAPASCAL_SECONDS, &
            PASCAL_SECONDS
    use quaff_energy_m, only: &
            energy_t, &
            fallible_energy_t, &
            energy_unit_t, &
            fallible_energy_unit_t, &
            operator(.unit.), &
            parse_energy, &
            parse_energy_unit, &
            sum, &
            abs, &
            DEFAULT_ENERGY_OUTPUT_UNITS => DEFAULT_OUTPUT_UNITS, &
            PROVIDED_ENERGY_UNITS => PROVIDED_UNITS, &
            BTU, &
            CALORIES, &
            JOULES, &
            KILOJOULES, &
            MEGABTU, &
            MEGAWATT_DAYS, &
            POUNDS_FORCE_FEET
    use quaff_energy_per_amount_m, only: &
            energy_per_amount_t, &
            fallible_energy_per_amount_t, &
            energy_per_amount_unit_t, &
            fallible_energy_per_amount_unit_t, &
            operator(.unit.), &
            parse_energy_per_amount, &
            parse_energy_per_amount_unit, &
            sum, &
            abs, &
            DEFAULT_ENERGY_PER_AMOUNT_OUTPUT_UNITS => DEFAULT_OUTPUT_UNITS, &
            PROVIDED_ENERGY_PER_AMOUNT_UNITS => PROVIDED_UNITS, &
            JOULES_PER_MOL, &
            KILOJOULES_PER_MOL
    use quaff_energy_per_temperature_amount_m, only: &
            energy_per_temperature_amount_t, &
            fallible_energy_per_temperature_amount_t, &
            energy_per_temperature_amount_unit_t, &
            fallible_energy_per_temperature_amount_unit_t, &
            operator(.unit.), &
            parse_energy_per_temperature_amount, &
            parse_energy_per_temperature_amount_unit, &
            sum, &
            abs, &
            DEFAULT_ENERGY_PER_AMOUNT_TEMPERATURE_OUTPUT_UNITS => DEFAULT_OUTPUT_UNITS, &
            PROVIDED_ENERGY_PER_TEMPERATURE_AMOUNT_UNITS => PROVIDED_UNITS, &
            JOULES_PER_KELVIN_MOL, &
            KILOJOULES_PER_KELVIN_MOL
    use quaff_enthalpy_m, only: &
            enthalpy_t, &
            fallible_enthalpy_t, &
            enthalpy_unit_t, &
            fallible_enthalpy_unit_t, &
            operator(.unit.), &
            parse_enthalpy, &
            parse_enthalpy_unit, &
            sum, &
            abs, &
            DEFAULT_ENTHALPY_OUTPUT_UNITS => DEFAULT_OUTPUT_UNITS, &
            PROVIDED_ENTHALPY_UNITS => PROVIDED_UNITS, &
            JOULES_PER_KILOGRAM, &
            KILOJOULES_PER_KILOGRAM
    use quaff_fluence_m, only: &
            fluence_t, &
            fallible_fluence_t, &
            fluence_unit_t, &
            fallible_fluence_unit_t, &
            operator(.unit.), &
            parse_fluence, &
            parse_fluence_unit, &
            sum, &
            abs, &
            DEFAULT_FLUENCE_OUTPUT_UNITS => DEFAULT_OUTPUT_UNITS, &
            PROVIDED_FLUENCE_UNITS => PROVIDED_UNITS, &
            PARTICLES_PER_SQUARE_METER, &
            PARTICLES_PER_SQUARE_CENTIMETER
    use quaff_force_m, only: &
            force_t, &
            fallible_force_t, &
            force_unit_t, &
            fallible_force_unit_t, &
            operator(.unit.), &
            parse_force, &
            parse_force_unit, &
            sum, &
            abs, &
            DEFAULT_FORCE_OUTPUT_UNITS => DEFAULT_OUTPUT_UNITS, &
            PROVIDED_FORCE_UNITS => PROVIDED_UNITS, &
            DYNES, &
            KILOPONDS, &
            MILLINEWTONS, &
            NEWTONS, &
            POUNDS_FORCE
    use quaff_fracture_toughness_m, only: &
            fracture_toughness_t, &
            fallible_fracture_toughness_t, &
            fracture_toughness_unit_t, &
            fallible_fracture_toughness_unit_t, &
            operator(.unit.), &
            parse_fracture_toughness, &
            parse_fracture_toughness_unit, &
            sum, &
            abs, &
            DEFAULT_FRACTURE_TOUGHNESS_OUTPUT_UNITS => DEFAULT_OUTPUT_UNITS, &
            PROVIDED_FRACTURE_TOUGHNESS_UNITS => PROVIDED_UNITS, &
            PASCAL_ROOT_METER, &
            MEGAPASCAL_ROOT_METER, &
            KSI_ROOT_INCH
    use quaff_frequency_m, only: &
            frequency_t, &
            fallible_frequency_t, &
            frequency_unit_t, &
            fallible_frequency_unit_t, &
            operator(.unit.), &
            parse_frequency, &
            parse_frequency_unit, &
            sum, &
            abs, &
            DEFAULT_FREQUENCY_OUTPUT_UNITS => DEFAULT_OUTPUT_UNITS, &
            PROVIDED_FREQUENCY_UNITS => PROVIDED_UNITS, &
            HERTZ, &
            PER_SECOND, &
            PER_MINUTE
    use quaff_length_m, only: &
            length_t, &
            fallible_length_t, &
            length_unit_t, &
            fallible_length_unit_t, &
            operator(.unit.), &
            parse_length, &
            parse_length_unit, &
            sum, &
            abs, &
            DEFAULT_LENGTH_OUTPUT_UNITS => DEFAULT_OUTPUT_UNITS, &
            PROVIDED_LENGTH_UNITS => PROVIDED_UNITS, &
            CENTIMETERS, &
            FEET, &
            INCHES, &
            METERS, &
            MICROINCHES, &
            MICROMETERS, &
            MILLIMETERS
    use quaff_mass_m, only: &
            mass_t, &
            fallible_mass_t, &
            mass_unit_t, &
            fallible_mass_unit_t, &
            operator(.unit.), &
            parse_mass, &
            parse_mass_unit, &
            sum, &
            abs, &
            DEFAULT_MASS_OUTPUT_UNITS => DEFAULT_OUTPUT_UNITS, &
            PROVIDED_MASS_UNITS => PROVIDED_UNITS, &
            GRAMS, &
            KILOGRAMS, &
            POUNDS_MASS, &
            TONS
    use quaff_mass_rate_m, only: &
            mass_rate_t, &
            fallible_mass_rate_t, &
            mass_rate_unit_t, &
            fallible_mass_rate_unit_t, &
            operator(.unit.), &
            parse_mass_rate, &
            parse_mass_rate_unit, &
            sum, &
            abs, &
            DEFAULT_MASS_RATE_OUTPUT_UNITS => DEFAULT_OUTPUT_UNITS, &
            PROVIDED_MASS_RATE_UNITS => PROVIDED_UNITS, &
            GRAMS_PER_SECOND, &
            KILOGRAMS_PER_SECOND
    use quaff_molar_mass_m, only: &
            molar_mass_t, &
            fallible_molar_mass_t, &
            molar_mass_unit_t, &
            fallible_molar_mass_unit_t, &
            operator(.unit.), &
            parse_molar_mass, &
            parse_molar_mass_unit, &
            sum, &
            abs, &
            DEFAULT_MOLAR_MASS_OUTPUT_UNITS => DEFAULT_OUTPUT_UNITS, &
            PROVIDED_MOLAR_MASS_UNITS => PROVIDED_UNITS, &
            GRAMS_PER_MOL, &
            KILOGRAMS_PER_MOL
    use quaff_power_m, only: &
            power_t, &
            fallible_power_t, &
            power_unit_t, &
            fallible_power_unit_t, &
            operator(.unit.), &
            parse_power, &
            parse_power_unit, &
            sum, &
            abs, &
            DEFAULT_POWER_OUTPUT_UNITS => DEFAULT_OUTPUT_UNITS, &
            PROVIDED_POWER_UNITS => PROVIDED_UNITS, &
            BTU_PER_HOUR, &
            CALORIES_PER_SECOND, &
            MEGABTU_PER_HOUR, &
            MEGAWATTS, &
            WATTS
    use quaff_pressure_m, only: &
            pressure_t, &
            fallible_pressure_t, &
            pressure_unit_t, &
            fallible_pressure_unit_t, &
            operator(.unit.), &
            abs, &
            parse_pressure, &
            parse_pressure_unit, &
            sum, &
            DEFAULT_PRESSURE_OUTPUT_UNITS => DEFAULT_OUTPUT_UNITS, &
            PROVIDED_PRESSURE_UNITS => PROVIDED_UNITS, &
            ATMOSPHERES, &
            BAR, &
            DYNES_PER_SQUARE_CENTIMETER, &
            KILOPASCALS, &
            KILOPONDS_PER_SQUARE_CENTIMETER, &
            MEGAPASCALS, &
            PASCALS, &
            POUNDS_PER_SQUARE_INCH, &
            KILOPOUNDS_PER_SQUARE_INCH
    use quaff_specific_heat_m, only: &
            specific_heat_t, &
            fallible_specific_heat_t, &
            specific_heat_unit_t, &
            fallible_specific_heat_unit_t, &
            operator(.unit.), &
            parse_specific_heat, &
            parse_specific_heat_unit, &
            sum, &
            abs, &
            DEFAULT_SPECIFIC_HEAT_OUTPUT_UNITS => DEFAULT_OUTPUT_UNITS, &
            PROVIDED_SPECIFIC_HEAT_UNITS => PROVIDED_UNITS, &
            JOULES_PER_KILOGRAM_KELVIN, &
            BTU_PER_POUNDS_RANKINE, &
            BTU_PER_POUNDS_FAHRENHEIT
    use quaff_speed_m, only: &
            speed_t, &
            fallible_speed_t, &
            speed_unit_t, &
            fallible_speed_unit_t, &
            operator(.unit.), &
            parse_speed, &
            parse_speed_unit, &
            sum, &
            abs, &
            DEFAULT_SPEED_OUTPUT_UNITS => DEFAULT_OUTPUT_UNITS, &
            PROVIDED_SPEED_UNITS => PROVIDED_UNITS, &
            CENTIMETERS_PER_SECOND, &
            FEET_PER_SECOND, &
            METERS_PER_SECOND
    use quaff_stress_intensity_factor_m, only: &
            stress_intensity_factor_t, &
            fallible_stress_intensity_factor_t, &
            stress_intensity_factor_unit_t, &
            fallible_stress_intensity_factor_unit_t, &
            operator(.unit.), &
            parse_stress_intensity_factor, &
            parse_stress_intensity_factor_unit, &
            sum, &
            DEFAULT_STRESS_INTENSITY_FACTOR_OUTPUT_UNITS => DEFAULT_OUTPUT_UNITS, &
            PROVIDED_STRESS_INTENSITY_FACTOR_UNITS => PROVIDED_UNITS, &
            STRESS_INTENSITY_FACTOR_PASCAL_ROOT_METER => PASCAL_ROOT_METER, &
            STRESS_INTENSITY_FACTOR_MEGAPASCAL_ROOT_METER => MEGAPASCAL_ROOT_METER, &
            STRESS_INTENSITY_FACTOR_KSI_ROOT_INCH => KSI_ROOT_INCH
    use quaff_temperature_m, only: &
            temperature_t, &
            fallible_temperature_t, &
            temperature_unit_t, &
            fallible_temperature_unit_t, &
            operator(.unit.), &
            parse_temperature, &
            parse_temperature_unit, &
            DEFAULT_TEMPERATURE_OUTPUT_UNITS => DEFAULT_OUTPUT_UNITS, &
            PROVIDED_TEMPERATURE_UNITS => PROVIDED_UNITS, &
            CELSIUS, &
            FAHRENHEIT, &
            KELVIN, &
            RANKINE
    use quaff_thermal_conductivity_m, only: &
            thermal_conductivity_t, &
            fallible_thermal_conductivity_t, &
            thermal_conductivity_unit_t, &
            fallible_thermal_conductivity_unit_t, &
            operator(.unit.), &
            parse_thermal_conductivity, &
            parse_thermal_conductivity_unit, &
            sum, &
            DEFAULT_THERMAL_CONDUCTIVITY_OUTPUT_UNITS => DEFAULT_OUTPUT_UNITS, &
            PROVIDED_THERMAL_CONDUCTIVITY_UNITS => PROVIDED_UNITS, &
            CALORIES_PER_SECOND_CENTIMETER_KELVIN, &
            WATTS_PER_CENTIMETER_KELVIN, &
            WATTS_PER_METER_KELVIN, &
            BTU_PER_HOUR_FEET_FAHRENHEIT, &
            BTU_PER_HOUR_FEET_RANKINE
    use quaff_thermal_expansion_coeffecient_m, only: &
            thermal_expansion_coeffecient_t, &
            fallible_thermal_expansion_coeffecient_t, &
            thermal_expansion_coeffecient_unit_t, &
            fallible_thermal_expansion_coeffecient_unit_t, &
            operator(.unit.), &
            parse_thermal_expansion_coeffecient, &
            parse_thermal_expansion_coeffecient_unit, &
            sum, &
            DEFAULT_THERMAL_EXPANSION_COEFFECIENT_OUTPUT_UNITS => DEFAULT_OUTPUT_UNITS, &
            PROVIDED_THERMAL_EXPANSION_COEFFECIENT_UNITS => PROVIDED_UNITS, &
            PER_KELVIN, &
            PER_RANKINE, &
            PER_FAHRENHEIT
    use quaff_time_m, only: &
            time_t, &
            fallible_time_t, &
            time_unit_t, &
            fallible_time_unit_t, &
            operator(.unit.), &
            abs, &
            parse_time, &
            parse_time_unit, &
            sum, &
            DEFAULT_TIME_OUTPUT_UNITS => DEFAULT_OUTPUT_UNITS, &
            PROVIDED_TIME_UNITS => PROVIDED_UNITS, &
            DAYS, &
            HOURS, &
            MICROSECONDS, &
            MILLISECONDS, &
            MINUTES, &
            SECONDS, &
            YEARS
    use quaff_volume_m, only: &
            volume_t, &
            fallible_volume_t, &
            volume_unit_t, &
            fallible_volume_unit_t, &
            operator(.unit.), &
            parse_volume, &
            parse_volume_unit, &
            sum, &
            DEFAULT_VOLUME_OUTPUT_UNITS => DEFAULT_OUTPUT_UNITS, &
            PROVIDED_VOLUME_UNITS => PROVIDED_UNITS, &
            CUBIC_CENTIMETERS, &
            CUBIC_METERS, &
            CUBIC_MILLIMETERS, &
            LITERS
    use quaff_physical_constants, only: &
            ATMOSPHERIC_PRESSURE, &
            GRAVITY, &
            UNIVERSAL_GAS_CONSTANT
    use quaff_interquantity_operators_m, only: operator(*), operator(/), operator(-), operator(+), as_burnup
end module
