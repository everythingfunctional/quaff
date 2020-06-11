module quaff
    use Acceleration_m, only: &
            Acceleration_t, &
            AccelerationUnit_t, &
            operator(.unit.), &
            fromString, &
            sum, &
            DEFAULT_ACCELERATION_OUTPUT_UNITS => DEFAULT_OUTPUT_UNITS, &
            PROVIDED_ACCELERATION_UNITS => PROVIDED_UNITS, &
            CENTIMETERS_PER_SQUARE_SECOND, &
            FEET_PER_SQUARE_SECOND, &
            METERS_PER_SQUARE_SECOND
    use Amount_m, only: &
            Amount_t, &
            AmountUnit_t, &
            operator(.unit.), &
            fromString, &
            sum, &
            DEFAULT_AMOUNT_OUTPUT_UNITS => DEFAULT_OUTPUT_UNITS, &
            PROVIDED_AMOUNT_UNITS => PROVIDED_UNITS, &
            MOLS, &
            PARTICLES
    use Angle_m, only: &
            Angle_t, &
            AngleUnit_t, &
            operator(.unit.), &
            fromString, &
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
    use Area_m, only: &
            Area_t, &
            AreaUnit_t, &
            operator(.unit.), &
            fromString, &
            sum, &
            DEFAULT_AREA_OUTPUT_UNITS => DEFAULT_OUTPUT_UNITS, &
            PROVIDED_AREA_UNITS => PROVIDED_UNITS, &
            SQUARE_CENTIMETERS, &
            SQUARE_FEET, &
            SQUARE_INCHES, &
            SQUARE_METERS
    use Burnup_m, only: &
            Burnup_t, &
            BurnupUnit_t, &
            operator(.unit.), &
            fromString, &
            sum, &
            DEFAULT_BURNUP_OUTPUT_UNITS => DEFAULT_OUTPUT_UNITS, &
            PROVIDED_BURNUP_UNITS => PROVIDED_UNITS, &
            MEGAWATT_DAYS_PER_TON, &
            WATT_SECONDS_PER_KILOGRAM
    use Density_m, only: &
            Density_t, &
            DensityUnit_t, &
            operator(.unit.), &
            fromString, &
            sum, &
            DEFAULT_DENSITY_OUTPUT_UNITS => DEFAULT_OUTPUT_UNITS, &
            PROVIDED_DENSITY_UNITS => PROVIDED_UNITS, &
            GRAMS_PER_CUBIC_METER, &
            KILOGRAMS_PER_CUBIC_METER
    use Dynamic_viscosity_m, only: &
            DynamicViscosity_t, &
            DynamicViscosityUnit_t, &
            operator(.unit.), &
            fromString, &
            sum, &
            DEFAULT_DYNAMIC_VISCOSITY_OUTPUT_UNITS => DEFAULT_OUTPUT_UNITS, &
            PROVIDED_DYNAMIC_VISCOSITY_UNITS => PROVIDED_UNITS, &
            MEGAPASCAL_SECONDS, &
            PASCAL_SECONDS
    use Energy_m, only: &
            Energy_t, &
            EnergyUnit_t, &
            operator(.unit.), &
            fromString, &
            sum, &
            DEFAULT_ENERGY_OUTPUT_UNITS => DEFAULT_OUTPUT_UNITS, &
            PROVIDED_ENERGY_UNITS => PROVIDED_UNITS, &
            BTU, &
            CALORIES, &
            JOULES, &
            KILOJOULES, &
            MEGABTU, &
            MEGAWATT_DAYS
    use Energy_per_temperature_amount_m, only: &
            EnergyPerTemperatureAmount_t, &
            EnergyPerTemperatureAmountUnit_t, &
            operator(.unit.), &
            fromString, &
            sum, &
            DEFAULT_ENERGY_PER_TEMPERATURE_AMOUNT_UNITS => DEFAULT_OUTPUT_UNITS, &
            PROVIDED_ENERGY_PER_TEMPERATURE_AMOUNT_UNITS => PROVIDED_UNITS, &
            JOULES_PER_KELVIN_MOL, &
            KILOJOULES_PER_KELVIN_MOL
    use Enthalpy_m, only: &
            Enthalpy_t, &
            EnthalpyUnit_t, &
            operator(.unit.), &
            fromString, &
            sum, &
            DEFAULT_ENTHALPY_OUTPUT_UNITS => DEFAULT_OUTPUT_UNITS, &
            PROVIDED_ENTHALPY_UNITS => PROVIDED_UNITS, &
            KILOJOULES_PER_KILOGRAM, &
            JOULES_PER_KILOGRAM
    use Force_m, only: &
            Force_t, &
            ForceUnit_t, &
            operator(.unit.), &
            fromString, &
            sum, &
            DEFAULT_FORCE_OUTPUT_UNITS => DEFAULT_OUTPUT_UNITS, &
            PROVIDED_FORCE_UNITS => PROVIDED_UNITS, &
            DYNES, &
            KILOPONDS, &
            MILLINEWTONS, &
            NEWTONS, &
            POUNDS_FORCE
    use quaff_Interquantity_operators_m, only: operator(*), operator(/), asBurnup
    use Length_m, only: &
            Length_t, &
            LengthUnit_t, &
            operator(.unit.), &
            fromString, &
            sum, &
            DEFAULT_LENGTH_OUTPUT_UNITS => DEFAULT_OUTPUT_UNITS, &
            PROVIDED_LENGTH_UNITS => PROVIDED_UNITS, &
            CENTIMETERS, &
            FEET, &
            INCHES, &
            METERS, &
            MICROINCHES, &
            MICROMETERS
    use Mass_m, only: &
            Mass_t, &
            MassUnit_t, &
            operator(.unit.), &
            fromString, &
            sum, &
            DEFAULT_MASS_OUTPUT_UNITS => DEFAULT_OUTPUT_UNITS, &
            PROVIDED_MASS_UNITS => PROVIDED_UNITS, &
            GRAMS, &
            KILOGRAMS, &
            POUNDS_MASS, &
            TONS
    use Molar_mass_m, only: &
            MolarMass_t, &
            MolarMassUnit_t, &
            operator(.unit.), &
            fromString, &
            sum, &
            DEFAULT_MOLAR_MASS_OUTPUT_UNITS => DEFAULT_OUTPUT_UNITS, &
            PROVIDED_MOLAR_MASS_UNITS => PROVIDED_UNITS, &
            GRAMS_PER_MOL, &
            KILOGRAMS_PER_MOL
    use Power_m, only: &
            Power_t, &
            PowerUnit_t, &
            operator(.unit.), &
            fromString, &
            sum, &
            DEFAULT_POWER_OUTPUT_UNITS => DEFAULT_OUTPUT_UNITS, &
            PROVIDED_POWER_UNITS => PROVIDED_UNITS, &
            BTU_PER_HOUR, &
            CALORIES_PER_SECOND, &
            MEGABTU_PER_HOUR, &
            MEGAWATTS, &
            WATTS
    use Pressure_m, only: &
            Pressure_t, &
            PressureUnit_t, &
            operator(.unit.), &
            fromString, &
            sum, &
            DEFAULT_PRESSURE_OUTPUT_UNITS => DEFAULT_OUTPUT_UNITS, &
            PROVIDED_PRESSURE_UNITS => PROVIDED_UNITS, &
            DYNES_PER_SQUARE_CENTIMETER, &
            KILOPASCALS, &
            KILOPONDS_PER_SQUARE_CENTIMETER, &
            MEGAPASCALS, &
            PASCALS, &
            POUNDS_PER_SQUARE_INCH
    use Quantity_module_m, only: &
            QuantityCamel_t, &
            QuantityCamelUnit_t, &
            operator(.unit.), &
            fromString, &
            sum, &
            DEFAULT_QUANTITY_CAPITAL_OUTPUT_UNITS => DEFAULT_OUTPUT_UNITS, &
            PROVIDED_QUANTITY_CAPITAL_UNITS => PROVIDED_UNITS, &
            UNITS_CAPITAL, &
            UNITS_CAPITAL2
    use Speed_m, only: &
            Speed_t, &
            SpeedUnit_t, &
            operator(.unit.), &
            fromString, &
            sum, &
            DEFAULT_SPEED_OUTPUT_UNITS => DEFAULT_OUTPUT_UNITS, &
            PROVIDED_SPEED_UNITS => PROVIDED_UNITS, &
            CENTIMETERS_PER_SECOND, &
            FEET_PER_SECOND, &
            METERS_PER_SECOND
    use Temperature_m, only: &
            Temperature_t, &
            TemperatureUnit_t, &
            operator(.unit.), &
            fromString, &
            sum, &
            DEFAULT_TEMPERATURE_OUTPUT_UNITS => DEFAULT_OUTPUT_UNITS, &
            PROVIDED_TEMPERATURE_UNITS => PROVIDED_UNITS, &
            CELSIUS, &
            FAHRENHEIT, &
            KELVIN, &
            RANKINE
    use Thermal_conductivity_m, only: &
            ThermalConductivity_t, &
            ThermalConductivityUnit_t, &
            operator(.unit.), &
            fromString, &
            sum, &
            DEFAULT_THERMAL_CONDUCTIVITY_OUTPUT_UNITS => DEFAULT_OUTPUT_UNITS, &
            PROVIDED_THERMAL_CONDUCTIVITY_UNITS => PROVIDED_UNITS, &
            CALORIES_PER_SECOND_CENTIMETER_KELVIN, &
            WATTS_PER_CENTIMETER_KELVIN, &
            WATTS_PER_METER_KELVIN
    use Time_m, only: &
            Time_t, &
            TimeUnit_t, &
            operator(.unit.), &
            fromString, &
            sum, &
            DEFAULT_TIME_OUTPUT_UNITS => DEFAULT_OUTPUT_UNITS, &
            PROVIDED_TIME_UNITS => PROVIDED_UNITS, &
            DAYS, &
            HOURS, &
            MINUTES, &
            SECONDS
    use Volume_m, only: &
            Volume_t, &
            VolumeUnit_t, &
            operator(.unit.), &
            fromString, &
            sum, &
            DEFAULT_VOLUME_OUTPUT_UNITS => DEFAULT_OUTPUT_UNITS, &
            PROVIDED_VOLUME_UNITS => PROVIDED_UNITS, &
            CUBIC_CENTIMETERS, &
            CUBIC_METERS
end module quaff
