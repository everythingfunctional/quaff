module quaff_physical_constants
    use quaff_acceleration_m, only: acceleration_t
    use quaff_energy_per_temperature_amount_m, only: energy_per_temperature_amount_t
    use quaff_pressure_m, only: pressure_t
    use quaff_conversion_factors_m, only: &
            GRAVITY_ => GRAVITY, &
            PASCALS_PER_ATMOSPHERE

    implicit none

    type(pressure_t), parameter :: ATMOSPHERIC_PRESSURE = &
            pressure_t(pascals = PASCALS_PER_ATMOSPHERE)
    type(acceleration_t), parameter :: GRAVITY = &
            acceleration_t(meters_per_square_second = GRAVITY_)
    type(energy_per_temperature_amount_t), parameter :: UNIVERSAL_GAS_CONSTANT = &
            energy_per_temperature_amount_t(joules_per_kelvin_mol = 8.31446261815324d0)
end module
