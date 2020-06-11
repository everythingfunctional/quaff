module quaff_physical_constants
    use Acceleration_m, only: Acceleration_t
    use Energy_per_temperature_amount_m, only: EnergyPerTemperatureAmount_t
    use Pressure_m, only: Pressure_t
    use quaff_Conversion_factors_m, only: &
            GRAVITY_ => GRAVITY, &
            PASCALS_PER_ATMOSPHERE

    implicit none

    type(Pressure_t), parameter :: ATMOSPHERIC_PRESSURE = &
            Pressure_t(pascals = PASCALS_PER_ATMOSPHERE)
    type(Acceleration_t), parameter :: GRAVITY = &
            Acceleration_t(meters_per_square_second = GRAVITY_)
    type(EnergyPerTemperatureAmount_t), parameter :: UNIVERSAL_GAS_CONSTANT = &
            EnergyPerTemperatureAmount_t(joules_per_kelvin_mol = 8.31446261815324d0)
end module quaff_physical_constants
