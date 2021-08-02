! Generated by make_vegetable_driver. DO NOT EDIT
program main
    implicit none

    call run()
contains
    subroutine run()
        use acceleration_test, only: &
                acceleration_acceleration => test_acceleration
        use amount_test, only: &
                amount_amount => test_amount
        use angle_test, only: &
                angle_angle => test_angle
        use area_test, only: &
                area_area => test_area
        use burnup_test, only: &
                burnup_burnup => test_burnup
        use convective_heat_transfer_test, only: &
                convective_heat_transfer_convective_heat_transfer => test_convective_heat_transfer
        use density_test, only: &
                density_density => test_density
        use dynamic_viscosity_test, only: &
                dynamic_viscosity_dynamic_viscosity => test_dynamic_viscosity
        use energy_per_amount_test, only: &
                energy_per_amount_energy_per_amount => test_energy_per_amount
        use energy_per_temperature_amount_test, only: &
                energy_per_temperature_amount_energy_per_temperature_amount => test_energy_per_temperature_amount
        use energy_test, only: &
                energy_energy => test_energy
        use enthalpy_test, only: &
                enthalpy_enthalpy => test_enthalpy
        use fluence_test, only: &
                fluence_fluence => test_fluence
        use force_test, only: &
                force_force => test_force
        use fracture_toughness_test, only: &
                fracture_toughness_fracture_toughness => test_fracture_toughness
        use interquantity_test, only: &
                interquantity_interquantity_operators => test_interquantity_operators
        use length_test, only: &
                length_length => test_length
        use mass_test, only: &
                mass_mass => test_mass
        use molar_mass_test, only: &
                molar_mass_molar_mass => test_molar_mass
        use performance_test, only: &
                performance_performance => test_performance
        use power_test, only: &
                power_power => test_power
        use pressure_test, only: &
                pressure_pressure => test_pressure
        use specific_heat_test, only: &
                specific_heat_specific_heat => test_specific_heat
        use speed_test, only: &
                speed_speed => test_speed
        use temperature_test, only: &
                temperature_temperature => test_temperature
        use thermal_conductivity_test, only: &
                thermal_conductivity_thermal_conductivity => test_thermal_conductivity
        use thermal_expansion_coeffecient_test, only: &
                thermal_expansion_coeffecient_thermal_expansion_coeffecient => test_thermal_expansion_coeffecient
        use time_test, only: &
                time_time => test_time
        use volume_test, only: &
                volume_volume => test_volume
        use vegetables, only: test_item_t, test_that, run_tests

        type(test_item_t) :: tests
        type(test_item_t) :: individual_tests(29)

        individual_tests(1) = acceleration_acceleration()
        individual_tests(2) = amount_amount()
        individual_tests(3) = angle_angle()
        individual_tests(4) = area_area()
        individual_tests(5) = burnup_burnup()
        individual_tests(6) = convective_heat_transfer_convective_heat_transfer()
        individual_tests(7) = density_density()
        individual_tests(8) = dynamic_viscosity_dynamic_viscosity()
        individual_tests(9) = energy_per_amount_energy_per_amount()
        individual_tests(10) = energy_per_temperature_amount_energy_per_temperature_amount()
        individual_tests(11) = energy_energy()
        individual_tests(12) = enthalpy_enthalpy()
        individual_tests(13) = fluence_fluence()
        individual_tests(14) = force_force()
        individual_tests(15) = fracture_toughness_fracture_toughness()
        individual_tests(16) = interquantity_interquantity_operators()
        individual_tests(17) = length_length()
        individual_tests(18) = mass_mass()
        individual_tests(19) = molar_mass_molar_mass()
        individual_tests(20) = performance_performance()
        individual_tests(21) = power_power()
        individual_tests(22) = pressure_pressure()
        individual_tests(23) = specific_heat_specific_heat()
        individual_tests(24) = speed_speed()
        individual_tests(25) = temperature_temperature()
        individual_tests(26) = thermal_conductivity_thermal_conductivity()
        individual_tests(27) = thermal_expansion_coeffecient_thermal_expansion_coeffecient()
        individual_tests(28) = time_time()
        individual_tests(29) = volume_volume()
        tests = test_that(individual_tests)

        call run_tests(tests)
    end subroutine
end program
