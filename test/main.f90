! Generated by cart. DO NOT EDIT
program main
    implicit none

    if (.not.run()) stop 1
contains
    function run() result(passed)
        use acceleration_test, only: &
                acceleration_acceleration => &
                    test_acceleration
        use amount_rate_test, only: &
                amount_rate_amount_rate => &
                    test_amount_rate
        use amount_temperature_rate_test, only: &
                amount_temperature_rate_amount_temperature_rate => &
                    test_amount_temperature_rate
        use amount_temperature_test, only: &
                amount_temperature_amount_temperature => &
                    test_amount_temperature
        use amount_test, only: &
                amount_amount => &
                    test_amount
        use angle_test, only: &
                angle_angle => &
                    test_angle
        use area_test, only: &
                area_area => &
                    test_area
        use burnup_test, only: &
                burnup_burnup => &
                    test_burnup
        use convective_heat_transfer_test, only: &
                convective_heat_transfer_convective_heat_transfer => &
                    test_convective_heat_transfer
        use delta_temperature_test, only: &
                delta_temperature_delta_temperature => &
                    test_delta_temperature
        use density_test, only: &
                density_density => &
                    test_density
        use dynamic_viscosity_test, only: &
                dynamic_viscosity_dynamic_viscosity => &
                    test_dynamic_viscosity
        use energy_per_temperature_amount_test, only: &
                energy_per_temperature_amount_energy_per_temperature_amount => &
                    test_energy_per_temperature_amount
        use energy_per_temperature_test, only: &
                energy_per_temperature_energy_per_temperature => &
                    test_energy_per_temperature
        use energy_test, only: &
                energy_energy => &
                    test_energy
        use enthalpy_test, only: &
                enthalpy_enthalpy => &
                    test_enthalpy
        use fluence_test, only: &
                fluence_fluence => &
                    test_fluence
        use force_test, only: &
                force_force => &
                    test_force
        use fracture_toughness_test, only: &
                fracture_toughness_fracture_toughness => &
                    test_fracture_toughness
        use frequency_test, only: &
                frequency_frequency => &
                    test_frequency
        use interquantity_test, only: &
                interquantity_interquantity_operators => &
                    test_interquantity_operators
        use length_test, only: &
                length_length => &
                    test_length
        use mass_rate_test, only: &
                mass_rate_mass_rate => &
                    test_mass_rate
        use mass_test, only: &
                mass_mass => &
                    test_mass
        use molar_enthalpy_test, only: &
                molar_enthalpy_molar_enthalpy => &
                    test_molar_enthalpy
        use molar_mass_test, only: &
                molar_mass_molar_mass => &
                    test_molar_mass
        use power_test, only: &
                power_power => &
                    test_power
        use pressure_test, only: &
                pressure_pressure => &
                    test_pressure
        use specific_heat_test, only: &
                specific_heat_specific_heat => &
                    test_specific_heat
        use speed_test, only: &
                speed_speed => &
                    test_speed
        use stress_intensity_factor_test, only: &
                stress_intensity_factor_stress_intensity_factor => &
                    test_stress_intensity_factor
        use temperature_test, only: &
                temperature_temperature => &
                    test_temperature
        use thermal_conductivity_test, only: &
                thermal_conductivity_thermal_conductivity => &
                    test_thermal_conductivity
        use thermal_expansion_coefficient_test, only: &
                thermal_expansion_coefficient_thermal_expansion_coefficient => &
                    test_thermal_expansion_coefficient
        use time_test, only: &
                time_time => &
                    test_time
        use volume_test, only: &
                volume_volume => &
                    test_volume
        use veggies, only: test_item_t, test_that, run_tests



        logical :: passed

        type(test_item_t) :: tests
        type(test_item_t) :: individual_tests(36)

        individual_tests(1) = acceleration_acceleration()
        individual_tests(2) = amount_rate_amount_rate()
        individual_tests(3) = amount_temperature_rate_amount_temperature_rate()
        individual_tests(4) = amount_temperature_amount_temperature()
        individual_tests(5) = amount_amount()
        individual_tests(6) = angle_angle()
        individual_tests(7) = area_area()
        individual_tests(8) = burnup_burnup()
        individual_tests(9) = convective_heat_transfer_convective_heat_transfer()
        individual_tests(10) = delta_temperature_delta_temperature()
        individual_tests(11) = density_density()
        individual_tests(12) = dynamic_viscosity_dynamic_viscosity()
        individual_tests(13) = energy_per_temperature_amount_energy_per_temperature_amount()
        individual_tests(14) = energy_per_temperature_energy_per_temperature()
        individual_tests(15) = energy_energy()
        individual_tests(16) = enthalpy_enthalpy()
        individual_tests(17) = fluence_fluence()
        individual_tests(18) = force_force()
        individual_tests(19) = fracture_toughness_fracture_toughness()
        individual_tests(20) = frequency_frequency()
        individual_tests(21) = interquantity_interquantity_operators()
        individual_tests(22) = length_length()
        individual_tests(23) = mass_rate_mass_rate()
        individual_tests(24) = mass_mass()
        individual_tests(25) = molar_enthalpy_molar_enthalpy()
        individual_tests(26) = molar_mass_molar_mass()
        individual_tests(27) = power_power()
        individual_tests(28) = pressure_pressure()
        individual_tests(29) = specific_heat_specific_heat()
        individual_tests(30) = speed_speed()
        individual_tests(31) = stress_intensity_factor_stress_intensity_factor()
        individual_tests(32) = temperature_temperature()
        individual_tests(33) = thermal_conductivity_thermal_conductivity()
        individual_tests(34) = thermal_expansion_coefficient_thermal_expansion_coefficient()
        individual_tests(35) = time_time()
        individual_tests(36) = volume_volume()
        tests = test_that(individual_tests)


        passed = run_tests(tests)

    end function
end program
