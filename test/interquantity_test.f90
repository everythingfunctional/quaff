module interquantity_test
    use quaff, only: &
            operator(*), &
            operator(/), &
            operator(-), &
            operator(+), &
            operator(.unit.), &
            as_burnup, &
            sqrt, &
            CUBIC_METERS, &
            DELTA_KELVIN, &
            HERTZ, &
            JOULES, &
            JOULES_PER_KELVIN, &
            JOULES_PER_KELVIN_MOL, &
            JOULES_PER_KILOGRAM, &
            JOULES_PER_KILOGRAM_KELVIN, &
            JOULES_PER_MOL, &
            KELVIN, &
            KILOGRAMS, &
            KILOGRAMS_PER_CUBIC_METER, &
            KILOGRAMS_PER_MOL, &
            KILOGRAMS_PER_SECOND, &
            METERS, &
            METERS_PER_SECOND, &
            METERS_PER_SQUARE_SECOND, &
            MOLS, &
            MOLS_PER_CUBIC_METER, &
            MOLS_PER_KILOGRAM, &
            NEWTONS, &
            PASCAL_SECONDS, &
            PASCALS, &
            PER_KELVIN, &
            SECONDS, &
            SQUARE_METERS, &
            WATT_SECONDS_PER_KILOGRAM, &
            WATTS
    use quaff_asserts_m, only: assert_equals
    use veggies, only: result_t, test_item_t, describe, it, assert_equals

    implicit none
    private
    public :: test_interquantity_operators
contains
    function test_interquantity_operators() result(tests)
        type(test_item_t) :: tests

        tests = describe( &
                "multiplying or dividing different quantities and subtracting temperatures", &
                [ it("2 m * 3 m = 6 m^2", check_length_times_length) &
                , it("6 m^2 / 3 m = 2 m", check_area_divided_by_length) &
                , it("sqrt(9 m^2) = 3 m", check_square_root_of_area) &
                , it("2 m^2 * 3 m = 6 m^3", check_area_times_length) &
                , it("2 m * 3 m^2 = 6 m^3", check_length_times_area) &
                , it("6 m^3 / 3 m^2 = 2 m", check_volume_divided_by_area) &
                , it("6 m^3 / 3 m = 2 m^2", check_volume_divided_by_length) &
                , it("6 kg / 3 m^3 = 2 kg/m^3", check_mass_divided_by_volume) &
                , it("2 kg/m^3 * 3 m^3 = 6 kg", check_density_times_volume) &
                , it("2 m^3 * 3 kg/m^3 = 6 kg", check_volume_times_density) &
                , it("6 kg / 3 kg/m^3 = 2 m^3", check_mass_divided_by_density) &
                , it("6 m / 3 s = 2 m/s", check_length_divided_by_time) &
                , it("2 m/s * 3 s = 6 m", check_speed_times_time) &
                , it("2 s * 3 m/s = 6 m", check_time_times_speed) &
                , it("6 m / 3 m/s = 2 s", check_length_divided_by_speed) &
                , it("6 m/s / 3 s = 2 m/s^2", check_speed_divided_by_time) &
                , it("2 m/s^2 * 3 s = 6 m/s", check_acceleration_times_time) &
                , it("2 s * 3 m/s^2 = 6 m/s", check_time_times_acceleration) &
                , it("6 m/s / 3 m/s^2 = 2 s", check_speed_divided_by_acceleration) &
                , it("2 kg * 3 m/s^2 = 6 N", check_mass_times_acceleration) &
                , it("2 m/s^2 * 3 kg = 6 N", check_acceleration_times_mass) &
                , it("6 N / 3 m/s^2 = 2 kg", check_force_divided_by_acceleration) &
                , it("6 N / 3 kg = 2 m/s^2", check_force_divided_by_mass) &
                , it("6 N / 3 m^2 = 2 Pa", check_force_divided_by_area) &
                , it("2 Pa * 3 m^2 = 6 N", check_pressure_times_area) &
                , it("2 m^2 * 3 Pa = 6 N", check_area_times_pressure) &
                , it("6 N / 3 Pa = 2 m^2", check_force_divided_by_pressure) &
                , it("2 N * 3 m = 6 J", check_force_times_length) &
                , it("2 m * 3 N = 6 J", check_length_times_force) &
                , it("6 J / 3 N = 2 m", check_energy_divided_by_force) &
                , it("6 J / 3 m = 2 N", check_energy_divided_by_length) &
                , it("6 J / 3 s = 2 W", check_energy_divided_by_time) &
                , it("2 W * 3 s = 6 J", check_power_times_time) &
                , it("2 s * 3 W = 6 J", check_time_times_power) &
                , it("6 J / 3 W = 2 s", check_energy_divided_by_power) &
                , it("2 Pa * 3 s = 6 Pa s", check_pressure_times_time) &
                , it("2 s * 3 Pa = 6 Pa s", check_time_times_pressure) &
                , it("6 Pa s / 3 s = 2 Pa", check_dynamic_viscosity_divided_by_time) &
                , it("6 Pa s / 3 Pa = 2 s", check_dynamic_viscosity_divided_by_pressure) &
                , it("6 J / 3 kg = 2 J/kg", check_energy_divided_by_mass) &
                , it("2 J/kg * 3 kg = 6 J", check_enthalpy_times_mass) &
                , it("2 kg * 3 J/kg = 6 J", check_mass_times_enthalpy) &
                , it("6 J / 3 J/kg = 2 kg", check_energy_divided_by_enthalpy) &
                , it("6 kg / 3 mol = 2 kg/mol", check_mass_divided_by_amount) &
                , it("2 kg/mol * 3 mol = 6 kg", check_molar_mass_times_amount) &
                , it("2 mol * 3 kg/mol = 6 kg", check_amount_times_molar_mass) &
                , it("6 kg / 3 kg/mol = 2 mol", check_mass_divided_by_molar_mass) &
                , it("2 (W s)/kg * 3 kg = 6 J", check_burnup_times_mass) &
                , it("2 kg * 3 (W s)/kg = 6 J", check_mass_times_burnup) &
                , it("6 J / 3 (W s)/kg = 2 kg", check_energy_divided_by_burnup) &
                , it("2 J/kg = 2 (W s)/kg", check_enthalpy_to_burnup) &
                , it("2 Pa * 3 m^3 = 6 J", check_pressure_times_volume) &
                , it("2 m^3 * 3 Pa = 6 J", check_volume_times_pressure) &
                , it("2 J/(K mol) * 3 K = 6 J/mol", check_molar_specific_heat_times_temperature) &
                , it("2 K * 3 J/(K mol) = 6 J/mol", check_temperature_times_molar_specific_heat) &
                , it("6 J / 3 J/mol = 2 mol", check_energy_divided_by_molar_enthalpy) &
                , it("3 /K * 2 K = 6.0", check_thermal_expansion_times_delta_temp) &
                , it("1 K - 1 K = 0 K", check_temp_diff_gives_delta_temperature) &
                , it("1 K + 1 K = 2 K", check_temperature_plus_delta_temperature) &
                , it("1 K + 1 K = 2 K", check_delta_temperature_plus_temperature) &
                , it("2 J/mol * 3 mol = 6 J", check_molar_enthalpy_times_amount) &
                , it("2 mol * 3 J/mol = 6 J", check_amount_times_molar_enthalpy) &
                , it("6 J / 3 m^3 = 2 Pa", check_energy_divided_by_volume) &
                , it("2 mol * 3 J/(K mol) = 6 J/K", check_amount_times_molar_specific_heat) &
                , it("2 mol * 3 J/(K mol) = 6 J/K", check_molar_specific_heat_times_amount) &
                , it("6 J / 3 J/K = 2 K", check_energy_divided_by_energy_per_temperature) &
                , it("6 J/mol / 3 kg/mol = 2 J/kg", check_molar_enthalpy_divided_by_molar_mass) &
                , it("6 J/(K mol) / 3 kg/mol = 2 J/(kg K)", check_molar_specific_heat_divided_by_molar_mass) &
                , it("2 J/(kg K) * 3 K = 6 J/kg", check_specific_heat_times_temperature) &
                , it("2 K * 3 J/(kg K) = 6 J/kg", check_temperature_times_specific_heat) &
                , it("sqrt(9 J/kg) = 3 m/s", check_square_root_of_enthalpy) &
                , it("6 N / 3 m/s = 2 kg/s", check_force_divided_by_speed) &
                , it("6 J/kg / 3 m/s = 2 m/s", check_enthalpy_divided_by_speed) &
                , it("2 kg/s * 3 J/kg = 6 W", check_mass_rate_times_enthalpy) &
                , it("2 J/kg * 3 kg/s = 6 W", check_enthalpy_times_mass_rate) &
                , it("2 kg/s * 3 s = 6 kg", check_mass_rate_times_time) &
                , it("2 s * 3 kg/s = 6 kg", check_time_times_mass_rate) &
                , it("6 / 3 s = 2 Hz", check_number_divided_by_time) &
                , it("6 / 3 Hz = 2 s", check_number_divided_by_frequency) &
                , it("6 / 3 kg/mol = 2 mol/kg", check_number_divided_by_molar_mass) &
                , it("6 / 3 mol/kg = 2 kg/mol", check_number_divided_by_inverse_molar_mass) &
                , it("6 mol / 3 m^3 = 2 mol/m^3", check_amount_divided_by_volume) &
                , it("2 mol/m^3 * 3 m^3 = 6 mol", check_molar_density_times_volume) &
                , it("2 m^3 * 3 mol/m^3 = 6 mol", check_volume_times_molar_density) &
                , it("6 kg/m^3 / 3 kg/mol = 2 mol/m^3", check_density_divided_by_molar_mass) &
                , it("2 mol/m^3 * 3 J/mol = 6 Pa", check_molar_density_times_molar_enthalpy) &
                ])
    end function test_interquantity_operators

    pure function check_length_times_length() result(result_)
        type(result_t) :: result_

        result_ = assert_equals( &
                6.0d0.unit.SQUARE_METERS, &
                (2.0d0.unit.METERS) * (3.0d0.unit.METERS))
    end function

    pure function check_area_divided_by_length() result(result_)
        type(result_t) :: result_

        result_ = assert_equals( &
                2.0d0.unit.METERS, &
                (6.0d0.unit.SQUARE_METERS) / (3.0d0.unit.METERS))
    end function

    pure function check_square_root_of_area() result(result_)
        type(result_t) :: result_

        result_ = assert_equals( &
                3.d0.unit.METERS, &
                sqrt(9.d0.unit.SQUARE_METERS))
    end function

    pure function check_area_times_length() result(result_)
        type(result_t) :: result_

        result_ = assert_equals( &
                6.0d0.unit.CUBIC_METERS, &
                (2.0d0.unit.SQUARE_METERS) * (3.0d0.unit.METERS))
    end function

    pure function check_length_times_area() result(result_)
        type(result_t) :: result_

        result_ = assert_equals( &
                6.0d0.unit.CUBIC_METERS, &
                (2.0d0.unit.METERS) * (3.0d0.unit.SQUARE_METERS))
    end function

    pure function check_volume_divided_by_area() result(result_)
        type(result_t) :: result_

        result_ = assert_equals( &
                2.0d0.unit.METERS, &
                (6.0d0.unit.CUBIC_METERS) / (3.0d0.unit.SQUARE_METERS))
    end function

    pure function check_volume_divided_by_length() result(result_)
        type(result_t) :: result_

        result_ = assert_equals( &
                2.0d0.unit.SQUARE_METERS, &
                (6.0d0.unit.CUBIC_METERS) / (3.0d0.unit.METERS))
    end function

    pure function check_mass_divided_by_volume() result(result_)
        type(result_t) :: result_

        result_ = assert_equals( &
                2.0d0.unit.KILOGRAMS_PER_CUBIC_METER, &
                (6.0d0.unit.KILOGRAMS) / (3.0d0.unit.CUBIC_METERS))
    end function

    pure function check_density_times_volume() result(result_)
        type(result_t) :: result_

        result_ = assert_equals( &
                6.0d0.unit.KILOGRAMS, &
                (2.0d0.unit.KILOGRAMS_PER_CUBIC_METER) * (3.0d0.unit.CUBIC_METERS))
    end function

    pure function check_volume_times_density() result(result_)
        type(result_t) :: result_

        result_ = assert_equals( &
                6.0d0.unit.KILOGRAMS, &
                (2.0d0.unit.CUBIC_METERS) * (3.0d0.unit.KILOGRAMS_PER_CUBIC_METER))
    end function

    pure function check_mass_divided_by_density() result(result_)
        type(result_t) :: result_

        result_ = assert_equals( &
                2.0d0.unit.CUBIC_METERS, &
                (6.0d0.unit.KILOGRAMS) / (3.0d0.unit.KILOGRAMS_PER_CUBIC_METER))
    end function

    pure function check_length_divided_by_time() result(result_)
        type(result_t) :: result_

        result_ = assert_equals( &
                2.0d0.unit.METERS_PER_SECOND, &
                (6.0d0.unit.METERS) / (3.0d0.unit.SECONDS))
    end function

    pure function check_speed_times_time() result(result_)
        type(result_t) :: result_

        result_ = assert_equals( &
                6.0d0.unit.METERS, &
                (2.0d0.unit.METERS_PER_SECOND) * (3.0d0.unit.SECONDS))
    end function

    pure function check_time_times_speed() result(result_)
        type(result_t) :: result_

        result_ = assert_equals( &
                6.0d0.unit.METERS, &
                (2.0d0.unit.SECONDS) * (3.0d0.unit.METERS_PER_SECOND))
    end function

    pure function check_length_divided_by_speed() result(result_)
        type(result_t) :: result_

        result_ = assert_equals( &
                2.0d0.unit.SECONDS, &
                (6.0d0.unit.METERS) / (3.0d0.unit.METERS_PER_SECOND))
    end function

    pure function check_speed_divided_by_time() result(result_)
        type(result_t) :: result_

        result_ = assert_equals( &
                2.0d0.unit.METERS_PER_SQUARE_SECOND, &
                (6.0d0.unit.METERS_PER_SECOND) / (3.0d0.unit.SECONDS))
    end function

    pure function check_acceleration_times_time() result(result_)
        type(result_t) :: result_

        result_ = assert_equals( &
                6.0d0.unit.METERS_PER_SECOND, &
                (2.0d0.unit.METERS_PER_SQUARE_SECOND) * (3.0d0.unit.SECONDS))
    end function

    pure function check_time_times_acceleration() result(result_)
        type(result_t) :: result_

        result_ = assert_equals( &
                6.0d0.unit.METERS_PER_SECOND, &
                (2.0d0.unit.SECONDS) * (3.0d0.unit.METERS_PER_SQUARE_SECOND))
    end function

    pure function check_speed_divided_by_acceleration() result(result_)
        type(result_t) :: result_

        result_ = assert_equals( &
                2.0d0.unit.SECONDS, &
                (6.0d0.unit.METERS_PER_SECOND) / (3.0d0.unit.METERS_PER_SQUARE_SECOND))
    end function

    pure function check_mass_times_acceleration() result(result_)
        type(result_t) :: result_

        result_ = assert_equals( &
                6.0d0.unit.NEWTONS, &
                (2.0d0.unit.KILOGRAMS) * (3.0d0.unit.METERS_PER_SQUARE_SECOND))
    end function

    pure function check_acceleration_times_mass() result(result_)
        type(result_t) :: result_

        result_ = assert_equals( &
                6.0d0.unit.NEWTONS, &
                (2.0d0.unit.METERS_PER_SQUARE_SECOND) * (3.0d0.unit.KILOGRAMS))
    end function

    pure function check_force_divided_by_acceleration() result(result_)
        type(result_t) :: result_

        result_ = assert_equals( &
                2.0d0.unit.KILOGRAMS, &
                (6.0d0.unit.NEWTONS) / (3.0d0.unit.METERS_PER_SQUARE_SECOND))
    end function

    pure function check_force_divided_by_mass() result(result_)
        type(result_t) :: result_

        result_ = assert_equals( &
                2.0d0.unit.METERS_PER_SQUARE_SECOND, &
                (6.0d0.unit.NEWTONS) / (3.0d0.unit.KILOGRAMS))
    end function

    pure function check_force_divided_by_area() result(result_)
        type(result_t) :: result_

        result_ = assert_equals( &
                2.0d0.unit.PASCALS, &
                (6.0d0.unit.NEWTONS) / (3.0d0.unit.SQUARE_METERS))
    end function

    pure function check_pressure_times_area() result(result_)
        type(result_t) :: result_

        result_ = assert_equals( &
                6.0d0.unit.NEWTONS, &
                (2.0d0.unit.PASCALS) * (3.0d0.unit.SQUARE_METERS))
    end function

    pure function check_area_times_pressure() result(result_)
        type(result_t) :: result_

        result_ = assert_equals( &
                6.0d0.unit.NEWTONS, &
                (2.0d0.unit.SQUARE_METERS) * (3.0d0.unit.PASCALS))
    end function

    pure function check_force_divided_by_pressure() result(result_)
        type(result_t) :: result_

        result_ = assert_equals( &
                2.0d0.unit.SQUARE_METERS, &
                (6.0d0.unit.NEWTONS) / (3.0d0.unit.PASCALS))
    end function

    pure function check_force_times_length() result(result_)
        type(result_t) :: result_

        result_ = assert_equals( &
                6.0d0.unit.JOULES, &
                (2.0d0.unit.NEWTONS) * (3.0d0.unit.METERS))
    end function

    pure function check_length_times_force() result(result_)
        type(result_t) :: result_

        result_ = assert_equals( &
                6.0d0.unit.JOULES, &
                (2.0d0.unit.METERS) * (3.0d0.unit.NEWTONS))
    end function

    pure function check_energy_divided_by_force() result(result_)
        type(result_t) :: result_

        result_ = assert_equals( &
                2.0d0.unit.METERS, &
                (6.0d0.unit.JOULES) / (3.0d0.unit.NEWTONS))
    end function

    pure function check_energy_divided_by_length() result(result_)
        type(result_t) :: result_

        result_ = assert_equals( &
                2.0d0.unit.NEWTONS, &
                (6.0d0.unit.JOULES) / (3.0d0.unit.METERS))
    end function

    pure function check_energy_divided_by_time() result(result_)
        type(result_t) :: result_

        result_ = assert_equals( &
                2.0d0.unit.WATTS, &
                (6.0d0.unit.JOULES) / (3.0d0.unit.SECONDS))
    end function

    pure function check_power_times_time() result(result_)
        type(result_t) :: result_

        result_ = assert_equals( &
                6.0d0.unit.JOULES, &
                (2.0d0.unit.WATTS) * (3.0d0.unit.SECONDS))
    end function

    pure function check_time_times_power() result(result_)
        type(result_t) :: result_

        result_ = assert_equals( &
                6.0d0.unit.JOULES, &
                (2.0d0.unit.SECONDS) * (3.0d0.unit.WATTS))
    end function

    pure function check_energy_divided_by_power() result(result_)
        type(result_t) :: result_

        result_ = assert_equals( &
                2.0d0.unit.SECONDS, &
                (6.0d0.unit.JOULES) / (3.0d0.unit.WATTS))
    end function

    pure function check_pressure_times_time() result(result_)
        type(result_t) :: result_

        result_ = assert_equals( &
                6.0d0.unit.PASCAL_SECONDS, &
                (2.0d0.unit.PASCALS) * (3.0d0.unit.SECONDS))
    end function

    pure function check_time_times_pressure() result(result_)
        type(result_t) :: result_

        result_ = assert_equals( &
                6.0d0.unit.PASCAL_SECONDS, &
                (2.0d0.unit.SECONDS) * (3.0d0.unit.PASCALS))
    end function

    pure function check_dynamic_viscosity_divided_by_time() result(result_)
        type(result_t) :: result_

        result_ = assert_equals( &
                2.0d0.unit.PASCALS, &
                (6.0d0.unit.PASCAL_SECONDS) / (3.0d0.unit.SECONDS))
    end function

    pure function check_dynamic_viscosity_divided_by_pressure() result(result_)
        type(result_t) :: result_

        result_ = assert_equals( &
                2.0d0.unit.SECONDS, &
                (6.0d0.unit.PASCAL_SECONDS) / (3.0d0.unit.PASCALS))
    end function

    pure function check_energy_divided_by_mass() result(result_)
        type(result_t) :: result_

        result_ = assert_equals( &
                2.0d0.unit.JOULES_PER_KILOGRAM, &
                (6.0d0.unit.JOULES) / (3.0d0.unit.KILOGRAMS))
    end function

    pure function check_enthalpy_times_mass() result(result_)
        type(result_t) :: result_

        result_ = assert_equals( &
                6.0d0.unit.JOULES, &
                (2.0d0.unit.JOULES_PER_KILOGRAM) * (3.0d0.unit.KILOGRAMS))
    end function

    pure function check_mass_times_enthalpy() result(result_)
        type(result_t) :: result_

        result_ = assert_equals( &
                6.0d0.unit.JOULES, &
                (2.0d0.unit.KILOGRAMS) * (3.0d0.unit.JOULES_PER_KILOGRAM))
    end function

    pure function check_energy_divided_by_enthalpy() result(result_)
        type(result_t) :: result_

        result_ = assert_equals( &
                2.0d0.unit.KILOGRAMS, &
                (6.0d0.unit.JOULES) / (3.0d0.unit.JOULES_PER_KILOGRAM))
    end function

    pure function check_mass_divided_by_amount() result(result_)
        type(result_t) :: result_

        result_ = assert_equals( &
                2.0d0.unit.KILOGRAMS_PER_MOL, &
                (6.0d0.unit.KILOGRAMS) / (3.0d0.unit.MOLS))
    end function

    pure function check_molar_mass_times_amount() result(result_)
        type(result_t) :: result_

        result_ = assert_equals( &
                6.0d0.unit.KILOGRAMS, &
                (2.0d0.unit.KILOGRAMS_PER_MOL) * (3.0d0.unit.MOLS))
    end function

    pure function check_amount_times_molar_mass() result(result_)
        type(result_t) :: result_

        result_ = assert_equals( &
                6.0d0.unit.KILOGRAMS, &
                (2.0d0.unit.MOLS) * (3.0d0.unit.KILOGRAMS_PER_MOL))
    end function

    pure function check_mass_divided_by_molar_mass() result(result_)
        type(result_t) :: result_

        result_ = assert_equals( &
                2.0d0.unit.MOLS, &
                (6.0d0.unit.KILOGRAMS) / (3.0d0.unit.KILOGRAMS_PER_MOL))
    end function

    pure function check_burnup_times_mass() result(result_)
        type(result_t) :: result_

        result_ = assert_equals( &
                6.0d0.unit.JOULES, &
                (2.0d0.unit.WATT_SECONDS_PER_KILOGRAM) * (3.0d0.unit.KILOGRAMS))
    end function

    pure function check_mass_times_burnup() result(result_)
        type(result_t) :: result_

        result_ = assert_equals( &
                6.0d0.unit.JOULES, &
                (2.0d0.unit.KILOGRAMS) * (3.0d0.unit.WATT_SECONDS_PER_KILOGRAM))
    end function

    pure function check_energy_divided_by_burnup() result(result_)
        type(result_t) :: result_

        result_ = assert_equals( &
                2.0d0.unit.KILOGRAMS, &
                (6.0d0.unit.JOULES) / (3.0d0.unit.WATT_SECONDS_PER_KILOGRAM))
    end function

    pure function check_enthalpy_to_burnup() result(result_)
        type(result_t) :: result_

        result_ = assert_equals( &
                2.0d0.unit.WATT_SECONDS_PER_KILOGRAM, &
                as_burnup(2.0d0.unit.JOULES_PER_KILOGRAM))
    end function

    pure function check_pressure_times_volume() result(result_)
        type(result_t) :: result_

        result_ = assert_equals( &
                6.0d0.unit.JOULES, &
                (2.0d0.unit.PASCALS) * (3.0d0.unit.CUBIC_METERS))
    end function

    pure function check_volume_times_pressure() result(result_)
        type(result_t) :: result_

        result_ = assert_equals( &
                6.0d0.unit.JOULES, &
                (2.0d0.unit.CUBIC_METERS) * (3.0d0.unit.PASCALS))
    end function

    pure function check_molar_specific_heat_times_temperature() result(result_)
        type(result_t) :: result_

        result_ = assert_equals( &
                6.0d0.unit.JOULES_PER_MOL, &
                (2.0d0.unit.JOULES_PER_KELVIN_MOL) * (3.0d0.unit.KELVIN))
    end function

    pure function check_temperature_times_molar_specific_heat() result(result_)
        type(result_t) :: result_

        result_ = assert_equals( &
                6.0d0.unit.JOULES_PER_MOL, &
                (2.0d0.unit.KELVIN) * (3.0d0.unit.JOULES_PER_KELVIN_MOL))
    end function

    pure function check_energy_divided_by_molar_enthalpy() result(result_)
        type(result_t) :: result_

        result_ = assert_equals( &
                2.0d0.unit.MOLS, &
                (6.0d0.unit.JOULES) / (3.0d0.unit.JOULES_PER_MOL))
    end function

    pure function check_thermal_expansion_times_delta_temp() result(result_)
        type(result_t) :: result_

        result_ = assert_equals( &
                6.0d0, &
                (3.0d0.unit.PER_KELVIN) * (2.0d0.unit.DELTA_KELVIN))
    end function


    pure function check_temp_diff_gives_delta_temperature() result(result_)
        type(result_t) :: result_

        result_ = assert_equals( &
                0.d0.unit.DELTA_KELVIN, &
                (1.0d0.unit.KELVIN) - (1.0d0.unit.KELVIN))
    end function

    pure function check_temperature_plus_delta_temperature() result(result_)
        type(result_t) :: result_

        result_ = assert_equals( &
                2.d0.unit.KELVIN, &
                (1.0d0.unit.KELVIN) + (1.0d0.unit.DELTA_KELVIN))
    end function

    pure function check_delta_temperature_plus_temperature() result(result_)
        type(result_t) :: result_

        result_ = assert_equals( &
                2.d0.unit.KELVIN, &
                (1.0d0.unit.DELTA_KELVIN) + (1.0d0.unit.KELVIN))
    end function

    pure function check_molar_enthalpy_times_amount() result(result_)
        type(result_t) :: result_

        result_ = assert_equals( &
                6.d0.unit.JOULES, &
                (2.d0.unit.JOULES_PER_MOL) * (3.d0.unit.MOLS))
    end function

    pure function check_amount_times_molar_enthalpy() result(result_)
        type(result_t) :: result_

        result_ = assert_equals( &
                6.d0.unit.JOULES, &
                (2.d0.unit.MOLS) * (3.d0.unit.JOULES_PER_MOL))
    end function

    pure function check_energy_divided_by_volume() result(result_)
        type(result_t) :: result_

        result_ = assert_equals( &
                2.d0.unit.PASCALS, &
                (6.d0.unit.JOULES) / (3.d0.unit.CUBIC_METERS))
    end function

    pure function check_amount_times_molar_specific_heat() result(result_)
        type(result_t) :: result_

        result_ = assert_equals( &
                6.d0.unit.JOULES_PER_KELVIN, &
                (2.d0.unit.MOLS) * (3.d0.unit.JOULES_PER_KELVIN_MOL))
    end function

    pure function check_molar_specific_heat_times_amount() result(result_)
        type(result_t) :: result_

        result_ = assert_equals( &
                6.d0.unit.JOULES_PER_KELVIN, &
                (2.d0.unit.JOULES_PER_KELVIN_MOL) * (3.d0.unit.MOLS))
    end function

    pure function check_energy_divided_by_energy_per_temperature() result(result_)
        type(result_t) :: result_

        result_ = assert_equals( &
                2.d0.unit.DELTA_KELVIN, &
                (6.d0.unit.JOULES) / (3.d0.unit.JOULES_PER_KELVIN))
    end function

    pure function check_molar_enthalpy_divided_by_molar_mass() result(result_)
        type(result_t) :: result_

        result_ = assert_equals( &
                2.d0.unit.JOULES_PER_KILOGRAM, &
                (6.d0.unit.JOULES_PER_MOL) / (3.d0.unit.KILOGRAMS_PER_MOL))
    end function

    pure function check_molar_specific_heat_divided_by_molar_mass() result(result_)
        type(result_t) :: result_

        result_ = assert_equals( &
                2.d0.unit.JOULES_PER_KILOGRAM_KELVIN, &
                (6.d0.unit.JOULES_PER_KELVIN_MOL) / (3.d0.unit.KILOGRAMS_PER_MOL))
    end function

    pure function check_specific_heat_times_temperature() result(result_)
        type(result_t) :: result_

        result_ = assert_equals( &
                6.d0.unit.JOULES_PER_KILOGRAM, &
                (2.d0.unit.JOULES_PER_KILOGRAM_KELVIN) * (3.d0.unit.KELVIN))
    end function

    pure function check_temperature_times_specific_heat() result(result_)
        type(result_t) :: result_

        result_ = assert_equals( &
                6.d0.unit.JOULES_PER_KILOGRAM, &
                (2.d0.unit.KELVIN) * (3.d0.unit.JOULES_PER_KILOGRAM_KELVIN))
    end function

    pure function check_square_root_of_enthalpy() result(result_)
        type(result_t) :: result_

        result_ = assert_equals( &
                3.d0.unit.METERS_PER_SECOND, &
                sqrt(9.d0.unit.JOULES_PER_KILOGRAM))
    end function

    pure function check_force_divided_by_speed() result(result_)
        type(result_t) :: result_

        result_ = assert_equals( &
                2.d0.unit.KILOGRAMS_PER_SECOND, &
                (6.d0.unit.NEWTONS) / (3.d0.unit.METERS_PER_SECOND))
    end function

    pure function check_enthalpy_divided_by_speed() result(result_)
        type(result_t) :: result_

        result_ = assert_equals( &
                2.d0.unit.METERS_PER_SECOND, &
                (6.d0.unit.JOULES_PER_KILOGRAM) / (3.d0.unit.METERS_PER_SECOND))
    end function

    pure function check_mass_rate_times_enthalpy() result(result_)
        type(result_t) :: result_

        result_ = assert_equals( &
                6.d0.unit.WATTS, &
                (2.d0.unit.KILOGRAMS_PER_SECOND) * (3.d0.unit.JOULES_PER_KILOGRAM))
    end function

    pure function check_enthalpy_times_mass_rate() result(result_)
        type(result_t) :: result_

        result_ = assert_equals( &
                6.d0.unit.WATTS, &
                (2.d0.unit.JOULES_PER_KILOGRAM) * (3.d0.unit.KILOGRAMS_PER_SECOND))
    end function

    pure function check_mass_rate_times_time() result(result_)
        type(result_t) :: result_

        result_ = assert_equals( &
                6.d0.unit.KILOGRAMS, &
                (2.d0.unit.KILOGRAMS_PER_SECOND) * (3.d0.unit.SECONDS))
    end function

    pure function check_time_times_mass_rate() result(result_)
        type(result_t) :: result_

        result_ = assert_equals( &
                6.d0.unit.KILOGRAMS, &
                (2.d0.unit.SECONDS) * (3.d0.unit.KILOGRAMS_PER_SECOND))
    end function

    pure function check_number_divided_by_time() result(result_)
        type(result_t) :: result_

        result_ = assert_equals( &
                2.d0.unit.HERTZ, &
                6.d0 / (3d0.unit.SECONDS))
    end function

    pure function check_number_divided_by_frequency() result(result_)
        type(result_t) :: result_

        result_ = assert_equals( &
                2.d0.unit.SECONDS, &
                6.d0 / (3.d0.unit.HERTZ))
    end function

    pure function check_number_divided_by_molar_mass() result(result_)
        type(result_t) :: result_

        result_ = assert_equals( &
                2.d0.unit.MOLS_PER_KILOGRAM, &
                6.d0 / (3.d0.unit.KILOGRAMS_PER_MOL))
    end function

    pure function check_number_divided_by_inverse_molar_mass() result(result_)
        type(result_t) :: result_

        result_ = assert_equals( &
                2.d0.unit.KILOGRAMS_PER_MOL, &
                6.d0 / (3.d0.unit.MOLS_PER_KILOGRAM))
    end function

    pure function check_amount_divided_by_volume() result(result_)
        type(result_t) :: result_

        result_ = assert_equals( &
                2.d0.unit.MOLS_PER_CUBIC_METER, &
                (6.d0.unit.MOLS) / (3.d0.unit.CUBIC_METERS))
    end function

    pure function check_molar_density_times_volume() result(result_)
        type(result_t) :: result_

        result_ = assert_equals( &
                6.d0.unit.MOLS, &
                (2.d0.unit.MOLS_PER_CUBIC_METER) * (3.d0.unit.CUBIC_METERS))
    end function

    pure function check_volume_times_molar_density() result(result_)
        type(result_t) :: result_

        result_ = assert_equals( &
                6.d0.unit.MOLS, &
                (2.d0.unit.CUBIC_METERS) * (3.d0.unit.MOLS_PER_CUBIC_METER))
    end function

    pure function check_density_divided_by_molar_mass() result(result_)
        type(result_t) :: result_

        result_ = assert_equals( &
                2.d0.unit.MOLS_PER_CUBIC_METER, &
                (6.d0.unit.KILOGRAMS_PER_CUBIC_METER) / (3.d0.unit.KILOGRAMS_PER_MOL))
    end function

    pure function check_molar_density_times_molar_enthalpy() result(result_)
        type(result_t) :: result_

        result_ = assert_equals( &
                6.d0.unit.PASCALS, &
                (2.d0.unit.MOLS_PER_CUBIC_METER) * (3.d0.unit.JOULES_PER_MOL))
    end function
end module
