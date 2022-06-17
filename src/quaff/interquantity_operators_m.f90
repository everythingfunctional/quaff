module quaff_interquantity_operators_m
    use quaff_acceleration_m, only: acceleration_t
    use quaff_amount_m, only: amount_t
    use quaff_area_m, only: area_t
    use quaff_burnup_m, only: burnup_t
    use quaff_delta_temperature_m, only: delta_temperature_t
    use quaff_density_m, only: density_t
    use quaff_dynamic_viscosity_m, only: dynamic_viscosity_t
    use quaff_energy_m, only: energy_t
    use quaff_molar_enthalpy_m, only: molar_enthalpy_t
    use quaff_energy_per_temperature_m, only: energy_per_temperature_t
    use quaff_molar_specific_heat_m, only: molar_specific_heat_t
    use quaff_enthalpy_m, only: enthalpy_t
    use quaff_force_m, only: force_t
    use quaff_length_m, only: length_t
    use quaff_mass_m, only: mass_t
    use quaff_molar_mass_m, only: molar_mass_t
    use quaff_power_m, only: power_t
    use quaff_pressure_m, only: pressure_t
    use quaff_specific_heat_m, only: specific_heat_t
    use quaff_speed_m, only: speed_t
    use quaff_temperature_m, only: temperature_t
    use quaff_thermal_expansion_coefficient_m, only: thermal_expansion_coefficient_t
    use quaff_time_m, only: time_t
    use quaff_volume_m, only: volume_t

    implicit none
    private
    public :: operator(*), operator(/), operator(-), operator(+), as_burnup

    interface operator(*)
        module procedure acceleration_times_mass
        module procedure acceleration_times_time
        module procedure amount_times_molar_enthalpy
        module procedure amount_times_molar_mass
        module procedure amount_times_molar_specific_heat
        module procedure area_times_length
        module procedure area_times_pressure
        module procedure burnup_times_mass
        module procedure density_times_volume
        module procedure enthalpy_times_mass
        module procedure force_times_length
        module procedure length_times_area
        module procedure length_times_force
        module procedure length_times_length
        module procedure mass_times_acceleration
        module procedure mass_times_burnup
        module procedure mass_times_enthalpy
        module procedure molar_enthalpy_times_amount
        module procedure molar_mass_times_amount
        module procedure molar_specific_heat_times_amount
        module procedure molar_specific_heat_times_temperature
        module procedure power_times_time
        module procedure pressure_times_area
        module procedure pressure_times_time
        module procedure pressure_times_volume
        module procedure speed_times_time
        module procedure temperature_times_molar_specific_heat
        module procedure thermal_expansion_coefficient_times_delta_temperature
        module procedure time_times_acceleration
        module procedure time_times_power
        module procedure time_times_pressure
        module procedure time_times_speed
        module procedure volume_times_density
        module procedure volume_times_pressure
    end interface

    interface operator(/)
        module procedure area_divided_by_length
        module procedure dynamic_viscosity_divided_by_pressure
        module procedure dynamic_viscosity_divided_by_time
        module procedure energy_divided_by_burnup
        module procedure energy_divided_by_energy_per_temperature
        module procedure energy_divided_by_enthalpy
        module procedure energy_divided_by_force
        module procedure energy_divided_by_length
        module procedure energy_divided_by_mass
        module procedure energy_divided_by_molar_enthalpy
        module procedure energy_divided_by_power
        module procedure energy_divided_by_time
        module procedure energy_divided_by_volume
        module procedure force_divided_by_acceleration
        module procedure force_divided_by_area
        module procedure force_divided_by_mass
        module procedure force_divided_by_pressure
        module procedure length_divided_by_speed
        module procedure length_divided_by_time
        module procedure mass_divided_by_amount
        module procedure mass_divided_by_density
        module procedure mass_divided_by_molar_mass
        module procedure mass_divided_by_volume
        module procedure molar_enthalpy_divided_by_molar_mass
        module procedure molar_specific_heat_divided_by_molar_mass
        module procedure speed_divided_by_acceleration
        module procedure speed_divided_by_time
        module procedure volume_divided_by_area
        module procedure volume_divided_by_length
    end interface

    interface operator (-)
          module procedure temperature_minus_temperature
    end interface

    interface operator (+)
      module procedure temperature_plus_delta_temperature
    end interface
contains
    elemental function acceleration_times_mass(acceleration, mass) result(force)
        type(acceleration_t), intent(in) :: acceleration
        type(mass_t), intent(in) :: mass
        type(force_t) :: force

        force%newtons = acceleration%meters_per_square_second * mass%kilograms
    end function

    elemental function acceleration_times_time(acceleration, time) result(speed)
        type(acceleration_t), intent(in) :: acceleration
        type(time_t), intent(in) :: time
        type(speed_t) :: speed

        speed%meters_per_second = acceleration%meters_per_square_second * time%seconds
    end function

    elemental function amount_times_molar_enthalpy(amount, molar_enthalpy) result(energy)
        type(amount_t), intent(in) :: amount
        type(molar_enthalpy_t), intent(in) :: molar_enthalpy
        type(energy_t) :: energy

        energy%joules = amount%mols * molar_enthalpy%joules_per_mol
    end function

    elemental function amount_times_molar_mass(amount, molar_mass) result(mass)
        type(amount_t), intent(in) :: amount
        type(molar_mass_t), intent(in) :: molar_mass
        type(mass_t) :: mass

        mass%kilograms = amount%mols * molar_mass%kilograms_per_mol
    end function

    elemental function amount_times_molar_specific_heat( &
            amount, molar_specific_heat) result(energy_per_temperature)
        type(amount_t), intent(in) :: amount
        type(molar_specific_heat_t), intent(in) :: molar_specific_heat
        type(energy_per_temperature_t) :: energy_per_temperature

        energy_per_temperature%joules_per_kelvin = &
            amount%mols * molar_specific_heat%joules_per_kelvin_mol
    end function

    elemental function area_divided_by_length(numerator, denomenator) result(length)
        type(area_t), intent(in) :: numerator
        type(length_t), intent(in) :: denomenator
        type(length_t) :: length

        length%meters = numerator%square_meters / denomenator%meters
    end function

    elemental function area_times_length(area, length) result(volume)
        type(area_t), intent(in) :: area
        type(length_t), intent(in) :: length
        type(volume_t) :: volume

        volume%cubic_meters = area%square_meters * length%meters
    end function

    elemental function area_times_pressure(area, pressure) result(force)
        type(area_t), intent(in) :: area
        type(pressure_t), intent(in) :: pressure
        type(force_t) :: force

        force%newtons = area%square_meters * pressure%pascals
    end function

    elemental function as_burnup(enthalpy) result(burnup)
        type(enthalpy_t), intent(in) :: enthalpy
        type(burnup_t) :: burnup

        burnup%watt_seconds_per_kilogram = enthalpy%joules_per_kilogram
    end function

    elemental function burnup_times_mass(burnup, mass) result(energy)
        type(burnup_t), intent(in) :: burnup
        type(mass_t), intent(in) :: mass
        type(energy_t) :: energy

        energy%joules = burnup%watt_seconds_per_kilogram * mass%kilograms
    end function

    elemental function density_times_volume(density, volume) result(mass)
        type(density_t), intent(in) :: density
        type(volume_t), intent(in) :: volume
        type(mass_t) :: mass

        mass%kilograms = density%kilograms_per_cubic_meter * volume%cubic_meters
    end function

    elemental function dynamic_viscosity_divided_by_pressure(dynamic_viscosity, pressure) result(time)
        type(dynamic_viscosity_t), intent(in) :: dynamic_viscosity
        type(pressure_t), intent(in) :: pressure
        type(time_t) :: time

        time%seconds = dynamic_viscosity%pascal_seconds / pressure%pascals
    end function

    elemental function dynamic_viscosity_divided_by_time(dynamic_viscosity, time) result(pressure)
        type(dynamic_viscosity_t), intent(in) :: dynamic_viscosity
        type(time_t), intent(in) :: time
        type(pressure_t) :: pressure

        pressure%pascals = dynamic_viscosity%pascal_seconds / time%seconds
    end function

    elemental function energy_divided_by_burnup(energy, burnup) result(mass)
        type(energy_t), intent(in) :: energy
        type(burnup_t), intent(in) :: burnup
        type(mass_t) :: mass

        mass%kilograms = energy%joules / burnup%watt_seconds_per_kilogram
    end function

    elemental function energy_divided_by_energy_per_temperature( &
            energy, energy_per_temperature) result(delta_temperature)
        type(energy_t), intent(in) :: energy
        type(energy_per_temperature_t), intent(in) :: energy_per_temperature
        type(delta_temperature_t) :: delta_temperature

        delta_temperature%delta_kelvin = &
                energy%joules / energy_per_temperature%joules_per_kelvin
    end function

    elemental function energy_divided_by_enthalpy(energy, enthalpy) result(mass)
        type(energy_t), intent(in) :: energy
        type(enthalpy_t), intent(in) :: enthalpy
        type(mass_t) :: mass

        mass%kilograms = energy%joules / enthalpy%joules_per_kilogram
    end function

    elemental function energy_divided_by_force(energy, force) result(length)
        type(energy_t), intent(in) :: energy
        type(force_t), intent(in) :: force
        type(length_t) :: length

        length%meters = energy%joules / force%newtons
    end function

    elemental function energy_divided_by_length(energy, length) result(force)
        type(energy_t), intent(in) :: energy
        type(length_t), intent(in) :: length
        type(force_t) :: force

        force%newtons = energy%joules / length%meters
    end function

    elemental function energy_divided_by_mass(energy, mass) result(enthalpy)
        type(energy_t), intent(in) :: energy
        type(mass_t), intent(in) :: mass
        type(enthalpy_t) :: enthalpy

        enthalpy%joules_per_kilogram = energy%joules / mass%kilograms
    end function

    elemental function energy_divided_by_molar_enthalpy( &
            energy, molar_enthalpy) result(amount)
        type(energy_t), intent(in) :: energy
        type(molar_enthalpy_t), intent(in) :: molar_enthalpy
        type(amount_t) :: amount

        amount%mols = energy%joules / molar_enthalpy%joules_per_mol
    end function

    elemental function energy_divided_by_power(energy, power) result(time)
        type(energy_t), intent(in) :: energy
        type(power_t), intent(in) :: power
        type(time_t) :: time

        time%seconds = energy%joules / power%watts
    end function

    elemental function energy_divided_by_time(energy, time) result(power)
        type(energy_t), intent(in) :: energy
        type(time_t), intent(in) :: time
        type(power_t) :: power

        power%watts = energy%joules / time%seconds
    end function

    elemental function energy_divided_by_volume(energy, volume) result(pressure)
        type(energy_t), intent(in) :: energy
        type(volume_t), intent(in) :: volume
        type(pressure_t) :: pressure

        pressure%pascals = energy%joules / volume%cubic_meters
    end function

    elemental function enthalpy_times_mass(enthalpy, mass) result(energy)
        type(enthalpy_t), intent(in) :: enthalpy
        type(mass_t), intent(in) :: mass
        type(energy_t) :: energy

        energy%joules = enthalpy%joules_per_kilogram * mass%kilograms
    end function

    elemental function force_divided_by_acceleration(force, acceleration) result(mass)
        type(force_t), intent(in) :: force
        type(acceleration_t), intent(in) :: acceleration
        type(mass_t) :: mass

        mass%kilograms = force%newtons / acceleration%meters_per_square_second
    end function

    elemental function force_divided_by_area(force, area) result(pressure)
        type(force_t), intent(in) :: force
        type(area_t), intent(in) :: area
        type(pressure_t) :: pressure

        pressure%pascals = force%newtons / area%square_meters
    end function

    elemental function force_divided_by_mass(force, mass) result(acceleration)
        type(force_t), intent(in) :: force
        type(mass_t), intent(in) :: mass
        type(acceleration_t) :: acceleration

        acceleration%meters_per_square_second = force%newtons / mass%kilograms
    end function

    elemental function force_divided_by_pressure(force, pressure) result(area)
        type(force_t), intent(in) :: force
        type(pressure_t), intent(in) :: pressure
        type(area_t) :: area

        area%square_meters = force%newtons / pressure%pascals
    end function

    elemental function force_times_length(force, length) result(energy)
        type(force_t), intent(in) :: force
        type(length_t), intent(in) :: length
        type(energy_t) :: energy

        energy%joules = force%newtons * length%meters
    end function

    elemental function length_divided_by_speed(length, speed) result(time)
        type(length_t), intent(in) :: length
        type(speed_t), intent(in) :: speed
        type(time_t) :: time

        time%seconds = length%meters / speed%meters_per_second
    end function

    elemental function length_divided_by_time(length, time) result(speed)
        type(length_t), intent(in) :: length
        type(time_t), intent(in) :: time
        type(speed_t) :: speed

        speed%meters_per_second = length%meters / time%seconds
    end function

    elemental function length_times_area(length, area) result(volume)
        type(length_t), intent(in) :: length
        type(area_t), intent(in) :: area
        type(volume_t) :: volume

        volume%cubic_meters = length%meters * area%square_meters
    end function

    elemental function length_times_force(length, force) result(energy)
        type(length_t), intent(in) :: length
        type(force_t), intent(in) :: force
        type(energy_t) :: energy

        energy%joules = length%meters * force%newtons
    end function

    elemental function length_times_length(lhs, rhs) result(area)
        type(length_t), intent(in) :: lhs
        type(length_t), intent(in) :: rhs
        type(area_t) :: area

        area%square_meters = lhs%meters * rhs%meters
    end function

    elemental function mass_divided_by_amount(mass, amount) result(molar_mass)
        type(mass_t), intent(in) :: mass
        type(amount_t), intent(in) :: amount
        type(molar_mass_t) :: molar_mass

        molar_mass%kilograms_per_mol = mass%kilograms / amount%mols
    end function

    elemental function mass_divided_by_density(mass, density) result(volume)
        type(mass_t), intent(in) :: mass
        type(density_t), intent(in) :: density
        type(volume_t) :: volume

        volume%cubic_meters = mass%kilograms / density%kilograms_per_cubic_meter
    end function

    elemental function mass_divided_by_molar_mass(mass, molar_mass) result(amount)
        type(mass_t), intent(in) :: mass
        type(molar_mass_t), intent(in) :: molar_mass
        type(amount_t) :: amount

        amount%mols = mass%kilograms / molar_mass%kilograms_per_mol
    end function

    elemental function mass_divided_by_volume(mass, volume) result(density)
        type(mass_t), intent(in) :: mass
        type(volume_t), intent(in) :: volume
        type(density_t) :: density

        density%kilograms_per_cubic_meter = mass%kilograms / volume%cubic_meters
    end function

    elemental function mass_times_acceleration(mass, acceleration) result(force)
        type(mass_t), intent(in) :: mass
        type(acceleration_t), intent(in) :: acceleration
        type(force_t) :: force

        force%newtons = mass%kilograms * acceleration%meters_per_square_second
    end function

    elemental function mass_times_burnup(mass, burnup) result(energy)
        type(mass_t), intent(in) :: mass
        type(burnup_t), intent(in) :: burnup
        type(energy_t) :: energy

        energy%joules = mass%kilograms * burnup%watt_seconds_per_kilogram
    end function

    elemental function mass_times_enthalpy(mass, enthalpy) result(energy)
        type(mass_t), intent(in) :: mass
        type(enthalpy_t), intent(in) :: enthalpy
        type(energy_t) :: energy

        energy%joules = mass%kilograms * enthalpy%joules_per_kilogram
    end function

    elemental function molar_enthalpy_divided_by_molar_mass( &
            molar_enthalpy, molar_mass) result(enthalpy)
        type(molar_enthalpy_t), intent(in) :: molar_enthalpy
        type(molar_mass_t), intent(in) :: molar_mass
        type(enthalpy_t) :: enthalpy

        enthalpy%joules_per_kilogram = &
                molar_enthalpy%joules_per_mol / molar_mass%kilograms_per_mol
    end function

    elemental function molar_enthalpy_times_amount(molar_enthalpy, amount) result(energy)
        type(molar_enthalpy_t), intent(in) :: molar_enthalpy
        type(amount_t), intent(in) :: amount
        type(energy_t) :: energy

        energy%joules = molar_enthalpy%joules_per_mol * amount%mols
    end function

    elemental function molar_mass_times_amount(molar_mass, amount) result(mass)
        type(molar_mass_t), intent(in) :: molar_mass
        type(amount_t), intent(in) :: amount
        type(mass_t) :: mass

        mass%kilograms = molar_mass%kilograms_per_mol * amount%mols
    end function

    elemental function molar_specific_heat_divided_by_molar_mass( &
            molar_specific_heat, molar_mass) result(specific_heat)
        type(molar_specific_heat_t), intent(in) :: molar_specific_heat
        type(molar_mass_t), intent(in) :: molar_mass
        type(specific_heat_t) :: specific_heat

        specific_heat%joules_per_kilogram_kelvin = &
                molar_specific_heat%joules_per_kelvin_mol &
                / molar_mass%kilograms_per_mol
    end function

    elemental function molar_specific_heat_times_amount( &
            molar_specific_heat, amount) result(energy_per_temperature)
        type(molar_specific_heat_t), intent(in) :: molar_specific_heat
        type(amount_t), intent(in) :: amount
        type(energy_per_temperature_t) :: energy_per_temperature

        energy_per_temperature%joules_per_kelvin = &
            molar_specific_heat%joules_per_kelvin_mol * amount%mols
    end function

    elemental function molar_specific_heat_times_temperature( &
            molar_specific_heat, temperature) result(molar_enthalpy)
        type(molar_specific_heat_t), intent(in) :: molar_specific_heat
        type(temperature_t), intent(in) :: temperature
        type(molar_enthalpy_t) :: molar_enthalpy

        molar_enthalpy%joules_per_mol = &
                molar_specific_heat%joules_per_kelvin_mol * temperature%kelvin
    end function

    elemental function power_times_time(power, time) result(energy)
        type(power_t), intent(in) :: power
        type(time_t), intent(in) :: time
        type(energy_t) :: energy

        energy%joules = power%watts * time%seconds
    end function

    elemental function pressure_times_area(pressure, area) result(force)
        type(pressure_t), intent(in) :: pressure
        type(area_t), intent(in) :: area
        type(force_t) :: force

        force%newtons = pressure%pascals * area%square_meters
    end function

    elemental function pressure_times_time(pressure, time) result(dynamic_viscosity)
        type(pressure_t), intent(in) :: pressure
        type(time_t), intent(in) :: time
        type(dynamic_viscosity_t) :: dynamic_viscosity

        dynamic_viscosity%pascal_seconds = pressure%pascals * time%seconds
    end function

    elemental function pressure_times_volume(pressure, volume) result(energy)
        type(pressure_t), intent(in) :: pressure
        type(volume_t), intent(in) :: volume
        type(energy_t) :: energy

        energy%joules = pressure%pascals * volume%cubic_meters
    end function

    elemental function speed_divided_by_acceleration(speed, acceleration) result(time)
        type(speed_t), intent(in) :: speed
        type(acceleration_t), intent(in) :: acceleration
        type(time_t) :: time

        time%seconds = speed%meters_per_second / acceleration%meters_per_square_second
    end function

    elemental function speed_divided_by_time(speed, time) result(acceleration)
        type(speed_t), intent(in) :: speed
        type(time_t), intent(in) :: time
        type(acceleration_t) :: acceleration

        acceleration%meters_per_square_second = speed%meters_per_second / time%seconds
    end function

    elemental function speed_times_time(speed, time) result(length)
        type(speed_t), intent(in) :: speed
        type(time_t), intent(in) :: time
        type(length_t) :: length

        length%meters = speed%meters_per_second * time%seconds
    end function

    elemental function temperature_times_molar_specific_heat( &
            temperature, molar_specific_heat) result(molar_enthalpy)
        type(temperature_t), intent(in) :: temperature
        type(molar_specific_heat_t), intent(in) :: molar_specific_heat
        type(molar_enthalpy_t) :: molar_enthalpy

        molar_enthalpy%joules_per_mol = &
                temperature%kelvin * molar_specific_heat%joules_per_kelvin_mol
    end function

    elemental function thermal_expansion_coefficient_times_delta_temperature ( &
          thermal_expansion, delta_temperature) result(factor)
      type(thermal_expansion_coefficient_t), intent(in) :: thermal_expansion
      type(delta_temperature_t), intent(in) :: delta_temperature
      double precision :: factor

      factor = thermal_expansion%per_kelvin * delta_temperature%delta_kelvin
    end function

    elemental function time_times_acceleration(time, acceleration) result(speed)
        type(time_t), intent(in) :: time
        type(acceleration_t), intent(in) :: acceleration
        type(speed_t) :: speed

        speed%meters_per_second = time%seconds * acceleration%meters_per_square_second
    end function

    elemental function time_times_power(time, power) result(energy)
        type(time_t), intent(in) :: time
        type(power_t), intent(in) :: power
        type(energy_t) :: energy

        energy%joules = time%seconds * power%watts
    end function

    elemental function time_times_pressure(time, pressure) result(dynamic_viscosity)
        type(time_t), intent(in) :: time
        type(pressure_t), intent(in) :: pressure
        type(dynamic_viscosity_t) :: dynamic_viscosity

        dynamic_viscosity%pascal_seconds = time%seconds * pressure%pascals
    end function

    elemental function time_times_speed(time, speed) result(length)
        type(time_t), intent(in) :: time
        type(speed_t), intent(in) :: speed
        type(length_t) :: length

        length%meters = time%seconds * speed%meters_per_second
    end function

    elemental function volume_divided_by_area(volume, area) result(length)
        type(volume_t), intent(in) :: volume
        type(area_t), intent(in) :: area
        type(length_t) :: length

        length%meters = volume%cubic_meters / area%square_meters
    end function

    elemental function volume_divided_by_length(volume, length) result(area)
        type(volume_t), intent(in) :: volume
        type(length_t), intent(in) :: length
        type(area_t) :: area

        area%square_meters = volume%cubic_meters / length%meters
    end function

    elemental function volume_times_density(volume, density) result(mass)
        type(volume_t), intent(in) :: volume
        type(density_t), intent(in) :: density
        type(mass_t) :: mass

        mass%kilograms = volume%cubic_meters * density%kilograms_per_cubic_meter
    end function

    elemental function volume_times_pressure(volume, pressure) result(energy)
        type(volume_t), intent(in) :: volume
        type(pressure_t), intent(in) :: pressure
        type(energy_t) :: energy

        energy%joules = volume%cubic_meters * pressure%pascals
    end function

    elemental function temperature_minus_temperature(lhs, rhs) result(delta_temperature)
        type(temperature_t), intent(in) :: lhs, rhs
        type(delta_temperature_t) :: delta_temperature

        delta_temperature%delta_kelvin = lhs%kelvin - rhs%kelvin
    end function

    elemental function temperature_plus_delta_temperature(temperature, delta_temperature) result(new_temperature)
        type(temperature_t), intent(in) :: temperature
        type(delta_temperature_t), intent(in) :: delta_temperature
        type(temperature_t) :: new_temperature

        new_temperature%kelvin = temperature%kelvin + delta_temperature%delta_kelvin
    end function
end module
