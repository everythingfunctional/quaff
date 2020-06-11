module quaff_Interquantity_operators_m
    use Acceleration_m, only: Acceleration_t
    use Amount_m, only: Amount_t
    use Area_m, only: Area_t
    use Burnup_m, only: Burnup_t
    use Density_m, only: Density_t
    use Dynamic_viscosity_m, only: DynamicViscosity_t
    use Energy_m, only: Energy_t
    use Energy_per_amount_m, only: EnergyPerAmount_t
    use Energy_per_temperature_amount_m, only: EnergyPerTemperatureAmount_t
    use Enthalpy_m, only: Enthalpy_t
    use Force_m, only: Force_t
    use Length_m, only: Length_t
    use Mass_m, only: Mass_t
    use Molar_mass_m, only: MolarMass_t
    use Power_m, only: Power_t
    use Pressure_m, only: Pressure_t
    use Speed_m, only: Speed_t
    use Temperature_m, only: Temperature_t
    use Time_m, only: Time_t
    use Volume_m, only: Volume_t

    implicit none
    private

    interface operator(*)
        module procedure accelerationTimesMass
        module procedure accelerationTimesTime
        module procedure amountTimesMolarMass
        module procedure areaTimesLength
        module procedure areaTimesPressure
        module procedure burnupTimesMass
        module procedure densityTimesVolume
        module procedure energyPerTemperatureAmountTimesTemperature
        module procedure enthalpyTimesMass
        module procedure forceTimesLength
        module procedure lengthTimesArea
        module procedure lengthTimesForce
        module procedure lengthTimesLength
        module procedure massTimesAcceleration
        module procedure massTimesBurnup
        module procedure massTimesEnthalpy
        module procedure molarMassTimesAmount
        module procedure powerTimesTime
        module procedure pressureTimesArea
        module procedure pressureTimesTime
        module procedure pressureTimesVolume
        module procedure speedTimesTime
        module procedure temperatureTimesEnergyPerTemperatureAmount
        module procedure timeTimesAcceleration
        module procedure timeTimesPower
        module procedure timeTimesPressure
        module procedure timeTimesSpeed
        module procedure volumeTimesDensity
        module procedure volumeTimesPressure
    end interface operator(*)

    interface operator(/)
        module procedure areaDividedByLength
        module procedure dynamicViscosityDividedByPressure
        module procedure dynamicViscosityDividedByTime
        module procedure energyDividedByBurnup
        module procedure energyDividedByEnergyPerAmount
        module procedure energyDividedByEnthalpy
        module procedure energyDividedByForce
        module procedure energyDividedByLength
        module procedure energyDividedByMass
        module procedure energyDividedByPower
        module procedure energyDividedByTime
        module procedure forceDividedByAcceleration
        module procedure forceDividedByArea
        module procedure forceDividedByMass
        module procedure forceDividedByPressure
        module procedure lengthDividedBySpeed
        module procedure lengthDividedByTime
        module procedure massDividedByAmount
        module procedure massDividedByDensity
        module procedure massDividedByMolarMass
        module procedure massDividedByVolume
        module procedure speedDividedByAcceleration
        module procedure speedDividedByTime
        module procedure volumeDividedByArea
        module procedure volumeDividedByLength
    end interface operator(/)

    public :: operator(*), operator(/), asBurnup
contains
    elemental function accelerationTimesMass(acceleration, mass) result(force)
        type(Acceleration_t), intent(in) :: acceleration
        type(Mass_t), intent(in) :: mass
        type(Force_t) :: force

        force%newtons = acceleration%meters_per_square_second * mass%kilograms
    end function accelerationTimesMass

    elemental function accelerationTimesTime(acceleration, time) result(speed)
        type(Acceleration_t), intent(in) :: acceleration
        type(Time_t), intent(in) :: time
        type(Speed_t) :: speed

        speed%meters_per_second = acceleration%meters_per_square_second * time%seconds
    end function accelerationTimesTime

    elemental function amountTimesMolarMass(amount, molar_mass) result(mass)
        type(Amount_t), intent(in) :: amount
        type(MolarMass_t), intent(in) :: molar_mass
        type(Mass_t) :: mass

        mass%kilograms = amount%mols * molar_mass%kilograms_per_mol
    end function amountTimesMolarMass

    elemental function areaDividedByLength(numerator, denomenator) result(length)
        type(Area_t), intent(in) :: numerator
        type(Length_t), intent(in) :: denomenator
        type(Length_t) :: length

        length%meters = numerator%square_meters / denomenator%meters
    end function areaDividedByLength

    elemental function areaTimesLength(area, length) result(volume)
        type(Area_t), intent(in) :: area
        type(Length_t), intent(in) :: length
        type(Volume_t) :: volume

        volume%cubic_meters = area%square_meters * length%meters
    end function areaTimesLength

    elemental function areaTimesPressure(area, pressure) result(force)
        type(Area_t), intent(in) :: area
        type(Pressure_t), intent(in) :: pressure
        type(Force_t) :: force

        force%newtons = area%square_meters * pressure%pascals
    end function areaTimesPressure

    elemental function asBurnup(enthalpy) result(burnup)
        type(Enthalpy_t), intent(in) :: enthalpy
        type(Burnup_t) :: burnup

        burnup%watt_seconds_per_kilogram = enthalpy%joules_per_kilogram
    end function asBurnup

    elemental function burnupTimesMass(burnup, mass) result(energy)
        type(Burnup_t), intent(in) :: burnup
        type(Mass_t), intent(in) :: mass
        type(Energy_t) :: energy

        energy%joules = burnup%watt_seconds_per_kilogram * mass%kilograms
    end function burnupTimesMass

    elemental function densityTimesVolume(density, volume) result(mass)
        type(Density_t), intent(in) :: density
        type(Volume_t), intent(in) :: volume
        type(Mass_t) :: mass

        mass%kilograms = density%kilograms_per_cubic_meter * volume%cubic_meters
    end function densityTimesVolume

    elemental function dynamicViscosityDividedByPressure(dynamic_viscosity, pressure) result(time)
        type(DynamicViscosity_t), intent(in) :: dynamic_viscosity
        type(Pressure_t), intent(in) :: pressure
        type(Time_t) :: time

        time%seconds = dynamic_viscosity%pascal_seconds / pressure%pascals
    end function dynamicViscosityDividedByPressure

    elemental function dynamicViscosityDividedByTime(dynamic_viscosity, time) result(pressure)
        type(DynamicViscosity_t), intent(in) :: dynamic_viscosity
        type(Time_t), intent(in) :: time
        type(Pressure_t) :: pressure

        pressure%pascals = dynamic_viscosity%pascal_seconds / time%seconds
    end function dynamicViscosityDividedByTime

    elemental function energyDividedByBurnup(energy, burnup) result(mass)
        type(Energy_t), intent(in) :: energy
        type(Burnup_t), intent(in) :: burnup
        type(Mass_t) :: mass

        mass%kilograms = energy%joules / burnup%watt_seconds_per_kilogram
    end function energyDividedByBurnup

    elemental function energyDividedByEnergyPerAmount(energy, energy_per_amount) result(amount)
        type(Energy_t), intent(in) :: energy
        type(EnergyPerAmount_t), intent(in) :: energy_per_amount
        type(Amount_t) :: amount

        amount%mols = energy%joules / energy_per_amount%joules_per_mol
    end function energyDividedByEnergyPerAmount

    elemental function energyDividedByEnthalpy(energy, enthalpy) result(mass)
        type(Energy_t), intent(in) :: energy
        type(Enthalpy_t), intent(in) :: enthalpy
        type(Mass_t) :: mass

        mass%kilograms = energy%joules / enthalpy%joules_per_kilogram
    end function energyDividedByEnthalpy

    elemental function energyDividedByForce(energy, force) result(length)
        type(Energy_t), intent(in) :: energy
        type(Force_t), intent(in) :: force
        type(Length_t) :: length

        length%meters = energy%joules / force%newtons
    end function energyDividedByForce

    elemental function energyDividedByLength(energy, length) result(force)
        type(Energy_t), intent(in) :: energy
        type(Length_t), intent(in) :: length
        type(Force_t) :: force

        force%newtons = energy%joules / length%meters
    end function energyDividedByLength

    elemental function energyDividedByMass(energy, mass) result(enthalpy)
        type(Energy_t), intent(in) :: energy
        type(Mass_t), intent(in) :: mass
        type(Enthalpy_t) :: enthalpy

        enthalpy%joules_per_kilogram = energy%joules / mass%kilograms
    end function energyDividedByMass

    elemental function energyDividedByPower(energy, power) result(time)
        type(Energy_t), intent(in) :: energy
        type(Power_t), intent(in) :: power
        type(Time_t) :: time

        time%seconds = energy%joules / power%watts
    end function energyDividedByPower

    elemental function energyDividedByTime(energy, time) result(power)
        type(Energy_t), intent(in) :: energy
        type(Time_t), intent(in) :: time
        type(Power_t) :: power

        power%watts = energy%joules / time%seconds
    end function energyDividedByTime

    elemental function energyPerTemperatureAmountTimesTemperature( &
            energy_per_temperature_amount, temperature) result(energy_per_amount)
        type(EnergyPerTemperatureAmount_t), intent(in) :: energy_per_temperature_amount
        type(Temperature_t), intent(in) :: temperature
        type(EnergyPerAmount_t) :: energy_per_amount

        energy_per_amount%joules_per_mol = &
                energy_per_temperature_amount%joules_per_kelvin_mol * temperature%kelvin
    end function energyPerTemperatureAmountTimesTemperature

    elemental function enthalpyTimesMass(enthalpy, mass) result(energy)
        type(Enthalpy_t), intent(in) :: enthalpy
        type(Mass_t), intent(in) :: mass
        type(Energy_t) :: energy

        energy%joules = enthalpy%joules_per_kilogram * mass%kilograms
    end function enthalpyTimesMass

    elemental function forceDividedByAcceleration(force, acceleration) result(mass)
        type(Force_t), intent(in) :: force
        type(Acceleration_t), intent(in) :: acceleration
        type(Mass_t) :: mass

        mass%kilograms = force%newtons / acceleration%meters_per_square_second
    end function forceDividedByAcceleration

    elemental function forceDividedByArea(force, area) result(pressure)
        type(Force_t), intent(in) :: force
        type(Area_t), intent(in) :: area
        type(Pressure_t) :: pressure

        pressure%pascals = force%newtons / area%square_meters
    end function forceDividedByArea

    elemental function forceDividedByMass(force, mass) result(acceleration)
        type(Force_t), intent(in) :: force
        type(Mass_t), intent(in) :: mass
        type(Acceleration_t) :: acceleration

        acceleration%meters_per_square_second = force%newtons / mass%kilograms
    end function forceDividedByMass

    elemental function forceDividedByPressure(force, pressure) result(area)
        type(Force_t), intent(in) :: force
        type(Pressure_t), intent(in) :: pressure
        type(Area_t) :: area

        area%square_meters = force%newtons / pressure%pascals
    end function forceDividedByPressure

    elemental function forceTimesLength(force, length) result(energy)
        type(Force_t), intent(in) :: force
        type(Length_t), intent(in) :: length
        type(Energy_t) :: energy

        energy%joules = force%newtons * length%meters
    end function forceTimesLength

    elemental function lengthDividedBySpeed(length, speed) result(time)
        type(Length_t), intent(in) :: length
        type(Speed_t), intent(in) :: speed
        type(Time_t) :: time

        time%seconds = length%meters / speed%meters_per_second
    end function lengthDividedBySpeed

    elemental function lengthDividedByTime(length, time) result(speed)
        type(Length_t), intent(in) :: length
        type(Time_t), intent(in) :: time
        type(Speed_t) :: speed

        speed%meters_per_second = length%meters / time%seconds
    end function lengthDividedByTime

    elemental function lengthTimesArea(length, area) result(volume)
        type(Length_t), intent(in) :: length
        type(Area_t), intent(in) :: area
        type(Volume_t) :: volume

        volume%cubic_meters = length%meters * area%square_meters
    end function lengthTimesArea

    elemental function lengthTimesForce(length, force) result(energy)
        type(Length_t), intent(in) :: length
        type(Force_t), intent(in) :: force
        type(Energy_t) :: energy

        energy%joules = length%meters * force%newtons
    end function lengthTimesForce

    elemental function lengthTimesLength(lhs, rhs) result(area)
        type(Length_t), intent(in) :: lhs
        type(Length_t), intent(in) :: rhs
        type(Area_t) :: area

        area%square_meters = lhs%meters * rhs%meters
    end function lengthTimesLength

    elemental function massDividedByAmount(mass, amount) result(molar_mass)
        type(Mass_t), intent(in) :: mass
        type(Amount_t), intent(in) :: amount
        type(MolarMass_t) :: molar_mass

        molar_mass%kilograms_per_mol = mass%kilograms / amount%mols
    end function massDividedByAmount

    elemental function massDividedByDensity(mass, density) result(volume)
        type(Mass_t), intent(in) :: mass
        type(Density_t), intent(in) :: density
        type(Volume_t) :: volume

        volume%cubic_meters = mass%kilograms / density%kilograms_per_cubic_meter
    end function massDividedByDensity

    elemental function massDividedByMolarMass(mass, molar_mass) result(amount)
        type(Mass_t), intent(in) :: mass
        type(MolarMass_t), intent(in) :: molar_mass
        type(Amount_t) :: amount

        amount%mols = mass%kilograms / molar_mass%kilograms_per_mol
    end function massDividedByMolarMass

    elemental function massDividedByVolume(mass, volume) result(density)
        type(Mass_t), intent(in) :: mass
        type(Volume_t), intent(in) :: volume
        type(Density_t) :: density

        density%kilograms_per_cubic_meter = mass%kilograms / volume%cubic_meters
    end function massDividedByVolume

    elemental function massTimesAcceleration(mass, acceleration) result(force)
        type(Mass_t), intent(in) :: mass
        type(Acceleration_t), intent(in) :: acceleration
        type(Force_t) :: force

        force%newtons = mass%kilograms * acceleration%meters_per_square_second
    end function massTimesAcceleration

    elemental function massTimesBurnup(mass, burnup) result(energy)
        type(Mass_t), intent(in) :: mass
        type(Burnup_t), intent(in) :: burnup
        type(Energy_t) :: energy

        energy%joules = mass%kilograms * burnup%watt_seconds_per_kilogram
    end function massTimesBurnup

    elemental function massTimesEnthalpy(mass, enthalpy) result(energy)
        type(Mass_t), intent(in) :: mass
        type(Enthalpy_t), intent(in) :: enthalpy
        type(Energy_t) :: energy

        energy%joules = mass%kilograms * enthalpy%joules_per_kilogram
    end function massTimesEnthalpy

    elemental function molarMassTimesAmount(molar_mass, amount) result(mass)
        type(MolarMass_t), intent(in) :: molar_mass
        type(Amount_t), intent(in) :: amount
        type(Mass_t) :: mass

        mass%kilograms = molar_mass%kilograms_per_mol * amount%mols
    end function molarMassTimesAmount

    elemental function powerTimesTime(power, time) result(energy)
        type(Power_t), intent(in) :: power
        type(Time_t), intent(in) :: time
        type(Energy_t) :: energy

        energy%joules = power%watts * time%seconds
    end function powerTimesTime

    elemental function pressureTimesArea(pressure, area) result(force)
        type(Pressure_t), intent(in) :: pressure
        type(Area_t), intent(in) :: area
        type(Force_t) :: force

        force%newtons = pressure%pascals * area%square_meters
    end function pressureTimesArea

    elemental function pressureTimesTime(pressure, time) result(dynamic_viscosity)
        type(Pressure_t), intent(in) :: pressure
        type(Time_t), intent(in) :: time
        type(DynamicViscosity_t) :: dynamic_viscosity

        dynamic_viscosity%pascal_seconds = pressure%pascals * time%seconds
    end function pressureTimesTime

    elemental function pressureTimesVolume(pressure, volume) result(energy)
        type(Pressure_t), intent(in) :: pressure
        type(Volume_t), intent(in) :: volume
        type(Energy_t) :: energy

        energy%joules = pressure%pascals * volume%cubic_meters
    end function pressureTimesVolume

    elemental function speedDividedByAcceleration(speed, acceleration) result(time)
        type(Speed_t), intent(in) :: speed
        type(Acceleration_t), intent(in) :: acceleration
        type(Time_t) :: time

        time%seconds = speed%meters_per_second / acceleration%meters_per_square_second
    end function speedDividedByAcceleration

    elemental function speedDividedByTime(speed, time) result(acceleration)
        type(Speed_t), intent(in) :: speed
        type(Time_t), intent(in) :: time
        type(Acceleration_t) :: acceleration

        acceleration%meters_per_square_second = speed%meters_per_second / time%seconds
    end function speedDividedByTime

    elemental function speedTimesTime(speed, time) result(length)
        type(Speed_t), intent(in) :: speed
        type(Time_t), intent(in) :: time
        type(Length_t) :: length

        length%meters = speed%meters_per_second * time%seconds
    end function speedTimesTime

    elemental function temperatureTimesEnergyPerTemperatureAmount( &
            temperature, energy_per_temperature_amount) result(energy_per_amount)
        type(Temperature_t), intent(in) :: temperature
        type(EnergyPerTemperatureAmount_t), intent(in) :: energy_per_temperature_amount
        type(EnergyPerAmount_t) :: energy_per_amount

        energy_per_amount%joules_per_mol = &
                temperature%kelvin * energy_per_temperature_amount%joules_per_kelvin_mol
    end function temperatureTimesEnergyPerTemperatureAmount

    elemental function timeTimesAcceleration(time, acceleration) result(speed)
        type(Time_t), intent(in) :: time
        type(Acceleration_t), intent(in) :: acceleration
        type(Speed_t) :: speed

        speed%meters_per_second = time%seconds * acceleration%meters_per_square_second
    end function timeTimesAcceleration

    elemental function timeTimesPower(time, power) result(energy)
        type(Time_t), intent(in) :: time
        type(Power_t), intent(in) :: power
        type(Energy_t) :: energy

        energy%joules = time%seconds * power%watts
    end function timeTimesPower

    elemental function timeTimesPressure(time, pressure) result(dynamic_viscosity)
        type(Time_t), intent(in) :: time
        type(Pressure_t), intent(in) :: pressure
        type(DynamicViscosity_t) :: dynamic_viscosity

        dynamic_viscosity%pascal_seconds = time%seconds * pressure%pascals
    end function timeTimesPressure

    elemental function timeTimesSpeed(time, speed) result(length)
        type(Time_t), intent(in) :: time
        type(Speed_t), intent(in) :: speed
        type(Length_t) :: length

        length%meters = time%seconds * speed%meters_per_second
    end function timeTimesSpeed

    elemental function volumeDividedByArea(volume, area) result(length)
        type(Volume_t), intent(in) :: volume
        type(Area_t), intent(in) :: area
        type(Length_t) :: length

        length%meters = volume%cubic_meters / area%square_meters
    end function volumeDividedByArea

    elemental function volumeDividedByLength(volume, length) result(area)
        type(Volume_t), intent(in) :: volume
        type(Length_t), intent(in) :: length
        type(Area_t) :: area

        area%square_meters = volume%cubic_meters / length%meters
    end function volumeDividedByLength

    elemental function volumeTimesDensity(volume, density) result(mass)
        type(Volume_t), intent(in) :: volume
        type(Density_t), intent(in) :: density
        type(Mass_t) :: mass

        mass%kilograms = volume%cubic_meters * density%kilograms_per_cubic_meter
    end function volumeTimesDensity

    elemental function volumeTimesPressure(volume, pressure) result(energy)
        type(Volume_t), intent(in) :: volume
        type(Pressure_t), intent(in) :: pressure
        type(Energy_t) :: energy

        energy%joules = volume%cubic_meters * pressure%pascals
    end function volumeTimesPressure
end module quaff_Interquantity_operators_m
