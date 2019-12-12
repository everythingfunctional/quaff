module Interquantity_operators_m
    use Area_m, only: Area_t
    use Density_m, only: Density_t
    use Length_m, only: Length_t
    use Mass_m, only: Mass_t
    use Speed_m, only: Speed_t
    use Time_m, only: Time_t
    use Volume_m, only: Volume_t

    implicit none
    private

    interface operator(*)
        module procedure areaTimesLength
        module procedure densityTimesVolume
        module procedure lengthTimesArea
        module procedure lengthTimesLength
        module procedure speedTimesTime
        module procedure timeTimesSpeed
        module procedure volumeTimesDensity
    end interface operator(*)

    interface operator(/)
        module procedure areaDividedByLength
        module procedure lengthDividedBySpeed
        module procedure lengthDividedByTime
        module procedure massDividedByDensity
        module procedure massDividedByVolume
        module procedure volumeDividedByArea
        module procedure volumeDividedByLength
    end interface operator(/)

    public :: operator(*), operator(/)
contains
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

    elemental function densityTimesVolume(density, volume) result(mass)
        type(Density_t), intent(in) :: density
        type(Volume_t), intent(in) :: volume
        type(Mass_t) :: mass

        mass%kilograms = density%kilograms_per_cubic_meter * volume%cubic_meters
    end function densityTimesVolume

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

    elemental function lengthTimesLength(lhs, rhs) result(area)
        type(Length_t), intent(in) :: lhs
        type(Length_t), intent(in) :: rhs
        type(Area_t) :: area

        area%square_meters = lhs%meters * rhs%meters
    end function lengthTimesLength

    elemental function massDividedByDensity(mass, density) result(volume)
        type(Mass_t), intent(in) :: mass
        type(Density_t), intent(in) :: density
        type(Volume_t) :: volume

        volume%cubic_meters = mass%kilograms / density%kilograms_per_cubic_meter
    end function massDividedByDensity

    elemental function massDividedByVolume(mass, volume) result(density)
        type(Mass_t), intent(in) :: mass
        type(Volume_t), intent(in) :: volume
        type(Density_t) :: density

        density%kilograms_per_cubic_meter = mass%kilograms / volume%cubic_meters
    end function massDividedByVolume

    elemental function speedTimesTime(speed, time) result(length)
        type(Speed_t), intent(in) :: speed
        type(Time_t), intent(in) :: time
        type(Length_t) :: length

        length%meters = speed%meters_per_second * time%seconds
    end function speedTimesTime

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
end module Interquantity_operators_m
