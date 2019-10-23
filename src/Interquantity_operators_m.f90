module Interquantity_operators_m
    implicit none
    private

    interface operator(*)
        module procedure areaTimesLength
        module procedure densityTimesVolume
        module procedure lengthTimesArea
        module procedure lengthTimesLength
        module procedure volumeTimesDensity
    end interface operator(*)

    interface operator(/)
        module procedure areaDividedByLength
        module procedure massDividedByDensity
        module procedure massDividedByVolume
        module procedure volumeDividedByArea
        module procedure volumeDividedByLength
    end interface operator(/)

    public :: operator(*), operator(/)
contains
    elemental function areaDividedByLength(numerator, denomenator) result(length)
        use Area_m, only: Area_t
        use Length_m, only: Length_t

        type(Area_t), intent(in) :: numerator
        type(Length_t), intent(in) :: denomenator
        type(Length_t) :: length

        length%meters = numerator%square_meters / denomenator%meters
    end function areaDividedByLength

    elemental function areaTimesLength(area, length) result(volume)
        use Area_m, only: Area_t
        use Length_m, only: Length_t
        use Volume_m, only: Volume_t

        type(Area_t), intent(in) :: area
        type(Length_t), intent(in) :: length
        type(Volume_t) :: volume

        volume%cubic_meters = area%square_meters * length%meters
    end function areaTimesLength

    elemental function densityTimesVolume(density, volume) result(mass)
        use Density_m, only: Density_t
        use Mass_m, only: Mass_t
        use Volume_m, only: Volume_t

        type(Density_t), intent(in) :: density
        type(Volume_t), intent(in) :: volume
        type(Mass_t) :: mass

        mass%kilograms = density%kilograms_per_cubic_meter * volume%cubic_meters
    end function densityTimesVolume

    elemental function lengthTimesArea(length, area) result(volume)
        use Area_m, only: Area_t
        use Length_m, only: Length_t
        use Volume_m, only: Volume_t

        type(Length_t), intent(in) :: length
        type(Area_t), intent(in) :: area
        type(Volume_t) :: volume

        volume%cubic_meters = length%meters * area%square_meters
    end function lengthTimesArea

    elemental function lengthTimesLength(lhs, rhs) result(area)
        use Area_m, only: Area_t
        use Length_m, only: Length_t

        type(Length_t), intent(in) :: lhs
        type(Length_t), intent(in) :: rhs
        type(Area_t) :: area

        area%square_meters = lhs%meters * rhs%meters
    end function lengthTimesLength

    elemental function massDividedByDensity(mass, density) result(volume)
        use Density_m, only: Density_t
        use Mass_m, only: Mass_t
        use Volume_m, only: Volume_t

        type(Mass_t), intent(in) :: mass
        type(Density_t), intent(in) :: density
        type(Volume_t) :: volume

        volume%cubic_meters = mass%kilograms / density%kilograms_per_cubic_meter
    end function massDividedByDensity

    elemental function massDividedByVolume(mass, volume) result(density)
        use Density_m, only: Density_t
        use Mass_m, only: Mass_t
        use Volume_m, only: Volume_t

        type(Mass_t), intent(in) :: mass
        type(Volume_t), intent(in) :: volume
        type(Density_t) :: density

        density%kilograms_per_cubic_meter = mass%kilograms / volume%cubic_meters
    end function massDividedByVolume

    elemental function volumeDividedByArea(volume, area) result(length)
        use Area_m, only: Area_t
        use Length_m, only: Length_t
        use Volume_m, only: Volume_t

        type(Volume_t), intent(in) :: volume
        type(Area_t), intent(in) :: area
        type(Length_t) :: length

        length%meters = volume%cubic_meters / area%square_meters
    end function volumeDividedByArea

    elemental function volumeDividedByLength(volume, length) result(area)
        use Area_m, only: Area_t
        use Length_m, only: Length_t
        use Volume_m, only: Volume_t

        type(Volume_t), intent(in) :: volume
        type(Length_t), intent(in) :: length
        type(Area_t) :: area

        area%square_meters = volume%cubic_meters / length%meters
    end function volumeDividedByLength

    elemental function volumeTimesDensity(volume, density) result(mass)
        use Density_m, only: Density_t
        use Mass_m, only: Mass_t
        use Volume_m, only: Volume_t

        type(Volume_t), intent(in) :: volume
        type(Density_t), intent(in) :: density
        type(Mass_t) :: mass

        mass%kilograms = volume%cubic_meters * density%kilograms_per_cubic_meter
    end function volumeTimesDensity
end module Interquantity_operators_m
