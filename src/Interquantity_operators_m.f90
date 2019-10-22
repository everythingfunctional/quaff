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
        use Area_m, only: Area_t, SQUARE_METERS
        use Length_m, only: Length_t, operator(.unit.), METERS

        type(Area_t), intent(in) :: numerator
        type(Length_t), intent(in) :: denomenator
        type(Length_t) :: length

        length = ((numerator.in.SQUARE_METERS) / (denomenator.in.METERS)).unit.METERS
    end function areaDividedByLength

    elemental function areaTimesLength(area, length) result(volume)
        use Area_m, only: Area_t, SQUARE_METERS
        use Length_m, only: Length_t, METERS
        use Volume_m, only: Volume_t, operator(.unit.), CUBIC_METERS

        type(Area_t), intent(in) :: area
        type(Length_t), intent(in) :: length
        type(Volume_t) :: volume

        volume = ((area.in.SQUARE_METERS) * (length.in.METERS)).unit.CUBIC_METERS
    end function areaTimesLength

    elemental function densityTimesVolume(density, volume) result(mass)
        use Density_m, only: Density_t, KILOGRAMS_PER_CUBIC_METER
        use Mass_m, only: Mass_t, operator(.unit.), KILOGRAMS
        use Volume_m, only: Volume_t, CUBIC_METERS

        type(Density_t), intent(in) :: density
        type(Volume_t), intent(in) :: volume
        type(Mass_t) :: mass

        mass = ((density.in.KILOGRAMS_PER_CUBIC_METER) * (volume.in.CUBIC_METERS)).unit.KILOGRAMS
    end function densityTimesVolume

    elemental function lengthTimesArea(length, area) result(volume)
        use Area_m, only: Area_t, SQUARE_METERS
        use Length_m, only: Length_t, METERS
        use Volume_m, only: Volume_t, operator(.unit.), CUBIC_METERS

        type(Length_t), intent(in) :: length
        type(Area_t), intent(in) :: area
        type(Volume_t) :: volume

        volume = ((length.in.METERS) * (area.in.SQUARE_METERS)).unit.CUBIC_METERS
    end function lengthTimesArea

    elemental function lengthTimesLength(lhs, rhs) result(area)
        use Area_m, only: Area_t, operator(.unit.), SQUARE_METERS
        use Length_m, only: Length_t, METERS

        type(Length_t), intent(in) :: lhs
        type(Length_t), intent(in) :: rhs
        type(Area_t) :: area

        area = ((lhs.in.METERS) * (rhs.in.METERS)).unit.SQUARE_METERS
    end function lengthTimesLength

    elemental function massDividedByDensity(mass, density) result(volume)
        use Density_m, only: Density_t, KILOGRAMS_PER_CUBIC_METER
        use Mass_m, only: Mass_t, KILOGRAMS
        use Volume_m, only: Volume_t, operator(.unit.), CUBIC_METERS

        type(Mass_t), intent(in) :: mass
        type(Density_t), intent(in) :: density
        type(Volume_t) :: volume

        volume = ((mass.in.KILOGRAMS) / (density.in.KILOGRAMS_PER_CUBIC_METER)).unit.CUBIC_METERS
    end function massDividedByDensity

    elemental function massDividedByVolume(mass, volume) result(density)
        use Density_m, only: Density_t, operator(.unit.), KILOGRAMS_PER_CUBIC_METER
        use Mass_m, only: Mass_t, KILOGRAMS
        use Volume_m, only: Volume_t, CUBIC_METERS

        type(Mass_t), intent(in) :: mass
        type(Volume_t), intent(in) :: volume
        type(Density_t) :: density

        density = ((mass.in.KILOGRAMS) / (volume.in.CUBIC_METERS)).unit.KILOGRAMS_PER_CUBIC_METER
    end function massDividedByVolume

    elemental function volumeDividedByArea(volume, area) result(length)
        use Area_m, only: Area_t, SQUARE_METERS
        use Length_m, only: Length_t, operator(.unit.), METERS
        use Volume_m, only: Volume_t, CUBIC_METERS

        type(Volume_t), intent(in) :: volume
        type(Area_t), intent(in) :: area
        type(Length_t) :: length

        length = ((volume.in.CUBIC_METERS) / (area.in.SQUARE_METERS)).unit.METERS
    end function volumeDividedByArea

    elemental function volumeDividedByLength(volume, length) result(area)
        use Area_m, only: Area_t, operator(.unit.), SQUARE_METERS
        use Length_m, only: Length_t, METERS
        use Volume_m, only: Volume_t, CUBIC_METERS

        type(Volume_t), intent(in) :: volume
        type(Length_t), intent(in) :: length
        type(Area_t) :: area

        area = ((volume.in.CUBIC_METERS) / (length.in.METERS)).unit.SQUARE_METERS
    end function volumeDividedByLength

    elemental function volumeTimesDensity(volume, density) result(mass)
        use Density_m, only: Density_t, KILOGRAMS_PER_CUBIC_METER
        use Mass_m, only: Mass_t, operator(.unit.), KILOGRAMS
        use Volume_m, only: Volume_t, CUBIC_METERS

        type(Volume_t), intent(in) :: volume
        type(Density_t), intent(in) :: density
        type(Mass_t) :: mass

        mass = ((volume.in.CUBIC_METERS) * (density.in.KILOGRAMS_PER_CUBIC_METER)).unit.KILOGRAMS
    end function volumeTimesDensity
end module Interquantity_operators_m
