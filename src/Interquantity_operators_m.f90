module Interquantity_operators_m
    implicit none
    private

    interface operator(*)
        module procedure areaTimesLength
        module procedure lengthTimesArea
        module procedure lengthTimesLength
    end interface operator(*)

    interface operator(/)
        module procedure areaDividedByLength
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

    elemental function volumeDividedByArea(numerator, denomenator) result(length)
        use Area_m, only: Area_t, SQUARE_METERS
        use Length_m, only: Length_t, operator(.unit.), METERS
        use Volume_m, only: Volume_t, CUBIC_METERS

        type(Volume_t), intent(in) :: numerator
        type(Area_t), intent(in) :: denomenator
        type(Length_t) :: length

        length = ((numerator.in.CUBIC_METERS) / (denomenator.in.SQUARE_METERS)).unit.METERS
    end function volumeDividedByArea

    elemental function volumeDividedByLength(numerator, denomenator) result(area)
        use Area_m, only: Area_t, operator(.unit.), SQUARE_METERS
        use Length_m, only: Length_t, METERS
        use Volume_m, only: Volume_t, CUBIC_METERS

        type(Volume_t), intent(in) :: numerator
        type(Length_t), intent(in) :: denomenator
        type(Area_t) :: area

        area = ((numerator.in.CUBIC_METERS) / (denomenator.in.METERS)).unit.SQUARE_METERS
    end function volumeDividedByLength
end module Interquantity_operators_m
