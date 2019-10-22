module Interquantity_operators_m
    implicit none
    private

    interface operator(*)
        module procedure lengthTimesLength
    end interface operator(*)

    interface operator(/)
        module procedure areaDividedByLength
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

    elemental function lengthTimesLength(lhs, rhs) result(area)
        use Area_m, only: Area_t, operator(.unit.), SQUARE_METERS
        use Length_m, only: Length_t, METERS

        type(Length_t), intent(in) :: lhs
        type(Length_t), intent(in) :: rhs
        type(Area_t) :: area

        area = ((lhs.in.METERS) * (rhs.in.METERS)).unit.SQUARE_METERS
    end function lengthTimesLength
end module Interquantity_operators_m
