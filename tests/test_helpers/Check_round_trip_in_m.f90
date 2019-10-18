module Check_round_trip_in_m
    implicit none
    private

    interface checkRoundTripIn
        module procedure checkRoundTripInQuantityCamel
        module procedure checkRoundTripInLength
        module procedure checkRoundTripInMass
        module procedure checkRoundTripInTemperature
    end interface checkRoundTripIn

    double precision, parameter :: ROUND_TRIP_TEST_VALUES(*) = &
            [-2.0d0, -1.0d0, 0.0d0, 1.0d0, 2.0d0]

    public :: checkRoundTripIn
contains
    function checkRoundTripInQuantityCamel(units) result(result_)
        use Quantity_module_m, only: &
                QuantityCamel_t, QuantityCamelUnit_t, operator(.unit.)
        use Vegetables_m, only: Result_t, assertEquals

        type(QuantityCamelUnit_t), intent(in) :: units
        type(Result_t) :: result_

        integer :: i
        type(QuantityCamel_t) :: intermediate

        do i = 1, size(ROUND_TRIP_TEST_VALUES)
            intermediate = ROUND_TRIP_TEST_VALUES(i).unit.units
            result_ = result_.and.assertEquals( &
                    ROUND_TRIP_TEST_VALUES(i), &
                    intermediate.in.units, &
                    units%toString())
        end do
    end function checkRoundTripInQuantityCamel

    function checkRoundTripInLength(units) result(result_)
        use Length_m, only: &
                Length_t, LengthUnit_t, operator(.unit.)
        use Vegetables_m, only: Result_t, assertEquals

        type(LengthUnit_t), intent(in) :: units
        type(Result_t) :: result_

        integer :: i
        type(Length_t) :: intermediate

        do i = 1, size(ROUND_TRIP_TEST_VALUES)
            intermediate = ROUND_TRIP_TEST_VALUES(i).unit.units
            result_ = result_.and.assertEquals( &
                    ROUND_TRIP_TEST_VALUES(i), &
                    intermediate.in.units, &
                    units%toString())
        end do
    end function checkRoundTripInLength

    function checkRoundTripInMass(units) result(result_)
        use Mass_m, only: Mass_t, MassUnit_t, operator(.unit.)
        use Vegetables_m, only: Result_t, assertEquals

        type(MassUnit_t), intent(in) :: units
        type(Result_t) :: result_

        integer :: i
        type(Mass_t) :: intermediate

        do i = 1, size(ROUND_TRIP_TEST_VALUES)
            intermediate = ROUND_TRIP_TEST_VALUES(i).unit.units
            result_ = result_.and.assertEquals( &
                    ROUND_TRIP_TEST_VALUES(i), &
                    intermediate.in.units, &
                    units%toString())
        end do
    end function checkRoundTripInMass

    function checkRoundTripInTemperature(units) result(result_)
        use Temperature_m, only: Temperature_t, TemperatureUnit_t, operator(.unit.)
        use Vegetables_m, only: Result_t, assertEquals

        type(TemperatureUnit_t), intent(in) :: units
        type(Result_t) :: result_

        integer :: i
        type(Temperature_t) :: intermediate

        do i = 1, size(ROUND_TRIP_TEST_VALUES)
            intermediate = ROUND_TRIP_TEST_VALUES(i).unit.units
            result_ = result_.and.assertEquals( &
                    ROUND_TRIP_TEST_VALUES(i), &
                    intermediate.in.units, &
                    units%toString())
        end do
    end function checkRoundTripInTemperature
end module Check_round_trip_in_m
