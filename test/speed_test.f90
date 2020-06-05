module speed_test
    use quaff, only: &
            Density_t, &
            Mass_t, &
            Volume_t, &
            operator(/), &
            operator(.unit.), &
            CUBIC_METERS, &
            KILOGRAMS
    use Vegetables_m, only: Result_t, TestItem_t, assertFasterThan, Describe, It

    implicit none
    private

    public :: test_speed
contains
    function test_speed() result(tests)
        type(TestItem_t) :: tests

        type(TestItem_t) :: individual_tests(1)

        individual_tests(1) = It( &
                "take no more than 5 times as long as regular math", &
                checkInterquantitySpeed)
        tests = Describe("Quantity operations", individual_tests)
    end function test_speed

    function checkInterquantitySpeed() result(result_)
        type(Result_t) :: result_

        type(Density_t) :: density
        double precision :: density_
        type(Mass_t) :: mass
        double precision :: mass_
        type(Volume_t) :: volume
        double precision :: volume_

        mass_ = 6.0d0
        mass = mass_.unit.KILOGRAMS
        volume_ = 3.0d0
        volume = volume_.unit.CUBIC_METERS

        result_ = assertFasterThan(doRegularMath, doQuantityMath, 100)
    contains
        subroutine doQuantityMath
            integer :: i

            do i = 1, 100
                density = mass / volume
            end do
        end subroutine doQuantityMath

        subroutine doRegularMath
            integer :: i

            do i = 1, 500
                density_ = mass_ / volume_
            end do
        end subroutine doRegularMath
    end function checkInterquantitySpeed
end module speed_test
