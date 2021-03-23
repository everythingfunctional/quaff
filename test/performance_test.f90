module performance_test
    use quaff, only: &
            density_t, &
            mass_t, &
            volume_t, &
            operator(/), &
            operator(.unit.), &
            CUBIC_METERS, &
            KILOGRAMS
    use vegetables, only: result_t, test_item_t, assert_faster_than, describe, it

    implicit none
    private
    public :: test_performance
contains
    function test_performance() result(tests)
        type(test_item_t) :: tests

        tests = describe( &
                "mathematical operations between quantities", &
                [ it( &
                        "take no more than twice as long as regular math", &
                        check_interquantity_speed)&
                ])
    end function

    function check_interquantity_speed() result(result_)
        type(result_t) :: result_

        type(density_t) :: density
        double precision :: density_
        type(mass_t) :: mass
        double precision :: mass_
        type(volume_t) :: volume
        double precision :: volume_

        mass_ = 6.0d0
        mass = mass_.unit.KILOGRAMS
        volume_ = 3.0d0
        volume = volume_.unit.CUBIC_METERS

        result_ = assert_faster_than(do_regular_math, do_quantity_math, 100)
    contains
        subroutine do_regular_math
            integer :: i

            do i = 1, 2000
                density_ = mass_ / volume_
            end do
        end subroutine

        subroutine do_quantity_math
            integer :: i

            do i = 1, 1000
                density = mass / volume
            end do
        end subroutine
    end function
end module
