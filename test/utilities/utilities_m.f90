module test_utilities_m
    implicit none
    private
    public :: combinations
contains
    pure recursive function combinations(num_items) result(num_combinations)
        integer, intent(in) :: num_items
        integer :: num_combinations

        if (num_items <= 1) then
            num_combinations = 0
        else
            num_combinations = num_items - 1 + combinations(num_items - 1)
        end if
    end function
end module
