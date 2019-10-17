module Units_examples_m
    use Units_m, only: Unit_t
    use Vegetables_m, only: Example_t, Input_t

    implicit none
    private

    type, public, extends(Input_t) :: UnitsInput_t
        class(Unit_t), allocatable :: unit
    end type UnitsInput_t

    type, public, extends(Input_t) :: UnitsPairInput_t
        class(Unit_t), allocatable :: first
        class(Unit_t), allocatable :: second
    end type UnitsPairInput_t

    type, public :: UnitsExamples_t
        type(Example_t), allocatable :: units(:)
        type(Example_t), allocatable :: pairs(:)
    end type UnitsExamples_t

    public :: makeUnitsExamples
contains
    function makeUnitsExamples(units) result(examples)
        use Vegetables_m, only: Example

        class(Unit_t), intent(in) :: units(:)
        type(UnitsExamples_t) :: examples

        integer :: i
        integer :: j
        integer :: num_pairs
        integer :: num_units
        type(UnitsPairInput_t) :: pair
        integer :: pair_index
        type(UnitsInput_t) :: input

        num_units = size(units)
        allocate(examples%units(num_units))
        do i = 1, num_units
            allocate(input%unit, source = units(i))
            examples%units(i) = Example(input)
            deallocate(input%unit)
        end do

        num_pairs = combinations(num_units)
        allocate(examples%pairs(num_pairs))
        pair_index = 1
        do i = 1, num_units - 1
            allocate(pair%first, source = units(i))
            do j = i + 1, num_units
                allocate(pair%second, source = units(j))
                examples%pairs(pair_index) = Example(pair)
                pair_index = pair_index + 1
                deallocate(pair%second)
            end do
            deallocate(pair%first)
        end do
    end function makeUnitsExamples

    recursive function combinations(num_items) result(num_combinations)
        integer, intent(in) :: num_items
        integer :: num_combinations

        if (num_items <= 1) then
            num_combinations = 0
        else
            num_combinations = num_items - 1 + combinations(num_items - 1)
        end if
    end function combinations
end module Units_examples_m
