module force_utilities_m
    use quaff, only: force_unit_t
    use test_utilities_m, only: combinations
    use units_examples_m, only: units_examples_t
    use vegetables, only: example_t, input_t

    implicit none
    private
    public :: units_input_t, units_pair_input_t, make_units_examples

    type, extends(input_t) :: units_input_t
        private
        class(force_unit_t), allocatable :: unit_
    contains
        private
        procedure, public :: unit
    end type

    type, extends(input_t) :: units_pair_input_t
        private
        class(force_unit_t), allocatable :: first_
        class(force_unit_t), allocatable :: second__
    contains
        private
        procedure, public :: first
        procedure, public :: second_
    end type

    interface units_input_t
        module procedure units_input_constructor
    end interface

    interface units_pair_input_t
        module procedure units_pair_input_constructor
    end interface
contains
    function units_input_constructor(unit) result(units_input)
        class(force_unit_t), intent(in) :: unit
        type(units_input_t) :: units_input

        allocate(units_input%unit_, source = unit)
    end function

    function unit(self)
        class(units_input_t), intent(in) :: self
        class(force_unit_t), allocatable :: unit

        allocate(unit, source = self%unit_)
    end function

    function units_pair_input_constructor(first, second_) result(units_pair_input)
        class(force_unit_t), intent(in) :: first
        class(force_unit_t), intent(in) :: second_
        type(units_pair_input_t) :: units_pair_input

        allocate(units_pair_input%first_, source = first)
        allocate(units_pair_input%second__, source = second_)
    end function

    function first(self)
        class(units_pair_input_t), intent(in) :: self
        class(force_unit_t), allocatable :: first

        allocate(first, source = self%first_)
    end function

    function second_(self)
        class(units_pair_input_t), intent(in) :: self
        class(force_unit_t), allocatable :: second_

        allocate(second_, source = self%second__)
    end function

    function make_units_examples(units) result(units_examples)
        class(force_unit_t), intent(in) :: units(:)
        type(units_examples_t) :: units_examples

        integer :: i
        integer :: j
        integer :: num_pairs
        type(example_t), allocatable :: pair_examples(:)
        integer :: pair_index
        type(example_t) :: single_examples(size(units))

        single_examples = [(example_t(units_input_t(units(i))), i = 1, size(units))]
        num_pairs = combinations(size(units))
        allocate(pair_examples(num_pairs))
        pair_index = 1
        do i = 1, size(units) - 1
            do j = i + 1, size(units)
                pair_examples(pair_index) = example_t(units_pair_input_t( &
                        units(i), units(j)))
                pair_index = pair_index + 1
            end do
        end do
        units_examples = units_examples_t(single_examples, pair_examples)
    end function
end module
