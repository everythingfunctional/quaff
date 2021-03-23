module units_examples_m
    use vegetables, only: example_t

    implicit none
    private
    public :: units_examples_t

    type :: units_examples_t
        private
        type(example_t), allocatable :: units_(:)
        type(example_t), allocatable :: pairs_(:)
    contains
        private
        procedure, public :: units
        procedure, public :: pairs
    end type

    interface units_examples_t
        module procedure constructor
    end interface
contains
    function constructor(units, pairs) result(units_examples)
        type(example_t), intent(in) :: units(:)
        type(example_t), intent(in) :: pairs(:)
        type(units_examples_t) :: units_examples

        allocate(units_examples%units_, source = units)
        allocate(units_examples%pairs_, source = pairs)
    end function

    function units(self)
        class(units_examples_t), intent(in) :: self
        type(example_t), allocatable :: units(:)

        allocate(units, source = self%units_)
    end function

    function pairs(self)
        class(units_examples_t), intent(in) :: self
        type(example_t), allocatable :: pairs(:)

        allocate(pairs, source = self%pairs_)
    end function
end module
