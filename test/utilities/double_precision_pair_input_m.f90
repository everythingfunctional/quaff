module double_precision_pair_input_m
    use veggies, only: input_t

    implicit none
    private
    public :: double_precision_pair_input_t

    type, extends(input_t) :: double_precision_pair_input_t
        private
        double precision :: first_
        double precision :: second__
    contains
        private
        procedure, public :: first
        procedure, public :: second_
    end type

    interface double_precision_pair_input_t
        module procedure constructor
    end interface
contains
    pure function constructor(first, second_) result(double_precision_pair_input)
        double precision, intent(in) :: first
        double precision, intent(in) :: second_
        type(double_precision_pair_input_t) :: double_precision_pair_input

        double_precision_pair_input%first_ = first
        double_precision_pair_input%second__ = second_
    end function

    pure function first(self)
        class(double_precision_pair_input_t), intent(in) :: self
        double precision :: first

        first = self%first_
    end function

    pure function second_(self)
        class(double_precision_pair_input_t), intent(in) :: self
        double precision :: second_

        second_ = self%second__
    end function
end module
