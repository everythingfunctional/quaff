module non_zero_double_precision_pair_generator_m
    use double_precision_pair_input_m, only: double_precision_pair_input_t
    use veggies, only: &
            double_precision_input_t, &
            generated_t, &
            generator_t, &
            input_t, &
            shrink_result_t, &
            get_random_double_precision_with_magnitude, &
            shrunk_value, &
            simplest_value

    implicit none
    private
    public :: &
            non_zero_double_precision_pair_generator_t, &
            NON_ZERO_DOUBLE_PRECISION_PAIR_GENERATOR

    type, extends(generator_t) :: non_zero_double_precision_pair_generator_t
    contains
        private
        procedure, public :: generate
        procedure, public, nopass :: shrink
    end type

    type(non_zero_double_precision_pair_generator_t) :: &
            NON_ZERO_DOUBLE_PRECISION_PAIR_GENERATOR = &
                    non_zero_double_precision_pair_generator_t()
contains
    function generate(self) result(random_double)
        class(non_zero_double_precision_pair_generator_t), intent(in) :: self
        type(generated_t) :: random_double

        double precision :: first
        double precision :: second_

        associate(unused => self)
        end associate

        do
            first = get_random_double_precision_with_magnitude(1.0d12)
            if (abs(first) >= 1.0d0) exit
        end do
        do
            second_ = get_random_double_precision_with_magnitude(1.0d12)
            if (abs(second_) >= 1.0d0) exit
        end do
        random_double = generated_t(double_precision_pair_input_t(first, second_))
    end function

    function shrink(input) result(shrunk)
        class(input_t), intent(in) :: input
        type(shrink_result_t) :: shrunk

        double precision :: new_first
        double precision :: new_second

        select type (input)
        type is (double_precision_pair_input_t)
            if (abs(input%first()) <= 1.0d0) then
                new_first = input%first() / abs(input%first())
                if (abs(input%second_()) <= 1.0d0) then
                    shrunk = simplest_value(double_precision_pair_input_t( &
                            new_first, input%second_() / abs(input%second_())))
                else
                    shrunk = shrunk_value(double_precision_pair_input_t( &
                            new_first, input%second_() / 2.0d0))
                end if
            else
                new_first = input%first() / 2.0d0
                if (abs(input%second_()) <= 1.0d0) then
                    new_second = input%second_() / abs(input%second_())
                else
                    new_second = input%second_() / 2.0d0
                end if
                shrunk = shrunk_value(double_precision_pair_input_t(new_first, new_second))
            end if
        end select
    end function
end module
