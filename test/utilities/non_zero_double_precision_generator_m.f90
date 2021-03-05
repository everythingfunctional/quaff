module non_zero_double_precision_generator_m
    use vegetables, only: &
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

    type, public, extends(generator_t) :: non_zero_double_precision_generator_t
    contains
        private
        procedure, public :: generate
        procedure, public, nopass :: shrink
    end type

    type(non_zero_double_precision_generator_t), public :: &
            NON_ZERO_DOUBLE_PRECISION_GENERATOR = &
                    non_zero_double_precision_generator_t()
contains
    function generate(self) result(random_double)
        class(non_zero_double_precision_generator_t), intent(in) :: self
        type(generated_t) :: random_double

        double precision :: value_

        associate(unused => self)
        end associate

        do
            value_ = get_random_double_precision_with_magnitude(1.0d12)
            if (abs(value_) >= 1.0d0) exit
        end do
        random_double = generated_t(double_precision_input_t(value_))
    end function

    function shrink(input) result(shrunk)
        class(input_t), intent(in) :: input
        type(shrink_result_t) :: shrunk

        select type (input)
        type is (double_precision_input_t)
            if (abs(input%input()) <= 1.0d0) then
                shrunk = simplest_value(double_precision_input_t(input%input() / abs(input%input())))
            else
                shrunk = shrunk_value(double_precision_input_t(input%input() / 2.0d0))
            end if
        end select
    end function
end module
