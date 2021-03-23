module double_precision_pair_generator_m
    use double_precision_pair_input_m, only: double_precision_pair_input_t
    use quaff_utilities_m, only: effectively_zero
    use vegetables, only: &
            generated_t, &
            generator_t, &
            input_t, &
            shrink_result_t, &
            get_random_double_precision_with_magnitude, &
            shrunk_value, &
            simplest_value

    implicit none
    private
    public :: double_precision_pair_generator_t, DOUBLE_PRECISION_PAIR_GENERATOR

    type, extends(generator_t) :: double_precision_pair_generator_t
    contains
        private
        procedure, public :: generate
        procedure, public, nopass :: shrink
    end type

    type(double_precision_pair_generator_t) :: DOUBLE_PRECISION_PAIR_GENERATOR = &
            double_precision_pair_generator_t()
contains
    function generate(self) result(random_double)
        class(double_precision_pair_generator_t), intent(in) :: self
        type(generated_t) :: random_double

        associate(unused => self)
        end associate

        random_double = generated_t(double_precision_pair_input_t( &
                get_random_double_precision_with_magnitude(1.0d12), &
                get_random_double_precision_with_magnitude(1.0d12)))
    end function

    function shrink(input) result(shrunk)
        class(input_t), intent(in) :: input
        type(shrink_result_t) :: shrunk

        double precision :: new_first
        double precision :: new_second

        select type (input)
        type is (double_precision_pair_input_t)
            if (effectively_zero(input%first())) then
                new_first = 0.0d0
                if (effectively_zero(input%second_())) then
                    shrunk = simplest_value(double_precision_pair_input_t(new_first, 0.0d0))
                else
                    shrunk = shrunk_value(double_precision_pair_input_t(new_first, input%second_() / 2.0d0))
                end if
            else
                new_first = input%first() / 2.0d0
                if (effectively_zero(input%second_())) then
                    new_second = 0.0d0
                else
                    new_second = input%second_() / 2.0d0
                end if
                shrunk = shrunk_value(double_precision_pair_input_t(new_first, new_second))
            end if
        end select
    end function
end module
