module NonZeroDoublePrecisionGenerator_m
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

    type, public, extends(generator_t) :: NonZeroDoublePrecisiongenerator_t
    contains
        private
        procedure, public :: generate
        procedure, public, nopass :: shrink
    end type NonZeroDoublePrecisiongenerator_t

    type(NonZeroDoublePrecisiongenerator_t), public :: &
            NON_ZERO_DOUBLE_PRECISION_GENERATOR = &
                    NonZeroDoublePrecisiongenerator_t()
contains
    function generate(self) result(random_double)
        class(NonZeroDoublePrecisiongenerator_t), intent(in) :: self
        type(generated_t) :: random_double

        double precision :: value_

        associate(a => self)
        end associate

        do
            value_ = get_random_double_precision_with_magnitude(1.0d12)
            if (abs(value_) >= 1.0d0) exit
        end do
        random_double = generated_t(double_precision_input_t(value_))
    end function generate

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
    end function shrink
end module NonZeroDoublePrecisionGenerator_m
