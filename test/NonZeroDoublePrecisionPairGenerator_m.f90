module NonZeroDoublePrecisionPairGenerator_m
    use DoublePrecisionPairGenerator_m, only: DoublePrecisionPairInput_t
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

    type, public, extends(generator_t) :: NonZeroDoublePrecisionPairGenerator_t
    contains
        private
        procedure, public :: generate
        procedure, public, nopass :: shrink
    end type NonZeroDoublePrecisionPairGenerator_t

    type(NonZeroDoublePrecisionPairGenerator_t), public :: &
            NON_ZERO_DOUBLE_PRECISION_PAIR_GENERATOR = &
                    NonZeroDoublePrecisionPairGenerator_t()
contains
    function generate(self) result(random_double)
        class(NonZeroDoublePrecisionPairGenerator_t), intent(in) :: self
        type(generated_t) :: random_double

        type(DoublePrecisionPairInput_t) :: the_input

        associate(a => self)
        end associate

        do
            the_input%first = get_random_double_precision_with_magnitude(1.0d12)
            if (abs(the_input%first) >= 1.0d0) exit
        end do
        do
            the_input%second = get_random_double_precision_with_magnitude(1.0d12)
            if (abs(the_input%second) >= 1.0d0) exit
        end do
        random_double = generated_t(the_input)
    end function generate

    function shrink(input) result(shrunk)
        class(input_t), intent(in) :: input
        type(shrink_result_t) :: shrunk

        type(DoublePrecisionPairInput_t) :: new_input

        select type (input)
        type is (DoublePrecisionPairInput_t)
            if (abs(input%first) <= 1.0d0) then
                new_input%first = input%first / abs(input%first)
                if (abs(input%second) <= 1.0d0) then
                    new_input%second = input%second / abs(input%second)
                    shrunk = simplest_value(new_input)
                else
                    new_input%second = input%second / 2.0d0
                    shrunk = shrunk_value(new_input)
                end if
            else
                new_input%first = input%first / 2.0d0
                if (abs(input%second) <= 1.0d0) then
                    new_input%second = input%second / abs(input%second)
                else
                    new_input%second = input%second / 2.0d0
                end if
                shrunk = shrunk_value(new_input)
            end if
        end select
    end function shrink
end module NonZeroDoublePrecisionPairGenerator_m
