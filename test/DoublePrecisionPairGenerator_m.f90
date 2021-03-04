module DoublePrecisionPairGenerator_m
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

    type, public, extends(input_t) :: DoublePrecisionPairInput_t
        double precision :: first
        double precision :: second
    end type DoublePrecisionPairInput_t

    type, public, extends(generator_t) :: DoublePrecisionPairGenerator_t
    contains
        private
        procedure, public :: generate
        procedure, public, nopass :: shrink
    end type DoublePrecisionPairGenerator_t

    type(DoublePrecisionPairGenerator_t), public :: DOUBLE_PRECISION_PAIR_GENERATOR = &
            DoublePrecisionPairGenerator_t()
contains
    function generate(self) result(random_double)
        class(DoublePrecisionPairGenerator_t), intent(in) :: self
        type(generated_t) :: random_double

        type(DoublePrecisionPairInput_t) :: the_input

        associate(a => self)
        end associate

        the_input%first = get_random_double_precision_with_magnitude(1.0d12)
        the_input%second = get_random_double_precision_with_magnitude(1.0d12)
        random_double = generated_t(the_input)
    end function generate

    function shrink(input) result(shrunk)
        class(input_t), intent(in) :: input
        type(shrink_result_t) :: shrunk

        type(DoublePrecisionPairInput_t) :: new_input

        select type (input)
        type is (DoublePrecisionPairInput_t)
            if (effectivelyZero(input%first)) then
                new_input%first = 0.0d0
                if (effectivelyZero(input%second)) then
                    new_input%second = 0.0d0
                    shrunk = simplest_value(new_input)
                else
                    new_input%second = input%second / 2.0d0
                    shrunk = shrunk_value(new_input)
                end if
            else
                new_input%first = input%first / 2.0d0
                if (effectivelyZero(input%second)) then
                    new_input%second = 0.0d0
                else
                    new_input%second = input%second / 2.0d0
                end if
                shrunk = shrunk_value(new_input)
            end if
        end select
    end function shrink

    pure function effectivelyZero(value_)
        double precision, intent(in) :: value_
        logical :: effectivelyZero

        effectivelyZero = abs(value_) < epsilon(value_)
    end function effectivelyZero
end module DoublePrecisionPairGenerator_m
