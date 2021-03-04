module DoublePrecisionGenerator_m
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

    type, public, extends(generator_t) :: DoublePrecisionGenerator_t
    contains
        private
        procedure, public :: generate
        procedure, public, nopass :: shrink
    end type DoublePrecisionGenerator_t

    type(DoublePrecisionGenerator_t), public :: DOUBLE_PRECISION_GENERATOR = &
            DoublePrecisionGenerator_t()
contains
    function generate(self) result(random_double)
        class(DoublePrecisionGenerator_t), intent(in) :: self
        type(generated_t) :: random_double

        associate(a => self)
        end associate

        random_double = generated_t(double_precision_input_t( &
                get_random_double_precision_with_magnitude(1.0d12)))
    end function generate

    function shrink(input) result(shrunk)
        class(input_t), intent(in) :: input
        type(shrink_result_t) :: shrunk

        select type (input)
        type is (double_precision_input_t)
            if (effectivelyZero(input%input())) then
                shrunk = simplest_value(double_precision_input_t(0.0d0))
            else
                shrunk = shrunk_value(double_precision_input_t(input%input() / 2.0d0))
            end if
        end select
    end function shrink

    pure function effectivelyZero(value_)
        double precision, intent(in) :: value_
        logical :: effectivelyZero

        effectivelyZero = abs(value_) < epsilon(value_)
    end function effectivelyZero
end module DoublePrecisionGenerator_m
