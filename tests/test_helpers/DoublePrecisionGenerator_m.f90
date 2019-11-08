module DoublePrecisionGenerator_m
    use Vegetables_m, only: &
            DoublePrecisionInput_t, &
            Generated_t, &
            Generator_t, &
            Input_t, &
            ShrinkResult_t, &
            Generated, &
            getRandomDoublePrecisionWithMagnitude, &
            ShrunkValue, &
            SimplestValue

    implicit none
    private

    type, public, extends(Generator_t) :: DoublePrecisionGenerator_t
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
        type(Generated_t) :: random_double

        type(DoublePrecisionInput_t) :: the_input

        associate(a => self)
        end associate

        the_input%value_ = getRandomDoublePrecisionWithMagnitude(1.0d12)
        random_double = Generated(the_input)
    end function generate

    pure function shrink(input) result(shrunk)
        class(Input_t), intent(in) :: input
        type(ShrinkResult_t) :: shrunk

        type(DoublePrecisionInput_t) :: new_input

        select type (input)
        type is (DoublePrecisionInput_t)
            if (effectivelyZero(input%value_)) then
                new_input%value_ = 0.0d0
                shrunk = SimplestValue(new_input)
            else
                new_input%value_ = input%value_ / 2.0d0
                shrunk = ShrunkValue(new_input)
            end if
        end select
    end function shrink

    pure function effectivelyZero(value_)
        double precision, intent(in) :: value_
        logical :: effectivelyZero

        effectivelyZero = abs(value_) < epsilon(value_)
    end function effectivelyZero
end module DoublePrecisionGenerator_m
