module NonZeroDoublePrecisionGenerator_m
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

    type, public, extends(Generator_t) :: NonZeroDoublePrecisionGenerator_t
    contains
        private
        procedure, public :: generate
        procedure, public, nopass :: shrink
    end type NonZeroDoublePrecisionGenerator_t

    type(NonZeroDoublePrecisionGenerator_t), public :: &
            NON_ZERO_DOUBLE_PRECISION_GENERATOR = &
                    NonZeroDoublePrecisionGenerator_t()
contains
    function generate(self) result(random_double)
        class(NonZeroDoublePrecisionGenerator_t), intent(in) :: self
        type(Generated_t) :: random_double

        type(DoublePrecisionInput_t) :: the_input

        associate(a => self)
        end associate

        do
            the_input%value_ = getRandomDoublePrecisionWithMagnitude(1.0d12)
            if (abs(the_input%value_) >= 1.0d0) exit
        end do
        random_double = Generated(the_input)
    end function generate

    function shrink(input) result(shrunk)
        class(Input_t), intent(in) :: input
        type(ShrinkResult_t) :: shrunk

        type(DoublePrecisionInput_t) :: new_input

        select type (input)
        type is (DoublePrecisionInput_t)
            if (abs(input%value_) <= 1.0d0) then
                new_input%value_ = input%value_ / abs(input%value_)
                shrunk = SimplestValue(new_input)
            else
                new_input%value_ = input%value_ / 2.0d0
                shrunk = ShrunkValue(new_input)
            end if
        end select
    end function shrink
end module NonZeroDoublePrecisionGenerator_m
