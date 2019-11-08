module NonZeroDoublePrecisionPairGenerator_m
    use DoublePrecisionPairGenerator_m, only: DoublePrecisionPairInput_t
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

    type, public, extends(Generator_t) :: NonZeroDoublePrecisionPairGenerator_t
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
        type(Generated_t) :: random_double

        type(DoublePrecisionPairInput_t) :: the_input

        associate(a => self)
        end associate

        do
            the_input%first = getRandomDoublePrecisionWithMagnitude(1.0d12)
            if (abs(the_input%first) >= 1.0d0) exit
        end do
        do
            the_input%second = getRandomDoublePrecisionWithMagnitude(1.0d12)
            if (abs(the_input%second) >= 1.0d0) exit
        end do
        random_double = Generated(the_input)
    end function generate

    function shrink(input) result(shrunk)
        class(Input_t), intent(in) :: input
        type(ShrinkResult_t) :: shrunk

        type(DoublePrecisionPairInput_t) :: new_input

        select type (input)
        type is (DoublePrecisionPairInput_t)
            if (abs(input%first) <= 1.0d0) then
                new_input%first = input%first / abs(input%first)
                if (abs(input%second) <= 1.0d0) then
                    new_input%second = input%second / abs(input%second)
                    shrunk = SimplestValue(new_input)
                else
                    new_input%second = input%second / 2.0d0
                    shrunk = ShrunkValue(new_input)
                end if
            else
                new_input%first = input%first / 2.0d0
                if (abs(input%second) <= 1.0d0) then
                    new_input%second = input%second / abs(input%second)
                else
                    new_input%second = input%second / 2.0d0
                end if
                shrunk = ShrunkValue(new_input)
            end if
        end select
    end function shrink
end module NonZeroDoublePrecisionPairGenerator_m
