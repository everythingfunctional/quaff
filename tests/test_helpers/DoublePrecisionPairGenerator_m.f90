module DoublePrecisionPairGenerator_m
    use Vegetables_m, only: Generator_t, Input_t

    implicit none
    private

    type, public, extends(Input_t) :: DoublePrecisionPairInput_t
        double precision :: first
        double precision :: second
    end type DoublePrecisionPairInput_t

    type, public, extends(Generator_t) :: DoublePrecisionPairGenerator_t
    contains
        private
        procedure, public :: generate
        procedure, public, nopass :: shrink
    end type DoublePrecisionPairGenerator_t

    type(DoublePrecisionPairGenerator_t), public :: DOUBLE_PRECISION_PAIR_GENERATOR = &
            DoublePrecisionPairGenerator_t()
contains
    function generate(self) result(random_double)
        use Vegetables_m, only: &
                DoublePrecisionInput_t, &
                Generated_t, &
                Generated, &
                getRandomDoublePrecisionWithMagnitude

        class(DoublePrecisionPairGenerator_t), intent(in) :: self
        type(Generated_t) :: random_double

        type(DoublePrecisionPairInput_t) :: the_input

        associate(a => self)
        end associate

        the_input%first = getRandomDoublePrecisionWithMagnitude(1.0d12)
        the_input%second = getRandomDoublePrecisionWithMagnitude(1.0d12)
        random_double = Generated(the_input)
    end function generate

    function shrink(input) result(shrunk)
        use Vegetables_m, only: &
                DoublePrecisionInput_t, &
                Input_t, &
                ShrinkResult_t, &
                ShrunkValue, &
                SimplestValue

        class(Input_t), intent(in) :: input
        type(ShrinkResult_t) :: shrunk

        type(DoublePrecisionPairInput_t) :: new_input

        select type (input)
        type is (DoublePrecisionPairInput_t)
            if (effectivelyZero(input%first)) then
                new_input%first = 0.0d0
                if (effectivelyZero(input%second)) then
                    new_input%second = 0.0d0
                    shrunk = SimplestValue(new_input)
                else
                    new_input%second = input%second / 2.0d0
                    shrunk = ShrunkValue(new_input)
                end if
            else
                new_input%first = input%first / 2.0d0
                if (effectivelyZero(input%second)) then
                    new_input%second = 0.0d0
                else
                    new_input%second = input%second / 2.0d0
                end if
                shrunk = ShrunkValue(new_input)
            end if
        end select
    end function shrink

    pure function effectivelyZero(value_)
        double precision, intent(in) :: value_
        logical :: effectivelyZero

        effectivelyZero = abs(value_) < epsilon(value_)
    end function effectivelyZero
end module DoublePrecisionPairGenerator_m
