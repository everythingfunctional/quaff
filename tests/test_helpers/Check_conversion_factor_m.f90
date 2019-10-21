module Check_conversion_factor_m
    implicit none
    private

    interface checkConversionFactorsAreInverse
        module procedure checkConversionFactorsAreInverseForQuantityCamel
        module procedure checkConversionFactorsAreInverseForAmount
        module procedure checkConversionFactorsAreInverseForAngle
        module procedure checkConversionFactorsAreInverseForLength
        module procedure checkConversionFactorsAreInverseForMass
        module procedure checkConversionFactorsAreInverseForTime
    end interface checkConversionFactorsAreInverse

    public :: checkConversionFactorsAreInverse
contains
    function checkConversionFactorsAreInverseForQuantityCamel( &
            from, to) result(result_)
        use Quantity_module_m, only: &
                QuantityCamel_t, QuantityCamelUnit_t, operator(.unit.)
        use iso_varying_string, only: operator(//)
        use Vegetables_m, only: Result_t, assertEqualsWithinRelative

        type(QuantityCamelUnit_t), intent(in) :: to
        type(QuantityCamelUnit_t), intent(in) :: from
        type(Result_t) :: result_

        double precision :: factor1
        double precision :: factor2

        factor1 = (1.0d0.unit.from).in.to
        factor2 = (1.0d0.unit.to).in.from
        result_ = assertEqualsWithinRelative( &
                factor1, &
                1.0d0 / factor2, &
                1.0d-12, &
                from%toString() // " to " // to%toString())
    end function checkConversionFactorsAreInverseForQuantityCamel

    function checkConversionFactorsAreInverseForAmount( &
            from, to) result(result_)
        use Amount_m, only: &
                Amount_t, AmountUnit_t, operator(.unit.)
        use iso_varying_string, only: operator(//)
        use Vegetables_m, only: Result_t, assertEqualsWithinRelative

        type(AmountUnit_t), intent(in) :: to
        type(AmountUnit_t), intent(in) :: from
        type(Result_t) :: result_

        double precision :: factor1
        double precision :: factor2

        factor1 = (1.0d0.unit.from).in.to
        factor2 = (1.0d0.unit.to).in.from
        result_ = assertEqualsWithinRelative( &
                factor1, &
                1.0d0 / factor2, &
                1.0d-12, &
                from%toString() // " to " // to%toString())
    end function checkConversionFactorsAreInverseForAmount

    function checkConversionFactorsAreInverseForAngle( &
            from, to) result(result_)
        use Angle_m, only: &
                Angle_t, AngleUnit_t, operator(.unit.)
        use iso_varying_string, only: operator(//)
        use Vegetables_m, only: Result_t, assertEqualsWithinRelative

        type(AngleUnit_t), intent(in) :: to
        type(AngleUnit_t), intent(in) :: from
        type(Result_t) :: result_

        double precision :: factor1
        double precision :: factor2

        factor1 = (1.0d0.unit.from).in.to
        factor2 = (1.0d0.unit.to).in.from
        result_ = assertEqualsWithinRelative( &
                factor1, &
                1.0d0 / factor2, &
                1.0d-12, &
                from%toString() // " to " // to%toString())
    end function checkConversionFactorsAreInverseForAngle

    function checkConversionFactorsAreInverseForLength( &
            from, to) result(result_)
        use Length_m, only: &
                Length_t, LengthUnit_t, operator(.unit.)
        use iso_varying_string, only: operator(//)
        use Vegetables_m, only: Result_t, assertEqualsWithinRelative

        type(LengthUnit_t), intent(in) :: to
        type(LengthUnit_t), intent(in) :: from
        type(Result_t) :: result_

        double precision :: factor1
        double precision :: factor2

        factor1 = (1.0d0.unit.from).in.to
        factor2 = (1.0d0.unit.to).in.from
        result_ = assertEqualsWithinRelative( &
                factor1, &
                1.0d0 / factor2, &
                1.0d-12, &
                from%toString() // " to " // to%toString())
    end function checkConversionFactorsAreInverseForLength

    function checkConversionFactorsAreInverseForMass( &
            from, to) result(result_)
        use Mass_m, only: &
                Mass_t, MassUnit_t, operator(.unit.)
        use iso_varying_string, only: operator(//)
        use Vegetables_m, only: Result_t, assertEqualsWithinRelative

        type(MassUnit_t), intent(in) :: to
        type(MassUnit_t), intent(in) :: from
        type(Result_t) :: result_

        double precision :: factor1
        double precision :: factor2

        factor1 = (1.0d0.unit.from).in.to
        factor2 = (1.0d0.unit.to).in.from
        result_ = assertEqualsWithinRelative( &
                factor1, &
                1.0d0 / factor2, &
                1.0d-12, &
                from%toString() // " to " // to%toString())
    end function checkConversionFactorsAreInverseForMass

    function checkConversionFactorsAreInverseForTime( &
            from, to) result(result_)
        use Time_m, only: &
                Time_t, TimeUnit_t, operator(.unit.)
        use iso_varying_string, only: operator(//)
        use Vegetables_m, only: Result_t, assertEqualsWithinRelative

        type(TimeUnit_t), intent(in) :: to
        type(TimeUnit_t), intent(in) :: from
        type(Result_t) :: result_

        double precision :: factor1
        double precision :: factor2

        factor1 = (1.0d0.unit.from).in.to
        factor2 = (1.0d0.unit.to).in.from
        result_ = assertEqualsWithinRelative( &
                factor1, &
                1.0d0 / factor2, &
                1.0d-12, &
                from%toString() // " to " // to%toString())
    end function checkConversionFactorsAreInverseForTime
end module Check_conversion_factor_m
