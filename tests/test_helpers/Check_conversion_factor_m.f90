module Check_conversion_factor_m
    implicit none
    private

    interface checkConversionFactorsAreInverse
        module procedure checkConversionFactorsAreInverseForQuantityCamel
        module procedure checkConversionFactorsAreInverseForLength
        module procedure checkConversionFactorsAreInverseForMass
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
        use Mass_m, only: Mass_t, MassUnit_t, operator(.unit.)
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
end module Check_conversion_factor_m
