module interquantity_test
    implicit none
    private

    public :: test_interquantity_operators
contains
    function test_interquantity_operators() result(tests)
        use Vegetables_m, only: TestItem_t, Describe, It

        type(TestItem_t) :: tests

        type(TestItem_t) :: individual_tests(2)

        individual_tests(1) = It( &
                "2 m * 2 m = 4 m^2", checkLengthTimesLength)
        individual_tests(2) = It( &
                "4 m^2 / 2 m = 2 m", checkAreaDividedByLength)
        tests = Describe("Interquantity operations", individual_tests)
    end function test_interquantity_operators

    function checkLengthTimesLength() result(result_)
        use Area_m, only: operator(.unit.), SQUARE_METERS
        use Area_asserts_m, only: assertEquals
        use Interquantity_operators_m, only: operator(*)
        use Length_m, only: operator(.unit.), METERS
        use Vegetables_m, only: Result_t

        type(Result_t) :: result_

        result_ = assertEquals( &
                4.0d0.unit.SQUARE_METERS, &
                (2.0d0.unit.METERS) * (2.0d0.unit.METERS))
    end function checkLengthTimesLength

    function checkAreaDividedByLength() result(result_)
        use Area_m, only: operator(.unit.), SQUARE_METERS
        use Interquantity_operators_m, only: operator(/)
        use Length_m, only: operator(.unit.), METERS
        use Length_asserts_m, only: assertEquals
        use Vegetables_m, only: Result_t

        type(Result_t) :: result_

        result_ = assertEquals( &
                2.0d0.unit.METERS, &
                (4.0d0.unit.SQUARE_METERS) / (2.0d0.unit.METERS))
    end function checkAreaDividedByLength
end module interquantity_test
