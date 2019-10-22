module interquantity_test
    implicit none
    private

    public :: test_interquantity_operators
contains
    function test_interquantity_operators() result(tests)
        use Vegetables_m, only: TestItem_t, Describe, It

        type(TestItem_t) :: tests

        type(TestItem_t) :: individual_tests(6)

        individual_tests(1) = It( &
                "2 m * 2 m = 4 m^2", checkLengthTimesLength)
        individual_tests(2) = It( &
                "4 m^2 / 2 m = 2 m", checkAreaDividedByLength)
        individual_tests(3) = It( &
                "2 m^2 * 2 m = 4 m^3", checkAreaTimesLength)
        individual_tests(4) = It( &
                "2 m * 2 m^2 = 4 m^3", checkLengthTimesArea)
        individual_tests(5) = It( &
                "4 m^3 / 2 m^2 = 2 m", checkVolumeDividedByArea)
        individual_tests(6) = It( &
                "4 m^3 / 2 m = 2 m^2", checkVolumeDividedByLength)
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

    function checkAreaTimesLength() result(result_)
        use Area_m, only: operator(.unit.), SQUARE_METERS
        use Interquantity_operators_m, only: operator(*)
        use Length_m, only: operator(.unit.), METERS
        use Vegetables_m, only: Result_t
        use Volume_m, only: operator(.unit.), CUBIC_METERS
        use Volume_asserts_m, only: assertEquals

        type(Result_t) :: result_

        result_ = assertEquals( &
                4.0d0.unit.CUBIC_METERS, &
                (2.0d0.unit.SQUARE_METERS) * (2.0d0.unit.METERS))
    end function checkAreaTimesLength

    function checkLengthTimesArea() result(result_)
        use Area_m, only: operator(.unit.), SQUARE_METERS
        use Interquantity_operators_m, only: operator(*)
        use Length_m, only: operator(.unit.), METERS
        use Vegetables_m, only: Result_t
        use Volume_m, only: operator(.unit.), CUBIC_METERS
        use Volume_asserts_m, only: assertEquals

        type(Result_t) :: result_

        result_ = assertEquals( &
                4.0d0.unit.CUBIC_METERS, &
                (2.0d0.unit.METERS) * (2.0d0.unit.SQUARE_METERS))
    end function checkLengthTimesArea

    function checkVolumeDividedByArea() result(result_)
        use Area_m, only: operator(.unit.), SQUARE_METERS
        use Interquantity_operators_m, only: operator(/)
        use Length_m, only: operator(.unit.), METERS
        use Length_asserts_m, only: assertEquals
        use Vegetables_m, only: Result_t
        use Volume_m, only: operator(.unit.), CUBIC_METERS

        type(Result_t) :: result_

        result_ = assertEquals( &
                2.0d0.unit.METERS, &
                (4.0d0.unit.CUBIC_METERS) / (2.0d0.unit.SQUARE_METERS))
    end function checkVolumeDividedByArea

    function checkVolumeDividedByLength() result(result_)
        use Area_m, only: operator(.unit.), SQUARE_METERS
        use Area_asserts_m, only: assertEquals
        use Interquantity_operators_m, only: operator(/)
        use Length_m, only: operator(.unit.), METERS
        use Vegetables_m, only: Result_t
        use Volume_m, only: operator(.unit.), CUBIC_METERS

        type(Result_t) :: result_

        result_ = assertEquals( &
                2.0d0.unit.SQUARE_METERS, &
                (4.0d0.unit.CUBIC_METERS) / (2.0d0.unit.METERS))
    end function checkVolumeDividedByLength
end module interquantity_test