module interquantity_test
    use quaff, only: &
            operator(*), &
            operator(/), &
            operator(.unit.), &
            asBurnup, &
            CUBIC_METERS, &
            JOULES, &
            JOULES_PER_KILOGRAM, &
            KILOGRAMS, &
            KILOGRAMS_PER_CUBIC_METER, &
            KILOGRAMS_PER_MOL, &
            METERS, &
            METERS_PER_SECOND, &
            METERS_PER_SQUARE_SECOND, &
            MOLS, &
            NEWTONS, &
            PASCAL_SECONDS, &
            PASCALS, &
            SECONDS, &
            SQUARE_METERS, &
            WATT_SECONDS_PER_KILOGRAM, &
            WATTS
    use quaff_asserts_m, only: assertEquals
    use Vegetables_m, only: Result_t, TestItem_t, Describe, It

    implicit none
    private

    public :: test_interquantity_operators
contains
    function test_interquantity_operators() result(tests)
        type(TestItem_t) :: tests

        type(TestItem_t) :: individual_tests(50)

        individual_tests(1) = It( &
                "2 m * 3 m = 6 m^2", checkLengthTimesLength)
        individual_tests(2) = It( &
                "6 m^2 / 3 m = 2 m", checkAreaDividedByLength)
        individual_tests(3) = It( &
                "2 m^2 * 3 m = 6 m^3", checkAreaTimesLength)
        individual_tests(4) = It( &
                "2 m * 3 m^2 = 6 m^3", checkLengthTimesArea)
        individual_tests(5) = It( &
                "6 m^3 / 3 m^2 = 2 m", checkVolumeDividedByArea)
        individual_tests(6) = It( &
                "6 m^3 / 3 m = 2 m^2", checkVolumeDividedByLength)
        individual_tests(7) = It( &
                "6 kg / 3 m^3 = 2 kg/m^3", checkMassDividedByVolume)
        individual_tests(8) = It( &
                "2 kg/m^3 * 3 m^3 = 6 kg", checkDensityTimesVolume)
        individual_tests(9) = It( &
                "2 m^3 * 3 kg/m^3 = 6 kg", checkVolumeTimesDensity)
        individual_tests(10) = It( &
                "6 kg / 3 kg/m^3 = 2 m^3", checkMassDividedByDensity)
        individual_tests(11) = It( &
                "6 m / 3 s = 2 m/s", checkLengthDividedByTime)
        individual_tests(12) = It( &
                "2 m/s * 3 s = 6 m", checkSpeedTimesTime)
        individual_tests(13) = It( &
                "2 s * 3 m/s = 6 m", checkTimeTimesSpeed)
        individual_tests(14) = It( &
                "6 m / 3 m/s = 2 s", checkLengthDividedBySpeed)
        individual_tests(15) = It( &
                "6 m/s / 3 s = 2 m/s^2", checkSpeedDividedByTime)
        individual_tests(16) = It( &
                "2 m/s^2 * 3 s = 6 m/s", checkAccelerationTimesTime)
        individual_tests(17) = It( &
                "2 s * 3 m/s^2 = 6 m/s", checkTimeTimesAcceleration)
        individual_tests(18) = It( &
                "6 m/s / 3 m/s^2 = 2 s", checkSpeedDividedByAcceleration)
        individual_tests(19) = It( &
                "2 kg * 3 m/s^2 = 6 N", checkMassTimesAcceleration)
        individual_tests(20) = It( &
                "2 m/s^2 * 3 kg = 6 N", checkAccelerationTimesMass)
        individual_tests(21) = It( &
                "6 N / 3 m/s^2 = 2 kg", checkForceDividedByAcceleration)
        individual_tests(22) = It( &
                "6 N / 3 kg = 2 m/s^2", checkForceDividedByMass)
        individual_tests(23) = It( &
                "6 N / 3 m^2 = 2 Pa", checkForceDividedByArea)
        individual_tests(24) = It( &
                "2 Pa * 3 m^2 = 6 N", checkPressureTimesArea)
        individual_tests(25) = It( &
                "2 m^2 * 3 Pa = 6 N", checkAreaTimesPressure)
        individual_tests(26) = It( &
                "6 N / 3 Pa = 2 m^2", checkForceDividedByPressure)
        individual_tests(27) = It( &
                "2 N * 3 m = 6 J", checkForceTimesLength)
        individual_tests(28) = It( &
                "2 m * 3 N = 6 J", checkLengthTimesForce)
        individual_tests(29) = It( &
                "6 J / 3 N = 2 m", checkEnergyDividedByForce)
        individual_tests(30) = It( &
                "6 J / 3 m = 2 N", checkEnergyDividedByLength)
        individual_tests(31) = It( &
                "6 J / 3 s = 2 W", checkEnergyDividedByTime)
        individual_tests(32) = It( &
                "2 W * 3 s = 6 J", checkPowerTimesTime)
        individual_tests(33) = It( &
                "2 s * 3 W = 6 J", checkTimeTimesPower)
        individual_tests(34) = It( &
                "6 J / 3 W = 2 s", checkEnergyDividedByPower)
        individual_tests(35) = It( &
                "2 Pa * 3 s = 6 Pa s", checkPressureTimesTime)
        individual_tests(36) = It( &
                "2 s * 3 Pa = 6 Pa s", checkTimeTimesPressure)
        individual_tests(37) = It( &
                "6 Pa s / 3 s = 2 Pa", checkDynamicViscosityDividedByTime)
        individual_tests(38) = It( &
                "6 Pa s / 3 Pa = 2 s", checkDynamicViscosityDividedByPressure)
        individual_tests(39) = It( &
                "6 J / 3 kg = 2 J/kg", checkEnergyDividedByMass)
        individual_tests(40) = It( &
                "2 J/kg * 3 kg = 6 J", checkEnthalpyTimesMass)
        individual_tests(41) = It( &
                "2 kg * 3 J/kg = 6 J", checkMassTimesEnthalpy)
        individual_tests(42) = It( &
                "6 J / 3 J/kg = 2 kg", checkEnergyDividedByEnthalpy)
        individual_tests(43) = It( &
                "6 kg / 3 mol = 2 kg/mol", checkMassDividedByAmount)
        individual_tests(44) = It( &
                "2 kg/mol * 3 mol = 6 kg", checkMolarMassTimesAmount)
        individual_tests(45) = It( &
                "2 mol * 3 kg/mol = 6 kg", checkAmountTimesMolarMass)
        individual_tests(46) = It( &
                "6 kg / 3 kg/mol = 2 mol", checkMassDividedByMolarMass)
        individual_tests(47) = It( &
                "2 (W s)/kg * 3 kg = 6 J", checkBurnupTimesMass)
        individual_tests(48) = It( &
                "2 kg * 3 (W s)/kg = 6 J", checkMassTimesBurnup)
        individual_tests(49) = It( &
                "6 J / 3 (W s)/kg = 2 kg", checkEnergyDividedByBurnup)
        individual_tests(50) = It( &
                "2 J/kg = 2 (W s)/kg", checkEnthalpyToBurnup)
        tests = Describe("Interquantity operations", individual_tests)
    end function test_interquantity_operators

    pure function checkLengthTimesLength() result(result_)
        type(Result_t) :: result_

        result_ = assertEquals( &
                6.0d0.unit.SQUARE_METERS, &
                (2.0d0.unit.METERS) * (3.0d0.unit.METERS))
    end function checkLengthTimesLength

    pure function checkAreaDividedByLength() result(result_)
        type(Result_t) :: result_

        result_ = assertEquals( &
                2.0d0.unit.METERS, &
                (6.0d0.unit.SQUARE_METERS) / (3.0d0.unit.METERS))
    end function checkAreaDividedByLength

    pure function checkAreaTimesLength() result(result_)
        type(Result_t) :: result_

        result_ = assertEquals( &
                6.0d0.unit.CUBIC_METERS, &
                (2.0d0.unit.SQUARE_METERS) * (3.0d0.unit.METERS))
    end function checkAreaTimesLength

    pure function checkLengthTimesArea() result(result_)
        type(Result_t) :: result_

        result_ = assertEquals( &
                6.0d0.unit.CUBIC_METERS, &
                (2.0d0.unit.METERS) * (3.0d0.unit.SQUARE_METERS))
    end function checkLengthTimesArea

    pure function checkVolumeDividedByArea() result(result_)
        type(Result_t) :: result_

        result_ = assertEquals( &
                2.0d0.unit.METERS, &
                (6.0d0.unit.CUBIC_METERS) / (3.0d0.unit.SQUARE_METERS))
    end function checkVolumeDividedByArea

    pure function checkVolumeDividedByLength() result(result_)
        type(Result_t) :: result_

        result_ = assertEquals( &
                2.0d0.unit.SQUARE_METERS, &
                (6.0d0.unit.CUBIC_METERS) / (3.0d0.unit.METERS))
    end function checkVolumeDividedByLength

    pure function checkMassDividedByVolume() result(result_)
        type(Result_t) :: result_

        result_ = assertEquals( &
                2.0d0.unit.KILOGRAMS_PER_CUBIC_METER, &
                (6.0d0.unit.KILOGRAMS) / (3.0d0.unit.CUBIC_METERS))
    end function checkMassDividedByVolume

    pure function checkDensityTimesVolume() result(result_)
        type(Result_t) :: result_

        result_ = assertEquals( &
                6.0d0.unit.KILOGRAMS, &
                (2.0d0.unit.KILOGRAMS_PER_CUBIC_METER) * (3.0d0.unit.CUBIC_METERS))
    end function checkDensityTimesVolume

    pure function checkVolumeTimesDensity() result(result_)
        type(Result_t) :: result_

        result_ = assertEquals( &
                6.0d0.unit.KILOGRAMS, &
                (2.0d0.unit.CUBIC_METERS) * (3.0d0.unit.KILOGRAMS_PER_CUBIC_METER))
    end function checkVolumeTimesDensity

    pure function checkMassDividedByDensity() result(result_)
        type(Result_t) :: result_

        result_ = assertEquals( &
                2.0d0.unit.CUBIC_METERS, &
                (6.0d0.unit.KILOGRAMS) / (3.0d0.unit.KILOGRAMS_PER_CUBIC_METER))
    end function checkMassDividedByDensity

    pure function checkLengthDividedByTime() result(result_)
        type(Result_t) :: result_

        result_ = assertEquals( &
                2.0d0.unit.METERS_PER_SECOND, &
                (6.0d0.unit.METERS) / (3.0d0.unit.SECONDS))
    end function checkLengthDividedByTime

    pure function checkSpeedTimesTime() result(result_)
        type(Result_t) :: result_

        result_ = assertEquals( &
                6.0d0.unit.METERS, &
                (2.0d0.unit.METERS_PER_SECOND) * (3.0d0.unit.SECONDS))
    end function checkSpeedTimesTime

    pure function checkTimeTimesSpeed() result(result_)
        type(Result_t) :: result_

        result_ = assertEquals( &
                6.0d0.unit.METERS, &
                (2.0d0.unit.SECONDS) * (3.0d0.unit.METERS_PER_SECOND))
    end function

    pure function checkLengthDividedBySpeed() result(result_)
        type(Result_t) :: result_

        result_ = assertEquals( &
                2.0d0.unit.SECONDS, &
                (6.0d0.unit.METERS) / (3.0d0.unit.METERS_PER_SECOND))
    end function checkLengthDividedBySpeed

    pure function checkSpeedDividedByTime() result(result_)
        type(Result_t) :: result_

        result_ = assertEquals( &
                2.0d0.unit.METERS_PER_SQUARE_SECOND, &
                (6.0d0.unit.METERS_PER_SECOND) / (3.0d0.unit.SECONDS))
    end function checkSpeedDividedByTime

    pure function checkAccelerationTimesTime() result(result_)
        type(Result_t) :: result_

        result_ = assertEquals( &
                6.0d0.unit.METERS_PER_SECOND, &
                (2.0d0.unit.METERS_PER_SQUARE_SECOND) * (3.0d0.unit.SECONDS))
    end function checkAccelerationTimesTime

    pure function checkTimeTimesAcceleration() result(result_)
        type(Result_t) :: result_

        result_ = assertEquals( &
                6.0d0.unit.METERS_PER_SECOND, &
                (2.0d0.unit.SECONDS) * (3.0d0.unit.METERS_PER_SQUARE_SECOND))
    end function checkTimeTimesAcceleration

    pure function checkSpeedDividedByAcceleration() result(result_)
        type(Result_t) :: result_

        result_ = assertEquals( &
                2.0d0.unit.SECONDS, &
                (6.0d0.unit.METERS_PER_SECOND) / (3.0d0.unit.METERS_PER_SQUARE_SECOND))
    end function checkSpeedDividedByAcceleration

    pure function checkMassTimesAcceleration() result(result_)
        type(Result_t) :: result_

        result_ = assertEquals( &
                6.0d0.unit.NEWTONS, &
                (2.0d0.unit.KILOGRAMS) * (3.0d0.unit.METERS_PER_SQUARE_SECOND))
    end function checkMassTimesAcceleration

    pure function checkAccelerationTimesMass() result(result_)
        type(Result_t) :: result_

        result_ = assertEquals( &
                6.0d0.unit.NEWTONS, &
                (2.0d0.unit.METERS_PER_SQUARE_SECOND) * (3.0d0.unit.KILOGRAMS))
    end function checkAccelerationTimesMass

    pure function checkForceDividedByAcceleration() result(result_)
        type(Result_t) :: result_

        result_ = assertEquals( &
                2.0d0.unit.KILOGRAMS, &
                (6.0d0.unit.NEWTONS) / (3.0d0.unit.METERS_PER_SQUARE_SECOND))
    end function checkForceDividedByAcceleration

    pure function checkForceDividedByMass() result(result_)
        type(Result_t) :: result_

        result_ = assertEquals( &
                2.0d0.unit.METERS_PER_SQUARE_SECOND, &
                (6.0d0.unit.NEWTONS) / (3.0d0.unit.KILOGRAMS))
    end function checkForceDividedByMass

    pure function checkForceDividedByArea() result(result_)
        type(Result_t) :: result_

        result_ = assertEquals( &
                2.0d0.unit.PASCALS, &
                (6.0d0.unit.NEWTONS) / (3.0d0.unit.SQUARE_METERS))
    end function checkForceDividedByArea

    pure function checkPressureTimesArea() result(result_)
        type(Result_t) :: result_

        result_ = assertEquals( &
                6.0d0.unit.NEWTONS, &
                (2.0d0.unit.PASCALS) * (3.0d0.unit.SQUARE_METERS))
    end function checkPressureTimesArea

    pure function checkAreaTimesPressure() result(result_)
        type(Result_t) :: result_

        result_ = assertEquals( &
                6.0d0.unit.NEWTONS, &
                (2.0d0.unit.SQUARE_METERS) * (3.0d0.unit.PASCALS))
    end function checkAreaTimesPressure

    pure function checkForceDividedByPressure() result(result_)
        type(Result_t) :: result_

        result_ = assertEquals( &
                2.0d0.unit.SQUARE_METERS, &
                (6.0d0.unit.NEWTONS) / (3.0d0.unit.PASCALS))
    end function checkForceDividedByPressure

    pure function checkForceTimesLength() result(result_)
        type(Result_t) :: result_

        result_ = assertEquals( &
                6.0d0.unit.JOULES, &
                (2.0d0.unit.NEWTONS) * (3.0d0.unit.METERS))
    end function checkForceTimesLength

    pure function checkLengthTimesForce() result(result_)
        type(Result_t) :: result_

        result_ = assertEquals( &
                6.0d0.unit.JOULES, &
                (2.0d0.unit.METERS) * (3.0d0.unit.NEWTONS))
    end function checkLengthTimesForce

    pure function checkEnergyDividedByForce() result(result_)
        type(Result_t) :: result_

        result_ = assertEquals( &
                2.0d0.unit.METERS, &
                (6.0d0.unit.JOULES) / (3.0d0.unit.NEWTONS))
    end function checkEnergyDividedByForce

    pure function checkEnergyDividedByLength() result(result_)
        type(Result_t) :: result_

        result_ = assertEquals( &
                2.0d0.unit.NEWTONS, &
                (6.0d0.unit.JOULES) / (3.0d0.unit.METERS))
    end function checkEnergyDividedByLength

    pure function checkEnergyDividedByTime() result(result_)
        type(Result_t) :: result_

        result_ = assertEquals( &
                2.0d0.unit.WATTS, &
                (6.0d0.unit.JOULES) / (3.0d0.unit.SECONDS))
    end function checkEnergyDividedByTime

    pure function checkPowerTimesTime() result(result_)
        type(Result_t) :: result_

        result_ = assertEquals( &
                6.0d0.unit.JOULES, &
                (2.0d0.unit.WATTS) * (3.0d0.unit.SECONDS))
    end function checkPowerTimesTime

    pure function checkTimeTimesPower() result(result_)
        type(Result_t) :: result_

        result_ = assertEquals( &
                6.0d0.unit.JOULES, &
                (2.0d0.unit.SECONDS) * (3.0d0.unit.WATTS))
    end function checkTimeTimesPower

    pure function checkEnergyDividedByPower() result(result_)
        type(Result_t) :: result_

        result_ = assertEquals( &
                2.0d0.unit.SECONDS, &
                (6.0d0.unit.JOULES) / (3.0d0.unit.WATTS))
    end function checkEnergyDividedByPower

    pure function checkPressureTimesTime() result(result_)
        type(Result_t) :: result_

        result_ = assertEquals( &
                6.0d0.unit.PASCAL_SECONDS, &
                (2.0d0.unit.PASCALS) * (3.0d0.unit.SECONDS))
    end function checkPressureTimesTime

    pure function checkTimeTimesPressure() result(result_)
        type(Result_t) :: result_

        result_ = assertEquals( &
                6.0d0.unit.PASCAL_SECONDS, &
                (2.0d0.unit.SECONDS) * (3.0d0.unit.PASCALS))
    end function checkTimeTimesPressure

    pure function checkDynamicViscosityDividedByTime() result(result_)
        type(Result_t) :: result_

        result_ = assertEquals( &
                2.0d0.unit.PASCALS, &
                (6.0d0.unit.PASCAL_SECONDS) / (3.0d0.unit.SECONDS))
    end function checkDynamicViscosityDividedByTime

    pure function checkDynamicViscosityDividedByPressure() result(result_)
        type(Result_t) :: result_

        result_ = assertEquals( &
                2.0d0.unit.SECONDS, &
                (6.0d0.unit.PASCAL_SECONDS) / (3.0d0.unit.PASCALS))
    end function checkDynamicViscosityDividedByPressure

    pure function checkEnergyDividedByMass() result(result_)
        type(Result_t) :: result_

        result_ = assertEquals( &
                2.0d0.unit.JOULES_PER_KILOGRAM, &
                (6.0d0.unit.JOULES) / (3.0d0.unit.KILOGRAMS))
    end function checkEnergyDividedByMass

    pure function checkEnthalpyTimesMass() result(result_)
        type(Result_t) :: result_

        result_ = assertEquals( &
                6.0d0.unit.JOULES, &
                (2.0d0.unit.JOULES_PER_KILOGRAM) * (3.0d0.unit.KILOGRAMS))
    end function checkEnthalpyTimesMass

    pure function checkMassTimesEnthalpy() result(result_)
        type(Result_t) :: result_

        result_ = assertEquals( &
                6.0d0.unit.JOULES, &
                (2.0d0.unit.KILOGRAMS) * (3.0d0.unit.JOULES_PER_KILOGRAM))
    end function checkMassTimesEnthalpy

    pure function checkEnergyDividedByEnthalpy() result(result_)
        type(Result_t) :: result_

        result_ = assertEquals( &
                2.0d0.unit.KILOGRAMS, &
                (6.0d0.unit.JOULES) / (3.0d0.unit.JOULES_PER_KILOGRAM))
    end function checkEnergyDividedByEnthalpy

    pure function checkMassDividedByAmount() result(result_)
        type(Result_t) :: result_

        result_ = assertEquals( &
                2.0d0.unit.KILOGRAMS_PER_MOL, &
                (6.0d0.unit.KILOGRAMS) / (3.0d0.unit.MOLS))
    end function checkMassDividedByAmount

    pure function checkMolarMassTimesAmount() result(result_)
        type(Result_t) :: result_

        result_ = assertEquals( &
                6.0d0.unit.KILOGRAMS, &
                (2.0d0.unit.KILOGRAMS_PER_MOL) * (3.0d0.unit.MOLS))
    end function checkMolarMassTimesAmount

    pure function checkAmountTimesMolarMass() result(result_)
        type(Result_t) :: result_

        result_ = assertEquals( &
                6.0d0.unit.KILOGRAMS, &
                (2.0d0.unit.MOLS) * (3.0d0.unit.KILOGRAMS_PER_MOL))
    end function checkAmountTimesMolarMass

    pure function checkMassDividedByMolarMass() result(result_)
        type(Result_t) :: result_

        result_ = assertEquals( &
                2.0d0.unit.MOLS, &
                (6.0d0.unit.KILOGRAMS) / (3.0d0.unit.KILOGRAMS_PER_MOL))
    end function checkMassDividedByMolarMass

    pure function checkBurnupTimesMass() result(result_)
        type(Result_t) :: result_

        result_ = assertEquals( &
                6.0d0.unit.JOULES, &
                (2.0d0.unit.WATT_SECONDS_PER_KILOGRAM) * (3.0d0.unit.KILOGRAMS))
    end function checkBurnupTimesMass

    pure function checkMassTimesBurnup() result(result_)
        type(Result_t) :: result_

        result_ = assertEquals( &
                6.0d0.unit.JOULES, &
                (2.0d0.unit.KILOGRAMS) * (3.0d0.unit.WATT_SECONDS_PER_KILOGRAM))
    end function checkMassTimesBurnup

    pure function checkEnergyDividedByBurnup() result(result_)
        type(Result_t) :: result_

        result_ = assertEquals( &
                2.0d0.unit.KILOGRAMS, &
                (6.0d0.unit.JOULES) / (3.0d0.unit.WATT_SECONDS_PER_KILOGRAM))
    end function checkEnergyDividedByBurnup

    pure function checkEnthalpyToBurnup() result(result_)
        type(Result_t) :: result_

        result_ = assertEquals( &
                2.0d0.unit.WATT_SECONDS_PER_KILOGRAM, &
                asBurnup(2.0d0.unit.JOULES_PER_KILOGRAM))
    end function checkEnthalpyToBurnup
end module interquantity_test
