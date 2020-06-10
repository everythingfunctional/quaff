module dynamic_viscosity_type_test
    use DoublePrecisionGenerator_m, only: DOUBLE_PRECISION_GENERATOR
    use erloff, only: ErrorList_t
    use iso_varying_string, only: operator(//)
    use Miscellaneous_m, only: PARSE_ERROR
    use quaff, only: &
            DynamicViscosity_t, &
            DynamicViscosityUnit_t, &
            operator(.unit.), &
            fromString, &
            sum, &
            PROVIDED_DYNAMIC_VISCOSITY_UNITS, &
            PASCAL_SECONDS
    use quaff_asserts_m, only: assertEquals
    use Vegetables_m, only: &
            DoublePrecisionInput_t, &
            Example_t, &
            Input_t, &
            Result_t, &
            TestItem_t, &
            TestResultItem_t, &
            assertEqualsWithinRelative, &
            assertNot, &
            assertThat, &
            Describe, &
            Example, &
            fail, &
            It

    implicit none
    private

    type, public, extends(Input_t) :: UnitsInput_t
        class(DynamicViscosityUnit_t), allocatable :: unit
    end type UnitsInput_t

    type, public, extends(Input_t) :: UnitsPairInput_t
        class(DynamicViscosityUnit_t), allocatable :: first
        class(DynamicViscosityUnit_t), allocatable :: second
    end type UnitsPairInput_t

    type, public :: UnitsExamples_t
        type(Example_t), allocatable :: units(:)
        type(Example_t), allocatable :: pairs(:)
    end type UnitsExamples_t

    public :: test_dynamic_viscosity
contains
    function test_dynamic_viscosity() result(tests)
        type(TestItem_t) :: tests

        type(UnitsExamples_t) :: examples
        type(TestItem_t) :: individual_tests(7)

        examples = makeUnitsExamples(PROVIDED_DYNAMIC_VISCOSITY_UNITS)
        individual_tests(1) = it( &
                "gets the same value given the same units", &
                examples%units, &
                checkRoundTrip)
        individual_tests(2) = it( &
                "the conversion factors between 2 units are inverses", &
                examples%pairs, &
                checkConversionFactorsInverse)
        individual_tests(3) = it( &
                "can be converted to and from a string", &
                examples%units, &
                checkToAndFromString)
        individual_tests(4) = it( &
                "Trying to parse a bad string is an error", &
                checkBadString)
        individual_tests(5) = it( &
                "Trying to parse with an unknown unit is an error", &
                checkBadUnit)
        individual_tests(6) = it( &
                "Trying to parse a bad number is an error", &
                checkBadNumber)
        individual_tests(7) = it("Can be summed", checkSum)
        tests = describe("DynamicViscosity_t", individual_tests)
    end function test_dynamic_viscosity

    function checkRoundTrip(units) result(result_)
        class(Input_t), intent(in) :: units
        type(Result_t) :: result_

        select type (units)
        type is (UnitsInput_t)
            result_ = checkRoundTripIn(units%unit)
        class default
            result_ = fail("Expected to get an UnitsInput_t")
        end select
    end function checkRoundTrip

    pure function checkConversionFactorsInverse(pair) result(result_)
        class(Input_t), intent(in) :: pair
        type(Result_t) :: result_

        select type (pair)
        type is (UnitsPairInput_t)
            result_ = checkConversionFactorsAreInverse(pair%first, pair%second)
        class default
            result_ = fail("Expected to get a UnitsPairInput_t")
        end select
    end function checkConversionFactorsInverse

    function checkToAndFromString(units) result(result_)
        class(Input_t), intent(in) :: units
        type(Result_t) :: result_

        select type (units)
        type is (UnitsInput_t)
            result_ = checkStringTrip(units%unit)
        class default
            result_ = fail("Expected to get an UnitsInput_t")
        end select
    end function checkToAndFromString

    pure function checkBadString() result(result_)
        type(Result_t) :: result_

        type(ErrorList_t) :: errors
        type(DynamicViscosity_t) :: dynamic_viscosity

        call fromString("bad", errors, dynamic_viscosity)
        result_ = assertThat(errors.hasType.PARSE_ERROR, errors%toString())
    end function checkBadString

    pure function checkBadUnit() result(result_)
        type(Result_t) :: result_

        type(ErrorList_t) :: errors
        type(DynamicViscosity_t) :: dynamic_viscosity

        call fromString( &
                "1.0 bad", [PASCAL_SECONDS], errors, dynamic_viscosity)
        result_ = assertThat(errors.hasType.PARSE_ERROR, errors%toString())
    end function checkBadUnit

    pure function checkBadNumber() result(result_)
        type(Result_t) :: result_

        type(ErrorList_t) :: errors
        type(DynamicViscosity_t) :: dynamic_viscosity

        call fromString("bad Pa s", errors, dynamic_viscosity)
        result_ = assertThat(errors.hasType.PARSE_ERROR, errors%toString())
    end function checkBadNumber

    pure function checkSum() result(result_)
        type(Result_t) :: result_

        double precision, parameter :: numbers(*) = [1.0d0, 2.0d0, 3.0d0]

        result_ = assertEquals( &
                sum(numbers).unit.PASCAL_SECONDS, &
                sum(numbers.unit.PASCAL_SECONDS))
    end function checkSum

    pure function makeUnitsExamples(units) result(examples)
        class(DynamicViscosityUnit_t), intent(in) :: units(:)
        type(UnitsExamples_t) :: examples

        integer :: i
        integer :: j
        integer :: num_pairs
        integer :: num_units
        type(UnitsPairInput_t) :: pair
        integer :: pair_index
        type(UnitsInput_t) :: input

        num_units = size(units)
        allocate(examples%units(num_units))
        do i = 1, num_units
            allocate(input%unit, source = units(i))
            examples%units(i) = Example(input)
            deallocate(input%unit)
        end do

        num_pairs = combinations(num_units)
        allocate(examples%pairs(num_pairs))
        pair_index = 1
        do i = 1, num_units - 1
            allocate(pair%first, source = units(i))
            do j = i + 1, num_units
                allocate(pair%second, source = units(j))
                examples%pairs(pair_index) = Example(pair)
                pair_index = pair_index + 1
                deallocate(pair%second)
            end do
            deallocate(pair%first)
        end do
    contains
        pure recursive function combinations(num_items) result(num_combinations)
            integer, intent(in) :: num_items
            integer :: num_combinations

            if (num_items <= 1) then
                num_combinations = 0
            else
                num_combinations = num_items - 1 + combinations(num_items - 1)
            end if
        end function combinations
    end function makeUnitsExamples

    function checkRoundTripIn(units) result(result_)
        class(DynamicViscosityUnit_t), intent(in) :: units
        type(Result_t) :: result_

        type(TestItem_t) :: the_test
        type(TestResultItem_t) :: the_result

        the_test = It(units%toString(), DOUBLE_PRECISION_GENERATOR, checkRoundTrip_)
        the_result = the_test%run()
        result_ = assertThat(the_result%passed(), the_result%verboseDescription(.false.))
    contains
        pure function checkRoundTrip_(input) result(result__)
            class(Input_t), intent(in) :: input
            type(Result_t) :: result__

            type(DynamicViscosity_t) :: intermediate

            select type (input)
            type is (DoublePrecisionInput_t)
                intermediate = input%value_.unit.units
                result__ = assertEqualsWithinRelative( &
                        input%value_, &
                        intermediate.in.units, &
                        1.0d-12)
            class default
                result__ = fail("Expected to get a DoublePrecisionInput_t")
            end select
        end function checkRoundTrip_
    end function checkRoundTripIn

    pure function checkConversionFactorsAreInverse( &
            from, to) result(result_)
        class(DynamicViscosityUnit_t), intent(in) :: to
        class(DynamicViscosityUnit_t), intent(in) :: from
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
    end function checkConversionFactorsAreInverse

    function checkStringTrip(units) result(result_)
        class(DynamicViscosityUnit_t), intent(in) :: units
        type(Result_t) :: result_

        type(TestItem_t) :: the_test
        type(TestResultItem_t) :: the_result

        the_test = It(units%toString(), DOUBLE_PRECISION_GENERATOR, doCheck)
        the_result = the_test%run()
        result_ = assertThat(the_result%passed(), the_result%verboseDescription(.false.))
    contains
        pure function doCheck(input) result(result__)
            class(Input_t), intent(in) :: input
            type(Result_t) :: result__

            type(ErrorList_t) :: errors
            type(DynamicViscosity_t) :: original_dynamic_viscosity
            type(DynamicViscosity_t) :: new_dynamic_viscosity

            select type (input)
            type is (DoublePrecisionInput_t)
                original_dynamic_viscosity = input%value_.unit.units
                call fromString( &
                        original_dynamic_viscosity%toStringIn(units), &
                        errors, &
                        new_dynamic_viscosity)
                result__ = &
                        assertEquals( &
                                original_dynamic_viscosity, &
                                new_dynamic_viscosity) &
                        .and.assertNot(errors%hasAny(), errors%toString())
            class default
                result__ = fail("Expected to get a DoublePrecisionInput_t")
            end select
        end function doCheck
    end function checkStringTrip
end module dynamic_viscosity_type_test
