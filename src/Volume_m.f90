module Volume_m
    use erloff, only: ErrorList_t, Fatal, Module_, Procedure_
    use iso_varying_string, only: &
            VARYING_STRING, &
            assignment(=), &
            operator(==), &
            operator(//), &
            len, &
            split, &
            var_str
    use parff, only: &
            ParsedRational_t, &
            ParseResult_t, &
            ParserOutput_t, &
            State_t, &
            dropThen, &
            parseChar, &
            parseRational, &
            parseString, &
            parseWith, &
            thenDrop
    use quaff_Conversion_factors_m, only: CUBIC_CENTIMETERS_PER_CUBIC_METER
    use quaff_Utilities_m, only: &
            operator(.safeEq.), &
            equalWithinAbsolute_ => equalWithinAbsolute, &
            equalWithinRelative_ => equalWithinRelative, &
            parseSpace, &
            PARSE_ERROR, &
            UNKNOWN_UNIT
    use strff, only: join, toString

    implicit none
    private

    type, public :: Volume_t
        double precision :: cubic_meters
    contains
        private
        procedure :: toUnits
        generic, public :: operator(.in.) => toUnits
        procedure, pass(volume) :: doubleTimesVolume
        procedure, pass(volume) :: integerTimesVolume
        procedure, pass(volume) :: volumeTimesDouble
        procedure, pass(volume) :: volumeTimesInteger
        generic, public :: operator(*) => &
                doubleTimesVolume, &
                integerTimesVolume, &
                volumeTimesDouble, &
                volumeTimesInteger
        procedure :: volumeDividedByDouble
        procedure :: volumeDividedByInteger
        procedure, pass(numerator) :: volumeDividedByVolume
        generic, public :: operator(/) => &
                volumeDividedByDouble, &
                volumeDividedByInteger, &
                volumeDividedByVolume
        procedure :: volumePlusVolume
        generic, public :: operator(+) => volumePlusVolume
        procedure :: volumeMinusVolume
        generic, public :: operator(-) => volumeMinusVolume
        procedure :: greaterThan
        generic, public :: operator(>) => greaterThan
        procedure :: lessThan
        generic, public :: operator(<) => lessThan
        procedure :: greaterThanOrEqual
        generic, public :: operator(>=) => greaterThanOrEqual
        procedure :: lessThanOrEqual
        generic, public :: operator(<=) => lessThanOrEqual
        procedure :: equal_
        generic, public :: operator(==) => equal_
        procedure :: equalWithinAbsolute
        procedure :: equalWithinRelative
        generic, public :: equal => &
                equal_, equalWithinAbsolute, equalWithinRelative
        procedure :: notEqual
        generic, public :: operator(/=) => notEqual
        procedure :: toStringFullPrecision
        procedure :: toStringWithPrecision
        generic, public :: toString => &
                toStringFullPrecision, toStringWithPrecision
        procedure :: toStringInFullPrecision
        procedure :: toStringInWithPrecision
        generic, public :: toStringIn => &
                toStringInFullPrecision, toStringInWithPrecision
    end type Volume_t

    type, abstract, public :: VolumeUnit_t
        double precision :: conversion_factor
    contains
        procedure(justUnitToString), deferred :: unitToString
        procedure(unitWithValueToString), deferred :: valueToString
        generic :: toString => unitToString, valueToString
        procedure(parseAsI), deferred :: parseAs
    end type VolumeUnit_t

    type, extends(VolumeUnit_t), public :: VolumeSimpleUnit_t
        character(len=20) :: symbol
    contains
        procedure :: unitToString => simpleUnitToString
        procedure :: valueToString => simpleValueToString
        procedure :: parseAs => simpleParseAs
    end type VolumeSimpleUnit_t

    type, extends(VolumeUnit_t), public :: VolumeGnuplotUnit_t
        character(len=50) :: symbol
    contains
        procedure :: unitToString => gnuplotUnitToString
        procedure :: valueToString => gnuplotValueToString
        procedure :: parseAs => gnuplotParseAs
    end type VolumeGnuplotUnit_t

    abstract interface
        elemental function justUnitToString(self) result(string)
            import VolumeUnit_t, VARYING_STRING
            class(VolumeUnit_t), intent(in) :: self
            type(VARYING_STRING) :: string
        end function justUnitToString

        pure function unitWithValueToString(self, value_) result(string)
            import VolumeUnit_t, VARYING_STRING
            class(VolumeUnit_t), intent(in) :: self
            type(VARYING_STRING), intent(in) :: value_
            type(VARYING_STRING) :: string
        end function unitWithValueToString

        pure subroutine parseAsI(self, string, errors, volume)
            import ErrorList_t, Volume_t, VolumeUnit_t, VARYING_STRING
            class(VolumeUnit_t), intent(in) :: self
            type(VARYING_STRING), intent(in) :: string
            type(ErrorList_t), intent(out) :: errors
            type(Volume_t), intent(out) :: volume
        end subroutine parseAsI
    end interface

    interface operator(.unit.)
        module procedure fromUnits
    end interface operator(.unit.)

    interface fromString
        module procedure fromStringBasicC
        module procedure fromStringBasicS
        module procedure fromStringWithUnitsC
        module procedure fromStringWithUnitsS
        module procedure simpleUnitFromStringC
        module procedure simpleUnitFromStringS
        module procedure simpleUnitFromStringWithUnitsC
        module procedure simpleUnitFromStringWithUnitsS
    end interface fromString

    interface sum
        module procedure sumVolume
    end interface sum

    type(VolumeSimpleUnit_t), parameter, public :: CUBIC_CENTIMETERS = &
            VolumeSimpleUnit_t( &
                    conversion_factor = CUBIC_CENTIMETERS_PER_CUBIC_METER, &
                    symbol = "cm^3")
    type(VolumeGnuplotUnit_t), parameter, public :: CUBIC_CENTIMETERS_GNUPLOT = &
            VolumeGnuplotUnit_t( &
                    conversion_factor = CUBIC_CENTIMETERS_PER_CUBIC_METER, &
                    symbol = "cm^3")
    type(VolumeSimpleUnit_t), parameter, public :: CUBIC_METERS = &
            VolumeSimpleUnit_t( &
                    conversion_factor = 1.0d0, &
                    symbol = "m^3")
    type(VolumeGnuplotUnit_t), parameter, public :: CUBIC_METERS_GNUPLOT = &
            VolumeGnuplotUnit_t( &
                    conversion_factor = 1.0d0, &
                    symbol = "m^3")

    type(VolumeSimpleUnit_t), public :: DEFAULT_OUTPUT_UNITS = CUBIC_METERS

    type(VolumeSimpleUnit_t), parameter, public :: PROVIDED_UNITS(*) = &
            [CUBIC_CENTIMETERS, CUBIC_METERS]
    type(VolumeGnuplotUnit_t), parameter, public :: PROVIDED_GNUPLOT_UNITS(*) = &
            [CUBIC_CENTIMETERS_GNUPLOT, CUBIC_METERS_GNUPLOT]

    public :: operator(.unit.), fromString, selectUnit, sum
contains
    pure subroutine fromStringBasicC(string, errors, volume)
        character(len=*), intent(in) :: string
        type(ErrorList_t), intent(out) :: errors
        type(Volume_t), intent(out) :: volume

        type(ErrorList_t) :: errors_

        call fromString( &
                var_str(string), PROVIDED_UNITS, errors_, volume)
        call errors%appendErrors( &
                errors_, &
                Module_("Volume_m"), &
                Procedure_("fromStringBasicC"))
    end subroutine fromStringBasicC

    pure subroutine fromStringBasicS(string, errors, volume)
        type(VARYING_STRING), intent(in) :: string
        type(ErrorList_t), intent(out) :: errors
        type(Volume_t), intent(out) :: volume

        type(ErrorList_t) :: errors_

        call fromString( &
                string, PROVIDED_UNITS, errors_, volume)
        call errors%appendErrors( &
                errors_, &
                Module_("Volume_m"), &
                Procedure_("fromStringBasicS"))
    end subroutine fromStringBasicS

    pure subroutine fromStringWithUnitsC(string, units, errors, volume)
        character(len=*), intent(in) :: string
        class(VolumeUnit_t), intent(in) :: units(:)
        type(ErrorList_t), intent(out) :: errors
        type(Volume_t), intent(out) :: volume

        type(ErrorList_t) :: errors_

        call fromString( &
                var_str(string), units, errors_, volume)
        call errors%appendErrors( &
                errors_, &
                Module_("Volume_m"), &
                Procedure_("fromStringWithUnitsC"))
    end subroutine fromStringWithUnitsC

    pure subroutine fromStringWithUnitsS(string, units, errors, volume)
        type(VARYING_STRING), intent(in) :: string
        class(VolumeUnit_t), intent(in) :: units(:)
        type(ErrorList_t), intent(out) :: errors
        type(Volume_t), intent(out) :: volume

        type(ErrorList_t) :: all_errors(size(units))
        integer :: i

        do i = 1, size(units)
            call units(i)%parseAs(string, all_errors(i), volume)
            if (.not. all_errors(i)%hasAny()) return
        end do
        do i = 1, size(units)
            call errors%appendErrors( &
                    all_errors(i), &
                    Module_("Volume_m"), &
                    Procedure_("fromStringWithUnitsS"))
        end do
    end subroutine fromStringWithUnitsS

    elemental function fromUnits(value_, units) result(volume)
        double precision, intent(in) :: value_
        class(VolumeUnit_t), intent(in) :: units
        type(Volume_t) :: volume

        volume%cubic_meters = value_ / units%conversion_factor
    end function fromUnits

    elemental function toUnits(self, units) result(volume)
        class(Volume_t), intent(in) :: self
        class(VolumeUnit_t), intent(in) :: units
        double precision :: volume

        volume = self%cubic_meters * units%conversion_factor
    end function toUnits

    elemental function doubleTimesVolume( &
            multiplier, volume) result(new_volume)
        double precision, intent(in) :: multiplier
        class(Volume_t), intent(in) :: volume
        type(Volume_t) :: new_volume

        new_volume%cubic_meters = &
                multiplier * volume%cubic_meters
    end function doubleTimesVolume

    elemental function integerTimesVolume( &
            multiplier, volume) result(new_volume)
        integer, intent(in) :: multiplier
        class(Volume_t), intent(in) :: volume
        type(Volume_t) :: new_volume

        new_volume%cubic_meters = &
                dble(multiplier) * volume%cubic_meters
    end function integerTimesVolume

    elemental function volumeTimesDouble( &
            volume, multiplier) result(new_volume)
        class(Volume_t), intent(in) :: volume
        double precision, intent(in) :: multiplier
        type(Volume_t) :: new_volume

        new_volume%cubic_meters = &
                volume%cubic_meters * multiplier
    end function volumeTimesDouble

    elemental function volumeTimesInteger( &
            volume, multiplier) result(new_volume)
        class(Volume_t), intent(in) :: volume
        integer, intent(in) :: multiplier
        type(Volume_t) :: new_volume

        new_volume%cubic_meters = &
                volume%cubic_meters * dble(multiplier)
    end function volumeTimesInteger

    elemental function volumeDividedByDouble( &
            volume, divisor) result(new_volume)
        class(Volume_t), intent(in) :: volume
        double precision, intent(in) :: divisor
        type(Volume_t) :: new_volume

        new_volume%cubic_meters = &
                volume%cubic_meters / divisor
    end function volumeDividedByDouble

    elemental function volumeDividedByInteger( &
            volume, divisor) result(new_volume)
        class(Volume_t), intent(in) :: volume
        integer, intent(in) :: divisor
        type(Volume_t) :: new_volume

        new_volume%cubic_meters = &
                volume%cubic_meters / dble(divisor)
    end function volumeDividedByInteger

    elemental function volumeDividedByVolume( &
            numerator, denomenator) result(ratio)
        class(Volume_t), intent(in) :: numerator
        class(Volume_t), intent(in) :: denomenator
        double precision :: ratio

        ratio = numerator%cubic_meters / denomenator%cubic_meters
    end function volumeDividedByVolume

    elemental function volumePlusVolume( &
            volume1, volume2) result(new_volume)
        class(Volume_t), intent(in) :: volume1
        class(Volume_t), intent(in) :: volume2
        type(Volume_t) :: new_volume

        new_volume%cubic_meters = &
                volume1%cubic_meters + volume2%cubic_meters
    end function volumePlusVolume

    elemental function volumeMinusVolume( &
            volume1, volume2) result(new_volume)
        class(Volume_t), intent(in) :: volume1
        class(Volume_t), intent(in) :: volume2
        type(Volume_t) :: new_volume

        new_volume%cubic_meters = &
                volume1%cubic_meters - volume2%cubic_meters
    end function volumeMinusVolume

    pure function sumVolume(volumes)
        type(Volume_t), intent(in) :: volumes(:)
        type(Volume_t) :: sumVolume

        sumVolume%cubic_meters = sum(volumes%cubic_meters)
    end function sumVolume

    elemental function greaterThan(lhs, rhs)
        class(Volume_t), intent(in) :: lhs
        class(Volume_t), intent(in) :: rhs
        logical :: greaterThan

        greaterThan = lhs%cubic_meters > rhs%cubic_meters
    end function greaterThan

    elemental function lessThan(lhs,rhs)
        class(Volume_t), intent(in) :: lhs
        class(Volume_t), intent(in) :: rhs
        logical :: lessThan

        lessThan = lhs%cubic_meters < rhs%cubic_meters
    end function lessThan

    elemental function greaterThanOrEqual(lhs, rhs)
        class(Volume_t), intent(in) :: lhs
        class(Volume_t), intent(in) :: rhs
        logical :: greaterThanOrEqual

        greaterThanOrEqual = lhs%cubic_meters >= rhs%cubic_meters
    end function greaterThanOrEqual

    elemental function lessThanOrEqual(lhs, rhs)
        class(Volume_t), intent(in) :: lhs
        class(Volume_t), intent(in) :: rhs
        logical :: lessThanOrEqual

        lessThanOrEqual = lhs%cubic_meters <= rhs%cubic_meters
    end function lessThanOrEqual

    elemental function equal_(lhs,rhs)
        class(Volume_t), intent(in) :: lhs
        class(Volume_t), intent(in) :: rhs
        logical :: equal_

        equal_ = lhs%cubic_meters .safeEq. rhs%cubic_meters
    end function equal_

    elemental function equalWithinAbsolute(lhs, rhs, within)
        class(Volume_t), intent(in) :: lhs
        class(Volume_t), intent(in) :: rhs
        class(Volume_t), intent(in) :: within
        logical :: equalWithinAbsolute

        equalWithinAbsolute = equalWithinAbsolute_( &
                lhs%cubic_meters, rhs%cubic_meters, within%cubic_meters)
    end function equalWithinAbsolute

    elemental function equalWithinRelative(lhs, rhs, within)
        class(Volume_t), intent(in) :: lhs
        class(Volume_t), intent(in) :: rhs
        double precision, intent(in) :: within
        logical :: equalWithinRelative

        equalWithinRelative = equalWithinRelative_( &
                lhs%cubic_meters, rhs%cubic_meters, within)
    end function equalWithinRelative

    elemental function notEqual(lhs, rhs)
        class(Volume_t), intent(in) :: lhs
        class(Volume_t), intent(in) :: rhs
        logical :: notEqual

        notEqual = .not. lhs == rhs
    end function notEqual

    elemental function toStringFullPrecision(self) result(string)
        class(Volume_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = self%toStringIn(DEFAULT_OUTPUT_UNITS)
    end function toStringFullPrecision

    elemental function toStringWithPrecision(self, significant_digits) result(string)
        class(Volume_t), intent(in) :: self
        integer, intent(in) :: significant_digits
        type(VARYING_STRING) :: string

        string = self%toStringIn(DEFAULT_OUTPUT_UNITS, significant_digits)
    end function toStringWithPrecision

    elemental function toStringInFullPrecision(self, units) result(string)
        class(Volume_t), intent(in) :: self
        class(VolumeUnit_t), intent(in) :: units
        type(VARYING_STRING) :: string

        string = units%toString(toString(self.in.units))
    end function toStringInFullPrecision

    elemental function toStringInWithPrecision( &
            self, units, significant_digits) result(string)
        class(Volume_t), intent(in) :: self
        class(VolumeUnit_t), intent(in) :: units
        integer, intent(in) :: significant_digits
        type(VARYING_STRING) :: string

        string = units%toString(toString(self.in.units, significant_digits))
    end function toStringInWithPrecision

    elemental function simpleUnitToString(self) result(string)
        class(VolumeSimpleUnit_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = trim(self%symbol)
    end function simpleUnitToString

    pure function simpleValueToString(self, value_) result(string)
        class(VolumeSimpleUnit_t), intent(in) :: self
        type(VARYING_STRING), intent(in) :: value_
        type(VARYING_STRING) :: string

        string = value_ // " " // self%toString()
    end function simpleValueToString

    pure subroutine simpleParseAs(self, string, errors, volume)
        class(VolumeSimpleUnit_t), intent(in) :: self
        type(VARYING_STRING), intent(in) :: string
        type(ErrorList_t), intent(out) :: errors
        type(Volume_t), intent(out) :: volume

        type(ParseResult_t) :: parse_result

        parse_result = parseWith(theParser, string)
        if (parse_result%ok) then
            select type (the_number => parse_result%parsed)
            type is (ParsedRational_t)
                volume = the_number%value_.unit.self
            end select
        else
            call errors%appendError(Fatal( &
                    PARSE_ERROR, &
                    Module_("Volume_m"), &
                    Procedure_("simpleParseAs"), &
                    parse_result%message))
        end if
    contains
        pure function theParser(state_) result(result_)
            type(State_t), intent(in) :: state_
            type(ParserOutput_t) :: result_

            result_ = thenDrop( &
                    thenDrop(parseRational, parseSpace, state_), &
                    parseUnit)
        end function theParser

        pure function parseUnit(state_) result(result_)
            type(State_t), intent(in) :: state_
            type(ParserOutput_t) :: result_

            result_ = parseString(trim(self%symbol), state_)
        end function parseUnit
    end subroutine simpleParseAs

    pure subroutine gnuplotParseAs(self, string, errors, volume)
        class(VolumeGnuplotUnit_t), intent(in) :: self
        type(VARYING_STRING), intent(in) :: string
        type(ErrorList_t), intent(out) :: errors
        type(Volume_t), intent(out) :: volume

        type(ParseResult_t) :: parse_result

        parse_result = parseWith(theParser, string)
        if (parse_result%ok) then
            select type (the_number => parse_result%parsed)
            type is (ParsedRational_t)
                volume = the_number%value_.unit.self
            end select
        else
            call errors%appendError(Fatal( &
                    PARSE_ERROR, &
                    Module_("Volume_m"), &
                    Procedure_("gnuplotParseAs"), &
                    parse_result%message))
        end if
    contains
        pure function theParser(state_) result(result_)
            type(State_t), intent(in) :: state_
            type(ParserOutput_t) :: result_

            result_ = thenDrop( &
                    thenDrop(parseRational, parseSpace, state_), &
                    parseUnit)
        end function theParser

        pure function parseUnit(state_) result(result_)
            type(State_t), intent(in) :: state_
            type(ParserOutput_t) :: result_

            result_ = parseString(trim(self%symbol), state_)
        end function parseUnit
    end subroutine gnuplotParseAs

    elemental function gnuplotUnitToString(self) result(string)
        class(VolumeGnuplotUnit_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = trim(self%symbol)
    end function gnuplotUnitToString

    pure function gnuplotValueToString(self, value_) result(string)
        class(VolumeGnuplotUnit_t), intent(in) :: self
        type(VARYING_STRING), intent(in) :: value_
        type(VARYING_STRING) :: string

        string = value_ // " " // self%toString()
    end function gnuplotValueToString

    pure subroutine simpleUnitFromStringC(string, errors, unit)
        character(len=*), intent(in) :: string
        type(ErrorList_t), intent(out) :: errors
        type(VolumeSimpleUnit_t), intent(out) :: unit

        type(ErrorList_t) :: errors_

        call fromString(var_str(string), PROVIDED_UNITS, errors_, unit)
        call errors%appendErrors( &
                errors_, &
                Module_("Volume_m"), &
                Procedure_("simpleUnitFromStringC"))
    end subroutine simpleUnitFromStringC

    pure subroutine simpleUnitFromStringS(string, errors, unit)
        type(VARYING_STRING), intent(in) :: string
        type(ErrorList_t), intent(out) :: errors
        type(VolumeSimpleUnit_t), intent(out) :: unit

        type(ErrorList_t) :: errors_

        call fromString(string, PROVIDED_UNITS, errors_, unit)
        call errors%appendErrors( &
                errors_, &
                Module_("Volume_m"), &
                Procedure_("simpleUnitFromStringS"))
    end subroutine simpleUnitFromStringS

    pure subroutine simpleUnitFromStringWithUnitsC(string, units, errors, unit)
        character(len=*), intent(in) :: string
        type(VolumeSimpleUnit_t), intent(in) :: units(:)
        type(ErrorList_t), intent(out) :: errors
        type(VolumeSimpleUnit_t), intent(out) :: unit

        type(ErrorList_t) :: errors_

        call fromString(var_str(string), units, errors_, unit)
        call errors%appendErrors( &
                errors_, &
                Module_("Volume_m"), &
                Procedure_("simpleUnitFromStringWithUnitsC"))
    end subroutine simpleUnitFromStringWithUnitsC

    pure subroutine simpleUnitFromStringWithUnitsS(string, units, errors, unit)
        type(VARYING_STRING), intent(in) :: string
        type(VolumeSimpleUnit_t), intent(in) :: units(:)
        type(ErrorList_t), intent(out) :: errors
        type(VolumeSimpleUnit_t), intent(out) :: unit

        type(ErrorList_t) :: errors_
        integer :: which_unit

        call selectUnit(string, units, errors, which_unit)
        if (errors_%hasAny()) then
            call errors%appendErrors( &
                    errors_, &
                    Module_("Volume_m"), &
                    Procedure_("simpleUnitFromStringWithUnitsS"))
        else
            unit = units(which_unit)
        end if
    end subroutine simpleUnitFromStringWithUnitsS

    pure subroutine selectUnit(string, units, errors, index)
        type(VARYING_STRING), intent(in) :: string
        class(VolumeUnit_t), intent(in) :: units(:)
        type(ErrorList_t), intent(out) :: errors
        integer, intent(out) :: index

        integer :: i
        type(VARYING_STRING) :: unit_strings(size(units))

        do i = 1, size(units)
            if (string == units(i)%toString()) then
                index = i
                return
            end if
        end do
        do i = 1, size(units)
            unit_strings(i) = units(i)%toString()
        end do
        call errors%appendError(Fatal( &
                UNKNOWN_UNIT, &
                Module_("Volume_m"), &
                Procedure_("selectUnit"), &
                '"' // string // '", known units: [' // join(unit_strings, ', ') // ']'))
    end subroutine selectUnit
end module Volume_m
