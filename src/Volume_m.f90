module Volume_m
    use Conversion_factors_m, only: CUBIC_CENTIMETERS_PER_CUBIC_METER

    implicit none
    private

    type, public :: Volume_t
        private
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
        procedure, public :: toString
        procedure, public :: toStringIn
    end type Volume_t

    type, public :: VolumeUnit_t
        double precision :: conversion_factor
        character(len=10) :: symbol
    contains
        procedure :: toString => unitToString
    end type VolumeUnit_t

    interface operator(.unit.)
        module procedure fromUnits
    end interface operator(.unit.)

    interface volumeFromString
        module procedure fromStringBasicC
        module procedure fromStringBasicS
        module procedure fromStringWithUnitsC
        module procedure fromStringWithUnitsS
    end interface volumeFromString

    interface volumeUnitFromString
        module procedure unitFromStringBasicC
        module procedure unitFromStringBasicS
        module procedure unitFromStringWithUnitsC
        module procedure unitFromStringWithUnitsS
    end interface volumeUnitFromString

    type(VolumeUnit_t), parameter, public :: CUBIC_CENTIMETERS = &
            VolumeUnit_t( &
                    conversion_factor = CUBIC_CENTIMETERS_PER_CUBIC_METER, &
                    symbol = "cm^3")
    type(VolumeUnit_t), parameter, public :: CUBIC_METERS = &
            VolumeUnit_t( &
                    conversion_factor = 1.0d0, &
                    symbol = "m^3")

    type(VolumeUnit_t), public :: DEFAULT_OUTPUT_UNITS = CUBIC_METERS

    type(VolumeUnit_t), parameter, public :: PROVIDED_UNITS(*) = &
            [CUBIC_CENTIMETERS, CUBIC_METERS]

    public :: &
            operator(.unit.), &
            volumeFromString, &
            volumeUnitFromString
contains
    function fromStringBasicC(string, errors) result(volume)
        use Error_list_m, only: ErrorList_t
        use iso_varying_string, only: var_str
        use Module_m, only: Module_
        use Procedure_m, only: Procedure_

        character(len=*), intent(in) :: string
        type(ErrorList_t), intent(out) :: errors
        type(Volume_t) :: volume

        type(ErrorList_t) :: errors_

        volume = volumeFromString( &
                var_str(string), PROVIDED_UNITS, errors_)
        call errors%appendErrors( &
                errors_, &
                Module_("Volume_m"), &
                Procedure_("fromStringBasicC"))
    end function fromStringBasicC

    function fromStringBasicS(string, errors) result(volume)
        use Error_list_m, only: ErrorList_t
        use iso_varying_string, only: VARYING_STRING
        use Module_m, only: Module_
        use Procedure_m, only: Procedure_

        type(VARYING_STRING), intent(in) :: string
        type(ErrorList_t), intent(out) :: errors
        type(Volume_t) :: volume

        type(ErrorList_t) :: errors_

        volume = volumeFromString( &
                string, PROVIDED_UNITS, errors_)
        call errors%appendErrors( &
                errors_, &
                Module_("Volume_m"), &
                Procedure_("fromStringBasicS"))
    end function fromStringBasicS

    function fromStringWithUnitsC(string, units, errors) result(volume)
        use Error_list_m, only: ErrorList_t
        use iso_varying_string, only: var_str
        use Module_m, only: Module_
        use Procedure_m, only: Procedure_

        character(len=*), intent(in) :: string
        type(VolumeUnit_t), intent(in) :: units(:)
        type(ErrorList_t), intent(out) :: errors
        type(Volume_t) :: volume

        type(ErrorList_t) :: errors_

        volume = volumeFromString( &
                var_str(string), units, errors_)
        call errors%appendErrors( &
                errors_, &
                Module_("Volume_m"), &
                Procedure_("fromStringWithUnitsC"))
    end function fromStringWithUnitsC

    function fromStringWithUnitsS(string, units, errors) result(volume)
        use Error_list_m, only: ErrorList_t
        use iso_varying_string, only: &
                VARYING_STRING, &
                assignment(=), &
                operator(//), &
                operator(==), &
                len, &
                split
        use Message_m, only: Fatal
        use Miscellaneous_m, only: PARSE_ERROR
        use Module_m, only: Module_
        use Procedure_m, only: Procedure_
        use strff, only: join

        type(VARYING_STRING), intent(in) :: string
        type(VolumeUnit_t), intent(in) :: units(:)
        type(ErrorList_t), intent(out) :: errors
        type(Volume_t) :: volume

        double precision :: number
        character(len=100) :: number_chars
        type(VARYING_STRING) :: number_string
        integer :: status
        type(VARYING_STRING) :: symbol
        type(VolumeUnit_t) :: unit
        type(ErrorList_t) :: unit_errors

        volume%cubic_meters = 0.0d0
        symbol = string
        call split(symbol, number_string, " ")
        if (len(symbol) == 0) then
            call errors%appendError(Fatal( &
                    PARSE_ERROR, &
                    Module_("Volume_m"), &
                    Procedure_("fromStringWithUnitsS"), &
                    'No unit symbol found in string "' // string // '"'))
            return
        end if
        number_chars = number_string
        read(number_chars, *, iostat=status) number
        if (status /= 0) then
            call errors%appendError(Fatal( &
                    PARSE_ERROR, &
                    Module_("Volume_m"), &
                    Procedure_("fromStringWithUnitsS"), &
                    'Error parsing number from string "' // number_string // '"'))
        end if
        unit = volumeUnitFromString(symbol, units, unit_errors)
        volume = number.unit.unit
        call errors%appendErrors( &
                unit_errors, &
                Module_("Volume_m"), &
                Procedure_("fromStringWithUnitsS"))
    end function fromStringWithUnitsS

    elemental function fromUnits(value_, units) result(volume)
        double precision, intent(in) :: value_
        type(VolumeUnit_t), intent(in) :: units
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
        use Miscellaneous_m, only: operator(.safeEq.)

        class(Volume_t), intent(in) :: lhs
        class(Volume_t), intent(in) :: rhs
        logical :: equal_

        equal_ = lhs%cubic_meters .safeEq. rhs%cubic_meters
    end function equal_

    function equalWithinAbsolute(lhs, rhs, within)
        use Miscellaneous_m, only: equalWithinAbsolute_ => equalWithinAbsolute

        class(Volume_t), intent(in) :: lhs
        class(Volume_t), intent(in) :: rhs
        class(Volume_t), intent(in) :: within
        logical :: equalWithinAbsolute

        equalWithinAbsolute = equalWithinAbsolute_( &
                lhs%cubic_meters, rhs%cubic_meters, within%cubic_meters)
    end function equalWithinAbsolute

    function equalWithinRelative(lhs, rhs, within)
        use Miscellaneous_m, only: equalWithinRelative_ => equalWithinRelative

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

    function toString(self) result(string)
        use iso_varying_string, only: VARYING_STRING

        class(Volume_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = self%toStringIn(DEFAULT_OUTPUT_UNITS)
    end function toString

    function toStringIn(self, units) result(string)
        use iso_varying_string, only: VARYING_STRING, operator(//)
        use strff, only: toString

        class(Volume_t), intent(in) :: self
        class(VolumeUnit_t), intent(in) :: units
        type(VARYING_STRING) :: string

        string = toString(self.in.units) // " " // units%toString()
    end function toStringIn

    function unitFromStringBasicC(string, errors) result(unit)
        use Error_list_m, only: ErrorList_t
        use iso_varying_string, only: var_str
        use Module_m, only: Module_
        use Procedure_m, only: Procedure_

        character(len=*), intent(in) :: string
        type(ErrorList_t), intent(out) :: errors
        type(VolumeUnit_t) :: unit

        type(ErrorList_t) :: errors_

        unit = volumeUnitFromString( &
                var_str(string), PROVIDED_UNITS, errors_)
        call errors%appendErrors( &
                errors_, &
                Module_("Volume_m"), &
                Procedure_("unitFromStringBasicC"))
    end function unitFromStringBasicC

    function unitFromStringBasicS(string, errors) result(unit)
        use Error_list_m, only: ErrorList_t
        use iso_varying_string, only: VARYING_STRING
        use Module_m, only: Module_
        use Procedure_m, only: Procedure_

        type(VARYING_STRING), intent(in) :: string
        type(ErrorList_t), intent(out) :: errors
        type(VolumeUnit_t) :: unit

        type(ErrorList_t) :: errors_

        unit = volumeUnitFromString( &
                string, PROVIDED_UNITS, errors_)
        call errors%appendErrors( &
                errors_, &
                Module_("Volume_m"), &
                Procedure_("unitFromStringBasicS"))
    end function unitFromStringBasicS

    function unitFromStringWithUnitsC(string, units, errors) result(unit)
        use Error_list_m, only: ErrorList_t
        use iso_varying_string, only: var_str
        use Module_m, only: Module_
        use Procedure_m, only: Procedure_

        character(len=*), intent(in) :: string
        type(VolumeUnit_t), intent(in) :: units(:)
        type(ErrorList_t), intent(out) :: errors
        type(VolumeUnit_t) :: unit

        type(ErrorList_t) :: errors_

        unit = volumeUnitFromString(var_str(string), units, errors_)
        call errors%appendErrors( &
                errors_, &
                Module_("Volume_m"), &
                Procedure_("unitFromStringWithUnitsC"))
    end function unitFromStringWithUnitsC

    function unitFromStringWithUnitsS(string, units, errors) result(unit)
        use Error_list_m, only: ErrorList_t
        use iso_varying_string, only: VARYING_STRING, operator(==), operator(//)
        use Message_m, only: Fatal
        use Miscellaneous_m, only: UNKNOWN_UNIT
        use Module_m, only: Module_
        use Procedure_m, only: Procedure_
        use strff, only: join

        type(VARYING_STRING), intent(in) :: string
        type(VolumeUnit_t), intent(in) :: units(:)
        type(ErrorList_t), intent(out) :: errors
        type(VolumeUnit_t) :: unit

        integer :: i
        type(VARYING_STRING) :: unit_strings(size(units))

        do i = 1, size(units)
            if (string == units(i)%symbol) then
                unit = units(i)
                exit
            end if
        end do
        if (i > size(units)) then
            do i = 1, size(units)
                unit_strings(i) = units(i)%toString()
            end do
            call errors%appendError(Fatal( &
                    UNKNOWN_UNIT, &
                    Module_("Volume_m"), &
                    Procedure_("unitFromStringWithUnitsS"), &
                    '"' // string // '", known units: [' // join(unit_strings, ', ') // ']' ))
        end if
    end function unitFromStringWithUnitsS

    function unitToString(self) result(string)
        use iso_varying_string, only: VARYING_STRING, assignment(=)

        class(VolumeUnit_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = trim(self%symbol)
    end function unitToString
end module Volume_m
