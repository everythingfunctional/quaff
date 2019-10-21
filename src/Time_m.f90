module Time_m
    use Conversion_factors_m, only: HOURS_PER_SECOND

    implicit none
    private

    type, public :: Time_t
        private
        double precision :: seconds
    contains
        private
        procedure :: toUnits
        generic, public :: operator(.in.) => toUnits
        procedure, pass(time) :: doubleTimesTime
        procedure, pass(time) :: integerTimesTime
        procedure, pass(time) :: timeTimesDouble
        procedure, pass(time) :: timeTimesInteger
        generic, public :: operator(*) => &
                doubleTimesTime, &
                integerTimesTime, &
                timeTimesDouble, &
                timeTimesInteger
        procedure :: timeDividedByDouble
        procedure :: timeDividedByInteger
        procedure, pass(numerator) :: timeDividedByTime
        generic, public :: operator(/) => &
                timeDividedByDouble, &
                timeDividedByInteger, &
                timeDividedByTime
        procedure :: timePlusTime
        generic, public :: operator(+) => timePlusTime
        procedure :: timeMinusTime
        generic, public :: operator(-) => timeMinusTime
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
    end type Time_t

    type, public :: TimeUnit_t
        double precision :: conversion_factor
        character(len=10) :: symbol
    contains
        procedure :: toString => unitToString
    end type TimeUnit_t

    interface operator(.unit.)
        module procedure fromUnits
    end interface operator(.unit.)

    interface timeFromString
        module procedure fromStringBasicC
        module procedure fromStringBasicS
        module procedure fromStringWithUnitsC
        module procedure fromStringWithUnitsS
    end interface timeFromString

    interface timeUnitFromString
        module procedure unitFromStringBasicC
        module procedure unitFromStringBasicS
        module procedure unitFromStringWithUnitsC
        module procedure unitFromStringWithUnitsS
    end interface timeUnitFromString

    type(TimeUnit_t), parameter, public :: SECONDS = &
            TimeUnit_t( &
                    conversion_factor = 1.0d0, &
                    symbol = "s")
    type(TimeUnit_t), parameter, public :: HOURS = &
            TimeUnit_t( &
                    conversion_factor = HOURS_PER_SECOND, &
                    symbol = "hr")

    type(TimeUnit_t), public :: DEFAULT_OUTPUT_UNITS = SECONDS

    type(TimeUnit_t), parameter, public :: PROVIDED_UNITS(*) = &
            [HOURS, SECONDS]

    public :: &
            operator(.unit.), &
            timeFromString, &
            timeUnitFromString
contains
    function fromStringBasicC(string, errors) result(time)
        use Error_list_m, only: ErrorList_t
        use iso_varying_string, only: var_str
        use Module_m, only: Module_
        use Procedure_m, only: Procedure_

        character(len=*), intent(in) :: string
        type(ErrorList_t), intent(out) :: errors
        type(Time_t) :: time

        type(ErrorList_t) :: errors_

        time = timeFromString( &
                var_str(string), PROVIDED_UNITS, errors_)
        call errors%appendErrors( &
                errors_, &
                Module_("Time_m"), &
                Procedure_("fromStringBasicC"))
    end function fromStringBasicC

    function fromStringBasicS(string, errors) result(time)
        use Error_list_m, only: ErrorList_t
        use iso_varying_string, only: VARYING_STRING
        use Module_m, only: Module_
        use Procedure_m, only: Procedure_

        type(VARYING_STRING), intent(in) :: string
        type(ErrorList_t), intent(out) :: errors
        type(Time_t) :: time

        type(ErrorList_t) :: errors_

        time = timeFromString( &
                string, PROVIDED_UNITS, errors_)
        call errors%appendErrors( &
                errors_, &
                Module_("Time_m"), &
                Procedure_("fromStringBasicS"))
    end function fromStringBasicS

    function fromStringWithUnitsC(string, units, errors) result(time)
        use Error_list_m, only: ErrorList_t
        use iso_varying_string, only: var_str
        use Module_m, only: Module_
        use Procedure_m, only: Procedure_

        character(len=*), intent(in) :: string
        type(TimeUnit_t), intent(in) :: units(:)
        type(ErrorList_t), intent(out) :: errors
        type(Time_t) :: time

        type(ErrorList_t) :: errors_

        time = timeFromString( &
                var_str(string), units, errors_)
        call errors%appendErrors( &
                errors_, &
                Module_("Time_m"), &
                Procedure_("fromStringWithUnitsC"))
    end function fromStringWithUnitsC

    function fromStringWithUnitsS(string, units, errors) result(time)
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
        type(TimeUnit_t), intent(in) :: units(:)
        type(ErrorList_t), intent(out) :: errors
        type(Time_t) :: time

        double precision :: number
        character(len=100) :: number_chars
        type(VARYING_STRING) :: number_string
        integer :: status
        type(VARYING_STRING) :: symbol
        type(TimeUnit_t) :: unit
        type(ErrorList_t) :: unit_errors

        time%seconds = 0.0d0
        symbol = string
        call split(symbol, number_string, " ")
        if (len(symbol) == 0) then
            call errors%appendError(Fatal( &
                    PARSE_ERROR, &
                    Module_("Time_m"), &
                    Procedure_("fromStringWithUnitsS"), &
                    'No unit symbol found in string "' // string // '"'))
            return
        end if
        number_chars = number_string
        read(number_chars, *, iostat=status) number
        if (status /= 0) then
            call errors%appendError(Fatal( &
                    PARSE_ERROR, &
                    Module_("Time_m"), &
                    Procedure_("fromStringWithUnitsS"), &
                    'Error parsing number from string "' // number_string // '"'))
        end if
        unit = timeUnitFromString(symbol, units, unit_errors)
        time = number.unit.unit
        call errors%appendErrors( &
                unit_errors, &
                Module_("Time_m"), &
                Procedure_("fromStringWithUnitsS"))
    end function fromStringWithUnitsS

    function fromUnits(value_, units) result(time)
        double precision, intent(in) :: value_
        type(TimeUnit_t), intent(in) :: units
        type(Time_t) :: time

        time%seconds = value_ / units%conversion_factor
    end function fromUnits

    function toUnits(self, units) result(time)
        class(Time_t), intent(in) :: self
        class(TimeUnit_t), intent(in) :: units
        double precision :: time

        time = self%seconds * units%conversion_factor
    end function toUnits

    function doubleTimesTime( &
            multiplier, time) result(new_time)
        double precision, intent(in) :: multiplier
        class(Time_t), intent(in) :: time
        type(Time_t) :: new_time

        new_time%seconds = &
                multiplier * time%seconds
    end function doubleTimesTime

    function integerTimesTime( &
            multiplier, time) result(new_time)
        integer, intent(in) :: multiplier
        class(Time_t), intent(in) :: time
        type(Time_t) :: new_time

        new_time%seconds = &
                dble(multiplier) * time%seconds
    end function integerTimesTime

    function timeTimesDouble( &
            time, multiplier) result(new_time)
        class(Time_t), intent(in) :: time
        double precision, intent(in) :: multiplier
        type(Time_t) :: new_time

        new_time%seconds = &
                time%seconds * multiplier
    end function timeTimesDouble

    function timeTimesInteger( &
            time, multiplier) result(new_time)
        class(Time_t), intent(in) :: time
        integer, intent(in) :: multiplier
        type(Time_t) :: new_time

        new_time%seconds = &
                time%seconds * dble(multiplier)
    end function timeTimesInteger

    function timeDividedByDouble( &
            time, divisor) result(new_time)
        class(Time_t), intent(in) :: time
        double precision, intent(in) :: divisor
        type(Time_t) :: new_time

        new_time%seconds = &
                time%seconds / divisor
    end function timeDividedByDouble

    function timeDividedByInteger( &
            time, divisor) result(new_time)
        class(Time_t), intent(in) :: time
        integer, intent(in) :: divisor
        type(Time_t) :: new_time

        new_time%seconds = &
                time%seconds / dble(divisor)
    end function timeDividedByInteger

    function timeDividedByTime( &
            numerator, denomenator) result(ratio)
        class(Time_t), intent(in) :: numerator
        class(Time_t), intent(in) :: denomenator
        double precision :: ratio

        ratio = numerator%seconds / denomenator%seconds
    end function timeDividedByTime

    function timePlusTime( &
            time1, time2) result(new_time)
        class(Time_t), intent(in) :: time1
        class(Time_t), intent(in) :: time2
        type(Time_t) :: new_time

        new_time%seconds = &
                time1%seconds + time2%seconds
    end function timePlusTime

    function timeMinusTime( &
            time1, time2) result(new_time)
        class(Time_t), intent(in) :: time1
        class(Time_t), intent(in) :: time2
        type(Time_t) :: new_time

        new_time%seconds = &
                time1%seconds - time2%seconds
    end function timeMinusTime

    function greaterThan(lhs, rhs)
        class(Time_t), intent(in) :: lhs
        class(Time_t), intent(in) :: rhs
        logical :: greaterThan

        greaterThan = lhs%seconds > rhs%seconds
    end function greaterThan

    function lessThan(lhs,rhs)
        class(Time_t), intent(in) :: lhs
        class(Time_t), intent(in) :: rhs
        logical :: lessThan

        lessThan = lhs%seconds < rhs%seconds
    end function lessThan

    function greaterThanOrEqual(lhs, rhs)
        class(Time_t), intent(in) :: lhs
        class(Time_t), intent(in) :: rhs
        logical :: greaterThanOrEqual

        greaterThanOrEqual = lhs%seconds >= rhs%seconds
    end function greaterThanOrEqual

    function lessThanOrEqual(lhs, rhs)
        class(Time_t), intent(in) :: lhs
        class(Time_t), intent(in) :: rhs
        logical :: lessThanOrEqual

        lessThanOrEqual = lhs%seconds <= rhs%seconds
    end function lessThanOrEqual

    function equal_(lhs,rhs)
        use Miscellaneous_m, only: operator(.safeEq.)

        class(Time_t), intent(in) :: lhs
        class(Time_t), intent(in) :: rhs
        logical :: equal_

        equal_ = lhs%seconds .safeEq. rhs%seconds
    end function equal_

    function equalWithinAbsolute(lhs, rhs, within)
        use Miscellaneous_m, only: equalWithinAbsolute_ => equalWithinAbsolute

        class(Time_t), intent(in) :: lhs
        class(Time_t), intent(in) :: rhs
        class(Time_t), intent(in) :: within
        logical :: equalWithinAbsolute

        equalWithinAbsolute = equalWithinAbsolute_( &
                lhs%seconds, rhs%seconds, within%seconds)
    end function equalWithinAbsolute

    function equalWithinRelative(lhs, rhs, within)
        use Miscellaneous_m, only: equalWithinRelative_ => equalWithinRelative

        class(Time_t), intent(in) :: lhs
        class(Time_t), intent(in) :: rhs
        double precision, intent(in) :: within
        logical :: equalWithinRelative

        equalWithinRelative = equalWithinRelative_( &
                lhs%seconds, rhs%seconds, within)
    end function equalWithinRelative

    function notEqual(lhs, rhs)
        class(Time_t), intent(in) :: lhs
        class(Time_t), intent(in) :: rhs
        logical :: notEqual

        notEqual = .not. lhs == rhs
    end function notEqual

    function toString(self) result(string)
        use iso_varying_string, only: VARYING_STRING

        class(Time_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = self%toStringIn(DEFAULT_OUTPUT_UNITS)
    end function toString

    function toStringIn(self, units) result(string)
        use iso_varying_string, only: VARYING_STRING, operator(//)
        use strff, only: toString

        class(Time_t), intent(in) :: self
        class(TimeUnit_t), intent(in) :: units
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
        type(TimeUnit_t) :: unit

        type(ErrorList_t) :: errors_

        unit = timeUnitFromString( &
                var_str(string), PROVIDED_UNITS, errors_)
        call errors%appendErrors( &
                errors_, &
                Module_("Time_m"), &
                Procedure_("unitFromStringBasicC"))
    end function unitFromStringBasicC

    function unitFromStringBasicS(string, errors) result(unit)
        use Error_list_m, only: ErrorList_t
        use iso_varying_string, only: VARYING_STRING
        use Module_m, only: Module_
        use Procedure_m, only: Procedure_

        type(VARYING_STRING), intent(in) :: string
        type(ErrorList_t), intent(out) :: errors
        type(TimeUnit_t) :: unit

        type(ErrorList_t) :: errors_

        unit = timeUnitFromString( &
                string, PROVIDED_UNITS, errors_)
        call errors%appendErrors( &
                errors_, &
                Module_("Time_m"), &
                Procedure_("unitFromStringBasicS"))
    end function unitFromStringBasicS

    function unitFromStringWithUnitsC(string, units, errors) result(unit)
        use Error_list_m, only: ErrorList_t
        use iso_varying_string, only: var_str
        use Module_m, only: Module_
        use Procedure_m, only: Procedure_

        character(len=*), intent(in) :: string
        type(TimeUnit_t), intent(in) :: units(:)
        type(ErrorList_t), intent(out) :: errors
        type(TimeUnit_t) :: unit

        type(ErrorList_t) :: errors_

        unit = timeUnitFromString(var_str(string), units, errors_)
        call errors%appendErrors( &
                errors_, &
                Module_("Time_m"), &
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
        type(TimeUnit_t), intent(in) :: units(:)
        type(ErrorList_t), intent(out) :: errors
        type(TimeUnit_t) :: unit

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
                    Module_("Time_m"), &
                    Procedure_("unitFromStringWithUnitsS"), &
                    '"' // string // '", known units: [' // join(unit_strings, ', ') // ']' ))
        end if
    end function unitFromStringWithUnitsS

    function unitToString(self) result(string)
        use iso_varying_string, only: VARYING_STRING, assignment(=)

        class(TimeUnit_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = trim(self%symbol)
    end function unitToString
end module Time_m
