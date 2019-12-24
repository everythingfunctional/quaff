module Time_m
    use Conversion_factors_m, only: &
            DAYS_PER_SECOND, HOURS_PER_SECOND, MINUTES_PER_SECOND
    use erloff, only: ErrorList_t, Fatal, Module_, Procedure_
    use iso_varying_string, only: &
            VARYING_STRING, &
            assignment(=), &
            operator(==), &
            operator(//), &
            len, &
            split, &
            var_str
    use Miscellaneous_m, only: &
            operator(.safeEq.), &
            equalWithinAbsolute_ => equalWithinAbsolute, &
            equalWithinRelative_ => equalWithinRelative, &
            wrapInLatexQuantity, &
            PARSE_ERROR, &
            UNKNOWN_UNIT
    use strff, only: join, toString

    implicit none
    private

    type, public :: Time_t
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
        procedure :: toStringFullPrecision
        procedure :: toStringWithPrecision
        generic, public :: toString => &
                toStringFullPrecision, toStringWithPrecision
        procedure :: toStringInFullPrecision
        procedure :: toStringInWithPrecision
        generic, public :: toStringIn => &
                toStringInFullPrecision, toStringInWithPrecision
        procedure :: toGnuplotStringFullPrecision
        procedure :: toGnuplotStringWithPrecision
        generic, public :: toGnuplotString => &
                toGnuplotStringFullPrecision, toGnuplotStringWithPrecision
        procedure :: toGnuplotStringInFullPrecision
        procedure :: toGnuplotStringInWithPrecision
        generic, public :: toGnuplotStringIn => &
                toGnuplotStringInFullPrecision, toGnuplotStringInWithPrecision
        procedure :: toLatexStringFullPrecision
        procedure :: toLatexStringWithPrecision
        generic, public :: toLatexString => &
                toLatexStringFullPrecision, toLatexStringWithPrecision
        procedure :: toLatexStringInFullPrecision
        procedure :: toLatexStringInWithPrecision
        generic, public :: toLatexStringIn => &
                toLatexStringInFullPrecision, toLatexStringInWithPrecision
    end type Time_t

    type, public :: TimeUnit_t
        double precision :: conversion_factor
        character(len=10) :: symbol
        character(len=50) :: gnuplot_symbol
        character(len=100) :: latex_symbol
    contains
        procedure :: toString => unitToString
        procedure :: toGnuplotString => unitToGnuplotString
        procedure :: toLatexString => unitToLatexString
    end type TimeUnit_t

    interface operator(.unit.)
        module procedure fromUnits
    end interface operator(.unit.)

    interface fromString
        module procedure fromStringBasicC
        module procedure fromStringBasicS
        module procedure fromStringWithUnitsC
        module procedure fromStringWithUnitsS
        module procedure unitFromStringBasicC
        module procedure unitFromStringBasicS
        module procedure unitFromStringWithUnitsC
        module procedure unitFromStringWithUnitsS
    end interface fromString

    interface sum
        module procedure sumTime
    end interface sum

    type(TimeUnit_t), parameter, public :: DAYS = &
            TimeUnit_t( &
                    conversion_factor = DAYS_PER_SECOND, &
                    symbol = "d", &
                    gnuplot_symbol = "d", &
                    latex_symbol = "\day")
    type(TimeUnit_t), parameter, public :: HOURS = &
            TimeUnit_t( &
                    conversion_factor = HOURS_PER_SECOND, &
                    symbol = "hr", &
                    gnuplot_symbol = "hr", &
                    latex_symbol = "\hour")
    type(TimeUnit_t), parameter, public :: MINUTES = &
            TimeUnit_t( &
                    conversion_factor = MINUTES_PER_SECOND, &
                    symbol = "min", &
                    gnuplot_symbol = "min", &
                    latex_symbol = "\minute")
    type(TimeUnit_t), parameter, public :: SECONDS = &
            TimeUnit_t( &
                    conversion_factor = 1.0d0, &
                    symbol = "s", &
                    gnuplot_symbol = "s", &
                    latex_symbol = "\second")

    type(TimeUnit_t), public :: DEFAULT_OUTPUT_UNITS = SECONDS

    type(TimeUnit_t), parameter, public :: PROVIDED_UNITS(*) = &
            [DAYS, HOURS, MINUTES, SECONDS]

    public :: operator(.unit.), fromString, sum
contains
    pure subroutine fromStringBasicC(string, errors, time)
        character(len=*), intent(in) :: string
        type(ErrorList_t), intent(out) :: errors
        type(Time_t), intent(out) :: time

        type(ErrorList_t) :: errors_

        call fromString( &
                var_str(string), PROVIDED_UNITS, errors_, time)
        call errors%appendErrors( &
                errors_, &
                Module_("Time_m"), &
                Procedure_("fromStringBasicC"))
    end subroutine fromStringBasicC

    pure subroutine fromStringBasicS(string, errors, time)
        type(VARYING_STRING), intent(in) :: string
        type(ErrorList_t), intent(out) :: errors
        type(Time_t), intent(out) :: time

        type(ErrorList_t) :: errors_

        call fromString( &
                string, PROVIDED_UNITS, errors_, time)
        call errors%appendErrors( &
                errors_, &
                Module_("Time_m"), &
                Procedure_("fromStringBasicS"))
    end subroutine fromStringBasicS

    pure subroutine fromStringWithUnitsC(string, units, errors, time)
        character(len=*), intent(in) :: string
        type(TimeUnit_t), intent(in) :: units(:)
        type(ErrorList_t), intent(out) :: errors
        type(Time_t), intent(out) :: time

        type(ErrorList_t) :: errors_

        call fromString( &
                var_str(string), units, errors_, time)
        call errors%appendErrors( &
                errors_, &
                Module_("Time_m"), &
                Procedure_("fromStringWithUnitsC"))
    end subroutine fromStringWithUnitsC

    pure subroutine fromStringWithUnitsS(string, units, errors, time)
        type(VARYING_STRING), intent(in) :: string
        type(TimeUnit_t), intent(in) :: units(:)
        type(ErrorList_t), intent(out) :: errors
        type(Time_t), intent(out) :: time

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
        call fromString(symbol, units, unit_errors, unit)
        time = number.unit.unit
        call errors%appendErrors( &
                unit_errors, &
                Module_("Time_m"), &
                Procedure_("fromStringWithUnitsS"))
    end subroutine fromStringWithUnitsS

    elemental function fromUnits(value_, units) result(time)
        double precision, intent(in) :: value_
        type(TimeUnit_t), intent(in) :: units
        type(Time_t) :: time

        time%seconds = value_ / units%conversion_factor
    end function fromUnits

    elemental function toUnits(self, units) result(time)
        class(Time_t), intent(in) :: self
        class(TimeUnit_t), intent(in) :: units
        double precision :: time

        time = self%seconds * units%conversion_factor
    end function toUnits

    elemental function doubleTimesTime( &
            multiplier, time) result(new_time)
        double precision, intent(in) :: multiplier
        class(Time_t), intent(in) :: time
        type(Time_t) :: new_time

        new_time%seconds = &
                multiplier * time%seconds
    end function doubleTimesTime

    elemental function integerTimesTime( &
            multiplier, time) result(new_time)
        integer, intent(in) :: multiplier
        class(Time_t), intent(in) :: time
        type(Time_t) :: new_time

        new_time%seconds = &
                dble(multiplier) * time%seconds
    end function integerTimesTime

    elemental function timeTimesDouble( &
            time, multiplier) result(new_time)
        class(Time_t), intent(in) :: time
        double precision, intent(in) :: multiplier
        type(Time_t) :: new_time

        new_time%seconds = &
                time%seconds * multiplier
    end function timeTimesDouble

    elemental function timeTimesInteger( &
            time, multiplier) result(new_time)
        class(Time_t), intent(in) :: time
        integer, intent(in) :: multiplier
        type(Time_t) :: new_time

        new_time%seconds = &
                time%seconds * dble(multiplier)
    end function timeTimesInteger

    elemental function timeDividedByDouble( &
            time, divisor) result(new_time)
        class(Time_t), intent(in) :: time
        double precision, intent(in) :: divisor
        type(Time_t) :: new_time

        new_time%seconds = &
                time%seconds / divisor
    end function timeDividedByDouble

    elemental function timeDividedByInteger( &
            time, divisor) result(new_time)
        class(Time_t), intent(in) :: time
        integer, intent(in) :: divisor
        type(Time_t) :: new_time

        new_time%seconds = &
                time%seconds / dble(divisor)
    end function timeDividedByInteger

    elemental function timeDividedByTime( &
            numerator, denomenator) result(ratio)
        class(Time_t), intent(in) :: numerator
        class(Time_t), intent(in) :: denomenator
        double precision :: ratio

        ratio = numerator%seconds / denomenator%seconds
    end function timeDividedByTime

    elemental function timePlusTime( &
            time1, time2) result(new_time)
        class(Time_t), intent(in) :: time1
        class(Time_t), intent(in) :: time2
        type(Time_t) :: new_time

        new_time%seconds = &
                time1%seconds + time2%seconds
    end function timePlusTime

    elemental function timeMinusTime( &
            time1, time2) result(new_time)
        class(Time_t), intent(in) :: time1
        class(Time_t), intent(in) :: time2
        type(Time_t) :: new_time

        new_time%seconds = &
                time1%seconds - time2%seconds
    end function timeMinusTime

    pure function sumTime(times)
        type(Time_t), intent(in) :: times(:)
        type(Time_t) :: sumTime

        sumTime%seconds = sum(times%seconds)
    end function sumTime

    elemental function greaterThan(lhs, rhs)
        class(Time_t), intent(in) :: lhs
        class(Time_t), intent(in) :: rhs
        logical :: greaterThan

        greaterThan = lhs%seconds > rhs%seconds
    end function greaterThan

    elemental function lessThan(lhs,rhs)
        class(Time_t), intent(in) :: lhs
        class(Time_t), intent(in) :: rhs
        logical :: lessThan

        lessThan = lhs%seconds < rhs%seconds
    end function lessThan

    elemental function greaterThanOrEqual(lhs, rhs)
        class(Time_t), intent(in) :: lhs
        class(Time_t), intent(in) :: rhs
        logical :: greaterThanOrEqual

        greaterThanOrEqual = lhs%seconds >= rhs%seconds
    end function greaterThanOrEqual

    elemental function lessThanOrEqual(lhs, rhs)
        class(Time_t), intent(in) :: lhs
        class(Time_t), intent(in) :: rhs
        logical :: lessThanOrEqual

        lessThanOrEqual = lhs%seconds <= rhs%seconds
    end function lessThanOrEqual

    elemental function equal_(lhs,rhs)
        class(Time_t), intent(in) :: lhs
        class(Time_t), intent(in) :: rhs
        logical :: equal_

        equal_ = lhs%seconds .safeEq. rhs%seconds
    end function equal_

    elemental function equalWithinAbsolute(lhs, rhs, within)
        class(Time_t), intent(in) :: lhs
        class(Time_t), intent(in) :: rhs
        class(Time_t), intent(in) :: within
        logical :: equalWithinAbsolute

        equalWithinAbsolute = equalWithinAbsolute_( &
                lhs%seconds, rhs%seconds, within%seconds)
    end function equalWithinAbsolute

    elemental function equalWithinRelative(lhs, rhs, within)
        class(Time_t), intent(in) :: lhs
        class(Time_t), intent(in) :: rhs
        double precision, intent(in) :: within
        logical :: equalWithinRelative

        equalWithinRelative = equalWithinRelative_( &
                lhs%seconds, rhs%seconds, within)
    end function equalWithinRelative

    elemental function notEqual(lhs, rhs)
        class(Time_t), intent(in) :: lhs
        class(Time_t), intent(in) :: rhs
        logical :: notEqual

        notEqual = .not. lhs == rhs
    end function notEqual

    elemental function toStringFullPrecision(self) result(string)
        class(Time_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = self%toStringIn(DEFAULT_OUTPUT_UNITS)
    end function toStringFullPrecision

    elemental function toStringWithPrecision(self, significant_digits) result(string)
        class(Time_t), intent(in) :: self
        integer, intent(in) :: significant_digits
        type(VARYING_STRING) :: string

        string = self%toStringIn(DEFAULT_OUTPUT_UNITS, significant_digits)
    end function toStringWithPrecision

    elemental function toStringInFullPrecision(self, units) result(string)
        class(Time_t), intent(in) :: self
        class(TimeUnit_t), intent(in) :: units
        type(VARYING_STRING) :: string

        string = toString(self.in.units) // " " // units%toString()
    end function toStringInFullPrecision

    elemental function toStringInWithPrecision( &
            self, units, significant_digits) result(string)
        class(Time_t), intent(in) :: self
        class(TimeUnit_t), intent(in) :: units
        integer, intent(in) :: significant_digits
        type(VARYING_STRING) :: string

        string = &
                toString(self.in.units, significant_digits) &
                // " " // units%toString()
    end function toStringInWithPrecision

    elemental function toGnuplotStringFullPrecision(self) result(string)
        class(Time_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = self%toGnuplotStringIn(DEFAULT_OUTPUT_UNITS)
    end function toGnuplotStringFullPrecision

    elemental function toGnuplotStringWithPrecision( &
            self, significant_digits) result(string)
        class(Time_t), intent(in) :: self
        integer, intent(in) :: significant_digits
        type(VARYING_STRING) :: string

        string = self%toGnuplotStringIn( &
                DEFAULT_OUTPUT_UNITS, significant_digits)
    end function toGnuplotStringWithPrecision

    elemental function toGnuplotStringInFullPrecision(self, units) result(string)
        class(Time_t), intent(in) :: self
        class(TimeUnit_t), intent(in) :: units
        type(VARYING_STRING) :: string

        string = toString(self.in.units) // " " // units%toGnuplotString()
    end function toGnuplotStringInFullPrecision

    elemental function toGnuplotStringInWithPrecision( &
            self, units, significant_digits) result(string)
        class(Time_t), intent(in) :: self
        class(TimeUnit_t), intent(in) :: units
        integer, intent(in) :: significant_digits
        type(VARYING_STRING) :: string

        string = &
                toString(self.in.units, significant_digits) &
                // " " // units%toGnuplotString()
    end function toGnuplotStringInWithPrecision

    elemental function toLatexStringFullPrecision(self) result(string)
        class(Time_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = self%toLatexStringIn(DEFAULT_OUTPUT_UNITS)
    end function toLatexStringFullPrecision

    elemental function toLatexStringWithPrecision(self, significant_digits) result(string)
        class(Time_t), intent(in) :: self
        integer, intent(in) :: significant_digits
        type(VARYING_STRING) :: string

        string = self%toLatexStringIn(DEFAULT_OUTPUT_UNITS, significant_digits)
    end function toLatexStringWithPrecision

    elemental function toLatexStringInFullPrecision(self, units) result(string)
        class(Time_t), intent(in) :: self
        class(TimeUnit_t), intent(in) :: units
        type(VARYING_STRING) :: string

        string = wrapInLatexQuantity( &
                toString(self.in.units), units%toLatexString())
    end function toLatexStringInFullPrecision

    elemental function toLatexStringInWithPrecision( &
            self, units, significant_digits) result(string)
        class(Time_t), intent(in) :: self
        class(TimeUnit_t), intent(in) :: units
        integer, intent(in) :: significant_digits
        type(VARYING_STRING) :: string

        string = wrapInLatexQuantity( &
                toString(self.in.units, significant_digits), &
                units%toLatexString())
    end function toLatexStringInWithPrecision

    pure subroutine unitFromStringBasicC(string, errors, unit)
        character(len=*), intent(in) :: string
        type(ErrorList_t), intent(out) :: errors
        type(TimeUnit_t), intent(out) :: unit

        type(ErrorList_t) :: errors_

        call fromString( &
                var_str(string), PROVIDED_UNITS, errors_, unit)
        call errors%appendErrors( &
                errors_, &
                Module_("Time_m"), &
                Procedure_("unitFromStringBasicC"))
    end subroutine unitFromStringBasicC

    pure subroutine unitFromStringBasicS(string, errors, unit)
        type(VARYING_STRING), intent(in) :: string
        type(ErrorList_t), intent(out) :: errors
        type(TimeUnit_t), intent(out) :: unit

        type(ErrorList_t) :: errors_

        call fromString( &
                string, PROVIDED_UNITS, errors_, unit)
        call errors%appendErrors( &
                errors_, &
                Module_("Time_m"), &
                Procedure_("unitFromStringBasicS"))
    end subroutine unitFromStringBasicS

    pure subroutine unitFromStringWithUnitsC(string, units, errors, unit)
        character(len=*), intent(in) :: string
        type(TimeUnit_t), intent(in) :: units(:)
        type(ErrorList_t), intent(out) :: errors
        type(TimeUnit_t), intent(out) :: unit

        type(ErrorList_t) :: errors_

        call fromString(var_str(string), units, errors_, unit)
        call errors%appendErrors( &
                errors_, &
                Module_("Time_m"), &
                Procedure_("unitFromStringWithUnitsC"))
    end subroutine unitFromStringWithUnitsC

    pure subroutine unitFromStringWithUnitsS(string, units, errors, unit)
        type(VARYING_STRING), intent(in) :: string
        type(TimeUnit_t), intent(in) :: units(:)
        type(ErrorList_t), intent(out) :: errors
        type(TimeUnit_t), intent(out) :: unit

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
    end subroutine unitFromStringWithUnitsS

    elemental function unitToString(self) result(string)
        class(TimeUnit_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = trim(self%symbol)
    end function unitToString

    elemental function unitToGnuplotString(self) result(string)
        class(TimeUnit_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = trim(self%gnuplot_symbol)
    end function unitToGnuplotString

    elemental function unitToLatexString(self) result(string)
        class(TimeUnit_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = trim(self%latex_symbol)
    end function unitToLatexString
end module Time_m
