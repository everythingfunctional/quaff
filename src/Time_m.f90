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
            parseCloseBrace, &
            parseOpenBrace, &
            parseSI, &
            parseSpace, &
            wrapInLatexQuantity, &
            wrapInLatexUnit, &
            PARSE_ERROR, &
            UNKNOWN_UNIT
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
    end type Time_t

    type, abstract, public :: TimeUnit_t
        double precision :: conversion_factor
    contains
        procedure(justUnitToString), deferred :: unitToString
        procedure(unitWithValueToString), deferred :: valueToString
        generic :: toString => unitToString, valueToString
        procedure(parseAsI), deferred :: parseAs
    end type TimeUnit_t

    type, extends(TimeUnit_t), public :: TimeSimpleUnit_t
        character(len=20) :: symbol
    contains
        procedure :: unitToString => simpleUnitToString
        procedure :: valueToString => simpleValueToString
        procedure :: parseAs => simpleParseAs
    end type TimeSimpleUnit_t

    type, extends(TimeUnit_t), public :: TimeGnuplotUnit_t
        character(len=50) :: symbol
    contains
        procedure :: unitToString => gnuplotUnitToString
        procedure :: valueToString => gnuplotValueToString
        procedure :: parseAs => gnuplotParseAs
    end type TimeGnuplotUnit_t

    type, extends(TimeUnit_t), public :: TimeLatexUnit_t
        character(len=100) :: symbol
    contains
        procedure :: unitToString => latexUnitToString
        procedure :: valueToString => latexValueToString
        procedure :: parseAs => latexParseAs
    end type TimeLatexUnit_t

    abstract interface
        elemental function justUnitToString(self) result(string)
            import TimeUnit_t, VARYING_STRING
            class(TimeUnit_t), intent(in) :: self
            type(VARYING_STRING) :: string
        end function justUnitToString

        pure function unitWithValueToString(self, value_) result(string)
            import TimeUnit_t, VARYING_STRING
            class(TimeUnit_t), intent(in) :: self
            type(VARYING_STRING), intent(in) :: value_
            type(VARYING_STRING) :: string
        end function unitWithValueToString

        pure subroutine parseAsI(self, string, errors, time)
            import ErrorList_t, Time_t, TimeUnit_t, VARYING_STRING
            class(TimeUnit_t), intent(in) :: self
            type(VARYING_STRING), intent(in) :: string
            type(ErrorList_t), intent(out) :: errors
            type(Time_t), intent(out) :: time
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
        module procedure sumTime
    end interface sum

    type(TimeSimpleUnit_t), parameter, public :: DAYS = &
            TimeSimpleUnit_t( &
                    conversion_factor = DAYS_PER_SECOND, &
                    symbol = "d")
    type(TimeGnuplotUnit_t), parameter, public :: DAYS_GNUPLOT = &
            TimeGnuplotUnit_t( &
                    conversion_factor = DAYS_PER_SECOND, &
                    symbol = "d")
    type(TimeLatexUnit_t), parameter, public :: DAYS_LATEX = &
            TimeLatexUnit_t( &
                    conversion_factor = DAYS_PER_SECOND, &
                    symbol = "\day")
    type(TimeSimpleUnit_t), parameter, public :: HOURS = &
            TimeSimpleUnit_t( &
                    conversion_factor = HOURS_PER_SECOND, &
                    symbol = "h")
    type(TimeGnuplotUnit_t), parameter, public :: HOURS_GNUPLOT = &
            TimeGnuplotUnit_t( &
                    conversion_factor = HOURS_PER_SECOND, &
                    symbol = "h")
    type(TimeLatexUnit_t), parameter, public :: HOURS_LATEX = &
            TimeLatexUnit_t( &
                    conversion_factor = HOURS_PER_SECOND, &
                    symbol = "\hour")
    type(TimeSimpleUnit_t), parameter, public :: MINUTES = &
            TimeSimpleUnit_t( &
                    conversion_factor = MINUTES_PER_SECOND, &
                    symbol = "min")
    type(TimeGnuplotUnit_t), parameter, public :: MINUTES_GNUPLOT = &
            TimeGnuplotUnit_t( &
                    conversion_factor = MINUTES_PER_SECOND, &
                    symbol = "min")
    type(TimeLatexUnit_t), parameter, public :: MINUTES_LATEX = &
            TimeLatexUnit_t( &
                    conversion_factor = MINUTES_PER_SECOND, &
                    symbol = "\minute")
    type(TimeSimpleUnit_t), parameter, public :: SECONDS = &
            TimeSimpleUnit_t( &
                    conversion_factor = 1.0d0, &
                    symbol = "s")
    type(TimeGnuplotUnit_t), parameter, public :: SECONDS_GNUPLOT = &
            TimeGnuplotUnit_t( &
                    conversion_factor = 1.0d0, &
                    symbol = "s")
    type(TimeLatexUnit_t), parameter, public :: SECONDS_LATEX = &
            TimeLatexUnit_t( &
                    conversion_factor = 1.0d0, &
                    symbol = "\second")

    type(TimeSimpleUnit_t), public :: DEFAULT_OUTPUT_UNITS = SECONDS

    type(TimeSimpleUnit_t), parameter, public :: PROVIDED_UNITS(*) = &
            [DAYS, HOURS, MINUTES, SECONDS]
    type(TimeGnuplotUnit_t), parameter, public :: PROVIDED_GNUPLOT_UNITS(*) = &
            [DAYS_GNUPLOT, HOURS_GNUPLOT, MINUTES_GNUPLOT, SECONDS_GNUPLOT]
    type(TimeLatexUnit_t), parameter, public :: PROVIDED_LATEX_UNITS(*) = &
            [DAYS_LATEX, HOURS_LATEX, MINUTES_LATEX, SECONDS_LATEX]

    public :: operator(.unit.), fromString, selectUnit, sum
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
        class(TimeUnit_t), intent(in) :: units(:)
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
        class(TimeUnit_t), intent(in) :: units(:)
        type(ErrorList_t), intent(out) :: errors
        type(Time_t), intent(out) :: time

        type(ErrorList_t) :: all_errors(size(units))
        integer :: i

        do i = 1, size(units)
            call units(i)%parseAs(string, all_errors(i), time)
            if (.not. all_errors(i)%hasAny()) return
        end do
        do i = 1, size(units)
            call errors%appendErrors( &
                    all_errors(i), &
                    Module_("Time_m"), &
                    Procedure_("fromStringWithUnitsS"))
        end do
    end subroutine fromStringWithUnitsS

    elemental function fromUnits(value_, units) result(time)
        double precision, intent(in) :: value_
        class(TimeUnit_t), intent(in) :: units
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

        string = units%toString(toString(self.in.units))
    end function toStringInFullPrecision

    elemental function toStringInWithPrecision( &
            self, units, significant_digits) result(string)
        class(Time_t), intent(in) :: self
        class(TimeUnit_t), intent(in) :: units
        integer, intent(in) :: significant_digits
        type(VARYING_STRING) :: string

        string = units%toString(toString(self.in.units, significant_digits))
    end function toStringInWithPrecision

    elemental function simpleUnitToString(self) result(string)
        class(TimeSimpleUnit_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = trim(self%symbol)
    end function simpleUnitToString

    pure function simpleValueToString(self, value_) result(string)
        class(TimeSimpleUnit_t), intent(in) :: self
        type(VARYING_STRING), intent(in) :: value_
        type(VARYING_STRING) :: string

        string = value_ // " " // self%toString()
    end function simpleValueToString

    pure subroutine simpleParseAs(self, string, errors, time)
        class(TimeSimpleUnit_t), intent(in) :: self
        type(VARYING_STRING), intent(in) :: string
        type(ErrorList_t), intent(out) :: errors
        type(Time_t), intent(out) :: time

        type(ParseResult_t) :: parse_result

        parse_result = parseWith(theParser, string)
        if (parse_result%ok) then
            select type (the_number => parse_result%parsed)
            type is (ParsedRational_t)
                time = the_number%value_.unit.self
            end select
        else
            call errors%appendError(Fatal( &
                    PARSE_ERROR, &
                    Module_("Time_m"), &
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

    pure subroutine gnuplotParseAs(self, string, errors, time)
        class(TimeGnuplotUnit_t), intent(in) :: self
        type(VARYING_STRING), intent(in) :: string
        type(ErrorList_t), intent(out) :: errors
        type(Time_t), intent(out) :: time

        type(ParseResult_t) :: parse_result

        parse_result = parseWith(theParser, string)
        if (parse_result%ok) then
            select type (the_number => parse_result%parsed)
            type is (ParsedRational_t)
                time = the_number%value_.unit.self
            end select
        else
            call errors%appendError(Fatal( &
                    PARSE_ERROR, &
                    Module_("Time_m"), &
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

    pure subroutine latexParseAs(self, string, errors, time)
        class(TimeLatexUnit_t), intent(in) :: self
        type(VARYING_STRING), intent(in) :: string
        type(ErrorList_t), intent(out) :: errors
        type(Time_t), intent(out) :: time

        type(ParseResult_t) :: parse_result

        parse_result = parseWith(theParser, string)
        if (parse_result%ok) then
            select type (the_number => parse_result%parsed)
            type is (ParsedRational_t)
                time = the_number%value_.unit.self
            end select
        else
            call errors%appendError(Fatal( &
                    PARSE_ERROR, &
                    Module_("Time_m"), &
                    Procedure_("latexParseAs"), &
                    parse_result%message))
        end if
    contains
        pure function theParser(state_) result(result_)
            type(State_t), intent(in) :: state_
            type(ParserOutput_t) :: result_

            result_ = thenDrop( &
                    thenDrop( &
                            thenDrop( &
                                    thenDrop( &
                                            dropThen( &
                                                    dropThen(parseSI, parseOpenBrace, state_), &
                                                    parseRational), &
                                            parseCloseBrace), &
                                    parseOpenBrace), &
                            parseUnit), &
                    parseCloseBrace)
        end function theParser

        pure function parseUnit(state_) result(result_)
            type(State_t), intent(in) :: state_
            type(ParserOutput_t) :: result_

            result_ = parseString(trim(self%symbol), state_)
        end function parseUnit
    end subroutine latexParseAs

    elemental function gnuplotUnitToString(self) result(string)
        class(TimeGnuplotUnit_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = trim(self%symbol)
    end function gnuplotUnitToString

    pure function gnuplotValueToString(self, value_) result(string)
        class(TimeGnuplotUnit_t), intent(in) :: self
        type(VARYING_STRING), intent(in) :: value_
        type(VARYING_STRING) :: string

        string = value_ // " " // self%toString()
    end function gnuplotValueToString

    elemental function latexUnitToString(self) result(string)
        class(TimeLatexUnit_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = wrapInLatexUnit(trim(self%symbol))
    end function latexUnitToString

    pure function latexValueToString(self, value_) result(string)
        class(TimeLatexUnit_t), intent(in) :: self
        type(VARYING_STRING), intent(in) :: value_
        type(VARYING_STRING) :: string

        string = wrapInLatexQuantity(value_, trim(self%symbol))
    end function latexValueToString

    pure subroutine simpleUnitFromStringC(string, errors, unit)
        character(len=*), intent(in) :: string
        type(ErrorList_t), intent(out) :: errors
        type(TimeSimpleUnit_t), intent(out) :: unit

        type(ErrorList_t) :: errors_

        call fromString(var_str(string), PROVIDED_UNITS, errors_, unit)
        call errors%appendErrors( &
                errors_, &
                Module_("Time_m"), &
                Procedure_("simpleUnitFromStringC"))
    end subroutine simpleUnitFromStringC

    pure subroutine simpleUnitFromStringS(string, errors, unit)
        type(VARYING_STRING), intent(in) :: string
        type(ErrorList_t), intent(out) :: errors
        type(TimeSimpleUnit_t), intent(out) :: unit

        type(ErrorList_t) :: errors_

        call fromString(string, PROVIDED_UNITS, errors_, unit)
        call errors%appendErrors( &
                errors_, &
                Module_("Time_m"), &
                Procedure_("simpleUnitFromStringS"))
    end subroutine simpleUnitFromStringS

    pure subroutine simpleUnitFromStringWithUnitsC(string, units, errors, unit)
        character(len=*), intent(in) :: string
        type(TimeSimpleUnit_t), intent(in) :: units(:)
        type(ErrorList_t), intent(out) :: errors
        type(TimeSimpleUnit_t), intent(out) :: unit

        type(ErrorList_t) :: errors_

        call fromString(var_str(string), units, errors_, unit)
        call errors%appendErrors( &
                errors_, &
                Module_("Time_m"), &
                Procedure_("simpleUnitFromStringWithUnitsC"))
    end subroutine simpleUnitFromStringWithUnitsC

    pure subroutine simpleUnitFromStringWithUnitsS(string, units, errors, unit)
        type(VARYING_STRING), intent(in) :: string
        type(TimeSimpleUnit_t), intent(in) :: units(:)
        type(ErrorList_t), intent(out) :: errors
        type(TimeSimpleUnit_t), intent(out) :: unit

        type(ErrorList_t) :: errors_
        integer :: which_unit

        call selectUnit(string, units, errors, which_unit)
        if (errors_%hasAny()) then
            call errors%appendErrors( &
                    errors_, &
                    Module_("Time_m"), &
                    Procedure_("simpleUnitFromStringWithUnitsS"))
        else
            unit = units(which_unit)
        end if
    end subroutine simpleUnitFromStringWithUnitsS

    pure subroutine selectUnit(string, units, errors, index)
        type(VARYING_STRING), intent(in) :: string
        class(TimeUnit_t), intent(in) :: units(:)
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
                Module_("Time_m"), &
                Procedure_("selectUnit"), &
                '"' // string // '", known units: [' // join(unit_strings, ', ') // ']'))
    end subroutine selectUnit
end module Time_m
