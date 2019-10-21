module Amount_m
    use Conversion_factors_m, only: AVOGADROS_NUMBER
    use Units_m, only: Unit_t

    implicit none
    private

    type, public :: Amount_t
        private
        double precision :: mols
    contains
        private
        procedure :: toUnits
        generic, public :: operator(.in.) => toUnits
        procedure, pass(amount) :: doubleTimesAmount
        procedure, pass(amount) :: integerTimesAmount
        procedure, pass(amount) :: amountTimesDouble
        procedure, pass(amount) :: amountTimesInteger
        generic, public :: operator(*) => &
                doubleTimesAmount, &
                integerTimesAmount, &
                amountTimesDouble, &
                amountTimesInteger
        procedure :: amountDividedByDouble
        procedure :: amountDividedByInteger
        procedure, pass(numerator) :: amountDividedByAmount
        generic, public :: operator(/) => &
                amountDividedByDouble, &
                amountDividedByInteger, &
                amountDividedByAmount
        procedure :: amountPlusAmount
        generic, public :: operator(+) => amountPlusAmount
        procedure :: amountMinusAmount
        generic, public :: operator(-) => amountMinusAmount
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
    end type Amount_t

    type, public, extends(Unit_t) :: AmountUnit_t
    contains
        procedure :: toString => unitToString
    end type AmountUnit_t

    interface operator(.unit.)
        module procedure fromUnits
    end interface operator(.unit.)

    interface amountFromString
        module procedure fromStringBasicC
        module procedure fromStringBasicS
        module procedure fromStringWithUnitsC
        module procedure fromStringWithUnitsS
    end interface amountFromString

    interface amountUnitFromString
        module procedure unitFromStringBasicC
        module procedure unitFromStringBasicS
        module procedure unitFromStringWithUnitsC
        module procedure unitFromStringWithUnitsS
    end interface amountUnitFromString

    type(AmountUnit_t), parameter, public :: MOLS = &
            AmountUnit_t( &
                    multiplier = 1.0d0, &
                    symbol = "mol")
    type(AmountUnit_t), parameter, public :: NUMBER = &
            AmountUnit_t( &
                    multiplier = AVOGADROS_NUMBER, &
                    symbol = "number")

    type(AmountUnit_t), public :: DEFAULT_OUTPUT_UNITS = MOLS

    type(AmountUnit_t), parameter, public :: PROVIDED_UNITS(*) = &
            [MOLS, NUMBER]

    public :: &
            operator(.unit.), &
            amountFromString, &
            amountUnitFromString
contains
    function fromStringBasicC(string, errors) result(amount)
        use Error_list_m, only: ErrorList_t
        use iso_varying_string, only: var_str
        use Module_m, only: Module_
        use Procedure_m, only: Procedure_

        character(len=*), intent(in) :: string
        type(ErrorList_t), intent(out) :: errors
        type(Amount_t) :: amount

        type(ErrorList_t) :: errors_

        amount = amountFromString( &
                var_str(string), PROVIDED_UNITS, errors_)
        call errors%appendErrors( &
                errors_, &
                Module_("Amount_m"), &
                Procedure_("fromStringBasicC"))
    end function fromStringBasicC

    function fromStringBasicS(string, errors) result(amount)
        use Error_list_m, only: ErrorList_t
        use iso_varying_string, only: VARYING_STRING
        use Module_m, only: Module_
        use Procedure_m, only: Procedure_

        type(VARYING_STRING), intent(in) :: string
        type(ErrorList_t), intent(out) :: errors
        type(Amount_t) :: amount

        type(ErrorList_t) :: errors_

        amount = amountFromString( &
                string, PROVIDED_UNITS, errors_)
        call errors%appendErrors( &
                errors_, &
                Module_("Amount_m"), &
                Procedure_("fromStringBasicS"))
    end function fromStringBasicS

    function fromStringWithUnitsC(string, units, errors) result(amount)
        use Error_list_m, only: ErrorList_t
        use iso_varying_string, only: var_str
        use Module_m, only: Module_
        use Procedure_m, only: Procedure_

        character(len=*), intent(in) :: string
        type(AmountUnit_t), intent(in) :: units(:)
        type(ErrorList_t), intent(out) :: errors
        type(Amount_t) :: amount

        type(ErrorList_t) :: errors_

        amount = amountFromString( &
                var_str(string), units, errors_)
        call errors%appendErrors( &
                errors_, &
                Module_("Amount_m"), &
                Procedure_("fromStringWithUnitsC"))
    end function fromStringWithUnitsC

    function fromStringWithUnitsS(string, units, errors) result(amount)
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
        type(AmountUnit_t), intent(in) :: units(:)
        type(ErrorList_t), intent(out) :: errors
        type(Amount_t) :: amount

        double precision :: number
        character(len=100) :: number_chars
        type(VARYING_STRING) :: number_string
        integer :: status
        type(VARYING_STRING) :: symbol
        type(AmountUnit_t) :: unit
        type(ErrorList_t) :: unit_errors

        amount%mols = 0.0d0
        symbol = string
        call split(symbol, number_string, " ")
        if (len(symbol) == 0) then
            call errors%appendError(Fatal( &
                    PARSE_ERROR, &
                    Module_("Amount_m"), &
                    Procedure_("fromStringWithUnitsS"), &
                    'No unit symbol found in string "' // string // '"'))
            return
        end if
        number_chars = number_string
        read(number_chars, *, iostat=status) number
        if (status /= 0) then
            call errors%appendError(Fatal( &
                    PARSE_ERROR, &
                    Module_("Amount_m"), &
                    Procedure_("fromStringWithUnitsS"), &
                    'Error parsing number from string "' // number_string // '"'))
        end if
        unit = amountUnitFromString(symbol, units, unit_errors)
        amount = number.unit.unit
        call errors%appendErrors( &
                unit_errors, &
                Module_("Amount_m"), &
                Procedure_("fromStringWithUnitsS"))
    end function fromStringWithUnitsS

    function fromUnits(value_, units) result(amount)
        double precision, intent(in) :: value_
        type(AmountUnit_t), intent(in) :: units
        type(Amount_t) :: amount

        amount%mols = value_ / units%multiplier
    end function fromUnits

    function toUnits(self, units) result(amount)
        class(Amount_t), intent(in) :: self
        class(AmountUnit_t), intent(in) :: units
        double precision :: amount

        amount = self%mols * units%multiplier
    end function toUnits

    function doubleTimesAmount( &
            multiplier, amount) result(new_amount)
        double precision, intent(in) :: multiplier
        class(Amount_t), intent(in) :: amount
        type(Amount_t) :: new_amount

        new_amount%mols = &
                multiplier * amount%mols
    end function doubleTimesAmount

    function integerTimesAmount( &
            multiplier, amount) result(new_amount)
        integer, intent(in) :: multiplier
        class(Amount_t), intent(in) :: amount
        type(Amount_t) :: new_amount

        new_amount%mols = &
                dble(multiplier) * amount%mols
    end function integerTimesAmount

    function amountTimesDouble( &
            amount, multiplier) result(new_amount)
        class(Amount_t), intent(in) :: amount
        double precision, intent(in) :: multiplier
        type(Amount_t) :: new_amount

        new_amount%mols = &
                amount%mols * multiplier
    end function amountTimesDouble

    function amountTimesInteger( &
            amount, multiplier) result(new_amount)
        class(Amount_t), intent(in) :: amount
        integer, intent(in) :: multiplier
        type(Amount_t) :: new_amount

        new_amount%mols = &
                amount%mols * dble(multiplier)
    end function amountTimesInteger

    function amountDividedByDouble( &
            amount, divisor) result(new_amount)
        class(Amount_t), intent(in) :: amount
        double precision, intent(in) :: divisor
        type(Amount_t) :: new_amount

        new_amount%mols = &
                amount%mols / divisor
    end function amountDividedByDouble

    function amountDividedByInteger( &
            amount, divisor) result(new_amount)
        class(Amount_t), intent(in) :: amount
        integer, intent(in) :: divisor
        type(Amount_t) :: new_amount

        new_amount%mols = &
                amount%mols / dble(divisor)
    end function amountDividedByInteger

    function amountDividedByAmount( &
            numerator, denomenator) result(ratio)
        class(Amount_t), intent(in) :: numerator
        class(Amount_t), intent(in) :: denomenator
        double precision :: ratio

        ratio = numerator%mols / denomenator%mols
    end function amountDividedByAmount

    function amountPlusAmount( &
            amount1, amount2) result(new_amount)
        class(Amount_t), intent(in) :: amount1
        class(Amount_t), intent(in) :: amount2
        type(Amount_t) :: new_amount

        new_amount%mols = &
                amount1%mols + amount2%mols
    end function amountPlusAmount

    function amountMinusAmount( &
            amount1, amount2) result(new_amount)
        class(Amount_t), intent(in) :: amount1
        class(Amount_t), intent(in) :: amount2
        type(Amount_t) :: new_amount

        new_amount%mols = &
                amount1%mols - amount2%mols
    end function amountMinusAmount

    function greaterThan(lhs, rhs)
        class(Amount_t), intent(in) :: lhs
        class(Amount_t), intent(in) :: rhs
        logical :: greaterThan

        greaterThan = lhs%mols > rhs%mols
    end function greaterThan

    function lessThan(lhs,rhs)
        class(Amount_t), intent(in) :: lhs
        class(Amount_t), intent(in) :: rhs
        logical :: lessThan

        lessThan = lhs%mols < rhs%mols
    end function lessThan

    function greaterThanOrEqual(lhs, rhs)
        class(Amount_t), intent(in) :: lhs
        class(Amount_t), intent(in) :: rhs
        logical :: greaterThanOrEqual

        greaterThanOrEqual = lhs%mols >= rhs%mols
    end function greaterThanOrEqual

    function lessThanOrEqual(lhs, rhs)
        class(Amount_t), intent(in) :: lhs
        class(Amount_t), intent(in) :: rhs
        logical :: lessThanOrEqual

        lessThanOrEqual = lhs%mols <= rhs%mols
    end function lessThanOrEqual

    function equal_(lhs,rhs)
        use Miscellaneous_m, only: operator(.safeEq.)

        class(Amount_t), intent(in) :: lhs
        class(Amount_t), intent(in) :: rhs
        logical :: equal_

        equal_ = lhs%mols .safeEq. rhs%mols
    end function equal_

    function equalWithinAbsolute(lhs, rhs, within)
        use Miscellaneous_m, only: equalWithinAbsolute_ => equalWithinAbsolute

        class(Amount_t), intent(in) :: lhs
        class(Amount_t), intent(in) :: rhs
        class(Amount_t), intent(in) :: within
        logical :: equalWithinAbsolute

        equalWithinAbsolute = equalWithinAbsolute_( &
                lhs%mols, rhs%mols, within%mols)
    end function equalWithinAbsolute

    function equalWithinRelative(lhs, rhs, within)
        use Miscellaneous_m, only: equalWithinRelative_ => equalWithinRelative

        class(Amount_t), intent(in) :: lhs
        class(Amount_t), intent(in) :: rhs
        double precision, intent(in) :: within
        logical :: equalWithinRelative

        equalWithinRelative = equalWithinRelative_( &
                lhs%mols, rhs%mols, within)
    end function equalWithinRelative

    function notEqual(lhs, rhs)
        class(Amount_t), intent(in) :: lhs
        class(Amount_t), intent(in) :: rhs
        logical :: notEqual

        notEqual = .not. lhs == rhs
    end function notEqual

    function toString(self) result(string)
        use iso_varying_string, only: VARYING_STRING

        class(Amount_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = self%toStringIn(DEFAULT_OUTPUT_UNITS)
    end function toString

    function toStringIn(self, units) result(string)
        use iso_varying_string, only: VARYING_STRING, operator(//)
        use strff, only: toString

        class(Amount_t), intent(in) :: self
        class(AmountUnit_t), intent(in) :: units
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
        type(AmountUnit_t) :: unit

        type(ErrorList_t) :: errors_

        unit = amountUnitFromString( &
                var_str(string), PROVIDED_UNITS, errors_)
        call errors%appendErrors( &
                errors_, &
                Module_("Amount_m"), &
                Procedure_("unitFromStringBasicC"))
    end function unitFromStringBasicC

    function unitFromStringBasicS(string, errors) result(unit)
        use Error_list_m, only: ErrorList_t
        use iso_varying_string, only: VARYING_STRING
        use Module_m, only: Module_
        use Procedure_m, only: Procedure_

        type(VARYING_STRING), intent(in) :: string
        type(ErrorList_t), intent(out) :: errors
        type(AmountUnit_t) :: unit

        type(ErrorList_t) :: errors_

        unit = amountUnitFromString( &
                string, PROVIDED_UNITS, errors_)
        call errors%appendErrors( &
                errors_, &
                Module_("Amount_m"), &
                Procedure_("unitFromStringBasicS"))
    end function unitFromStringBasicS

    function unitFromStringWithUnitsC(string, units, errors) result(unit)
        use Error_list_m, only: ErrorList_t
        use iso_varying_string, only: var_str
        use Module_m, only: Module_
        use Procedure_m, only: Procedure_

        character(len=*), intent(in) :: string
        type(AmountUnit_t), intent(in) :: units(:)
        type(ErrorList_t), intent(out) :: errors
        type(AmountUnit_t) :: unit

        type(ErrorList_t) :: errors_

        unit = amountUnitFromString(var_str(string), units, errors_)
        call errors%appendErrors( &
                errors_, &
                Module_("Amount_m"), &
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
        type(AmountUnit_t), intent(in) :: units(:)
        type(ErrorList_t), intent(out) :: errors
        type(AmountUnit_t) :: unit

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
                    Module_("Amount_m"), &
                    Procedure_("unitFromStringWithUnitsS"), &
                    '"' // string // '", known units: [' // join(unit_strings, ', ') // ']' ))
        end if
    end function unitFromStringWithUnitsS

    function unitToString(self) result(string)
        use iso_varying_string, only: VARYING_STRING, assignment(=)

        class(AmountUnit_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = trim(self%symbol)
    end function unitToString
end module Amount_m
