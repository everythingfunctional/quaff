module Amount_m
    use Conversion_factors_m, only: AVOGADROS_NUMBER
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

    type, public :: Amount_t
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
    end type Amount_t

    type, public :: AmountUnit_t
        double precision :: conversion_factor
        character(len=20) :: symbol
        character(len=50) :: gnuplot_symbol
        character(len=100) :: latex_symbol
    contains
        procedure :: toString => unitToString
        procedure :: toGnuplotString => unitToGnuplotString
        procedure :: toLatexString => unitToLatexString
    end type AmountUnit_t

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
        module procedure sumAmount
    end interface sum

    type(AmountUnit_t), parameter, public :: MOLS = &
            AmountUnit_t( &
                    conversion_factor = 1.0d0, &
                    symbol = "mol", &
                    gnuplot_symbol = "mol", &
                    latex_symbol = "\mole")
    type(AmountUnit_t), parameter, public :: PARTICLES = &
            AmountUnit_t( &
                    conversion_factor = AVOGADROS_NUMBER, &
                    symbol = "particles", &
                    gnuplot_symbol = "particles", &
                    latex_symbol = ".particles")

    type(AmountUnit_t), public :: DEFAULT_OUTPUT_UNITS = MOLS

    type(AmountUnit_t), parameter, public :: PROVIDED_UNITS(*) = &
            [MOLS, PARTICLES]

    public :: operator(.unit.), fromString, sum
contains
    pure subroutine fromStringBasicC(string, errors, amount)
        character(len=*), intent(in) :: string
        type(ErrorList_t), intent(out) :: errors
        type(Amount_t), intent(out) :: amount

        type(ErrorList_t) :: errors_

        call fromString( &
                var_str(string), PROVIDED_UNITS, errors_, amount)
        call errors%appendErrors( &
                errors_, &
                Module_("Amount_m"), &
                Procedure_("fromStringBasicC"))
    end subroutine fromStringBasicC

    pure subroutine fromStringBasicS(string, errors, amount)
        type(VARYING_STRING), intent(in) :: string
        type(ErrorList_t), intent(out) :: errors
        type(Amount_t), intent(out) :: amount

        type(ErrorList_t) :: errors_

        call fromString( &
                string, PROVIDED_UNITS, errors_, amount)
        call errors%appendErrors( &
                errors_, &
                Module_("Amount_m"), &
                Procedure_("fromStringBasicS"))
    end subroutine fromStringBasicS

    pure subroutine fromStringWithUnitsC(string, units, errors, amount)
        character(len=*), intent(in) :: string
        type(AmountUnit_t), intent(in) :: units(:)
        type(ErrorList_t), intent(out) :: errors
        type(Amount_t), intent(out) :: amount

        type(ErrorList_t) :: errors_

        call fromString( &
                var_str(string), units, errors_, amount)
        call errors%appendErrors( &
                errors_, &
                Module_("Amount_m"), &
                Procedure_("fromStringWithUnitsC"))
    end subroutine fromStringWithUnitsC

    pure subroutine fromStringWithUnitsS(string, units, errors, amount)
        type(VARYING_STRING), intent(in) :: string
        type(AmountUnit_t), intent(in) :: units(:)
        type(ErrorList_t), intent(out) :: errors
        type(Amount_t), intent(out) :: amount

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
        call fromString(symbol, units, unit_errors, unit)
        amount = number.unit.unit
        call errors%appendErrors( &
                unit_errors, &
                Module_("Amount_m"), &
                Procedure_("fromStringWithUnitsS"))
    end subroutine fromStringWithUnitsS

    elemental function fromUnits(value_, units) result(amount)
        double precision, intent(in) :: value_
        type(AmountUnit_t), intent(in) :: units
        type(Amount_t) :: amount

        amount%mols = value_ / units%conversion_factor
    end function fromUnits

    elemental function toUnits(self, units) result(amount)
        class(Amount_t), intent(in) :: self
        class(AmountUnit_t), intent(in) :: units
        double precision :: amount

        amount = self%mols * units%conversion_factor
    end function toUnits

    elemental function doubleTimesAmount( &
            multiplier, amount) result(new_amount)
        double precision, intent(in) :: multiplier
        class(Amount_t), intent(in) :: amount
        type(Amount_t) :: new_amount

        new_amount%mols = &
                multiplier * amount%mols
    end function doubleTimesAmount

    elemental function integerTimesAmount( &
            multiplier, amount) result(new_amount)
        integer, intent(in) :: multiplier
        class(Amount_t), intent(in) :: amount
        type(Amount_t) :: new_amount

        new_amount%mols = &
                dble(multiplier) * amount%mols
    end function integerTimesAmount

    elemental function amountTimesDouble( &
            amount, multiplier) result(new_amount)
        class(Amount_t), intent(in) :: amount
        double precision, intent(in) :: multiplier
        type(Amount_t) :: new_amount

        new_amount%mols = &
                amount%mols * multiplier
    end function amountTimesDouble

    elemental function amountTimesInteger( &
            amount, multiplier) result(new_amount)
        class(Amount_t), intent(in) :: amount
        integer, intent(in) :: multiplier
        type(Amount_t) :: new_amount

        new_amount%mols = &
                amount%mols * dble(multiplier)
    end function amountTimesInteger

    elemental function amountDividedByDouble( &
            amount, divisor) result(new_amount)
        class(Amount_t), intent(in) :: amount
        double precision, intent(in) :: divisor
        type(Amount_t) :: new_amount

        new_amount%mols = &
                amount%mols / divisor
    end function amountDividedByDouble

    elemental function amountDividedByInteger( &
            amount, divisor) result(new_amount)
        class(Amount_t), intent(in) :: amount
        integer, intent(in) :: divisor
        type(Amount_t) :: new_amount

        new_amount%mols = &
                amount%mols / dble(divisor)
    end function amountDividedByInteger

    elemental function amountDividedByAmount( &
            numerator, denomenator) result(ratio)
        class(Amount_t), intent(in) :: numerator
        class(Amount_t), intent(in) :: denomenator
        double precision :: ratio

        ratio = numerator%mols / denomenator%mols
    end function amountDividedByAmount

    elemental function amountPlusAmount( &
            amount1, amount2) result(new_amount)
        class(Amount_t), intent(in) :: amount1
        class(Amount_t), intent(in) :: amount2
        type(Amount_t) :: new_amount

        new_amount%mols = &
                amount1%mols + amount2%mols
    end function amountPlusAmount

    elemental function amountMinusAmount( &
            amount1, amount2) result(new_amount)
        class(Amount_t), intent(in) :: amount1
        class(Amount_t), intent(in) :: amount2
        type(Amount_t) :: new_amount

        new_amount%mols = &
                amount1%mols - amount2%mols
    end function amountMinusAmount

    pure function sumAmount(amounts)
        type(Amount_t), intent(in) :: amounts(:)
        type(Amount_t) :: sumAmount

        sumAmount%mols = sum(amounts%mols)
    end function sumAmount

    elemental function greaterThan(lhs, rhs)
        class(Amount_t), intent(in) :: lhs
        class(Amount_t), intent(in) :: rhs
        logical :: greaterThan

        greaterThan = lhs%mols > rhs%mols
    end function greaterThan

    elemental function lessThan(lhs,rhs)
        class(Amount_t), intent(in) :: lhs
        class(Amount_t), intent(in) :: rhs
        logical :: lessThan

        lessThan = lhs%mols < rhs%mols
    end function lessThan

    elemental function greaterThanOrEqual(lhs, rhs)
        class(Amount_t), intent(in) :: lhs
        class(Amount_t), intent(in) :: rhs
        logical :: greaterThanOrEqual

        greaterThanOrEqual = lhs%mols >= rhs%mols
    end function greaterThanOrEqual

    elemental function lessThanOrEqual(lhs, rhs)
        class(Amount_t), intent(in) :: lhs
        class(Amount_t), intent(in) :: rhs
        logical :: lessThanOrEqual

        lessThanOrEqual = lhs%mols <= rhs%mols
    end function lessThanOrEqual

    elemental function equal_(lhs,rhs)
        class(Amount_t), intent(in) :: lhs
        class(Amount_t), intent(in) :: rhs
        logical :: equal_

        equal_ = lhs%mols .safeEq. rhs%mols
    end function equal_

    elemental function equalWithinAbsolute(lhs, rhs, within)
        class(Amount_t), intent(in) :: lhs
        class(Amount_t), intent(in) :: rhs
        class(Amount_t), intent(in) :: within
        logical :: equalWithinAbsolute

        equalWithinAbsolute = equalWithinAbsolute_( &
                lhs%mols, rhs%mols, within%mols)
    end function equalWithinAbsolute

    elemental function equalWithinRelative(lhs, rhs, within)
        class(Amount_t), intent(in) :: lhs
        class(Amount_t), intent(in) :: rhs
        double precision, intent(in) :: within
        logical :: equalWithinRelative

        equalWithinRelative = equalWithinRelative_( &
                lhs%mols, rhs%mols, within)
    end function equalWithinRelative

    elemental function notEqual(lhs, rhs)
        class(Amount_t), intent(in) :: lhs
        class(Amount_t), intent(in) :: rhs
        logical :: notEqual

        notEqual = .not. lhs == rhs
    end function notEqual

    elemental function toStringFullPrecision(self) result(string)
        class(Amount_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = self%toStringIn(DEFAULT_OUTPUT_UNITS)
    end function toStringFullPrecision

    elemental function toStringWithPrecision(self, significant_digits) result(string)
        class(Amount_t), intent(in) :: self
        integer, intent(in) :: significant_digits
        type(VARYING_STRING) :: string

        string = self%toStringIn(DEFAULT_OUTPUT_UNITS, significant_digits)
    end function toStringWithPrecision

    elemental function toStringInFullPrecision(self, units) result(string)
        class(Amount_t), intent(in) :: self
        class(AmountUnit_t), intent(in) :: units
        type(VARYING_STRING) :: string

        string = toString(self.in.units) // " " // units%toString()
    end function toStringInFullPrecision

    elemental function toStringInWithPrecision( &
            self, units, significant_digits) result(string)
        class(Amount_t), intent(in) :: self
        class(AmountUnit_t), intent(in) :: units
        integer, intent(in) :: significant_digits
        type(VARYING_STRING) :: string

        string = &
                toString(self.in.units, significant_digits) &
                // " " // units%toString()
    end function toStringInWithPrecision

    elemental function toGnuplotStringFullPrecision(self) result(string)
        class(Amount_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = self%toGnuplotStringIn(DEFAULT_OUTPUT_UNITS)
    end function toGnuplotStringFullPrecision

    elemental function toGnuplotStringWithPrecision( &
            self, significant_digits) result(string)
        class(Amount_t), intent(in) :: self
        integer, intent(in) :: significant_digits
        type(VARYING_STRING) :: string

        string = self%toGnuplotStringIn( &
                DEFAULT_OUTPUT_UNITS, significant_digits)
    end function toGnuplotStringWithPrecision

    elemental function toGnuplotStringInFullPrecision(self, units) result(string)
        class(Amount_t), intent(in) :: self
        class(AmountUnit_t), intent(in) :: units
        type(VARYING_STRING) :: string

        string = toString(self.in.units) // " " // units%toGnuplotString()
    end function toGnuplotStringInFullPrecision

    elemental function toGnuplotStringInWithPrecision( &
            self, units, significant_digits) result(string)
        class(Amount_t), intent(in) :: self
        class(AmountUnit_t), intent(in) :: units
        integer, intent(in) :: significant_digits
        type(VARYING_STRING) :: string

        string = &
                toString(self.in.units, significant_digits) &
                // " " // units%toGnuplotString()
    end function toGnuplotStringInWithPrecision

    elemental function toLatexStringFullPrecision(self) result(string)
        class(Amount_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = self%toLatexStringIn(DEFAULT_OUTPUT_UNITS)
    end function toLatexStringFullPrecision

    elemental function toLatexStringWithPrecision(self, significant_digits) result(string)
        class(Amount_t), intent(in) :: self
        integer, intent(in) :: significant_digits
        type(VARYING_STRING) :: string

        string = self%toLatexStringIn(DEFAULT_OUTPUT_UNITS, significant_digits)
    end function toLatexStringWithPrecision

    elemental function toLatexStringInFullPrecision(self, units) result(string)
        class(Amount_t), intent(in) :: self
        class(AmountUnit_t), intent(in) :: units
        type(VARYING_STRING) :: string

        string = wrapInLatexQuantity( &
                toString(self.in.units), units%toLatexString())
    end function toLatexStringInFullPrecision

    elemental function toLatexStringInWithPrecision( &
            self, units, significant_digits) result(string)
        class(Amount_t), intent(in) :: self
        class(AmountUnit_t), intent(in) :: units
        integer, intent(in) :: significant_digits
        type(VARYING_STRING) :: string

        string = wrapInLatexQuantity( &
                toString(self.in.units, significant_digits), &
                units%toLatexString())
    end function toLatexStringInWithPrecision

    pure subroutine unitFromStringBasicC(string, errors, unit)
        character(len=*), intent(in) :: string
        type(ErrorList_t), intent(out) :: errors
        type(AmountUnit_t), intent(out) :: unit

        type(ErrorList_t) :: errors_

        call fromString( &
                var_str(string), PROVIDED_UNITS, errors_, unit)
        call errors%appendErrors( &
                errors_, &
                Module_("Amount_m"), &
                Procedure_("unitFromStringBasicC"))
    end subroutine unitFromStringBasicC

    pure subroutine unitFromStringBasicS(string, errors, unit)
        type(VARYING_STRING), intent(in) :: string
        type(ErrorList_t), intent(out) :: errors
        type(AmountUnit_t), intent(out) :: unit

        type(ErrorList_t) :: errors_

        call fromString( &
                string, PROVIDED_UNITS, errors_, unit)
        call errors%appendErrors( &
                errors_, &
                Module_("Amount_m"), &
                Procedure_("unitFromStringBasicS"))
    end subroutine unitFromStringBasicS

    pure subroutine unitFromStringWithUnitsC(string, units, errors, unit)
        character(len=*), intent(in) :: string
        type(AmountUnit_t), intent(in) :: units(:)
        type(ErrorList_t), intent(out) :: errors
        type(AmountUnit_t), intent(out) :: unit

        type(ErrorList_t) :: errors_

        call fromString(var_str(string), units, errors_, unit)
        call errors%appendErrors( &
                errors_, &
                Module_("Amount_m"), &
                Procedure_("unitFromStringWithUnitsC"))
    end subroutine unitFromStringWithUnitsC

    pure subroutine unitFromStringWithUnitsS(string, units, errors, unit)
        type(VARYING_STRING), intent(in) :: string
        type(AmountUnit_t), intent(in) :: units(:)
        type(ErrorList_t), intent(out) :: errors
        type(AmountUnit_t), intent(out) :: unit

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
    end subroutine unitFromStringWithUnitsS

    elemental function unitToString(self) result(string)
        class(AmountUnit_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = trim(self%symbol)
    end function unitToString

    elemental function unitToGnuplotString(self) result(string)
        class(AmountUnit_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = trim(self%gnuplot_symbol)
    end function unitToGnuplotString

    elemental function unitToLatexString(self) result(string)
        class(AmountUnit_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = trim(self%latex_symbol)
    end function unitToLatexString
end module Amount_m
