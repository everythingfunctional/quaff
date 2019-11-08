module Area_m
    use Conversion_factors_m, only: SQUARE_CENTIMETERS_PER_SQUARE_METER
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

    type, public :: Area_t
        double precision :: square_meters
    contains
        private
        procedure :: toUnits
        generic, public :: operator(.in.) => toUnits
        procedure, pass(area) :: doubleTimesArea
        procedure, pass(area) :: integerTimesArea
        procedure, pass(area) :: areaTimesDouble
        procedure, pass(area) :: areaTimesInteger
        generic, public :: operator(*) => &
                doubleTimesArea, &
                integerTimesArea, &
                areaTimesDouble, &
                areaTimesInteger
        procedure :: areaDividedByDouble
        procedure :: areaDividedByInteger
        procedure, pass(numerator) :: areaDividedByArea
        generic, public :: operator(/) => &
                areaDividedByDouble, &
                areaDividedByInteger, &
                areaDividedByArea
        procedure :: areaPlusArea
        generic, public :: operator(+) => areaPlusArea
        procedure :: areaMinusArea
        generic, public :: operator(-) => areaMinusArea
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
    end type Area_t

    type, public :: AreaUnit_t
        double precision :: conversion_factor
        character(len=10) :: symbol
        character(len=50) :: gnuplot_symbol
        character(len=100) :: latex_symbol
    contains
        procedure :: toString => unitToString
        procedure :: toGnuplotString => unitToGnuplotString
        procedure :: toLatexString => unitToLatexString
    end type AreaUnit_t

    interface operator(.unit.)
        module procedure fromUnits
    end interface operator(.unit.)

    interface areaFromString
        module procedure fromStringBasicC
        module procedure fromStringBasicS
        module procedure fromStringWithUnitsC
        module procedure fromStringWithUnitsS
    end interface areaFromString

    interface areaUnitFromString
        module procedure unitFromStringBasicC
        module procedure unitFromStringBasicS
        module procedure unitFromStringWithUnitsC
        module procedure unitFromStringWithUnitsS
    end interface areaUnitFromString

    type(AreaUnit_t), parameter, public :: SQUARE_CENTIMETERS = &
            AreaUnit_t( &
                    conversion_factor = SQUARE_CENTIMETERS_PER_SQUARE_METER, &
                    symbol = "cm^2", &
                    gnuplot_symbol = "cm^2", &
                    latex_symbol = "\square\centi\meter")
    type(AreaUnit_t), parameter, public :: SQUARE_METERS = &
            AreaUnit_t( &
                    conversion_factor = 1.0d0, &
                    symbol = "m^2", &
                    gnuplot_symbol = "m^2", &
                    latex_symbol = "\square\meter")

    type(AreaUnit_t), public :: DEFAULT_OUTPUT_UNITS = SQUARE_METERS

    type(AreaUnit_t), parameter, public :: PROVIDED_UNITS(*) = &
            [SQUARE_CENTIMETERS, SQUARE_METERS]

    public :: &
            operator(.unit.), &
            areaFromString, &
            areaUnitFromString
contains
    function fromStringBasicC(string, errors) result(area)
        character(len=*), intent(in) :: string
        type(ErrorList_t), intent(out) :: errors
        type(Area_t) :: area

        type(ErrorList_t) :: errors_

        area = areaFromString( &
                var_str(string), PROVIDED_UNITS, errors_)
        call errors%appendErrors( &
                errors_, &
                Module_("Area_m"), &
                Procedure_("fromStringBasicC"))
    end function fromStringBasicC

    function fromStringBasicS(string, errors) result(area)
        type(VARYING_STRING), intent(in) :: string
        type(ErrorList_t), intent(out) :: errors
        type(Area_t) :: area

        type(ErrorList_t) :: errors_

        area = areaFromString( &
                string, PROVIDED_UNITS, errors_)
        call errors%appendErrors( &
                errors_, &
                Module_("Area_m"), &
                Procedure_("fromStringBasicS"))
    end function fromStringBasicS

    function fromStringWithUnitsC(string, units, errors) result(area)
        character(len=*), intent(in) :: string
        type(AreaUnit_t), intent(in) :: units(:)
        type(ErrorList_t), intent(out) :: errors
        type(Area_t) :: area

        type(ErrorList_t) :: errors_

        area = areaFromString( &
                var_str(string), units, errors_)
        call errors%appendErrors( &
                errors_, &
                Module_("Area_m"), &
                Procedure_("fromStringWithUnitsC"))
    end function fromStringWithUnitsC

    function fromStringWithUnitsS(string, units, errors) result(area)
        type(VARYING_STRING), intent(in) :: string
        type(AreaUnit_t), intent(in) :: units(:)
        type(ErrorList_t), intent(out) :: errors
        type(Area_t) :: area

        double precision :: number
        character(len=100) :: number_chars
        type(VARYING_STRING) :: number_string
        integer :: status
        type(VARYING_STRING) :: symbol
        type(AreaUnit_t) :: unit
        type(ErrorList_t) :: unit_errors

        area%square_meters = 0.0d0
        symbol = string
        call split(symbol, number_string, " ")
        if (len(symbol) == 0) then
            call errors%appendError(Fatal( &
                    PARSE_ERROR, &
                    Module_("Area_m"), &
                    Procedure_("fromStringWithUnitsS"), &
                    'No unit symbol found in string "' // string // '"'))
            return
        end if
        number_chars = number_string
        read(number_chars, *, iostat=status) number
        if (status /= 0) then
            call errors%appendError(Fatal( &
                    PARSE_ERROR, &
                    Module_("Area_m"), &
                    Procedure_("fromStringWithUnitsS"), &
                    'Error parsing number from string "' // number_string // '"'))
        end if
        unit = areaUnitFromString(symbol, units, unit_errors)
        area = number.unit.unit
        call errors%appendErrors( &
                unit_errors, &
                Module_("Area_m"), &
                Procedure_("fromStringWithUnitsS"))
    end function fromStringWithUnitsS

    elemental function fromUnits(value_, units) result(area)
        double precision, intent(in) :: value_
        type(AreaUnit_t), intent(in) :: units
        type(Area_t) :: area

        area%square_meters = value_ / units%conversion_factor
    end function fromUnits

    elemental function toUnits(self, units) result(area)
        class(Area_t), intent(in) :: self
        class(AreaUnit_t), intent(in) :: units
        double precision :: area

        area = self%square_meters * units%conversion_factor
    end function toUnits

    elemental function doubleTimesArea( &
            multiplier, area) result(new_area)
        double precision, intent(in) :: multiplier
        class(Area_t), intent(in) :: area
        type(Area_t) :: new_area

        new_area%square_meters = &
                multiplier * area%square_meters
    end function doubleTimesArea

    elemental function integerTimesArea( &
            multiplier, area) result(new_area)
        integer, intent(in) :: multiplier
        class(Area_t), intent(in) :: area
        type(Area_t) :: new_area

        new_area%square_meters = &
                dble(multiplier) * area%square_meters
    end function integerTimesArea

    elemental function areaTimesDouble( &
            area, multiplier) result(new_area)
        class(Area_t), intent(in) :: area
        double precision, intent(in) :: multiplier
        type(Area_t) :: new_area

        new_area%square_meters = &
                area%square_meters * multiplier
    end function areaTimesDouble

    elemental function areaTimesInteger( &
            area, multiplier) result(new_area)
        class(Area_t), intent(in) :: area
        integer, intent(in) :: multiplier
        type(Area_t) :: new_area

        new_area%square_meters = &
                area%square_meters * dble(multiplier)
    end function areaTimesInteger

    elemental function areaDividedByDouble( &
            area, divisor) result(new_area)
        class(Area_t), intent(in) :: area
        double precision, intent(in) :: divisor
        type(Area_t) :: new_area

        new_area%square_meters = &
                area%square_meters / divisor
    end function areaDividedByDouble

    elemental function areaDividedByInteger( &
            area, divisor) result(new_area)
        class(Area_t), intent(in) :: area
        integer, intent(in) :: divisor
        type(Area_t) :: new_area

        new_area%square_meters = &
                area%square_meters / dble(divisor)
    end function areaDividedByInteger

    elemental function areaDividedByArea( &
            numerator, denomenator) result(ratio)
        class(Area_t), intent(in) :: numerator
        class(Area_t), intent(in) :: denomenator
        double precision :: ratio

        ratio = numerator%square_meters / denomenator%square_meters
    end function areaDividedByArea

    elemental function areaPlusArea( &
            area1, area2) result(new_area)
        class(Area_t), intent(in) :: area1
        class(Area_t), intent(in) :: area2
        type(Area_t) :: new_area

        new_area%square_meters = &
                area1%square_meters + area2%square_meters
    end function areaPlusArea

    elemental function areaMinusArea( &
            area1, area2) result(new_area)
        class(Area_t), intent(in) :: area1
        class(Area_t), intent(in) :: area2
        type(Area_t) :: new_area

        new_area%square_meters = &
                area1%square_meters - area2%square_meters
    end function areaMinusArea

    elemental function greaterThan(lhs, rhs)
        class(Area_t), intent(in) :: lhs
        class(Area_t), intent(in) :: rhs
        logical :: greaterThan

        greaterThan = lhs%square_meters > rhs%square_meters
    end function greaterThan

    elemental function lessThan(lhs,rhs)
        class(Area_t), intent(in) :: lhs
        class(Area_t), intent(in) :: rhs
        logical :: lessThan

        lessThan = lhs%square_meters < rhs%square_meters
    end function lessThan

    elemental function greaterThanOrEqual(lhs, rhs)
        class(Area_t), intent(in) :: lhs
        class(Area_t), intent(in) :: rhs
        logical :: greaterThanOrEqual

        greaterThanOrEqual = lhs%square_meters >= rhs%square_meters
    end function greaterThanOrEqual

    elemental function lessThanOrEqual(lhs, rhs)
        class(Area_t), intent(in) :: lhs
        class(Area_t), intent(in) :: rhs
        logical :: lessThanOrEqual

        lessThanOrEqual = lhs%square_meters <= rhs%square_meters
    end function lessThanOrEqual

    elemental function equal_(lhs,rhs)
        class(Area_t), intent(in) :: lhs
        class(Area_t), intent(in) :: rhs
        logical :: equal_

        equal_ = lhs%square_meters .safeEq. rhs%square_meters
    end function equal_

    function equalWithinAbsolute(lhs, rhs, within)
        class(Area_t), intent(in) :: lhs
        class(Area_t), intent(in) :: rhs
        class(Area_t), intent(in) :: within
        logical :: equalWithinAbsolute

        equalWithinAbsolute = equalWithinAbsolute_( &
                lhs%square_meters, rhs%square_meters, within%square_meters)
    end function equalWithinAbsolute

    function equalWithinRelative(lhs, rhs, within)
        class(Area_t), intent(in) :: lhs
        class(Area_t), intent(in) :: rhs
        double precision, intent(in) :: within
        logical :: equalWithinRelative

        equalWithinRelative = equalWithinRelative_( &
                lhs%square_meters, rhs%square_meters, within)
    end function equalWithinRelative

    elemental function notEqual(lhs, rhs)
        class(Area_t), intent(in) :: lhs
        class(Area_t), intent(in) :: rhs
        logical :: notEqual

        notEqual = .not. lhs == rhs
    end function notEqual

    function toStringFullPrecision(self) result(string)
        class(Area_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = self%toStringIn(DEFAULT_OUTPUT_UNITS)
    end function toStringFullPrecision

    function toStringWithPrecision(self, significant_digits) result(string)
        class(Area_t), intent(in) :: self
        integer, intent(in) :: significant_digits
        type(VARYING_STRING) :: string

        string = self%toStringIn(DEFAULT_OUTPUT_UNITS, significant_digits)
    end function toStringWithPrecision

    function toStringInFullPrecision(self, units) result(string)
        class(Area_t), intent(in) :: self
        class(AreaUnit_t), intent(in) :: units
        type(VARYING_STRING) :: string

        string = toString(self.in.units) // " " // units%toString()
    end function toStringInFullPrecision

    function toStringInWithPrecision( &
            self, units, significant_digits) result(string)
        class(Area_t), intent(in) :: self
        class(AreaUnit_t), intent(in) :: units
        integer, intent(in) :: significant_digits
        type(VARYING_STRING) :: string

        string = &
                toString(self.in.units, significant_digits) &
                // " " // units%toString()
    end function toStringInWithPrecision

    function toGnuplotStringFullPrecision(self) result(string)
        class(Area_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = self%toGnuplotStringIn(DEFAULT_OUTPUT_UNITS)
    end function toGnuplotStringFullPrecision

    function toGnuplotStringWithPrecision( &
            self, significant_digits) result(string)
        class(Area_t), intent(in) :: self
        integer, intent(in) :: significant_digits
        type(VARYING_STRING) :: string

        string = self%toGnuplotStringIn( &
                DEFAULT_OUTPUT_UNITS, significant_digits)
    end function toGnuplotStringWithPrecision

    function toGnuplotStringInFullPrecision(self, units) result(string)
        class(Area_t), intent(in) :: self
        class(AreaUnit_t), intent(in) :: units
        type(VARYING_STRING) :: string

        string = toString(self.in.units) // " " // units%toGnuplotString()
    end function toGnuplotStringInFullPrecision

    function toGnuplotStringInWithPrecision( &
            self, units, significant_digits) result(string)
        class(Area_t), intent(in) :: self
        class(AreaUnit_t), intent(in) :: units
        integer, intent(in) :: significant_digits
        type(VARYING_STRING) :: string

        string = &
                toString(self.in.units, significant_digits) &
                // " " // units%toGnuplotString()
    end function toGnuplotStringInWithPrecision

    function toLatexStringFullPrecision(self) result(string)
        class(Area_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = self%toLatexStringIn(DEFAULT_OUTPUT_UNITS)
    end function toLatexStringFullPrecision

    function toLatexStringWithPrecision(self, significant_digits) result(string)
        class(Area_t), intent(in) :: self
        integer, intent(in) :: significant_digits
        type(VARYING_STRING) :: string

        string = self%toLatexStringIn(DEFAULT_OUTPUT_UNITS, significant_digits)
    end function toLatexStringWithPrecision

    function toLatexStringInFullPrecision(self, units) result(string)
        class(Area_t), intent(in) :: self
        class(AreaUnit_t), intent(in) :: units
        type(VARYING_STRING) :: string

        string = wrapInLatexQuantity( &
                toString(self.in.units), units%toLatexString())
    end function toLatexStringInFullPrecision

    function toLatexStringInWithPrecision( &
            self, units, significant_digits) result(string)
        class(Area_t), intent(in) :: self
        class(AreaUnit_t), intent(in) :: units
        integer, intent(in) :: significant_digits
        type(VARYING_STRING) :: string

        string = wrapInLatexQuantity( &
                toString(self.in.units, significant_digits), &
                units%toLatexString())
    end function toLatexStringInWithPrecision

    function unitFromStringBasicC(string, errors) result(unit)
        character(len=*), intent(in) :: string
        type(ErrorList_t), intent(out) :: errors
        type(AreaUnit_t) :: unit

        type(ErrorList_t) :: errors_

        unit = areaUnitFromString( &
                var_str(string), PROVIDED_UNITS, errors_)
        call errors%appendErrors( &
                errors_, &
                Module_("Area_m"), &
                Procedure_("unitFromStringBasicC"))
    end function unitFromStringBasicC

    function unitFromStringBasicS(string, errors) result(unit)
        type(VARYING_STRING), intent(in) :: string
        type(ErrorList_t), intent(out) :: errors
        type(AreaUnit_t) :: unit

        type(ErrorList_t) :: errors_

        unit = areaUnitFromString( &
                string, PROVIDED_UNITS, errors_)
        call errors%appendErrors( &
                errors_, &
                Module_("Area_m"), &
                Procedure_("unitFromStringBasicS"))
    end function unitFromStringBasicS

    function unitFromStringWithUnitsC(string, units, errors) result(unit)
        character(len=*), intent(in) :: string
        type(AreaUnit_t), intent(in) :: units(:)
        type(ErrorList_t), intent(out) :: errors
        type(AreaUnit_t) :: unit

        type(ErrorList_t) :: errors_

        unit = areaUnitFromString(var_str(string), units, errors_)
        call errors%appendErrors( &
                errors_, &
                Module_("Area_m"), &
                Procedure_("unitFromStringWithUnitsC"))
    end function unitFromStringWithUnitsC

    function unitFromStringWithUnitsS(string, units, errors) result(unit)
        type(VARYING_STRING), intent(in) :: string
        type(AreaUnit_t), intent(in) :: units(:)
        type(ErrorList_t), intent(out) :: errors
        type(AreaUnit_t) :: unit

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
                    Module_("Area_m"), &
                    Procedure_("unitFromStringWithUnitsS"), &
                    '"' // string // '", known units: [' // join(unit_strings, ', ') // ']' ))
        end if
    end function unitFromStringWithUnitsS

    function unitToString(self) result(string)
        class(AreaUnit_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = trim(self%symbol)
    end function unitToString

    function unitToGnuplotString(self) result(string)
        class(AreaUnit_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = trim(self%gnuplot_symbol)
    end function unitToGnuplotString

    function unitToLatexString(self) result(string)
        class(AreaUnit_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = trim(self%latex_symbol)
    end function unitToLatexString
end module Area_m
