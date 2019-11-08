module Miscellaneous_m
    use iso_varying_string, only: VARYING_STRING, operator(//), var_str
    use Message_m, only: MessageType_t

    implicit none
    private

    interface operator(.safeEq.)
        module procedure safeEq
    end interface operator(.safeEq.)

    interface wrapInLatexQuantity
        module procedure wrapInLatexQuantityCC
        module procedure wrapInLatexQuantityCS
        module procedure wrapInLatexQuantitySC
        module procedure wrapInLatexQuantitySS
    end interface wrapInLatexQuantity

    interface wrapInLatexUnit
        module procedure wrapInLatexUnitC
        module procedure wrapInLatexUnitS
    end interface wrapInLatexUnit

    double precision, parameter :: MACHINE_EPSILON = epsilon(1.0d0)

    type(MessageType_t), parameter, public :: PARSE_ERROR = &
            MessageType_t("Parse Error")
    type(MessageType_t), parameter, public :: UNKNOWN_UNIT = &
            MessageType_t("Unknown Unit")

    public :: &
            effectivelyZero, &
            equalWithinAbsolute, &
            equalWithinRelative, &
            operator(.safeEq.), &
            wrapInLatexQuantity, &
            wrapInLatexUnit
contains
    elemental function effectivelyZero(a)
        double precision, intent(in) :: a
        logical :: effectivelyZero

        effectivelyZero = abs(a) < MACHINE_EPSILON
    end function effectivelyZero

    pure function equalWithinAbsolute(a, b, tolerance)
        double precision, intent(in) :: a
        double precision, intent(in) :: b
        double precision, intent(in) :: tolerance
        logical :: equalWithinAbsolute

        equalWithinAbsolute = abs(a - b) < tolerance
    end function equalWithinAbsolute

    pure function equalWithinRelative(a, b, tolerance)
        double precision, intent(in) :: a
        double precision, intent(in) :: b
        double precision, intent(in) :: tolerance
        logical :: equalWithinRelative

        equalWithinRelative = &
                (effectivelyZero(a) .and. effectivelyZero(b)) &
                .or. (abs(a - b) / max(abs(a), abs(b))) < tolerance
    end function equalWithinRelative

    elemental function safeEq(a, b)
        double precision, intent(in) :: a
        double precision, intent(in) :: b
        logical :: safeEq

        safeEq = equalWithinRelative(a, b, MACHINE_EPSILON)
    end function safeEq

    function wrapInLatexQuantityCC(number, units) result(latex_command)
        character(len=*), intent(in) :: number
        character(len=*), intent(in) :: units
        type(VARYING_STRING) :: latex_command

        latex_command = wrapInLatexQuantity(var_str(number), var_str(units))
    end function wrapInLatexQuantityCC

    function wrapInLatexQuantityCS(number, units) result(latex_command)
        character(len=*), intent(in) :: number
        type(VARYING_STRING), intent(in) :: units
        type(VARYING_STRING) :: latex_command

        latex_command = wrapInLatexQuantity(var_str(number), units)
    end function wrapInLatexQuantityCS

    function wrapInLatexQuantitySC(number, units) result(latex_command)
        type(VARYING_STRING), intent(in) :: number
        character(len=*), intent(in) :: units
        type(VARYING_STRING) :: latex_command

        latex_command = wrapInLatexQuantity(number, var_str(units))
    end function wrapInLatexQuantitySC

    function wrapInLatexQuantitySS(number, units) result(latex_command)
        type(VARYING_STRING), intent(in) :: number
        type(VARYING_STRING), intent(in) :: units
        type(VARYING_STRING) :: latex_command

        latex_command = "\SI{" // number // "}{" // units // "}"
    end function wrapInLatexQuantitySS

    function wrapInLatexUnitC(units) result(latex_command)
        character(len=*), intent(in) :: units
        type(VARYING_STRING) :: latex_command

        latex_command = wrapInLatexUnit(var_str(units))
    end function wrapInLatexUnitC

    function wrapInLatexUnitS(units) result(latex_command)
        type(VARYING_STRING), intent(in) :: units
        type(VARYING_STRING) :: latex_command

        latex_command = "\si{" // units // "}"
    end function wrapInLatexUnitS
end module Miscellaneous_m
