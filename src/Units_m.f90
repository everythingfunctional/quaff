module Units_m
    implicit none
    private

    type, public, abstract :: Unit_t
        double precision :: multiplier
        character(len=10) :: symbol
    contains
        procedure(toString_), deferred :: toString
    end type Unit_t

    abstract interface
        function toString_(self) result(string)
            use iso_varying_string, only: VARYING_STRING
            import Unit_t
            class(Unit_t), intent(in) :: self
            type(VARYING_STRING) :: string
        end function toString_
    end interface
end module Units_m
