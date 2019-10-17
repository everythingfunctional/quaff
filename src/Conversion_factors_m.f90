module Conversion_factors_m
    implicit none

    ! SI Scaling
    double precision, parameter :: CENTI_PER_BASE = 100.0d0
    double precision, parameter :: BASE_PER_KILO = 1.0d3

    ! Length
    double precision, parameter :: CENTIMETERS_PER_METER = CENTI_PER_BASE

    ! Mass
    double precision, parameter :: GRAMS_PER_KILOGRAM = BASE_PER_KILO
end module Conversion_factors_m
