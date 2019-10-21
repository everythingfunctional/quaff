module Conversion_factors_m
    implicit none

    ! SI Scaling
    double precision, parameter :: CENTI_PER_BASE = 100.0d0
    double precision, parameter :: BASE_PER_KILO = 1.0d3

    ! Length
    double precision, parameter :: CENTIMETERS_PER_METER = CENTI_PER_BASE

    ! Mass
    double precision, parameter :: GRAMS_PER_KILOGRAM = BASE_PER_KILO

    ! Temperature
    double precision, parameter :: CELSIUS_KELVIN_DIFFERENCE = 273.15d0
    double precision, parameter :: FAHRENHEIT_RANKINE_DIFFERENCE = 459.67d0
    double precision, parameter :: RANKINE_PER_KELVIN = 9.0d0 / 5.0d0

    ! Amount
    double precision, parameter :: AVOGADROS_NUMBER = 6.022140857d23
end module Conversion_factors_m
