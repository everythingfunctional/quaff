module Conversion_factors_m
    implicit none

    ! SI Scaling
    double precision, parameter :: CENTI_PER_BASE = 100.0d0
    double precision, parameter :: MILLI_PER_BASE = 1.0d3
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

    ! Angle
    double precision, parameter :: PI = 3.14159265359d0
    double precision, parameter :: DEGREES_PER_RADIAN = 180.0d0 / PI

    ! Time
    double precision, parameter :: SECONDS_PER_MINUTE = 60.0d0
    double precision, parameter :: MINUTES_PER_HOUR = 60.0d0
    double precision, parameter :: SECONDS_PER_HOUR = SECONDS_PER_MINUTE * MINUTES_PER_HOUR
    double precision, parameter :: HOURS_PER_SECOND = 1.0d0 / SECONDS_PER_HOUR

    ! Area
    double precision, parameter :: SQUARE_CENTIMETERS_PER_SQUARE_METER = CENTIMETERS_PER_METER**2

    ! Volume
    double precision, parameter :: CUBIC_CENTIMETERS_PER_CUBIC_METER = CENTIMETERS_PER_METER**3

    ! Density
    double precision, parameter :: GRAMS_PER_CUBIC_METER_PER_KILOGRAMS_PER_CUBIC_METER = GRAMS_PER_KILOGRAM

    ! Speed
    double precision, parameter :: CENTIMETERS_PER_SECOND_PER_METERS_PER_SECOND = CENTIMETERS_PER_METER

    ! Acceleration
    double precision, parameter :: CENTIMETERS_PER_SQUARE_SECOND_PER_METERS_PER_SQUARE_SECOND = CENTIMETERS_PER_METER

    ! Force
    double precision, parameter :: MILLINEWTONS_PER_NEWTON = MILLI_PER_BASE
end module Conversion_factors_m
