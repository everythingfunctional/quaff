module Conversion_factors_m
    implicit none

    ! SI Scaling
    double precision, parameter :: CENTI_PER_BASE = 100.0d0
    double precision, parameter :: MILLI_PER_BASE = 1.0d3
    double precision, parameter :: MICRO_PER_BASE = 1.0d6
    double precision, parameter :: BASE_PER_KILO = 1.0d3
    double precision, parameter :: KILO_PER_BASE = 1.0d0 / BASE_PER_KILO
    double precision, parameter :: BASE_PER_MEGA = 1.0d6
    double precision, parameter :: MEGA_PER_BASE = 1.0d0 / BASE_PER_MEGA

    ! Length
    double precision, parameter :: METERS_PER_INCH = 0.0254d0
    double precision, parameter :: INCHES_PER_METER = 1.0d0 / METERS_PER_INCH
    double precision, parameter :: INCHES_PER_FOOT = 12.0d0
    double precision, parameter :: FEET_PER_INCH = 1.0d0 / INCHES_PER_FOOT
    double precision, parameter :: CENTIMETERS_PER_METER = CENTI_PER_BASE
    double precision, parameter :: FEET_PER_METER = INCHES_PER_METER * FEET_PER_INCH
    double precision, parameter :: MICROINCHES_PER_METER = INCHES_PER_METER * MICRO_PER_BASE
    double precision, parameter :: MICROMETERS_PER_METER = MICRO_PER_BASE

    ! Mass
    double precision, parameter :: GRAMS_PER_OUNCE = 28.34952d0
    double precision, parameter :: OUNCES_PER_GRAM = 1.0d0 / GRAMS_PER_OUNCE
    double precision, parameter :: OUNCES_PER_POUND = 16.0d0
    double precision, parameter :: POUNDS_PER_OUNCE = 1.0d0 / OUNCES_PER_POUND
    double precision, parameter :: POUNDS_PER_TON = 2000.0d0
    double precision, parameter :: TONS_PER_POUND = 1.0d0 / POUNDS_PER_TON
    double precision, parameter :: GRAMS_PER_KILOGRAM = BASE_PER_KILO
    double precision, parameter :: OUNCES_PER_KILOGRAM = OUNCES_PER_GRAM * GRAMS_PER_KILOGRAM
    double precision, parameter :: POUNDS_PER_KILOGRAM = POUNDS_PER_OUNCE * OUNCES_PER_KILOGRAM
    double precision, parameter :: TONS_PER_KILOGRAM = TONS_PER_POUND * POUNDS_PER_KILOGRAM

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
    double precision, parameter :: HOURS_PER_DAY = 24.0d0
    double precision, parameter :: DAYS_PER_HOUR = 1.0d0 / HOURS_PER_DAY
    double precision, parameter :: SECONDS_PER_HOUR = SECONDS_PER_MINUTE * MINUTES_PER_HOUR
    double precision, parameter :: MINUTES_PER_SECOND = 1.0d0 / SECONDS_PER_MINUTE
    double precision, parameter :: HOURS_PER_SECOND = 1.0d0 / SECONDS_PER_HOUR
    double precision, parameter :: DAYS_PER_SECOND = DAYS_PER_HOUR * HOURS_PER_SECOND

    ! Area
    double precision, parameter :: SQUARE_CENTIMETERS_PER_SQUARE_METER = CENTIMETERS_PER_METER**2
    double precision, parameter :: SQUARE_INCHES_PER_SQUARE_METER = INCHES_PER_METER**2
    double precision, parameter :: SQUARE_FEET_PER_SQUARE_METER = FEET_PER_METER**2

    ! Volume
    double precision, parameter :: CUBIC_CENTIMETERS_PER_CUBIC_METER = CENTIMETERS_PER_METER**3

    ! Density
    double precision, parameter :: GRAMS_PER_CUBIC_METER_PER_KILOGRAMS_PER_CUBIC_METER = GRAMS_PER_KILOGRAM

    ! Speed
    double precision, parameter :: CENTIMETERS_PER_SECOND_PER_METERS_PER_SECOND = CENTIMETERS_PER_METER
    double precision, parameter :: FEET_PER_SECOND_PER_METERS_PER_SECOND = FEET_PER_METER

    ! Acceleration
    double precision, parameter :: GRAVITY = 9.80665d0 ! m/s^2 according to Wikipedia
    double precision, parameter :: CENTIMETERS_PER_SQUARE_SECOND_PER_METERS_PER_SQUARE_SECOND = CENTIMETERS_PER_METER
    double precision, parameter :: FEET_PER_SQUARE_SECOND_PER_METERS_PER_SQUARE_SECOND = FEET_PER_METER

    ! Force
    ! Note: 1 N = 1 (kg m)/s^2
    double precision, parameter :: DYNES_PER_NEWTON = &
            GRAMS_PER_KILOGRAM * CENTIMETERS_PER_SQUARE_SECOND_PER_METERS_PER_SQUARE_SECOND
    double precision, parameter :: KILOPONDS_PER_NEWTON = 1.0d0 / GRAVITY ! 1 kp = 1 kg * gravity
    double precision, parameter :: MILLINEWTONS_PER_NEWTON = MILLI_PER_BASE
    double precision, parameter :: POUNDS_PER_NEWTON = POUNDS_PER_KILOGRAM / GRAVITY ! 1 lbf = 1 lbm * gravity

    ! Energy
    ! Note: 1 J = 1 (N m)
    double precision, parameter :: JOULES_PER_CALORIE = 4.184d0
    double precision, parameter :: BTU_PER_JOULE = 1055.06d0
    double precision, parameter :: CALORIES_PER_JOULE = 1.0d0 / JOULES_PER_CALORIE
    double precision, parameter :: KILOJOULES_PER_JOULE = KILO_PER_BASE
    double precision, parameter :: MEGABTU_PER_JOULE = MEGA_PER_BASE * BTU_PER_JOULE
    double precision, parameter :: MEGAWATT_DAYS_PER_JOULE = MEGA_PER_BASE * DAYS_PER_SECOND

    ! Power
    ! Note: 1 W = 1 J/s
    double precision, parameter :: BTU_PER_HOUR_PER_WATT = BTU_PER_JOULE * SECONDS_PER_HOUR
    double precision, parameter :: CALORIES_PER_SECOND_PER_WATT = CALORIES_PER_JOULE
    double precision, parameter :: MEGABTU_PER_HOUR_PER_WATT = MEGA_PER_BASE * BTU_PER_HOUR_PER_WATT
    double precision, parameter :: MEGAWATTS_PER_WATT = MEGA_PER_BASE

    ! Pressure
    ! Note: 1 Pa = 1 N/m^2
    double precision, parameter :: DYNES_PER_SQUARE_CENTIMETER_PER_PASCAL = DYNES_PER_NEWTON / SQUARE_CENTIMETERS_PER_SQUARE_METER
    double precision, parameter :: KILOPASCALS_PER_PASCAL = KILO_PER_BASE
    double precision, parameter :: KILOPONDS_PER_SQUARE_CENTIMETER_PER_PASCAL = &
            KILOPONDS_PER_NEWTON / SQUARE_CENTIMETERS_PER_SQUARE_METER
    double precision, parameter :: MEGAPASCALS_PER_PASCAL = MEGA_PER_BASE
    double precision, parameter :: POUNDS_PER_SQUARE_INCH_PER_PASCAL = POUNDS_PER_NEWTON / SQUARE_INCHES_PER_SQUARE_METER

    ! Dynamic Viscosity
    double precision, parameter :: MEGAPASCAL_SECONDS_PER_PASCAL_SECOND = MEGA_PER_BASE

    ! Enthalpy
    double precision, parameter :: KILOJOULES_PER_KILOGRAM_PER_JOULES_PER_KILOGRAM = KILO_PER_BASE
end module Conversion_factors_m
