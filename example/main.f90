program speed_calculator
    use erloff, only: error_list_t
    use iso_varying_string, only: varying_string, operator(//), get, put_line
    use quaff, only: &
            fallible_length_t, &
            fallible_speed_unit_t, &
            fallible_time_t, &
            length_t, &
            speed_t, &
            speed_unit_t, &
            time_t, &
            parse_length, &
            parse_speed_unit, &
            parse_time, &
            operator(/)
    implicit none

    type(length_t) :: distance
    type(error_list_t) :: errors
    type(fallible_length_t) :: maybe_distance
    type(fallible_speed_unit_t) :: maybe_speed_unit
    type(fallible_time_t) :: maybe_time
    type(varying_string) :: response
    type(speed_t) :: speed
    class(speed_unit_t), allocatable :: speed_unit
    type(time_t) :: time

    do
        call put_line("How far did you travel?")
        call get(response)
        maybe_distance = parse_length(response)
        if (maybe_distance%failed()) then
            errors = maybe_distance%errors()
            call put_line(errors%to_string())
        else
            distance = maybe_distance%length()
            exit
        end if
    end do
    do
        call put_line("How long did it take you?")
        call get(response)
        maybe_time = parse_time(response)
        if (maybe_time%failed()) then
            errors = maybe_time%errors()
            call put_line(errors%to_string())
        else
            time = maybe_time%time()
            exit
        end if
    end do
    do
        call put_line("What units would you like for output?")
        call get(response)
        maybe_speed_unit = parse_speed_unit(response)
        if (maybe_speed_unit%failed()) then
            errors = maybe_speed_unit%errors()
            call put_line(errors%to_string())
        else
            speed_unit = maybe_speed_unit%unit()
            exit
        end if
    end do
    speed = distance / time
    call put_line("Speed was " // speed%to_string_in(speed_unit))
end program
