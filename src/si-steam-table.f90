module SI_SteamTable
    use class_Quantity

    implicit none
    private

    public::sat_p_t

contains
    function interpolate(x, y_0, y_1, x_0, x_1) result(y)
        real,intent(in)::y_0,y_1,x,x_0,x_1
        real::y

        y = y_0 + (x - x_0) * ((y_1 - y_0) / (x_1 - x_0))
    end function interpolate

    function sat_p_t(input_pressure) result(output_temperature)
        class(Quantity),intent(in)::input_pressure
        real::pressure,temperature
        type(Quantity)::output_temperature

        call verify_value(input_pressure)

        pressure = input_pressure%get_in(kPa)

        if (pressure == 3) then
            temperature = 24.100
        else if (pressure == 1000) then
            temperature = 179.88
        else if (pressure > 1000 .and. pressure < 1500) then
            temperature = interpolate(pressure, 179.88, 198.29, 1000.0, 1500.0)
        else if (pressure == 1500) then
            temperature = 198.29
        else if (pressure == 5000) then
            temperature = 263.91
        else if (pressure > 5000 .and. pressure < 7500) then
            temperature = interpolate(pressure, 263.91, 290.50, 5000.0, 7500.0)
        else if (pressure == 7500) then
            temperature = 290.50
        else
            write(*,"(A,F6.1)") "Unexpected pressure: ", pressure
            stop "Unhandled pressure encountered"
        endif
        output_temperature = Q(temperature, C)
        write(*,"(A,F6.1,A,F8.3,A)") "Saturated water temperature at ", pressure, " kPa: ", temperature, " C"
    end function sat_p_t

    subroutine verify_value(value)
        type(Quantity),intent(in)::value

        if (value%get_unit() == 0) then
            stop "Attempting to use unknown value"
        endif
    end subroutine verify_value
end module SI_SteamTable
