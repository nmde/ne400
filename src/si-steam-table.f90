module SI_SteamTable
    use class_Quantity

    implicit none
    private
    real::pressure_table(5,2)

    public::initialize_steam_tables,sat_p_t

contains
    subroutine initialize_steam_tables()
        ! TODO: Supply path to table in arguments
        pressure_table(1,1) = 3
        pressure_table(1,2) = 24.1
        pressure_table(2,1) = 1000
        pressure_table(2,2) = 179.88
        pressure_table(3,1) = 1500
        pressure_table(3,2) = 198.29
        pressure_table(4,1) = 5000
        pressure_table(4,2) = 263.91
        pressure_table(5,1) = 7500
        pressure_table(5,2) = 290.501
    end subroutine initialize_steam_tables

    function interpolate(x, y_0, y_1, x_0, x_1) result(y)
        real,intent(in)::y_0,y_1,x,x_0,x_1
        real::y

        y = y_0 + (x - x_0) * ((y_1 - y_0) / (x_1 - x_0))
    end function interpolate

    function query_pressure_table(pressure, column) result(v)
        real,intent(in)::pressure
        integer,intent(in)::column
        logical::found
        integer::i
        real::v

        found = .false.
        i = 1
        do while (i < 6 .and. (.not. found))
            if (pressure == pressure_table(i, 1)) then
                v = pressure_table(i, column)
                found = .true.
            else if (i > 1) then
                if ((pressure > pressure_table(i - 1, 1)) .and. (pressure < pressure_table(i, 1))) then
                    v = interpolate( &
                            pressure, &
                            pressure_table(i - 1, column), &
                            pressure_table(i, column), &
                            pressure_table(i - 1, 1), &
                            pressure_table(i, 1))
                    found = .true.
                end if
            endif
            i = i + 1
        end do

        if (.not. found) then
            write(*,"(A,F6.1)") "Pressure not in table: ", pressure
            stop "Unhandled pressure encountered"
        end if
    end function query_pressure_table

    function sat_p_t(input_pressure) result(output_temperature)
        class(Quantity),intent(in)::input_pressure
        real::pressure,temperature
        type(Quantity)::output_temperature

        call verify_value(input_pressure)
        pressure = input_pressure%get_in(kPa)

        temperature = query_pressure_table(pressure, 2)
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
