module SI_SteamTable
    use class_Quantity

    implicit none
    private
    real*8,allocatable::pressure_table(:,:)

    public::initialize_steam_tables,sat_p_t,sat_p_vf,sat_p_vg,sat_p_hf,sat_p_hfg,sat_p_hg,sat_p_sf,sat_p_sfg,sat_p_sg

contains
    subroutine initialize_steam_tables()
        integer::io_stat,rows=5,i

        allocate(pressure_table(5,10))

        open(unit=11,file="C:\Users\dmnev\Documents\nmde\thermo\src\sat-pressure-table.csv", &
              status="old",action="read",iostat=io_stat)
        if (io_stat /= 0) then
            stop "Could not read steam tables"
        end if

        read(11,*)
        do i=1,rows
            read(11,"(F6.1,F8.3,F10.7,F10.6,F8.2,F7.1,F7.1,F7.4,F7.4,F7.4)") pressure_table(i,1), &
                pressure_table(i,2), pressure_table(i,3), pressure_table(i,4), &
                pressure_table(i,5), pressure_table(i,6), pressure_table(i,7), &
                pressure_table(i,8), pressure_table(i,9), pressure_table(i,10)
        end do
    end subroutine initialize_steam_tables

    function interpolate(x, y_0, y_1, x_0, x_1) result(y)
        real*8,intent(in)::y_0,y_1,x,x_0,x_1
        real*8::y

        y = y_0 + (x - x_0) * ((y_1 - y_0) / (x_1 - x_0))
        write(*,"(A,F8.3,A,F8.3,A,F8.3A,F8.3)") "Interpolated between ", &
            x_0, ", ", x_1, " for x = ", x, ": y = ", y
    end function interpolate

    function query_pressure_table(pressure, column) result(v)
        real*8,intent(in)::pressure
        integer,intent(in)::column
        logical::found
        integer::i
        real*8::v

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
            write(*,"(A,F8.3)") "Pressure not in table: ", pressure
            stop "Unhandled pressure encountered"
        end if
    end function query_pressure_table

    function sat_p_t(input_pressure) result(output_temperature)
        class(Quantity),intent(in)::input_pressure
        real*8::pressure,temperature
        type(Quantity)::output_temperature

        call verify_value(input_pressure)
        pressure = input_pressure%get_in(kPa)

        temperature = query_pressure_table(pressure, 2)
        output_temperature = Q(temperature, C)
        write(*,"(A,F6.1,A,F8.3,A)") "Saturated temperature at ", pressure, " kPa: ", temperature, " C"
    end function sat_p_t

    function sat_p_vf(input_pressure) result(output_vf)
        class(Quantity),intent(in)::input_pressure
        real*8::pressure,vf
        type(Quantity)::output_vf

        call verify_value(input_pressure)
        pressure = input_pressure%get_in(kPa)

        vf = query_pressure_table(pressure, 3)
        output_vf = Q(vf, m3_kg)
        write(*,"(A,F6.1,A,F8.3,A)") "Saturated fluid volume at ", pressure, " kPa: ", vf, " m^3/kg"
    end function sat_p_vf

    function sat_p_vg(input_pressure) result(output_vg)
        class(Quantity),intent(in)::input_pressure
        real*8::pressure,vg
        type(Quantity)::output_vg

        call verify_value(input_pressure)
        pressure = input_pressure%get_in(kPa)

        vg = query_pressure_table(pressure, 4)
        output_vg = Q(vg, m3_kg)
        write(*,"(A,F6.1,A,F8.3,A)") "Saturated vapor volume at ", pressure, " kPa: ", vg, " m^3/kg"
    end function sat_p_vg

    function sat_p_hf(input_pressure) result(output_hf)
        class(Quantity),intent(in)::input_pressure
        real*8::pressure,hf
        type(Quantity)::output_hf

        call verify_value(input_pressure)
        pressure = input_pressure%get_in(kPa)

        hf = query_pressure_table(pressure, 5)
        output_hf = Q(hf, kJ_kg)
        write(*,"(A,F6.1,A,F8.3,A)") "Saturated fluid enthalpy at ", pressure, " kPa: ", hf, " kJ/kg"
    end function sat_p_hf

    function sat_p_hfg(input_pressure) result(output_hfg)
        class(Quantity),intent(in)::input_pressure
        real*8::pressure,hfg
        type(Quantity)::output_hfg

        call verify_value(input_pressure)
        pressure = input_pressure%get_in(kPa)

        hfg = query_pressure_table(pressure, 6)
        output_hfg = Q(hfg, kJ_kg)
        write(*,"(A,F6.1,A,F8.3,A)") "Saturated evap. enthalpy at ", pressure, " kPa: ", hfg, " kJ/kg"
    end function sat_p_hfg

    function sat_p_hg(input_pressure) result(output_hg)
        class(Quantity),intent(in)::input_pressure
        real*8::pressure,hg
        type(Quantity)::output_hg

        call verify_value(input_pressure)
        pressure = input_pressure%get_in(kPa)

        hg = query_pressure_table(pressure, 7)
        output_hg = Q(hg, kJ_kg)
        write(*,"(A,F6.1,A,F8.3,A)") "Saturated vapor enthalpy at ", pressure, " kPa: ", hg, " kJ/kg"
    end function sat_p_hg

    function sat_p_sf(input_pressure) result(output_sf)
        class(Quantity),intent(in)::input_pressure
        real*8::pressure,sf
        type(Quantity)::output_sf

        call verify_value(input_pressure)
        pressure = input_pressure%get_in(kPa)

        sf = query_pressure_table(pressure, 8)
        output_sf = Q(sf, kJ_kgK)
        write(*,"(A,F6.1,A,F8.3,A)") "Saturated fluid entropy at ", pressure, " kPa: ", sf, " kJ/kg*K"
    end function sat_p_sf

    function sat_p_sfg(input_pressure) result(output_sfg)
        class(Quantity),intent(in)::input_pressure
        real*8::pressure,sfg
        type(Quantity)::output_sfg

        call verify_value(input_pressure)
        pressure = input_pressure%get_in(kPa)

        sfg = query_pressure_table(pressure, 9)
        output_sfg = Q(sfg, kJ_kgK)
        write(*,"(A,F6.1,A,F8.3,A)") "Saturated evap. entropy at ", pressure, " kPa: ", sfg, " kJ/kg*K"
    end function sat_p_sfg

    function sat_p_sg(input_pressure) result(output_sg)
        class(Quantity),intent(in)::input_pressure
        real*8::pressure,sg
        type(Quantity)::output_sg

        call verify_value(input_pressure)
        pressure = input_pressure%get_in(kPa)

        sg = query_pressure_table(pressure, 10)
        output_sg = Q(sg, kJ_kgK)
        write(*,"(A,F6.1,A,F8.3,A)") "Saturated vapor entropy at ", pressure, " kPa: ", sg, " kJ/kg*K"
    end function sat_p_sg

    subroutine verify_value(value)
        type(Quantity),intent(in)::value

        if (value%get_unit() == 0) then
            stop "Attempting to use unknown value"
        endif
    end subroutine verify_value
end module SI_SteamTable
