module SteamTable
    use class_Quantity

    implicit none
    private
    real*8,allocatable::pressure_table(:,:),temperature_table(:,:)
    integer::units,ptable_rows,ttable_rows,pressure_units,temperature_units,v_units,h_units,s_units

    public::initialize_steam_tables,sat_p_t,sat_p_vf,sat_p_vg,sat_p_hf,sat_p_hfg,sat_p_hg,sat_p_sf,sat_p_sfg,sat_p_sg, &
        sat_t_p

contains
    subroutine initialize_steam_tables(set_units)
        integer,intent(in)::set_units
        integer::io_stat,i

        units = set_units

        if (units == 0) then
            pressure_units = kPa
            temperature_units = C
            v_units = m3_kg
            h_units = kJ_kg
            s_units = kJ_kgK
        else
            pressure_units = psia
            temperature_units = F
            v_units = ft3_lbm
            h_units = btu_lbm
            s_units = btu_lbmR
        endif

        if (units == 0) then
            write(*,"(A)") "Using SI steam tables"
            open(unit=11,file="C:\Users\dmnev\Documents\nmde\thermo\src\si/pressure-table.txt", &
                status="old",action="read",iostat=io_stat)
            ptable_rows = 5
        else
            write(*,"(A)") "Using imperial steam tables"
            open(unit=11,file="C:\Users\dmnev\Documents\nmde\thermo\src\imperial/pressure-table.txt", &
                status="old",action="read",iostat=io_stat)
            ptable_rows = 31
        end if

        if (io_stat /= 0) then
            stop "Could not read steam tables"
        end if

        allocate(pressure_table(ptable_rows,10))

        if (units == 0) then
            write(*,"(A)") "Using SI temperature tables"
            open(unit=12,file="C:\Users\dmnev\Documents\nmde\thermo\src\si/temperature-table.txt", &
                status="old",action="read",iostat=io_stat)
            ttable_rows = 0
        else
            write(*,"(A)") "Using imperial temperature tables"
            open(unit=12,file="C:\Users\dmnev\Documents\nmde\thermo\src\imperial/temperature-table.txt", &
                status="old",action="read",iostat=io_stat)
            ttable_rows = 2
        end if

        if (io_stat /= 0) then
            stop "Could not read temperature tables"
        end if

        allocate(temperature_table(ttable_rows,2))

        read(11,*)
        do i=1,ptable_rows
            read(11,"(F6.1,F8.3,F10.7,F11.6,F8.2,F7.1,F7.1,F7.4,F7.4,F7.4)") pressure_table(i,1), &
                pressure_table(i,2), pressure_table(i,3), pressure_table(i,4), &
                pressure_table(i,5), pressure_table(i,6), pressure_table(i,7), &
                pressure_table(i,8), pressure_table(i,9), pressure_table(i,10)
        end do

        read(12,*)
        do i=1,ttable_rows
            read(12,"(F5.1,F8.3)") temperature_table(i,1), &
                temperature_table(i,2)
        end do

        write(*,"(A,F8.3)") "Sanity check: Last read row = ", pressure_table(ptable_rows, 1)
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
        do while (i < ptable_rows + 1 .and. (.not. found))
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

    function query_temperature_table(temperature, column) result(v)
        real*8,intent(in)::temperature
        integer,intent(in)::column
        logical::found
        integer::i
        real*8::v

        found = .false.
        i = 1
        do while (i < ttable_rows + 1 .and. (.not. found))
            if (temperature == temperature_table(i, 1)) then
                v = temperature_table(i, column)
                found = .true.
            else if (i > 1) then
                if ((temperature > temperature_table(i - 1, 1)) .and. (temperature < temperature_table(i, 1))) then
                    v = interpolate( &
                            temperature, &
                            temperature_table(i - 1, column), &
                            temperature_table(i, column), &
                            temperature_table(i - 1, 1), &
                            temperature_table(i, 1))
                    found = .true.
                end if
            endif
            i = i + 1
        end do

        if (.not. found) then
            write(*,"(A,F8.3)") "Temperature not in table: ", temperature
            stop "Unhandled temperature encountered"
        end if
    end function query_temperature_table

    function get_in_units(qu, type) result(v)
        type(Quantity),intent(in)::qu
        integer,intent(in)::type
        real*8::v
        type(Quantity)::temp

        call verify_value(qu)

        if (type == 0) then
            temp = qu%get_in(pressure_units)
            v = temp%get_value()
        else if (type == 1) then
            temp = qu%get_in(temperature_units)
            v = temp%get_value()
        end if
    end function get_in_units

    function sat_p_t(input_pressure) result(output_temperature)
        class(Quantity),intent(in)::input_pressure
        real*8::pressure,temperature
        type(Quantity)::output_temperature

        pressure = get_in_units(input_pressure, 0)
        temperature = query_pressure_table(pressure, 2)
        output_temperature = Q(temperature, temperature_units)
        write(*,"(A,F6.1,A,F8.3,A)") "Saturated temperature at ", pressure, " kPa: ", temperature, " C"
    end function sat_p_t

    function sat_t_p(input_temperature) result(output_pressure)
        class(Quantity),intent(in)::input_temperature
        real*8::pressure,temperature
        type(Quantity)::output_pressure

        temperature = get_in_units(input_temperature, 1)
        pressure = query_temperature_table(temperature, 2)
        output_pressure = Q(pressure, pressure_units)
        write(*,"(A,F6.1,A,F8.3,A)") "Saturated pressure at ", temperature, " F: ", pressure, " psia"
    end function sat_t_p

    function sat_p_vf(input_pressure) result(output_vf)
        class(Quantity),intent(in)::input_pressure
        real*8::pressure,vf
        type(Quantity)::output_vf

        pressure = get_in_units(input_pressure, 0)
        vf = query_pressure_table(pressure, 3)
        output_vf = Q(vf, v_units)
        write(*,"(A,F6.1,A,F8.3,A)") "Saturated fluid volume at ", pressure, " kPa: ", vf, " m^3/kg"
    end function sat_p_vf

    function sat_p_vg(input_pressure) result(output_vg)
        class(Quantity),intent(in)::input_pressure
        real*8::pressure,vg
        type(Quantity)::output_vg

        pressure = get_in_units(input_pressure, 0)
        vg = query_pressure_table(pressure, 4)
        output_vg = Q(vg, v_units)
        write(*,"(A,F6.1,A,F8.3,A)") "Saturated vapor volume at ", pressure, " kPa: ", vg, " m^3/kg"
    end function sat_p_vg

    function sat_p_hf(input_pressure) result(output_hf)
        class(Quantity),intent(in)::input_pressure
        real*8::pressure,hf
        type(Quantity)::output_hf

        pressure = get_in_units(input_pressure, 0)
        hf = query_pressure_table(pressure, 5)
        output_hf = Q(hf, h_units)
        write(*,"(A,F6.1,A,F8.3,A)") "Saturated fluid enthalpy at ", pressure, " kPa: ", hf, " kJ/kg"
    end function sat_p_hf

    function sat_p_hfg(input_pressure) result(output_hfg)
        class(Quantity),intent(in)::input_pressure
        real*8::pressure,hfg
        type(Quantity)::output_hfg

        pressure = get_in_units(input_pressure, 0)
        hfg = query_pressure_table(pressure, 6)
        output_hfg = Q(hfg, h_units)
        write(*,"(A,F6.1,A,F8.3,A)") "Saturated evap. enthalpy at ", pressure, " kPa: ", hfg, " kJ/kg"
    end function sat_p_hfg

    function sat_p_hg(input_pressure) result(output_hg)
        class(Quantity),intent(in)::input_pressure
        real*8::pressure,hg
        type(Quantity)::output_hg

        pressure = get_in_units(input_pressure, 0)
        hg = query_pressure_table(pressure, 7)
        output_hg = Q(hg, h_units)
        write(*,"(A,F6.1,A,F8.3,A)") "Saturated vapor enthalpy at ", pressure, " kPa: ", hg, " kJ/kg"
    end function sat_p_hg

    function sat_p_sf(input_pressure) result(output_sf)
        class(Quantity),intent(in)::input_pressure
        real*8::pressure,sf
        type(Quantity)::output_sf

        pressure = get_in_units(input_pressure, 0)
        sf = query_pressure_table(pressure, 8)
        output_sf = Q(sf, s_units)
        write(*,"(A,F6.1,A,F8.3,A)") "Saturated fluid entropy at ", pressure, " kPa: ", sf, " kJ/kg*K"
    end function sat_p_sf

    function sat_p_sfg(input_pressure) result(output_sfg)
        class(Quantity),intent(in)::input_pressure
        real*8::pressure,sfg
        type(Quantity)::output_sfg

        pressure = get_in_units(input_pressure, 0)
        sfg = query_pressure_table(pressure, 9)
        output_sfg = Q(sfg, s_units)
        write(*,"(A,F6.1,A,F8.3,A)") "Saturated evap. entropy at ", pressure, " kPa: ", sfg, " kJ/kg*K"
    end function sat_p_sfg

    function sat_p_sg(input_pressure) result(output_sg)
        class(Quantity),intent(in)::input_pressure
        real*8::pressure,sg
        type(Quantity)::output_sg

        pressure = get_in_units(input_pressure, 0)
        sg = query_pressure_table(pressure, 10)
        output_sg = Q(sg, s_units)
        write(*,"(A,F6.1,A,F8.3,A)") "Saturated vapor entropy at ", pressure, " kPa: ", sg, " kJ/kg*K"
    end function sat_p_sg

    subroutine verify_value(value)
        type(Quantity),intent(in)::value

        if (value%get_unit() == 0) then
            stop "Attempting to use unknown value"
        endif
    end subroutine verify_value
end module SteamTable
