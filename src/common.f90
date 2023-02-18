module common
    use SteamTable
    use class_Quantity
    
    implicit none
    private

    public::h_at,s_at,given
contains
    subroutine start_eq()
        write(11,"(A)") "\begin{equation}"
    end subroutine start_eq

    subroutine end_eq()
        write(11,"(A)") "\end{equation}"
        write(11,"(A)") ""
    end subroutine end_eq

    function units_str(qu) result(str)
        type(Quantity),intent(in)::qu
        character(39)::str

        str = "\textnormal{" // qu%get_unit_str() // "}"
    end function units_str

    function given(point, varname, value, units) result(qu)
        integer,intent(in)::point,units
        character(1),intent(in)::varname
        real,intent(in)::value
        type(Quantity)::qu

        qu = Q(real(value, 8), units)
        call start_eq()
        write(11,"(A,I2,A,F8.3,A)") varname // "_{", point, "} = ", value, units_str(qu)
        call end_eq()
    end function given

    ! TODO - merge these functions into a problem class so they can access the pressures, etc. directly
    function h_at(point, pressure, quality) result(h)
        integer,intent(in)::point
        type(Quantity),intent(in)::pressure,quality
        type(Quantity)::h,hf,hfg

        hf = sat_p_hf(pressure)
        hfg = sat_p_hfg(pressure)

        h = hf%plus(quality%times(hfg, hfg%get_unit()))
        ! TODO - units
        call start_eq()
        write(11,"(A,I2,A,I2,A,I2,A,F8.3,A,F4.2,A,F8.3,A,F8.3)") "  h_{", point, "} = (h_{f} + x_{", point, &
            "}h_{fg})_{@P", point, "} = ", hf%get_value(), " + ", quality%get_value(), "\cdot", &
            hfg%get_value(), " = ", h%get_value(), units_str(h)
        call end_eq()
    end function h_at

    function s_at(point, pressure, quality) result(s)
        integer,intent(in)::point
        type(Quantity),intent(in)::pressure,quality
        type(Quantity)::s,sf,sfg

        sf = sat_p_sf(pressure)
        sfg = sat_p_sfg(pressure)

        s = sf%plus(quality%times(sfg, sfg%get_unit()))

        call start_eq()
        write(11,"(A,I2,A,I2,A,I2,A,F8.3,A,F4.2,A,F8.3,A,F8.3)") "  s_{", point, "} = (s_{f} + x_{", point, &
            "}s_{fg})_{@P", point, "} = ", sf%get_value(), " + ", quality%get_value(), "\cdot", &
            sfg%get_value(), " = ", s%get_value(), units_str(s)
        call end_eq()
    end function s_at
end module common
