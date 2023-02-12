module hw2p3
    use class_Quantity
    use SteamTable

    implicit none
    private

    type(Quantity)::pressure(14),temperature(14),enthalpy(14),entropy(14)

    public::problem_3
contains
    subroutine problem_3
        type(Quantity)::hf,v,x,x2,Whpt,Wlpt,Wcp,Qh

        call initialize_steam_tables(1)

        ! Givens
        temperature(1) = Q(395D0, F)
        pressure(2) = Q(1220D0, psia)
        x2 = Q(0.95D0, unitless)
        pressure(3) = Q(840D0, psia)
        pressure(4) = Q(350D0, psia)
        temperature(7) = Q(530D0, F)
        pressure(8) = Q(3.0D0, psia)
        pressure(14) = Q(65D0, psia)

        ! Point 2
        enthalpy(2) = sat_p_hg(pressure(2))
    end subroutine problem_3

    subroutine report_point(point)
        integer,intent(in)::point
        type(Quantity)::p,h,s

        p = pressure(point)
        h = enthalpy(point)
        s = entropy(point)

        write(*,"(A,I3,A)",advance='no') "Point ", point, ": P="
        ! TODO - report actual units for all quantities
        write(*,"(F8.3,A,F8.3,A,F8.3,A)") p%get_value(), " kPa, h=", &
            " kJ/kg, s=", s%get_value(), " kJ/kg*K"
        write(*,"(A)") "================================================="
    end subroutine report_point
end module hw2p3
