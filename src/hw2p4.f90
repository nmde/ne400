module hw2p4
    use class_Quantity
    use SteamTable

    implicit none
    private

    type(Quantity)::pressure(14),temperature(14),enthalpy(14),entropy(14)

    public::problem_4
contains
    subroutine problem_4
        type(Quantity)::hf,hfg,sf,sfg,v,x,Wt,Qh,Wcp

        call initialize_steam_tables(1)

        ! Givens
        
    end subroutine problem_4

    subroutine report_point(point)
        integer,intent(in)::point
        type(Quantity)::p,h,s

        p = pressure(point)
        h = enthalpy(point)
        s = entropy(point)

        write(*,"(A,I3,A)",advance='no') "Point ", point, ": P="
        ! TODO - report actual units for all quantities
        write(*,"(F8.3,A,F8.3,A,F8.3,A)") p%get_value(), " kPa, h=", &
            h%get_value(), " kJ/kg, s=", s%get_value(), " kJ/kg*K"
        write(*,"(A)") "================================================="
    end subroutine report_point
end module hw2p4
