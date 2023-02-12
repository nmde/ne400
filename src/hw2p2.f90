module hw2p2
    use class_Quantity
    use SteamTable

    implicit none
    private

    type(Quantity)::pressure(9),temperature(9),enthalpy_s(9),enthalpy_a(9),entropy(9)

    public::problem_2
contains
    subroutine problem_2
        call initialize_steam_tables(0)
        call problem_2_a
    end subroutine problem_2

    subroutine problem_2_a
        
    end subroutine problem_2_a

    subroutine report_point(point)
        integer,intent(in)::point
        type(Quantity)::p,h_s,h_a,s

        p = pressure(point)
        h_s = enthalpy_s(point)
        h_a = enthalpy_a(point)
        s = entropy(point)

        write(*,"(A,I3,A)",advance='no') "Point ", point, ": P="
        ! TODO - report actual units for all quantities
        write(*,"(F8.3,A,F8.3,A,F8.3,A,F8.3,A)") p%get_value(), " kPa, hs=", h_s%get_value(), &
        " kJ/kg, ha=", h_a%get_value(), " kJ/kg, s=", s%get_value(), " kJ/kg*K"
        write(*,"(A)") "================================================="
    end subroutine report_point
end module hw2p2
