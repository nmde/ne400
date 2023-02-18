module hw3p1
    use class_Quantity
    use SteamTable
    use common

    implicit none
    private

    type(Quantity)::pressure(21),temperature(21),enthalpy(21),entropy(21),quality(21)

    public::hw3_problem_1
contains
    subroutine hw3_problem_1
        type(Quantity)::turbine_efficiency,pump_efficiency,env_temperature,hf,v,x_s,x_a,&
            Wshpt,Wahpt,Wslpt,Walpt,Wsmcp,Wamcp,Wsfwp,Wafwp,Qah,Walpt_m1,ratio,Wamcp_m1, &
            efficiency,hfg,sf,sfg,i_hpt,i_lpt,i_mcp,i_fwp,temp
        integer::io_err

        call initialize_steam_tables(1)

        open(unit=11,file="C:\Users\dmnev\Documents\nmde\thermo\output\hw3p1.tex",action="write",iostat=io_err)
        if (io_err /= 0) then
            stop "Could not write to hw3p1.tex"
        end if

        ! Point 2
        quality(2) = given(2, "x", 0.95, unitless)
        pressure(2) = given(2, "P", 1240., psia)
        enthalpy(2) = h_at(2, pressure(2), quality(2))
        entropy(2) = s_at(2, pressure(2), quality(2))
        call report_point(2)

        ! Point 1
        temperature(1) = Q(480D0, F)
        pressure(1) = Q(580D0, psia)

        ! Point 3
        pressure(3) = Q(940D0, psia)

        ! Point 4
        pressure(4) = Q(385D0, psia)

        ! Point 5
        pressure(5) = Q(160D0, psia)

        ! Point 6
        temperature(6) = Q(560D0, F)

        ! Point 7

        ! Point 8
        pressure(8) = Q(110D0, psia)

        ! Point 9
        pressure(9) = Q(25D0, psia)

        ! Point 10
        pressure(10) = Q(2D0, psia)

        ! Point 16
        temperature(16) = Q(330D0, F)

        ! Point 18
        temperature(18) = Q(645D0, F)
        pressure(18) = Q(2350D0, psia)

        ! Point 19
        temperature(19) = Q(575D0, F)
        pressure(19) = Q(2250D0, psia)

        ! Point 20
        pressure(20) = Q(2350D0, psia)
    end subroutine hw3_problem_1

    subroutine report_point(point)
        integer,intent(in)::point
        type(Quantity)::p,h,s

        p = pressure(point)
        h = enthalpy(point)
        s = entropy(point)

        write(*,"(A,I3,A)",advance='no') "Point ", point, ": P="
        ! TODO - report actual units for all quantities
        write(*,"(F8.3,A,F8.3,A,F8.3,A)") p%get_value(), " kPa, h=", h%get_value(), " kJ/kg, s=", s%get_value(), " kJ/kg*K"
        write(*,"(A)") "================================================="
    end subroutine report_point
end module hw3p1
