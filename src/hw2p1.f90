module hw2p1
    use class_Quantity
    use SteamTable

    implicit none
    private

    type(Quantity)::pressure(9),temperature(9),enthalpy_s(9),enthalpy_a(9),entropy_a(9),entropy_s(9)

    public::problem_1
contains
    subroutine problem_1
        type(Quantity)::turbine_efficiency,pump_efficiency,env_temperature,hf,v,x_s,x_a,&
            Wshpt,Wahpt,Wslpt,Walpt,Wsmcp,Wamcp,Wsfwp,Wafwp,Qah,Walpt_m1,ratio,Wamcp_m1, &
            efficiency,hfg,sf,sfg,i_hpt,i_lpt,i_mcp,i_fwp,temp

        call initialize_steam_tables(0)

        ! Givens
        pressure(1) = Q(6880D0, kPa)
        pressure(2) = Q(1280D0, kPa)
        pressure(3) = Q(1280D0, kPa)
        pressure(4) = Q(1280D0, kPa)
        pressure(5) = Q(3D0, kPa)
        pressure(6) = Q(3D0, kPa)
        pressure(7) = Q(1280D0, kPa)
        pressure(8) = Q(1280D0, kPa)
        pressure(9) = Q(6880D0, kPa)
        turbine_efficiency = Q(0.9D0, unitless)
        pump_efficiency = Q(0.8D0, unitless)
        env_temperature = Q(27D0, C)
        temp = Q(env_temperature%get_in(K), K)

        ! Point 1 - saturated vapor
        temperature(1) = sat_p_t(pressure(1))
        enthalpy_s(1) = sat_p_hg(pressure(1))
        enthalpy_a(1) = enthalpy_s(1)
        entropy_s(1) = sat_p_sg(pressure(1))
        entropy_a(1) = entropy_s(1)
        call report_point(1)

        ! Point 2
        entropy_s(2) = entropy_s(1)
        x_s = entropy_s(2)%minus(sat_p_sf(pressure(2)))
        x_s = x_s%divide(sat_p_sfg(pressure(2)), unitless)
        hf = sat_p_hf(pressure(2))
        hfg = sat_p_hfg(pressure(2))
        enthalpy_s(2) = hf%plus(x_s%times(hfg, kJ_kg))

        Wshpt = enthalpy_s(1)%minus(enthalpy_s(2))
        write(*,"(A,F8.3)") "Ideal HPT work: ", Wshpt%get_value()

        enthalpy_a(2) = enthalpy_a(1)%minus(Wshpt%times(turbine_efficiency, kJ_kg))
        x_a = enthalpy_a(2)%minus(hf)
        x_a = x_a%divide(hfg, unitless)
        sf = sat_p_sf(pressure(2))
        sfg = sat_p_sfg(pressure(2))
        entropy_a(2) = sf%plus(x_a%times(sfg, kJ_kgK))
        call report_point(2)

        Wahpt = enthalpy_a(1)%minus(enthalpy_a(2))
        write(*,"(A,F8.3)") "Actual HPT work: ", Wahpt%get_value()

        ! Point 3 - saturated vapor
        enthalpy_s(3) = sat_p_hg(pressure(3))
        enthalpy_a(3) = enthalpy_s(3)
        entropy_s(3) = sat_p_sg(pressure(3))
        entropy_a(3) = entropy_s(3)
        call report_point(3)

        ! Point 5
        entropy_s(5) = entropy_s(3)
        x_s = entropy_s(5)%minus(sat_p_sf(pressure(5)))
        x_s = x_s%divide(sat_p_sfg(pressure(5)), unitless)
        hf = sat_p_hf(pressure(5))
        hfg = sat_p_hfg(pressure(5))
        enthalpy_s(5) = hf%plus(x_s%times(hfg, kJ_kg))

        Wslpt = enthalpy_s(3)%minus(enthalpy_s(5))
        write(*,"(A,F8.3)") "Ideal LPT work: ", Wslpt%get_value()

        enthalpy_a(5) = enthalpy_a(3)%minus(Wslpt%times(turbine_efficiency, kJ_kg))
        x_a = enthalpy_a(5)%minus(hf)
        x_a = x_a%divide(hfg, unitless)
        sf = sat_p_sf(pressure(5))
        sfg = sat_p_sfg(pressure(5))
        entropy_a(5) = sf%plus(x_a%times(sfg, kJ_kgK))
        call report_point(5)

        Walpt = enthalpy_a(3)%minus(enthalpy_a(5))
        write(*,"(A,F8.3)") "Actual LPT work: ", Walpt%get_value()

        ! Point 6 - saturated liquid
        enthalpy_s(6) = sat_p_hf(pressure(6))
        enthalpy_a(6) = enthalpy_s(6)
        entropy_s(6) = sat_p_sf(pressure(6))
        entropy_a(6) = entropy_s(6)
        v = sat_p_vf(pressure(6))
        call report_point(6)

        Wsmcp = v%times(pressure(7)%minus(pressure(6)), kJ_kg)
        write(*,"(A,F8.3)") "Ideal MCP work: ", Wsmcp%get_value()

        enthalpy_a(7) = enthalpy_s(6)%minus(pump_efficiency%times(Wsmcp, kJ_kg))

        Wamcp = enthalpy_a(7)%minus(enthalpy_s(6))
        Wamcp = Wamcp%times(Q(-1D0, unitless), kJ_kg)
        write(*,"(A,F8.3)") "Actual MCP work: ", Wamcp%get_value() ! POTENTIAL PROBLEM

        enthalpy_s(7) = Wsmcp%plus(enthalpy_s(6))
        hf = sat_p_hf(pressure(7))
        hfg = sat_p_hfg(pressure(7))
        x_a = enthalpy_a(7)%minus(hf)
        x_a = x_a%divide(hfg, unitless)
        sf = sat_p_sf(pressure(7))
        sfg = sat_p_sfg(pressure(7))
        entropy_a(7) = sf%plus(x_a%times(sfg, kJ_kgK))
        call report_point(7)

        ! Point 8
        enthalpy_s(8) = sat_p_hf(pressure(8))
        enthalpy_a(8) = enthalpy_s(8)
        entropy_s(8) = sat_p_sf(pressure(8))
        entropy_a(8) = entropy_s(8)

        v = sat_p_vf(pressure(8))
        Wsfwp = v%times(pressure(9)%minus(pressure(8)), kJ_kg)
        write(*,"(A,F8.3)") "Ideal FWP work: ", Wsfwp%get_value()

        call report_point(8)

        enthalpy_a(9) = enthalpy_s(8)%minus(pump_efficiency%times(Wsfwp, kJ_kg))

        Wafwp = enthalpy_a(9)%minus(enthalpy_a(8))
        Wafwp = Wafwp%times(Q(-1D0, unitless), kJ_kg)
        write(*,"(A,F8.3)") "Actual FWP work: ", Wafwp%get_value()

        enthalpy_s(9) = Wsfwp%plus(enthalpy_s(8))
        hf = sat_p_hf(pressure(9))
        hfg = sat_p_hfg(pressure(9))
        x_a = enthalpy_a(9)%minus(hf)
        x_a = x_a%divide(hfg, unitless)
        sf = sat_p_sf(pressure(9))
        sfg = sat_p_sfg(pressure(9))
        entropy_a(9) = sf%plus(x_a%times(sfg, kJ_kgK))
        call report_point(9)

        Qah = enthalpy_a(1)%minus(enthalpy_a(9))
        write(*,"(A,F8.3)") "Heat in: ", Qah%get_value()

        ! Point 4 - saturated liquid
        enthalpy_s(4) = enthalpy_a(2)
        enthalpy_a(4) = enthalpy_s(4)
        entropy_s(4) = sat_p_sg(pressure(4))
        call report_point(4)

        ratio = enthalpy_a(4)%minus(enthalpy_a(7))
        ratio = ratio%divide(enthalpy_a(4)%minus(enthalpy_a(8)), unitless)
        write(*,"(A,F8.3)") "Mass rate ratio: ", ratio%get_value()

        Walpt_m1 = Walpt%divide(ratio, kJ_kg)
        write(*,"(A,F8.3)") "Adjusted Walpt: ", Walpt_m1%get_value()

        Wamcp_m1 = Wamcp%divide(ratio, kJ_kg)
        write(*,"(A,F8.3)") "Adjusted Wamcp: ", Wamcp_m1%get_value()

        efficiency = Wahpt%plus(Walpt_m1%plus(Wamcp_m1%plus(Wafwp)))
        efficiency = efficiency%divide(Qah, unitless)
        write(*, "(A,F8.3)") "Cycle efficiency: ", efficiency%get_value()

        i_hpt = temp%times(entropy_a(1)%plus(entropy_a(2)), kJ_kg)
        write(*, "(A,F8.3)") "HPT irreversiblity: ", i_hpt%get_value()

        i_lpt = temp%times(entropy_a(3)%plus(entropy_a(5)), kJ_kg)
        i_lpt = i_lpt%divide(ratio, kJ_kg)
        write(*, "(A,F8.3)") "LPT irreversiblity: ", i_lpt%get_value()

        i_mcp = temp%times(entropy_a(6)%plus(entropy_a(7)), kJ_kg)
        i_mcp = i_mcp%divide(ratio, kJ_kg)
        write(*, "(A,F8.3)") "MCP irreversiblity: ", i_mcp%get_value()

        i_fwp = temp%times(entropy_a(8)%plus(entropy_a(9)), kJ_kg)
        write(*, "(A,F8.3)") "FWP irreversiblity: ", i_fwp%get_value()
    end subroutine problem_1

    subroutine report_point(point)
        integer,intent(in)::point
        type(Quantity)::p,h_s,h_a,s_s,s_a

        p = pressure(point)
        h_s = enthalpy_s(point)
        h_a = enthalpy_a(point)
        s_s = entropy_s(point)
        s_a = entropy_a(point)

        write(*,"(A,I3,A)",advance='no') "Point ", point, ": P="
        ! TODO - report actual units for all quantities
        write(*,"(F8.3,A,F8.3,A,F8.3,A,F8.3,A,F8.3,A)") p%get_value(), " kPa, hs=", h_s%get_value(), &
        " kJ/kg, ha=", h_a%get_value(), " kJ/kg, ss=", s_s%get_value(), " kJ/kg*K, sa=", s_a%get_value(), " kJ/kg*K"
        write(*,"(A)") "================================================="
    end subroutine report_point
end module hw2p1
