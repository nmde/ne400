module hw2p4
    use class_Quantity
    use SteamTable

    implicit none
    private

    type(Quantity)::pressure(14),temperature(14),enthalpy(14),entropy(14)

    public::hw2_problem_4
contains
    subroutine hw2_problem_4
        type(Quantity)::hf,hfg,sf,sfg,v,x,Wt,Qh,Wcp,Wfwp,ratio,one,efficiency

        call initialize_steam_tables(1)

        ! Extraction pressure
        pressure(3) = Q(125D0, psia)

        ! Point 2
        pressure(2) = Q(780D0, psia)
        temperature(2) = Q(800D0, F)
        enthalpy(2) = Q(1400.06D0, btu_lbm)
        entropy(2) = Q(1.6014D0, btu_lbmR)
        call report_point(2)

        ! Point 3
        entropy(3) = entropy(2)
        hf = sat_p_hf(pressure(3))
        hfg = sat_p_hfg(pressure(3))
        sf = sat_p_sf(pressure(3))
        sfg = sat_p_sfg(pressure(3))
        x = entropy(3)%minus(sf)
        x = x%divide(sfg, unitless)
        enthalpy(3) = hf%plus(x%times(hfg, btu_lbm))
        call report_point(3)

        ! Point 4
        pressure(4) = Q(3D0, psia)
        entropy(4) = entropy(2)
        hf = sat_p_hf(pressure(4))
        hfg = sat_p_hfg(pressure(4))
        sf = sat_p_sf(pressure(4))
        sfg = sat_p_sfg(pressure(4))
        x = entropy(4)%minus(sf)
        x = x%divide(sfg, unitless)
        enthalpy(4) = hf%plus(x%times(hfg, btu_lbm))
        call report_point(4)

        ! Point 5
        pressure(5) = pressure(4)
        enthalpy(5) = sat_p_hf(pressure(5))
        entropy(5) = sat_p_sf(pressure(5))
        call report_point(5)

        ! Point 6
        pressure(6) = pressure(3)
        v = sat_p_vf(pressure(6))
        enthalpy(6) = enthalpy(5)%plus(v%times(pressure(6)%minus(pressure(5)), btu_lbm))
        sf = sat_p_sf(pressure(6))
        sfg = sat_p_sfg(pressure(6))
        x = enthalpy(6)%minus(sat_p_hf(pressure(6)))
        x = x%divide(sat_p_hfg(pressure(6)), unitless)
        entropy(6) = sf%plus(x%times(sfg, btu_lbmR))
        call report_point(6)

        ! Point 7
        pressure(7) = pressure(3)
        enthalpy(7) = sat_p_hf(pressure(7))
        entropy(7) = sat_p_sf(pressure(7))
        call report_point(7)

        ! Point 1
        pressure(1) = pressure(2)
        v = sat_p_vf(pressure(7))
        enthalpy(1) = enthalpy(7)%plus(v%times(pressure(1)%minus(pressure(7)), btu_lbm))

        ratio = enthalpy(7)%minus(enthalpy(6))
        ratio = ratio%divide(enthalpy(3)%minus(enthalpy(6)), unitless)
        
        Wt = enthalpy(2)
        Wt = Wt%minus(ratio%times(enthalpy(3), btu_lbm))
        one =  Q(1D0, unitless)
        Wt = Wt%minus(enthalpy(4)%times(one%minus(ratio),btu_lbm))
        write(*,"(A,F8.3)") "Turbine work: ", Wt%get_value()

        Wcp = enthalpy(6)%minus(enthalpy(5))
        Wcp = Wcp%times(one%minus(ratio), btu_lbm)
        write(*,"(A,F8.3)") "Condensate pump work: ", Wcp%get_value()

        Wfwp = enthalpy(1)%minus(enthalpy(7))
        write(*,"(A,F8.3)") "Feedwater pump work: ", Wfwp%get_value()

        Qh = enthalpy(2)%minus(enthalpy(1))
        write(*,"(A,F8.3)") "Qh: ", Qh%get_value()

        efficiency = Wt%plus(Wcp%plus(Wfwp))
        efficiency = efficiency%divide(Qh, unitless)
        write(*,"(A,F8.3)") "Cycle efficiency: ", efficiency%get_value()
    end subroutine hw2_problem_4

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
