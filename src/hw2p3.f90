module hw2p3
    use class_Quantity
    use SteamTable

    implicit none
    private

    type(Quantity)::pressure(14),temperature(14),enthalpy(14),entropy(14)

    public::problem_3
contains
    subroutine problem_3
        type(Quantity)::hf,hfg,sf,sfg,v,x,x2,Whpt,Wlpt,Wcp,Wfwp,Qh,m3_m1, &
            m2_m1

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
        x = x2
        hf = sat_p_hf(pressure(2))
        hfg = sat_p_hfg(pressure(2))
        sf = sat_p_sf(pressure(2))
        sfg = sat_p_sfg(pressure(2))
        enthalpy(2) = hf%plus(x%times(hfg, btu_lbm))
        entropy(2) = sf%plus(x%times(sfg, btu_lbmR))
        call report_point(2)

        ! Point 3
        entropy(3) = entropy(2)
        sf = sat_p_sf(pressure(3))
        sfg = sat_p_sfg(pressure(3))
        x = entropy(3)%minus(sf)
        x = x%divide(sfg, unitless)
        write(*,"(A,F8.3)") "Point 3 quality: ", x%get_value()
        hf = sat_p_hf(pressure(3))
        hfg = sat_p_hfg(pressure(3))
        enthalpy(3) = hf%plus(x%times(hfg, btu_lbm))
        call report_point(3)

        ! Point 4
        entropy(4) = entropy(2)
        sf = sat_p_sf(pressure(4))
        sfg = sat_p_sfg(pressure(4))
        x = entropy(4)%minus(sf)
        x = x%divide(sfg, unitless)
        write(*,"(A,F8.3)") "Point 4 quality: ", x%get_value()
        hf = sat_p_hf(pressure(4))
        hfg = sat_p_hfg(pressure(4))
        enthalpy(4) = hf%plus(x%times(hfg, btu_lbm))
        call report_point(4)

        ! Point 5 - saturated liquid
        pressure(5) = pressure(4)
        enthalpy(5) = sat_p_hf(pressure(5))
        entropy(5) = sat_p_sg(pressure(5))
        call report_point(5)

        ! Point 6
        pressure(6) = pressure(2)
        enthalpy(6) = sat_p_hf(pressure(6))
        entropy(6) = sat_p_sf(pressure(6))
        call report_point(6)

        ! Point 7
        pressure(7) = pressure(4) ! Possible issue
        enthalpy(7) = Q(1270D0, btu_lbm)
        entropy(7) = Q(1.5671D0, btu_lbmR)
        call report_point(7)

        ! Point 8
        entropy(8) = entropy(7)
        hf = sat_p_hf(pressure(8))
        hfg = sat_p_hfg(pressure(8))
        sf = sat_p_sf(pressure(8))
        sfg = sat_p_sfg(pressure(8))
        x = entropy(8)%minus(sf)
        x = x%divide(sfg, unitless)
        write(*,"(A,F8.3)") "Point 8 quality: ", x%get_value()
        enthalpy(8) = hf%plus(x%times(hfg, btu_lbm))
        call report_point(8)

        ! Point 14
        entropy(14) = entropy(7)
        hf = sat_p_hf(pressure(14))
        hfg = sat_p_hfg(pressure(14))
        sf = sat_p_sf(pressure(14))
        sfg = sat_p_sfg(pressure(14))
        x = entropy(14)%minus(sf)
        x = x%divide(sfg, unitless)
        write(*,"(A,F8.3)") "Point 14 quality: ", x%get_value()
        enthalpy(14) = hf%plus(x%times(hfg, btu_lbm))
        call report_point(14)

        ! Point 13
        pressure(13) = pressure(3)
        enthalpy(13) = sat_p_hf(pressure(13))
        entropy(13) = sat_p_sf(pressure(13))
        call report_point(13)

        ! Point 9
        pressure(9) = pressure(8)
        enthalpy(9) = sat_p_hf(pressure(9))
        entropy(9) = sat_p_sf(pressure(9))
        call report_point(9)

        ! Point 10
        pressure(10) = pressure(14)
        v = sat_p_vf(pressure(9))
        enthalpy(10) = enthalpy(9)%plus(v%times(pressure(10)%minus(pressure(9)), btu_lbm))
        hf = sat_p_hf(pressure(10))
        hfg = sat_p_hfg(pressure(10))
        x = enthalpy(10)%minus(hf)
        x = x%divide(hfg, unitless) ! This is definitely wrong
        write(*,"(A,F8.3)") "Point 10 quality: ", x%get_value()
        sf = sat_p_sf(pressure(10))
        sfg = sat_p_sfg(pressure(10))
        entropy(10) = sf%plus(x%times(sfg, btu_lbmR))
        call report_point(10)

        ! Point 11
        pressure(11) = pressure(14)
        enthalpy(11) = sat_p_hf(pressure(11))
        entropy(11) = sat_p_sf(pressure(11))
        call report_point(11)

        ! Point 12
        pressure(12) = pressure(2)
        v = sat_p_vf(pressure(11))
        enthalpy(12) = enthalpy(11)%plus(v%times(pressure(12)%minus(pressure(11)), btu_lbm))
        hf = sat_p_hf(pressure(12))
        hfg = sat_p_hfg(pressure(12))
        sf = sat_p_sf(pressure(12))
        sfg = sat_p_sfg(pressure(12))
        x = enthalpy(12)%minus(hf)
        x = x%divide(hfg, unitless)
        write(*,"(A,F8.3)") "Point 12 quality: ", x%get_value()
        entropy(12) = sf%plus(x%times(sfg, btu_lbmR))
        call report_point(12)

        Wfwp = enthalpy(12)%minus(enthalpy(11))
        write(*,"(A,F8.3)") "Feed Water Pump Work: ", Wfwp%get_value()

        ! Point 1
        pressure(1) = pressure(2)
        enthalpy(1) = sat_p_hf(pressure(1))
        entropy(1) = sat_p_sf(pressure(1))
        call report_point(1)

        Qh = enthalpy(2)%minus(enthalpy(1))
        write(*,"(A,F8.3)") "Qh: ", Qh%get_value()

        m3_m1 = enthalpy(1)%minus(enthalpy(12))
        m3_m1 = m3_m1%divide(enthalpy(3)%minus(enthalpy(13)), unitless)
        write(*,"(A,F8.3)") "m3/m1: ", m3_m1%get_value()
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
            h%get_value(), " kJ/kg, s=", s%get_value(), " kJ/kg*K"
        write(*,"(A)") "================================================="
    end subroutine report_point
end module hw2p3
