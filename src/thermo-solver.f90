module thermo_solver
  use class_Quantity
  use SI_SteamTable

  implicit none
  private

  type(Quantity)::pressure(9),temperature(9),enthalpy_s(9),enthalpy_a(9),entropy(9)

  public::solve
contains
  subroutine solve
    call initialize_steam_tables()
    call problem_1_a()
  end subroutine solve

  subroutine problem_1_a
    type(Quantity)::turbine_efficiency,pump_efficiency,env_temperature,hf,v,x,&
      Wshpt,Wahpt,Wslpt,Walpt,Wsmcp,Wamcp

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

    ! Point 1 - saturated vapor
    temperature(1) = sat_p_t(pressure(1))
    enthalpy_s(1) = sat_p_hg(pressure(1))
    enthalpy_a(1) = enthalpy_s(1)
    entropy(1) = sat_p_sg(pressure(1))
    call report_point(1)

    ! Point 2
    entropy(2) = entropy(1)
    x = entropy(2)%minus(sat_p_sf(pressure(2)))
    x = x%divide(sat_p_sfg(pressure(2)), unitless)
    hf = sat_p_hf(pressure(2))
    enthalpy_s(2) = hf%plus(x%times(sat_p_hfg(pressure(2)), kJ_kg))

    Wshpt = enthalpy_s(1)%minus(enthalpy_s(2))
    write(*,"(A,F8.3)") "Ideal HPT work: ", Wshpt%get_value()

    enthalpy_a(2) = enthalpy_s(1)%minus(Wshpt%times(turbine_efficiency, kJ_kg))
    call report_point(2)

    Wahpt = enthalpy_s(1)%minus(enthalpy_a(2))
    write(*,"(A,F8.3)") "Actual HPT work: ", Wahpt%get_value()

    ! Point 3 - saturated vapor
    enthalpy_s(3) = sat_p_hg(pressure(3))
    enthalpy_a(3) = enthalpy_s(3)
    entropy(3) = sat_p_sg(pressure(3))
    call report_point(3)

    ! Point 5
    entropy(5) = entropy(3)
    x = entropy(5)%minus(sat_p_sf(pressure(5)))
    x = x%divide(sat_p_sfg(pressure(5)), unitless)
    hf = sat_p_hf(pressure(5))
    enthalpy_s(5) = hf%plus(x%times(sat_p_hfg(pressure(5)), kJ_kg))

    Wslpt = enthalpy_s(3)%minus(enthalpy_s(5))
    write(*,"(A,F8.3)") "Ideal LPT work: ", Wslpt%get_value()

    enthalpy_a(5) = enthalpy_s(3)%minus(Wslpt%times(turbine_efficiency, kJ_kg))
    call report_point(5)

    Walpt = enthalpy_s(3)%minus(enthalpy_a(5))
    write(*,"(A,F8.3)") "Actual LPT work: ", Walpt%get_value()

    ! Point 6 - saturated liquid
    enthalpy_s(6) = sat_p_hf(pressure(6))
    enthalpy_a(6) = enthalpy_s(6)
    entropy(6) = sat_p_sf(pressure(6))
    v = sat_p_vf(pressure(6))
    call report_point(6)

    Wsmcp = v%times(pressure(7)%minus(pressure(6)), kJ_kg)
    write(*,"(A,F8.3)") "Ideal MCP work: ", Wsmcp%get_value()

    enthalpy_a(7) = enthalpy_s(6)%minus(pump_efficiency%times(Wsmcp, kJ_kg))

    call report_point(7)

    Wamcp = enthalpy_a(7)%minus(enthalpy_s(6))
    write(*,"(A,F8.3)") "Actual MCP work: ", Wamcp%get_value() ! POTENTIAL PROBLEM

    ! Point 4 - saturated liquid
    enthalpy_s(4) = sat_p_hf(pressure(4))
    enthalpy_a(4) = enthalpy_s(4)
    entropy(4) = sat_p_sf(pressure(4))
    call report_point(4)

  end subroutine problem_1_a

  subroutine report_point(point)
    integer,intent(in)::point
    type(Quantity)::p,t,h_s,h_a,s

    p = pressure(point)
    t = temperature(point)
    h_s = enthalpy_s(point)
    h_a = enthalpy_a(point)
    s = entropy(point)

    write(*,"(A,I3,A)",advance='no') "Point ", point, ": P="
    ! TODO - report actual units for all quantities
    write(*,"(F8.3,A,F8.3,A,F8.3,A,F8.3,A)") p%get_value(), " kPa, hs=", h_s%get_value(), &
      " kJ/kg, ha=", h_a%get_value(), " kJ/kg, s=", s%get_value(), " kJ/kg*K"
    write(*,"(A)") "================================================="
  end subroutine report_point
end module thermo_solver
