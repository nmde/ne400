module thermo_solver
  use class_Quantity
  use SI_SteamTable

  implicit none
  private

  public :: solve
contains
  subroutine solve
    type(Quantity)::pressure(9),turbine_efficiency,pump_efficiency,env_temperature,temperature(9)

    call initialize_steam_tables()

    pressure(1) = Q(6880.0, kPa)
    pressure(2) = Q(1280.0, kPa)
    pressure(3) = Q(1280.0, kPa)
    pressure(4) = Q(1280.0, kPa)
    pressure(5) = Q(3.0, 1)
    pressure(6) = Q(3.0, 1)
    pressure(7) = Q(1280.0, kPa)
    pressure(8) = Q(1280.0, kPa)
    pressure(9) = Q(6880.0, kPa)

    temperature(1) = sat_p_t(pressure(1))
    temperature(2) = Q(0.0, 0) ! Unknown
    temperature(3) = sat_p_t(pressure(3))
    temperature(4) = sat_p_t(pressure(4))
    temperature(5) = Q(0.0, 0) ! Unknown
    temperature(6) = sat_p_t(pressure(5))
    temperature(7) = Q(0.0, 0) ! Unknown
    temperature(8) = Q(0.0, 0) ! Unknown
    temperature(9) = Q(0.0, 0) ! Unknown

    turbine_efficiency = Q(0.9, unitless)
    pump_efficiency = Q(0.8, unitless)
    env_temperature = Q(27.0, C)

  end subroutine solve
end module thermo_solver
