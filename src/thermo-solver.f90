module thermo_solver
  use class_Quantity
  use SI_SteamTable

  implicit none
  private

  public :: solve
contains
  subroutine solve
    type(Quantity)::pressures(9),turbine_efficiency,pump_efficiency,env_temperature,temperatures(9)

    pressures(1) = Q(6880.0, 1)
    pressures(2) = Q(1280.0, 1)
    pressures(3) = Q(1280.0, 1)
    pressures(4) = Q(1280.0, 1)
    pressures(5) = Q(3.0, 1)
    pressures(6) = Q(3.0, 1)
    pressures(7) = Q(1280.0, 1)
    pressures(8) = Q(1280.0, 1)
    pressures(9) = Q(6880.0, 1)

    temperatures(1) = sat_p_t(pressures(1))
    temperatures(2) = Q(0.0, 0) ! Unknown
    temperatures(3) = sat_p_t(pressures(3))
    temperatures(4) = sat_p_t(pressures(4))
    temperatures(5) = Q(0.0, 0) ! Unknown
    temperatures(6) = sat_p_t(pressures(5))
    temperatures(7) = Q(0.0, 0) ! Unknown
    temperatures(8) = Q(0.0, 0) ! Unknown
    temperatures(9) = Q(0.0, 0) ! Unknown

    turbine_efficiency = Q(0.9, 7)
    pump_efficiency = Q(0.8, 7)
    env_temperature = Q(27.0, 3)

  end subroutine solve
end module thermo_solver
