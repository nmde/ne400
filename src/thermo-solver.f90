module thermo_solver
  use Conversions
  use SI_SteamTable

  implicit none
  private

  public :: solve
contains
  subroutine solve
    real::pressures(9),turbine_efficiency,pump_efficiency,env_temperature,temperatures(9)

    pressures(1) = 6880
    pressures(2) = 1280
    pressures(3) = 1280
    pressures(4) = 1280
    pressures(5) = 3
    pressures(6) = 3
    pressures(7) = 1280
    pressures(8) = 1280
    pressures(9) = 6880

    temperatures(1) = sat_p_t(pressures(1))
    temperatures(2) = -1 ! Unknown
    temperatures(3) = sat_p_t(pressures(3))
    temperatures(4) = sat_p_t(pressures(4))
    temperatures(5) = -1 ! Unknown
    temperatures(6) = sat_p_t(pressures(5))
    temperatures(7) = -1 ! Unknown
    temperatures(8) = -1 ! Unknown
    temperatures(9) = -1 ! Unknown

    turbine_efficiency = 0.9
    pump_efficiency = 0.8
    env_temperature = C_to_K(27)


  end subroutine solve
end module thermo_solver
