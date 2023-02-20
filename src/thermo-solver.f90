module thermo_solver
  use ne401project

  implicit none
  private

  public::solve
contains
  subroutine solve
    call project()
  end subroutine solve
end module thermo_solver
