module thermo_solver
  use lab2

  implicit none
  private

  public::solve
contains
  subroutine solve
    call lab_2()
  end subroutine solve
end module thermo_solver
