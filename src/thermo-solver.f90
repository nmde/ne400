module thermo_solver
  use hw3p1, only: hw3_problem_1

  implicit none
  private

  public::solve
contains
  subroutine solve
    call hw3_problem_1()
  end subroutine solve
end module thermo_solver
