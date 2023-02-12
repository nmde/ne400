module thermo_solver
  use hw2p1
  use hw2p2
  use hw2p3

  implicit none
  private

  public::solve
contains
  subroutine solve
    call problem_1()
  end subroutine solve
end module thermo_solver
