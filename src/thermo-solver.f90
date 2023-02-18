module thermo_solver
  use hw2p1
  use hw2p2
  use hw2p3
  use hw2p4
  use lab2

  implicit none
  private

  public::solve
contains
  subroutine solve
    call lab_2()
  end subroutine solve
end module thermo_solver
