module thermo_solver
  use hw2p1, only: hw2_problem_1
  use hw2p2, only: hw2_problem_2
  use hw2p3, only: hw2_problem_3
  use hw2p4, only: hw2_problem_4
  use hw3p1, only: hw3_problem_1
  use lab2, only: lab_2
  use ne401project, only: project

  implicit none
  private

  public::solve
contains
  subroutine solve
    call project()
  end subroutine solve
end module thermo_solver
