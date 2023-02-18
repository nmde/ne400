module hw3p1
    use class_Quantity
    use class_Problem
    use common

    implicit none
    private

    public::hw3_problem_1
contains
    subroutine hw3_problem_1
        type(Problem)::p1

        p1 = create_problem(21, 1, "hw3p1.tex", "NE 400 Homework 3", "Daniel Nevius", "February 20th, 2023")

        ! Point 2
        call p1%points(2)%given_x(0.95)
        call p1%points(2)%given_P(1240.0, psia)
        call p1%points(2)%calc_h()
        call p1%points(2)%calc_s()
        call p1%report_point(2)

        ! Point 3
        call p1%eq_P(3,2)
        call p1%eq_h(3,2)
        call p1%eq_s(3,2)
        call p1%report_point(3)

        call tex_finish()
    end subroutine hw3_problem_1
end module hw3p1
