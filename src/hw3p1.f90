module hw3p1
    use class_Quantity
    use class_Problem
    use class_Flow
    use class_Turbine
    use common

    implicit none
    private

    public::hw3_problem_1
contains
    subroutine hw3_problem_1
        type(Problem)::p1
        type(Turbine)::hpt
        integer::yellow(2),green(1),blue(3)
        type(Flow)::yellow_flow,green_flow,blue_flow

        p1 = create_problem(21, 1, "hw3p1.tex")
        call tex_create_document("NE 400 Homework 3", "Daniel Nevius", "February 20th, 2023")

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

        call tex_label("HP Turbine:")
        hpt = create_Turbine(1, 2, "hpt")
        call hpt%add_input(create_Flow(2, (/1,3/), 3))
        call hpt%add_output(create_Flow(1, (/4/), 4))
        call hpt%add_output(create_Flow(3, (/1,3,4/), 5))
        call hpt%print()

        call tex_end_document()
    end subroutine hw3_problem_1
end module hw3p1
