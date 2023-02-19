module hw3p1
    use class_Quantity
    use class_Problem
    use class_Flow
    use class_Efficiency
    use class_Turbine
    use class_Pump
    use class_HeatExchanger
    use common

    implicit none
    private

    public::hw3_problem_1
contains
    subroutine hw3_problem_1
        type(Problem)::p
        type(Turbine)::hpt,lpt
        type(Pump)::cp,cbp,fp1,fp2
        type(HeatExchanger)::open_heater,he1,he2
        type(Efficiency)::cycle_efficiency
        real::turbine_efficiency,pump_efficiency

        p = create_problem(21, 1, "hw3p1.tex")
        call tex_create_document("NE 400 Homework 3", "Daniel Nevius", "February 20th, 2023")

        turbine_efficiency = 0.95
        pump_efficiency = 0.8

        call tex_label("Cycle Efficiency:")
        cycle_efficiency = create_Efficiency(6, 2, "\eta_{a}")
        call cycle_efficiency%add_input("htp")
        call cycle_efficiency%add_input("ltp")
        call cycle_efficiency%add_input("cp")
        call cycle_efficiency%add_input("cbp")
        call cycle_efficiency%add_input("fp1")
        call cycle_efficiency%add_input("fp2")
        call cycle_efficiency%add_output("SG1")
        call cycle_efficiency%add_output("SG2")
        call cycle_efficiency%print()

        call tex_simple_val("\eta_{t}", turbine_efficiency)
        call tex_simple_val("\eta_{p}", pump_efficiency)

        ! Point 2
        call p%points(2)%given_x(0.95)
        call p%points(2)%given_P(1240.0, psia)
        call p%points(2)%calc_h_s()
        call p%points(2)%calc_s_s()
        call p%points(2)%is_ideal()
        call p%report_point(2)

        ! Point 3
        call p%points(3)%given_P(940.0, psia)
        call p%report_point(3)

        call tex_label("HP Turbine:")
        hpt = create_Turbine(1, 2, "hpt", turbine_efficiency)
        call hpt%add_input(create_Flow(2, (/1,3/), 3))
        call hpt%add_output(create_Flow(1, (/4/), 4))
        call hpt%add_output(create_Flow(3, (/1,3,4/), 5))
        call hpt%print()

        call tex_label("LP Turbine:")
        lpt = create_Turbine(1, 3, "lpt", turbine_efficiency)
        call lpt%add_input(create_Flow(3, (/1,3,4/), 6))
        call lpt%add_output(create_Flow(1, (/5/), 8))
        call lpt%add_output(create_Flow(1, (/6/), 9))
        call lpt%add_output(create_Flow(5, (/1,3,4,5,6/), 10))
        call lpt%print()

        call tex_label("Condensate Pump:")
        cp = create_Pump(1, 1, "cp")
        call cp%add_input(create_Flow(5, (/1,3,4,5,6/), 11))
        call cp%add_output(create_Flow(5, (/1,3,4,5,6/), 12))
        call cp%print()

        call tex_label("Open Heater:")
        open_heater = create_HeatExchanger(3, 1)
        call open_heater%add_input(create_Flow(1, (/6/), 9))
        call open_heater%add_input(create_Flow(5, (/1,3,4,5,6/), 12))
        call open_heater%add_input(create_Flow(3, (/3,4,5/), 15, 1))
        call open_heater%add_output(create_Flow(1, (/1/), 13))
        call open_heater%print()

        call tex_label("CB Pump:")
        cbp = create_Pump(1, 1, "cbp")
        call cbp%add_input(create_Flow(1, (/1/), 13))
        call cbp%add_output(create_Flow(1, (/1/), 14))
        call cbp%print()

        call tex_label("Heat Exchanger 1")
        he1 = create_HeatExchanger(4, 2)
        call he1%add_input(create_Flow(1, (/4/), 17))
        call he1%add_input(create_Flow(1, (/5/), 8))
        call he1%add_input(create_Flow(1, (/3/), 7))
        call he1%add_input(create_Flow(1, (/1/), 14))
        call he1%add_output(create_Flow(1, (/1/), 16))
        call he1%add_output(create_Flow(3, (/3,4,5/), 15, 1))
        call he1%print()

        call tex_label("Heat Exchanger 2")
        he2 = create_HeatExchanger(2, 2)
        call he2%add_input(create_Flow(1, (/1/), 16))
        call he2%add_input(create_Flow(1, (/4/), 4))
        call he2%add_output(create_Flow(1, (/1/), 1))
        call he2%add_output(create_Flow(1, (/4/), 17))
        call he2%print()

        call tex_label("FP 1")
        fp1 = create_Pump(1, 1, "fp1")
        call fp1%add_input(create_Flow(1, (/2/), 1))
        call fp1%add_output(create_Flow(1, (/2/), 21))
        call fp1%print()
        
        call tex_label("FP 2")
        fp2 = create_Pump(1, 1, "fp2")
        call fp2%add_input(create_Flow(1, (/2/), 1))
        call fp2%add_output(create_Flow(1, (/2/), 21))
        call fp2%print()

        call tex_end_document()
    end subroutine hw3_problem_1
end module hw3p1