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
        type(Pump)::cp,cbp,fp1,fp2,rcp1,rcp2
        type(HeatExchanger)::open_heater,he1,he2,rh
        type(Efficiency)::cycle_efficiency
        type(MassFlow)::m1,m2,m3,m4,m5,m6,mp,mc
        real::turbine_efficiency,pump_efficiency,Whpt

        p = create_problem(21, 1, "hw3p1.tex")
        call tex_create_document("NE 400 Homework 3", "Daniel Nevius", "February 20th, 2023")

        turbine_efficiency = 0.95
        pump_efficiency = 0.8

        m1 = create_MassFlow(1)
        m2 = create_MassFlow(2)
        m3 = create_MassFlow(3)
        m4 = create_MassFlow(4)
        m5 = create_MassFlow(5)
        m6 = create_MassFlow(6)
        mp = create_MassFlow(7)
        mc = create_MassFlow(8)

        call tex_label("Cycle Efficiency:")
        cycle_efficiency = create_Efficiency(8, 2, "\eta_{a}")
        call cycle_efficiency%add_input("htp")
        call cycle_efficiency%add_input("ltp")
        call cycle_efficiency%add_input("cp")
        call cycle_efficiency%add_input("cbp")
        call cycle_efficiency%add_input("fp1")
        call cycle_efficiency%add_input("fp2")
        call cycle_efficiency%add_input("rcp1")
        call cycle_efficiency%add_input("rcp2")
        call cycle_efficiency%add_output("SG1")
        call cycle_efficiency%add_output("SG2")
        call cycle_efficiency%print()

        call tex_simple_val("\eta_{t}", turbine_efficiency)
        call tex_simple_val("\eta_{p}", pump_efficiency)

        call tex_label("HP Turbine:")
        hpt = create_Turbine(1, 2, "hpt", turbine_efficiency)
        call hpt%add_input(create_Flow(2, (/m1,m3/), p%point(3)))
        call hpt%add_output(create_Flow(1, (/m4/), p%point(4)))
        call hpt%add_output(create_Flow(3, (/m1,m3,m4/), p%point(5)))
        call hpt%print()

        ! Point 2
        call p%point(2)%given_x_s(0.95)
        call p%point(2)%given_P(1240.0, psia)
        call p%point(2)%calc_h_s()
        call p%point(2)%calc_s_s()
        call p%point(2)%set_ideal()

        ! Point 3
        call p%point(3)%given_P(940.0, psia)
        call p%eq_h_s(3, 2)
        call p%point(3)%calc_x_s_from_h_s()
        call p%point(3)%calc_s_s()
        call p%point(3)%set_ideal()

        ! Point 4
        call p%point(4)%given_P(385.0, psia)
        call p%eq_s_s(4, 3)
        call p%point(4)%calc_x_s_from_s_s()
        call p%point(4)%calc_h_s()

        ! Point 5
        call p%point(5)%given_P(160.0, psia)
        call p%eq_s_s(5, 3)
        call p%point(5)%calc_x_s_from_s_s()
        call p%point(5)%calc_h_s()

        ! Point 1
        call p%point(1)%given_T(480.0, F)
        call p%point(1)%given_P(580.0, psia)

        ! Point 6
        call p%point(6)%given_T(560.0, F)

        ! Point 7

        ! Point 8
        call p%point(8)%given_P(110.0, psia)
        call p%eq_s_s(8, 6)

        ! Point 9
        call p%point(9)%given_P(25.0, psia)
        call p%eq_s_s(8, 6)

        ! Point 10
        call p%point(10)%given_P(2.0, psia)
        call p%eq_s_s(10, 6)

        ! Point 11
        call p%eq_P(11, 10)
        call p%point(11)%calc_h_s()
        call p%point(11)%calc_s_s()

        ! Point 12

        ! Point 13

        ! Point 14

        ! Point 15

        ! Point 16
        call p%point(16)%given_T(330.0, F)

        ! Point 17

        ! Point 18
        call p%point(18)%given_T(645.0, F)
        call p%point(18)%given_P(2350.0, psia)

        ! Point 19
        call p%point(19)%given_T(575.0, F)
        call p%point(19)%given_P(2250.0, psia)

        ! Point 20
        call p%point(20)%given_P(2350.0, psia)

        ! Point 21


        call tex_label("Reheater:")
        rh = create_HeatExchanger(2, 2)
        call rh%add_input(create_Flow(1, (/m3/), p%point(2)))
        call rh%add_output(create_Flow(1, (/m3/), p%point(7)))
        call rh%add_input(create_Flow(3, (/m1,m3,m4/), p%point(5)))
        call rh%add_output(create_Flow(3, (/m1,m3,m4/), p%point(6)))
        call rh%print()

        call tex_label("LP Turbine:")
        lpt = create_Turbine(1, 3, "lpt", turbine_efficiency)
        call lpt%add_input(create_Flow(3, (/m1,m3,m4/), p%point(6)))
        call lpt%add_output(create_Flow(1, (/m5/), p%point(8)))
        call lpt%add_output(create_Flow(1, (/m6/), p%point(9)))
        call lpt%add_output(create_Flow(5, (/m1,m3,m4,m5,m6/), p%point(10)))
        call lpt%print()

        call tex_label("Condensate Pump:")
        cp = create_Pump(1, 1, "cp", pump_efficiency)
        call cp%add_input(create_Flow(5, (/m1,m3,m4,m5,m6/), p%point(11)))
        call cp%add_output(create_Flow(5, (/m1,m3,m4,m5,m6/), p%point(12)))
        call cp%print()

        call tex_label("Open Heater:")
        open_heater = create_HeatExchanger(3, 1)
        call open_heater%add_input(create_Flow(1, (/m6/), p%point(9)))
        call open_heater%add_input(create_Flow(5, (/m1,m3,m4,m5,m6/), p%point(12)))
        call open_heater%add_input(create_Flow(3, (/m3,m4,m5/), p%point(15), 1))
        call open_heater%add_output(create_Flow(1, (/m1/), p%point(13)))
        call open_heater%print()

        call tex_label("CB Pump:")
        cbp = create_Pump(1, 1, "cbp", pump_efficiency)
        call cbp%add_input(create_Flow(1, (/m1/), p%point(13)))
        call cbp%add_output(create_Flow(1, (/m1/), p%point(14)))
        call cbp%print()

        call tex_label("Heat Exchanger 1")
        he1 = create_HeatExchanger(4, 2)
        call he1%add_input(create_Flow(1, (/m1/), p%point(14)))
        call he1%add_input(create_Flow(1, (/m3/), p%point(7)))
        call he1%add_input(create_Flow(1, (/m4/), p%point(17)))
        call he1%add_input(create_Flow(1, (/m5/), p%point(8)))
        call he1%add_output(create_Flow(1, (/m1/), p%point(16)))
        call he1%add_output(create_Flow(3, (/m3,m4,m5/), p%point(15), 1))
        call he1%print()

        call tex_label("Heat Exchanger 2")
        he2 = create_HeatExchanger(2, 2)
        call he2%add_input(create_Flow(1, (/m1/), p%point(16)))
        call he2%add_input(create_Flow(1, (/m4/), p%point(4)))
        call he2%add_output(create_Flow(1, (/m1/), p%point(1)))
        call he2%add_output(create_Flow(1, (/m4/), p%point(17)))
        call he2%print()

        call tex_label("FP 1")
        fp1 = create_Pump(1, 1, "fp1", pump_efficiency)
        call fp1%add_input(create_Flow(1, (/m2/), p%point(1)))
        call fp1%add_output(create_Flow(1, (/m2/), p%point(21)))
        call fp1%print()
        
        call tex_label("FP 2")
        fp2 = create_Pump(1, 1, "fp2", pump_efficiency)
        call fp2%add_input(create_Flow(1, (/m2/), p%point(1)))
        call fp2%add_output(create_Flow(1, (/m2/), p%point(21)))
        call fp2%print()

        call tex_label("RCP 1")
        rcp1 = create_Pump(1, 1, "rcp1", pump_efficiency)
        call rcp1%add_input(create_Flow(1, (/mp/), p%point(20)))
        call rcp1%add_output(create_Flow(1, (/mp/), p%point(19)))
        call rcp1%print()

        call tex_label("RCP 2")
        rcp2 = create_Pump(1, 1, "rcp2", pump_efficiency)
        call rcp2%add_input(create_Flow(1, (/mp/), p%point(20)))
        call rcp2%add_output(create_Flow(1, (/mp/), p%point(19)))
        call rcp2%print()

        call tex_end_document()
        call p%report_all_solved()
    end subroutine hw3_problem_1
end module hw3p1
