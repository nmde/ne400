module hw3p1
    use class_Quantity
    use class_Problem
    use class_Flow
    use class_Efficiency
    use class_Turbine
    use class_Pump
    use class_HeatExchanger
    use class_Boiler
    use SteamTable
    use common

    implicit none
    private

    public::hw3_problem_1
contains
    subroutine hw3_problem_1
        type(Problem)::p
        type(Turbine)::hpt,lpt
        type(Pump)::cp,cbp,fp1,rcp1
        type(HeatExchanger)::open_heater,he1,he2,rh,sg1
        type(Boiler)::Rx
        type(Efficiency)::cycle_efficiency
        type(MassFlow)::m1,m2,m3,m4,m5,m6,mp,mc
        type(Quantity)::vf,Wcps,temp,Wcbps,m4_m1,m3_m1,m5_m1,m6_m1,Whpts,Wlpts,Wrcps,Wfps,m8_m1,Qh, &
            eta_s,eta_a,Wfps_total,Wrcps_total
        real::turbine_efficiency,pump_efficiency

        p = create_problem(21, 1, "hw3p1.tex")
        call tex_create_document("NE 400 Homework 3", "Daniel Nevius", "February 20th, 2023")

        call tex_begin("interpolation")
        write(13,"(A)") "y = y_1 + (x - x_1) \frac{y_2 - y_1}{x_2 - x_1}"
        call tex_end()

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
        cycle_efficiency = create_Efficiency(8, 1, "\eta_{a}")
        call cycle_efficiency%add_input("htp")
        call cycle_efficiency%add_input("ltp")
        call cycle_efficiency%add_input("cp")
        call cycle_efficiency%add_input("cbp")
        call cycle_efficiency%add_input("fp1")
        call cycle_efficiency%add_input("fp2")
        call cycle_efficiency%add_input("rcp1")
        call cycle_efficiency%add_input("rcp2")
        call cycle_efficiency%add_output("Rx")
        call cycle_efficiency%print()

        call tex_simple_val("\eta_{t}", turbine_efficiency)
        call tex_simple_val("\eta_{p}", pump_efficiency)

        ! Point 2
        call p%point(2)%given_x_s(0.95)
        call p%point(2)%given_P(1240.0, psia)
        call p%point(2)%calc_h_s()
        call p%point(2)%calc_s_s()
        call p%point(2)%set_ideal()
        call p%report_point(2)

        ! Point 3
        call p%point(3)%given_P(940.0, psia)
        call p%eq_h_s(3, 2)
        call p%point(3)%calc_x_s_from_h_s()
        call p%point(3)%calc_s_s()
        call p%point(3)%set_ideal()
        call p%report_point(3)

        ! Point 4
        call p%point(4)%given_P(385.0, psia)
        call p%eq_s_s(4, 3)
        call p%point(4)%calc_x_s_from_s_s()
        call p%point(4)%calc_h_s()
        call p%report_point(4)

        ! Point 5
        call p%point(5)%given_P(160.0, psia)
        call p%eq_s_s(5, 3)
        call p%point(5)%calc_x_s_from_s_s()
        call p%point(5)%calc_h_s()
        call p%report_point(5)

        ! Point 6
        call p%point(6)%given_T(560.0, F)
        call p%eq_P(6, 5)

        write(13,"(A)") "Interpolating from superheated steam tables using equation (\ref{interpolation}):"
        call p%point(6)%set_h_s(1304.76, btu_lbm)
        call p%point(6)%set_s_s(1.6840, btu_lbmR)
        call p%report_point(6)

        ! Point 7
        call p%eq_P(7,2)
        ! TODO - make this output prettier
        call p%point(7)%set_x_s(0.0)
        call p%point(7)%calc_h_s()
        call p%point(7)%calc_s_s()
        call p%report_point(7)

        ! Point 8
        call p%point(8)%given_P(110.0, psia)
        call p%eq_s_s(8, 6)
        call p%point(8)%calc_x_s_from_s_s()
        call p%point(8)%calc_h_s()
        call p%report_point(8)

        ! Point 9
        call p%point(9)%given_P(25.0, psia)
        call p%eq_s_s(9, 6)
        call p%point(9)%calc_x_s_from_s_s()
        call p%point(9)%calc_h_s()
        call p%report_point(9)

        ! Point 10
        call p%point(10)%given_P(2.0, psia)
        call p%eq_s_s(10, 6)
        call p%point(10)%calc_x_s_from_s_s()
        call p%point(10)%calc_h_s()
        call p%report_point(10)

        ! Point 11
        call p%eq_P(11, 10)
        call p%point(11)%calc_h_s()
        call p%point(11)%calc_s_s()
        call p%report_point(11)

        ! Point 12
        call p%eq_P(12, 9)

        call tex_label("Condensate Pump:")
        cp = create_Pump(1, 1, "cp", pump_efficiency)
        call cp%add_input(create_Flow(5, (/m1,m3,m4,m5,m6/), p%point(11)))
        call cp%add_output(create_Flow(5, (/m1,m3,m4,m5,m6/), p%point(12)))
        call cp%print()

        vf = sat_p_vf(p%point(11)%pressure)
        Wcps = vf%times(p%point(12)%pressure%minus(p%point(11)%pressure), btu_lbm)
        call tex_begin()
        write(13,"(A,F8.3,A,F8.3,A,F8.3,A,F8.3,A)") "-\frac{\dot{W}_{cp,s}}{1 - \frac{\dot{m}_{ 3}}{\dot{m}_{ 1}} - " &
            // "\frac{\dot{m}_{ 4}}{\dot{m}_{ 1}} - \frac{\dot{m}_{ 5}}{\dot{m}_{ 1}} - \frac{\dot{m}_{ 6}}{\dot{m}_{ 1}}} = " &
            // "h_{12,s} - h_{11,s} = v_{@P11}(P_{12} - P_{11}) = ", &
            vf%get_value(), "(", p%point(12)%pressure%get_value(), " - ", p%point(11)%pressure%get_value(), &
            ") = ", Wcps%get_value(), tex_units(Wcps)
        call tex_end()
        write(*,"(A,F8.3)") "Wcp = ", Wcps%get_value()

        temp = Wcps%plus(p%point(11)%enthalpy_s)
        call p%point(12)%set_h_s(real(temp%get_value(), 4), btu_lbm)
        call tex_begin()
        write(13,"(A,F8.3,A,F8.3,A,F8.3,A,F8.3,A)") "h_{12,s} = -\frac{\dot{W}_{cp,s}}{1 - \frac{\dot{m}_{ 3}}{\dot{m}_{ 1}} - " &
            // "\frac{\dot{m}_{ 4}}{\dot{m}_{ 1}} - \frac{\dot{m}_{ 5}}{\dot{m}_{ 1}} - \frac{\dot{m}_{ 6}}{\dot{m}_{ 1}}} + " &
            // "h_{11,s} = ", Wcps%get_value(), " + ", p%point(11)%enthalpy_s%get_value(), " = ", &
            p%point(12)%enthalpy_s%get_value(), tex_units(p%point(11)%enthalpy_s)
        call tex_end()

        call p%point(12)%calc_x_s_from_h_s()
        call p%point(12)%calc_s_s()

        call p%report_point(12)

        call tex_label("CB Pump:")
        cbp = create_Pump(1, 1, "cbp", pump_efficiency)
        call cbp%add_input(create_Flow(1, (/m1/), p%point(13)))
        call cbp%add_output(create_Flow(1, (/m1/), p%point(14)))
        call cbp%print()

        ! Point 13
        call p%eq_P(13, 12)
        write(13,"(A)") "Sat. liquid at P13:"
        call p%point(13)%set_h_s(208.52, btu_lbm)
        call p%point(13)%set_s_s(0.35347, btu_lbmR)
        call p%report_point(13)

        ! Point 14
        call p%eq_P(14, 8)

        vf = sat_p_vf(p%point(13)%pressure)
        Wcbps = vf%times(p%point(14)%pressure%minus(p%point(13)%pressure), btu_lbm)
        call tex_begin()
        write(13,"(A,F8.3,A,F8.3,A,F8.3,A,F8.3,A)") "-\frac{\dot{W}_{cbp,s}}{\dot{m}_{ 1}} = " &
            // "h_{14,s} - h_{13,s} = v_{@P31}(P_{14} - P_{13}) = ", &
            vf%get_value(), "(", p%point(14)%pressure%get_value(), " - ", p%point(13)%pressure%get_value(), &
            ") = ", Wcbps%get_value(), tex_units(Wcbps)
        call tex_end()
        write(*,"(A,F8.3)") "Wcbp = ", Wcbps%get_value()

        temp = Wcbps%plus(p%point(13)%enthalpy_s)
        call p%point(14)%set_h_s(real(temp%get_value(), 4), btu_lbm)
        call tex_begin()
        write(13,"(A,F8.3,A,F8.3,A,F8.3,A,F8.3,A)") "h_{14,s} = -\frac{\dot{W}_{cbp,s}}{\dot{m}_{1}} + " &
            // "h_{13,s} = ", Wcbps%get_value(), " + ", p%point(13)%enthalpy_s%get_value(), " = ", &
            p%point(14)%enthalpy_s%get_value(), tex_units(p%point(14)%enthalpy_s)
        call tex_end()

        call p%point(14)%calc_x_s_from_h_s()
        call p%point(14)%calc_s_s()
        call p%report_point(14)

        ! Point 15
        call p%eq_P(15, 8)
        write(13,"(A)") "Sat. liquid at P15:"
        call p%point(15)%set_h_s(305.78, btu_lbm)
        call p%point(15)%set_s_s(0.48341, btu_lbmR)
        call p%report_point(15)

        ! Point 16
        call p%point(16)%given_T(330.0, F)
        call p%eq_P(16, 8)
        write(13,"(A)") "Values for superheated steam directly from the steam charts:"
        call p%point(16)%set_h_s(1197.7, btu_lbm)
        call p%point(16)%set_s_s(1.6061, btu_lbmR)
        call p%report_point(16)

        call tex_label("Heat Exchanger 2")
        he2 = create_HeatExchanger(2, 2)
        call he2%add_input(create_Flow(1, (/m1/), p%point(16)))
        call he2%add_input(create_Flow(1, (/m4/), p%point(4)))
        call he2%add_output(create_Flow(1, (/m1/), p%point(1)))
        call he2%add_output(create_Flow(1, (/m4/), p%point(17)))
        call he2%print()

        call tex_begin()
        write(13,"(A)") "\frac{\dot{m}_{4}}{\dot{m}_{1}} = \frac{h_{1,s} - h_{16,s}}{h_{4,s} - h_{17,s}}"
        call tex_end()

        ! Point 17
        call p%eq_P(17, 4)
        call p%point(17)%set_x_s(0.0)
        call p%point(17)%calc_h_s()
        call p%point(17)%calc_s_s()
        call p%report_point(17)

        ! Point 1
        call p%point(1)%given_T(480.0, F)
        call p%point(1)%given_P(580.0, psia)
        call tex_label("Using values for 500\(^{\circ}\) F from the superheated steam table:")
        call p%point(1)%set_h_s(1219.1, btu_lbm)
        call p%point(1)%set_s_s(1.4654, btu_lbmR)
        call p%report_point(1)

        ! TODO - somehow solve this within the HeatExchanger class
        m4_m1 = p%point(1)%enthalpy_s%minus(p%point(16)%enthalpy_s)
        m4_m1 = m4_m1%divide(p%point(4)%enthalpy_s%minus(p%point(17)%enthalpy_s), unitless)
        call tex_begin()
        write(13,"(A,F8.3,A,F8.3,A,F8.3,A,F8.3,A,F8.3)") "\frac{\dot{m}_{4}}{\dot{m}_{1}} = \frac{", &
            p%point(1)%enthalpy_s%get_value(), " - ", p%point(16)%enthalpy_s%get_value(), "}{", &
            p%point(4)%enthalpy_s%get_value(), " - ", p%point(17)%enthalpy_s%get_value(), "} = ", &
            m4_m1%get_value()
        call tex_end()
        write(*,"(A,F8.3,A)") "m4/m1 = ", m4_m1%get_value(), tex_units(m4_m1)

        call tex_label("Reheater:")
        rh = create_HeatExchanger(2, 2)
        call rh%add_input(create_Flow(1, (/m3/), p%point(2)))
        call rh%add_output(create_Flow(1, (/m3/), p%point(7)))
        call rh%add_input(create_Flow(3, (/m1,m3,m4/), p%point(5)))
        call rh%add_output(create_Flow(3, (/m1,m3,m4/), p%point(6)))
        call rh%print()

        call tex_begin()
        write(13,"(A)") "\frac{\dot{m}_{3}}{\dot{m}_{1}} = -\frac{(\frac{\dot{m}_{4}}{\dot{m}_{ 1}} - 1)(h_{6,s} - " &
         // "h_{5,s})}{h_{6,s} + h_{2,s} - h_{5,s} - h_{7,s}}"
        call tex_end()

        m3_m1 = m4_m1%minus(Q(1D0,unitless))
        m3_m1 = m3_m1%times(p%point(6)%enthalpy_s%minus(p%point(5)%enthalpy_s), unitless)
        m3_m1 = m3_m1%divide(p%point(6)%enthalpy_s%plus(p%point(2)%enthalpy_s%minus(p%point(5)%enthalpy_s%minus &
            (p%point(7)%enthalpy_s))), unitless)
        call tex_begin()
        write(13,"(A,F8.3,A,F8.3,A,F8.3,A,F8.3,A,F8.3,A,F8.3,A,F8.3,A,F8.3)") "\frac{\dot{m}_{3}}{\dot{m}_{1}} = -\frac{(", &
            m4_m1%get_value(), " - 1)(", p%point(6)%enthalpy_s%get_value(), " - ", p%point(5)%enthalpy_s%get_value(), &
            ")}{", p%point(6)%enthalpy_s%get_value(), " + ", p%point(2)%enthalpy_s%get_value(), " - ", &
            p%point(5)%enthalpy_s%get_value(), " - ", p%point(7)%enthalpy_s%get_value(), "} = ", m3_m1%get_value()
        call tex_end()
        write(*,"(A,F8.3,A)") "m3/m1 = ", m3_m1%get_value(), tex_units(m3_m1)

        call tex_label("HP Turbine:")
        hpt = create_Turbine(1, 2, "hpt", turbine_efficiency)
        call hpt%add_input(create_Flow(2, (/m1,m3/), p%point(3)))
        call hpt%add_output(create_Flow(1, (/m4/), p%point(4)))
        call hpt%add_output(create_Flow(3, (/m1,m3,m4/), p%point(5)))
        call hpt%print()

        temp = Q(1D0, unitless)
        Whpts = p%point(3)%enthalpy_s%times(temp%minus(m3_m1), btu_lbm)
        Whpts = Whpts%minus(m4_m1%times(p%point(4)%enthalpy_s, btu_lbm))
        Whpts = Whpts%minus(p%point(5)%enthalpy_s%times(temp%minus(m3_m1%minus(m4_m1)), btu_lbm))
        call tex_label("Plugging into equation (\ref{hpt_s}):")
        call tex_begin()
        write(13,"(A,F8.3)") "\frac{\dot{W}_{hpt,s}}{m_1} = ", Whpts%get_value()
        call tex_end()
        write(*,"(A,F8.3,A)") "Whpts = ", Whpts%get_value(), tex_units(Whpts)

        call tex_label("Heat Exchanger 1")
        he1 = create_HeatExchanger(4, 2)
        call he1%add_input(create_Flow(1, (/m1/), p%point(14)))
        call he1%add_input(create_Flow(1, (/m3/), p%point(7)))
        call he1%add_input(create_Flow(1, (/m4/), p%point(17)))
        call he1%add_input(create_Flow(1, (/m5/), p%point(8)))
        call he1%add_output(create_Flow(1, (/m1/), p%point(16)))
        call he1%add_output(create_Flow(3, (/m3,m4,m5/), p%point(15), 1))
        call he1%print()

        call tex_begin()
        write(13,"(A)") "\frac{\dot{m}_{5}}{\dot{m}_{1}} = \frac{\frac{\dot{m}_{3}}{\dot{m}_{1}}(h_{15,s} - h_{7,s}) + " &
            // "\frac{\dot{m}_{4}}{\dot{m}_{1}}h_{15,s} - \frac{\dot{m}_{4}}{\dot{m}_{1}}h_{17,s} - h_{14,s} + " &
            // "h_{16,s}}{h_{8,s} - h_{15,s}}"
        call tex_end()

        write(*,"(A)") "m5_m1:"
        m5_m1 = m3_m1%times(p%point(15)%enthalpy_s%minus(p%point(7)%enthalpy_s), btu_lbm)
        m5_m1 = m5_m1%plus(m4_m1%times(p%point(15)%enthalpy_s, btu_lbm))
        m5_m1 = m5_m1%minus(m4_m1%times(p%point(17)%enthalpy_s, btu_lbm))
        m5_m1 = m5_m1%minus(p%point(14)%enthalpy_s)
        m5_m1 = m5_m1%plus(p%point(16)%enthalpy_s)
        m5_m1 = m5_m1%divide(p%point(8)%enthalpy_s%minus(p%point(15)%enthalpy_s), unitless)
        call tex_begin()
        write(13,"(A,F8.3)") "\frac{\dot{m}_{5}}{\dot{m}_{1}} = ", m5_m1%get_value()
        call tex_end()
        write(*,"(A,F8.3,A)") "m5/m1 = ", m5_m1%get_value(), tex_units(m5_m1)

        call tex_label("Open Heater:")
        open_heater = create_HeatExchanger(3, 1)
        call open_heater%add_input(create_Flow(1, (/m6/), p%point(9)))
        call open_heater%add_input(create_Flow(5, (/m1,m3,m4,m5,m6/), p%point(12)))
        call open_heater%add_input(create_Flow(3, (/m3,m4,m5/), p%point(15), 1))
        call open_heater%add_output(create_Flow(1, (/m1/), p%point(13)))
        call open_heater%print()

        call tex_begin()
        write(13,"(A)") "\frac{\dot{m}_{ 6}}{\dot{m}_{ 1}} = \frac{h_{12,s}(\frac{\dot{m}_{3}}{\dot{m}_{1}} + " &
            // "\frac{\dot{m}_{4}}{\dot{m}_{1}} + \frac{\dot{m}_{5}}{\dot{m}_{1}} - 1) - " &
            // "h_{15,s}(\frac{\dot{m}_{3}}{\dot{m}_{1}} + " &
            // "\frac{\dot{m}_{4}}{\dot{m}_{1}} + \frac{\dot{m}_{5}}{\dot{m}_{1}}) + h_{13,s}}{h_{9,s} - h_{12,s}}"
        call tex_end()

        m6_m1 = p%point(12)%enthalpy_s%times(m3_m1%plus(m4_m1%plus(m5_m1%minus(Q(1D0,unitless)))), btu_lbm)
        m6_m1 = m6_m1%minus(p%point(15)%enthalpy_s%times(m3_m1%plus(m4_m1%plus(m5_m1)), btu_lbm))
        m6_m1 = m6_m1%plus(p%point(13)%enthalpy_s)
        m6_m1 = m6_m1%divide(p%point(9)%enthalpy_s%minus(p%point(12)%enthalpy_s), unitless)
        call tex_begin()
        write(13,"(A,F8.3)") "\frac{\dot{m}_{ 6}}{\dot{m}_{ 1}} = ", m6_m1%get_value()
        call tex_end()
        write(*,"(A,F8.3,A)") "m6/m1 = ", m6_m1%get_value(), tex_units(m6_m1)

        call tex_label("LP Turbine:")
        lpt = create_Turbine(1, 3, "lpt", turbine_efficiency)
        call lpt%add_input(create_Flow(3, (/m1,m3,m4/), p%point(6)))
        call lpt%add_output(create_Flow(1, (/m5/), p%point(8)))
        call lpt%add_output(create_Flow(1, (/m6/), p%point(9)))
        call lpt%add_output(create_Flow(5, (/m1,m3,m4,m5,m6/), p%point(10)))
        call lpt%print()

        temp = Q(1D0, unitless)
        Wlpts = p%point(6)%enthalpy_s%times(temp%minus(m3_m1%minus(m4_m1)), btu_lbm)
        Wlpts = Wlpts%minus(p%point(8)%enthalpy_s%times(m5_m1, btu_lbm))
        Wlpts = Wlpts%minus(p%point(9)%enthalpy_s%times(m6_m1, btu_lbm))
        Wlpts = Wlpts%minus(p%point(10)%enthalpy_s%times(temp%minus(m3_m1%minus(m4_m1%minus(m5_m1%minus(m6_m1)))), &
            btu_lbm))
        call tex_label("Plugging into equation (\ref{lpt_s}):")
        call tex_begin()
        write(13,"(A,F8.3)") "\frac{\dot{W}_{lpt,s}}{m_1} = ", Wlpts%get_value()
        call tex_end()
        write(*,"(A,F8.3,A)") "Wlpts = ", Wlpts%get_value(), tex_units(Wlpts)

        ! Point 18
        call p%point(18)%given_T(645.0, F)
        call p%point(18)%given_P(2350.0, psia)
        write(13,"(A)") "Using the tables for superheated vapor at P=2400 psia \& T=700 \(^{\circ}\)F:"
        call p%point(18)%set_h_s(1191.6, btu_lbm)
        call p%point(18)%set_s_s(1.3232, btu_lbmR)
        call p%report_point(18)

        ! Point 19
        call p%point(19)%given_T(575.0, F)
        call p%point(19)%given_P(2250.0, psia)
        write(13,"(A)") "Interpolating using the tables for superheated liquid at P=2250 psia \& T= \(^{\circ}\)F:"
        call p%point(19)%set_h_s(701.36, btu_lbm)
        call p%point(19)%set_s_s(0.8877, btu_lbmR)
        call p%report_point(19)

        call tex_label("RCP 1")
        rcp1 = create_Pump(1, 1, "rcp1", pump_efficiency)
        call rcp1%add_input(create_Flow(1, (/mp/), p%point(19)))
        call rcp1%add_output(create_Flow(1, (/mp/), p%point(20)))
        call rcp1%print()

        ! Point 20
        call p%point(20)%given_P(2350.0, psia)

        vf = sat_p_vf(p%point(19)%pressure)
        Wrcps = vf%times(p%point(20)%pressure%minus(p%point(19)%pressure), btu_lbm)
        call tex_begin()
        write(13,"(A,F8.3,A,F8.3,A,F8.3,A,F8.3,A)") "-\frac{\dot{W}_{rcp,s}}{\dot{m}_{ 1}} = " &
            // "h_{20,s} - h_{19,s} = v_{@P19}(P_{20} - P_{19}) = ", &
            vf%get_value(), "(", p%point(20)%pressure%get_value(), " - ", p%point(19)%pressure%get_value(), &
            ") = ", Wrcps%get_value(), tex_units(Wrcps)
        call tex_end()
        write(*,"(A,F8.3)") "Wrcp = ", Wrcps%get_value()

        temp = Wrcps%plus(p%point(19)%enthalpy_s)
        call p%point(20)%set_h_s(real(temp%get_value(), 4), btu_lbm)
        call tex_begin()
        write(13,"(A,F8.3,A,F8.3,A,F8.3,A,F8.3,A)") "h_{20,s} = -\frac{\dot{W}_{rcp,s}}{\dot{m}_{1}} + " &
            // "h_{19,s} = ", Wrcps%get_value(), " + ", p%point(19)%enthalpy_s%get_value(), " = ", &
            p%point(20)%enthalpy_s%get_value(), tex_units(p%point(20)%enthalpy_s)
        call tex_end()

        call p%point(20)%calc_x_s_from_h_s()
        call p%point(20)%calc_s_s()
        call p%report_point(20)

        call tex_label("FP 1")
        fp1 = create_Pump(1, 1, "fp1", pump_efficiency)
        call fp1%add_input(create_Flow(1, (/m2/), p%point(1)))
        call fp1%add_output(create_Flow(1, (/m2/), p%point(21)))
        call fp1%print()

        ! Point 21
        call tex_begin()
        write(13,"(A)") "P_{21} = \frac{1}{2} P_{1}"
        call tex_end()
        temp = p%point(1)%pressure%divide(Q(2D0, unitless), psia)
        call p%point(21)%given_P(real(temp%get_value(), 4), psia)

        vf = sat_p_vf(p%point(1)%pressure)
        Wfps = vf%times(p%point(21)%pressure%minus(p%point(1)%pressure), btu_lbm)
        call tex_begin()
        write(13,"(A,F8.3,A,F8.3,A,F8.3,A,F8.3,A)") "-\frac{\dot{W}_{fp,s}}{\dot{m}_{ 1}} = " &
            // "h_{21,s} - h_{1,s} = v_{@P1}(P_{21} - P_{1}) = ", &
            vf%get_value(), "(", p%point(21)%pressure%get_value(), " - ", p%point(1)%pressure%get_value(), &
            ") = ", Wfps%get_value(), tex_units(Wfps)
        call tex_end()
        write(*,"(A,F8.3)") "Wfps = ", Wfps%get_value(), tex_units(Wfps)

        temp = Wfps%plus(p%point(1)%enthalpy_s)
        call p%point(21)%set_h_s(real(temp%get_value(), 4), btu_lbm)
        call tex_begin()
        write(13,"(A,F8.3,A,F8.3,A,F8.3,A,F8.3,A)") "h_{21,s} = -\frac{\dot{W}_{fp,s}}{\dot{m}_{1}} + " &
            // "h_{21,s} = ", Wfps%get_value(), " + ", p%point(1)%enthalpy_s%get_value(), " = ", &
            p%point(21)%enthalpy_s%get_value(), tex_units(p%point(21)%enthalpy_s)
        call tex_end()

        call p%point(21)%calc_x_s_from_h_s()
        call p%point(21)%calc_s_s()
        call p%report_point(21)

        call tex_label("SG1: ")
        sg1 = create_HeatExchanger(2, 2)
        call sg1%add_input(create_Flow(1, (/mp/), p%point(18)))
        call sg1%add_input(create_Flow(1, (/m2/), p%point(21)))
        call sg1%add_output(create_Flow(1, (/mp/), p%point(19)))
        call sg1%add_output(create_Flow(1, (/m2/), p%point(2)))
        call sg1%print()

        call tex_begin()
        write(13,"(A)") "\frac{\dot{m}_{ 7}}{\dot{m}_{ 1}} =  \frac{\dot{m}_{ 2}}{\dot{m}_{ 1}}(\frac{h_{ 2,s} - " &
            // "h_{21,s}}{h_{18,s} - h_{19,s}})"
        call tex_end()

        call tex_begin()
        write(13,"(A)") "\frac{\dot{m}_{ 7}}{\dot{m}_{ 1}} = \frac{1}{2}\frac{\dot{m}_{ 8}}{\dot{m}_{ 1}} "
        call tex_end()

        call tex_begin()
        write(13,"(A)") "\frac{\dot{m}_{ 2}}{\dot{m}_{1}} = \frac{1}{2}"
        call tex_end()

        call tex_begin()
        write(13,"(A)") "\frac{\dot{m}_{ 8}}{\dot{m}_{ 1}} = \frac{h_{ 2,s} - h_{21,s}}{h_{18,s} - h_{19,s}}"
        call tex_end()

        m8_m1 = p%point(2)%enthalpy_s%minus(p%point(21)%enthalpy_s)
        m8_m1 = m8_m1%divide(p%point(18)%enthalpy_s%minus(p%point(19)%enthalpy_s), unitless)
        call tex_begin()
        write(13,"(A,F8.3,A,F8.3,A,F8.3,A,F8.3,A,F8.3)") "\frac{\dot{m}_{ 8}}{\dot{m}_{ 1}} = \frac{", &
            p%point(2)%enthalpy_s%get_value(), " - ", p%point(21)%enthalpy_s%get_value(), "}{", &
            p%point(18)%enthalpy_s%get_value(), " - ", p%point(19)%enthalpy_s%get_value(), "} = ", &
            m8_m1%get_value()
        call tex_end()

        call tex_label("Reactor:")
        Rx = create_Boiler(1, 1)
        call Rx%add_input(create_Flow(1, (/mc/), p%point(20)))
        call Rx%add_output(create_Flow(1, (/mc/), p%point(18)))
        call Rx%print()

        Qh = m8_m1%times(p%point(18)%enthalpy_s, btu_lbm)
        Qh = Qh%minus(m8_m1%times(p%point(20)%enthalpy_s, btu_lbm))
        call tex_begin()
        write(13,"(A,F8.3,A)") "\frac{\dot{Q}_{h}}{\dot{m}_{1}} = ", Qh%get_value(), tex_units(Qh)
        call tex_end()

        Wfps_total = Wfps%plus(Wfps)
        Wrcps_total = Wrcps%plus(Wrcps)
        eta_s = Whpts%plus(Wlpts%plus(Wcps%plus(Wcbps%plus(Wfps_total%plus(Wrcps_total)))))
        eta_s = eta_s%divide(Qh, unitless)

        call tex_end_document()
        call p%report_all_solved()

        write(*,"(A,F8.3)") "Cycle efficiency: ", eta_s%get_value()
    end subroutine hw3_problem_1
end module hw3p1
