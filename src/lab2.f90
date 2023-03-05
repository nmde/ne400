module lab2
    use class_Quantity

    implicit none
    private
    
    type(Quantity)::t_avg(5,7),std(5,7)

    public::lab_2
contains
    subroutine process_power_level(power_level, pl)
        character(3),intent(in)::power_level
        integer,intent(in)::pl
        integer::io_err,entries,i,j
        character(9)::times(30)
        character(44)::output_dir
        character(49)::input_file
        real::temp,t_min(7,30),t_max(7,30),t_sum(7),sum,t_min_avg(7),t_max_avg(7)
        output_dir = "C:\Users\dmnev\Documents\nmde\thermo\output\"
        input_file = "C:\Users\dmnev\Documents\nmde\thermo\data\" // power_level // ".txt";
        entries = 30

        open(unit=11, file=input_file, action="read", iostat=io_err)
        if (io_err /= 0) then
            stop "Could not read from input file,"
        end if

        do j=1,7
            t_sum(j) = 0
        end do

        do i=1,entries
            read(11,"(A,F6.3,F6.3,F6.3,F6.3,F6.3,F7.2,F7.2,F7.2,F7.2,F6.2,F6.2,F6.2,F6.2,F6.2,F6.2)") &
                times(i), temp, t_min(1,i), t_max(1,i), t_min(2,i), t_max(2,i), t_min(3,i), t_max(3,i), &
                t_min(4,i), t_max(4,i), t_min(5,i), t_max(5,i), t_min(6,i), t_max(6,i), t_min(7,i), t_max(7,i)
            do j=1,7
                t_sum(j) = t_sum(j) + t_min(j,i) + t_max(j,i)
            end do
        end do

        do j=1,7
            t_avg(pl,j) = Q(REAL(t_sum(j) / 60, 8), F)
            sum = 0
            do i=1,30
                sum = sum + ((t_min(j,i) - REAL(t_avg(pl,j)%get_value())) ** 2)
            end do
            do i=1,30
                sum = sum + ((t_min(j,i) - REAL(t_avg(pl,j)%get_value())) ** 2)
            end do
            std(pl,j) = Q(REAL(sqrt((REAL(1) / REAL(30 - 1)) * sum), 8), F)
        end do

        open(unit=12, file=output_dir // power_level // ".txt", action="write", iostat=io_err)
        if (io_err /= 0) then
            stop "Could not write to averages file."
        end if

        do j=1,7
            write(12, "(A,I1,A,F6.2,A,F5.2,A)") "T-", j + 1, " & \(", t_avg(pl,j)%get_value(), "\pm", std(pl,j)%get_value(), "\) \\"
        end do

        open(unit=13, file=output_dir // power_level // ".txt")
    end subroutine process_power_level

    subroutine lab_2
        integer::i
        type(Quantity)::qdot_2_4(5),rate,Cp,density,mdot,Th,Tc,qdot_5_6(5),Qth,Qtc,minus_one
        real::err_2_4(5),err_5_6(5)

        rate = Q(500D0, gal_min)
        density = Q(8.272D0, lbm_gal)
        Cp = Q(0.998D0, btu_lbmF)

        call process_power_level("1mw", 1)
        call process_power_level("900", 2)
        call process_power_level("800", 3)
        call process_power_level("700", 4)
        call process_power_level("600", 5)

        mdot = rate%times(density, lbm_min)
        mdot = mdot%get_in(lbm_s)
        Qth = mdot%times(Cp, btu_sF)
        minus_one = Q(-1D0, unitless)
        Qtc = minus_one%times(mdot%times(Cp, btu_sF), btu_sF)
        write(*,"(A,F8.3)") "Mdot = ", mdot%get_value()
        write(*,"(A,F8.3)") "Qth = ", Qth%get_value()
        write(*,"(A,F8.3)") "Qtc = ", Qtc%get_value()

        do i=1,5
            Th = t_avg(i,3)
            Tc = t_avg(i,1)
            qdot_2_4(i) = mdot%times(Cp%times(Th%minus(Tc), btu_lbm), btu_s)
            err_2_4(i) = REAL(sqrt(((std(i,3)%get_value() ** 2) * (Qth%get_value() ** 2)) + &
                ((std(i,1)%get_value() ** 2) * (Qtc%get_value() ** 2))))
            Th = t_avg(i,4)
            Tc = t_avg(i,5)
            qdot_5_6(i) = mdot%times(Cp%times(Th%minus(Tc), btu_lbm), btu_s)
            err_5_6(i) = REAL(sqrt(((std(i,4)%get_value() ** 2) * (Qth%get_value() ** 2)) + &
                ((std(i,5)%get_value() ** 2) * (Qtc%get_value() ** 2))))
        end do

        do i=1,5
            write(*,"(A,I1,A,F8.3,A,F8.3)") "Qdot ", i, " T-2 to T-4 : ", qdot_2_4(i)%get_value(), " +/- ", err_2_4(i)
        end do

        do i=1,5
            write(*,"(A,I1,A,F8.3,A,F8.3)") "Qdot ", i, " T-5 to T-6 : ", qdot_5_6(i)%get_value(), " +/- ", err_2_4(i)
        end do
    end subroutine lab_2
    
end module lab2
