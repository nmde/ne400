module lab2_old
    use stdlib_string_type, only: string_type

    implicit none
    private
    
    real::t_avg(5,7,30),std(5,7,30)

    public::lab_2
contains
    subroutine process_power_level(power_level, pl)
        character(3),intent(in)::power_level
        integer,intent(in)::pl
        integer::io_err,entries,i,j
        character(9)::times(30)
        character(44)::output_dir
        character(49)::input_file
        real::temp,t_min(7,30),t_max(7,30)
        output_dir = "C:\Users\dmnev\Documents\nmde\thermo\output\"
        input_file = "C:\Users\dmnev\Documents\nmde\thermo\data\" // power_level // ".txt";
        entries = 30

        open(unit=11, file=input_file, action="read", iostat=io_err)
        if (io_err /= 0) then
            stop "Could not read from input file,"
        end if

        do i=1,entries
            read(11,"(A,F6.3,F6.3,F6.3,F6.3,F6.3,F7.2,F7.2,F7.2,F7.2,F6.2,F6.2,F6.2,F6.2,F6.2,F6.2)") &
                times(i), temp, t_min(1,i), t_max(1,i), t_min(2,i), t_max(2,i), t_min(3,i), t_max(3,i), &
                t_min(4,i), t_max(4,i), t_min(5,i), t_max(5,i), t_min(6,i), t_max(6,i), t_min(7,i), t_max(7,i)
            do j=1,7
                t_avg(pl,j,i) = (t_min(j,i) + t_max(j,i)) / 2
                std(pl,j,i) = sqrt(((t_min(j,i) - t_avg(pl,j,i)) ** 2) + &
                    ((t_max(j,i) - t_avg(pl,j,i)) ** 2))
            end do
        end do

        open(unit=12, file=output_dir // power_level // ".txt", action="write", iostat=io_err)
        if (io_err /= 0) then
            stop "Could not write to averages file."
        end if

        do i=1,entries
            write(12, "(A)", advance="no") times(i)
            do j=1,7
                write(12, "(A,F6.2,A,F5.2,A)", advance="no") " & \(", t_avg(pl,j,i), "\pm", std(pl,j,i), "\)"
            end do
            write(12,"(A)") " \\"
        end do

        open(unit=13, file=output_dir // power_level // ".txt")
    end subroutine process_power_level

    subroutine lab_2
        integer::i
        real::qdot,mdot,Cp

        call process_power_level("1mw", 1)
        call process_power_level("900", 2)
        call process_power_level("800", 3)
        call process_power_level("700", 4)
        call process_power_level("600", 5)

        do i=1,5
            qdot = mdot * Cp * (t_avg())
        end do
    end subroutine lab_2
    
end module lab2_old
