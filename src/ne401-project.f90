module ne401project
    implicit none
    private

    public::project
contains
    ! Reads user input and passes the input to the solve subroutine
    subroutine project()
        integer::bigM,bigI,i,n_max,io_stat,regions
        real*16,allocatable::nu(:),w(:),sigma_t(:),sigma_s(:),sigma_f(:), &
            S(:),x(:)
        real*16::sigma_t_const,sigma_s_const,sigma_f_const,S_const,x_0,x_1,step,left_boundary,right_boundary
        character(64)::input_file,output_file
        character(1)::temp

        !write(*,"(A)",advance="no") "Path to input file: "
        !read(*,*) input_file
        input_file = "C:\Users\dmnev\Documents\nmde\thermo\input\test1.txt" ! TODO - remove this
        open(unit=11,file=input_file,action="read",iostat=io_stat)
        if (io_stat /= 0) then
            stop "Could not read input file."
        end if

        !write(*,"(A)",advance="no") "Path to output file: "
        !read(*,*) output_file
        output_file = "C:\Users\dmnev\Documents\nmde\thermo\output\test1.txt" ! TODO - remove this
        open(unit=12,file=output_file,action="write",iostat=io_stat)
        if (io_stat /= 0) then
            stop "Could not open output file."
        end if

        read(11,*) temp, temp, x_0, x_1
        read(11,*) temp, temp, bigM
        read(11,*) temp, temp, bigI
        read(11,*) temp, temp, n_max
        read(11,*) temp, temp, regions

        write(*,"(A,I3,A,I3,A,I3,A,I3,A,F4.1,A,F4.1)") "Solving for M = ", bigM, " I = ", bigI, " n = ", n_max, &
            " and ", regions, " regions from x =", x_0, " to x =", x_1, "."

        read(11,*) temp, temp, sigma_t_const
        read(11,*) temp, temp, sigma_s_const
        read(11,*) temp, temp, sigma_f_const
        read(11,*) temp, temp, S_const
        read(11,*) temp, temp, left_boundary
        read(11,*) temp, temp, right_boundary

        allocate(nu(bigM))
        allocate(w(bigM))
        allocate(sigma_t(bigI))
        allocate(sigma_s(bigI))
        allocate(sigma_f(bigI))
        allocate(S(bigI))
        allocate(x(bigI + 1))

        read(11,*) temp
        do i=1,bigM
            read(11,*) nu(i)
        end do

        read(11,*) temp
        do i=1,bigM
            read(11,*) w(i)
        end do

        do i=1,bigI
            sigma_t(i) = sigma_t_const
            sigma_s(i) = sigma_s_const
            sigma_f(i) = sigma_f_const
            S(i) = S_const
        end do

        x(1) = x_0
        step = (x_1 - x_0) / bigI
        do i=2,bigI + 1
            x(i) = x(i - 1) + step
        end do

        call solve(x, bigI, nu, w, bigM, sigma_t, sigma_s, S, n_max, left_boundary, right_boundary)
    end subroutine project

    ! Helper function for printing a table to the output file.
    subroutine print_table(label, data, data_size)
        character(*),intent(in)::label
        real*16,intent(in)::data(*)
        integer,intent(in)::data_size
        integer::i

        do i=1,data_size
            write(*,"(A,I3,A,F19.16)") label // "(", i, ") = ", data(i)
        end do
        write(*,"(A)") ""
    end subroutine print_table

    ! Prints the output file
    subroutine print_output(phi, phibar, J, delta_r, bigI, n_max)
        101 FORMAT (A,I3,A,F12.6)

        real*16,intent(in)::phi(:,:),phibar(:,:),J(:,:),delta_r(:,:)
        integer,intent(in)::bigI,n_max
        real*16,allocatable::phi_final(:),phibar_final(:),J_final(:)
        integer::i,n
        character(24)::separator

        separator = "========================"

        allocate(phi_final(bigI))
        allocate(phibar_final(bigI))
        allocate(J_final(bigI))

        do n=0,n_max
            write(12,"(A,I3)") "n = ", n
            write(12,"(A)") separator
            write(12,"(A,I3,A,I3,A)") "  i  ϕ^(", n, ")     δr^(", n, ")"
            do i=1,bigI
                write(12,"(I3,A,F8.5,A,E11.5)") i, " ", phi(n + 1, i), "  ", delta_r(n + 1, i)
            end do
            write(12,"(A)") ""
        end do
    end subroutine print_output

    ! Applies the alogirhtm to solve the NTE.
    subroutine solve(x, bigI, nu, w, bigM, sigma_t, sigma_s, S, n_max, left_boundary, right_boundary)
        real*16,intent(in)::x(*),w(*),sigma_t(*),sigma_s(*),S(*),nu(*),left_boundary,right_boundary
        integer,intent(in)::bigI,bigM,n_max
        integer::i,m,n_real,n
        real*16,allocatable::delta_x(:),tau(:,:),alpha(:,:),S_tot(:,:),psi(:,:,:),phi(:,:),J(:,:), &
            delta_r(:,:),phi_final(:),phibar_final(:),j_final(:),phibar(:,:),psibar(:,:,:)

        allocate(delta_x(bigI))
        allocate(tau(bigM,bigI))
        allocate(alpha(bigM,bigI))
        allocate(S_tot(n_max + 1, bigI))
        allocate(psi(n_max + 1,bigM,bigI + 1))
        allocate(psibar(n_max + 1,bigM,bigI + 1))
        allocate(phi(n_max + 1,bigI + 1))
        allocate(phibar(n_max + 1,bigI + 1))
        allocate(J(n_max + 1,bigI + 1))
        allocate(delta_r(n_max + 1,bigI))
        allocate(phi_final(bigI))
        allocate(phibar_final(bigI))
        allocate(j_final(bigI))

        do i=1,bigI
            delta_x(i) = x(i + 1) - x(i)
        end do

        do m=1,bigM
            do i=1,bigI
                tau(m,i) = (sigma_t(i) * delta_x(i)) / nu(m)
                if (abs(tau(m,i)) >= 10E-5) then
                    if (tau(m,i) > 0) then
                        alpha(m,i) = (1 / tau(m,i)) - (exp(-1 * tau(m,i)) / (1 - exp(-1 * tau(m,i))))
                    else
                        alpha(m,i) = (1 / tau(m,i)) + (1 / (1 - exp(tau(m,i))))
                    end if
                else
                    alpha(m,i) = 0.5 - (tau(m,i) / 12.0)
                end if
            end do
        end do

        do n_real=0,n_max
            n = n_real + 1 ! Because FORTRAN arrays start at 1, when n_real = 0, n must be 1
            do i=1,bigI
                if (n == 1) then ! Adjusted for FORTRAN array indices
                    S_tot(n,i) = 0.5 * S(i)
                else
                    S_tot(n,i) = 0.5 * sigma_s(i) * phibar(n - 1, i)
                end if
            end do
            do m=1,bigM / 2
                if (n == 1) then ! Adjusted for FORTRAN array indices
                    psi(n,m,bigI + 1) = left_boundary
                else if (n > 0) then
                    psi(n,m,bigI + 1) = 0
                end if
                do i=bigI,1,-1
                    psi(n,m,i) = (psi(n,m,i + 1) * exp(tau(m,i))) + &
                        ((S_tot(n,i) / (sigma_t(i) + 10E-25)) * (1 - exp(tau(m,i))))
                end do
            end do
            do m=(bigM / 2) + 1,bigM
                if (n == 1) then ! Adjusted for FORTRAN array indices
                    psi(n,m,1) = right_boundary
                else if (n > 0) then
                    psi(n,m,1) = 0
                end if
                do i=1,bigI
                    psi(n,m,i + 1) = (psi(n,m,i) * exp(-1 * tau(m,i))) + &
                        ((S_tot(n,i) / (sigma_t(i) + 10E-25)) * (1 - exp(-1 * tau(m,i))))
                end do
            end do
            do m=1,bigM
                do i=1,bigI
                    psibar(n,m,i) = (alpha(m,i) * psi(n,m,i)) + ((1 - alpha(m,i)) * psi(n,m,i + 1))
                end do
            end do
            do i=1,bigI + 1
                phi(n,i) = 0
                J(n,i) = 0
                do m=1,bigM
                    phi(n,i) = phi(n,i) + (psi(n,m,i) * w(m))
                    J(n,i) = J(n,i) + (nu(m) * psi(n,m,i) * w(m))
                end do
            end do
            do i=1,bigI
                phibar(n,i) = 0
                do m=1,bigM
                    phibar(n,i) = phibar(n,i) + (psibar(n,m,i) * w(m))
                end do
            end do
            do i=1,bigI
                delta_r(n,i) = J(n,i + 1) - J(n, i) + (delta_x(i) * &
                    ((sigma_t(i) * phibar(n,i)) - (2 * S_tot(n,i))))
            end do
        end do

        call print_output(phi, phibar, J ,delta_r, bigI, n_max)
    end subroutine solve
end module ne401project
