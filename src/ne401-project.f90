module ne401project
    implicit none
    private

    public::project
contains
    ! Reads user input and passes the input to the solve subroutine
    subroutine project()
        integer::bigM,bigI,i,n_max,io_stat,regions,current_region
        real*16,allocatable::nu(:),w(:),sigma_t(:),sigma_s(:),sigma_f(:), &
            S(:),x(:),region_boundaries(:),sigma_t_regions(:),sigma_s_regions(:),sigma_f_regions(:), &
            S_regions(:)
        real*16::x_0,x_1,step,left_boundary,right_boundary
        character(64)::input_file
        character(1)::temp

        !write(*,"(A)",advance="no") "Path to input file: "
        !read(*,*) input_file
        input_file = "C:\Users\dmnev\Documents\nmde\thermo\input\test1-input.txt" ! TODO - remove this
        open(unit=11,file=input_file,action="read",iostat=io_stat)
        if (io_stat /= 0) then
            stop "Could not read input file."
        end if

        read(11,*) temp, temp, x_0, x_1
        read(11,*) temp, temp, bigM
        read(11,*) temp, temp, bigI
        read(11,*) temp, temp, n_max
        read(11,*) temp, temp, regions

        allocate(nu(bigM))
        allocate(w(bigM))
        allocate(sigma_t(bigI))
        allocate(sigma_s(bigI))
        allocate(sigma_f(bigI))
        allocate(S(bigI))
        allocate(x(bigI + 1))
        allocate(region_boundaries(regions))
        allocate(sigma_t_regions(regions))
        allocate(sigma_s_regions(regions))
        allocate(sigma_f_regions(regions))
        allocate(S_regions(regions))

        write(*,"(A,I3,A,I3,A,I3,A,I3,A,F4.1,A,F4.1)") "Solving for M = ", bigM, " I = ", bigI, " n = ", n_max, &
            " and ", regions, " regions from x =", x_0, " to x =", x_1

        read (11,*) temp
        do i=1,regions
            read(11,*) region_boundaries(i)
        end do

        read (11,*) temp
        do i=1,regions
            read(11,*) sigma_t_regions(i)
        end do

        read(11,*) temp
        do i=1,regions
            read(11,*) sigma_s_regions(i)
        end do

        read(11,*) temp
        do i=1,regions
            read(11,*) sigma_f_regions(i)
        end do

        read(11,*) temp
        do i=1,regions
            read(11,*) S_regions(i)
        end do

        read(11,*) temp, temp, left_boundary
        read(11,*) temp, temp, right_boundary

        read(11,*) temp
        do i=1,bigM
            read(11,*) nu(i)
        end do

        read(11,*) temp
        do i=1,bigM
            read(11,*) w(i)
        end do

        x(1) = x_0
        step = (x_1 - x_0) / bigI
        do i=2,bigI + 1
            x(i) = x(i - 1) + step
        end do

        current_region = 1
        do i=1,bigI
            if (x(i) > region_boundaries(current_region)) then
                current_region = current_region + 1
            end if
            sigma_t(i) = sigma_t_regions(current_region)
            sigma_s(i) = sigma_s_regions(current_region)
            sigma_f(i) = sigma_f_regions(current_region)
            S(i) = S_regions(current_region)
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

        call print_output(x, bigI, sigma_t, sigma_s, S, n_max, phi, phibar, J, delta_r, delta_x, nu, bigM, psi)
    end subroutine solve

    ! Prints the output files
    subroutine print_output(x, bigI, sigma_t, sigma_s, S, n_max, phi, phibar, J, delta_r, delta_x, nu, bigM, psi)
        real*16,intent(in)::x(*),phi(:,:),phibar(:,:),J(:,:),delta_r(:,:),delta_x(*),sigma_t(*),sigma_s(*),S(*),nu(*),psi(:,:,:)
        integer,intent(in)::bigI,n_max,bigM
        real*16,allocatable::phi_final(:),phibar_final(:),J_final(:),delta_r_final(:),psi_final(:,:)
        integer::i,n,io_stat,m
        character(24)::separator
        real*16::sigma_a

        separator = "========================"

        allocate(phi_final(bigI))
        allocate(phibar_final(bigI))
        allocate(J_final(bigI))
        allocate(delta_r_final(bigI))
        allocate(psi_final(bigM,bigI))

        open(unit=14,file="./tables.txt",action="write",iostat=io_stat)
        if (io_stat /= 0) then
            stop "Could not open output file."
        end if

        open(unit=16,file="./tables.tex",action="write",iostat=io_stat)
        if (io_stat /= 0) then
            stop "Could not open output file."
        end if

        do n=0,n_max
            write(14,"(A,I3)") "n = ", n
            write(14,"(A)") separator
            write(14,"(A,I3,A,I3,A)") "  i  ϕ^(", n, ")     δr^(", n, ")"
            write(16,"(A)") "\begin{table}[H]"
            write(16,"(A)") "\centering"
            write(16,"(A)") "\begin{tabular}{c|c|c}"
            write(16,"(A,I3,A,I3,A)") "i & \phi^{(", n, ")} & \delta r^{(", n, ")} \\"
            do i=1,bigI
                write(14,"(I3,A,F8.5,A,E11.5)") i, " ", phi(n + 1, i), "  ", delta_r(n + 1, i)
                write(16,"(I3,A,F8.5,A,E11.5,A)") i, " & ", phi(n + 1, i), " & ", delta_r(n + 1, i), " \\"
            end do
            write(14,"(A)") ""
            write(16,"(A)") "\end{tabular}"
            write(16,"(A,I3,A)") "\caption{n = ", n, "}"
            write(16,"(A)") "\end{table}"
            write(16,"(A)") ""
        end do

        open(unit=12,file="./graphs_1.csv",action="write",iostat=io_stat)
        if (io_stat /= 0) then
            stop "Could not open output file."
        end if

        write(12,"(A)") "  n,  i,    x,    ϕ^(n),      δr^(n),    J^(n), ϕavg^(n)"
        do n=0,n_max
            do i=1,bigI
                write(12,"(I3,A,I3,A,F4.2,A,F8.5,A,E11.5,A,F8.5,A,F8.5)") n, ",", i, ", ", x(i), ", ", phi(n + 1, i), ", ", &
                    delta_r(n + 1, i), ", ", J(n + 1, i), ", ", phibar(n + 1, i)
            end do
        end do

        open(unit=13,file="./graphs_2.csv",action="write",iostat=io_stat)
        if (io_stat /= 0) then
            stop "Could not open output file."
        end if

        write(13,"(A)") "   x,   Φ^(N), Φavg^(N),       F^(n)"
        do i=1,bigI
            phi_final(i) = 0
            J_final(i) = 0
            phibar_final(i) = 0
            do n=0,n_max
                phi_final(i) = phi_final(i) + phi(n + 1, i)
                J_final(i) = J_final(i) + J(n + 1, i)
                phibar_final(i) = phibar_final(i) + phibar(n + 1, i)
            end do
            write(13,"(F4.2,A,F8.5,A,F8.3,A,E11.5)") x(i), ",", phi_final(i), ", ", phibar_final(i), ", ", J_final(i)
        end do

        open(unit=15,file="./graphs_3.csv",action="write",iostat=io_stat)
        if (io_stat /= 0) then
            stop "Could not open output file."
        end if

        write(15,"(A)") "  i,         δR*"
        do i=1,bigI - 1
            delta_r_final(i) = 0
            do n=0,n_max
                sigma_a = sigma_t(i) - sigma_s(i)
                delta_r_final(i) = delta_r_final(i) + J_final(i + 1) - J_final(i) + &
                    (delta_x(i) * ((sigma_a * phibar_final(i)) - S(i)))
            end do
            write(15,"(I3,A,E11.5)") i, ", ", delta_r_final(i)
        end do

        open(unit=17,file="./graphs_4.csv",action="write",iostat=io_stat)
        if (io_stat /= 0) then
            stop "Could not open output file."
        end if

        write(17,"(A)") "x,              µ, Ψm^(10)"
        do i=1,bigI
            do m=1,bigM
                psi_final(m,i) = 0
                do n=1,n_max
                    psi_final(m,i) = psi_final(m,i) + psi(n,m,i)
                end do
                write(17,"(F4.2,A,E11.5,A,F8.5)") x(i), ", ", nu(m), ",", psi_final(m,i)
            end do
        end do
    end subroutine print_output
end module ne401project
