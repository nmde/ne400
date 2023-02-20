module ne401project
    implicit none
    private

    public::project
contains
    subroutine project()
        integer::bigM,bigI,i,n_max,io_stat,regions
        real*16,allocatable::nu(:),w(:),sigma_t(:),sigma_s(:),sigma_f(:), &
            S(:),x(:)
        real*16::sigma_t_const,sigma_s_const,sigma_f_const,S_const,x_0,x_1,step,left_boundary,right_boundary
        character(64)::input_file
        character(1)::temp

        !write(*,"(A)",advance="no") "Path to input file: "
        !read(*,*) input_file
        input_file = "C:\Users\dmnev\Documents\nmde\thermo\input\test1.txt" ! TODO - remove this
        open(unit=11,file=input_file,action="read",iostat=io_stat)
        if (io_stat /= 0) then
            stop "Could not read input file."
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
        call print_table("nu", nu, bigM)

        read(11,*) temp
        do i=1,bigM
            read(11,*) w(i)
        end do
        call print_table(" w", w, bigM)

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

        call solve(x, bigI, nu, w, bigM, sigma_t, sigma_s, S, n_max)
    end subroutine project

    subroutine print_table(label, data, data_size)
        character(2),intent(in)::label
        real*16,intent(in)::data(*)
        integer,intent(in)::data_size
        integer::i

        do i=1,data_size
            write(*,"(A,I3,A,F19.16)") label // "(", i, ") = ", data(i)
        end do
        write(*,"(A)") ""
    end subroutine print_table

    function psi_plus(nu) result(re)
        real*16,intent(in)::nu
        integer::re

        re = 1
    end function psi_plus

    function psi_minus(nu) result(re)
        real*16,intent(in)::nu
        integer::re

        re = 0
    end function psi_minus

    function get_n(n,n_max) result(n_index)
        integer,intent(in)::n,n_max
        integer::n_index,n_max_real

        n_max_real = (n_max) * 2

        n_index = 1 + n + (n_max_real / 2)
    end function get_n

    subroutine solve(x, bigI, nu, w, bigM, sigma_t, sigma_s, S, n_max)
        101 FORMAT (A,I3,A,F12.6)

        real*16,intent(in)::x(*),w(*),sigma_t(*),sigma_s(*),S(*),nu(*)
        integer,intent(in)::bigI,bigM,n_max
        integer::i,m,n,n_index,n_max_real
        real*16,allocatable::delta_x(:),tau(:,:),alpha(:,:),S_tot(:,:),psi(:,:,:),phi(:,:),J(:,:), &
            delta_r(:,:),phi_final(:),phibar_final(:),j_final(:),phibar(:,:),psibar(:,:,:)

        n_max_real = (n_max + 1) * 2
        allocate(delta_x(bigI))
        allocate(tau(bigM,bigI))
        allocate(alpha(bigM,bigI))
        allocate(S_tot(n_max_real + 1, bigI))
        allocate(psi(n_max_real + 1,bigM,bigI + 1))
        allocate(psibar(n_max_real + 1,bigM,bigI + 1))
        allocate(phi(n_max_real + 1,bigI + 1))
        allocate(phibar(n_max_real + 1,bigI + 1))
        allocate(J(n_max_real + 1,bigI + 1))
        allocate(delta_r(n_max_real + 1,bigI))
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
                    alpha(m,i) = 0.5 - (tau(m,i) / REAL(12))
                end if
            end do
        end do

        do n=0,n_max
            n_index = get_n(n, n_max)
            do i=1,bigI
                if (n == 0) then
                    S_tot(n_index,i) = 0.5 * S(i)
                else
                    S_tot(n_index,i) = 0.5 * sigma_s(i) * phi(get_n(-(n - 1), n_max), i) ! ??
                end if
            end do
            do m=1,int(bigM / 2.0)
                if (n == 0) then
                    psi(n_index,m,bigI + 1) = psi_minus(nu(m))
                else if (n > 0) then
                    psi(n_index,m,bigI + 1) = 0
                end if
                do i=1,bigI
                    psi(n_index,m,i) = psi(n_index,m,i + 1) * exp(tau(m,i)) + &
                        (S_tot(n_index,i) / (sigma_t(i) + 10E-25)) * (1 - exp(tau(m,i)))
                end do
            end do
            do m=int(bigM / 2.0) + 1,bigM
                if (n == 0) then
                    psi(n_index,m,1) = psi_plus(nu(m))
                else if (n > 0) then
                    psi(n_index,m,1) = 0
                end if
                do i=1,bigI
                    psi(n_index,m,i + 1) = psi(n_index,m,i) * exp(-1 * tau(m,i)) + &
                        (S_tot(n_index,i) / (sigma_t(i) + 10E-25)) * (1 - exp(-1 * tau(m,i)))
                end do
            end do
            do m=1,bigM
                do i=1,bigI
                    psibar(get_n(-1 * n, n_max),m,i) = (alpha(m,i) * psi(n_index,m,i)) + &
                        ((1 - alpha(m,i)) * psi(n_index,m,i + 1))
                end do
            end do
            do i=1,bigI + 1
                phi(n_index,i) = 0
                J(n_index,i) = 0
                do m=1,bigM
                    phi(n_index,i) = phi(n_index,i) + (psi(n_index,m,i) * w(m))
                    J(n_index,i) = J(n_index,i) + (nu(m) * psi(n_index,m,i) * w(m))
                end do
            end do
            do i=1,bigI
                phibar(get_n(-1 * n, n_max),i) = 0
                do m=1,bigM
                    phibar(get_n(-1 * n, n_max),i) = phibar(get_n(-1 * n, n_max),i) + &
                        (psibar(get_n(-1 * n, n_max),m,i) * w(m))
                end do
            end do
            do i=1,bigI
                delta_r(n_index,i) = J(n_index,i+1) - J(n_index,i) + (delta_x(i) * &
                    ((sigma_t(i) * phibar(get_n(-1 * n, n_max), i)) - (2 * S_tot(n_index,i))))
            end do
        end do

        do i=1,bigI
            phi_final(i) = 0
            phibar_final(i) = 0
            j_final(i) = 0
            do n=0,n_max
                phi_final(i) = phi_final(i) + phi(get_n(n,n_max),i)
                phibar_final(i) = phibar_final(i) + phibar(get_n(n,n_max),i)
                j_final(i) = j_final(i) + J(get_n(n,n_max),i)
            end do
            write(*,101) "phi*_", i, " = ", phi_final(i)
            write(*,101) "phibar*_", i, " = ", phi_final(i)
            write(*,101) "J*_", i, " = ", phi_final(i)
        end do
    end subroutine solve
end module ne401project
