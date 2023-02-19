module class_Flow
    implicit none
    private

    public::Flow,create_Flow

    type Flow
        integer,allocatable::mass_i(:)
        integer::enthalpy_i,num_masses,mode
    contains
        procedure::print,print_divided
    end type Flow
contains
    function create_Flow(mass_n, mass_i, enthalpy_i, mode) result(this)
        type(Flow)::this
        integer,intent(in)::mass_n,enthalpy_i,mass_i(mass_n)
        integer,intent(in),optional::mode

        allocate(this%mass_i(mass_n))

        if (present(mode)) then
            this%mode = mode
        else
            this%mode = 0
        end if

        this%num_masses = mass_n
        this%mass_i = mass_i
        this%enthalpy_i = enthalpy_i
    end function create_Flow

    subroutine print(this)
        class(Flow),intent(in)::this
        integer::j

        if (this%num_masses > 1) then
            write(13,"(A)",advance="no") "("
        end if
        write(13,"(A,I2,A)",advance="no") "\dot{m}_{", this%mass_i(1), "}"
        do j=2,this%num_masses
            if (this%mode == 0) then
                write(13,"(A,I2,A)",advance="no") " - "
            else
                write(13,"(A,I2,A)",advance="no") " + "
            end if
            write(13,"(A,I2,A)",advance="no") "\dot{m}_{", this%mass_i(j), "}"
        end do
        if (this%num_masses > 1) then
            write(13,"(A)",advance="no") ")"
        end if
        write(13,"(A,I2,A)",advance="no") "h_{", this%enthalpy_i, ",a}"
    end subroutine print

    subroutine print_divided(this, by)
        class(Flow),intent(in)::this
        integer,intent(in)::by
        integer::j

        if (this%num_masses == 1 .and. this%mass_i(1) == by) then
            write(13,"(A,I2,A)",advance="no") "h_{", this%enthalpy_i, ",a}"
        else
            if (this%num_masses > 1) then
                write(13,"(A)",advance="no") "("
            end if
            do j=1,this%num_masses
                if (j > 1) then
                    if (this%mode == 0) then
                        write(13,"(A,I2,A)",advance="no") " - "
                    else
                        write(13,"(A,I2,A)",advance="no") " + "
                    end if
                end if
                if (this%mass_i(j) == by) then
                    write(13,"(A)",advance="no") "1"
                else
                    write(13,"(A,I2,A,I2,A)",advance="no") "\frac{\dot{m}_{", this%mass_i(j), "}}{\dot{m}_{", by, "}}"
                end if
            end do
            if (this%num_masses > 1) then
                write(13,"(A)",advance="no") ")"
            end if
            write(13,"(A,I2,A)",advance="no") "h_{", this%enthalpy_i, ",a}"
        end if
    end subroutine print_divided
end module class_Flow
