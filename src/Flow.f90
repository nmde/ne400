module class_Flow
    use class_Point

    implicit none
    private

    public::Flow,create_Flow,MassFlow,create_MassFlow

    type Flow
        type(MassFlow),allocatable::mass_flows(:)
        type(Point)::point
        integer::num_masses,mode
    contains
        procedure::print,print_divided
    end type Flow

    type MassFlow
        integer::label
    end type MassFlow
contains
    function create_Flow(mass_n, mf, p, mode) result(this)
        type(Flow)::this
        integer,intent(in)::mass_n
        type(Point),intent(in)::p
        integer,intent(in),optional::mode
        type(MassFlow),intent(in)::mf(mass_n)

        allocate(this%mass_flows(mass_n))

        if (present(mode)) then
            this%mode = mode
        else
            this%mode = 0
        end if

        this%num_masses = mass_n
        this%mass_flows = mf
        this%point = p
    end function create_Flow

    function create_MassFlow(label) result(this)
        type(MassFlow)::this
        integer,intent(in)::label

        this%label = label
    end function create_MassFlow

    function m(this, index) result(label)
        class(Flow),intent(in)::this
        integer,intent(in)::index
        integer::label

        label = this%mass_flows(index)%label
    end function m

    subroutine print(this, in_mode)
        class(Flow),intent(in)::this
        integer,intent(in),optional::in_mode
        integer::j,mode

        if (present(in_mode)) then
            mode = in_mode
        else
            mode = 0
        end if

        if (this%num_masses > 1) then
            write(13,"(A)",advance="no") "("
        end if
        write(13,"(A,I2,A)",advance="no") "\dot{m}_{", m(this,1), "}"
        do j=2,this%num_masses
            if (this%mode == 0) then
                write(13,"(A,I2,A)",advance="no") " - "
            else
                write(13,"(A,I2,A)",advance="no") " + "
            end if
            write(13,"(A,I2,A)",advance="no") "\dot{m}_{", m(this,j), "}"
        end do
        if (this%num_masses > 1) then
            write(13,"(A)",advance="no") ")"
        end if
        write(13,"(A,I2)",advance="no") "h_{", this%point%index
        if (mode == 1) then
            write(13,"(A)",advance="no") ",s}"
        else
            write(13,"(A)",advance="no") ",a}"
        end if
    end subroutine print

    subroutine print_divided(this, by, in_mode)
        class(Flow),intent(in)::this
        integer,intent(in)::by
        integer,intent(in),optional::in_mode
        integer::j,mode

        if (present(in_mode)) then
            mode = in_mode
        else
            mode = 0
        end if

        if (this%num_masses == 1 .and. m(this,1) == by) then
            write(13,"(A,I2)",advance="no") "h_{", this%point%index
            if (mode == 1) then
                write(13,"(A)",advance="no") ",s}"
            else
                write(13,"(A)",advance="no") ",a}"
            end if
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
                if (m(this,j) == by) then
                    write(13,"(A)",advance="no") "1"
                else
                    write(13,"(A,I2,A,I2,A)",advance="no") "\frac{\dot{m}_{", m(this,j), "}}{\dot{m}_{", by, "}}"
                end if
            end do
            if (this%num_masses > 1) then
                write(13,"(A)",advance="no") ")"
            end if
            write(13,"(A,I2)",advance="no") "h_{", this%point%index
            if (mode == 1) then
                write(13,"(A)",advance="no") ",s}"
            else
                write(13,"(A)",advance="no") ",a}"
            end if
        end if
    end subroutine print_divided
end module class_Flow
