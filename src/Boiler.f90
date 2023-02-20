module class_Boiler
    use class_Flow
    use common

    implicit none
    private

    public::Boiler,create_Boiler

    type Boiler
        type(Flow),allocatable::inputs(:),outputs(:)
        integer::next_input,next_output,num_inputs,num_outputs
    contains
        procedure::add_input,add_output,print
    end type Boiler
contains
    function create_Boiler(num_inputs, num_outputs) result(this)
        type(Boiler)::this
        integer,intent(in)::num_inputs,num_outputs

        allocate(this%inputs(num_inputs))
        allocate(this%outputs(num_outputs))

        this%num_inputs = num_inputs
        this%num_outputs = num_outputs
        this%next_input = 1
        this%next_output = 1
    end function create_Boiler

    subroutine add_input(this, fl)
        class(Boiler),intent(inout)::this
        type(Flow),intent(in)::fl

        this%inputs(this%next_input) = fl
        this%next_input = this%next_input + 1
    end subroutine add_input

    subroutine add_output(this, fl)
        class(Boiler),intent(inout)::this
        type(Flow),intent(in)::fl

        this%outputs(this%next_output) = fl
        this%next_output = this%next_output + 1
    end subroutine add_output

    subroutine print(this)
        class(Boiler),intent(in)::this
        integer::i

        call tex_begin()
        write(13,"(A)",advance="no") "\dot{Q}_{h}"
        do i=1,this%num_inputs
            write(13,"(A)",advance="no") " + "
            call this%inputs(i)%print()
        end do
        write(13,"(A)",advance="no") " = "
        do i=1,this%num_outputs
            if (i > 1) then
                write(13,"(A)",advance="no") " + "
            end if
            call this%outputs(i)%print()
        end do
        call tex_end()

        call tex_begin()
        write(13,"(A)",advance="no") "\frac{\dot{Q}_{h}}{\dot{m}_{1}} = "
        do i=1,this%num_outputs
            if (i > 1) then
                write(13,"(A)",advance="no") " + "
            end if
            call this%outputs(i)%print_divided(1)
        end do
        do i=1,this%num_inputs
            write(13,"(A)",advance="no") " - "
            call this%inputs(i)%print_divided(1)
        end do
        call tex_end()
    end subroutine print
end module class_Boiler
