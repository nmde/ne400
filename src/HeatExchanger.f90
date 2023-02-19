module class_HeatExchanger
    use class_Flow
    use common

    implicit none
    private

    public::HeatExchanger,create_HeatExchanger

    type HeatExchanger
        type(Flow),allocatable::inputs(:),outputs(:)
        integer::next_input,next_output,num_inputs,num_outputs
    contains
        procedure::add_input,add_output,print
    end type HeatExchanger
contains
    function create_HeatExchanger(num_inputs, num_outputs) result(this)
        type(HeatExchanger)::this
        integer,intent(in)::num_inputs,num_outputs

        allocate(this%inputs(num_inputs))
        allocate(this%outputs(num_outputs))

        this%num_inputs = num_inputs
        this%num_outputs = num_outputs
        this%next_input = 1
        this%next_output = 1
    end function create_HeatExchanger

    subroutine add_input(this, fl)
        class(HeatExchanger),intent(inout)::this
        type(Flow),intent(in)::fl

        this%inputs(this%next_input) = fl
        this%next_input = this%next_input + 1
    end subroutine add_input

    subroutine add_output(this, fl)
        class(HeatExchanger),intent(inout)::this
        type(Flow),intent(in)::fl

        this%outputs(this%next_output) = fl
        this%next_output = this%next_output + 1
    end subroutine add_output

    subroutine print(this)
        class(HeatExchanger),intent(in)::this
        integer::i

        call tex_begin()
        do i=1,this%num_inputs
            if (i > 1) then
                write(13,"(A)",advance="no") " + "
            end if
            call this%inputs(i)%print(1)
        end do
        write(13,"(A)",advance="no") " = "
        do i=1,this%num_outputs
            if (i > 1) then
                write(13,"(A)",advance="no") " + "
            end if
            call this%outputs(i)%print(1)
        end do
        write(13,"(A)") ""
        call tex_end()

        call tex_begin()
        do i=1,this%num_inputs
            if (i > 1) then
                write(13,"(A)",advance="no") " + "
            end if
            call this%inputs(i)%print_divided(1, 1)
        end do
        write(13,"(A)",advance="no") " = "
        do i=1,this%num_outputs
            if (i > 1) then
                write(13,"(A)",advance="no") " + "
            end if
            call this%outputs(i)%print_divided(1, 1)
        end do
        write(13,"(A)") ""
        call tex_end()
    end subroutine print
end module class_HeatExchanger
