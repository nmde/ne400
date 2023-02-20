module class_Efficiency
    use class_Flow
    use common

    implicit none
    private

    public::Efficiency,create_Efficiency

    type Efficiency
        character(7),allocatable::inputs(:),outputs(:)
        integer::next_input,next_output,num_inputs,num_outputs
        character(8)::label
    contains
        procedure::add_input,add_output,print
    end type Efficiency
contains
    function create_Efficiency(num_inputs, num_outputs, label) result(this)
        type(Efficiency)::this
        integer,intent(in)::num_inputs,num_outputs
        character(*),intent(in)::label

        allocate(this%inputs(num_inputs))
        allocate(this%outputs(num_outputs))

        this%num_inputs = num_inputs
        this%num_outputs = num_outputs
        this%next_input = 1
        this%next_output = 1
        this%label = label
    end function create_Efficiency

    subroutine add_input(this, label)
        class(Efficiency),intent(inout)::this
        character(*),intent(in)::label

        this%inputs(this%next_input) = label
        this%next_input = this%next_input + 1
    end subroutine add_input

    subroutine add_output(this, label)
        class(Efficiency),intent(inout)::this
        character(*),intent(in)::label

        this%outputs(this%next_output) = label
        this%next_output = this%next_output + 1
    end subroutine add_output

    subroutine print(this)
        class(Efficiency),intent(in)::this
        integer::i

        call tex_begin()
        write(13,"(A)",advance="no") this%label // "_{s} = \frac{"
        do i=1,this%num_inputs
            if (i > 1) then
                write(13,"(A)",advance="no") " + "
            end if
            write(13,"(A)",advance="no") "\dot{W}_{" // this%inputs(i) // ",s} / \dot{m}_{1}"
        end do
        write(13,"(A)",advance="no") "}{"
        do i=1,this%num_outputs
            if (i > 1) then
                write(13,"(A)",advance="no") " + "
            end if
            write(13,"(A)",advance="no") "\dot{Q}_{" // this%outputs(i) // "} / \dot{m}_{1}"
        end do
        write(13,"(A)") "}"
        call tex_end()

        call tex_begin()
        write(13,"(A)",advance="no") this%label // "_{a} = \frac{"
        do i=1,this%num_inputs
            if (i > 1) then
                write(13,"(A)",advance="no") " + "
            end if
            write(13,"(A)",advance="no") "\dot{W}_{" // this%inputs(i) // ",a} / \dot{m}_{1}"
        end do
        write(13,"(A)",advance="no") "}{"
        do i=1,this%num_outputs
            if (i > 1) then
                write(13,"(A)",advance="no") " + "
            end if
            write(13,"(A)",advance="no") "\dot{Q}_{" // this%outputs(i) // "} / \dot{m}_{1}"
        end do
        write(13,"(A)") "}"
        call tex_end()
    end subroutine print
end module class_Efficiency
