module class_Turbine
    use class_Flow
    use common

    implicit none
    private

    public::Turbine,create_Turbine

    type Turbine
        type(Flow),allocatable::inputs(:),outputs(:)
        integer::next_input,next_output,num_inputs,num_outputs
        character(3)::label
        real::efficiency
    contains
        procedure::add_input,add_output,print,solve_ideal
    end type Turbine
contains
    function create_Turbine(num_inputs, num_outputs, label, efficiency) result(this)
        type(Turbine)::this
        integer,intent(in)::num_inputs,num_outputs
        character(*),intent(in)::label
        real,intent(in)::efficiency

        allocate(this%inputs(num_inputs))
        allocate(this%outputs(num_outputs))

        this%num_inputs = num_inputs
        this%num_outputs = num_outputs
        this%next_input = 1
        this%next_output = 1
        this%label = label
        this%efficiency = efficiency
    end function create_Turbine

    subroutine add_input(this, fl)
        class(Turbine),intent(inout)::this
        type(Flow),intent(in)::fl

        this%inputs(this%next_input) = fl
        this%next_input = this%next_input + 1
    end subroutine add_input

    subroutine add_output(this, fl)
        class(Turbine),intent(inout)::this
        type(Flow),intent(in)::fl

        this%outputs(this%next_output) = fl
        this%next_output = this%next_output + 1
    end subroutine add_output

    subroutine print(this)
        class(Turbine),intent(in)::this

        call tex_begin()
        call print_1stlaw(this)
        call tex_end()

        call tex_begin()
        write(13,"(A)",advance="no") "\frac{\dot{W}_{" // this%label // ",a}}{m_1} = "
        call print_equation(this, 0)
        call tex_end()

        call tex_begin()
        write(13,"(A)",advance="no") "\frac{\dot{W}_{" // this%label // ",s}}{m_1} = "
        call print_equation(this, 1)
        call tex_end()

        call tex_begin()
        call print_efficiency(this)
        call tex_end()
    end subroutine print
    
    subroutine print_1stlaw(this)
        class(Turbine),intent(in)::this
        integer::i
        do i=1,this%num_inputs
            if (i > 1) then
                write(13,"(A)",advance="no") " + "
            end if
            call this%inputs(i)%print()
        end do
        write(13,"(A)",advance="no") " = "
        do i=1,this%num_outputs
            if (i > 1) then
                write(13,"(A)",advance="no") " + "
            end if
            call this%outputs(i)%print()
        end do
        write(13,"(A)") " + \dot{W}_{" // this%label // ",a}"
    end subroutine print_1stlaw

    subroutine print_equation(this, mode)
        class(Turbine),intent(in)::this
        integer,intent(in)::mode
        integer::i

        do i=1,this%num_inputs
            if (i > 1) then
                write(13,"(A)",advance="no") " + "
            end if
            call this%inputs(i)%print_divided(1, mode)
        end do
        do i=1,this%num_outputs
            write(13,"(A)",advance="no") " - "
            call this%outputs(i)%print_divided(1, mode)
        end do
    end subroutine print_equation

    subroutine print_efficiency(this)
        class(Turbine),intent(in)::this

        write(13,"(A,F8.3,A)") "\eta_{" // this%label // "} = ", this%efficiency, &
            " = \frac{\dot{W}_{" // this%label // ",a} / \dot{m}_{1}}{\dot{W}_{" // this%label // ",s} / \dot{m}_{1}} = \frac{"
        call print_equation(this, 0)
        write(13,"(A)") "}{\dot{W}_{" // this%label // ",s} / \dot{m}_{1}}"
    end subroutine print_efficiency

    ! TODO
    function solve_ideal(this) result(work)
        class(Turbine),intent(in)::this
        real*8::work
        integer::i

        work = 0
        do i=1,this%num_inputs

        end do

        call tex_begin()
        write(13,"(A)",advance="no") "\frac{\dot{W}_{" // this%label // ",s}}{m_1} = "
        call tex_end()
    end function solve_ideal
end module class_Turbine
