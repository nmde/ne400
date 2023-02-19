module class_Problem
    use class_Quantity
    use class_Point
    use SteamTable
    use common

    implicit none
    private

    public::Problem,create_problem

    type Problem
        type(Point),allocatable::point(:)
        integer::num_points
    contains
        procedure::report_point,eq_T,eq_P,eq_h_s,eq_s_s,report_all,report_all_solved
    end type Problem
contains
    function create_problem(num_points, unit_system, output_file) result(this)
        type(Problem)::this
        integer,intent(in)::num_points,unit_system
        character(9),intent(in)::output_file
        integer::io_err,i

        allocate(this%point(num_points))

        this%num_points = num_points

        call initialize_steam_tables(unit_system)

        open(unit=13,file="C:\Users\dmnev\Documents\nmde\thermo\output\" // output_file,action="write",iostat=io_err)
        if (io_err /= 0) then
            stop "Could not write to " // output_file
        end if

        do i=1,num_points
            this%point(i) = create_Point(i)
        end do
    end function create_problem

    subroutine eq_T(this, point2, point1)
        class(Problem),intent(inout)::this
        integer,intent(in)::point1,point2
        type(Quantity)::t1

        t1 = this%point(point1)%temperature
        this%point(point2)%temperature = Q(t1%get_value(), t1%get_unit())

        if (.not. t1%is_known()) then
            write(*,"(A,I2,A)") "**** WARNING: Using T ", point1, " before it is known!"
        end if

        call tex_begin()
        write(13,"(A,I2,A,I2,A)") "T_{", point2, "} = T_{", point1, "}"
        call tex_end()
    end subroutine eq_T

    subroutine eq_P(this, point2, point1)
        class(Problem),intent(inout)::this
        integer,intent(in)::point1,point2
        type(Quantity)::p1

        p1 = this%point(point1)%pressure
        this%point(point2)%pressure = Q(p1%get_value(), p1%get_unit())

        if (.not. p1%is_known()) then
            write(*,"(A,I2,A)") "**** WARNING: Using P ", point1, " before it is known!"
        end if

        call tex_begin()
        write(13,"(A,I2,A,I2,A)") "P_{", point2, "} = P_{", point1, "}"
        call tex_end()
    end subroutine eq_P

    subroutine eq_h_s(this, point2, point1)
        class(Problem),intent(inout)::this
        integer,intent(in)::point1,point2
        type(Quantity)::h1

        h1 = this%point(point1)%enthalpy_s
        this%point(point2)%enthalpy_s = Q(h1%get_value(), h1%get_unit())

        if (.not. h1%is_known()) then
            write(*,"(A,I2,A)") "**** WARNING: Using h_s ", point1, " before it is known!"
        end if

        call tex_begin()
        write(13,"(A,I2,A,I2,A)") "h_{", point2, ",s} = h_{", point1, ",s}"
        call tex_end()
    end subroutine eq_h_s

    subroutine eq_s_s(this, point2, point1)
        class(Problem),intent(inout)::this
        integer,intent(in)::point1,point2
        type(Quantity)::s1

        s1 = this%point(point1)%entropy_s
        this%point(point2)%entropy_s = Q(s1%get_value(), s1%get_unit())

        if (.not. s1%is_known()) then
            write(*,"(A,I2,A)") "*** WARNING: Using s_s ", point1, " before it is known!"
        end if

        call tex_begin()
        write(13,"(A,I2,A,I2,A)") "s_{", point2, ",s} = s_{", point1, ",s}"
        call tex_end()
    end subroutine eq_s_s

    subroutine report_point(this, index)
        class(Problem),intent(in)::this
        integer,intent(in)::index
        type(Quantity)::p,h_s,h_a,s_s,s_a,x,t

        t = this%point(index)%temperature
        p = this%point(index)%pressure
        h_s = this%point(index)%enthalpy_s
        s_s = this%point(index)%entropy_s
        h_a = this%point(index)%enthalpy_a
        s_a = this%point(index)%entropy_a
        x = this%point(index)%quality

        write(*,"(A,I3,A)") "Point ", index, ": =========================="
        ! write(*,"(A,F8.3,A)" ) "    T = ", t%get_value(), " " // t%get_unit_str()
        write(*,"(A,F8.3,A)" ) "    P = ", p%get_value(), " " // p%get_unit_str()
        write(*,"(A,F8.3,A,F8.3,A)" ) "    h ideal = ", h_s%get_value(), ", actual = ", h_a%get_value(), " " // h_a%get_unit_str()
        write(*,"(A,F8.3,A,F8.3,A)" ) "    s ideal = ", s_s%get_value(), ", actual = ", s_a%get_value(), " " // s_a%get_unit_str()
        write(*,"(A,F8.3,A)" ) "    x = ", x%get_value(), " " // x%get_unit_str()
    end subroutine report_point

    subroutine report_all(this)
        class(Problem),intent(in)::this
        integer::i

        do i=1,this%num_points
            call report_point(this, i)
        end do
    end subroutine report_all

    subroutine report_all_solved(this)
        class(Problem),intent(in)::this
        integer::i,solved_count

        solved_count = 0
        do i=1,this%num_points
            if (this%point(i)%is_solved()) then
                call report_point(this, i)
                solved_count = solved_count + 1
            end if
        end do
        write(*,"(A,I3,A,I3)") "Solved points: ", solved_count, " / ", this%num_points
    end subroutine report_all_solved
end module class_Problem
