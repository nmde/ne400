module class_Problem
    use class_Quantity
    use class_Point
    use SteamTable
    use common

    implicit none
    private

    public::Problem,create_problem

    type Problem
        type(Point),allocatable::points(:)
    contains
        procedure::report_point,eq_T,eq_P,eq_h_s,eq_s_s
    end type Problem
contains
    function create_problem(num_points, unit_system, output_file) result(this)
        type(Problem)::this
        integer,intent(in)::num_points,unit_system
        character(9),intent(in)::output_file
        integer::io_err,i

        allocate(this%points(num_points))

        call initialize_steam_tables(unit_system)

        open(unit=13,file="C:\Users\dmnev\Documents\nmde\thermo\output\" // output_file,action="write",iostat=io_err)
        if (io_err /= 0) then
            stop "Could not write to " // output_file
        end if

        do i=1,num_points
            this%points(i) = create_point(i)
        end do
    end function create_problem

    subroutine eq_T(this, point2, point1)
        class(Problem),intent(inout)::this
        integer,intent(in)::point1,point2
        type(Quantity)::t1

        t1 = this%points(point1)%temperature
        this%points(point2)%temperature = Q(t1%get_value(), t1%get_unit())

        call tex_begin()
        write(13,"(A,I2,A,I2,A)") "T_{", point2, "} = T_{", point1, "}"
        call tex_end()
    end subroutine eq_T

    subroutine eq_P(this, point2, point1)
        class(Problem),intent(inout)::this
        integer,intent(in)::point1,point2
        type(Quantity)::p1

        p1 = this%points(point1)%pressure
        this%points(point2)%pressure = Q(p1%get_value(), p1%get_unit())

        call tex_begin()
        write(13,"(A,I2,A,I2,A)") "P_{", point2, "} = P_{", point1, "}"
        call tex_end()
    end subroutine eq_P

    subroutine eq_h_s(this, point2, point1)
        class(Problem),intent(inout)::this
        integer,intent(in)::point1,point2
        type(Quantity)::h1

        h1 = this%points(point1)%enthalpy_s
        this%points(point2)%enthalpy_s = Q(h1%get_value(), h1%get_unit())

        call tex_begin()
        write(13,"(A,I2,A,I2,A)") "h_{", point2, "} = h_{", point1, "}"
        call tex_end()
    end subroutine eq_h_s

    subroutine eq_s_s(this, point2, point1)
        class(Problem),intent(inout)::this
        integer,intent(in)::point1,point2
        type(Quantity)::s1

        s1 = this%points(point1)%entropy_s
        this%points(point2)%entropy_s = Q(s1%get_value(), s1%get_unit())

        call tex_begin()
        write(13,"(A,I2,A,I2,A)") "s_{", point2, "} = s_{", point1, "}"
        call tex_end()
    end subroutine eq_s_s

    subroutine report_point(this, index)
        class(Problem),intent(in)::this
        integer,intent(in)::index
        type(Quantity)::p,h_s,h_a,s_s,s_a,x,t

        t = this%points(index)%temperature
        p = this%points(index)%pressure
        h_s = this%points(index)%enthalpy_s
        s_s = this%points(index)%entropy_s
        h_a = this%points(index)%enthalpy_a
        s_a = this%points(index)%entropy_a
        x = this%points(index)%quality

        write(*,"(A,I3,A)") "Point ", index, ": =========================="
        write(*,"(A,F8.3,A)" ) "    T = ", t%get_value(), " " // t%get_unit_str()
        write(*,"(A,F8.3,A)" ) "    P = ", p%get_value(), " " // p%get_unit_str()
        write(*,"(A,F8.3,A,F8.3,A)" ) "    h ideal = ", h_s%get_value(), " actual = ", h_a%get_value(), " " // h_a%get_unit_str()
        write(*,"(A,F8.3,A,F8.3,A)" ) "    s ideal = ", s_s%get_value(), " actual = ", s_a%get_value(), " " // s_a%get_unit_str()
        write(*,"(A,F8.3,A)" ) "    x = ", x%get_value(), " " // x%get_unit_str()
    end subroutine report_point
end module class_Problem