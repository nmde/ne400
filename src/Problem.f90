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
        procedure::report_point,eq_T,eq_P,eq_h,eq_s
    end type Problem
contains
    function create_problem(num_points, unit_system, output_file, title, author, &
            date) result(this)
        type(Problem)::this
        integer,intent(in)::num_points,unit_system
        character(9),intent(in)::output_file
        character(17),intent(in)::title
        character(13),intent(in)::author
        character(19),intent(in)::date
        integer::io_err,i

        allocate(this%points(num_points))

        call initialize_steam_tables(unit_system)

        open(unit=13,file="C:\Users\dmnev\Documents\nmde\thermo\output\" // output_file,action="write",iostat=io_err)
        if (io_err /= 0) then
            stop "Could not write to " // output_file
        end if

        write(13,"(A)") "\documentclass{article}"
        write(13,"(A)") ""
        write(13,"(A)") "\title{" // title // "}"
        write(13,"(A)") "\author{" // author // "}"
        write(13,"(A)") "\date{" // date // "}"
        write(13,"(A)") ""
        write(13,"(A)") "\begin{document}"
        write(13,"(A)") ""
        write(13,"(A)") "\maketitle"
        write(13,"(A)") ""
        write(13,"(A)") "\section{}"
        write(13,"(A)") ""

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
        write(13,"(A,I2,A,I2,A,F8.3,A)") "T_{", point2, "} = T_{", point1, "} = ", t1%get_value(), tex_units(t1)
        call tex_end()
    end subroutine eq_T

    subroutine eq_P(this, point2, point1)
        class(Problem),intent(inout)::this
        integer,intent(in)::point1,point2
        type(Quantity)::p1

        p1 = this%points(point1)%pressure
        this%points(point2)%pressure = Q(p1%get_value(), p1%get_unit())

        call tex_begin()
        write(13,"(A,I2,A,I2,A,F8.3,A)") "P_{", point2, "} = P_{", point1, "} = ", p1%get_value(), tex_units(p1)
        call tex_end()
    end subroutine eq_P

    subroutine eq_h(this, point2, point1)
        class(Problem),intent(inout)::this
        integer,intent(in)::point1,point2
        type(Quantity)::h1

        h1 = this%points(point1)%enthalpy
        this%points(point2)%enthalpy = Q(h1%get_value(), h1%get_unit())

        call tex_begin()
        write(13,"(A,I2,A,I2,A,F8.3,A)") "h_{", point2, "} = h_{", point1, "} = ", h1%get_value(), tex_units(h1)
        call tex_end()
    end subroutine eq_h

    subroutine eq_s(this, point2, point1)
        class(Problem),intent(inout)::this
        integer,intent(in)::point1,point2
        type(Quantity)::s1

        s1 = this%points(point1)%entropy
        this%points(point2)%entropy = Q(s1%get_value(), s1%get_unit())

        call tex_begin()
        write(13,"(A,I2,A,I2,A,F8.3,A)") "s_{", point2, "} = s_{", point1, "} = ", s1%get_value(), tex_units(s1)
        call tex_end()
    end subroutine eq_s

    subroutine report_point(this, index)
        class(Problem),intent(in)::this
        integer,intent(in)::index
        type(Quantity)::p,h,s,x,t

        t = this%points(index)%temperature
        p = this%points(index)%pressure
        h = this%points(index)%enthalpy
        s = this%points(index)%entropy
        x = this%points(index)%quality

        write(*,"(A,I3,A)") "Point ", index, ": =========================="
        write(*,"(A,F8.3,A)" ) "    T = ", t%get_value(), " " // t%get_unit_str()
        write(*,"(A,F8.3,A)" ) "    P = ", p%get_value(), " " // p%get_unit_str()
        write(*,"(A,F8.3,A)" ) "    h = ", h%get_value(), " " // h%get_unit_str()
        write(*,"(A,F8.3,A)" ) "    s = ", s%get_value(), " " // s%get_unit_str()
        write(*,"(A,F8.3,A)" ) "    x = ", x%get_value(), " " // x%get_unit_str()
    end subroutine report_point
end module class_Problem
