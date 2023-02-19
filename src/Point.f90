module class_Point
    use class_Quantity
    use SteamTable
    use common

    implicit none
    private
    
    public::Point,create_Point

    type Point
        type(Quantity)::temperature,pressure,enthalpy_s,enthalpy_a,entropy_s,entropy_a,quality
        integer::index
    contains
        procedure::given_T,given_P,given_x,calc_h_s,calc_s_s,set_ideal,is_solved,calc_x_from_h_s
    end type Point
contains
    function create_Point(index) result(this)
        integer,intent(in)::index
        type(Point)::this

        this%temperature = Q(0D0, unknown)
        this%pressure = Q(0D0, unknown)
        this%enthalpy_s = Q(0D0, unknown)
        this%enthalpy_a = Q(0D0, unknown)
        this%entropy_s = Q(0D0, unknown)
        this%entropy_a = Q(0D0, unknown)
        this%index = index
    end function create_Point

    subroutine given_T(this, value, units)
        class(Point),intent(inout)::this
        integer,intent(in)::units
        real,intent(in)::value

        this%temperature = Q(real(value, 8), units)
        call tex_begin()
        write(13,"(A,I2,A,F8.3,A)") "T_{", this%index, "} = ", value, tex_units(this%temperature)
        call tex_end()
    end subroutine given_T

    subroutine given_P(this, value, units)
        class(Point),intent(inout)::this
        integer,intent(in)::units
        real,intent(in)::value

        this%pressure = Q(real(value, 8), units)
        call tex_begin()
        write(13,"(A,I2,A,F8.3,A)") "P_{", this%index, "} = ", value, tex_units(this%pressure)
        call tex_end()
    end subroutine given_P

    subroutine given_x(this, value)
        class(Point),intent(inout)::this
        real,intent(in)::value

        this%quality = Q(real(value, 8), unitless)
        call tex_begin()
        write(13,"(A,I2,A,F8.3,A)") "x_{", this%index, "} = ", value * 100, "\%"
        call tex_end()
    end subroutine given_x

    subroutine calc_h_s(this)
        class(Point),intent(inout)::this
        type(Quantity)::hf,hfg

        hf = sat_p_hf(this%pressure)
        hfg = sat_p_hfg(this%pressure)

        this%enthalpy_s = hf%plus(this%quality%times(hfg, hfg%get_unit()))
        call tex_begin()
        write(13,"(A,I2,A,I2,A,I2,A,F8.3,A,F4.2,A,F8.3,A,F8.3)") "  h_{", this%index, ",s} = (h_{f} + x_{", this%index, &
            "}h_{fg})_{@P", this%index, "} = ", hf%get_value(), " + ", this%quality%get_value(), "\cdot", &
            hfg%get_value(), " = ", this%enthalpy_s%get_value(), tex_units(this%enthalpy_s)
        call tex_end()
    end subroutine calc_h_s

    subroutine calc_s_s(this)
        class(Point),intent(inout)::this
        type(Quantity)::sf,sfg

        sf = sat_p_sf(this%pressure)
        sfg = sat_p_sfg(this%pressure)

        this%entropy_s = sf%plus(this%quality%times(sfg, sfg%get_unit()))
        call tex_begin()
        write(13,"(A,I2,A,I2,A,I2,A,F8.3,A,F4.2,A,F8.3,A,F8.3)") "  s_{", this%index, ",s} = (s_{f} + x_{", this%index, &
            "}s_{fg})_{@P", this%index, "} = ", sf%get_value(), " + ", this%quality%get_value(), "\cdot", &
            sfg%get_value(), " = ", this%entropy_s%get_value(), tex_units(this%entropy_s)
        call tex_end()
    end subroutine calc_s_s

    subroutine calc_x_from_h_s(this)
        class(Point),intent(inout)::this
        type(Quantity)::hf,hfg

        hf = sat_p_hf(this%pressure)
        hfg = sat_p_hfg(this%pressure)

        this%quality = this%enthalpy_s%minus(hf)
        this%quality = this%quality%divide(hfg, unitless)

        call tex_begin()
        write(13,"(A,I2,A,I2,A,I2,A,F8.3,A,F8.3,A,F8.3,A,F8.3)") "x_{", this%index, "} = (\frac{h_{", this%index, &
            ",s} - h_{f}}{h_fg})_{@P", this%index, "} = \frac{", this%enthalpy_s%get_value(), " - ", hf%get_value(), &
            "}{", hfg%get_value(), "} = ", this%quality%get_value()
        call tex_end()
    end subroutine calc_x_from_h_s

    subroutine set_ideal(this)
        class(Point),intent(inout)::this

        this%enthalpy_a = this%enthalpy_s
        call tex_begin()
        write(13,"(A,I2,A,I2,A)") "h_{", this%index, ",a} = h_{", this%index, ",s}"
        call tex_end()

        call tex_begin()
        this%entropy_a = this%entropy_s
        write(13,"(A,I2,A,I2,A)") "s_{", this%index, ",a} = s_{", this%index, ",s}"
        call tex_end()
    end subroutine set_ideal

    function is_solved(this) result(solved)
        class(Point),intent(in)::this
        logical::solved

        solved = .true.
        solved = solved .and. this%pressure%is_known()
        solved = solved .and. this%enthalpy_a%is_known()
        solved = solved .and. this%enthalpy_s%is_known()
        solved = solved .and. this%entropy_a%is_known()
        solved = solved .and. this%entropy_s%is_known()
        solved = solved .and. this%quality%is_known()
    end function is_solved
end module class_Point
