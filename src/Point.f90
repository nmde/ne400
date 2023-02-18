module class_Point
    use class_Quantity
    use SteamTable
    use common

    implicit none
    private
    
    public::Point,create_point

    type Point
        type(Quantity)::temperature,pressure,enthalpy,entropy,quality
        integer::index
    contains
        procedure::given_T,given_P,given_x,calc_h,calc_s
    end type Point
contains
    function create_point(index) result(this)
        integer,intent(in)::index
        type(Point)::this

        this%temperature = Q(0D0, unknown)
        this%pressure = Q(0D0, unknown)
        this%enthalpy = Q(0D0, unknown)
        this%entropy = Q(0D0, unknown)
        this%index = index
    end function create_point

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

    subroutine calc_h(this)
        class(Point),intent(inout)::this
        type(Quantity)::hf,hfg

        hf = sat_p_hf(this%pressure)
        hfg = sat_p_hfg(this%pressure)

        this%enthalpy = hf%plus(this%quality%times(hfg, hfg%get_unit()))
        call tex_begin()
        write(13,"(A,I2,A,I2,A,I2,A,F8.3,A,F4.2,A,F8.3,A,F8.3)") "  h_{", this%index, "} = (h_{f} + x_{", this%index, &
            "}h_{fg})_{@P", this%index, "} = ", hf%get_value(), " + ", this%quality%get_value(), "\cdot", &
            hfg%get_value(), " = ", this%enthalpy%get_value(), tex_units(this%enthalpy)
        call tex_end()
    end subroutine calc_h

    subroutine calc_s(this)
        class(Point),intent(inout)::this
        type(Quantity)::sf,sfg

        sf = sat_p_sf(this%pressure)
        sfg = sat_p_sfg(this%pressure)

        this%entropy = sf%plus(this%quality%times(sfg, sfg%get_unit()))
        call tex_begin()
        write(13,"(A,I2,A,I2,A,I2,A,F8.3,A,F4.2,A,F8.3,A,F8.3)") "  s_{", this%index, "} = (s_{f} + x_{", this%index, &
            "}s_{fg})_{@P", this%index, "} = ", sf%get_value(), " + ", this%quality%get_value(), "\cdot", &
            sfg%get_value(), " = ", this%entropy%get_value(), tex_units(this%entropy)
        call tex_end()
    end subroutine calc_s
end module class_Point
