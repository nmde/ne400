module class_Quantity
    implicit none
    private

    public::Quantity,Q,kPa,psia,C,K,F,R,unitless,kJ_kg,kJ_kgK,m3_kg,ft3_lbm,btu_lbm,btu_lbmR,lbm_gal,gal_min, &
        btu_lbmF,lbm_min,lbm_s,btu_s,btu_sF,unknown

    integer::unknown = 0, kPa = 1, psia = 2, C = 3, K = 4, F = 5, R = 6, unitless = 7, kJ_kg = 8, kJ_kgK = 9, m3_kg = 10, &
        ft3_lbm = 11, btu_lbm = 12, btu_lbmR = 13, lbm_gal = 14, gal_min = 15, btu_lbmF = 16, lbm_min = 17, &
        lbm_s = 18, btu_s = 19, btu_sF = 20

    type Quantity
        private

        real*8::value
        integer::unit

    contains
        procedure::get_value,get_unit,get_in,plus,minus,times,divide,get_unit_str,is_known
    end type Quantity

contains
    function Q(initial_value, initial_unit) result(this)
        type(Quantity)::this
        real*8,intent(in)::initial_value
        integer,intent(in)::initial_unit

        this%value = initial_value
        this%unit = initial_unit
    end function Q

    function get_value(this) result(value)
        class(Quantity),intent(in)::this
        real*8::value

        value = this%value
    end function get_value

    function get_unit(this) result(unit)
        class(Quantity),intent(in)::this
        integer::unit

        unit = this%unit
    end function get_unit

    function get_unit_str(this) result(unit_str)
        class(Quantity),intent(in)::this
        character(26)::unit_str

        if (this%unit == unitless) then
            unit_str = ""
        else if (this%unit == unknown) then
            unit_str = "UNKNOWN"
        else if (this%unit == F) then
            unit_str = "F"
        else if (this%unit == psia) then
            unit_str = "psia"
        else if (this%unit == btu_lbm) then
            unit_str = "BTU/lbm"
        else if (this%unit == btu_lbmR) then
            unit_str = "BTU/lbm}\cdot\textnormal{R"
        else
            write(*,"(A,I3)") "Unhandled unit to string conversion: ", this%unit
            stop "Unhandled unit to string conversion"
        end if
    end function get_unit_str

    function get_in(this, unit) result(v)
        101 FORMAT (F6.1,A,F6.1,A)
        class(Quantity),intent(in)::this
        integer,intent(in)::unit
        type(Quantity)::v

        if (unit == this%unit) then
            v = this
        else if (this%unit == kPa .and. unit == psia) then
            v = Q(0.145038 * this%value, psia)
            write(*,101) this%value, " kPa = ", v%get_value(), " psia"
        else if (this%unit == C .and. unit == K) then
            v = Q(273.15 + this%value, K)
            write(*,101) this%value, " C = ", v%get_value(), " K"
        else if (this%unit == F .and. unit == R) then
            v = Q(459.67 + this%value, R)
            write(*,101) this%value, " F = ", v%get_value(), " R"
        else if (this%unit == lbm_min .and. unit == lbm_s) then
            v = Q(this%value / 60, lbm_s)
            write(*,101) this%value, " lbm/min = ", v%get_value(), " lbm/s"
        else
            write(*,"(A,I0.3,A,I0.3)") "Unhandled conversion from ", this%unit, " to ", unit
            stop "Unhandled unit conversion"
        end if
    end function get_in

    function plus(this, otherQuantity) result(re)
        class(Quantity),intent(in)::this
        type(Quantity),intent(in)::otherQuantity
        type(Quantity)::re

        if (this%get_unit() == 0 .or. otherQuantity%get_unit() == 0) then
            write(*,"(A)") "Warning: Operation with unknown quantities"
        end if

        if (this%get_unit() /= otherQuantity%get_unit()) then
            write(*,"(A,I3,A,I3,A,F8.3,A,F8.3,A)") "Incompatible units for addition: ", this%get_unit(), ", ", &
                otherQuantity%get_unit(), " (", this%get_value(), " + ", otherQuantity%get_value(), ")"
            stop "Incompatible units"
        end if

        re = Q(this%get_value() + otherQuantity%get_value(), this%get_unit())
        write(*,"(F8.3,A,F9.3,A,F9.3)") this%get_value(), " + ", otherQuantity%get_value(), " = ", re%get_value()
    end function plus

    function minus(this, otherQuantity) result(re)
        class(Quantity),intent(in)::this
        type(Quantity),intent(in)::otherQuantity
        type(Quantity)::re

        if (this%get_unit() == 0 .or. otherQuantity%get_unit() == 0) then
            write(*,"(A)") "Warning: Operation with unknown quantities"
        end if

        if (this%get_unit() /= otherQuantity%get_unit()) then
            write(*,"(A,I3,A,I3,A,F8.3,A,F8.3,A)") "Incompatible units for subtraction: ", this%get_unit(), ", ", &
                otherQuantity%get_unit(), " (", this%get_value(), " - ", otherQuantity%get_value(), ")"
            stop "Incompatible units"
        end if

        re = Q(this%get_value() - otherQuantity%get_value(), this%get_unit())
        write(*,"(F8.3,A,F8.3,A,F8.3)") this%get_value(), " - ", otherQuantity%get_value(), " = ", re%get_value()
    end function minus

    function times(this, otherQuantity, outputUnits) result(re)
        class(Quantity),intent(in)::this
        type(Quantity),intent(in)::otherQuantity
        integer,intent(in)::outputUnits
        type(Quantity)::re

        if (this%get_unit() == 0 .or. otherQuantity%get_unit() == 0) then
            write(*,"(A)") "Warning: Operation with unknown quantities"
        end if

        ! TODO: automatically determine units
        re = Q(this%get_value() * otherQuantity%get_value(), outputUnits)
        write(*,"(F8.3,A,F8.3,A,F9.3)") this%get_value(), " * ", otherQuantity%get_value(), " = ", re%get_value()
    end function times

    function divide(this, otherQuantity, output_units) result(re)
        class(Quantity),intent(in)::this
        type(Quantity),intent(in)::otherQuantity
        integer,intent(in)::output_units
        type(Quantity)::re

        if (this%get_unit() == 0 .or. otherQuantity%get_unit() == 0) then
            write(*,"(A)") "Warning: Operation with unknown quantities"
        end if

        re = Q(this%get_value() / otherQuantity%get_value(), output_units)
        write(*,"(F8.3,A,F8.3,A,F8.3)") this%get_value(), " / ", otherQuantity%get_value(), " = ", re%get_value()
    end function divide

    function is_known(this) result(known)
        class(Quantity),intent(in)::this
        logical::known

        known = this%unit /= unknown
    end function is_known
end module class_Quantity
