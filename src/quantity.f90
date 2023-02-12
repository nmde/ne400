module class_Quantity
    implicit none
    private

    public::Quantity,Q,kPa,psia,C,K,F,R,unitless

    integer::kPa = 1, psia = 2, C = 3, K = 4, F = 5, R = 6, unitless = 7

    type Quantity
        private

        real::value
        integer::unit

    contains
        procedure::get_value,get_unit,get_in
    end type Quantity

contains
    function Q(initial_value, initial_unit) result(this)
        type(Quantity)::this
        real,intent(in)::initial_value
        integer,intent(in)::initial_unit

        this%value = initial_value
        this%unit = initial_unit
    end function Q

    function get_value(this) result(value)
        class(Quantity),intent(in)::this
        real::value

        value = this%value
    end function get_value

    function get_unit(this) result(unit)
        class(Quantity),intent(in)::this
        integer::unit

        unit = this%unit
    end function get_unit

    function get_in(this, unit) result(v)
        101 FORMAT (F6.1,A,F6.1,A)
        class(Quantity),intent(in)::this
        integer,intent(in)::unit
        real::v

        if (unit == this%unit) then
            v = this%value
        else if (this%unit == kPa .and. unit == psia) then
            v = 0.145038 * this%value
            write(*,101) this%value, " kPa = ", v, " psia"
        else if (this%unit == C .and. unit == K) then
            v = 273.15 + this%value
            write(*,101) this%value, " C = ", v, " K"
        else if (this%unit == F .and. unit == R) then
            v = 459.67 + this%value
            write(*,101) this%value, " F = ", v, " R"
        else
            write(*,"(A,I0.3,A,I0.3)") "Unhandled conversion from ", this%unit, " to ", unit
            stop "Unhandled unit conversion"
        end if
    end function get_in
end module class_Quantity
