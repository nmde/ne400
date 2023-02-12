! Unit Codes
! 0 Unknown quantity
! 1 kPa
! 2 psia
! 3 C
! 4 K
! 5 F
! 6 R
! 7 (unitless)

module class_Quantity
    implicit none
    private

    public::Quantity,Q

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

    function get_in(this, unit) result(value)
        101 FORMAT (F6.1,A,F6.1,A)
        class(Quantity),intent(in)::this
        integer,intent(in)::unit
        real::value

        if (unit == this%unit) then
            value = this%value
        else if (this%unit == 1 .and. unit == 2) then
            value = 0.145038 * this%value
            write(*,101) this%value, " kPa = ", value, " psia"
        else if (this%unit == 3 .and. unit == 4) then
            value = 273.15 + this%value
            write(*,101) this%value, " C = ", value, " K"
        else if (this%unit == 5 .and. unit == 6) then
            value = 459.67 + this%value
            write(*,101) this%value, " F = ", value, " R"
        else
            write(*,"(A,I0.3,A,I0.3)") "Unhandled conversion from ", this%unit, " to ", unit
            stop "Unhandled unit conversion"
        end if
    end function get_in
end module class_Quantity
