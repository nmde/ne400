! Unit Codes
! 1 kPa
! 2 psia
! 3 C
! 4 K
! 5 F
! 6 R

module class_Quantity
    implicit none
    private

    public::Quantity,Quantity_constructor

    type Quantity
        private

        real::value
        integer::unit

    contains
        procedure::get_value,get_unit
    end type Quantity

contains
    function Quantity_constructor(initial_value, initial_unit) result(this)
        type(Quantity)::this
        real,intent(in)::initial_value
        integer,intent(in)::initial_unit

        this%value = initial_value
        this%unit = initial_unit
    end function Quantity_constructor

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
end module class_Quantity
