module common
    use class_Quantity
    
    implicit none
    private

    public::tex_units,tex_begin,tex_end,tex_finish
contains
    function tex_units(qu) result(str)
        type(Quantity),intent(in)::qu
        character(39)::str

        str = "\textnormal{" // qu%get_unit_str() // "}"
    end function tex_units

    subroutine tex_begin()
        write(13,"(A)") "\begin{equation}"
    end subroutine tex_begin

    subroutine tex_end()
        write(13,"(A)") "\end{equation}"
        write(13,"(A)") ""
    end subroutine tex_end

    subroutine tex_finish()
        write(13,"(A)") ""
        write(13,"(A)") "\end{document}"
        write(13,"(A)") ""
    end subroutine
end module common
