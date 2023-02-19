module common
    use class_Quantity
    
    implicit none
    private

    public::tex_units,tex_begin,tex_end,tex_create_document,tex_end_document,tex_label,tex_simple_val,tex_equation
contains
    function tex_units(qu) result(str)
        type(Quantity),intent(in)::qu
        character(40)::str

        str = "\textnormal{ " // qu%get_unit_str() // "}"
    end function tex_units

    subroutine tex_begin()
        write(13,"(A)") "\begin{equation}"
    end subroutine tex_begin

    subroutine tex_end()
        write(13,"(A)") "\end{equation}"
        write(13,"(A)") ""
    end subroutine tex_end

    subroutine tex_create_document(title, author, date)
        character(17),intent(in)::title
        character(13),intent(in)::author
        character(19),intent(in)::date

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
    end subroutine tex_create_document

    subroutine tex_end_document()
        write(13,"(A)") ""
        write(13,"(A)") "\end{document}"
        write(13,"(A)") ""
    end subroutine tex_end_document

    subroutine tex_label(text)
        character(*)::text

        write(13,"(A)") ""
        write(13,"(A)") text
    end subroutine tex_label

    subroutine tex_simple_val(varname, value)
        character(8),intent(in)::varname
        real,intent(in)::value

        call tex_begin()
        write(13,"(A,F8.3)") varname // " = ", value
        call tex_end()
    end subroutine tex_simple_val

    subroutine tex_equation(text)
        character(*),intent(in)::text

        call tex_begin()
        write(13,"(A)") text
        call tex_end()
    end subroutine tex_equation
end module common
