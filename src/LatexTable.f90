module class_LatexTable
    use class_Quantity

    implicit none
    private

    public::LatexTable

    type LatexTable
        private
        character(1),allocatable::header(:)
        integer::cols,rows
        type(Quantity),allocatable::table(:,:)

    contains
        procedure::add_header
    end type LatexTable
contains
    function create_LatexTable(cols, rows) result(this)
        type(LatexTable)::this
        integer,intent(in)::cols
        integer,intent(in)::rows

        this%cols = cols
        this%rows = rows

        allocate(this%header(cols))
        allocate(this%table(cols,rows))
    end function create_LatexTable

    subroutine add_header(this, items)
        class(LatexTable),intent(inout)::this
        character(*),intent(in)::items(*)
        integer::i

        do i=1,this%cols
            this%header(i) = items(i)
        end do
    end subroutine add_header

    subroutine print(this)
        class(LatexTable),intent(inout)::this
        integer::c,r

        write(13,"(A)")
        do c=1,this%cols
        end do
    end subroutine print
    
end module class_LatexTable
