module list
    implicit none
    private

    type :: node
        private
        integer :: fila
        integer :: columna
        logical :: leido= .false.
        character(len=7) :: color
        type(node), pointer :: next => null()
    end type node

    type, public :: lista
        private
        type(node), pointer :: head => null()

    contains
        procedure :: append
        procedure :: print
        procedure :: print2
        final :: destructor
    end type lista

contains
    subroutine append(self, fila,columna,color)
        class(lista), intent(inout) :: self
        integer, intent(in) :: fila,columna
        character(len=7), intent(in) :: color

        type(node), pointer :: current
        type(node), pointer :: new
        allocate(new)

        new%fila= fila
        new%columna= columna
        new%color= color

        if(.not. associated(self%head)) then
            self%head => new
        else
            current => self%head
            do while(associated(current%next))
                current => current%next
            end do

            current%next => new
        end if

    end subroutine append

    subroutine print(self)
        class(lista), intent(in) :: self
        type(node), pointer :: current
        current => self%head

        do while(associated(current))
            print *, current%fila, ","
            current => current%next
        end do
    end subroutine print

    subroutine print2(self, fila, columna,color)
        class(lista), intent(in) :: self
        type(node), pointer :: current
        integer, intent(inout) :: fila,columna
        character(len=7), intent(inout) :: color
        current => self%head

        do while(associated(current))
            if (.not. current%leido) then
                fila = current%fila
                columna = current%columna
                color = current%color
                current%leido = .true.
                exit
            end if
            current => current%next
        end do
    end subroutine print2

    subroutine destructor(self)
        type(lista), intent(inout) :: self
        type(node), pointer :: aux

        do while(associated(self%head))
            aux => self%head%next
            deallocate(self%head)
            self%head = aux
        end do
    end subroutine destructor
end module list

PROGRAM main
use list
implicit none
integer :: fila,columna
character(len=7) :: color

type(lista) :: l
call l%append(1,1,'rojo')
call l%append(2,2,'verde')
call l%append(3,3,'azul')


end program main