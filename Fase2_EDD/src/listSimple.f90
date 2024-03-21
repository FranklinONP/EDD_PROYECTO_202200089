module linked_list_m
    implicit none
    private

    type :: node
        private
        integer :: fila,columna,nPixel
        character(len=7) :: color
        type(node), pointer :: next => null()
    end type node

    type, public :: linked_list
        private
        type(node), pointer :: head => null()

    contains
        procedure :: append
        procedure :: print
        procedure :: obtenerPixel
        final :: destructor
    end type linked_list

contains
    subroutine append(self, fila, columna, color, nPixel)
        class(linked_list), intent(inout) :: self
        integer, intent(in) :: fila,columna
        character(len=7), intent(in) :: color
        integer, intent(in) :: nPixel

        type(node), pointer :: current
        type(node), pointer :: new
        allocate(new)

        new%fila=fila
        new%columna=columna
        new%color=color
        new%nPixel=nPixel

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

    subroutine obtenerPixel(self,nPixel,pixel)
        class(linked_list), intent(in) :: self
        integer, intent(in) :: nPixel
        character(len=7), intent(out) :: pixel
        type(node), pointer :: current
        current => self%head

        do while(associated(current))
            if(current%nPixel==nPixel) then
                pixel=current%color
                return
            end if
            current => current%next
        end do
        pixel="null"
    end subroutine obtenerPixel


    subroutine print(self)
        class(linked_list), intent(in) :: self
        type(node), pointer :: current
        current => self%head

        do while(associated(current))
            print *, current%fila, ",", current%columna, ",", current%color, ",", current%nPixel
            write(*,*) " "
            current => current%next
        end do
    end subroutine print

    subroutine destructor(self)
        type(linked_list), intent(inout) :: self
        type(node), pointer :: aux

        do while(associated(self%head))
            aux => self%head%next
            deallocate(self%head)
            self%head = aux
        end do
    end subroutine destructor
end module linked_list_m