module lista_module
use matrix_m
    implicit none
    private

    type :: node
        private
        integer :: value,id
        type(matrix) :: mtx
        type(node), pointer :: next => null()
    end type node

    type, public :: lista_m
        private
        type(node), pointer :: head => null()

    contains
        procedure :: push
        procedure :: print
        procedure :: existe
        procedure :: cargarDatos

    end type lista_m

contains
    subroutine cargarDatos(self, value, id, mtx)
        class(lista_m), intent(inout) :: self
        integer, intent(in) :: value
        integer, intent(inout) :: id
        type(matrix), intent(inout) :: mtx
        type(node), pointer :: current

        current => self%head
        do while(associated(current))
            if(current%value == value) then
                id=current%id
                mtx=current%mtx
                exit
            end if
            current => current%next
        end do

    end subroutine cargarDatos


    subroutine existe(self,value,result)
        class(lista_m), intent(in) :: self
        integer, intent(in) :: value
        logical, intent(inout) :: result
        type(node), pointer :: current

        current => self%head
        do while(associated(current))
            if(current%value == value) then
                result = .true.
                print*, "Elemento encontrado"
                print*, 'Result = ', result
                !call current%mtx%print()
                print*, '========='
                exit
            end if
            current => current%next
        end do
    end subroutine existe


    subroutine push(self, value,id, mtx)
        class(lista_m), intent(inout) :: self
        type(matrix), intent(in) :: mtx
        integer, intent(in) :: value,id

        type(node), pointer :: new
        allocate(new)

        new%value = value
        new%id = id
        new%mtx = mtx

        if(.not. associated(self%head)) then
            self%head => new
        else    
            new%next => self%head
            self%head => new
        end if
    end subroutine push

    subroutine print(self)
        class(lista_m), intent(in) :: self
        type(node), pointer :: current

        current => self%head
        do while(associated(current))
            print*, current%value
            current => current%next
        end do
    end subroutine print

end module lista_module