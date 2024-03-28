module listOrden
    implicit none
    private


    type :: node
        integer(kind=8) :: index
        type(node), pointer :: next => null()
        type(node), pointer :: prev => null()
    contains
    end type node
    
    type, public :: listaOrdenamiento
        type(node), pointer :: head => null()
        type(node), pointer :: tail => null()
    
    contains
        procedure :: insert
        procedure :: printList
    end type listaOrdenamiento

contains
!index = 1
!0 -> ...  <- aux
!1 ->
!4 -> ...  <- aux%next
    subroutine insert(self, index)
        class(listaOrdenamiento), intent(inout) :: self
        integer(kind=8), intent(in) :: index

        type(node), pointer :: aux
        type(node), pointer :: new
        allocate(new)

        if(.not. associated(self%head)) then
            new%index = index
            self%head => new
            self%tail => new
        else
            if(index < self%head%index) then
                self%head%prev => new
                new%next => self%head
                self%head => new

                new%index = index

            else
                aux => self%head
                do while (associated(aux%next))
                    if(index < aux%next%index) then
                        if(index == aux%index) then
                        else
                            new%next => aux%next
                            new%prev => aux
                            aux%next%prev => new
                            aux%next => new

                            new%index = index

                        end if
                        return
                    end if
                    aux => aux%next
                end do

                if(index == aux%index) then

                else
                    self%tail%next => new
                    new%prev => self%tail
                    self%tail => new

                    new%index = index
            
                end if
            end if
        end if
    end subroutine insert

    subroutine printList(self)
        class(listaOrdenamiento), intent(in) :: self
        type(node), pointer :: aux

        aux => self%head

        do while(associated(aux))
            print *, 'INDICE: ', aux%index
            print *, ""
            aux => aux%next
        end do
    end subroutine printList


    
end module listOrden
!=======================================================================================================================================
