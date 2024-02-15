module List_of_lists_m
    implicit none
    private

    type :: string_node
        character(:), allocatable :: value
        type(string_node), pointer :: next => null()
    end type string_node

    !nodo de lista de lista
    type :: node
        integer :: index
        type(node), pointer :: next => null()
        type(string_node), pointer :: top => null()
    contains
        procedure :: push
        procedure :: print
    end type node
    
    !objeto de lista de lista
    type, public :: List_of_lists
        type(node), pointer :: head => null()
    contains
        procedure :: insert
        procedure :: printList
    end type List_of_lists

contains
    subroutine insert(self, index, value)
        class(List_of_lists), intent(inout) :: self
        integer, intent(in) :: index
        character(len=*), intent(in) :: value

        type(node), pointer :: aux
        type(node), pointer :: new
        allocate(new)

        if(.not. associated(self%head)) then
            new%index = index
            self%head => new
            call new%push(value)
        else
            if(index < self%head%index) then
                new%next => self%head
                self%head => new

                new%index = index
                call new%push(value)
            else
                aux => self%head
                do while (associated(aux%next))
                    if(index < aux%next%index) then
                        if(index == aux%index) then
                            call aux%push(value)
                        else
                            new%next => aux%next
                            aux%next => new

                            new%index = index
                            call new%push(value)
                        end if
                        return
                    end if
                    aux => aux%next
                end do

                if(index == aux%index) then
                    call aux%push(value)
                else
                    aux%next => new

                    new%index = index
                    call new%push(value)
                end if
            end if
        end if
    end subroutine insert

    subroutine printList(self)
        class(List_of_lists), intent(in) :: self
        type(node), pointer :: aux

        aux => self%head

        do while(associated(aux))
            print *, 'INDICE: ', aux%index
            call aux%print()
            print *, ""
            aux => aux%next
        end do
    end subroutine printList

    subroutine push(self, value)
        class(node), intent(inout) :: self
        character(len=*), intent(in) :: value

        type(string_node), pointer :: new
        allocate(new)
        new%value = value

        if(.not. associated(self%top)) then
            self%top => new
        else
            new%next => self%top
            self%top => new
        end if
    end subroutine push

    subroutine print(self)
        class(node), intent(in) :: self
        type(string_node), pointer :: aux
        aux => self%top

        print *, "Pila:"
        do while(associated(aux))
            print *, aux%value
            aux => aux%next
        end do
    end subroutine print

end module List_of_lists_m


program test
    use List_of_lists_m
    implicit none
    type(List_of_lists) :: list

    !En este caso el indice seria el numero de ventanilla
    call list%insert(1, "cabeza")
    call list%insert(1, "2")
    call list%insert(1, "3")
    call list%insert(2, "cabeza")
    call list%insert(3, "cabeza")
    call list%insert(3, "2")
    call list%insert(3, "3")
    call list%insert(3, "4")
    call list%insert(1, "5")

    call list%printList()
end program test
