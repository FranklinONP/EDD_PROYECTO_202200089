! Modulo de ventanilla=========================================================================================================
module listaAlbumes
    implicit none
    private

    type :: string_node
        integer :: value
        type(string_node), pointer :: next => null()
    end type string_node

    !nodo de lista de lista
    type :: node
        character(len=100) :: name
        type(node), pointer :: next => null()
        type(node), pointer :: prev => null()
        type(string_node), pointer :: top => null()
    contains
        procedure :: push
        procedure :: printer
        procedure :: search
        procedure :: agregarSublista
        procedure :: actualizar2
    end type node

    !objeto de lista de lista
    type, public :: List_of_listsA
        type(node), pointer :: head => null()
    contains
        procedure :: addNode
        procedure :: pushToNode
        procedure :: printList
        procedure :: graphV
        procedure :: actualizar
        
    end type List_of_listsA

contains

 subroutine push(this, value)
        class(node), intent(inout) :: this
        integer, intent(in) :: value

        type(string_node), pointer :: new
        allocate(new)
        new%value = value

        if(.not. associated(this%top)) then
            this%top => new
        else
            new%next => this%top
            this%top => new
        end if
    end subroutine push
subroutine addNode(this, name)
    class(List_of_listsA), intent(inout) :: this
    character(len=*), intent(in) :: name

    type(node), pointer :: temp, current
    allocate(temp)
    temp%name = name
    temp%next => null()

    if (.not. associated(this%head)) then
        this%head => temp
    else
        current => this%head
        do while(associated(current%next))
            current => current%next
        end do
        current%next => temp
    end if
end subroutine addNode

!anadir de forma doblemente enlazada
subroutine addNodeDouble(this, name)
    class(List_of_listsA), intent(inout) :: this
    character(len=*), intent(in) :: name

    type(node), pointer :: temp, current
    allocate(temp)
    temp%name = name
    temp%next => null()
    temp%prev => null()

    if (.not. associated(this%head)) then
        this%head => temp
    else
        current => this%head
        do while(associated(current%next))
            current => current%next
        end do
        current%next => temp
        temp%prev => current
    end if
end subroutine addNodeDouble

subroutine deleteNode(this, name)
        class(List_of_listsA), intent(inout) :: this
        character(len=*), intent(in) :: name

        type(node), pointer :: current, previous
        current => this%head
        previous => null()

        do while (associated(current) .and. current%name /= name)
            previous => current
            current => current%next
        end do

        if(associated(current) .and. current%name == name) then
            if(associated(previous)) then
                previous%next => current%next
            else
                this%head => current%next
            end if

            deallocate(current)
        end if
end subroutine deleteNode



subroutine pushToNode(this, name, value)
        class(List_of_listsA), intent(inout) :: this
        character(len=*), intent(in) :: name
        integer, intent(in):: value

        type(node), pointer :: aux
        aux => this%head

        do while(associated(aux))
            if(aux%name == name) then
                call aux%push(value)
                exit
            end if
            aux => aux%next
        end do
end subroutine pushToNode

subroutine printList(this)
        class(List_of_listsA), intent(in) :: this
        type(node), pointer :: aux

        aux => this%head

        do while(associated(aux))
            print *, 'Nombre: ', aux%name
            call aux%printer()
            print *, ""
            aux => aux%next
        end do
end subroutine printList


subroutine actualizar(this, valor)
    class(List_of_listsA), intent(inout) :: this
    integer, intent(in) :: valor

    type(node), pointer :: aux
    aux => this%head

    do while(associated(aux))
        call aux%actualizar2(valor)
        aux => aux%next
    end do
end subroutine actualizar

subroutine actualizar2(this, valor)
    class(node), intent(inout) :: this
    integer, intent(in) :: valor

    type(string_node), pointer :: aux, current, previous
    current => this%top
    previous => null()

    do while (associated(current))
        if (current%value == valor) then
            if (associated(previous)) then
                previous%next => current%next
            else
                this%top => current%next
            end if
            deallocate(current)
            exit
        end if
        previous => current
        current => current%next
    end do
end subroutine actualizar2


subroutine printer(this)
        class(node), intent(in) :: this
        type(string_node), pointer :: aux
        aux => this%top

        print *, "Pila:"
        do while(associated(aux))
            print *, aux%value
            aux => aux%next
        end do
end subroutine printer

function search(this, value) result(retval)
        class(node), intent(in) :: this
       integer, intent(in) :: value

        type(string_node), pointer :: current
        logical :: retval

        current => this%top
        retval = .false.

        do while(associated(current))
            if(current%value == value) then
                retval = .true.
                exit
            end if
            current => current%next
        end do
end function search

subroutine graphV(this)
    class(List_of_listsA), intent(in) :: this
    type(node), pointer :: current, previous
    integer :: i, j,nvv
    character(len=10) :: id_str, prev_id_str, g, idd, element_str,p,nv

    ! Abre un archivo en formato DOT
    open(unit=10, file='Albumes.dot', status='replace', action='write')
    write(10, *) 'digraph G {'
    write(10, *) 'rankdir=TB;'

    current => this%head
    previous => null()
    i = 0
    do while (associated(current))
        write(id_str, '(I10)') i
        write(nv,'(I10)') i+1
        p='0'
        g='0'

        write(10, *) 'node' // trim(adjustl(id_str)) // ' [label="' // &
            & trim(adjustl(current%name)) // '", color="red", shape="rectangle"];'


        call agregarSublista(current, i)

        if (associated(previous)) then
            write(prev_id_str, '(I10)') (i-1)
            write(10, *) 'node' // trim(adjustl(prev_id_str)) // ' -> node' // trim(adjustl(id_str)) // ' [dir="back"];'
            write(10, *) 'node' // trim(adjustl(prev_id_str)) // ' -> node' // trim(adjustl(id_str)) // ' [dir="forward"];'
            
        
        end if
        previous => current
        current => current%next
        i = i + 1
    end do

    write(10, *) '}'
    close(10)

    ! Ejecuta el comando para generar la imagen
    call system("dot -Tpng Albumes.dot -o Albumes.png")
    call system("Start ./Albumes.png")
end subroutine graphV

subroutine agregarSublista(this, parent_id)
    class(node), intent(in) :: this
    integer, intent(in) :: parent_id
    type(string_node), pointer :: aux, prev_aux
    integer :: j
    character(len=10) :: id_str, parent_id_str, prev_id_str, node_label
    character(len=10) :: int_string

    aux => this%top
    prev_aux => null()
    j = 0
    do while(associated(aux))
        write(id_str, '(I10)') j
        write(parent_id_str, '(I10)') parent_id
        if (j == 0) then
            write(int_string, '(I10)') aux%value
            node_label = trim(adjustl(int_string))
            write(10, *) 'element' // trim(adjustl(parent_id_str)) // trim(adjustl(id_str)) // &
            & ' [label="' // node_label // '", shape="ellipse"];'
            write(10, *) 'node' // trim(adjustl(parent_id_str)) // ' -> element' // &
            & trim(adjustl(parent_id_str)) // trim(adjustl(id_str)) // ' [dir="forward"];'
        else
            write(prev_id_str, '(I10)') (j-1)
            write(id_str, '(I10)') j
            write(10, *) 'element' // trim(adjustl(parent_id_str)) // trim(adjustl(prev_id_str)) // ' -> element' // &
            & trim(adjustl(parent_id_str)) // trim(adjustl(id_str)) // ' [dir="forward"];'
            write(int_string, '(I10)') aux%value
            node_label = trim(adjustl(int_string))
            write(10, *) 'element' // trim(adjustl(parent_id_str)) // trim(adjustl(id_str)) // &
            & ' [label="' // node_label // '", shape="ellipse"];'
        end if
        prev_aux => aux
        aux => aux%next
        j = j + 1
    end do
end subroutine agregarSublista


end module listaAlbumes
!=======================================================================================================================================

program main
use listaAlbumes
implicit none
type(List_of_listsA) :: lista
call lista%addNode( "alb1")
call lista%addNode( "alb2")
call lista%addNode( "alb3")
call lista%pushToNode("alb1", 1)
call lista%pushToNode("alb1", 2)
call lista%pushToNode("alb1", 3)
call lista%pushToNode("alb2", 4)
call lista%pushToNode("alb2", 2)
call lista%graphV()

call lista%printList()
call lista%actualizar(2)
print *, "==============================="
call lista%printList()
end program main