!Modulo para cola de clientes=========================================================================================================
! Este modulo implementa una cola de clientes
module colaClientes
    implicit none
    private
!Nodo de la cola
    type :: nodeC
        integer :: id
        character(:), allocatable :: nombre
        integer :: imagenesPequenas
        integer :: imagenesGrandes
        type(nodeC), pointer :: next => null()
    end type nodeC

    type, public :: queue
        private
        type(nodeC), pointer :: front => null()
        type(nodeC), pointer :: rear => null()
    contains
        procedure :: enqueue
        procedure :: dequeue
        procedure :: graph
        procedure :: print
    end type queue      

contains
    
!Encola un cliente
    subroutine enqueue(this, id, nombre, imagenesPequenas, imagenesGrandes)
            class(queue), intent(inout) :: this
            integer, intent(in) :: id
            character(len=*), intent(in) :: nombre
            integer, intent(in) :: imagenesPequenas
            integer, intent(in) :: imagenesGrandes

            type(nodeC), pointer :: temp

            allocate(temp)
            temp%id = id
            temp%nombre = nombre
            temp%imagenesPequenas = imagenesPequenas
            temp%imagenesGrandes = imagenesGrandes
            temp%next => null()

            if (.not. associated(this%front)) then
                this%front => temp
                this%rear => temp
            else
                this%rear%next => temp
                this%rear => temp
            end if
        end subroutine enqueue  


 subroutine dequeue(this, id, ip, ig, nombre)
        class(queue), intent(inout) :: this
        integer, intent(out) :: id
        integer, intent(out) :: ig
        integer, intent(out) :: ip
        character(len=*), intent(out) :: nombre
        type(nodeC), pointer :: temp

        if (associated(this%front)) then
            temp => this%front
                id = temp%id
                ig = temp%imagenesGrandes
                ip = temp%imagenesPequenas
                nombre = temp%nombre
            this%front => this%front%next
            deallocate(temp)
        else
            print *, 'La cola está vacía.'
        end if
    end subroutine dequeue



!Imprime la cola
    subroutine print(this)
        class(queue), intent(in) :: this
        type(nodeC), pointer :: current

        current => this%front

        do while (associated(current))
            print *,'Id:', current%id
            print*, 'Nombre: ', current%nombre
            print*, "#Imagenes Pequenas", current%imagenesPequenas
            print*, "#Imagenes Grandes" , current%imagenesGrandes
            current => current%next
        end do
    end subroutine print


!Agrega esta función al módulo colaClientes
subroutine graph(this)
    class(queue), intent(in) :: this
    type(nodeC), pointer :: current
    integer :: i
    character(len=10) :: id_str, next_id_str,p,g

    ! Abre un archivo en formato DOT
    open(unit=10, file='queue.dot', status='replace', action='write')
    write(10, *) 'digraph G {'
    write(10, *) 'rankdir=LR;'

    current => this%front
    i = 0
    do while (associated(current))
        write(id_str, '(I10)') current%id

        write(p, '(I10)') current%imagenesPequenas
        write(g, '(I10)') current%imagenesGrandes

        write(10, *) 'node' // trim(adjustl(id_str)) // ' [label="Cliente: ' // trim(adjustl(current%nombre)) // &
     & '\n''IMP: ' // trim(adjustl(p)) // '\n''IMG: ' // trim(adjustl(g)) // '", color="red", shape="rectangle"];'

        if (associated(current%next)) then
            write(next_id_str, '(I10)') current%next%id
            write(10, *) 'node' // trim(adjustl(next_id_str)) // ' -> node' // trim(adjustl(id_str)) // ' [dir="forward"];'
        end if
        current => current%next
        i = i + 1
    end do

    write(10, *) '}'
    close(10)

    ! Ejecuta el comando para generar la imagen
    call system("dot -Tpng queue.dot -o queue.png")
end subroutine graph

end module colaClientes    
!=======================================================================================================================================

! Modulo de ventanilla=========================================================================================================
module listaVentanilla
    implicit none
    private

    type :: string_node
        character(:), allocatable :: value
        type(string_node), pointer :: next => null()
    end type string_node

    !nodo de lista de lista
    type :: node
        integer :: index
        logical :: EstadoVentanilla = .false.
        integer :: NumeroImagenes=0
        integer :: imagenesPequenas=0
        integer :: imagenesGrandes=0
        integer :: ip
        integer :: ig
        integer :: ocupada=0
        logical :: encolar= .false.
        integer :: pasos

        character(:), allocatable :: name
        type(node), pointer :: next => null()
        type(node), pointer :: prev => null()
        type(string_node), pointer :: top => null()
    contains
        procedure :: push
        procedure :: printer
        procedure :: agregarSublista
        procedure :: delete
        procedure :: search
        procedure :: pop
    end type node

    !objeto de lista de lista
    type, public :: List_of_lists
        type(node), pointer :: head => null()
    contains
        procedure :: addNode
        procedure :: addNodeDouble
        procedure :: cabeza
        procedure :: pushToNode
        procedure :: printList
        procedure :: deleteNode
        procedure :: deleteNodeDouble
        procedure :: searchNode
        procedure :: printAtendidos
        procedure :: graphV
        procedure :: agregarAtendido
        procedure :: updateNode
        procedure :: agregarClienteAtendido
        procedure :: agregarImpresion
        procedure :: colar2
        procedure :: agregarImagenes
    end type List_of_lists

contains
subroutine addNode(this, index, name)
    class(List_of_lists), intent(inout) :: this
    integer, intent(in) :: index
    character(len=*), intent(in) :: name

    type(node), pointer :: temp, current
    allocate(temp)
    temp%index = index
    temp%name = name
    temp%next => null()

    if (.not. associated(this%head)) then
        this%head => temp
        print*, 'Nodo agregado'
        print*, 'Ventanilla No. ', index, ' creada'
        print*, 'Igrande', temp%imagenesGrandes
        print*, 'Ipequena', temp%imagenesPequenas
    else
        current => this%head
        do while(associated(current%next))
            current => current%next
        end do
        current%next => temp
                print*, 'Nodo agregado'
                print*, 'Ventanilla No. ', index, ' creada'
                print*, 'Igrande', temp%imagenesGrandes
                print*, 'Ipequena', temp%imagenesPequenas
    end if
end subroutine addNode


subroutine agregarAtendido(this, index, name,p,g,pasos)
    class(List_of_lists), intent(inout) :: this
    integer, intent(in) :: index,p,g,pasos
    character(len=*), intent(in) :: name

    type(node), pointer :: temp, current
    allocate(temp)
    temp%index = index
    temp%name = name
    temp%imagenesPequenas = p
    temp%imagenesGrandes = g
    temp%pasos = pasos
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
end subroutine agregarAtendido




!anadir de forma doblemente enlazada
subroutine addNodeDouble(this, index, name,numeroImagenes)
    class(List_of_lists), intent(inout) :: this
    integer, intent(in) ::index ,numeroImagenes
    character(len=*), intent(in) :: name

    type(node), pointer :: temp, current
    allocate(temp)
    temp%index = index
    temp%name = name
    temp%NumeroImagenes = numeroImagenes
    temp%next => null()
    temp%prev => null()

    if (.not. associated(this%head)) then
        this%head => temp
        print*, 'Nodo agregado'
        print*, 'Ventanilla No. ', index, ' creada'
    else
        current => this%head
        do while(associated(current%next))
            current => current%next
        end do
        current%next => temp
        temp%prev => current
        print*, 'Nodo agregado'
        print*, 'Ventanilla No. ', index, ' creada'
    end if
end subroutine addNodeDouble


    subroutine deleteNode(this, name)
        class(List_of_lists), intent(inout) :: this
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

subroutine deleteNodeDouble(this)
    class(List_of_lists), intent(inout) :: this

    type(node), pointer :: current, previous
    current => this%head
    previous => null()

    do while (associated(current))
        if (current%NumeroImagenes == 0) then
            if(associated(previous)) then
                previous%next => current%next
                if (associated(current%next)) then
                    current%next%prev => previous
                end if
            else
                this%head => current%next
                if (associated(this%head)) then
                    this%head%prev => null()
                end if
            end if

            deallocate(current)
            exit
        end if
            previous => current
            current => current%next
    end do
end subroutine deleteNodeDouble


subroutine agregarClienteAtendido(this,id,nombre,p,g,atendido)
    class(List_of_lists), intent(inout) :: this
    integer, intent(out) :: id
    character(len=*), intent(out) :: nombre
    integer, intent(out) :: p
    integer, intent(out) :: g
    logical, intent(out) :: atendido
    type(node), pointer :: current, previous
    current => this%head
    previous => null()
    
    do while (associated(current))
        if (current%NumeroImagenes == 0) then
            id=current%index
            nombre=current%name
            p=current%imagenesPequenas
            g=current%imagenesGrandes
            atendido = .true.
        end if
            previous => current
            current => current%next
    end do

end subroutine agregarClienteAtendido


    function searchNode(this) result(retval)
        class(List_of_lists), intent(in) :: this

        type(node), pointer :: current
        logical :: retval

        current => this%head
        retval = .false.

        do while(associated(current))
            if(current%EstadoVentanilla .eqv. .false.) then
                retval = .true.
                exit
            end if
            current => current%next
        end do
    end function searchNode

!Agrega imagenes y a su vez tambien elimina la pila de aquel nodo que haya termiando de recibir imagenes
    subroutine agregarImagenes(this)
        class(List_of_lists), intent(in) :: this
        integer :: ig, ip, id
        integer :: cont
        type(node), pointer :: current
        current => this%head


        do while(associated(current))
            if(current%EstadoVentanilla .eqv. .true.) then
                if (current%imagenesGrandes /= 0) then
                    print*, 'Valor de grandes', current%imagenesGrandes 
                    call current%push('Imagenes Grandes')
                    current%imagenesGrandes = current%imagenesGrandes - 1  
                    print*, 'Valor de grandes', current%imagenesGrandes
                    print*, '----------------------------------------------- '
                else  if (current%imagenesPequenas/=0) then
                    print*, 'Valor de pequenas', current%imagenesPequenas
                    call current%push('Imagenes Pequenas')
                    current%imagenesPequenas = current%imagenesPequenas - 1 
                    print*, 'Valor de pequenas', current%imagenesPequenas
                    print*, '----------------------------------------------- '
                else if (current%ocupada == 1) then
                    do cont = 1, current%NumeroImagenes
                        call current%pop()
                    end do
                    current%ocupada = 0
                    current%NumeroImagenes = 0
                    current%imagenesPequenas = 0
                    current%imagenesGrandes = 0
                    print*, 'Ventanilla No. ', current%index, ' vaciada'
                    print*, '----------------------------------------------- '
        !logica para sacar o variar la ventanilla dado caso ya no tenga mas imagenes
                    current%EstadoVentanilla = .false.
                    current%encolar = .true.
                    print*, 'Ventanilla No. ', current%index, ' cerrada'
                    print*, '----------------------------------------------- '
                end if
            end if
            current => current%next
        end do
    end subroutine agregarImagenes



        subroutine agregarImpresion(this,id,nombre,tipo)
            class(List_of_lists), intent(in) :: this
            integer, intent(in) :: id
            character(len=*), intent(in) :: nombre,tipo
            type(node), pointer :: current
            current => this%head
            do while(associated(current))
                if(current%index == id ) then
                    if(current%name == nombre)then
                        call current%push(tipo)
                        current%NumeroImagenes = current%NumeroImagenes - 1
                    end if
                end if
                current => current%next
            end do
        end subroutine agregarImpresion


function cabeza(this) result(retval)
    class(List_of_lists), intent(in) :: this

    type(node), pointer :: current
    logical :: retval

    current => this%head
    retval = .false.

    do while(associated(current))
        if(current%encolar .eqv. .true.) then
            retval = .true.
            exit
        end if
        current => current%next
    end do
end function cabeza

subroutine colar2(this, id, ig, ip, nombre) 
    class(List_of_lists), intent(inout) :: this
    integer, intent(out) :: id
    integer, intent(out) :: ig, ip
    character(len=*), intent(out) :: nombre

    type(node), pointer :: current


    current => this%head


    do while(associated(current))
        if(current%encolar .eqv. .true.) then
            id = current%index
            ig = current%ig
            ip = current%ip
            nombre = current%name
            current%encolar = .false.
            print *, 'El nodo ',current%index,'Con img G=',ig,' e img P=',ip,'  ha sido encolado'
            print*, '----------------------------------------------- ================================='
            exit
        end if
        current => current%next
    end do
end subroutine colar2

    subroutine updateNode(this,id,ip,ig,nombre) 
        class(List_of_lists), intent(in) :: this
        type(node), pointer :: current

        integer, intent(in) :: id, ig, ip
        character(len=*), intent(in) :: nombre

        current => this%head


        do while(associated(current))
            if(current%EstadoVentanilla .eqv. .false.) then
                current%EstadoVentanilla = .true.
                current%NumeroImagenes = ig + ip
                current%imagenesPequenas = ip
                current%imagenesGrandes = ig
                current%name = nombre
                current%index = id
                current%ig = ig
                current%ip = ip
                current%ocupada = 1
                print *, 'El nodo ',current%index,'  ha sido actualizado'
                exit
            end if
            current => current%next
        end do
    end subroutine updateNode


    subroutine pushToNode(this, name, value)
        class(List_of_lists), intent(inout) :: this
        character(len=*), intent(in) :: name, value

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
        class(List_of_lists), intent(in) :: this
        type(node), pointer :: aux

        aux => this%head

        do while(associated(aux))
            print *, 'INDICE: ', aux%index
            print *, 'Nombre: ', aux%name
            print *, 'Estado de la ventanilla: ', aux%EstadoVentanilla
            print *, 'Imagenes Pequenas: ', aux%imagenesPequenas
            print *, 'Imagenes Grandes: ', aux%imagenesGrandes
            call aux%printer()
            print *, ""
            aux => aux%next
        end do
    end subroutine printList

    subroutine printAtendidos(this)
        class(List_of_lists), intent(in) :: this
        type(node), pointer :: aux

        aux => this%head

        do while(associated(aux))
            print *, 'Cliente atendido'
            print *, 'INDICE: ', aux%index
            print *, 'Nombre: ', aux%name
            print *, 'Imagenes Pequenas: ', aux%imagenesPequenas
            print *, 'Imagenes Grandes: ', aux%imagenesGrandes
            print *, 'Pasos Ejecutados', aux%pasos
            print *, ""
            aux => aux%next
        end do
    end subroutine printAtendidos


    subroutine push(this, value)
        class(node), intent(inout) :: this
        character(len=*), intent(in) :: value

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

    subroutine pop(this)
        class(node), intent(inout) :: this
        type(string_node), pointer :: old

        if(associated(this%top)) then
            old => this%top
            this%top => old%next
            deallocate(old)
        else
            print *, "Error: la pila está vacía."
        end if
    end subroutine pop




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

    subroutine delete(this, value)
        class(node), intent(inout) :: this
        character(len=*), intent(in) :: value

        type(string_node), pointer :: current, previous
        current => this%top
        previous => null()

        do while (associated(current) .and. current%value /= value)
            previous => current
            current => current%next
        end do

        if(associated(current) .and. current%value == value) then
            if(associated(previous)) then
                previous%next => current%next
            else
                this%top => current%next
            end if

            deallocate(current)
        end if
    end subroutine delete

    function search(this, value) result(retval)
        class(node), intent(in) :: this
        character(len=*), intent(in) :: value

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
    class(List_of_lists), intent(in) :: this
    type(node), pointer :: current, previous
    integer :: i, j,nvv
    character(len=10) :: id_str, prev_id_str, g, idd, element_str,p,nv

    ! Abre un archivo en formato DOT
    open(unit=10, file='Ventanilla.dot', status='replace', action='write')
    write(10, *) 'digraph G {'
    write(10, *) 'rankdir=LR;'

    current => this%head
    previous => null()
    i = 0
    do while (associated(current))
        write(id_str, '(I10)') i
        write(nv,'(I10)') i+1
        write(g, '(I10)') current%imagenesGrandes
        write(p, '(I10)') current%imagenesPequenas
        write(idd, '(I10)') current%index

        if(current%imagenesGrandes == 0 .AND. current%imagenesPequenas==0) then
            if(current%ocupada == 0 )then
                write(10, *) 'node' // trim(adjustl(id_str)) // ' [label="Ventanilla: ' // trim(adjustl(nv)) // & 
                & '\n''Ventanilla Descoupada", color="red", shape="rectangle"];'
            else 
                write(10, *) 'node' // trim(adjustl(id_str)) // ' [label="Ventanilla: ' // trim(adjustl(nv)) // &
                    & '\n''Id: ' // trim(adjustl(idd))  //&
                    & '\n''Cliente: ' // trim(adjustl(current%name)) // '\n''Imagenes Grandes: ' // trim(adjustl(g)) //&
                    & '\n''Imagenes Pequenas: ' // trim(adjustl(p)) //'", color="red", shape="rectangle"];'
            end if
            
        else
            write(10, *) 'node' // trim(adjustl(id_str)) // ' [label="Ventanilla: ' // trim(adjustl(nv)) // &
                & '\n''Id: ' // trim(adjustl(idd))  //&
                & '\n''Cliente: ' // trim(adjustl(current%name)) // '\n''Imagenes Grandes: ' // trim(adjustl(g)) //&
                & '\n''Imagenes Pequenas: ' // trim(adjustl(p)) //'", color="red", shape="rectangle"];'
        end if

        

        call agregarSublista(current, i)

        if (associated(previous)) then
            write(prev_id_str, '(I10)') (i-1)
            write(10, *) 'node' // trim(adjustl(prev_id_str)) // ' -> node' // trim(adjustl(id_str)) // ' [dir="forward"];'
        end if
        previous => current
        current => current%next
        i = i + 1
    end do

    write(10, *) '}'
    close(10)

    ! Ejecuta el comando para generar la imagen
    call system("dot -Tpng Ventanilla.dot -o Ventanilla.png")
end subroutine graphV


subroutine agregarSublista(this, parent_id)
    class(node), intent(in) :: this
    integer, intent(in) :: parent_id
    type(string_node), pointer :: aux, prev_aux
    integer :: j
    character(len=10) :: id_str, parent_id_str, prev_id_str, node_label

    aux => this%top
    prev_aux => null()
    j = 0
    do while(associated(aux))
        write(id_str, '(I10)') j
        write(parent_id_str, '(I10)') parent_id
        if (j == 0) then
            node_label = trim(adjustl(aux%value))
            write(10, *) 'element' // trim(adjustl(parent_id_str)) // trim(adjustl(id_str)) // &
            & ' [label="' // node_label // '", shape="ellipse"];'
            write(10, *) 'node' // trim(adjustl(parent_id_str)) // ' -> element' // &
            & trim(adjustl(parent_id_str)) // trim(adjustl(id_str)) // ' [dir="forward"];'
        else
            write(prev_id_str, '(I10)') (j-1)
            write(id_str, '(I10)') j
            write(10, *) 'element' // trim(adjustl(parent_id_str)) // trim(adjustl(prev_id_str)) // ' -> element' // &
            & trim(adjustl(parent_id_str)) // trim(adjustl(id_str)) // ' [dir="forward"];'
            node_label = trim(adjustl(aux%value))
            write(10, *) 'element' // trim(adjustl(parent_id_str)) // trim(adjustl(id_str)) // &
            & ' [label="' // node_label // '", shape="ellipse"];'
        end if
        prev_aux => aux
        aux => aux%next
        j = j + 1
    end do
end subroutine agregarSublista


end module listaVentanilla
!=======================================================================================================================================
!Modulo para cola de impresiones=========================================================================================================

module colaImpresiones
    implicit none
    private
!Nodo de la cola
    type :: nodeI
        integer :: id
        character(:), allocatable :: nombre
        integer :: peso
        character(:), allocatable :: tipo
        type(nodeI), pointer :: next => null()
    end type nodeI

    type, public :: cola_Impresion
        private
        type(nodeI), pointer :: front => null()
        type(nodeI), pointer :: rear => null()
    contains
        procedure :: enqueue
        procedure :: dequeue
        procedure :: bajarUnidad
        procedure :: verificarImpresion
        procedure :: obtenerDatos
        procedure :: graphP
        procedure :: graphG 
        procedure :: print
    end type cola_Impresion    

contains
    
!Encola un cliente
 subroutine enqueue(this, id, nombre, tipo, peso)
    class(cola_Impresion), intent(inout) :: this
    integer, intent(in) :: id  ! Change here
    character(len=*), intent(in) :: nombre
    character(len=*), intent(in) :: tipo
    integer, intent(in) :: peso

    type(nodeI), pointer :: temp

    allocate(temp)
    temp%id = id
    temp%nombre = nombre
    temp%tipo = tipo
    temp%peso = peso
    temp%next => null()

    if (.not. associated(this%front)) then
        this%front => temp
        this%rear => temp
    else
        this%rear%next => temp
        this%rear => temp
    end if
end subroutine enqueue


 subroutine dequeue(this)
        class(cola_Impresion), intent(inout) :: this
        type(nodeI), pointer :: temp

        if (associated(this%front)) then
            temp => this%front

            this%front => this%front%next
            deallocate(temp)
        else
            print *, 'La cola está vacía.'
        end if
    end subroutine dequeue

subroutine obtenerDatos(this,id,nombre)
    class(cola_Impresion), intent(in) :: this
    type(nodeI), pointer :: current
    integer, intent(out) :: id
    character(len=*), intent(out) :: nombre
    !integer, intent(out) :: peso
    current => this%front
    if (associated(this%front)) then
        print*, '=========================='
        print*, 'Imagen saliente'
        print*, 'Id:', current%id
        print*, 'Nombre:', current%nombre
        print*, 'Peso:', current%peso
        print*, '=========================='
        id = current%id
        nombre = current%nombre
    else
        print *, 'La cola está vacía.'
    end if
    print*, '==========================' 

end subroutine obtenerDatos


subroutine bajarUnidad(this)
        class(cola_Impresion), intent(in) :: this
        type(nodeI), pointer :: current
        if (associated(this%front)) then
            current => this%front
            current%peso = current%peso - 1
        else
            print *, 'La cola está vacía.'
        end if

end subroutine bajarUnidad

subroutine verificarImpresion(this, desicion)
        class(cola_Impresion), intent(in) :: this
        type(nodeI), pointer :: current
        logical, intent(out) :: desicion

        current => this%front
        if (associated(this%front)) then
            if (current%peso == 0) then
                desicion = .true.
            else
                desicion = .false.
            end if

        else
            print *, 'La cola está vacía.'
        end if

end subroutine verificarImpresion

!Imprime la cola
    subroutine print(this)
        class(cola_Impresion), intent(in) :: this
        type(nodeI), pointer :: current

        current => this%front

        do while (associated(current))
            print *,'=========================='
            print *,'Id:', current%id
            print*, 'Nombre: ', current%nombre
            print*, "Peso", current%peso
            print *,'=========================='
            current => current%next
        end do
    end subroutine print


!Agrega esta función al módulo colaClientes
subroutine graphP(this)
    class(cola_Impresion), intent(in) :: this
    type(nodeI), pointer :: current, previous
    integer :: i
    character(len=10) :: id_str, prev_id_str, g,idd

    ! Abre un archivo en formato DOT
    open(unit=10, file='pequena.dot', status='replace', action='write')
    write(10, *) 'digraph G {'
    write(10, *) 'rankdir=LR;'

    current => this%front
    previous => null()
    i = 0
    do while (associated(current))
         write(id_str, '(I10)') i
        write(g, '(I10)') current%peso
        write(idd, '(I10)') current%id


        write(10, *) 'node' // trim(adjustl(id_str)) // ' [label="Cliente: ' // trim(adjustl(current%nombre)) // &
     & '\n''Id: ' // trim(adjustl(idd)) // '\n''Peso: ' // trim(adjustl(g)) // '", color="red", shape="rectangle"];'

        if (associated(previous)) then
            write(prev_id_str, '(I10)') (i-1)
            write(10, *) 'node' // trim(adjustl(prev_id_str)) // ' -> node' // trim(adjustl(id_str)) // ' [dir="forward"];'
        end if
        previous => current
        current => current%next
        i = i + 1
    end do

    write(10, *) '}'
    close(10)

    ! Ejecuta el comando para generar la imagen
    call system("dot -Tpng pequena.dot -o pequena.png")
end subroutine graphP
subroutine graphG(this)
    class(cola_Impresion), intent(in) :: this
    type(nodeI), pointer :: current, previous
    integer :: i
    character(len=10) :: id_str, prev_id_str, g,idd

    ! Abre un archivo en formato DOT
    open(unit=10, file='grande.dot', status='replace', action='write')
    write(10, *) 'digraph G {'
    write(10, *) 'rankdir=LR;'

    current => this%front
    previous => null()
    i = 0
    do while (associated(current))
        write(id_str, '(I10)') i
        write(g, '(I10)') current%peso
        write(idd, '(I10)') current%id


        write(10, *) 'node' // trim(adjustl(id_str)) // ' [label="Cliente: ' // trim(adjustl(current%nombre)) // &
     & '\n''Id: ' // trim(adjustl(idd)) // '\n''Peso: ' // trim(adjustl(g)) // '", color="red", shape="rectangle"];'

        if (associated(previous)) then
            write(prev_id_str, '(I10)') (i-1)
            write(10, *) 'node' // trim(adjustl(prev_id_str)) // ' -> node' // trim(adjustl(id_str)) // ' [dir="forward"];'
        end if
        previous => current
        current => current%next
        i = i + 1
    end do

    write(10, *) '}'
    close(10)

    ! Ejecuta el comando para generar la imagen
    call system("dot -Tpng grande.dot -o grande.png")
end subroutine graphG

end module colaImpresiones

    


!=======================================================================================================================================


! Modulo Datos Personales===============================================================================================================
module datosPersonales
    implicit none
contains
    subroutine print_datosPersonales
        print*, ''
        print*, '============================================'
        print *, '            Acerca de...                   '
        print *, 'Estudiante:   Franklin Orlando Noj Perez'
        print *, 'Carnet:       202200089'
        print *, 'Curso:        Estructura de Datos'
        print *, 'Seccion:      B'
        print *, 'Carrera:      Ingenieria en Ciencias y Sistemas'
        print *, 'Universidad:  Universidad de San Carlos de Guatemala'
        print*, '============================================'
        print*, ''
    end subroutine print_datosPersonales
end module datosPersonales
!=======================================================================================================================================


!Menu del proyecto
program Proyecto_202200089
    use datosPersonales
    use colaClientes
    use listaVentanilla
    use colaImpresiones
    use iso_fortran_env, only: 
    

    implicit none
    logical::cabeza,encolarImpresion,verificacion
    type(List_of_lists) :: atendidos
    type(List_of_lists) :: list_Ventanilla
    type(List_of_lists)::listaEspera
    type(queue) :: colaVentanilla   
    type(cola_Impresion)::impresionesPequenas
    type(cola_Impresion) :: impresionesGrandes

    integer :: idAI
    character(len=100) :: nombreAI

    logical :: atendido
    integer :: idAtendido,gAtendido,pAtendido
    character :: nombreAtendido

    integer :: opcion, num1, num2,i
    integer :: id, imagenesPequenas, imagenesGrandes,paso
    integer :: idC,igC,ipC,peso
    character(len=50):: nombreC
    character(len=50) :: nombre,tipoC

    character(len=100) :: imgCliente
    integer :: imgPeso
    character(len=100) :: imgTipo
    integer :: imgId,g,p

    logical :: descp,descg
    descp   = .false.
    descg   = .false.


    verificacion = .false.
    paso = 0
    do  
        print*, ''
        print*, '============================================'
        print *, 'Bienvenido al sistema de gestion de clientes'
        print *, '1. Parametros iniciales'
        print *, '2. Ejecutar Paso'
        print *, '3. Estado de memoria de las estructuras'
        print *, '4. Reportes'
        print *, '5. Acerca de...'
        print *, '6. Salir'
        print *, 'Por favor, elige una opcion: '
        print*, '============================================'
        print*, '||||||||||||||||||||||||||||||||||||||||||||'
        print*, '============================================'
        print*, ''
        read *, opcion

        select case (opcion)
            case (1)

    !Se cargan los clientes a la cola
                print *, 'Ingrese la cantidad de clientes para encolar'
                read *, num1

                do i = 1, num1
                if (i==1) then
                                                !id,nombre,pequenas,grandes
                                                !Numero, no peso
                    call colaVentanilla%enqueue(2020, 'Cliente ', 2, 2)
                else 
                    call colaVentanilla%enqueue(i, 'Cliente ', 1, 1)
                end if
                end do

                print*, 'Clientes encolados'
                call colaVentanilla%print()
                print*, '----------------------------------------------- '

    !Se setea la cantidad de ventanillas que estaran funcionando
                print *, 'Ingrese la cantidad de ventanillas que existiran'
                read *, num2
                do i = 1, num2
                    call list_Ventanilla%addNode(i, 'Ventanilla ')
                end do
                call list_Ventanilla%printList()
                print*, '----------------------------------------------- '    
            case (2)
!case2
                paso = paso + 1
                print*, 'Paso No. ', paso
                !Ejecutar paso
    !Apila imagenes en ventanilla si se pudiera
                call list_Ventanilla%agregarImagenes()
                print*, '----------------------------------------------- '
                print*, 'Para visualizacion de las imagenes agregadas a las ventanillas:'
                call list_Ventanilla%printList()
                print*, '----------------------------------------------- '


!Si tengo tengo clientes con todas sus imagenes recibidas los saco de la lista de espera
!===========================================================================================================================
                !Aca agrego ya los clientes atendidos
                call listaEspera%agregarClienteAtendido(idAtendido,nombreAtendido,gAtendido,pAtendido,atendido)
                if (atendido) then
                    call atendidos%agregarAtendido(idAtendido,nombreAtendido,pAtendido,gAtendido,paso)
                    atendido = .false.
                end if
                !Aca verifico numero de imagenes de la lista de espera si encuntra n clientes satisfechos salen de la lista de espera
                call listaEspera%deleteNodeDouble()
!Por logica desencolo primero, luego encolo
                !Aca bajo una unidad a las dos colas o tipos de impresion
                !Neccesito crear la subrutina
                call impresionesPequenas%bajarUnidad()
                call impresionesGrandes%bajarUnidad()

                call impresionesPequenas%verificarImpresion(descp)
                call impresionesGrandes%verificarImpresion(descg)

                !las imagenes que voy sacando las voy agregando ami lista de espera
                if (descp) then
                    print*,'Imagen pequena impresa'
                    call impresionesPequenas%obtenerDatos(idAI,nombreAI)
                    call listaEspera%agregarImpresion(idAI,nombreAI,'pequena')
                    call impresionesPequenas%dequeue()
                end if
                if (descg) then
                    print*,'Imagen grande impresa'
                    call impresionesGrandes%obtenerDatos(idAI,nombreAI)
                    call listaEspera%agregarImpresion(idAI,nombreAI,'grande')
                    call impresionesGrandes%dequeue()
                end if

                !Aca reviso las ventanillas listas para mandar a encolar imagenes
                !Aca salen de ventanillas a colas de impresion
                !Aca es donde se reservan los lugares para la lista de clientes en espera
                do
                    
                    cabeza =  list_Ventanilla%cabeza()
                    print*, 'Valor de cabeza',cabeza
                    if (.not. cabeza) exit
                        call list_Ventanilla%colar2(idC,igC,ipC,nombreC)
                        print*, 'Id',idC
                        print*, 'Nombre',nombreC
                        print*, 'Imagenes Grandes',igC
                        print*, 'Imagenes Pequenas',ipC
                        print*, '----------------------------------------------- '
                        print*, 'Visualizacion de imagenes agregadas a cola de impresion'
                        call listaEspera%addNodeDouble(idC, nombreC, igC+ipC)
                        !Aca puedo agregar el nodo a lista doble enlazada de clientes en espera

                        if (igC > 0) then
                            do g = 1, igC
                                print*, 'Imagen Grande'
                                call impresionesGrandes%enqueue(idC, nombreC,'Grande',2)
                                igC=0
                            end do
                        end if
                        if (ipC > 0) then
                            do p = 1, ipC
                                print*, 'Imagen Pequena'
                                call impresionesPequenas%enqueue(idC, nombreC,'Pequena',1)
                                ipC=0
                            end do
                    end if             
                end do

                print*, 'Cola impresiones Pequenas'
                call impresionesPequenas%print()
                print*, 'Cola impresiones Grandes'
                call impresionesGrandes%print() 

                print*, '----------------------------------------------- '
                print*, '----------------------------------------------- '
                print*, '----------------------------------------------- '
    !Revisa si hay ventanillas disponibles para poder pasar a atender a un cliente
        !Como es una cola no puede entrar mas de 1 a la vez
        !        do while(.true.)
        !            if(.not. list_Ventanilla%searchNode()) then
        !                print *, 'No hay ventanillas disponibles'
        !                exit
        !            else
        !                print*, 'Visualizacion de la ventanilla que fue apartada'
        !                call colaVentanilla%dequeue(id, imagenesPequenas, imagenesGrandes, nombre)
        !                call list_Ventanilla%updateNode(id, imagenesGrandes, imagenesPequenas, nombre)
        !                call list_Ventanilla%printList()
        !            end if
        !        end do  

                if(.not. list_Ventanilla%searchNode()) then
                    print *, 'No hay ventanillas disponibles'
                else
                    print*, 'Visualizacion de la ventanilla que fue apartada'
                    call colaVentanilla%dequeue(id, imagenesPequenas, imagenesGrandes, nombre)
                    call list_Ventanilla%updateNode(id,imagenesPequenas , imagenesGrandes, nombre)
                    call list_Ventanilla%printList()
                end if
            case (3)
                print *, 'Estado de memoria de las estructuras'
                call colaVentanilla%graph()
                call impresionesPequenas%graphP()
                call impresionesGrandes%graphG()
                print*, 'cola pequenas'
                call impresionesPequenas%print()
                print*, 'cola grandes'
                call impresionesGrandes%print()

                call list_Ventanilla%graphV()

            case (4)
                print *, 'Reportes'
                print*, 'Impresion de lista de espera para mientras'
                call listaEspera%printList()
                print*, 'Impresion de lista de atendidos'
                call atendidos%printAtendidos()
            case (5)
                call print_datosPersonales
            case (6)    
                print *, 'Salir'
                exit
            case default
                print *, 'Opcion no valida. Por favor, elige una opcion del 1 al 5.'
        end select
    end do

end program Proyecto_202200089