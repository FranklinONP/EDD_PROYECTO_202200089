!Modulo para cola de clientes=========================================================================================================
! Este modulo implementa una cola de clientes
module colaClientes
    implicit none
    private
!Nodo de la cola
    type :: node
        integer :: id
        character(:), allocatable :: nombre
        integer :: imagenesPequenas
        integer :: imagenesGrandes
        type(node), pointer :: next => null()
    end type node

    type, public :: queue
        private
        type(node), pointer :: front => null()
        type(node), pointer :: rear => null()
    contains
        procedure :: enqueue
        procedure :: dequeue
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

            type(node), pointer :: temp

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


 subroutine dequeue(this, id, ig, ip, nombre)
        class(queue), intent(inout) :: this
        integer, intent(out) :: id
        integer, intent(out) :: ig
        integer, intent(out) :: ip
        character(len=*), intent(out) :: nombre
        type(node), pointer :: temp

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
        type(node), pointer :: current

        current => this%front

        do while (associated(current))
            print *,'Id:', current%id
            print*, 'Nombre: ', current%nombre
            print*, "#Imagenes Pequenas", current%imagenesPequenas
            print*, "#Imagenes Grandes" , current%imagenesGrandes
            current => current%next
        end do
    end subroutine print



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
        integer :: NumeroImagenes
        integer :: imagenesPequenas
        integer :: imagenesGrandes

        character(:), allocatable :: name
        type(node), pointer :: next => null()
        type(string_node), pointer :: top => null()
    contains
        procedure :: push
        procedure :: printer
        procedure :: delete
        procedure :: search
    end type node

    !objeto de lista de lista
    type, public :: List_of_lists
        type(node), pointer :: head => null()
    contains
        procedure :: addNode
        procedure :: pushToNode
        procedure :: printList
        procedure :: deleteNode
        procedure :: searchNode
        procedure :: updateNode
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
    else
        current => this%head
        do while(associated(current%next))
            current => current%next
        end do
        current%next => temp
    end if
end subroutine addNode



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

    subroutine updateNode(this,id,ig,ip,nombre) 
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
            call aux%printer()
            print *, ""
            aux => aux%next
        end do
    end subroutine printList

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

end module listaVentanilla
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

    implicit none

    type(List_of_lists) :: list_Ventanilla
    type(queue) :: colaVentanilla
    integer :: opcion, num1, num2,i
    integer :: id, imagenesPequenas, imagenesGrandes
    character(len=50) :: nombre

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
        print*, ''
        read *, opcion

        select case (opcion)
            case (1)

    !Se cargan los clientes a la cola
                print *, 'Ingrese la cantidad de clientes para encolar'
                read *, num1

                do i = 1, num1
                    call colaVentanilla%enqueue(i, 'Cliente ', 1, 2)
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
                !Ejecutar paso
    !Primero revisa si hay ventanillas disponibles para poder pasar a atender a un cliente
                if(.not. list_Ventanilla%searchNode()) then
                    print *, 'No hay ventanillas disponibles'
                else
                    call colaVentanilla%dequeue(id, imagenesPequenas, imagenesGrandes, nombre)
                    call list_Ventanilla%updateNode(id, imagenesGrandes, imagenesPequenas, nombre)
                    print*, '----------------------------------------------- '
                    print*, 'id: ', id
                    print*, 'Cliente atendido',nombre
                    print*, 'Imagenes Pequenas: ', imagenesPequenas
                    print*, 'Imagenes Grandes: ', imagenesGrandes   
                    print*, '----------------------------------------------- '
                    call list_Ventanilla%printList()
                end if

            case (3)
                print *, 'Estado de memoria de las estructuras'
            case (4)
                print *, 'Reportes'
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
