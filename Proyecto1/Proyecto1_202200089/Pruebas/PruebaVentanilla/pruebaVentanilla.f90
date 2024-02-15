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




    
!Desencola un cliente
    subroutine dequeue(this)
        class(queue), intent(inout) :: this
        type(node), pointer :: temp

        if (associated(this%front)) then
            temp => this%front
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


! Modulo Lista Simple Enlazada=========================================================================================================
! Lista para ventanilla
! Este modulo implementa una lista simple enlazada
module listaSimpleEnlazada
    implicit none
    private
!Nodo de la lista
    type, public :: node
        private
        integer :: numeroVentanilla
        logical :: EstadoVentanilla = .false.
        integer :: NumeroImagenes
        integer :: imagenesPequenas
        integer :: imagenesGrandes
        character(:), allocatable :: name
        type(node), pointer :: next => null()
    end type node

!Lista que guardara los nodos
    type, public :: listaSimple
        private
        type(node), pointer :: head => null()
    contains
        procedure :: append
        procedure :: searchVentanilla
        procedure :: updateVentanilla
        procedure :: print

    end type listaSimple

contains
!Append me va a servir para guardar o reservar los espacios que simularan las n ventanillas que el cliente ingrese
    subroutine append(this, value)
        class(listaSimple), intent(inout) :: this
        integer, intent(in) :: value

        type(node), pointer :: temp
        type(node), pointer :: current

        allocate(temp)
        temp%numeroVentanilla = value
        temp%next => null()

        if (.not. associated(this%head)) then
            this%head => temp
        else
            current => this%head
            do while (associated(current%next))
                current => current%next
            end do
            current%next => temp
        end if

        print *, 'appended ', value
    end subroutine append
!Verifica si existe una ventanilla disponible       
    function searchVentanilla(this) result(retval)
        class(listaSimple), intent(in) :: this

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

    end function searchVentanilla

!Busca la primer ventanilla desocupada o libre para asignarle un cliente (True)
subroutine updateVentanilla(this)
    class(listaSimple), intent(inout) :: this

    type(node), pointer :: current

    if (associated(this%head)) then
        current => this%head
        do while (associated(current))
            if (current%EstadoVentanilla .eqv. .false.) then
                current%EstadoVentanilla = .true.
                print *, 'El nodo ',current%numeroVentanilla,'  ha sido actualizado'
                exit
            end if
            current => current%next
        end do
    else
        print *, 'La lista está vacía.'
    end if
end subroutine updateVentanilla


    subroutine print(this)
        class(listaSimple), intent(in) :: this
        type(node), pointer :: current

        current => this%head

        do while (associated(current))
            print *, current%numeroVentanilla
            print *, current%EstadoVentanilla
            current => current%next
        end do 
    end subroutine print
    
end module listaSimpleEnlazada
!=======================================================================================================================================
!Menu del proyecto
program Proyecto_202200089
    use listaSimpleEnlazada
    use colaClientes

    implicit none
    integer :: opcion, nVentanillas,i
    type(listaSimple) :: list
    type(queue) :: colaVentanilla

    do  
        print*, ''
        print*, '============================================'
        print *, 'Bienvenido al sistema de gestion de clientes'
        print *, '1. Reserve numero de Ventanillas'
        print *, '2. Ejecutra Paso'
        print *, '3. Salir'
        print *, 'Por favor, elige una opcion: '
        print*, '============================================'
        print*, ''
        read *, opcion

        select case (opcion)
            case (1)
                call colaVentanilla%enqueue(1, 'Cliente 1', 5, 10)
                call colaVentanilla%enqueue(2, 'Cliente 2', 5, 10)
                call colaVentanilla%enqueue(3, 'Cliente 3', 5, 10)
                call colaVentanilla%enqueue(4, 'Cliente 4', 5, 10)
                call colaVentanilla%enqueue(5, 'Cliente 5', 5, 10)
                print *, ' ==========================================  '
                print *, 'Se imprime la cola'
                call colaVentanilla%print()
                print *, ' ==========================================  '

                print *, 'Se han reservado ', nVentanillas, ' ventanillas'
            case (2)
                !Ejecutar paso

                !Paso para que el cliente de la cola pase a la ventanilla X que este disponible en el paso a ejecutarse
                if (list%searchVentanilla()) then
                    call list%updateVentanilla()
                    call colaVentanilla%dequeue()
                else
                    print *, 'No hay ventanillas disponibles'
                end if
                call list%print()

                !Paso para recibir imagenes en la pila por paso en cada nodo de la ventanilla que este ocupada/atendiendo

                
            case (3)
                exit
        end select
    end do

end program Proyecto_202200089