! Modulo Lista Simple Enlazada=========================================================================================================
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

    implicit none
    integer :: opcion, nVentanillas,i
    type(listaSimple) :: list

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
                
                print *, 'Ingrese el numero de ventanillas: '
                read *, nVentanillas
                do i = 1, nVentanillas
                    call list%append(i)
                end do
                print *, 'Se han reservado ', nVentanillas, ' ventanillas'
            case (2)
                !Ejecutar paso
                if (list%searchVentanilla()) then
                    call list%updateVentanilla()
                else
                    print *, 'No hay ventanillas disponibles'
                end if
                call list%print()
            case (3)
                exit
        end select
    end do

end program Proyecto_202200089