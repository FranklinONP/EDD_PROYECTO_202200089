! Modulo Lista Simple Enlazada=========================================================================================================
! Este modulo implementa una lista simple enlazada
module listaSimpleEnlazada
    implicit none
    private

    type, public ::node
        private
        integer :: value
        type(node), pointer :: next     
    end type node

    type, public :: listaSimple
        private
        type(node), pointer :: head => null()
    contains
        procedure :: push
        procedure :: append
        procedure :: print
        procedure :: delete
        procedure :: search
    end type listaSimple

contains

    subroutine push(this, value)
        class(listaSimple), intent(inout) :: this
        integer, intent(in) :: value

        type(node), pointer :: temp
        allocate(temp)
        temp%value = value
        temp%next => null()

        if (.not. associated(this%head)) then
            this%head => temp
        else
            temp%next => this%head
            this%head => temp
        end if

        print *, 'pushed ', value
    end subroutine push

    subroutine append(this, value)
        class(listaSimple), intent(inout) :: this
        integer, intent(in) :: value

        type(node), pointer :: temp
        type(node), pointer :: current

        allocate(temp)
        temp%value = value
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

    subroutine delete(this, value)
        class(listaSimple), intent(inout) :: this
        integer, intent(in) :: value
        type(node), pointer :: current, previous

        current => this%head
        previous => null()

        ! Buscar el nodo a eliminar
        do while (associated(current) .and. current%value /= value)
            previous => current
            current => current%next
        end do

        ! Si se encontró el nodo
        if(associated(current) .and. current%value == value) then
            if(associated(previous)) then
                previous%next => current%next
            else
                this%head => current%next
            end if

            deallocate(current)
            print *, 'Se eliminó el valor ', value
        else
            print *, 'No se encontró el valor ', value
        end if

    end subroutine delete

    function search(this, value) result(retval)
        class(listaSimple), intent(in) :: this
        integer, intent(in) :: value

        type(node), pointer :: current

        logical :: retval

        current => this%head
        retval = .false.

        do while(associated(current))
            if(current%value == value) then
                retval = .true.
                exit
            end if
            current => current%next
        end do

    end function search

    subroutine print(this)
        class(listaSimple), intent(in) :: this
        type(node), pointer :: current

        current => this%head

        do while (associated(current))
            print *, current%value
            current => current%next
        end do 
    end subroutine print
    
end module listaSimpleEnlazada
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
    use listaSimpleEnlazada

    implicit none

    type(listaSimple) :: list
    integer :: opcion, num1, num2

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
                print *, 'Ingrese la direccion del archivo para la carga masiva de clientes'
                read *, num1
                print *, 'Ingrese la cantidad de ventanillas que existiran'
                read *, num2
                print *, 'Carga de datos desde la direccion ',num1
                print *, 'La cantidad de ventanillas seran de: ', num2
            case (2)
                print *, 'Ejecutar paso'
            case (3)
                print *, 'Estado de memoria de las estructuras'
            case (4)

                    call list%append(1)
                    call list%append(2)
                    call list%append(3)
                    call list%append(4)
                    call list%append(5)

                    print *, '//-----------------//'
                    print *, 'La lista es:'
                    print *, '//-----------------//'
                    call list%print()

                    print *, '//-----------------//'
                    if(list%search(1)) then
                        print *, 'El valor 1 esta en la lista'
                    else
                        print *, 'El valor 1 no esta en la lista'
                    end if
                    call list%delete(1)
                    if(list%search(1)) then
                        print *, 'El valor 1 esta en la lista'
                    else
                        print *, 'El valor 1 no esta en la lista'
                    end if

                    print *, '//-----------------//'
                    print *, 'La lista es:'
                    print *, '//-----------------//'
                    call list%print()
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
