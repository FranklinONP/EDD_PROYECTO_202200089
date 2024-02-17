!Pila de impresiones=========================================================================================================
module pilaImpresion
    implicit none
    private
!Nodo de la cola
    type :: node
        character(:), allocatable :: nombreCliente
        character(:), allocatable :: tipoImpresion
        integer :: pesoImagen=0
        type(node), pointer :: next => null()
    end type node

    type, public :: pila
        private
        type(node), pointer :: front => null()
        type(node), pointer :: rear => null()
    contains
        procedure :: push
        procedure :: pop
        procedure :: EjecutarPaso
        procedure :: print
    end type pila     

contains
!Apilar imagen
subroutine push(this, nombreCliente, tipoImpresion, pesoImagen)
    class(pila), intent(inout) :: this
    character(len=*), intent(in) :: nombreCliente,tipoImpresion
    integer, intent(in) :: pesoImagen

    type(node), pointer :: temp

    allocate(temp)
    temp%nombreCliente = nombreCliente
    temp%tipoImpresion = tipoImpresion
    temp%pesoImagen = pesoImagen-1
    temp%next => null()

    if (.not. associated(this%front)) then
        this%front => temp
        this%rear => temp
    else
        this%rear%next => temp
        this%rear => temp
    end if
end subroutine push


!Desapila una impresion
subroutine pop(this)
    class(pila), intent(inout) :: this
    type(node), pointer :: temp

    if (associated(this%front)) then
        temp => this%front
        this%front => this%front%next
        deallocate(temp)
    else
        print *, 'La cola está vacía.'
    end if
end subroutine pop

subroutine EjecutarPaso(this)
    class(pila), intent(inout) :: this
    type(node), pointer :: temp,temp2

    temp => this%front

    if (associated(temp)) then
        if (temp%pesoImagen > 0) then
            if(temp%tipoImpresion == "pequena") then
                temp%pesoImagen = temp%pesoImagen - 1
            else if (temp%tipoImpresion == "grande") then
                temp%pesoImagen = temp%pesoImagen - 1
            end if
        else
        print*, '||||||||||||||||||||||||||||||||||||||||||||||||||'
            print*, 'Aca mando el nodo para la lista de espera'
            print*, 'Nombre: ', temp%nombreCliente
            print*, "Tipo imagen", temp%tipoImpresion
            print*, "Peso imagen" , temp%pesoImagen
        print*, '||||||||||||||||||||||||||||||||||||||||||||||||||'
            temp2 => this%front
            this%front => this%front%next
        end if
    else
        print *, 'La pila está vacía.'
    end if
end subroutine EjecutarPaso

!Imprime la cola
    subroutine print(this)
        class(pila), intent(in) :: this
        type(node), pointer :: current

        current => this%front

        do while (associated(current))
            print*, 'Nombre: ', current%nombreCliente
            print*, "Tipo imagen", current%tipoImpresion
            print*, "Peso imagen" , current%pesoImagen
            current => current%next
        end do
    end subroutine print
end module pilaImpresion  

program pilaaa
    use pilaImpresion   
    type(pila) :: pil
        call pil%push("Juan","pequena",1)
        call pil%push("Pedro","pequena",1)
        call pil%push("Maria","pequena",1)
        print*, "Pila de impresiones"
        call pil%print()
        print*, "============================"
        print*, "Ejecutando paso 1"
        print*, "============================"
        call pil%EjecutarPaso()
        call pil%print() 
        print*, "============================"
        print*, "Ejecutando paso 2"
        print*, "============================"
        call pil%EjecutarPaso()
        call pil%print()
        print*, "============================"
        print*, "Ejecutando paso 3"
        print*, "============================"
        call pil%EjecutarPaso()
        call pil%print()
        print*, "============================"
        print*, "Ejecutando paso 4"
        print*, "============================"
        call pil%EjecutarPaso()
        call pil%print()


end program pilaaa