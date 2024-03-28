module avl_m
    use abb_m
    use matrix_m
    use uuid_module
    implicit none
    private

    type :: nodo
        integer :: valor
        integer :: altura = 1
        type(abb) :: abb
        type(matrix) :: mtx
        logical :: cargada = .false.
        type(nodo), pointer :: derecha => null()
        type(nodo), pointer :: izquierda => null() 
    end type

    type, public :: avl
        type(nodo), pointer :: raiz => null()
    
contains
        procedure :: insert
        procedure :: delete
        procedure :: preorden
        procedure :: graficar
        procedure :: search
        procedure :: abbImagen
        procedure :: crearImagen
        procedure :: imagenPreorden
        procedure :: imagenInorden
        procedure :: imagenPostOrden
        procedure :: validarID
        procedure :: cadena
        procedure :: extraccion
        procedure :: top_5_imagenes

    end type  avl
contains

subroutine top_5_imagenes(self)
        class(avl), intent(inout) :: self
        integer :: max1, max2, max3, max4, max5
        integer :: id1, id2, id3, id4, id5
        max1 = 0
        max2 = 0
        max3 = 0
        max4 = 0
        max5 = 0
        id1 = 0
        id2 = 0
        id3 = 0
        id4 = 0
        id5 = 0
        call buscar_top_5(self%raiz, max1, max2, max3, max4, max5, id1, id2, id3, id4, id5)
        print *, "Top 5 de imágenes con más número de capas:"
        print *, "ID imagen:", id1, "Numero de capas:", max1
        print *, "ID imagen:", id2, "Número de capas:", max2
        print *, "ID imagen:", id3, "Número de capas:", max3
        print *, "ID imagen:", id4, "Número de capas:", max4
        print *, "ID imagen:", id5, "Número de capas:", max5
    end subroutine top_5_imagenes
    
    recursive subroutine buscar_top_5(raiz, max1, max2, max3, max4, max5, id1, id2, id3, id4, id5)
        type(nodo), pointer, intent(in) :: raiz
        integer, intent(inout) :: max1, max2, max3, max4, max5
        integer, intent(inout) :: id1, id2, id3, id4, id5
        integer :: num_nodos
        if (.not. associated(raiz)) return
        num_nodos = raiz%abb%numero_nodos()
        if (num_nodos > max1) then
            max5 = max4
            id5 = id4
            max4 = max3
            id4 = id3
            max3 = max2
            id3 = id2
            max2 = max1
            id2 = id1
            max1 = num_nodos
            id1 = raiz%valor
        else if (num_nodos > max2) then
            max5 = max4
            id5 = id4
            max4 = max3
            id4 = id3
            max3 = max2
            id3 = id2
            max2 = num_nodos
            id2 = raiz%valor
        else if (num_nodos > max3) then
            max5 = max4
            id5 = id4
            max4 = max3
            id4 = id3
            max3 = num_nodos
            id3 = raiz%valor
        else if (num_nodos > max4) then
            max5 = max4
            id5 = id4
            max4 = num_nodos
            id4 = raiz%valor
        else if (num_nodos > max5) then
            max5 = num_nodos
            id5 = raiz%valor
        end if
        call buscar_top_5(raiz%izquierda, max1, max2, max3, max4, max5, id1, id2, id3, id4, id5)
        call buscar_top_5(raiz%derecha, max1, max2, max3, max4, max5, id1, id2, id3, id4, id5)
end subroutine buscar_top_5

subroutine extraccion(self, val, idCapa, mtx)
    class(avl), intent(inout) :: self
    integer, intent(in) :: val, idCapa
    type(matrix), intent(inout) :: mtx
    type(matrix) :: mtx2
    print*, 'Extrayendo imagen:', val
    call extraccionRec(self%raiz, val, idCapa, mtx2)
    mtx = mtx2
    print*,'Impresion desde el avl '
    call mtx2%print()
end subroutine extraccion

recursive subroutine extraccionRec(raiz, val, idCapa, mtx)
    type(nodo), pointer :: raiz
    integer, intent(in) :: val, idCapa
    type(matrix), intent(inout) :: mtx

    if (.not. associated(raiz)) then
        print*, 'Imagen no encontrada con ID', val
        return
    end if

    if (val < raiz%valor) then
        call extraccionRec(raiz%izquierda, val, idCapa, mtx)
    else if (val > raiz%valor) then
        call extraccionRec(raiz%derecha, val, idCapa, mtx)
    else
        print*, 'Imagen encontrada:', val
        call raiz%abb%extraerMatriz(idCapa, mtx)
    end if
end subroutine extraccionRec
!-----------------------------------------------------------------------------------------------------------------------
!Con esta search busco el nodo X de este arbol, para unificar las matrices que ya tiene guardadas
   subroutine cadena(self, val,cadenaRecorrido)
        class(avl), intent(inout) :: self
        integer, intent(in) :: val
        character(len=:), allocatable,intent(inout) :: cadenaRecorrido
        character(len=:), allocatable :: cad

        call cadenaRec(self%raiz, val,cad)
        cadenaRecorrido = cad

    end subroutine cadena

    recursive subroutine cadenaRec(raiz, val, cadenaRecorrido) 
        type(nodo), pointer :: raiz
        integer, intent(in) :: val
        character(len=:), allocatable,intent(inout) :: cadenaRecorrido

        if(.not. associated(raiz)) then
            print*, 'Valor no encontrado:', val
            return
        end if
        if(val < raiz%valor) then
            call cadenaRec(raiz%izquierda, val, cadenaRecorrido) 
        else if(val > raiz%valor) then
            call cadenaRec(raiz%derecha, val, cadenaRecorrido)
        else
            print*, 'Encontre id de esa imagen' 
            call raiz%abb%recorrido_amplitud(cadenaRecorrido)
        end if

    end subroutine cadenaRec
!-----------------------------------------------------------------------------------------------------------------------
    subroutine validarID(self,val,validacion)
        class(avl), intent(in) :: self
        logical, intent(inout) :: validacion
        integer,intent(in) :: val
        print *, 'Validando ID',validacion
        call validarIDRec(self%raiz,val,validacion)
    end subroutine validarID

    recursive subroutine validarIDRec(raiz,val,validacion)
        type(nodo), pointer, intent(in) :: raiz
        logical, intent(inout) :: validacion
        integer, intent(in) :: val  

        if(associated(raiz)) then
            if(raiz%valor == val) then
                validacion = .true.
                print*, 'El ide existe -->',val
            end if
            call validarIDRec(raiz%izquierda,val,validacion)
            call validarIDRec(raiz%derecha,val,validacion)
        end if

    end subroutine validarIDRec
!-----------------------------------------------------------------------------------------------------------------------
!-----------------------------------------------------------------------------------------------------------------------
!Con esta search busco el nodo X de este arbol, para unificar las matrices que ya tiene guardadas
   subroutine imagenPostOrden(self, val,limite)
        class(avl), intent(inout) :: self
        integer, intent(in) :: val
        integer, intent(in) :: limite

        call imagenPostOrdenRec(self%raiz, val,limite)

    end subroutine imagenPostOrden
    recursive subroutine imagenPostOrdenRec(raiz, val,limite) 
        type(nodo), pointer :: raiz
        integer, intent(in) :: val
        integer, intent(in) :: limite

        if(.not. associated(raiz)) then
            print*, 'Valor no encontrado:', val
            return
        end if
        if(val < raiz%valor) then
            call imagenPostOrdenRec(raiz%izquierda, val,limite) 
        else if(val > raiz%valor) then
            call imagenPostOrdenRec(raiz%derecha, val,limite)
        else
            print*, 'Valor encontrado:', val
            !Si ya tengo la matriz creada entonces ya lo la grafico
            if (raiz%cargada .eqv. .true.)then
                print*, 'Matriz ya creada=================================================================================='
                call raiz%mtx%graficar()
                call raiz%mtx%tabla('Preorder')
            else 
                !call raiz%abb%GrapPostOrden(raiz%mtx,limite)
                print*, 'Matriz unida'
                call raiz%mtx%graficar()
                call raiz%mtx%tabla('PostOrden')
                raiz%cargada = .true. 
            end if
        end if

    end subroutine imagenPostOrdenRec
!-----------------------------------------------------------------------------------------------------------------------
!-----------------------------------------------------------------------------------------------------------------------
!Con esta search busco el nodo X de este arbol, para unificar las matrices que ya tiene guardadas
   subroutine imagenInorden(self, val,limite)
        class(avl), intent(inout) :: self
        integer, intent(in) :: val
        integer, intent(in) :: limite

        call imagenInordenRec(self%raiz, val,limite)

    end subroutine imagenInorden

    recursive subroutine imagenInordenRec(raiz, val,limite) 
        type(nodo), pointer :: raiz
        integer, intent(in) :: val
        integer, intent(in) :: limite

        if(.not. associated(raiz)) then
            print*, 'Valor no encontrado:', val
            return
        end if
        if(val < raiz%valor) then
            call imagenInordenRec(raiz%izquierda, val,limite) 
        else if(val > raiz%valor) then
            call imagenInordenRec(raiz%derecha, val,limite)
        else
            print*, 'Valor encontrado:', val
            !Si ya tengo la matriz creada entonces ya lo la grafico
            if (raiz%cargada .eqv. .true.)then
                print*, 'Matriz ya creada=================================================================================='
                call raiz%mtx%graficar()
                call raiz%mtx%tabla('Inorden')
            else 
                !call raiz%abb%GrapInorden(raiz%mtx,limite)
                print*, 'Matriz unida'
                call raiz%mtx%graficar()
                call raiz%mtx%tabla('Inorde')
                raiz%cargada = .true. 
            end if
        end if

    end subroutine imagenInordenRec
!-----------------------------------------------------------------------------------------------------------------------
!-----------------------------------------------------------------------------------------------------------------------
!Con esta search busco el nodo X de este arbol, para unificar las matrices que ya tiene guardadas
   subroutine imagenPreorden(self, val,limite)
        class(avl), intent(inout) :: self
        integer, intent(in) :: val
        integer, intent(in) :: limite

        call imagenPreordenRec(self%raiz, val,limite)

    end subroutine imagenPreorden

    recursive subroutine imagenPreordenRec(raiz, val,limite) 
        type(nodo), pointer :: raiz
        integer, intent(in) :: val
        integer, intent(in) :: limite

        if(.not. associated(raiz)) then
            print*, 'Valor no encontrado:', val
            return
        end if
        if(val < raiz%valor) then
            call imagenPreordenRec(raiz%izquierda, val,limite) 
        else if(val > raiz%valor) then
            call imagenPreordenRec(raiz%derecha, val,limite)
        else
            print*, 'Valor encontrado:', val
            !Si ya tengo la matriz creada entonces ya lo la grafico
            if (raiz%cargada .eqv. .true.)then
                print*, 'Matriz ya creada=================================================================================='
                call raiz%mtx%graficar()
                call raiz%mtx%tabla('Preorder')
            else 
                !call raiz%abb%GrapPreorder(raiz%mtx,limite)
                print*, 'Matriz unida'
                call raiz%mtx%graficar()
                call raiz%mtx%tabla('Preorder')
                raiz%cargada = .true. 
            end if
        end if

    end subroutine imagenPreordenRec
!-----------------------------------------------------------------------------------------------------------------------
!-----------------------------------------------------------------------------------------------------------------------
!Con esta search busco el nodo X de este arbol, para unificar las matrices que ya tiene guardadas
   subroutine crearImagen(self, val)
        class(avl), intent(inout) :: self
        integer, intent(in) :: val
        call crearImagenRec(self%raiz, val)
    end subroutine crearImagen
    recursive subroutine crearImagenRec(raiz, val) 
        type(nodo), pointer :: raiz
        integer, intent(in) :: val
        if(.not. associated(raiz)) then
            print*, 'Valor no encontrado:', val
            return
        end if
        if(val < raiz%valor) then
           call crearImagenRec(raiz%izquierda, val) 
        else if(val > raiz%valor) then
            call crearImagenRec(raiz%derecha, val)
        else
            print*, 'NodoImagen a crear encontrado:', val
            if (raiz%cargada .eqv. .true.)then
                print*, 'Matriz ya creada=================================================================================='
                call raiz%mtx%graficar()
                call raiz%mtx%tabla('Logrado')
            else 
                print*, 'Graficando la capa 5 de este cliente'
                call raiz%abb%grapEspecifico(1)
                print*, 'Se grafico la capa 5 de este cliente'
                call raiz%abb%unirMatrices(raiz%mtx)
                print*, 'Matriz unida'
                call raiz%mtx%graficar()
                call raiz%mtx%tabla('Logrado')
                raiz%cargada = .true. 
            end if
        end if

    end subroutine crearImagenRec
!-----------------------------------------------------------------------------------------------------------------------
!-----------------------------------------------------------------------------------------------------------------------
!Grafica el abb de cada nodo de este arbol
subroutine abbImagen(self, val)
        class(avl), intent(inout) :: self
        integer, intent(in) :: val

        call searchRec2(self%raiz, val)
    end subroutine abbImagen

    recursive subroutine searchRec2(raiz, val) 
        type(nodo), pointer :: raiz
        integer, intent(in) :: val

        if(.not. associated(raiz)) then
            print*, 'Valor no encontrado:', val
            return
        end if

        if(val < raiz%valor) then
           call searchRec2(raiz%izquierda, val)
        
        else if(val > raiz%valor) then
            call searchRec2(raiz%derecha, val)

        else
            print*, 'Valor encontrado:', val
            call raiz%abb%graph("ABB_AVL")

        end if
    end subroutine searchRec2
!-----------------------------------------------------------------------------------------------------------------------
!-----------------------------------------------------------------------------------------------------------------------
!Con esta search busco el nodo X de este arbol, y a ese nodo le creo el abb con su matriz
   subroutine search(self, val, nodoABB,mtx)
        class(avl), intent(inout) :: self
        integer, intent(in) :: val
        integer, intent(in) :: nodoABB
        type(matrix), intent(in) :: mtx

        call searchRec(self%raiz, val,nodoABB,mtx)
    end subroutine search

    recursive subroutine searchRec(raiz, val, nodoABB,mtx) 
        type(nodo), pointer :: raiz
        integer, intent(in) :: val
        integer, intent(in) :: nodoABB
        type(matrix), intent(in) :: mtx

        if(.not. associated(raiz)) then
            print*, 'Valor no encontrado:', val
            return
        end if

        if(val < raiz%valor) then
           call searchRec(raiz%izquierda, val, nodoABB,mtx)
        
        else if(val > raiz%valor) then
            call searchRec(raiz%derecha, val, nodoABB,mtx)

        else
            print*, 'Valor encontrado:', val
            call raiz%abb%insert(nodoABB)
            call raiz%abb%insertMatriz(nodoABB,mtx)
        end if
    end subroutine searchRec
!-----------------------------------------------------------------------------------------------------------------------
!-----------------------------------------------------------------------------------------------------------------------
!Con este insert creo un nuevo nodo a mi arbol AVL
    subroutine insert(self, val)
        class(avl), intent(inout) :: self
        integer, intent(in) :: val

        call insertRec(self%raiz, val)
    end subroutine insert

    recursive subroutine insertRec(raiz, val)
        type(nodo), pointer, intent(inout) :: raiz
        integer, intent(in) :: val

        if(.not. associated(raiz)) then
            allocate(raiz)
            raiz = nodo(valor=val)
        
        else if(val < raiz%valor) then 
            call insertRec(raiz%izquierda, val)

        else if(val > raiz%valor) then
            call insertRec(raiz%derecha, val)
        end if

        raiz%altura = maximo(obtenerAltura(raiz%izquierda), obtenerAltura(raiz%derecha)) + 1

        if(obtenerBalance(raiz) > 1) then
            if(obtenerBalance(raiz%derecha) < 0) then
                raiz%derecha => rotacionDerecha(raiz%derecha)
                raiz => rotacionIzquierda(raiz)
            else
                raiz => rotacionIzquierda(raiz)
            end if
        end if

        if(obtenerBalance(raiz) < -1) then
            if(obtenerBalance(raiz%izquierda) > 0) then
                raiz%izquierda => rotacionIzquierda(raiz%izquierda)
                raiz => rotacionDerecha(raiz)

            else
                raiz => rotacionDerecha(raiz)
            end if
        end if
    end subroutine insertRec
!-----------------------------------------------------------------------------------------------------------------------
!-----------------------------------------------------------------------------------------------------------------------
!Subrutina para eliminar un nodo de mi arbol AVL
    subroutine delete(self, val)
        class(avl), intent(inout) :: self
        integer, intent(in) :: val

        self%raiz => deleteRec(self%raiz, val)
    end subroutine delete

    recursive function deleteRec(raiz, val) result(res)
        type(nodo), pointer :: raiz
        integer, intent(in) :: val

        type(nodo), pointer :: temp
        type(nodo), pointer :: res 
        
        if(.not. associated(raiz)) then
            res => raiz
            return
        end if

        if(val < raiz%valor) then
            raiz%izquierda => deleteRec(raiz%izquierda, val)
        
        else if(val > raiz%valor) then
            raiz%derecha => deleteRec(raiz%derecha, val)

        else
            if(.not. associated(raiz%izquierda)) then
                temp => raiz%derecha
                deallocate(raiz)
                res => temp

            else if (.not. associated(raiz%derecha)) then
                temp => raiz%izquierda
                deallocate(raiz)
                res => temp
            
            else
                call obtenerMayorDeMenores(raiz%izquierda, temp)
                raiz%valor = temp%valor
                raiz%izquierda => deleteRec(raiz%izquierda, temp%valor)
            end if
        end if

        res => raiz
        if(.not. associated(raiz)) return

        raiz%altura = maximo(obtenerAltura(raiz%izquierda), obtenerAltura(raiz%derecha))

        if(obtenerBalance(raiz) > 1) then
            if(obtenerBalance(raiz%derecha) < 0) then
                raiz%derecha => rotacionDerecha(raiz%derecha)
                raiz => rotacionIzquierda(raiz)
            else
                raiz => rotacionIzquierda(raiz)
            end if
        end if

        if(obtenerBalance(raiz) < -1) then
            if(obtenerBalance(raiz%izquierda) > 0) then
                raiz%izquierda => rotacionIzquierda(raiz%izquierda)
                raiz => rotacionDerecha(raiz)

            else
                raiz => rotacionDerecha(raiz)
            end if
        end if

        res => raiz
    end function deleteRec
!-----------------------------------------------------------------------------------------------------------------------
!-----------------------------------------------------------------------------------------------------------------------
    function rotacionIzquierda(raiz) result(raizDerecha)
        type(nodo), pointer, intent(in) :: raiz
        type(nodo), pointer :: raizDerecha
        type(nodo), pointer :: temp

        raizDerecha => raiz%derecha
        temp => raizDerecha%izquierda

        raizDerecha%izquierda => raiz
        raiz%derecha => temp

        raiz%altura = maximo(obtenerAltura(raiz%izquierda), obtenerAltura(raiz%derecha)) + 1
        raizDerecha%altura = maximo(obtenerAltura(raizDerecha%izquierda), obtenerAltura(raizDerecha%derecha)) + 1
    end function rotacionIzquierda
!-----------------------------------------------------------------------------------------------------------------------
!-----------------------------------------------------------------------------------------------------------------------
    function rotacionDerecha(raiz) result(raizIzquierda)
        type(nodo), pointer, intent(in) :: raiz
        type(nodo), pointer :: raizIzquierda
        type(nodo), pointer :: temp

        raizIzquierda => raiz%izquierda
        temp => raizIzquierda%derecha

        raizIzquierda%derecha => raiz
        raiz%izquierda => temp

        raiz%altura = maximo(obtenerAltura(raiz%izquierda), obtenerAltura(raiz%derecha)) + 1
        raizIzquierda%altura = maximo(obtenerAltura(raizIzquierda%izquierda), obtenerAltura(raizIzquierda%derecha)) + 1
    end function rotacionDerecha
!-----------------------------------------------------------------------------------------------------------------------
!-----------------------------------------------------------------------------------------------------------------------
    recursive subroutine obtenerMayorDeMenores(raiz, mayor)
        type(nodo), pointer :: raiz, mayor
        if(associated(raiz%derecha)) then
            call obtenerMayorDeMenores(raiz%derecha, mayor)
        else
            mayor => raiz
        end if
    end subroutine obtenerMayorDeMenores
!-----------------------------------------------------------------------------------------------------------------------
!-----------------------------------------------------------------------------------------------------------------------
    subroutine preorden(self)
        class(avl), intent(in) :: self
        integer :: veces=0
        
        call preordenRec(self%raiz,veces)
        print *, "Numero de Imagenes del Usuario en el arbol AVL: ", veces
    end subroutine preorden

    recursive subroutine preordenRec(raiz,veces)
        type(nodo), pointer, intent(in) :: raiz
        integer, intent(inout) :: veces

        if(associated(raiz)) then
            veces = veces + 1
            call preordenRec(raiz%izquierda,veces)
            call preordenRec(raiz%derecha,veces)
        end if
    end subroutine preordenRec
!-----------------------------------------------------------------------------------------------------------------------
!-----------------------------------------------------------------------------------------------------------------------
    function maximo(izquierda, derecha) result(res)
        integer, intent(in) :: izquierda
        integer, intent(in) :: derecha

        integer :: res
        res = derecha

        if(izquierda >= derecha) then
            res = izquierda
            return
        end if
    end function maximo
!-----------------------------------------------------------------------------------------------------------------------
!-----------------------------------------------------------------------------------------------------------------------
    function obtenerBalance(raiz) result(res)
        type(nodo), pointer, intent(in) :: raiz
        integer :: res
        
        res = obtenerAltura(raiz%derecha) - obtenerAltura(raiz%izquierda)
    end function
!-----------------------------------------------------------------------------------------------------------------------
!-----------------------------------------------------------------------------------------------------------------------
    function obtenerAltura(n) result(res)
        type(nodo), pointer :: n
        integer :: res
        res = 0

        if(.not. associated(n)) return
        res = n%altura
    end function obtenerAltura
!-----------------------------------------------------------------------------------------------------------------------
!-----------------------------------------------------------------------------------------------------------------------
    recursive subroutine imprimirRec(raiz, nombre, io)
        type(nodo), pointer, intent(in) :: raiz
        character(len=36), intent(in) :: nombre
        integer :: io

        character(len=36) :: derecha
        character(len=36) :: izquierda

        derecha = generate_uuid()
        izquierda = generate_uuid()

        if(associated(raiz)) then
            !"Nodo_uuid"[Label="1"]
            write(io, *) '"Nodo'//nombre//'"[label= "', raiz%valor, '"]'

            if(associated(raiz%izquierda)) then
                !"Nodo_uuid"->"Nodo_uuidHijoIzquierdo"
                write(io, *) '"Nodo'//nombre//'"->"Nodo'//izquierda//'"'
            end if

            if(associated(raiz%derecha)) then
                !"Nodo_uuid"->"Nodo_uuidHijoDerecho"
                write(io, *) '"Nodo'//nombre//'"->"Nodo'//derecha//'"'
            end if
            call imprimirRec(raiz%izquierda, izquierda, io)
            call imprimirRec(raiz%derecha, derecha, io)
        end if
    end subroutine imprimirRec
!-----------------------------------------------------------------------------------------------------------------------
!-----------------------------------------------------------------------------------------------------------------------
!Grafica mi arbol AVL
    subroutine graficar(self)
        class(avl), intent(in) :: self
        integer :: io
        integer :: i
        character(len=100) :: comando

        io = 1
        open(newunit=io, file="./avl_tree.dot")
        comando = "dot -Tpng ./avl_tree.dot -o ./avl_tree.png"

        write(io, *) "digraph G {"
            !Graficar
        if(associated(self%raiz)) then
            call imprimirRec(self%raiz, generate_uuid(), io)
        end if
        write(io, *) "}"
        close(io)

        call execute_command_line(comando, exitstat=i)

        if(i == 1) then
            print *, "Error al momento de crear la imagen"
        else
            print *, "La imagen fue generada exitosamente"
            call system('start ./avl_tree.png')
        end if
    end subroutine graficar
!-----------------------------------------------------------------------------------------------------------------------
!-----------------------------------------------------------------------------------------------------------------------
end module avl_m