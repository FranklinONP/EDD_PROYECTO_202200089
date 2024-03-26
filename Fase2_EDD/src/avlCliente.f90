module avl_c
    use clienteTemporal
    use abb_m
    use matrix_m
    use uuid_module
    use lista_module
    implicit none
    private

    type :: nodo
        integer :: valor
        type(cliente) :: cliente
        integer :: altura = 1
        !type(abb) :: abb
        !type(matrix) :: mtx
        logical :: cargada = .false.
        type(nodo), pointer :: derecha => null()
        type(nodo), pointer :: izquierda => null() 
    end type

    type, public :: avlc
        type(nodo), pointer :: raiz => null()
    
    contains
        procedure :: insert
        procedure :: delete
        procedure :: graficar
        procedure :: search
        procedure :: abbImagen
        procedure :: crearImagen
        procedure :: imagenPreorden
        procedure :: imagenInorden
        procedure :: imagenPostOrden
        procedure :: validarID
       !----------------------
        procedure :: insertNodoCapa
        procedure :: insertCapa
        procedure :: insertNodoImagen
        procedure :: extraerM
        procedure :: insertMatriz 
        procedure :: Imagen
        procedure :: Grafica
        procedure :: Preorden
        procedure :: Inorden
        procedure :: PostOrden
        procedure :: graficaCapas
        procedure :: login
        procedure :: insertMatrizBB
        procedure :: cadena
        procedure :: oma
    end type avlc

contains
    subroutine oma(self, dpi,idImagen,idCapa, mtx)
        class(avlc), intent(in) :: self
        integer, intent(in) :: dpi,idImagen,idCapa
        type(matrix), intent(inout) :: mtx
        type(matrix) :: mtx2

        call omaRec(self%raiz, dpi,idImagen,idCapa, mtx2)
        mtx=mtx2
    end subroutine oma

    recursive subroutine omaRec(raiz, dpi,idImagen,idCapa, mtx) 
        type(nodo), pointer :: raiz
        integer, intent(in) :: dpi,idImagen,idCapa
        type(matrix), intent(inout) :: mtx

        if(.not. associated(raiz)) then
            print*, 'Valor no encontrado:'
            return
        end if

        if(dpi < raiz%valor) then
           call omaRec(raiz%izquierda,dpi,idImagen,idCapa, mtx)
        
        else if(dpi > raiz%valor) then
            call omaRec(raiz%derecha,dpi,idImagen,idCapa, mtx)

        else
            print*, 'DPI encontrado'
            print*, 'Empezar a extraerMatriz'
            !call raiz%cliente%avl%cadena(idImagen,idCapa, mtx)
            call raiz%cliente%avl%extraccion(idImagen,idCapa,mtx)
            print *, '^^^^^^^^^^^^^^^^^^^^'

        end if
    end subroutine omaRec
    subroutine cadena(self, dpi,idImagen,cadenaNodos)
        class(avlc), intent(in) :: self
        integer, intent(in) :: dpi,idImagen
        character(len=:), allocatable,intent(inout) :: cadenaNodos
        character(len=:), allocatable :: cad

        call cadenaRec(self%raiz, dpi,idImagen,cad)
        cadenaNodos = cad
    end subroutine cadena

    recursive subroutine cadenaRec(raiz, dpi, idImagen,cadenaNodos) 
        type(nodo), pointer :: raiz
        integer, intent(in) :: dpi,idImagen
        character(len=:), allocatable,intent(inout) :: cadenaNodos

        if(.not. associated(raiz)) then
            print*, 'Valor no encontrado:'
            return
        end if

        if(dpi < raiz%valor) then
           call cadenaRec(raiz%izquierda,dpi,idImagen,cadenaNodos)
        
        else if(dpi > raiz%valor) then
            call cadenaRec(raiz%derecha,dpi, idImagen,cadenaNodos)

        else
            print*, 'DPI encontrado'
            print*, 'Empezar a armar cadena'
            call raiz%cliente%avl%cadena(idImagen,cadenaNodos)
            print *, 'ABB del cliente graficado'

        end if
    end subroutine cadenaRec
!-----------------------------------------------------------------------------------------------------------------------
    subroutine validarID(self, dpi,idImagen,respuesta)
        class(avlc), intent(inout) :: self
        integer, intent(in) :: dpi,idImagen
        logical, intent(inout) :: respuesta

        call validarIDRec(self%raiz, dpi,idImagen,respuesta)
    end subroutine validarID

    recursive subroutine validarIDRec(raiz, dpi,idImagen,respuesta) 
        type(nodo), pointer :: raiz
        integer, intent(in) :: dpi,idImagen
        logical, intent(inout) :: respuesta

        if(.not. associated(raiz)) then
            print*, 'Valor no encontrado:'
            return
        end if

        if(dpi < raiz%valor) then
           call validarIDRec(raiz%izquierda,dpi,idImagen,respuesta)
        
        else if(dpi > raiz%valor) then
            call validarIDRec(raiz%derecha,dpi,idImagen,respuesta)

        else
            print*, 'DPI encontrado'
            call raiz%cliente%avl%validarID(idImagen,respuesta)
            print *, 'Nodo imagen insertado',idImagen

        end if
    end subroutine validarIDRec
!-----------------------------------------------------------------------------------------------------------------------
    subroutine login(self, dpii, password,respuesta)
        class(avlc), intent(inout) :: self
        character(len=*), intent(in) :: password,dpii
        logical, intent(inout) :: respuesta
        integer :: dpi
        read(dpii,*) dpi
        call loginRec(self%raiz, dpi,password,respuesta)
    end subroutine login

    recursive subroutine loginRec(raiz, dpi,password,respuesta) 
        type(nodo), pointer :: raiz
        character(len=*), intent(in) :: password
        integer, intent(in) :: dpi
        logical, intent(inout) :: respuesta

        if(.not. associated(raiz)) then
            print*, 'Valor no encontrado:'
            return
        end if

        if(dpi < raiz%valor) then
           call loginRec(raiz%izquierda,dpi,password,respuesta)
        
        else if(dpi > raiz%valor) then
            call loginRec(raiz%derecha,dpi,password,respuesta)

        else
            if(raiz%cliente%password .eq. password) then
                respuesta = .true.
            end if

        end if
    end subroutine loginRec
!-----------------------------------------------------------------------------------------------------------------------
    subroutine GraficaCapas(self, dpi)
        class(avlc), intent(inout) :: self
        integer, intent(in) :: dpi

        call GraficaCapasRec(self%raiz, dpi)
    end subroutine GraficaCapas

    recursive subroutine GraficaCapasRec(raiz, dpi) 
        type(nodo), pointer :: raiz
        integer, intent(in) :: dpi

        if(.not. associated(raiz)) then
            print*, 'Valor no encontrado:'
            return
        end if

        if(dpi < raiz%valor) then
           call GraficaCapasRec(raiz%izquierda,dpi)
        
        else if(dpi > raiz%valor) then
            call GraficaCapasRec(raiz%derecha,dpi)

        else
            print*, 'DPI encontrado'
            call raiz%cliente%tree%graph("ABB de Capas")
            print *, 'ABB del cliente graficado'

        end if
    end subroutine GraficaCapasRec
!-----------------------------------------------------------------------------------------------------------------------
    subroutine Grafica(self, dpi)
        class(avlc), intent(inout) :: self
        integer, intent(in) :: dpi

        call GraficaRec(self%raiz, dpi)
    end subroutine Grafica

    recursive subroutine GraficaRec(raiz, dpi) 
        type(nodo), pointer :: raiz
        integer, intent(in) :: dpi

        if(.not. associated(raiz)) then
            print*, 'Valor no encontrado:'
            return
        end if

        if(dpi < raiz%valor) then
           call GraficaRec(raiz%izquierda,dpi)
        
        else if(dpi > raiz%valor) then
            call GraficaRec(raiz%derecha,dpi)

        else
            print*, 'DPI encontrado'
            
            call raiz%cliente%avl%graficar()
            print *, 'Avl del cliente graficado'

        end if
    end subroutine GraficaRec
!-----------------------------------------------------------------------------------------------------------------------
!-----------------------------------------------------------------------------------------------------------------------
    subroutine insertMatrizBB(self, dpi,idImagen,idCapa,mtx)
        class(avlc), intent(inout) :: self
        integer, intent(in) :: dpi,idImagen,idCapa
        type(matrix), intent(in) :: mtx

        call insertMatrizBBRec(self%raiz,dpi,idImagen,idCapa,mtx)
    end subroutine insertMatrizBB

    recursive subroutine insertMatrizBBRec(raiz, dpi,idImagen,idCapa,mtx) 
        type(nodo), pointer :: raiz
        integer, intent(in) :: dpi,idImagen,idCapa
        type(matrix), intent(in) :: mtx

        if(.not. associated(raiz)) then
            print*, 'Valor no encontrado:'
            return
        end if

        if(dpi < raiz%valor) then
           call insertMatrizBBRec(raiz%izquierda,dpi,idImagen,idCapa,mtx)
        
        else if(dpi > raiz%valor) then
            call insertMatrizBBRec(raiz%derecha,dpi,idImagen,idCapa,mtx)

        else
            print*, 'DPI encontrado insertandoMatriz abb a Imagen'
            !call raiz%cliente%tree%GrapPreorder(listaT,limite)
            call raiz%cliente%avl%search(idImagen,idCapa,mtx)
            print *, '-------------------------------------------'

        end if
    end subroutine insertMatrizBBRec
    !-----------------------------------------------------------------------------------------------------------------------
    subroutine PostOrden(self, dpi,lista,limite)
        class(avlc), intent(inout) :: self
        integer, intent(in) :: dpi,limite
        type(lista_m),intent(inout) :: lista
        call PostOrdenRec(self%raiz,dpi,lista,limite)
    end subroutine PostOrden
    recursive subroutine PostOrdenRec(raiz, dpi,lista,limite) 
        type(nodo), pointer :: raiz
        integer, intent(in) :: dpi,limite
        type(lista_m),intent(inout) :: lista 

        if(.not. associated(raiz)) then
            print*, 'Valor no encontrado:'
            return
        end if
        if(dpi < raiz%valor) then
           call PostOrdenRec(raiz%izquierda,dpi,lista,limite)
        else if(dpi > raiz%valor) then
            call PostOrdenRec(raiz%derecha,dpi,lista,limite)
        else
            print*, 'DPI encontrado --Preorder'
            call raiz%cliente%tree%GrapPostOrden(lista,limite)
            print *, 'Preorder ---------------'
        end if
    end subroutine PostOrdenRec
    !-----------------------------------------------------------------------------------------------------------------------
    subroutine Inorden(self, dpi,lista,limite)
        class(avlc), intent(inout) :: self
        integer, intent(in) :: dpi,limite
        type(lista_m),intent(inout) :: lista
        call InordenRec(self%raiz,dpi,lista,limite)
    end subroutine Inorden
    recursive subroutine InordenRec(raiz, dpi,lista,limite) 
        type(nodo), pointer :: raiz
        integer, intent(in) :: dpi,limite
        type(lista_m),intent(inout) :: lista 

        if(.not. associated(raiz)) then
            print*, 'Valor no encontrado:'
            return
        end if
        if(dpi < raiz%valor) then
           call InordenRec(raiz%izquierda,dpi,lista,limite)
        else if(dpi > raiz%valor) then
            call InordenRec(raiz%derecha,dpi,lista,limite)
        else
            print*, 'DPI encontrado --Preorder'
            call raiz%cliente%tree%GrapInorden(lista,limite)
            print *, 'Preorder ---------------'
        end if
    end subroutine InordenRec
!-----------------------------------------------------------------------------------------------------------------------
    subroutine Preorden(self, dpi,lista,limite)
        class(avlc), intent(inout) :: self
        integer, intent(in) :: dpi,limite
        type(lista_m),intent(inout) :: lista


        call graficarPreordenRec(self%raiz,dpi,lista,limite)
    end subroutine Preorden

    recursive subroutine graficarPreordenRec(raiz, dpi,lista,limite) 
        type(nodo), pointer :: raiz
        integer, intent(in) :: dpi,limite
        type(lista_m),intent(inout) :: lista 

        if(.not. associated(raiz)) then
            print*, 'Valor no encontrado:'
            return
        end if

        if(dpi < raiz%valor) then
           call graficarPreordenRec(raiz%izquierda,dpi,lista,limite)
        
        else if(dpi > raiz%valor) then
            call graficarPreordenRec(raiz%derecha,dpi,lista,limite)

        else
            print*, 'DPI encontrado --Preorder'
            call raiz%cliente%tree%GrapPreorder(lista,limite)
            print *, 'Preorder ---------------'

        end if
    end subroutine graficarPreordenRec
!-----------------------------------------------------------------------------------------------------------------------
    subroutine Imagen(self, dpi,idImagen)
        class(avlc), intent(inout) :: self
        integer, intent(in) :: dpi,idImagen

        call ImagenRec(self%raiz, dpi,idImagen)
    end subroutine Imagen

    recursive subroutine ImagenRec(raiz, dpi,idImagen) 
        type(nodo), pointer :: raiz
        integer, intent(in) :: dpi,idImagen

        if(.not. associated(raiz)) then
            print*, 'Valor no encontrado:'
            return
        end if

        if(dpi < raiz%valor) then
           call insertNodoImagenRec(raiz%izquierda,dpi,idImagen)
        
        else if(dpi > raiz%valor) then
            call insertNodoImagenRec(raiz%derecha,dpi,idImagen)

        else
            print*, 'DPI encontrado'
            call raiz%cliente%avl%crearImagen(idImagen)
            print *, 'Nodo imagen insertado',idImagen

        end if
    end subroutine ImagenRec
!-----------------------------------------------------------------------------------------------------------------------
!-----------------------------------------------------------------------------------------------------------------------
!-----------------------------------------------------------------------------------------------------------------------
!Con esta search busco el nodo X de este arbol, para unificar las matrices que ya tiene guardadas
   subroutine imagenPostOrden(self, val,limite)
        class(avlc), intent(inout) :: self
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
                !call raiz%mtx%graficar()
                !call raiz%mtx%tabla('Preorder')
            else 
                !call raiz%abb%GrapPostOrden(raiz%mtx,limite)
                print*, 'Matriz unida'
                !call raiz%mtx%graficar()
                !call raiz%mtx%tabla('PostOrden')
                raiz%cargada = .true. 
            end if
        end if

    end subroutine imagenPostOrdenRec
!-----------------------------------------------------------------------------------------------------------------------
!-----------------------------------------------------------------------------------------------------------------------
!Con esta search busco el nodo X de este arbol, para unificar las matrices que ya tiene guardadas
   subroutine imagenInorden(self, val,limite)
        class(avlc), intent(inout) :: self
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
                !call raiz%mtx%graficar()
                !call raiz%mtx%tabla('Inorden')
            else 
                !call raiz%abb%GrapInorden(raiz%mtx,limite)
                print*, 'Matriz unida'
                !call raiz%mtx%graficar()
                !call raiz%mtx%tabla('Inorde')
                raiz%cargada = .true. 
            end if
        end if

    end subroutine imagenInordenRec
!-----------------------------------------------------------------------------------------------------------------------
!-----------------------------------------------------------------------------------------------------------------------
!Con esta search busco el nodo X de este arbol, para unificar las matrices que ya tiene guardadas
   subroutine imagenPreorden(self, val,limite)
        class(avlc), intent(inout) :: self
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
                !call raiz%mtx%graficar()
                !call raiz%mtx%tabla('Preorder')
            else 
                !call raiz%abb%GrapPreorder(raiz%mtx,limite)
                print*, 'Matriz unida'
                !call raiz%mtx%graficar()
                !call raiz%mtx%tabla('Preorder')
                raiz%cargada = .true. 
            end if
        end if

    end subroutine imagenPreordenRec
!-----------------------------------------------------------------------------------------------------------------------
!-----------------------------------------------------------------------------------------------------------------------
!Con esta search busco el nodo X de este arbol, para unificar las matrices que ya tiene guardadas
   subroutine crearImagen(self, val)
        class(avlc), intent(inout) :: self
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
            print*, 'Valor encontrado:', val
            !Si ya tengo la matriz creada entonces ya lo la grafico
            if (raiz%cargada .eqv. .true.)then
                print*, 'Matriz ya creada=================================================================================='
                !call raiz%mtx%graficar()
                !call raiz%mtx%tabla('Logrado')
            else 
                !call raiz%abb%unirMatrices(raiz%mtx)
                print*, 'Matriz unida'
                !call raiz%mtx%graficar()
                !call raiz%mtx%tabla('Logrado')
                raiz%cargada = .true. 
            end if
        end if

    end subroutine crearImagenRec
!-----------------------------------------------------------------------------------------------------------------------
!-----------------------------------------------------------------------------------------------------------------------
!Grafica el abb de cada nodo de este arbol
subroutine abbImagen(self, val)
        class(avlc), intent(inout) :: self
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
           ! call raiz%abb%graph("ABB_avlc")

        end if
    end subroutine searchRec2
!-----------------------------------------------------------------------------------------------------------------------
!-----------------------------------------------------------------------------------------------------------------------
!Con esta search busco el nodo X de este arbol, y a ese nodo le creo el abb con su matriz
   subroutine search(self, val, nombre,password)
        class(avlc), intent(inout) :: self
        integer, intent(in) :: val
        character(len=*), intent(in) :: nombre,password
        print *, '--------------------->',val
        call searchRec(self%raiz,val, nombre,password)
    end subroutine search

    recursive subroutine searchRec(raiz, val, nombre,password) 
        type(nodo), pointer :: raiz
        integer, intent(in) :: val
        character(len=*), intent(in) :: nombre,password

        if(.not. associated(raiz)) then
            print*, 'Cliente no encontrado', nombre
            return
        end if

        if(val < raiz%valor) then
           call searchRec(raiz%izquierda,  val, nombre,password)
        
        else if(val > raiz%valor) then
            call searchRec(raiz%derecha,  val, nombre,password)

        else
            print*, 'Id de cliente encontrado', val
            raiz%cliente%nombre = nombre
            raiz%cliente%dpi=val
            raiz%cliente%password=password
        end if
    end subroutine searchRec
    !-----------------------------------------------------------------------------------------------------------------------
    subroutine insertMatriz(self, dpi,idImagen,idCapaE,mtxT)
        class(avlc), intent(inout) :: self
        integer, intent(in) :: dpi,idImagen,idCapaE
        type(matrix), intent(in) :: mtxT

        call insertMatrizRec(self%raiz,dpi,idImagen,idCapaE,mtxT)
    end subroutine insertMatriz

    recursive subroutine insertMatrizRec(raiz,dpi,idImagen,idCapaE,mtxT) 
        type(nodo), pointer :: raiz
        integer, intent(in) :: dpi,idImagen,idCapaE
        type(matrix), intent(in) :: mtxT

        if(.not. associated(raiz)) then
            print*, 'Valor no encontrado:'
            return
        end if

        if(dpi < raiz%valor) then
           call insertMatrizRec(raiz%izquierda,dpi,idImagen,idCapaE,mtxT)
        
        else if(dpi > raiz%valor) then
            call insertMatrizRec(raiz%derecha,dpi,idImagen,idCapaE,mtxT)

        else
            print*, 'Nodo Imagen encontrada'
            call raiz%cliente%avl%search(idImagen,idCapaE,mtxT)
            print *, 'Imagen con abb lista'

        end if
    end subroutine insertMatrizRec
    !-----------------------------------------------------------------------------------------------------------------------
    subroutine extraerM(self, dpi,idCapaE,mtxT)
        class(avlc), intent(inout) :: self
        integer, intent(in) :: dpi,idCapaE
        type(matrix), intent(inout) :: mtxT

        call extraerMRec(self%raiz,dpi,idCapaE,mtxT)

    end subroutine extraerM

    recursive subroutine extraerMRec(raiz,dpi,idCapaE,mtxT) 
        type(nodo), pointer :: raiz
        integer, intent(in) :: dpi,idCapaE
        type(matrix), intent(inout) :: mtxT

        if(.not. associated(raiz)) then
            print*, 'Valor no encontrado:'
            return
        end if

        if(dpi < raiz%valor) then
           call extraerMRec(raiz%izquierda,dpi,idCapaE,mtxT)
        
        else if(dpi > raiz%valor) then
            call extraerMRec(raiz%derecha,dpi,idCapaE,mtxT)

        else
            print*, 'DPI encontrado'
            call raiz%cliente%tree%extraerMatriz(idCapaE,mtxT)
            print *, 'Extrayendo matriz'

        end if
    end subroutine extraerMRec
!-----------------------------------------------------------------------------------------------------------------------
    subroutine insertNodoImagen(self, dpi,idImagen)
        class(avlc), intent(inout) :: self
        integer, intent(in) :: dpi,idImagen

        call insertNodoImagenRec(self%raiz, dpi,idImagen)
    end subroutine insertNodoImagen

    recursive subroutine insertNodoImagenRec(raiz, dpi,idImagen) 
        type(nodo), pointer :: raiz
        integer, intent(in) :: dpi,idImagen

        if(.not. associated(raiz)) then
            print*, 'Valor no encontrado:'
            return
        end if

        if(dpi < raiz%valor) then
           call insertNodoImagenRec(raiz%izquierda,dpi,idImagen)
        
        else if(dpi > raiz%valor) then
            call insertNodoImagenRec(raiz%derecha,dpi,idImagen)

        else
            print*, 'DPI encontrado'
            print*, 'Capa encontrada'
            call raiz%cliente%avl%insert(idImagen)
            print *, 'Nodo imagen insertado',idImagen

        end if
    end subroutine insertNodoImagenRec
!-----------------------------------------------------------------------------------------------------------------------
    subroutine insertCapa(self, dpi,idCapaE,filaE,columnaE,color)
        class(avlc), intent(inout) :: self
        integer, intent(in) :: dpi,idCapaE,filaE,columnaE
        character(len=*), intent(in) :: color

        call insertCapaRec(self%raiz,dpi,idCapaE,filaE,columnaE,color)
    end subroutine insertCapa

    recursive subroutine insertCapaRec(raiz,dpi,idCapaE,filaE,columnaE,color) 
        type(nodo), pointer :: raiz
        integer, intent(in) :: dpi,idCapaE,filaE,columnaE
        character(len=*), intent(in) :: color

        if(.not. associated(raiz)) then
            print*, 'Valor no encontrado:'
            return
        end if

        if(dpi < raiz%valor) then
           call insertCapaRec(raiz%izquierda,dpi,idCapaE,columnaE,filaE,color)
        
        else if(dpi > raiz%valor) then
            call insertCapaRec(raiz%derecha,dpi,idCapaE,columnaE,filaE,color)

        else
            print*, 'DPI encontrado'
            print*, 'Capa encontrada'
            call raiz%cliente%tree%search(idCapaE,columnaE,filaE,color)
            print *, 'Capa insertada',idCapaE

        end if
    end subroutine insertCapaRec
!-----------------------------------------------------------------------------------------------------------------------
    subroutine insertNodoCapa(self, dpi,idCapa)
        class(avlc), intent(inout) :: self
        integer, intent(in) :: dpi,idCapa
        call insertNodoCapaRec(self%raiz,dpi,idCapa)
    end subroutine insertNodoCapa

    recursive subroutine insertNodoCapaRec(raiz,dpi,idCapa) 
        type(nodo), pointer :: raiz
        integer, intent(in) :: dpi,idCapa

        if(.not. associated(raiz)) then
            print*, 'Valor no encontrado:'
            return
        end if

        if(dpi < raiz%valor) then
           call insertNodoCapaRec(raiz%izquierda,dpi,idCapa)
        
        else if(dpi > raiz%valor) then
            call insertNodoCapaRec(raiz%derecha,dpi,idCapa)

        else
            print*, 'DPI encontrado'
            call raiz%cliente%tree%insert(idCapa)
            print *, 'Capa insertada',idCapa

        end if
    end subroutine insertNodoCapaRec
!-----------------------------------------------------------------------------------------------------------------------
!-----------------------------------------------------------------------------------------------------------------------
!Con este insert creo un nuevo nodo a mi arbol avlc
    subroutine insert(self, val)
        class(avlc), intent(inout) :: self
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
!Subrutina para eliminar un nodo de mi arbol avlc
    subroutine delete(self, val)
        class(avlc), intent(inout) :: self
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
!Grafica mi arbol avlc
    subroutine graficar(self)
        class(avlc), intent(in) :: self
        integer :: io
        integer :: i
        character(len=100) :: comando

        io = 1
        open(newunit=io, file="./avlc_tree.dot")
        comando = "dot -Tpng ./avlc_tree.dot -o ./avlc_tree.png"

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
        end if
    end subroutine graficar
!-----------------------------------------------------------------------------------------------------------------------
!-----------------------------------------------------------------------------------------------------------------------
end module avl_c