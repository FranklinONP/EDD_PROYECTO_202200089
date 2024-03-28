
program main
    use module_btree
    use module_ordinal_user
    use listOrden

    use avl_c
    use matrix_m
    use json_module
    use iso_fortran_env, only:
    use clienteTemporal
    use avl_m
    use lista_module
    
    implicit none
    type(listaOrdenamiento) :: listaOrden
    type(avlc) :: arbolClientes
    type(BTree) :: b
    type(ordinal_user) :: user
    type(avl) :: a
    type(matrix) :: mtx,mtx2

    type(json_file) :: json
    type(json_core) :: jsonc
    type(json_value), pointer :: listPointer, animalPointer, attributePointer
    logical :: found
    integer :: sizee, iC
    character(:), allocatable :: password,nombreCliente,dpi
    type(json_file) :: jsonAlbumes
    type(json_core) :: jsoncAlbumes
    type(json_value), pointer :: listPointerAlbumes, todoAlbumes, attributePointerAlbumes,imgsAlbumes
    logical :: foundAlbumes
    integer :: sizeAlbumes, iCAlbumes
    character(:), allocatable :: nombreAlbum,imgs,numImagenes
    character(:), allocatable :: idCapa,fila,columna,color
    type(json_value), pointer :: todoCapas,attributePointerCapas
    character(len=1000) :: direccion
    character(:), allocatable :: idImagen,capasImagenes
    character(len=7):: colorcito 
    !Importacionnes
    !Matriz Dispersa
    type(cliente) :: clienteObject
    integer :: values(17) = [20, 8, 3, 1, 0, 15, 30, 48, 26, 10, 7, 5, 60, 19, 11, 21, 3]
    integer :: desicion,del
    integer :: filaE,columnaE,idCapaE
    integer :: idImagenE,capaImagenE
    type(matrix) :: mtxTemporal
    
    integer :: keys(18) = [10, 5, 15, 3, 1, 12, 17, 7, 4, 6, 9, 11, 2, 20, 24, 21, 23, 28]
    integer :: i
    logical :: alvF

    logical :: validacion=.true.
    type(lista_m) :: listaTemporal
    type(matrix) :: mtxTemporal2
    logical :: resultLista=.false.
    integer :: idLista,contadorLista=0

    character(len=5) :: admin
    character(len=7) :: passwordAdmin
    character(len=100) :: dpiUser,passwordUser,nombreUser
    integer :: principal, principal1,principal2,principal3,principal4,principal5
    logical :: isLoggedIn = .false.
    logical :: exitAdmin=.false.,loginAdmin=.false.

    integer :: dpiInt, buscadorGeneral,opgc,idMN,AmplitudID,posA,countA,capitaE,capasUsuarioInt
    integer(kind=8) :: dpiInt2,dpiUserInt,idReporte
    logical :: respuesta=.false.
    character(len=:), allocatable :: cadena,capita,capasUsuario,cuAux
    character(len=1) :: respuestaIC,respuestaQ
    logical :: existeCapa=.false.
    logical :: idReporteR=.false.
    logical :: existeC=.true.   
    character(len=100)::  nombreum,passwordum
!======================================================================================================================================
!======================================================================================================================================
    do
        print *, 'Menu Principal'
        print *, '1. Administrador'
        print *, '2. Iniciar Sesión'
        print *, '3. Salir'
        read *, principal

        select case (principal)
            case (1)
                do while (.not. loginAdmin)
                    print *, 'Ingrese su usuario'
                    read *, dpiUser
                    print *, 'Ingrese su contraseña '
                    read *, passwordUser
                    if (dpiUser .eq. 'admin' .and. passwordUser .eq. 'EDD2024') then
                        loginAdmin = .true.
                    else
                        print *, 'Usuario o contraseña incorrectos'
                    end if
                end do

                exitAdmin = .false.
                do while (.not. exitAdmin)
                    print *, 'Menu Administrador'
                    print *, '1. Carga Masiva de Clientes'
                    print *, '2. Graficar árbol de Clientes'
                    print *, '3. Operaciones sobre Usuarios'
                    print *, '4. Reporte Administrador'
                    print *, '5. Regresar al Menu Principal'
                    read *, principal1

                    select case (principal1)
                        case (1)
                            print *, 'Carga Masiva de Clientes'
                                direccion="clientes.json"
                                call cargaMasivaCliente(direccion)
                        case (2)
                            print *, 'Graficar árbol de Clientes'
                            call b%graphBTree(myNode=b%returnRoot())
                        case (3)
                            do while (.true.)
                                print *, 'Menu Operaciones sobre Usuarios'
                                print *, '1. Insertar'
                                print *, '2. Modificar'
                                print *, '3. Eliminar'
                                print *, '4. Regresar'
                                read *, principal2

                                select case (principal2)
                                    case (1)
                                        print *, 'Insertar'
                                        print *, 'Ingrese el dpi del nuevo cliente'
                                        existeC=.false.
                                        read *, dpiInt2
                                        call arbolClientes%exist(dpiInt2,existeC)
                                        if(existeC)then
                                            print *, "El dpi ya existe"
                                        else
                                            print *, 'Ingrese el nombre del cliente'
                                            read*, nombreCliente
                                            print *, 'Ingrese la contraseña del cliente'
                                            read *, password

                                            call arbolClientes%insert(dpiInt2)
                                            !Busco ese id para setear datos del cliente
                                            call arbolClientes%search(dpiInt2,nombreCliente,password)
                                            call b%insert(ordinal_user(nombreCliente,dpiInt2, password))
                                            call listaOrden%insert(dpiInt2)
                                        end if
                                    case (2)
                                        print *, 'Modificar'
                                        print *, 'Ingrese el dpi del cliente a modificar'
                                        read *, dpiInt2
                                        print*, "Ingrese 1 si quiere modificar nombre   2. Saltar Modificacion"
                                        read*, desicion
                                        if(desicion .eq. 1) then
                                            print*, "Ingrese el nuevo nombre"
                                            read *,  nombreum
                                        end if
                                        print*, "Ingrese 1 si quiere modificar password  2. Saltar modificacion"
                                        read*, desicion
                                        if(desicion .eq. 1) then
                                            print*, "Ingrese el nuevo password"
                                            read *, passwordum
                                            print*, "Password modificado"
                                        end if
                                        call arbolClientes%cambiarDatos(dpiInt2,nombreum,passwordum)
                                        !call arbolClientes%hola()
                                    case (3)
                                        print *, 'Eliminar'
                                        print *, 'Ingrese el dpi del cliente a eliminar'
                                        read *, dpiInt2
                                        existeC=.false.
                                        call arbolClientes%exist(dpiInt2,existeC)
                                        if(existeC)then
                                            call arbolClientes%delete(dpiInt2)
                                            call b%remove(dpiInt2)
                                        else
                                            print *, "El Usuario no existe"
                                        end if
                                    case (4)
                                        exit
                                    case default
                                        print *, 'Opcion no válida'
                                end select
                            end do
                        case (4)
                            print *, "Ingrese el dpi de un cliente para ver su reporte"
                            read *, idReporte
                            idReporteR=.false.
                            print *, '============================================================'
                            print *, '=================== Reporte Administrador =================='
                            print *, '============================================================'
                            call arbolClientes%reporteAdmin(idReporte,idReporteR)
                            print *, '============================================================'
                            print *, '===== IMPRESION POR AMPLITUD DEL ABROL DE CLIENTES ========='
                            print *, '============================================================'
                            call listaOrden%printList()
                        case (5)
                            
                            exitAdmin = .true.
                        case default
                            print *, 'Opcion no válida'
                    end select
                end do

            case (2)
                do while (.not. isLoggedIn)
                    print *, 'Ingrese su usuario'
                    read *, dpiUser
                    print *, 'Ingrese su contraseña '
                    read *, passwordUser
                    !if (dpiUser .eq. 'frank' .and. passwordUser .eq. '1') then
                    !    isLoggedIn = .true.
                    print * , "Respusta ",respuesta
                    print * , "DPI: ",dpiUser," Password: ",passwordUser
                    call arbolClientes%login(dpiUser,passwordUser,respuesta)
                    print * , "Respusta ",respuesta
                    if (respuesta)then
                        isLoggedIn = .true.
                    else
                        print *, 'Usuario o contraseña incorrectos'
                    end if
                end do

                do while (.true.)
                    print *, 'SubMenu 2'
                    print *, '1. Cargar Jsons'
                    print *, '2. Gestion Imagenes'
                    print *, '3. Ver Estructuras'
                    print *, '4. Ver Reporte'
                    print *, '5. Salir'
                    read (dpiUser, *)dpiUserInt
                    read *, principal1

                    select case (principal1)
                        case (1)
                            do while (.true.)
                                print *, 'Menu Cargar Jsons'
                                print *, '1. Cargar Capas'
                                print *, '2. Cargar Albumes'
                                print *, '3. Cargar Imagenes'
                                print *, '4. Regresar'
                                read *, principal2

                                select case (principal2)
                                    case (1)
                                        print *, 'Cargar Capas'
                                        direccion = "capas.json"
                                        call cargaCapas(direccion)
                                    case (2)
                                        print *, 'Cargar Albumes'
                                        direccion = "albumes.json"
                                        call cargaAlbumes(direccion)
                                    case (3)
                                        print *, 'Cargar Imagenes'
                                        direccion = "imagenes.json"
                                        call cargaImagenes(direccion)
                                    case (4)
                                        exit
                                    case default
                                        print *, 'Opcion no válida'
                                end select
                            end do
                        case (2)
                            do while (.true.)
                                print *, 'Menu Crear Imagenes'
                                print *, '1. Preorden'
                                print *, '2. Inorden'
                                print *, '3. Postorden'
                                print *, '4. Ingresando capas'
                                print *, '5. A partir de una Imagen'
                                print *, '6. Eliminar una Imagen'
                                print *, '7. Regresar'
                                read *, principal2

                                select case (principal2)
                                    case (1)
                                        print *,'Crear Imagenes - Preorden'
                                        print *,'Ingrese el id de la imagen nueva'
                                        read *, idMN
                                        !idCliente-idImagenNueva-validacion
                                            call arbolClientes%validarID(dpiUserInt,idMN,validacion)
                                            if(validacion)then
                                                print *, "No existe el id"
                                                !creo mi nodo con ese id nuevo de mi imagen nueva
                                                !call clienteObject%avl%insert(2)
                                                 call arbolClientes%insertNodoImagen(dpiUserInt,idMN)
                                                 print *,"Ingrese el limite del recorrido"
                                                 read *, buscadorGeneral
                                                !Recorro el abb general y extraigo sus capas  id-matriz, mando lista-limite
                                                !call clienteObject%tree%GrapPreorder(listaTemporal,4)
                                                ! idCliente-listaTemporal-limite
                                                call arbolClientes%Preorden(dpiUserInt,listaTemporal,buscadorGeneral)
                                                !La listaTemporal viene con los id-matrices
                                                !Recorro la lista y extraigo la matriz de cada id junto a su id
                                                contadorLista = 0 
                                                do while (.true.)
                                                    contadorLista = contadorLista + 1
                                                    resultLista=.false.
                                                    call listaTemporal%existe(contadorLista,resultLista)
                                                    if (resultLista .eqv. .false.) then
                                                        exit
                                                    end if
                                                    call listaTemporal%cargarDatos(contadorLista,idLista,mtxTemporal2)
                                                    print *, "matrizExtraida"
                                                    !call mtxTemporal2%print()
                                                    !read *, desicion
                                                    !call clienteObject%avl%search(2,idLista,mtxTemporal2)
                                                    call arbolClientes%insertMatrizBB(dpiUserInt,idMN,idLista,mtxTemporal2)
                                                end do
                                                call arbolClientes%imagen(dpiUserInt,idMN)
                                                call listaTemporal%reset()
                                            end if
                                    case (2)
                                        print *, 'Crear Imagenes - Inorden'
                                        print *,'Ingrese el id de la imagen nueva'
                                        read *, idMN
                                            call arbolClientes%validarID(dpiUserInt,idMN,validacion)
                                            if(validacion)then
                                                print *, "No existe el id"
                                                !creo mi nodo con ese id nuevo de mi imagen nueva
                                                call arbolClientes%insertNodoImagen(dpiUserInt,idMN)
                                                print *,"Ingrese el limite del recorrido"
                                                 read *, buscadorGeneral
                                                !Recorro el abb general y extraigo sus capas  id-matriz, mando lista-limite
                                                !call clienteObject%tree%GrapInorden(listaTemporal,3)
                                                !call arbolClientes%Preorden(dpiUserInt,listaTemporal,buscadorGeneral)
                                                call arbolClientes%Inorden(dpiUserInt,listaTemporal,buscadorGeneral)
                                                !La listaTemporal viene con los id-matrices
                                                !Recorro la lista y extraigo la matriz de cada id junto a su id 
                                                contadorLista = 0
                                                do while (.true.)
                                                    contadorLista = contadorLista + 1
                                                    resultLista=.false.
                                                    call listaTemporal%existe(contadorLista,resultLista)
                                                    if (resultLista .eqv. .false.) then
                                                        exit
                                                    end if
                                                    call listaTemporal%cargarDatos(contadorLista,idLista,mtxTemporal2)
                                                    print *, "matrizExtraida"
                                                    !call mtxTemporal2%print()
                                                    !read *, desicion
                                                    !call clienteObject%avl%search(3,idLista,mtxTemporal2)
                                                    call arbolClientes%insertMatrizBB(dpiUserInt,idMN,idLista,mtxTemporal2)
                                                end do
                                                call arbolClientes%imagen(dpiUserInt,idMN)
                                                call listaTemporal%reset()
                                            else 
                                               print*,'El id que ingreso ya existe, intente de nuevo'
                                            end if
                                    case (3)
                                        print *, 'Crear Imagenes - Postorden'
                                        print *,'Ingrese el id de la imagen nueva'
                                        read *, idMN
                                            call arbolClientes%validarID(dpiUserInt,idMN,validacion)
                                            if(validacion)then
                                                print *, "No existe el id"
                                                !creo mi nodo con ese id nuevo de mi imagen nueva
                                                 call arbolClientes%insertNodoImagen(dpiUserInt,idMN)
                                                 print *,"Ingrese el limite del recorrido"
                                                 read *, buscadorGeneral
                                                !Recorro el abb general y extraigo sus capas  id-matriz, mando lista-limite
                                                !call clienteObject%tree%GrapPostOrden(listaTemporal,4)
                                                !call arbolClientes%Preorden(dpiUserInt,listaTemporal,buscadorGeneral)
                                                call arbolClientes%PostOrden(dpiUserInt,listaTemporal,buscadorGeneral)
                                                !La listaTemporal viene con los id-matrices
                                                !Recorro la lista y extraigo la matriz de cada id junto a su id 
                                                contadorLista = 0
                                                do while (.true.)
                                                    contadorLista = contadorLista + 1
                                                    resultLista=.false.
                                                    call listaTemporal%existe(contadorLista,resultLista)
                                                    if (resultLista .eqv. .false.) then
                                                        exit
                                                    end if
                                                    call listaTemporal%cargarDatos(contadorLista,idLista,mtxTemporal2)
                                                    print *, "matrizExtraida"
                                                    !call mtxTemporal2%print()
                                                    !read *, desicion
                                                    call arbolClientes%insertMatrizBB(dpiUserInt,idMN,idLista,mtxTemporal2)
                                                end do
                                                call arbolClientes%imagen(dpiUserInt,idMN)
                                                call listaTemporal%reset()
                                            end if 
                                    case (4)
                                        print *, 'Crear Imagenes - Ingresando capas'
                                        print *,'Ingrese el id de la imagen nueva'
                                        print *,'VAlidacion: ',validacion
                                        validacion=.false.
                                        read *, idMN
                                        call arbolClientes%validarID(dpiUserInt,idMN,validacion)
                                        if(validacion .eqv. .false.)then
                                        !Falta implementacion  respuestaIC  capasUsuario,cuAux
                                        call arbolClientes%insertNodoImagen(dpiUserInt,idMN)
                                        contadorLista=0
                                            do
                                                print*, 'Por favor, ingresa el ide de una Capa del ABB general'
                                                read*, capasUsuarioInt
                                                !Tengo la capa... le aviso si existe o no la capa y la agrego si si
                                                print*, 'Capa ingresada: ',capasUsuarioInt
                                                call arbolClientes%validarCapa(dpiUserInt,capasUsuarioInt,mtxTemporal2,existeCapa)
                                                if(existeCapa)then
                                                    print*, 'Capa encontrada'
                                                    !matriz ya viene segun la capa
                                                    call listaTemporal%push(contadorLista+1,capasUsuarioInt,mtxTemporal2)
                                                    existeCapa=.false.
                                                    print *, 'Error ========>'
                                                    call mtxTemporal2%print()
                                                else
                                                    print*, 'Capa no encontrada'
                                                end if
                                                print*, '¿Quieres agregar más números? (s/n)'
                                                read*, respuestaQ
                                                if (respuestaQ == 'n' .or. respuestaQ == 'N') exit
                                                contadorLista=contadorLista+1
                                            end do
                                            !aca ya tengo mi lista con las capas indicadas
                                            contadorLista = 0
                                                do while (.true.)
                                                    contadorLista = contadorLista + 1
                                                    resultLista=.false.
                                                    call listaTemporal%existe(contadorLista,resultLista)
                                                    if (resultLista .eqv. .false.) then
                                                        exit
                                                    end if
                                                    call listaTemporal%cargarDatos(contadorLista,idLista,mtxTemporal2)
                                                    call listaTemporal%print()
                                                    print *, "matrizExtraida --->"
                                                    print *, 'Impresion de matriz que fuer cargada'
                                                    print *, 'id ',contadorLista,'idCApa',idLista
                                                    call mtxTemporal2%print()
                                                    call arbolClientes%insertMatrizBB(dpiUserInt,idMN,idLista,mtxTemporal2)
                                                end do
                                                print*,"Empieza la creacion de la imagen----"
                                                print*, 'Usuario: ',dpiUserInt,' Id Imagen: ',idMN
                                                call arbolClientes%imagen(dpiUserInt,idMN)
                                                call listaTemporal%reset()
                                        else
                                            print*,'El id que ingreso ya existe, intente de nuevo'
                                        end if

                                            
                                    case (5)
                                        print *, 'Crear Imagenes - A partir de una imagen'
                                        print *,'Ingrese el id de la imagen nueva'
                                        read *, idMN
                                            call arbolClientes%validarID(dpiUserInt,idMN,validacion)
                                            if(validacion)then
                                                print *, "No existe el id"
                                                call arbolClientes%insertNodoImagen(dpiUserInt,idMN)
                                                print *,'Ingrese el limite'
                                                read *, buscadorGeneral
                                                print *,'Ingrese el id de la imagen de la cual se hara el recorrido'
                                                read *, AmplitudID
            
                                                call arbolClientes%cadena(dpiUserInt,AmplitudID,cadena)
                                                print *, "Cadena: ",cadena  
                                                posA=INDEX(cadena,'-')
                                                countA= 0
                                                do while (posA .NE. 0 .AND. countA <buscadorGeneral)
                                                    write(*,*) trim(adjustl(cadena(1:posA-1)))
                                                    capita=trim(adjustl(cadena(1:posA-1)))
                                                    print*,'Imprimiendo capita',capita
                                                    read(capita,*) capitaE
                                                    !recorro el abb de la imagen y extraigo sus matrices
                                                    print*, "voy a buscar la matriz del nodo"
                                                    call arbolClientes%oma(dpiUserInt,AmplitudID,capitaE,mtxTemporal2)
                                                    print *, "Impresin de la matriz qeu fui a buscar"  !!!
                                                    call mtxTemporal2%print()
                                                    !Aca mando mi matriz a mi lista para recorrerla despues
                                                    print*,'id ',countA,'idCApa',capitaE 
                                                    call listaTemporal%push(countA+1,capitaE,mtxTemporal2)
                                                    cadena=cadena(posA+1:)
                                                    posA=INDEX(cadena,'-')
                                                    countA = countA + 1
                                                end do
                                                print*, 'Segundo While ========================================================='
                                                call listaTemporal%print()
                                                print*, 'Segundo While ========================================================='
                                                    contadorLista = 0 
                                                do while (.true.)
                                                    contadorLista = contadorLista + 1
                                                    resultLista=.false.
                                                    call listaTemporal%existe(contadorLista,resultLista)
                                                    if (resultLista .eqv. .false.) then
                                                        exit
                                                    end if
                                                    call listaTemporal%cargarDatos(contadorLista,idLista,mtxTemporal2)
                                                    print *, "matrizExtraida"
                                                    print *, 'Impresion de matriz que fuer cargada'
                                                    call mtxTemporal2%print()
                                                    !call mtxTemporal2%print()
                                                    !read *, desicion
                                                    !call clienteObject%avl%search(2,idLista,mtxTemporal2)
                                                    call arbolClientes%insertMatrizBB(dpiUserInt,idMN,idLista,mtxTemporal2)
                                                end do
                                                print*,"Empieza la creacion de la imagen----"
                                                call arbolClientes%imagen(dpiUserInt,idMN)
                                                call listaTemporal%reset()
                                            end if 
                                    case (6)
                                        print *, 'Eliminar una Imagen'
                                        print *,'Ingrese el id de la imagen a eliminar'
                                        read*,buscadorGeneral
                                        call arbolClientes%eliminarImagen(dpiUserInt,buscadorGeneral)
                                    case (7)
                                        exit
                                    case default
                                        print *, 'Opcion no válida'
                                end select
                            end do
                        case (3)
                            print *, 'Visualizacion General de las estructuras del cliente'
                            do
                                print *, "1. Ver AVL de Imagenes"
                                print *, "2. Ver ABB de Capas"
                                print *, "3. Ver matriz en especifico"
                                print *, "4. Ver Imagen en especifico"
                                print *, "5. Ver lista Albumes"
                                print *, "6. Volver al menú principal"
                                read(*,*) opgc
                                select case (opgc)
                                    case (1)
                                        print *, "Grafica del avl de imagenes"
                                        call arbolClientes%grafica(dpiUserInt)
                                        
                                    case (2)
                                        print *, "Grafica del abb de capas"
                                        call arbolClientes%graficaCapas(dpiUserInt)
                                        
                                    case (3)
                                        print *, "Ingrese el id de la Matriz a graficar"
                                        read *,buscadorGeneral
                                        call arbolClientes%graficarMatriz(dpiUserInt,buscadorGeneral)
                                    case (4)
                                        print *, "Ingrese el id de la Imagen a graficar"
                                        read *,buscadorGeneral
                                        call arbolClientes%imagen(dpiUserInt,buscadorGeneral)
                                    case (5)
                                        call arbolClientes%graficarAlbumes(dpiUserInt)
                                    case (6)
                                        exit    
                                    case default
                                        print *, "Opción no válida, por favor seleccione nuevamente."
                                end select
                            end do

                        case (4)
                            print *, '================================================================='
                            print *, '======================== Reporte Cliente ========================'
                            print *, '================================================================='
                            call arbolClientes%top5(dpiUserInt) !AVL
                            print *, '================================================================='
                            call arbolClientes%hojas(dpiUserInt) !ABB
                            print *, '================================================================='
                            call arbolClientes%profundidadCapas(dpiUserInt) !ABB
                            print *, '================================================================='
                            call arbolClientes%recorridos(dpiUserInt) !ABB
                        case (5)
                            isLoggedIn=.false.
                            exit
                        case default
                            print *, 'Opcion no válida'
                    end select
                end do

            case (3)
                print *, 'Hasta luego.'
                exit

            case default
                print *, 'Opcion no válida'
        end select
    end do

contains 
subroutine cargaMasivaCliente(direccion)
        character(len=1000), intent(in) :: direccion
        
        call json%initialize()
       
        call json%load(filename=direccion)
        call json%info('',n_children=sizee)
        call json%get_core(jsonc)
        call json%get('', listPointer, found)

        do iC = 1, sizee
            call jsonc%get_child(listPointer, iC, animalPointer, found)

            call jsonc%get_child(animalPointer, 'dpi', attributePointer, found)
            call jsonc%get(attributePointer, dpi)

            call jsonc%get_child(animalPointer, 'nombre_cliente', attributePointer, found)
            call jsonc%get(attributePointer, nombreCliente)

            call jsonc%get_child(animalPointer, 'password', attributePointer, found) 
            call jsonc%get(attributePointer, password)
           
             !Inserto el id del cliente en el arbol
            read(dpi, *) dpiInt2
            call arbolClientes%exist(dpiInt2,existeC)
             call arbolClientes%insert(dpiInt2)
             !Busco ese id para setear datos del cliente
             call arbolClientes%search(dpiInt2,nombreCliente,password)
             !Name-DPI-Password
             call b%insert(ordinal_user(nombreCliente,dpiInt2, password))
             call listaOrden%insert(dpiInt2)
             !call arbolClientes%preorden()
            print *, "////////"
            print *, 'DPI: ',  dpi
            print *, 'Nombre Cliente: ', nombreCliente
            print *, 'Password: ', password

        end do
        call json%destroy()
end subroutine cargaMasivaCliente

subroutine cargaCapas(direccion)
        character(len=100), intent(in) :: direccion
        
        call json%initialize()
       
        call json%load(filename=direccion)

        call json%info('',n_children=sizee)
        call json%get_core(jsonc)
        call json%get('', listPointer, found) !obtengo cada trozo dentro de llaves

        do iC = 1, sizee
            call jsonc%get_child(listPointer, iC, animalPointer, found)

            call jsonc%get_child(animalPointer, 'id_capa', attributePointer, found)
            call jsonc%get(attributePointer, nombreAlbum)

            call jsonc%get_child(animalPointer, 'pixeles', attributePointer, found)
   
            call jsonc%info(attributePointer,n_children=sizeAlbumes)
            !Cargo el id de la capa
            read(nombreAlbum, *) idCapaE;
            read(dpiUser, *) dpiUserInt;
            !call clienteObject%tree%insert(idCapaE)
            !dpiActual-idCapa
            call arbolClientes%insertNodoCapa(dpiUserInt,idCapaE)
            
            do iCAlbumes = 1, sizeAlbumes
                call jsonc%get_child(attributePointer, iCAlbumes, todoAlbumes, found)

                call jsonc%get_child(todoAlbumes, 'fila', attributePointerCapas, found)
                call jsonc%get(attributePointerCapas, fila)

                call jsonc%get_child(todoAlbumes, 'columna', attributePointerCapas, found)
                call jsonc%get(attributePointerCapas, columna)

                call jsonc%get_child(todoAlbumes, 'color', attributePointerCapas, found)
                call jsonc%get(attributePointerCapas, color)
                !Cargo el color a ese id de la capa actual
                read(fila, *) filaE;
                read(columna, *) columnaE;
                !call clienteObject%tree%search(idCapaE,columnaE,filaE,color)
                !dpiActual-idCapa-fila-columna-color
                call arbolClientes%insertCapa(dpiUserInt,idCapaE,filaE,columnaE,color)
                print *, "----"
                print *, 'Id capa: ',  nombreAlbum
                print *, 'Pixeles '
                print *, 'Fila: ', fila
                print *, 'Columna: ', columna
                print *, 'Color: ', color
                

            end do

        end do
        call json%destroy()
end subroutine cargaCapas

subroutine cargaAlbumes(direccion)
        character(len=1000), intent(in) :: direccion
        integer :: imagenIntA
        
        call json%initialize()
       
        call json%load(filename=direccion)

        call json%info('',n_children=sizee)
        call json%get_core(jsonc)
        call json%get('', listPointer, found) !obtengo cada trozo dentro de llaves

        do iC = 1, sizee
            call jsonc%get_child(listPointer, iC, animalPointer, found)

            call jsonc%get_child(animalPointer, 'nombre_album', attributePointer, found)
            call jsonc%get(attributePointer, nombreAlbum)

            call jsonc%get_child(animalPointer, 'imgs', attributePointer, found)
   
            call jsonc%info(attributePointer,n_children=sizeAlbumes)

            call arbolClientes%agregarAlbumes(dpiUserInt, nombreAlbum)

            do iCAlbumes = 1, sizeAlbumes
                call jsonc%get_child(attributePointer, iCAlbumes, todoAlbumes, found)

                call jsonc%get(todoAlbumes, imgs)
                read (imgs, *) imagenIntA
               call arbolClientes%agregarImagenesAlbum(dpiUserInt,nombreAlbum,imagenIntA)
                print *, "----"
                print *, 'Nombre Album: ',  nombreAlbum
                print *, 'Imgs: ', imgs

            end do

        end do
        call json%destroy()
end subroutine cargaAlbumes

subroutine cargaImagenes(direccion)
        character(len=1000), intent(in) :: direccion
        
        call json%initialize()
       
        call json%load(filename=direccion)

        call json%info('',n_children=sizee)
        call json%get_core(jsonc)
        call json%get('', listPointer, found) !obtengo cada trozo dentro de llaves

        do iC = 1, sizee
            call jsonc%get_child(listPointer, iC, animalPointer, found)

            call jsonc%get_child(animalPointer, 'id', attributePointer, found)
            call jsonc%get(attributePointer, idImagen)

            call jsonc%get_child(animalPointer, 'capas', attributePointer, found)
   
            call jsonc%info(attributePointer,n_children=sizeAlbumes)

            !Inserto el nodo al avl del cliente
            read(idImagen, *) idImagenE;
            read(dpiUser, *) dpiUserInt
            !call clienteObject%avl%insert(idImagenE)
            call arbolClientes%insertNodoImagen(dpiUserInt,idImagenE)
 

            do iCAlbumes = 1, sizeAlbumes
                call jsonc%get_child(attributePointer, iCAlbumes, todoAlbumes, found)
                call jsonc%get(todoAlbumes, capasImagenes)

            
                print *, "----"
                print *, 'Id Imagen: ',  idImagen
                print *, 'Capas: ', capasimagenes 
                !Busco y extraigo la matriz del id actual
                read(capasimagenes, *) capaImagenE;
                !call clienteObject%tree%extraerMatriz(capaImagenE,mtxTemporal)
                !dpiActual-idCapa-matriz(de esa capa)
                call arbolClientes%extraerM(dpiUserInt,capaImagenE,mtxTemporal)
                !mtxTemporal contiene la matriz de la capa con id actual
                !Inserto la matriz al abb del avl
                !call clienteObject%avl%search(idImagenE,capaImagenE,mtxTemporal)
                !dpiActual-idImagen-idCapa-matriz
                call arbolClientes%insertMatriz(dpiUserInt,idImagenE,capaImagenE,mtxTemporal)
                print*, 'Matriz de imagen cargada a abb'
            end do

        end do
        call json%destroy()
end subroutine cargaImagenes

end program main
