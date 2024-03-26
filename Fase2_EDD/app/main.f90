program main
    use avl_c
    use matrix_m
    use json_module
    use iso_fortran_env, only:
    use clienteTemporal
    use avl_m
    use lista_module
    
    implicit none
    type(avlc) :: arbolClientes

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

    integer :: dpiInt,dpiInt2, buscadorGeneral,dpiUserInt,opgc,idMN,AmplitudID,posA,countA,capitaE
    logical :: respuesta=.false.
    character(len=:), allocatable :: cadena,capita
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
                                    case (2)
                                        print *, 'Modificar'
                                    case (3)
                                        print *, 'Eliminar'
                                    case (4)
                                        exit
                                    case default
                                        print *, 'Opcion no válida'
                                end select
                            end do
                        case (4)
                            print *, 'Reporte Administrador'
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
                    print *, '2. Crear Imagenes'
                    print *, '3. Ver Estructuras'
                    print *, '4. Salir'
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
                                print *, '6. Regresar'
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
                                            end if 
                                    case (4)
                                        print *, 'Crear Imagenes - Ingresando capas'
                                        !Falta implementacion
                                    case (5)
                                        print *, 'Crear Imagenes - A partir de una imagen'
                                        print *,'Ingrese el id de la imagen nueva'
                                        read *, idMN
                                            call arbolClientes%validarID(dpiUserInt,idMN,validacion)
                                            if(validacion)then
                                                print *, "No existe el id"
                                                print *,'Ingrese el limite'
                                                read *, buscadorGeneral
                                                print *,'Ingrese el id de la imagen de la cual se hara el recorrido'
                                                read *, AmplitudID
            
                                                call arbolClientes%cadena(dpiUserInt,AmplitudID,cadena)

                                                posA=INDEX(cadena,'-')
                                                countA= 0
                                                do while (posA .NE. 0 .AND. countA <buscadorGeneral)
                                                    write(*,*) trim(adjustl(cadena(1:posA-1)))
                                                    capita=trim(adjustl(cadena(1:posA-1)))
                                                    print*,'Imprimiendo capita',capita
                                                    read(capita,*) capitaE
                                                    !recorro el abb de la imagen y extraigo sus matrices
                                                    call arbolClientes%oma(dpiUserInt,idMN,capitaE,mtxTemporal2)
                                                    !Aca mando mi matriz a mi lista para recorrerla despues 
                                                    call listaTemporal%push(countA+1,capitaE,mtxTemporal2)
                                                    cadena=cadena(posA+1:)
                                                    posA=INDEX(cadena,'-')
                                                    countA = countA + 1
                                                end do
                                                !    contadorLista = 0 
                                                !do while (.true.)
                                                !    contadorLista = contadorLista + 1
                                                !    resultLista=.false.
                                                !    call listaTemporal%existe(contadorLista,resultLista)
                                                !    if (resultLista .eqv. .false.) then
                                                !        exit
                                                !    end if
                                                !    call listaTemporal%cargarDatos(contadorLista,idLista,mtxTemporal2)
                                                !    print *, "matrizExtraida"
                                                    !call mtxTemporal2%print()
                                                    !read *, desicion
                                                    !call clienteObject%avl%search(2,idLista,mtxTemporal2)
                                                !    call arbolClientes%insertMatrizBB(dpiUserInt,idMN,idLista,mtxTemporal2)
                                                !end do
                                                !call arbolClientes%imagen(dpiUserInt,idMN)
                                            end if 
                                    case (6)
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
                                print *, "5. Volver al menú principal"
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
                                    case (4)
                                        print *, "Ingrese el id de la Imagen a graficar"
                                        read *,buscadorGeneral
                                        call arbolClientes%imagen(dpiUserInt,buscadorGeneral)
                                    case (5)
                                        !Sale del menu de visualizacion
                                        exit    
                                    case default
                                        print *, "Opción no válida, por favor seleccione nuevamente."
                                end select
                            end do

                        case (4)
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
!======================================================================================================================================
!======================================================================================================================================
!direccion="clientes.json"
!    call cargaMasivaCliente(direccion)
!direccion="capas.json"
!    call cargaCapas(direccion)
!    print *, '..............................................>'
!    print *, '..............................................>'
!    print *, '..............................................>'
!direccion='imagenes.json'
!    call cargaImagenes(direccion)
!print *, '..............................................>'
!    call arbolClientes%grafica(1)
!    print *, 'Grafica del avl del cliente1'
!    call arbolClientes%imagen(1,3)

!print *,'Ahora a graficar en preorden limite 3, la imagen id 1, del cliente id-1'
!read *, desicion
!    call arbolClientes%graficarPreorden(1,3)
!=================================================================================================================
!======================================================================================================================================

    !call clienteObject%avl%newTree()
    !do i = 1, size(keys)
    !    call clienteObject%avl%insert(keys(i))
    !end do
    !call clienteObject%avl%generateGraph()

    !call clienteObject%tree%insert(5);
    !call clienteObject%tree%insert(6)
    !call clienteObject%tree%insert(3)
    !call clienteObject%tree%graph("ABB");

    !call clienteObject%tree%search(5,2,2,"#B22222")
    !call clienteObject%tree%grapEspecifico(5)
    !read *, desicion
    !call clienteObject%tree%search(5,2,2,"#B55555")
    !call clienteObject%tree%grapEspecifico(5)

    !direccion="MU.json"
    !call cargaCapas(direccion)
    !call clienteObject%tree%graph("ABB")
    !call clienteObject%tree%grapEspecifico(0)

!===============================================================================================================================
    !UNIR N MATRICES DISPERSAS  
    !Inserto o cargo capas, en este caso 10,5,15 ids
    !call clienteObject%tree%insert(10)
    !call clienteObject%tree%insert(5)
    !call clienteObject%tree%insert(15)

    !Agrego pixeles a la matriz de la capa 15
    !call clienteObject%tree%search(15,1,1,"#FF0000")
    !call clienteObject%tree%search(15,2,2,"#B22222")

    !Jalo la matriz con id x del arbol de capas para pasarlo al abb del avl
    !call clienteObject%tree%extraerMatriz(15,mtx)
    !call mtx%print()
    !tengo la matriz temporal la paso a abb del avl
    !primero agrego los nodos al avl
    !call a%insert(20)
    !call a%insert(10)
    !call a%insert(29)
    !Busco un nodo de los insertadors y le mando la matriz

    !idImagen-IdCapa-matriz
    !call a%search(20,15,mtx)
    !call a%search(20,11,mtx)
    
    !call mtx2%insert(10,10,.true.,"#000000")
    !call mtx2%graficar()
    !call a%search(20,18,mtx2)
    !print * , ""
    !print *, "==================================="
    !call a%abbImagen(20)
    !read *, desicion
    !idImagen-IdCapa-matriz
    !call a%search(29,25,mtx)
    !call a%search(29,11,mtx)
    !call a%search(29,38,mtx)
    !print * , ""
    !print *, "==================================="
    !call a%abbImagen(29)
    !idImagen-IdCapa-matriz
    !call a%search(10,25,mtx)
    !call a%search(10,11,mtx)
    !call a%search(10,38,mtx)
    !print * , ""
    !print *, "==================================="
    !call a%abbImagen(10)
    !call a%graficar()

    !Crear imagen
    !print * , "Crear Imagen"
    !print * , "@@@@@@@@@@@@@@@@@@@@@@@@@"
    !call a%crearImagen(20)
!===============================================================================================================================

    direccion="capas.json"
    call cargaCapas(direccion)
    print *, "= Cargo capas ="
    direccion="unionMario.json"
    call cargaImagenes(direccion)
    call clienteObject%avl%graficar()
    print *, "========================== IMAGEN PREORDER LIMITE 4 ========================="
    !solo creo la imagen a partir de lo que le mando pero aun no  se guarda en el avl como tal
    !En lugar de eso solo la sustituye, a partir de la que hago la nueva 
    !Call clienteObject%avl%crearImagen(1)
    !read *, desicion
    !Call clienteObject%avl%abbImagen(1)
    !Primero inserto el id de la imagen que voy a crear
    !Verifico sino existe ya ese id
    call clienteObject%avl%validarID(2,validacion)
    if(validacion)then
        print *, "No existe el id"
        !creo mi nodo con ese id nuevo de mi imagen nueva
        call clienteObject%avl%insert(2)
        !Recorro el abb general y extraigo sus capas  id-matriz, mando lista-limite
        call clienteObject%tree%GrapPreorder(listaTemporal,4)
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
            call clienteObject%avl%search(2,idLista,mtxTemporal2)
        end do

    else
        !Call clienteObject%avl%imagenPostOrden(1,2)
    end if
   
    call clienteObject%avl%graficar()
    call clienteObject%avl%crearImagen(2)
    call clienteObject%avl%abbImagen(2)
    call clienteObject%avl%crearImagen(2)
    print*,'======================== INORDEN =================================='
    read *, desicion    
    call clienteObject%avl%validarID(3,validacion)
    if(validacion)then
        print *, "No existe el id"
        !creo mi nodo con ese id nuevo de mi imagen nueva
        call clienteObject%avl%insert(3)
        !Recorro el abb general y extraigo sus capas  id-matriz, mando lista-limite
        call clienteObject%tree%GrapInorden(listaTemporal,3)
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
            call clienteObject%avl%search(3,idLista,mtxTemporal2)
        end do

    else
        !Call clienteObject%avl%imagenPostOrden(1,2)
    end if
   
    call clienteObject%avl%graficar()
    call clienteObject%avl%crearImagen(3)
    call clienteObject%avl%abbImagen(3)
    call clienteObject%avl%crearImagen(3)

    print*,'======================== POSTORDEN =================================='
    read *, desicion
    call clienteObject%avl%validarID(4,validacion)
    if(validacion)then
        print *, "No existe el id"
        !creo mi nodo con ese id nuevo de mi imagen nueva
        call clienteObject%avl%insert(4)
        !Recorro el abb general y extraigo sus capas  id-matriz, mando lista-limite
        call clienteObject%tree%GrapPostOrden(listaTemporal,4)
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
            call clienteObject%avl%search(4,idLista,mtxTemporal2)
        end do

    else
        !Call clienteObject%avl%imagenPostOrden(1,2)
    end if
   
    call clienteObject%avl%graficar()
    call clienteObject%avl%crearImagen(4)
    call clienteObject%avl%abbImagen(4)
    call clienteObject%avl%crearImagen(4)





    !print * , "Carga de imagenes"
    !Carga de imagenes
    !direccion="imagenes.json"
    !call cargaImagenes(direccion)

    !Ya puedo copiar y tengo capas en el abb de cada idImagen
    !toca unir todas las que tenga el abb
    

    !call a%insert(20)
    !call a%insert(10)
    !call a%insert(29)
    !call a%insert(8)
    !call a%insert(19)

    !print *, "Imprimiendo en preorden: "
    !call a%preorden()
    !call a%graficar()

    !call a%search(39,alvF)
    !print *, "Encontrado: ", alvF



    !call clienteObject%tree%insert(5)
    !call clienteObject%tree%search(5,1,1,"#B55555")
    !call clienteObject%tree%insert(1)
    !call clienteObject%tree%insert(17)
    !call clienteObject%tree%insert(3)
    !call clienteObject%tree%insert(8)
    !call clienteObject%tree%insert(15)
    !call clienteObject%tree%insert(7)
    !call clienteObject%tree%insert(6)
    !call clienteObject%tree%insert(22)

    !call clienteObject%tree%graph("inserted")
    !write(*, '(A)') "Escribiendo en preorden: "
    !call clienteObject%tree%preorder()
    !write(*, '(A)') "Escribiendo en inorder: "
    !call clienteObject%tree%inorder()
    !!print *, "Escribiendo en posorden: "
    !call clienteObject%tree%posorder()

    
    !call clienteObject%tree%search(3,2,2,"#B22222")
    !call clienteObject%tree%search(3,4,4,"#B22222")


    !call clienteObject%tree%grapEspecifico(5)
    !read *, desicion
    !call clienteObject%tree%grapEspecifico(3)
    !    read *, desicion
    !call clienteObject%tree%grapEspecifico(5)
    !direccion="capas.json"
    !call cargaCapas(direccion)
    !call clienteObject%tree%graph("inserted")

    !call clienteObject%tree%grapEspecifico(5)
    !read *, del
    !call clienteObject%tree%grapEspecifico(4)
    !read *, del
    !call clienteObject%tree%grapEspecifico(1)
    !read *, del
    !call clienteObject%tree%grapEspecifico(4)



    !print*, "==================================="
    !print*, "Carga masiva de clientes"
    !direccion="clientes.json"
    !call cargaMasivaCliente(direccion)
    !print*, "==================================="
    !print*, "Carga masiva de capas"
    !direccion="capas.json"
    !call cargaCapas(direccion)
    !print *, "==================================="
    !print *, "Carga masiva de albumes"
    !direccion="albumes.json"
    !call cargaAlbumes(direccion)
    !print *, "==================================="
    !print *, "Carga masiva de imagenes"
    !direccion="imagenes.json"
    !call cargaImagenes(direccion)

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
             call arbolClientes%insert(dpiInt2)
             !Busco ese id para setear datos del cliente
             call arbolClientes%search(dpiInt2,nombreCliente,password)
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
            
            do iCAlbumes = 1, sizeAlbumes
                call jsonc%get_child(attributePointer, iCAlbumes, todoAlbumes, found)

                call jsonc%get(todoAlbumes, imgs)

            
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
