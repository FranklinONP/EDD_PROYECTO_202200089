program main
    use json_module
    use modulo_arbol_avl
    use modulo_tabla_hash
    use ListaDeAdyacencia
    use merkle_m
    !merkle_m
    implicit none
    !DECLARACION PARA LAS RUTAS 
    type(ListaAdyacencia) :: grafo

    type(merkle) :: merk
    character(len=:), allocatable :: valorMerkle


    !LECTURA JSON
    type(json_file) :: json
    type(json_core) :: jsonc

    !LECTURA SUCURSAL
    type(json_value), pointer :: lista_puntero_s, puntero_s, atributo_puntero_s
    logical :: sucursal_encontrada
    integer :: size_sucursal, contador_surcusal
    character(:), allocatable :: id_s, direccion_s, departamento_s, password_s

    !LECTURA TECNICO
    type(json_value), pointer :: lista_puntero_t, puntero_t, atributo_puntero_t
    logical :: tecnico_encontrado
    integer :: size_tecnico, contador_tecnico
    character(:), allocatable :: dpi_t, nombre_t, apellido_t, genero_t, direccion_t, telefono_t
    
    !LECTURA RUTAS
    type(json_value), pointer :: lista_puntero_r, puntero_r, atributo_puntero_r
    type(json_value), pointer :: puntero_aux, atributo_puntero_aux
    logical :: grafo_encontrado
    integer :: size_grafo, contador_grafo
    integer :: size_ruta, contador_ruta
    character(:), allocatable :: s1, s2, distancia, imp_mantenimiento
    integer :: s1Int, s2Int, distanciaInt, imp_mantenimientoInt

    !ESTRUCTURAS
    type(arbol_avl) :: arbol_avl_sucursal  

    !VARIABLES GLOBALES
    integer :: opcion_principal, id_s_int
    integer(8) ::  dpi_t_int, telefono_t_int
    character(len=100) :: usuario
    character(len=100) :: contrasena
    character(len=100) :: documento_sucursal, documento_grafo, documento_tecnico

    integer :: idSucursalActual

    !Para ruta minima
    integer:: distanciaD
    integer:: impresionesD
    integer, dimension(:), pointer :: rutaDikstra

    !Para ruta maxima
    integer:: distanciaMax
    integer:: impresionesMax
    integer, dimension(:), pointer :: rutaDikstraMax

    integer :: costoDisktra, costoDisktraMax, elc
    integer :: i

    character(len=100) :: aux1, aux2,finalM,aux11,aux22

    do
        call mostrar_menu()
        read(*,*) opcion_principal
        select case(opcion_principal)
            case(1)
                call iniciar_sesion()
            case(2)
                exit
            case default
                print *, "OPCION INVALIDA"
        end select
    end do

    contains
    subroutine mostrar_menu()
        print *, "---------------------------------------"
        print *, "Menu Principal - Pixel Print Studio"
        print *, "1. Iniciar Sesion"
        print *, "2. Salir"
        print *, "---------------------------------------"
        print *, "Seleccione El Numero De Opcion:"
        print *, "---------------------------------------"
    end subroutine mostrar_menu

    subroutine iniciar_sesion()
        print *, "---------------------------------------"
        print *, "INICIAR SESION"   
        print *, "---------------------------------------"
        print *, "Ingrese su nombre de usuario:"
        read(*,*) usuario
        print *, "Ingrese su contrasenia:"
        read(*,*) contrasena
        print *, "usuario:  EDD1S2024"
        print *, "contrasena: ProyectoFase3"
        if(usuario == "EDD1S2024" .and. contrasena == "ProyectoFase3")then
            print *, "---------------------------------------"
            print*,"BIENVENIDO ADMINISTRADOR"
            call menu_administrador()
        else
            print *, "CREDENCIALES INCORRECTAS"
        end if
    end subroutine iniciar_sesion

    subroutine menu_administrador()
        integer :: opcion_admin
        do
            print *, "---------------------------------------"
            print *, " PIXE PRINT STUDIO - ADMINISTRADOR"
            print *, "1. CARGA ARCHIVOS"
            print *, "2. ACCESO A SUCURSALES"
            print *, "3. REPORTES GRAFICOS"
            print *, "4. DATOS DE LA EMPRESA"
            print *, "5. CERRAR SESION"
            print *, "---------------------------------------"
            print *, "Seleccione El Numero De Opcion:"
            print *, "---------------------------------------"
            read(*,*) opcion_admin
            select case(opcion_admin)
                case(1)
                    call carga_masiva()
                case(2)
                    call manejo_sucursal()
                case(3)
                    call reportes_graficos()
                case(4)
                    call reporte_datos_empresa()
                case(5)
                    exit
                case default
                    print *, "OPCION INVALIDA"
            end select
        end do
    end subroutine menu_administrador

    subroutine carga_masiva()
        integer :: opcion_carga
        do
            print *, "|-----------------------------------------|"
            print *, "|Menu de Carga Masiva - Pixel Print Studio|"
            print *, "|1. Sucursales                            |"
            print *, "|2. Rutas                                 |"
            print *, "|3. Regresar Al Menu Principal            |"
            print *, "|-----------------------------------------|"
            print *, "|Seleccione El Numero De Opcion:          |"  
            print *, "|-----------------------------------------|"
            read(*,*) opcion_carga
            select case(opcion_carga)
                case(1)
                    call carga_masiva_sucursal()
                    print*,"Carga De Sucursales Correctamente."
                case(2)
                    call carga_masiva_ruta()
                    print*,"Carga De Rutas Correctamente."
                case(3)
                    exit
                case default
                    print *, "OPCION INVALIDA"
            end select
        end do
    end subroutine carga_masiva

    subroutine manejo_sucursal()
        integer :: opcion_carga, id_sucursal
        character(len=100) :: contrasena
        logical :: existe_matriz
        integer :: idSucursalFinal
        print *, "---------------------------------------"
        print *, "LOGIN SUCURSALES"
        print *, "---------------------------------------"
        print *, "Escribe el ID de la sucursal:"
        read(*,*) id_sucursal
        idSucursalActual = id_sucursal
        print *, "Escribe la contrasena de la sucursal:"
        read(*,*) contrasena
        existe_matriz = arbol_avl_sucursal%valor_existe(id_sucursal, contrasena)
        if(existe_matriz)then
            do
                print *, "---------------------------------------"
                print *, "Bienvenido Sucursal: ", id_sucursal
                print *, "---------------------------------------"
                print *, "Menu de Manejo Sucursal - Pixel Print Studio"
                print *, "1. Carga De Tecnicos"
                print *, "2. Generar Recorrido Mas Optimo"
                print *, "3. Informacion Tecnico En Especifico"
                print *, "4. Listar Tecnicos"
                print *, "5. Regresar Al Menu Principal"
                print *, "****************************************"
                print *, "Seleccione:"
                print *, "****************************************"
                read(*,*) opcion_carga
                select case(opcion_carga)
                    case(1)
                        call carga_masiva_tecnico(id_sucursal, contrasena)
                    case(2)
                        print*,"Recorrido Más Optimo"
                        print *, "Ingrese el id de la sucursal final:"
                        read(*,*) idSucursalFinal
                        print *, "Calculando Ruta Más Optima..."
                        print *, "Ruta optima calculada."
                        print *, "======================================================================"
                        print *, "======================================================================"
                        print *, "======================================================================"
                        print *, "**********Calculando el camino mas corto en base a Distancia**********"
                        call grafo%dikstraDistancia(idSucursalActual, idSucursalFinal, distanciaD,impresionesD, rutaDikstra)
                        print *, "Imprieniendo obtenido en Dikstra"
                        print *, "Ruta: ", rutaDikstra
                        print *, "Distancia: ", distanciaD
                        print *, "Impresiones: ", impresionesD
                        print *, "                        ============="
                        print *, "********Calculando el camino mas caro en base a Mantenimiento*********"
                        call grafo%dijkstra_max(idSucursalActual, idSucursalFinal,distanciaMax,impresionesMax, rutaDikstraMax)
                        print *, "Imprimiendo en Dikstra Max"
                        print *, "Ruta: ", rutaDikstraMax
                        print *, "Distancia: ", distanciaMax
                        print *, "Impresiones: ", impresionesMax
                        print *, "======================================================================"
                        print *, "Ruta Recomendada"
                        costoDisktra = impresionesD*100-distanciaD *80
                        costoDisktraMax = impresionesMax * 100-distanciaMax * 80
                        print *, "Ganancia con Dikstra: ", costoDisktra
                        print *, "Ganancia con Dikstra Max: ", costoDisktraMax
                        if(costoDisktra < costoDisktraMax)then
                            print *, "Ruta Recomendada: ", rutaDikstraMax
                            print *, "Costo: ", costoDisktraMax
                            elc = costoDisktraMax - costoDisktra
                            !Hago el merk
                            do i = 2, size(rutaDikstraMax)
                                    print *,"s1: ", rutaDikstraMax(i-1)
                                    print *,"s2: ", rutaDikstraMax(i)
                                    write(aux11, '(I100)') rutaDikstraMax(i-1)
                                    write(aux22, '(I100)') rutaDikstraMax(i)
                                    valorMerkle =trim("s1:")//trim(adjustl(aux11))//trim("s2:")//trim(adjustl(aux22))
                                    print *, "Valor Merkle: ", valorMerkle
                                    call merk%agregar(valorMerkle)
                                end do
                            call merk%generar()

                        else
                            print *, "Ruta Recomendada: ", rutaDikstra
                            print *, "Costo: ", costoDisktra
                            elc = costoDisktra - costoDisktraMax
                            !Hago el merk
                            do i = 2, size(rutaDikstra)
                                    print *,"s1: ", rutaDikstra(i-1)
                                    print *,"s2: ", rutaDikstra(i)
                                    write(aux11, '(I100)') rutaDikstra(i-1)
                                    write(aux22, '(I100)') rutaDikstra(i)
                                    valorMerkle =trim("s1:")//trim(adjustl(aux11))//trim("s2:")//trim(adjustl(aux22))
                                    print *, "Valor Merkle: ", valorMerkle
                                    call merk%agregar(valorMerkle)
                                end do
                            call merk%generar()
                        end if
                        !Ruta Recomendada
                        print *, "======================== GANANCIAS Y COSTOS =========================="
                        print *, "Mediante la ruta minima en Km"
                        print *, "Costo de la ruta: ", costoDisktra
                        print *, "Mediante la ruta de mayor mantenimiento"
                        print *, "Costo de la ruta: ", costoDisktraMax
                        print *, "======================================================================"
                        print *, "Ganancia con la eleccion"
                        print *, "Ganancia: ", elc
                        print *, "======================================================================"
                        print *, "======================================================================"
                        print *, "======================================================================"


                    case(3)
                        call informacion_tecnico_especifico(id_sucursal, contrasena)
                    case(4)
                        call listar_informacion_tecnico(id_sucursal, contrasena)
                    case(5)
                        exit
                    case default
                        print *, "OPCION INVALIDA"
                end select
            end do
        else 
            print*, "Credenciales De Sucursal Incorrectas."
        end if
    end subroutine manejo_sucursal

    subroutine reportes_graficos()
        integer :: opcion_carga, id_sucursal
        character(len=100) :: contrasena
        logical :: existe_matriz
        do
            print *, "---------------------------------------"
            print *, "Menu de Reportes Graficos - Pixel Print Studio"
            print *, "1. Grafo De Sucursales Y Sus Rutas"
            print *, "2. Arbol De Sucursales"
            print *, "3. Tabla Hash Tecnico"
            print *, "4. Visualizacion del Arbol de Merkle"
            print *, "5. Regresar Al Menu Principal"
            print *, "****************************************"
            print *, "Seleccione:"
            print *, "****************************************"
            read(*,*) opcion_carga
            select case(opcion_carga)
                case(1)
                    call graficaRutas()
                case(2)
                    call arbol_avl_sucursal%graficar_arbol("Arbol_Sucursales")
                case(3)
                    print *, "---------------------------------------"
                    print *, "CREDENCIALES SUCURSALES"
                    print *, "---------------------------------------"
                    print *, "Escribe el ID de la sucursal:"
                    read(*,*) id_sucursal
                    print *, "Escribe la contrasena de la sucursal:"
                    read(*,*) contrasena
                    existe_matriz = arbol_avl_sucursal%valor_existe(id_sucursal, contrasena)
                    if(existe_matriz)then
                        call grafica_tabla_hash(id_sucursal, contrasena)
                    else
                        print*, "Credenciales De Sucursal Incorrectas."
                    end if
                case(4)
                    call merk%dot()
                case(5)
                    exit
                case default
                    print *, "OPCION INVALIDA"
            end select
        end do
    end subroutine reportes_graficos

    subroutine reporte_datos_empresa()
        integer :: opcion_carga
        do
            print *, "---------------------------------------"
            print *, "Menu de Datos de la Empresa - Pixel Print Studio"
            print *, "1. Top 5 Tecnicos Con Mas Trabajos Realizados"
            print *, "2. Top 5 Sucursales Con Mas Trabajos Colicitados."
            print *, "3. Ganancias.Costos.Ganancias Totales."
            print *, "4. Costos"
            print *, "5. Ganancias Totales."
            print *, "6. Ruta De Viaje Por Trabajo Realizado"
            print *, "7. Regresar Al Menu Principal"
            print *, "****************************************"
            print *, "Seleccione:"
            print *, "****************************************"
            read(*,*) opcion_carga
            select case(opcion_carga)
                case(1)
                    print*, "Reporte 1."
                case(2) 
                    print*, "Reporte 2."
                case(3) 
                    print*, "Reporte 3."
                case(4) 
                    print*, "Reporte 4."
                case(5) 
                    print*, "Reporte 5."
                case(6) 
                    print*, "Reporte 6."
                case(7)
                    exit
                case default
                    print *, "OPCION INVALIDA"
            end select
        end do
    end subroutine reporte_datos_empresa

    subroutine informacion_tecnico_especifico(id_sucursal, contrasena)
        type(nodo_avl), pointer :: sucursal_actual
        integer(8) :: dpi_int
        integer, intent(in) :: id_sucursal
        character(len=*), intent(in) :: contrasena
        sucursal_actual => arbol_avl_sucursal%obtener_nodo(id_sucursal, contrasena)
        print *, "---------------------------------------"
        print *, "TECNIFO EN ESPECIFICO"
        print *, "---------------------------------------"
        print *, "Ingrese el DPI del tecnico:"
        print *, "---------------------------------------"
        read(*,*) dpi_int
        call sucursal_actual%tabla%imprimir(dpi_int)
    end subroutine informacion_tecnico_especifico

    subroutine listar_informacion_tecnico(id_sucursal, contrasena)
        type(nodo_avl), pointer :: sucursal_actual
        integer, intent(in) :: id_sucursal
        character(len=*), intent(in) :: contrasena
        sucursal_actual => arbol_avl_sucursal%obtener_nodo(id_sucursal, contrasena)
        print *, "---------------------------------------"
        print *, "LISTAR INFORMACION TECNICOS"
        print *, "---------------------------------------"
        call sucursal_actual%tabla%listar_tecnico()
    end subroutine listar_informacion_tecnico

    subroutine grafica_tabla_hash(id_sucursal, contrasena)
        type(nodo_avl), pointer :: sucursal_actual
        integer, intent(in) :: id_sucursal
        character(len=*), intent(in) :: contrasena
        character(len=32) :: id_sucursal_str
        sucursal_actual => arbol_avl_sucursal%obtener_nodo(id_sucursal, contrasena)
        write(id_sucursal_str, '(I0)') id_sucursal
        print *, "---------------------------------------"
        print *, "GRAFICA TABLA HASH TECNICO"
        print *, "---------------------------------------"
        !call sucursal_actual%tabla%grafica_tabla("Sucursal_"//id_sucursal_str)
        call sucursal_actual%tabla%graphTecnicos()
    end subroutine grafica_tabla_hash

    subroutine graficaRutas()
        print *, "---------------------------------------"
        print *, "GRAFO DIRIGIDO DE LAS SUCURSALES"
        print *, "---------------------------------------"
        call grafo%crearGrafo()
    end subroutine graficaRutas
!------------------------------------------------------------------------
!CARGA MASIVA SUCURSALES
!------------------------------------------------------------------------
    subroutine carga_masiva_sucursal()
        print *, "---------------------------------------"

        !actual => arbol_avl_sucursal%cabeza()
        
        print *, "CARGA MASIVA SUCURSALES"
        print *, "---------------------------------------"
        print *, "Ingrese la ruta del json de SUCURSALES:"
        print *, "---------------------------------------"
        read(*,*) documento_sucursal
        print *, "---------------------------------------"
        call json%initialize()
        call json%load(filename=documento_sucursal)
        call json%info('',n_children=size_sucursal)
        call json%get_core(jsonc)
        call json%get('', lista_puntero_s, sucursal_encontrada)
        do contador_surcusal = 1, size_sucursal
            call jsonc%get_child(lista_puntero_s, contador_surcusal, puntero_s, sucursal_encontrada)
            call jsonc%get_child(puntero_s, 'id', atributo_puntero_s, sucursal_encontrada)
            call jsonc%get(atributo_puntero_s, id_s)
            call jsonc%get_child(puntero_s, 'departamento', atributo_puntero_s, sucursal_encontrada)
            call jsonc%get(atributo_puntero_s, departamento_s)
            call jsonc%get_child(puntero_s, 'direccion', atributo_puntero_s, sucursal_encontrada)
            call jsonc%get(atributo_puntero_s, direccion_s)
            call jsonc%get_child(puntero_s, 'password', atributo_puntero_s, sucursal_encontrada)
            call jsonc%get(atributo_puntero_s, password_s)
            read(id_s, *) id_s_int
            print *, "Procesando Sucursal: ", id_s
            call arbol_avl_sucursal%insertar_nodo(id_s_int, departamento_s, direccion_s, password_s)
            !Agregue esta linea...cada sucursal sera un nodo dentro del grafo
            !call arbol_avl_sucursal%grafo%insert(id_s_int)
            !call actual%grafo%insert(id_s_int)
            !call grafo%insert(id_s_int)
            !call grafo%insertar_nodo(id_s_int)
            call grafo%insert(id_s_int)
        end do
        call json%destroy()
    end subroutine carga_masiva_sucursal
!------------------------------------------------------------------------
!CARGA MASIVA TECNICOS
!------------------------------------------------------------------------
    subroutine carga_masiva_tecnico(id_sucursal, contrasena)
        type(nodo_avl), pointer :: sucursal_actual
        integer, intent(in) :: id_sucursal
        integer :: i
        character(len=*), intent(in) :: contrasena
        sucursal_actual => arbol_avl_sucursal%obtener_nodo(id_sucursal, contrasena)
        print *, "---------------------------------------"
        print *, "CARGA MASIVA TECNICOS"
        print *, "---------------------------------------"
        print *, "INGRESE LA RUTA DEL JSON DE TECNICOS:"
        print *, "---------------------------------------"
        read(*,*) documento_grafo
        print *, "---------------------------------------"
        call json%initialize()
        call json%load(filename=documento_grafo)
        call json%info('',n_children=size_tecnico)
        call json%get_core(jsonc)
        call json%get('', lista_puntero_t, tecnico_encontrado)
        do contador_tecnico = 1, size_tecnico
            call jsonc%get_child(lista_puntero_t, contador_tecnico, puntero_t, tecnico_encontrado)
            call jsonc%get_child(puntero_t, 'dpi', atributo_puntero_t, tecnico_encontrado)
            call jsonc%get(atributo_puntero_t, dpi_t)
            call jsonc%get_child(puntero_t, 'nombre', atributo_puntero_t, tecnico_encontrado)
            call jsonc%get(atributo_puntero_t, nombre_t)
            call jsonc%get_child(puntero_t, 'apellido', atributo_puntero_t, tecnico_encontrado)
            call jsonc%get(atributo_puntero_t, apellido_t)
            call jsonc%get_child(puntero_t, 'genero', atributo_puntero_t, tecnico_encontrado)
            call jsonc%get(atributo_puntero_t, genero_t)
            call jsonc%get_child(puntero_t, 'direccion', atributo_puntero_t, tecnico_encontrado)
            call jsonc%get(atributo_puntero_t, direccion_t)
            call jsonc%get_child(puntero_t, 'telefono', atributo_puntero_t, tecnico_encontrado)
            call jsonc%get(atributo_puntero_t, telefono_t)
            print *, "Procesando Tecnico: ", dpi_t
            read(dpi_t, *) dpi_t_int
            read(telefono_t, *) telefono_t_int
            call sucursal_actual%tabla%insertar(dpi_t_int, nombre_t, apellido_t, direccion_t, telefono_t_int, genero_t)
            call sucursal_actual%tabla%imprimir(dpi_t_int)
        end do
        print*,"Tecnicos Cargados Correctamente. Sucursal: ", id_sucursal
        call json%destroy()
    end subroutine carga_masiva_tecnico
!------------------------------------------------------------------------
!CARGA MASIVA RUTAS
!------------------------------------------------------------------------
    subroutine carga_masiva_ruta()
        print *, "---------------------------------------"
        print *, "CARGA MASIVA RUTAS"
        print *, "---------------------------------------"
        print *, "Ingrese el nombre del documento de ruta:"
        print *, "---------------------------------------"
        read(*,*) documento_grafo
        print *, "---------------------------------------"
        call json%initialize()
        call json%load(filename=documento_grafo)
        call json%info('',n_children=size_grafo)
        call json%get_core(jsonc)
        call json%get('', lista_puntero_r, grafo_encontrado)
        do contador_grafo = 1, size_grafo
            call jsonc%get_child(lista_puntero_r, contador_grafo, puntero_r, grafo_encontrado)
            call jsonc%get_child(puntero_r, 'grafo', atributo_puntero_r, grafo_encontrado)
            call jsonc%info(atributo_puntero_r, n_children=size_ruta)
            do contador_ruta = 1, size_ruta
                call jsonc%get_child(atributo_puntero_r, contador_ruta, puntero_aux, grafo_encontrado)
                call jsonc%get_child(puntero_aux, 's1', atributo_puntero_aux, grafo_encontrado)
                call jsonc%get(atributo_puntero_aux, s1)
                call jsonc%get_child(puntero_aux, 's2', atributo_puntero_aux, grafo_encontrado)
                call jsonc%get(atributo_puntero_aux, s2)
                call jsonc%get_child(puntero_aux, 'distancia', atributo_puntero_aux, grafo_encontrado)
                call jsonc%get(atributo_puntero_aux, distancia)
                call jsonc%get_child(puntero_aux, 'imp_mantenimiento', atributo_puntero_aux, grafo_encontrado)
                call jsonc%get(atributo_puntero_aux, imp_mantenimiento)
                print *, "--------------"
                print *, "s1: ", s1
                print *, "s1: ", s2
                print *, "distancia: ", distancia
                print *, "imp_mantenimiento: ", imp_mantenimiento
                 read(s1, *) s1Int
                  read(s2, *) s2Int
                    read(distancia, *) distanciaInt
                    read(imp_mantenimiento, *) imp_mantenimientoInt

                 call grafo%crearConexion(s1Int, s2Int, distanciaInt, imp_mantenimientoInt)  
                 !call grafo%insertar_nodo(s2Int, s1Int, distanciaInt, imp_mantenimientoInt)


            end do
        end do
        call json%destroy()
    end subroutine carga_masiva_ruta

end program main