
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
                    if (dpiUser .eq. 'frank' .and. passwordUser .eq. '1') then
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
                                    case (2)
                                        print *, 'Crear Imagenes - Inorden'
                                    case (3)
                                        print *, 'Crear Imagenes - Postorden'
                                    case (4)
                                        print *, 'Crear Imagenes - Ingresando capas'
                                    case (5)
                                        print *, 'Crear Imagenes - A partir de una Imagen'
                                    case (6)
                                        exit
                                    case default
                                        print *, 'Opcion no válida'
                                end select
                            end do
                        case (3)
                            print *, 'Ver Estructuras'
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