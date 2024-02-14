program json_example
    use json_module, rk => json_rk
    implicit none

    character(len=1000) :: json_path
    type(json_file) :: json
    type(json_value), pointer :: root, value
    integer :: i, n

    ! Asignar la ruta del archivo JSON
    json_path = 'C:/Users/50232/Documents/Biblioteca Frank/5to Semestre/Estructura de Datos/Laboratorio/Proyecto1/Proyecto1_202200089/ejemplo.json'

    ! Leer el JSON desde el archivo
    call json%load_from_file(json_path)

    ! Obtener el número de elementos en el array
    call json%array_size(n)

    ! Recorrer los elementos del arrays
    do i = 1, n
        ! Obtener el objeto actual
        call json%get(i, value)

        ! Acceder a los valores del objeto
        character(len=:), allocatable :: id, nombre, img_p, img_g
        call value%get('id', id)
        call value%get('nombre', nombre)
        call value%get('img_p', img_p)
        call value%get('img_g', img_g)

        ! Hacer algo con los valores obtenidos
        print *, 'ID:', id
        print *, 'Nombre:', nombre
        print *, 'Imagen principal:', img_p
        print *, 'Imagen secundaria:', img_g
        print *
    end do

    ! Liberar la memoria asignada
    call json%destroy()

end program json_example
