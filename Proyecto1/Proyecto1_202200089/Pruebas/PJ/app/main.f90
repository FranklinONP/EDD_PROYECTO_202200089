program main
use::json_module
implicit none

type(json_file) :: json
    type(json_core) :: jsonc
    type(json_value), pointer :: listPointer, animalPointer, attributePointer
    logical :: found 
    integer :: size, i,m,num_pasos = 1
    character(:), allocatable :: id, nombre, img_p, img_g
    integer :: opcion


        integer :: idInt,img_gInt,img_pInt
        print *, "--------------Carga Masiva Cliente-------------------------"
        
        call json%initialize()
        call json%load(filename='pruebaJson.json')
        call json%info('',n_children=size)
        call json%get_core(jsonc)
        call json%get('', listPointer, found)

        do i = 1, size
            call jsonc%get_child(listPointer, i, animalPointer, found)

            call jsonc%get_child(animalPointer, 'id', attributePointer, found)
            call jsonc%get(attributePointer, id)

            call jsonc%get_child(animalPointer, 'nombre', attributePointer, found)
            call jsonc%get(attributePointer, nombre)

            call jsonc%get_child(animalPointer, 'img_p', attributePointer, found) 
            call jsonc%get(attributePointer, img_p)

            call jsonc%get_child(animalPointer, 'img_g', attributePointer, found) 
            call jsonc%get(attributePointer, img_g)

            read(id, *) idInt;
            read(img_g, *) img_gInt;
            read(img_p, *) img_pInt;
            !call myCola%copia_push(idInt,trim(nombre),img_gInt,img_pInt)
            
             print *, "----"
             print *, 'ID: ', id
             print *, 'Nombre: ', nombre
             print *, 'img_p: ', img_p
             print *, 'img_g: ', img_g


        end do
        call json%destroy()
    




    do
        print*, 'Selecciona una opción:'
        print*, '1. Opción 1'
        print*, '2. Opción 2'
        print*, '3. Salir'
        read*, opcion

        select case (opcion)
            case (1)
                print*, 'Has seleccionado la opción 1.'
            case (2)
                print*, 'Has seleccionado la opción 2.'
            case (3)
                print*, 'Has seleccionado salir. Adiós!'
                exit
            case default
                print*, 'Opción no válida. Por favor, intenta de nuevo.'
        end select
    end do
   
end program main
