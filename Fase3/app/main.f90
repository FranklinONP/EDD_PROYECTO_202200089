
program Proyecto_202200089

    use json_module
    use iso_fortran_env, only:
    use P1
    implicit none
    type(json_file) :: json
    type(json_core) :: jsonc
    type(json_value), pointer :: listPointer, animalPointer, attributePointer
    logical :: found 
    integer :: size, iC,m,num_pasos = 1
    character(:), allocatable :: idCC, nombreCarga, img_p, img_g
    print*, "Inicio del programa"
   

contains 
subroutine cargaSucursales(direccion)
        integer :: idInt,img_gInt,img_pInt
        character(len=1000), intent(in) :: direccion
        print*, direccion
        
        call json%initialize()
       
        call json%load(filename=direccion)
        call json%info('',n_children=size)
        call json%get_core(jsonc)
        call json%get('', listPointer, found)

        do iC = 1, size
            call jsonc%get_child(listPointer, iC, animalPointer, found)

            call jsonc%get_child(animalPointer, 'nombre', attributePointer, found)
            call jsonc%get(attributePointer, nombreCarga)   

            call jsonc%get_child(animalPointer, 'id', attributePointer, found)
            call jsonc%get(attributePointer, idCC)

            call jsonc%get_child(animalPointer, 'img_g', attributePointer, found) 
            call jsonc%get(attributePointer, img_g)

            call jsonc%get_child(animalPointer, 'img_p', attributePointer, found) 
            call jsonc%get(attributePointer, img_p)

            read(idCC, *) idInt;
            read(img_g, *) img_gInt;
            read(img_p, *) img_pInt;
            
            
             print *, "----"
             print *, 'ID: ', idCC
             print *, 'Nombre: ', nombreCarga
             print *, 'img_p: ', img_p
             print *, 'img_g: ', img_g


        end do
        call json%destroy()
end subroutine cargaSucursales

end program Proyecto_202200089