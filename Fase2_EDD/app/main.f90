program main
    use json_module
    use iso_fortran_env, only:
    
    implicit none
    type(json_file) :: json
    type(json_core) :: jsonc
    type(json_value), pointer :: listPointer, animalPointer, attributePointer
    logical :: found 
    integer :: size, iC,m,num_pasos = 1
    character(:), allocatable :: password,nombreCliente,dpi
    character(len=1000) :: direccion
    direccion="clientes,json"
    call cargaMasivaCliente(direccion)


     

contains 
subroutine cargaMasivaCliente(direccion)
        character(len=1000), intent(in) :: direccion
        print*, direccion
        
        call json%initialize()
       
        call json%load(filename=direccion)
        call json%info('',n_children=size)
        call json%get_core(jsonc)
        call json%get('', listPointer, found)

        do iC = 1, size
            call jsonc%get_child(listPointer, iC, animalPointer, found)

            call jsonc%get_child(animalPointer, 'dpi', attributePointer, found)
            call jsonc%get(attributePointer, nombreCliente)
   

            call jsonc%get_child(animalPointer, 'nombre_cliente', attributePointer, found)
            call jsonc%get(attributePointer, dpi)


            call jsonc%get_child(animalPointer, 'password', attributePointer, found) 
            call jsonc%get(attributePointer, password)
           
            
             print *, "----"
             print *, 'DPI: ', dpi
             print *, 'Nombre: ', nombreCliente
             print *, 'password: ', password


        end do
        call json%destroy()
end subroutine cargaMasivaCliente

end program main
