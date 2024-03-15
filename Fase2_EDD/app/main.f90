program main
    use json_module
    use iso_fortran_env, only:
    use matrix_m
    
    implicit none
    !Para la lectura de los Json
    type(json_file) :: json
    type(json_core) :: jsonc
    type(json_value), pointer :: listPointer, animalPointer, attributePointer
    logical :: found
    integer :: size, iC
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

     character(len=7):: colorcito 
    !Importacionnes
    !Matriz Dispersa
    type(matrix) :: matriz

    !Fila---Columna
    colorcito="2coma2"
    call matriz%insert(2,2,.true.,colorcito)
    colorcito="1coma1"
    call matriz%insert(1,1,.true.,colorcito)
    colorcito="3coma3"
    call matriz%insert(3,3,.true.,colorcito)

    call matriz%print()

    call matriz%graph()




    !print*, "==================================="
    !print*, "Carga masiva de clientes"
    !direccion="clientes,json"
    !call cargaMasivaCliente(direccion)
    !print*, "==================================="
    !print*, "Carga masiva de capas"
    !direccion="capas.json"
    !call cargaCapas(direccion)
    !print *, "==================================="
    !print *, "Carga masiva de albumes"
    !direccion="albumes.json"
    !call cargaAlbumes(direccion)

contains 
subroutine cargaMasivaCliente(direccion)
        character(len=1000), intent(in) :: direccion
        
        call json%initialize()
       
        call json%load(filename=direccion)
        call json%info('',n_children=size)
        call json%get_core(jsonc)
        call json%get('', listPointer, found)

        do iC = 1, size
            call jsonc%get_child(listPointer, iC, animalPointer, found)

            call jsonc%get_child(animalPointer, 'dpi', attributePointer, found)
            call jsonc%get(attributePointer, dpi)

            call jsonc%get_child(animalPointer, 'nombre_cliente', attributePointer, found)
            call jsonc%get(attributePointer, nombreCliente)

            call jsonc%get_child(animalPointer, 'password', attributePointer, found) 
            call jsonc%get(attributePointer, password)
           
             print *, "----"
             print *, 'DPI: ', dpi
             print *, 'Nombre: ', nombreCliente
             print *, 'password: ', password

        end do
        call json%destroy()
end subroutine cargaMasivaCliente

subroutine cargaCapas(direccion)
        character(len=1000), intent(in) :: direccion
        
        call json%initialize()
       
        call json%load(filename=direccion)

        call json%info('',n_children=size)
        call json%get_core(jsonc)
        call json%get('', listPointer, found) !obtengo cada trozo dentro de llaves

        do iC = 1, size
            call jsonc%get_child(listPointer, iC, animalPointer, found)

            call jsonc%get_child(animalPointer, 'id_capa', attributePointer, found)
            call jsonc%get(attributePointer, nombreAlbum)

            call jsonc%get_child(animalPointer, 'pixeles', attributePointer, found)
   
            call jsonc%info(attributePointer,n_children=sizeAlbumes)

            do iCAlbumes = 1, sizeAlbumes
                call jsonc%get_child(attributePointer, iCAlbumes, todoAlbumes, found)

                call jsonc%get_child(todoAlbumes, 'fila', attributePointerCapas, found)
                call jsonc%get(attributePointerCapas, fila)

                call jsonc%get_child(todoAlbumes, 'columna', attributePointerCapas, found)
                call jsonc%get(attributePointerCapas, columna)

                call jsonc%get_child(todoAlbumes, 'color', attributePointerCapas, found)
                call jsonc%get(attributePointerCapas, color)

            
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

        call json%info('',n_children=size)
        call json%get_core(jsonc)
        call json%get('', listPointer, found) !obtengo cada trozo dentro de llaves

        do iC = 1, size
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

end program main
