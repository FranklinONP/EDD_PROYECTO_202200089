module clienteTemporal
    use abb_m
    use AVL_Tree_M
    implicit none
    type :: cliente
        character(len=100) :: nombre
        character(len=100) :: dpi
        character(len=100) :: password
        type(abb) :: tree
        type(Tree_t),pointer:: avl
    end type cliente
end module clienteTemporal

program main
    use json_module
    use iso_fortran_env, only:
    use clienteTemporal
    
    implicit none
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
    
    integer :: keys(18) = [10, 5, 15, 3, 1, 12, 17, 7, 4, 6, 9, 11, 2, 20, 24, 21, 23, 28]
    integer :: i

    !call clienteObject%avl%newTree()
    !do i = 1, size(keys)
    !    call clienteObject%avl%insert(keys(i))
    !end do
    !call clienteObject%avl%generateGraph()

    call clienteObject%tree%insert(5);
    call clienteObject%tree%insert(6)
    call clienteObject%tree%insert(3)

    call clienteObject%tree%graph("inserted")
    call clienteObject%tree%search(5,2,2,"#B22222")
    call clienteObject%tree%grapEspecifico(5)
    read *, desicion
    call clienteObject%tree%search(5,2,2,"#B55555")
    call clienteObject%tree%grapEspecifico(5)

    !direccion="mario.json"
    !call cargaCapasID(direccion)                           FUNCIONA CORRECTAMENTE EL ARBOL BB
    !call cargaCapas(direccion)
    !call clienteObject%tree%graph("inserted")
    !call clienteObject%tree%grapEspecifico(0)




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
           
             print *, "----"
             print *, 'DPI: ', dpi
             print *, 'Nombre: ', nombreCliente
             print *, 'password: ', password

        end do
        call json%destroy()
end subroutine cargaMasivaCliente
subroutine cargaCapasID(direccion)
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
            call clienteObject%tree%insert(idCapaE)
        end do
        call json%destroy()
end subroutine cargaCapasID

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
                call clienteObject%tree%search(idCapaE,filaE,columnaE,color)
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
            
            do iCAlbumes = 1, sizeAlbumes
                call jsonc%get_child(attributePointer, iCAlbumes, todoAlbumes, found)

                call jsonc%get(todoAlbumes, capasImagenes)

            
                print *, "----"
                print *, 'Id Imagen: ',  idImagen
                print *, 'Capas: ', capasimagenes 

            end do

        end do
        call json%destroy()
end subroutine cargaImagenes

end program main
