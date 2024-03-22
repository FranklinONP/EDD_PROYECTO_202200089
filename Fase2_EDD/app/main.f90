module clienteTemporal
    use abb_m
    use avl_m
    implicit none
    type :: cliente
        character(len=100) :: nombre
        character(len=100) :: dpi
        character(len=100) :: password
        type(abb) :: tree
        type(avl) :: avl
    end type cliente
end module clienteTemporal

program main
    use matrix_m
    use json_module
    use iso_fortran_env, only:
    use clienteTemporal
    use avl_m
    
    implicit none
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

    direccion="mario.json"
    call cargaCapas(direccion)
    print *, "= Cargo capas ="
    direccion="unionMario.json"
    call cargaImagenes(direccion)
    call clienteObject%avl%graficar()
    call clienteObject%avl%abbImagen(1)
    call clienteObject%avl%crearImagen(1)
    read *, desicion
    call clienteObject%avl%abbImagen(1)
    call clienteObject%avl%crearImagen(1)




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
           
             print *, "----"
             print *, 'DPI: ', dpi
             print *, 'Nombre: ', nombreCliente
             print *, 'password: ', password

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
            call clienteObject%tree%insert(idCapaE)

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
                call clienteObject%tree%search(idCapaE,columnaE,filaE,color)
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
            call clienteObject%avl%insert(idImagenE)
 

            do iCAlbumes = 1, sizeAlbumes
                call jsonc%get_child(attributePointer, iCAlbumes, todoAlbumes, found)
                call jsonc%get(todoAlbumes, capasImagenes)

            
                print *, "----"
                print *, 'Id Imagen: ',  idImagen
                print *, 'Capas: ', capasimagenes 
                !Busco y extraigo la matriz del id actual
                read(capasimagenes, *) capaImagenE;
                call clienteObject%tree%extraerMatriz(capaImagenE,mtxTemporal)
                !mtxTemporal contiene la matriz de la capa con id actual
                !Inserto la matriz al abb del avl
                call clienteObject%avl%search(idImagenE,capaImagenE,mtxTemporal)
                print *, "================ IMPRESION DE MATRIZ TEMPORAL ================"
                call mtxTemporal%print()

            end do

        end do
        call json%destroy()
end subroutine cargaImagenes

end program main
