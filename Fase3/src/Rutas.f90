module ListaDeAdyacencia

    implicit none
    
    type, private :: a_nodo
        integer :: destino
        integer :: peso
        integer :: imp
        type(a_nodo), pointer :: sig => null()
    end type a_nodo

    type, private :: v_nodo
        integer :: val
        type(v_nodo), pointer :: sig => null()
        type(a_nodo), pointer :: raiz => null()

    contains
        procedure :: insertarArista
        procedure :: graficarArista
    end type v_nodo

    type :: ListaAdyacencia
        private
        type(v_nodo), pointer :: raiz => null()

    contains
        procedure :: insert
        procedure :: crearConexion
        procedure :: crearGrafo
        procedure :: dikstraDistancia
        procedure :: dijkstra_max
    end type ListaAdyacencia

contains



subroutine dijkstra_max(self, origen, destino, conclusion_distancia, conclusion_impresoras, conclusiones_rutas)
    class(ListaAdyacencia), intent(inout) :: self
    integer, intent(in) :: origen
    integer, intent(in) :: destino
    integer :: valor_real
    integer, allocatable :: dist(:), index_map(:), peso_sum(:), index_map_inv(:), predecesor(:)
    logical, allocatable :: visitado(:)
    integer :: n, max_index, max_dist, i, j, contador_iteaciones2, contador_iteaciones
    type(v_nodo), pointer :: aux
    type(a_nodo), pointer :: edge
    
    integer, dimension(:), pointer, intent(out) :: conclusiones_rutas
    integer, intent(out) :: conclusion_distancia, conclusion_impresoras

    contador_iteaciones = 0
    contador_iteaciones2 = 0

    ! Contar el número de nodos y crear el mapeo de índices
    n = 0
    aux => self%raiz
    allocate(index_map(10000))
    allocate(index_map_inv(10000))
    do while(associated(aux))
        n = n + 1
        index_map(aux%val) = n
        index_map_inv(n) = aux%val
        aux => aux%sig
    end do


    ! Inicializar distancias y visitados
    allocate(dist(n), source=-huge(0))
    allocate(visitado(n), source=.false.)
    allocate(peso_sum(n), source=0)
    allocate(predecesor(n), source=0)
    dist(index_map(origen)) = 0

    do while (count(.not. visitado) > 0)  ! Continuar hasta que todos los nodos sean visitados
        ! Encontrar el nodo con la distancia máxima, del conjunto de nodos no visitados
        max_dist = -huge(max_dist)
        max_index = -1
        do j = 1, n
            if (.not. visitado(j) .and. dist(j) > max_dist) then
                max_dist = dist(j)
                max_index = j
            end if
        end do


        ! Si no hay camino, terminar
        if (max_index == -1) exit

        ! Marcar el nodo seleccionado como visitado
        visitado(max_index) = .true.

        ! Actualizar los valores de dist de los nodos adyacentes al nodo seleccionado
        aux => self%raiz
        do while(associated(aux))
            if (index_map(aux%val) == max_index) then
                edge => aux%raiz
                do while(associated(edge))
                    i = index_map(edge%destino)
                    if (.not. visitado(i)) then
                        if (dist(max_index) + edge%imp > dist(i)) then
                            dist(i) = dist(max_index) + edge%imp
                            peso_sum(i) = peso_sum(max_index) + edge%peso
                            predecesor(i) = max_index
                        elseif (dist(i) == -huge(0)) then
                            dist(i) = dist(max_index) + edge%imp
                            peso_sum(i) = peso_sum(max_index) + edge%peso
                            predecesor(i) = max_index
                        end if
                    end if
                    edge => edge%sig
                end do
            end if
            aux => aux%sig
        end do

    end do

    ! Imprimir la distancia más larga al destino
    if (dist(index_map(destino)) /= -huge(0)) then
       ! print *, "LA CANTIDAD DE MANTENIMIENTOS A IMPRESORAS DIGIENDONOS AL DESTINO ES:", dist(index_map(destino))
       ! print*, "EL PESO TOTAL ES: ", peso_sum(index_map(destino))
       ! print *, "Ruta: "
        i = index_map(destino)
        do while (i /= 0)
            valor_real = index_map_inv(i)
           ! print *, valor_real
            i = predecesor(i)
            contador_iteaciones = contador_iteaciones+1
        end do
    else
        print *, "No hay camino al destino"
    end if

    conclusion_distancia = peso_sum(index_map(destino))
    conclusion_impresoras = dist(index_map(destino))

    allocate(conclusiones_rutas(contador_iteaciones))

    if (dist(index_map(destino)) /= -huge(0)) then
        i = index_map(destino)
        do while (i /= 0)
            contador_iteaciones2 = contador_iteaciones2+1
            valor_real = index_map_inv(i)
            conclusiones_rutas(contador_iteaciones2) = index_map_inv(i)
            i = predecesor(i)
        end do
    else
        print *, "No hay camino al destino"
    end if

    deallocate(dist, visitado, index_map)

end subroutine dijkstra_max




subroutine dikstraDistancia(self, origen, destino, conclusion_distancia, conclusion_impresoras, conclusiones_rutas)
    class(ListaAdyacencia), intent(inout) :: self
    integer, intent(in) :: origen
    integer, intent(in) :: destino
    integer :: valor_real

    !VALORES QUE VAMOS A DEVOLVER
    integer, dimension(:), pointer, intent(out) :: conclusiones_rutas
    integer, intent(out) :: conclusion_distancia, conclusion_impresoras

    integer, allocatable :: dist(:), index_map(:), imp_sum(:), predecesor(:), index_map_inv(:)
    logical, allocatable :: visitado(:)
    integer :: n, min_index, min_dist, i, j, contador_iteaciones,contador_iteaciones2
    type(v_nodo), pointer :: aux
    type(a_nodo), pointer :: edge
    contador_iteaciones = 0
    contador_iteaciones2 = 0
    ! Contar el número de nodos y crear el mapeo de índices
    n = 0
    aux => self%raiz
    allocate(index_map(10000))
    allocate(index_map_inv(10000))
    do while(associated(aux))
        n = n + 1
        index_map(aux%val) = n
        index_map_inv(n) = aux%val
        aux => aux%sig
    end do

    

    ! Inicializar distancias, visitados e imp_sum
    allocate(dist(n), source=huge(0))
    allocate(visitado(n), source=.false.)
    allocate(imp_sum(n), source=0)
    allocate(predecesor(n), source=0)
    dist(index_map(origen)) = 0


    do
        min_dist = huge(min_dist)
        min_index = -1
        do j = 1, n
            if (.not. visitado(j) .and. dist(j) < min_dist) then
                min_dist = dist(j)
                min_index = j
            end if
        end do

        if (min_index == -1) exit

        visitado(min_index) = .true.

        aux => self%raiz
        do while(associated(aux))
            if (index_map(aux%val) == min_index) then
                edge => aux%raiz
                do while(associated(edge))
                    i = index_map(edge%destino)
                    if (.not. visitado(i) .and. dist(min_index) + edge%peso < dist(i)) then
                        dist(i) = dist(min_index) + edge%peso
                        imp_sum(i) = imp_sum(min_index) + edge%imp
                        predecesor(i) = min_index
                    end if
                    edge => edge%sig
                end do
            end if
            aux => aux%sig
        end do
    end do

    if (dist(index_map(destino)) /= huge(0)) then

        i = index_map(destino)
        do while (i /= 0)
            valor_real = index_map_inv(i)
            i = predecesor(i)
            contador_iteaciones = contador_iteaciones+1
        end do
    else
        print *, "No hay camino al destino"
    end if
    print *, ""
    
    conclusion_distancia = dist(index_map(destino))
    conclusion_impresoras = imp_sum(index_map(destino))
    !print *, "SOY CONTADOR ITERACIONES", contador_iteaciones
    allocate(conclusiones_rutas(contador_iteaciones))

    if (dist(index_map(destino)) /= huge(0)) then
        i = index_map(destino)
        do while (i /= 0)
            contador_iteaciones2 = contador_iteaciones2+1
            valor_real = index_map_inv(i)
            conclusiones_rutas(contador_iteaciones2) = index_map_inv(i)
            i = predecesor(i)
        end do
    else
        print *, "No hay camino al destino"
    end if

    deallocate(dist, visitado, index_map, imp_sum, predecesor)

end subroutine dikstraDistancia





    !Funcionalidades de la lista de adyacencia
subroutine insert(self, valor)
        class(ListaAdyacencia), intent(inout) :: self
        integer, intent(in) :: valor

        type(v_nodo), pointer :: aux
        type(v_nodo), pointer :: nuevo
        allocate(nuevo)
        nuevo%val = valor

        if(.not. associated(self%raiz)) then
            self%raiz => nuevo
        else
            aux => self%raiz
            if(valor < self%raiz%val) then
                nuevo%sig => self%raiz
                self%raiz => nuevo

            else
                do while(associated(aux%sig)) 
                    if(valor < aux%sig%val) then
                        nuevo%sig => aux%sig
                        aux%sig => nuevo
                        exit
                    end if
                    aux => aux%sig
                end do

                if(.not. associated(aux%sig)) then
                    aux%sig => nuevo
                end if
            end if
        end if
    end subroutine insert

    subroutine crearConexion(self, origen, destino, peso, imp)
        class(ListaAdyacencia), intent(inout) :: self
        integer, intent(in) :: origen
        integer, intent(in) :: destino
        integer, intent(in) :: peso
        integer, intent(in) :: imp
        type(v_nodo), pointer :: aux
        aux => self%raiz

        do while(associated(aux))
            if(aux%val == origen) then
                call aux%insertarArista(destino, peso, imp)

                exit
            end if
            aux => aux%sig
        end do
    end subroutine crearConexion

     subroutine crearGrafo(self)
        class(ListaAdyacencia), intent(inout) :: self

        type(v_nodo), pointer :: aux
        character(len=150) :: nodo_dec
        character(len=100) :: comando
        character(len=20) :: nombre
        character(len=10) :: str_aux
        integer :: io
        integer :: i

        aux => self%raiz
        io = 1
        comando = "dot -Tpng ./grafo.dot -o ./grafo.png"
        open(newunit=io, file='./grafo.dot')
        write(io, *) 'digraph g {'
        do while(associated(aux))
            write(str_aux, '(I10)') aux%val
            nombre = '"Nodo'//trim(adjustl(str_aux))//'"'
            nodo_dec = trim(adjustl(nombre))//'[label="'//trim(adjustl(str_aux))//'"]'
            write(io, *) nodo_dec
            call aux%graficarArista(io)
            aux => aux%sig
        end do
        write(io, *) '}'
        close(io)

        call execute_command_line(comando, exitstat=i)

        if (i == 1) then
            print *, "OcurriÃ³ un error al generar la imagen"

        else
            print *, "La imagen fue generada exitosamente"
            call system("start ./grafo.png")
        end if
    end subroutine crearGrafo



    !Funcionalidades del nodo que contiene los vértices (v_node)
    subroutine insertarArista(self, destino, peso, imp)
        class(v_nodo), intent(inout) :: self
        integer, intent(in) :: destino
        integer, intent(in) :: peso
        integer, intent(in) :: imp
        type(a_nodo), pointer :: nuevo
        type(a_nodo), pointer :: aux

        allocate(nuevo)
        nuevo%destino = destino
        nuevo%peso = peso
        nuevo%imp = imp
        if(.not. associated(self%raiz)) then
            self%raiz => nuevo

        else
            aux => self%raiz
            do while(associated(aux%sig))
                if(aux%destino == destino) then
                    return
                end if
                aux => aux%sig
            end do
            aux%sig => nuevo
        end if
    end subroutine insertarArista


subroutine graficarArista(self, io)
    class(v_nodo), intent(inout) :: self
    integer, intent(in) :: io

    type(a_nodo), pointer :: aux
    character(len=20) :: nombre_origen
    character(len=20) :: nombre_destino
    character(len=20) :: peso
    character(len=20) :: imp
    character(len=10) :: str_aux
    aux => self%raiz

    write(str_aux, '(I10)') self%val
    nombre_origen = '"Nodo'//trim(adjustl(str_aux))//'"'

    do while(associated(aux))
        !if(aux%destino < self%val) then
            write(str_aux, '(I10)') aux%destino
            nombre_destino = '"Nodo'//trim(adjustl(str_aux))//'"'
            write(str_aux, '(I10)') aux%peso  ! Use the actual weight of the edge
            peso = 'Distancia: '//trim(adjustl(str_aux))
            write(str_aux, '(I10)') aux%imp  ! Use the actual imp of the edge
            imp = '#Impresoras: '//trim(adjustl(str_aux))
            write(io, *) nombre_origen//'->'//nombre_destino//' [label="'//trim(peso)//', ' // trim(imp)//'"]'
        !end if
        aux => aux%sig
    end do
end subroutine graficarArista

end module ListaDeAdyacencia