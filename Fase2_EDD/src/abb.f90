module abb_m
use lista_module
use matrix_m
    implicit none
    private
    type :: Node_t
        integer :: value
        type(matrix) :: mtx
        type(Node_t), pointer :: right => null()
        type(Node_t), pointer :: left => null()
    end type Node_t
    type :: node
        private
        integer :: fila,columna,nPixel
        character(len=7) :: color
        type(node), pointer :: next => null()
    end type node
    type, public :: abb
        type(Node_t), pointer :: root => null()

    contains
        procedure :: insert
        procedure :: delete
        procedure :: graph
        procedure :: search
        procedure :: grapEspecifico
        procedure :: extraerMatriz
        procedure :: insertMatriz
        procedure :: unirMatrices
        procedure :: GrapPreorder
        procedure :: GrapInorden
        procedure :: GrapPostOrden
    end type abb

contains  
    subroutine unirMatrices(self,mR)
        class(abb), intent(in) :: self
        type(matrix), intent(inout) :: mR
        type(matrix) :: mtx

        print *, "Uniendo matrices"
        call unirMatricesRec(self%root,mtx)
        !call mtx%print()
        print *, "Matrices unidas"
        print *, "Matriz resultante"
        mR=mtx
        print *, "Matriz resultante"
        call mR%print()

    end subroutine unirMatrices
recursive subroutine unirMatricesRec(root,mtx)
    type(Node_t), pointer, intent(in) :: root
    type(matrix), intent(inout) :: mtx

        if (.not. associated(root)) then
            return
        else 
            call root%mtx%getPixels(mtx)
            call unirMatricesRec(root%left,mtx)
            call unirMatricesRec(root%right,mtx)
        end if

end subroutine unirMatricesRec

!==================================================================================================  
subroutine extraerMatriz(self, original_val, mtx)
    class(abb), intent(inout) :: self
    integer, intent(in) :: original_val
    type(matrix), intent(out) :: mtx

    mtx = search_and_modifyRec2(self%root, original_val)

end subroutine extraerMatriz

recursive function search_and_modifyRec2(root, original_value) result(mtx)
    type(Node_t), pointer, intent(in) :: root
    integer, intent(in) :: original_value
    type(matrix) :: mtx

    if (.not. associated(root)) then
        print *, "El valor", original_value, "no se encuentra en el árbol."
        return
    end if

    if (original_value < root%value) then
        mtx = search_and_modifyRec2(root%left, original_value)
    else if (original_value > root%value) then
        mtx = search_and_modifyRec2(root%right, original_value)
    else
        print *, "El valor",root%value, " se ha encontrado "
        mtx = root%mtx
    end if
end function search_and_modifyRec2
!==================================================================================================
subroutine insertMatriz(self, original_val,mtx)
    class(abb), intent(inout) :: self
    integer, intent(in) :: original_val
    type(matrix), intent(in) :: mtx

    call search_and_modifyRec3(self%root, original_val,mtx)
end subroutine insertMatriz

recursive subroutine search_and_modifyRec3(root, original_value,mtx)
    type(Node_t), pointer :: root
    integer, intent(in) :: original_value
    type(matrix), intent(in) :: mtx

    if (.not. associated(root)) then
        print *, "El valor", original_value, "no se encuentra en el árbol."
        return
    end if

    if (original_value < root%value) then
        call search_and_modifyRec3(root%left, original_value,mtx)
    else if (original_value > root%value) then
        call search_and_modifyRec3(root%right, original_value,mtx)
    else
        print *, "El valor",root%value, " se ha encontrado "

            root%mtx=mtx

    end if
end subroutine search_and_modifyRec3
!==================================================================================================
subroutine search(self, original_val,fila,columna,color)
    class(abb), intent(inout) :: self
    integer, intent(in) :: original_val
    integer, intent(in) :: fila,columna
    !character(len=7), intent(in) :: color
    character(len=7), intent(in) :: color

    call search_and_modifyRec(self%root, original_val,fila,columna,color)
end subroutine search
recursive subroutine search_and_modifyRec(root, original_value,fila,columna,color)
    type(Node_t), pointer :: root
    integer, intent(in) :: original_value
    integer, intent(in) :: fila,columna
    !character(len=7), intent(in) :: color
    character(len=7), intent(in) ::  color

    if (.not. associated(root)) then
        print *, "El valor", original_value, "no se encuentra en el árbol."
        return
    end if

    if (original_value < root%value) then
        call search_and_modifyRec(root%left, original_value,fila,columna,color)
    else if (original_value > root%value) then
        call search_and_modifyRec(root%right, original_value,fila,columna,color)
    else
        print *, "El valor",root%value, " se ha encontrado "

            call root%mtx%insert(fila,columna,.true.,color)  

    end if
end subroutine search_and_modifyRec


subroutine grapEspecifico(self, original_val)
    class(abb), intent(inout) :: self
    integer, intent(in) :: original_val
    call grapRec(self%root, original_val)
end subroutine  grapEspecifico

recursive subroutine grapRec(root, original_value)
    type(Node_t), pointer :: root
    integer, intent(in) :: original_value

    if (.not. associated(root)) then
        print *, "El valor", original_value, "no se encuentra en el árbol."
        return
    end if

    if (original_value < root%value) then
        call grapRec(root%left, original_value)
    else if (original_value > root%value) then
        call grapRec(root%right, original_value)
    else
        print *, "El valor", original_value, " se ha encontrado"
        call root%mtx%graficar()
        call root%mtx%tabla("tablaDOT")
        call root%mtx%print()
    end if
end subroutine grapRec


    subroutine insert(self, val)
        class(abb), intent(inout) :: self
        integer, intent(in) :: val

        if (.not. associated(self%root)) then
            allocate(self%root)
            self%root%value = val
        else
            call insertRec(self%root, val)
        end if
    end subroutine insert

    recursive subroutine insertRec(root, val)
        type(Node_t), pointer, intent(inout) :: root
        integer, intent(in) :: val
        
        if (val < root%value) then
            if (.not. associated(root%left)) then
                allocate(root%left)
                root%left%value = val
            else
                call insertRec(root%left, val)
            end if
        else if (val > root%value) then
            if (.not. associated(root%right)) then
                allocate(root%right)
                root%right%value = val
            else
                call insertRec(root%right, val)
            end if
        end if
    end subroutine insertRec

    subroutine delete(self, val)
        class(abb), intent(inout) :: self
        integer, intent(inout) :: val
    
        self%root => deleteRec(self%root, val)
    end subroutine delete

    recursive function deleteRec(root, value) result(res)
        type(Node_t), pointer :: root
        integer, intent(in) :: value
        type(Node_t), pointer :: res
        type(Node_t), pointer :: temp

        if (.not. associated(root)) then
            res => root
            return
        end if

        if (value < root%value) then
            root%left => deleteRec(root%left, value)
        else if (value > root%value) then
            root%right => deleteRec(root%right, value)
        else
            if (.not. associated(root%left)) then
                temp => root%right
                deallocate(root)
                res => temp
                return
            else if (.not. associated(root%right)) then
                temp => root%left
                deallocate(root)
                res => temp
                return
            else
                call getMajorOfMinors(root%left, temp)
                root%value = temp%value
                root%left => deleteRec(root%left, temp%value)
            end if
        end if

        res => root
    end function deleteRec

    recursive subroutine getMajorOfMinors(root, major)
        type(Node_t), pointer :: root, major
        if (associated(root%right)) then
            call getMajorOfMinors(root%right, major)
        else
            major => root
        end if
    end subroutine getMajorOfMinors

subroutine GrapPreorder(self,lR,n)
    class(abb), intent(in) :: self
    type(lista_m), intent(inout) :: lR
    type(lista_m) :: list    
    integer, intent(in) :: n
    integer :: contador = 0
    call preorderRec(self%root,list, n, contador)
    lR=list
    print *, "Matriz resultante Preorden"
    contador = 0
end subroutine GrapPreorder

recursive subroutine preorderRec(root,list, n, contador)
    type(Node_t), pointer, intent(in) :: root
    type(lista_m), intent(inout) :: list
    integer, intent(inout) :: contador
    integer, intent(in) :: n
    
    if (.not. associated(root)) then
        return
    else 
        if (contador >= n) return
        write(*, '(I0, A)', advance='no') root%value, " - "
        contador = contador + 1
        call list%push(contador,root%value,root%mtx)
        print *, "Contador: ", contador
        call preorderRec(root%left,list, n, contador)
        call preorderRec(root%right,list, n, contador)
    end if
end subroutine preorderRec

   subroutine GrapInorden(self,lR,n)
        class(abb), intent(in) :: self
        type(lista_m), intent(inout) :: lR
        type(lista_m) :: list   
        integer, intent(in) :: n
        integer :: contador = 0
        call GrapInordenRec(self%root,list,n,contador)
        lR=list
        print *, "Matriz resultante Inorden"
        contador = 0
    end subroutine GrapInorden

    recursive subroutine GrapInordenRec(root,list,n,contador)
        type(Node_t), pointer, intent(in) :: root
        type(lista_m), intent(inout) :: list
        integer, intent(inout) :: contador
        integer, intent(in) :: n

        if(associated(root)) then
            ! IZQ - RAIZ - DER
            call GrapInordenRec(root%left,list,n,contador)
            if (contador >= n) return
            contador = contador + 1
            call list%push(contador,root%value,root%mtx)
            write(*, '(I0 A)', advance='no') root%value, " - "
            call GrapInordenRec(root%right,list,n,contador)
        end if
    end subroutine GrapInordenRec

    subroutine GrapPostOrden(self,lR,n)
        class(abb), intent(in) :: self
        type(lista_m), intent(inout) :: lR
        type(lista_m) :: list   
        integer, intent(in) :: n
        integer :: contador = 0
        call GrapPostOrdenRec(self%root,list,n,contador)
        lR=list
        print *, "Matriz resultante PostOrden"
        contador = 0

    end subroutine GrapPostOrden
    recursive subroutine GrapPostOrdenRec(root,list,n,contador)
        type(Node_t), pointer, intent(in) :: root
        type(lista_m), intent(inout) :: list
        integer, intent(inout) :: contador
        integer, intent(in) :: n

        if(associated(root)) then
            ! IZQ - DER - RAIZ
            call GrapPostOrdenRec(root%left,list,n,contador)
            call GrapPostOrdenRec(root%right,list,n,contador)
             if (contador >= n) return
            contador = contador + 1
            call list%push(contador,root%value,root%mtx)
            write(*, '(I0 A)', advance='no') root%value, " - "
        end if
    end subroutine GrapPostOrdenRec

    subroutine graph(self, filename)
        class(abb), intent(in) :: self
        character(len=*), intent(in) :: filename
        character(len=:), allocatable :: dotStructure
        character(len=:), allocatable :: createNodes
        character(len=:), allocatable :: linkNodes
        
        createNodes = ''
        linkNodes = ''

        dotStructure = "digraph G{" // new_line('a')
        dotStructure = dotStructure // "node [shape=circle];" // new_line('a')

        if (associated(self%root)) then
            call RoamTree(self%root, createNodes, linkNodes)
        end if
        
        dotStructure = dotStructure // trim(createNodes) // trim(linkNodes) // "}" // new_line('a')
        call write_dot(filename, dotStructure)
        print *, "Archivo actualizado existosamente."
    end subroutine graph
    recursive subroutine RoamTree(current, createNodes, linkNodes)
        type(Node_t), pointer :: current
        character(len=:), allocatable, intent(inout) :: createNodes, linkNodes
        character(len=20) :: address, str_value

        if (associated(current)) then
            ! SE OBTIENE INFORMACION DEL NODO ACTUAL
          address = get_address_memory(current)
          write(str_value, '(I0)') current%Value
          createNodes = createNodes // '"' // trim(address) // '"' // '[label="' // trim(str_value) // '"];' // new_line('a')
          ! VIAJAMOS A LA SUBRAMA IZQ
          if (associated(current%Left)) then
            linkNodes = linkNodes // '"' // trim(address) // '"' // " -> "
            address = get_address_memory(current%Left)
            linkNodes = linkNodes // '"' // trim(address) // '" ' &
                      // '[label = "L"];' // new_line('a')
    
          end if
          ! VIAJAMOS A LA SUBRAMA DER
          if (associated(current%Right)) then
            address = get_address_memory(current)
            linkNodes = linkNodes // '"' // trim(address) // '"' // " -> "
            address = get_address_memory(current%Right)
            linkNodes = linkNodes // '"' // trim(address) // '" ' &
                      // '[label = "R"];' // new_line('a')
          end if
    
          call RoamTree(current%Left, createNodes, linkNodes)
          call RoamTree(current%Right, createNodes, linkNodes)
        end if
    end subroutine RoamTree
    subroutine write_dot(filename, code)
        character(len=*), intent(in) :: code, filename
        character(len=:), allocatable :: dot_filename, png_filename
        
        ! Agregar extensiones
        dot_filename = trim(filename) // ".dot"
        png_filename = trim(filename) // ".png"
        
        open(10, file="graph"//dot_filename, status='replace', action='write')
        write(10, '(A)') trim(code)
        close(10)

        ! Genera la imagen PNG
        call system("dot -Tpng graph"// dot_filename //" -o graph" // png_filename)
    end subroutine write_dot

    function get_address_memory(node) result(address)
        !class(matrix_t), intent(in) :: self
        type(Node_t), pointer :: node
        character(len=20) :: address
        ! integer 8
        integer*8 :: i
    
        i = loc(node) ! get the address of x
        ! convert the address to string
        write(address, 10) i 
        10 format(I0)
    
    end function get_address_memory

end module abb_m