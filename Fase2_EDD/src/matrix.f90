module matrix_m
use linked_list_m
    implicit none
    private
    type :: node_val
        private
        logical :: exists = .false.
        logical :: valor
        character(len=7):: color
    end type node_val

    type :: node
        private
        integer :: i,j
        logical :: valor = .false.
        character(len=7) :: color
        type(node), pointer :: arriba => null()
        type(node), pointer :: abajo => null()
        type(node), pointer :: derecha => null()
        type(node), pointer :: izquierda => null()
    end type node

    type, public :: matrix
        private
        type(node), pointer :: root => null()
        integer :: width = 0
        integer :: height = 0
    contains
        procedure :: insert
        procedure :: insertarCabeceraFila
        procedure :: insertarCabeceraColumna
        procedure :: buscarFila
        procedure :: buscarColumna
        procedure :: existeNodo
        procedure :: print
        procedure :: imprimirEncabezadoColumnas
        procedure :: graficar
        procedure :: tabla
        procedure :: obtenerValor
        procedure :: getPixels
    end type matrix

contains
    subroutine insert(self, i, j, valor,color)
        class(matrix), intent(inout) :: self
        integer, intent(in) :: i
        integer, intent(in) :: j
        logical, intent(in) :: valor
        character(len=7), intent(in) :: color

        type(node), pointer :: nuevo
        type(node), pointer :: fila
        type(node), pointer :: columna

        allocate(nuevo)
        nuevo = node(i=i, j=j, valor=valor,color=color)

        if(.not. associated(self%root)) then
            allocate(self%root)
            self%root = node(i=-1, j=-1,valor=.true.,color="black..")
        end if

        fila => self%buscarFila(j)
        columna => self%buscarColumna(i)

        if(i > self%width) self%width = i
        if(j > self%height) self%height = j

        if(.not. self%existeNodo(nuevo)) then
            if(.not. associated(columna)) then
                columna => self%insertarCabeceraColumna(i)
            end if

            if(.not. associated(fila)) then
                fila => self%insertarCabeceraFila(j)
            end if
            call insertarEnColumna(nuevo, fila)
            call insertarEnFila(nuevo, columna)
        end if
    end subroutine insert

    function insertarCabeceraColumna(self, i) result(nuevaCabeceraColumna)
        class(matrix), intent(inout) :: self
        integer, intent(in) :: i

        type(node), pointer :: nuevaCabeceraColumna
        allocate(nuevaCabeceraColumna)

        nuevaCabeceraColumna = node(i=i, j=-1,color="black..")
        call insertarEnColumna(nuevaCabeceraColumna, self%root)
    end function insertarCabeceraColumna
    
    function insertarCabeceraFila(self, j) result(nuevaCabeceraFila)
        class(matrix), intent(inout) :: self
        integer, intent(in) :: j

        type(node), pointer :: nuevaCabeceraFila
        allocate(nuevaCabeceraFila)

        nuevaCabeceraFila = node(i=-1, j=j,color="black..")
        call insertarEnFila(nuevaCabeceraFila, self%root)
    end function insertarCabeceraFila

    subroutine insertarEnFila(nuevo, CabeceraFila)
        type(node), pointer :: nuevo
        type(node), pointer :: cabeceraFila !head 

        type(node), pointer :: actual
        actual => cabeceraFila

        do while(associated(actual%abajo))
            if(nuevo%j < actual%abajo%j) then

                nuevo%abajo => actual%abajo
                nuevo%arriba => actual
                actual%abajo%arriba => nuevo
                actual%abajo => nuevo
                exit
            end if
            actual => actual%abajo
        end do

        if(.not. associated(actual%abajo)) then
            actual%abajo => nuevo
            nuevo%arriba => actual
        end if
    end subroutine insertarEnFila

    subroutine insertarEnColumna(nuevo, CabeceraColumna)
        type(node), pointer :: nuevo
        type(node), pointer :: CabeceraColumna !head 

        type(node), pointer :: actual
        actual => CabeceraColumna

        do while(associated(actual%derecha))
            if(nuevo%i < actual%derecha%i) then

                nuevo%derecha => actual%derecha
                nuevo%izquierda => actual
                actual%derecha%izquierda => nuevo
                actual%derecha => nuevo
                exit
            end if
            actual => actual%derecha
        end do

        if(.not. associated(actual%derecha)) then
            actual%derecha => nuevo
            nuevo%izquierda => actual
        end if
    end subroutine insertarEnColumna  

    function buscarFila(self, j) result(actual)
        class(matrix), intent(in) :: self
        integer, intent(in) :: j

        type(node), pointer :: actual
        actual => self%root

        do while(associated(actual)) 
            if(actual%j == j) return
            actual => actual%abajo
        end do
    end function buscarFila

    function buscarColumna(self, i) result(actual)
        class(matrix), intent(in) :: self
        integer, intent(in) :: i

        type(node), pointer :: actual
        actual => self%root 
        
        do while(associated(actual))
            if(actual%i == i) return
            actual => actual%derecha
        end do
    end function buscarColumna

    function existeNodo(self, nodo) result(existe)
        class(matrix), intent(inout) :: self
        type(node), pointer, intent(in) :: nodo

        logical :: existe
        type(node), pointer :: encabezadoFila
        type(node), pointer :: columna
        encabezadoFila => self%root
        existe = .false.

        do while(associated(encabezadoFila))
            if(encabezadoFila%j == nodo%j) then
                columna => encabezadoFila
                do while(associated(columna)) 
                    if(columna%i == nodo%i) then
                        columna%valor = nodo%valor
                        columna%color = nodo%color
                        existe = .true.
                        return
                    end if
                    columna => columna%derecha
                end do
                return
            end if
            encabezadoFila => encabezadoFila%abajo
        end do
        return
    end function existeNodo

    subroutine graficar(self)
        class(matrix), intent(in) :: self
        
        integer :: io
        integer :: i
        character(len=10) :: str_i
        character(len=10) :: str_j
        character(len=10) :: str_i_aux
        character(len=10) :: str_j_aux
        character(len=150) :: node_dec
        character(len=20) :: nombre

        character(len=100) :: comando
        character(len=50) :: contenido
        character(:), allocatable :: rank
        character(:), allocatable :: conexion
        character(:), allocatable :: conexionRev
        type(node), pointer :: fila_aux
        type(node), pointer :: columna_aux
        io = 1
        fila_aux => self%root
        comando = "dot  -Gnslimit=2 -Tpng ./matrix.dot -o ./matrix.png"

        open(newunit=io, file="./matrix.dot")

        write(io, *) "digraph Matrix {"
        write(io, *) 'node[shape = "box"]'

        do while (associated(fila_aux))
            rank = "{rank=same"
            columna_aux => fila_aux
            do while(associated(columna_aux)) 
                write(str_i, '(I10)') columna_aux%i + 1
                write(str_j, '(I10)') columna_aux%j + 1
                nombre = '"Nodo'//trim(adjustl(str_i))//'_'//trim(adjustl(str_j))//'"'

                if (columna_aux%i == -1 .and. columna_aux%j == -1) then
                    node_dec = trim(adjustl(nombre))//'[label = "root", group="'//trim(adjustl(str_i))//'"]'

                else if(columna_aux%i == -1) then
                    write(str_j_aux, '(I10)') columna_aux%j
                    contenido = trim(adjustl(str_j_aux))
                    node_dec = trim(adjustl(nombre))//'[label = "'//trim(adjustl(contenido))
                    node_dec = trim(adjustl(node_dec))//'", group="'//trim(adjustl(str_i))//'"]'
                    
                else if(columna_aux%j == -1) then
                    write(str_i_aux, '(I10)') columna_aux%i
                    contenido = trim(adjustl(str_i_aux))
                    node_dec = trim(adjustl(nombre))//'[label = "'//trim(adjustl(contenido))
                    node_dec = trim(adjustl(node_dec))//'", group="'//trim(adjustl(str_i))//'"]'
                        
                else
                    if(columna_aux%valor) then
                        contenido = columna_aux%color
                    else
                        contenido = 'F'
                    end if 
                    node_dec = trim(adjustl(nombre))//'[label = "'//trim(adjustl(contenido))
                    node_dec = trim(adjustl(node_dec))//'", style = filled, fillcolor= "'//&
                    & trim(contenido)//'" group="'// &
                    & trim(adjustl(str_i))//'"]'
                                        

                end if
                write(io, *) node_dec

                if(associated(columna_aux%derecha)) then
                    conexion = '"Nodo'//trim(adjustl(str_i))//'_'//trim(adjustl(str_j))//'"->'

                    write(str_i_aux, '(I10)') columna_aux%derecha%i + 1
                    write(str_j_aux, '(I10)') columna_aux%derecha%j + 1

                    conexion = conexion//'"Nodo'//trim(adjustl(str_i_aux))//'_'//trim(adjustl(str_j_aux))//'"'
                    conexionRev = conexion//'[dir = back]'
                    write(io, *) conexion
                    write(io, *) conexionRev
                end if

                if(associated(columna_aux%abajo)) then
                    conexion = '"Nodo'//trim(adjustl(str_i))//'_'//trim(adjustl(str_j))//'"->'

                    write(str_i_aux, '(I10)') columna_aux%abajo%i + 1
                    write(str_j_aux, '(I10)') columna_aux%abajo%j + 1

                    conexion = conexion//'"Nodo'//trim(adjustl(str_i_aux))//'_'//trim(adjustl(str_j_aux))//'"'
                    conexionRev = conexion//'[dir = back]'
                    write(io, *) conexion
                    write(io, *) conexionRev
                end if

                rank = rank//';"Nodo'//trim(adjustl(str_i))//'_'//trim(adjustl(str_j))//'"'
                columna_aux => columna_aux%derecha
            end do
            rank = rank//'}'
            write(io, *) rank

            fila_aux => fila_aux%abajo
        end do
        write(io, *) "}"
        close(io)
        
        call execute_command_line(comando, exitstat=i)

        if ( i == 1 ) then
            print *, "Ocurrió un error al momento de crear la imagen"
        else
            print *, "La imagen fue generada exitosamente"

        end if
    end subroutine graficar


    subroutine print(self)
        class(matrix), intent(in) :: self
        integer :: i
        integer :: j
        type(node), pointer :: aux
        type(node_val) :: val
        aux => self%root%abajo

        call self%imprimirEncabezadoColumnas()

        do j = 0, self%height
            print *, ""
            write(*, fmt='(I3)', advance='no') j

            do i = 0, self%width
                val = self%obtenerValor(i,j)
                if(.not. val%exists) then
                    write(*, fmt='(I3)', advance='no') 0
                else
                    write(*, fmt='(L3)', advance='no') val%valor
                end if

            end do
        end do
    end subroutine print


    subroutine getPixels(self,mtx)
        class(matrix), intent(in) :: self
        integer :: i,j
        type(matrix), intent(inout) :: mtx
        type(node_val) :: val

        print*, "Get pixels --->"
        do j = 0, self%height
            do i = 0, self%width
                val = self%obtenerValor(i,j)
                if(.not. val%exists) then
                else  
                    print * , '----'  
                    print * , 'i:',i,'j:',j,'val:',val%valor,'color:',val%color          
                    call mtx%insert(i,j,.true.,val%color) 
                    print * , '----'
                end if
            end do
        end do 
        print*, "Get pixels <---"
    end subroutine getPixels




subroutine tabla(self, dotFileName)
  class(matrix), intent(in) :: self
  character(len=*), intent(in) :: dotFileName

  integer :: i, j
  integer, parameter :: unit = 10
  type(node), pointer :: aux
  type(node_val) :: val
  character(len=3) :: jStr
  character(len=20) :: numStr
  character(len=100) :: dotFile, dotFilePath, pngFilePath
  character(len=200) :: command

  ! Add '.dot' extension to the filename
  character(len=*), parameter :: dotExtension = ".dot"
  character(len=*), parameter :: pngExtension = ".png"

  ! Concatenate the filename with the extension
  dotFile = trim(dotFileName) // dotExtension

  ! Set the DOT and PNG file paths
  dotFilePath = dotFile
  pngFilePath = trim(dotFileName) // pngExtension

  ! Open the DOT file for writing
  open(unit, file=dotFilePath, status='replace')

  ! Write the DOT code to the file
  write(unit, '(A)') "digraph {"
  write(unit, '(A)') "  node [ shape=plaintext fontname=Helvetica ]"
  write(unit, '(A)') ""
  write(unit, '(A)') "  n [ label = <"
  write(unit, '(A)') "    <table border=""0"" cellborder=""0"" cellspacing=""0"" bgcolor=""white"">"

  do j = 0, self%height
    write(unit, '(A)') "      <tr>"

    do i = 0, self%width
      val = self%obtenerValor(i, j)

      if (.not. val%exists) then
        write(unit, '(A)') "        <td></td>"
      else
        write(unit, '(A)') "        <td bgcolor=""" // trim(val%color) // """></td>"
      end if

    end do

    write(unit, '(A)') "      </tr>"
  end do

  write(unit, '(A)') "    </table>"
  write(unit, '(A)') "  > ]"
  write(unit, '(A)') ""
  write(unit, '(A)') "}"

  ! Close the DOT file
  close(unit)

   ! Convert DOT to PNG using Graphviz
  command = "dot -Gnslimit=2 -Tpng -o " // trim(pngFilePath) // " " // trim(dotFilePath)
  call system(command)
end subroutine tabla


    subroutine imprimirEncabezadoColumnas(self)
        class(matrix), intent(in) :: self
        integer :: i

        do i=-1, self%width
            write(*, fmt='(I3)', advance='no') i
        end do
    end subroutine imprimirEncabezadoColumnas


    function obtenerValor(self, i, j) result(val)
        class(matrix), intent(in) :: self
        integer, intent(in) :: i
        integer, intent(in) :: j

        type(node), pointer :: cabeceraFila
        type(node), pointer :: columna
        type(node_val) :: val
        cabeceraFila => self%root

        do while(associated(cabeceraFila))
            if(cabeceraFila%j == j) then
                columna => cabeceraFila
                do while(associated(columna)) 
                    if(columna%i == i) then
                        val%valor = columna%valor
                        val%color = columna%color
                        val%exists = .true.
                        return
                    end if
                    columna => columna%derecha
                end do
                return
            end if
            cabeceraFila => cabeceraFila%abajo
        end do
        return
    end function obtenerValor
end module matrix_m