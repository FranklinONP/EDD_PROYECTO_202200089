module tabla_hash_m
    implicit none
    private
    integer :: M = 13
    real :: R = 0.618034
    integer, parameter :: long = selected_int_kind(4)
    integer, parameter :: dp = selected_real_kind(15)

    type :: Tecnico
         integer::dpi
         character(len=:), allocatable :: nombre,apellido,genero,direccion,telefono
        ! Otros atributos de tipo que necesites
    end type Tecnico

    type, public :: TablaHash
        integer :: n = 0
        type(Tecnico), allocatable :: tabla(:)
       

    contains
        procedure :: insert
        procedure :: print
        procedure :: search
        
        procedure, private :: pruebaLineal
    end type TablaHash

contains
    subroutine insert(self, key, nombre, apellido, genero, direccion, telefono)
        class(TablaHash), intent(inout) :: self 
        type(TablaHash) :: ret
        integer, allocatable :: temp(:)
        integer(long), intent(in) :: key
        character(len=*), intent(in) :: nombre, apellido, genero, direccion, telefono
        integer :: pos

        if(.not. allocated(self%tabla)) then
            allocate(self%tabla(0:M-1))
            self%tabla(:) = -1
        end if

        pos = funcion_hash(key)!en parametros mando lo que tenga.. en este caso es un entero por eso

        if(self%tabla(pos) /= -1 .and. self%tabla(pos) /= key) then
            call self%pruebaLineal(pos)
        end if

        self%tabla(pos) = key

        self%n = self%n + 1

        if(self%n * 1.0_dp/M > 0.75) then
            temp = self%tabla
            deallocate(self%tabla)
            ret = rehashing(temp)
            self%tabla = ret%tabla
            self%n = ret%n
        end if
    end subroutine

    function rehashing(temp) result(val)
        integer, intent(in) :: temp(:)
        integer :: i
        type(TablaHash) :: val

        M = M*2
        allocate(val%tabla(0:M-1))
        val%tabla(:) = -1

        do i = 1, size(temp)
            if(temp(i) /= -1) then
                call val%insert(int(temp(i), kind=long))
            end if
        end do
    end function

    subroutine pruebaLineal(self, pos)
        class(TablaHash), intent(inout) :: self
        integer, intent(inout) :: pos

        do while(self%tabla(pos) /= -1)
            pos = pos + 1
            pos = mod(pos, M)
        end do
    end subroutine pruebaLineal

    subroutine search(self, val)
        class(TablaHash), intent(in) :: self
        integer, intent(in) :: val
        integer :: pos

        pos = funcion_hash(int(val, kind=long))
        print *, self%tabla(pos)
    end subroutine search

    

subroutine print(self)
  class(TablaHash), intent(in) :: self
  integer :: indice = 0
  character(len=100) :: dotFile, dotFilePath, pngFilePath
  character(len=200) :: command
 character(len=100) :: value,index

  ! Add '.dot' and '.png' extensions to the filename
  character(len=*), parameter :: dotExtension = ".dot"
  character(len=*), parameter :: pngExtension = ".png"
  dotFile = "tablaTecnicos" // dotExtension
  dotFilePath = dotFile
  pngFilePath = "tablaTecnicos" // pngExtension

  ! Open the DOT file for writing
  open(unit=10, file=dotFilePath, status='replace')

  ! Write the DOT code to the file
  write(10, '(A)') "digraph {"
  write(10, '(A)') "  node [ shape=plaintext fontname=Helvetica ]"
  write(10, '(A)') ""
  write(10, '(A)') "  n [ label = <"
  write(10, '(A)') "    <table border=""0"" cellborder=""1"" cellspacing=""0"" bgcolor=""white"" color=""black"">"

  do while (indice < M)
    write(index, '(I0)') indice
    write(value, '(I0)') self%tabla(indice)

    write(10, '(A)') "      <tr>"
    write(10, '(A)') "        <td>Posicion: " // trim(index) // "</td>"
    write(10, '(A)') "        <td>Valor: " // trim(value) // "</td>"
    write(10, '(A)') "      </tr>"
    indice = indice + 1
  end do

  write(10, '(A)') "    </table>"
  write(10, '(A)') "  > ]"
  write(10, '(A)') ""
  write(10, '(A)') "}"

  ! Close the DOT file
  close(unit=10)

  ! Convert DOT to PNG using Graphviz
  command = "dot -Gnslimit=2 -Tpng -o " // trim(pngFilePath) // " " // trim(dotFilePath)
  call system(command)

  ! Open the PNG file
  command = "start " // trim(pngFilePath)
  call system(command)
end subroutine print

    function funcion_hash(x) result(v)
        integer(long), intent(in) :: x
        real :: d
        integer :: v

        d = R*x - floor(R*x)
        v = floor(M*d)
    end function funcion_hash



end module tabla_hash_m

program main
    use tabla_hash_m
    implicit none
    
    type(TablaHash) :: tabla
    integer, parameter :: long = selected_int_kind(4)

    call tabla%insert(int(5, kind=long),"juan")

    call tabla%print()
    !call tabla%search(522)



end program main