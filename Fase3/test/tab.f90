module modulo_tabla_hash
    implicit none
    private
    integer :: tamano_tabla = 7
    integer, parameter :: porcentaje_maximo = 70
    type tecnico
        integer(8) :: dpi, telefono
        character(:), allocatable::nombre, apellido, genero, direccion
    end type tecnico
    type, public :: TablaHash
        integer :: elemento = 0
        type(tecnico), allocatable :: arreglo(:)
        contains
        procedure :: insertar, imprimir, listar_tecnico
        procedure, private :: resolver_colision
        procedure :: tablaGraph
    end type TablaHash

contains
    subroutine insertar(self, dpi, nombre, apellido, direccion, telefono, genero)
        class(TablaHash), intent(inout) :: self
        type(TablaHash) :: nueva_tabla
        integer(8), intent(in) :: dpi, telefono
        character(:), allocatable::nombre, apellido, direccion, genero
        type(tecnico), allocatable :: arreglo_anterior(:)
        real :: porcentaje_utilizado
        integer(8) :: posicion
        if(.not. allocated(self%arreglo)) then
            allocate(self%arreglo(0:tamano_tabla-1))
            self%arreglo(:)%dpi = -1
        end if
        posicion = obtener_posicion(dpi)
        if(self%arreglo(posicion)%dpi /= -1 .and. self%arreglo(posicion)%dpi /= dpi) then
            call self%resolver_colision(posicion)
        end if
        self%arreglo(posicion)%dpi=dpi
        self%arreglo(posicion)%nombre=nombre
        self%arreglo(posicion)%apellido= apellido
        self%arreglo(posicion)%direccion=direccion
        self%arreglo(posicion)%telefono=telefono
        self%arreglo(posicion)%genero=genero
        self%elemento = self%elemento + 1
        porcentaje_utilizado = (self%elemento * 1.0/tamano_tabla) * 100
        if(porcentaje_utilizado > porcentaje_maximo) then
            arreglo_anterior = self%arreglo
            deallocate(self%arreglo)
            nueva_tabla = rehashing(arreglo_anterior)
            self%arreglo = nueva_tabla%arreglo
            self%elemento = nueva_tabla%elemento
        end if
    end subroutine insertar

    function rehashing(arreglo_anterior) result(nueva_tabla)
        type(tecnico), intent(in) :: arreglo_anterior(:)
        integer :: i
        type(TablaHash) :: nueva_tabla
        tamano_tabla = tamano_tabla*2
        allocate(nueva_tabla%arreglo(0:tamano_tabla-1))
        nueva_tabla%arreglo(:)%dpi = -1
        do i = 1, size(arreglo_anterior)
            if(arreglo_anterior(i)%dpi /= -1) then
            call nueva_tabla%insertar(arreglo_anterior(i)%dpi,arreglo_anterior(i)%nombre,&
            arreglo_anterior(i)%apellido, &
            arreglo_anterior(i)%direccion,&
            arreglo_anterior(i)%telefono, arreglo_anterior(i)%genero)
            end if
        end do
    end function rehashing

    subroutine resolver_colision(self, posicion)
        class(TablaHash), intent(inout) :: self
        integer(8), intent(inout) :: posicion
        do while(self%arreglo(posicion)%dpi /= -1)
            posicion = posicion + 1
            posicion = mod(posicion, tamano_tabla)
        end do
    end subroutine resolver_colision

    function obtener_posicion(dpi) result(posicion)
        integer(8), intent(in) :: dpi
        integer(8) :: posicion
        posicion = mod(dpi,tamano_tabla)
    end function obtener_posicion

    subroutine imprimir(self, dpi)
        class(TablaHash), intent(inout) :: self
        integer(8), intent(in) :: dpi
        integer(8) :: posicion
        posicion = obtener_posicion(dpi)
        if (self%arreglo(posicion)%dpi == dpi) then
            print*, 'DPI: ', self%arreglo(posicion)%dpi
            print*, 'Nombre: ', trim(self%arreglo(posicion)%nombre)
            print*, 'Apellido: ', trim(self%arreglo(posicion)%apellido)
            print*, 'Direccion: ', trim(self%arreglo(posicion)%direccion)
            print*, 'Telefono: ', self%arreglo(posicion)%telefono
            print*, 'Genero: ', trim(self%arreglo(posicion)%genero)
        else
            print*, 'No Existe Un Tecnico Con DPI: ',dpi
        end if
    end subroutine imprimir

    subroutine listar_tecnico(self)
        class(TablaHash), intent(inout) :: self
        integer :: i
        do i = 0, size(self%arreglo)-1
            if (self%arreglo(i)%dpi /= -1) then
                print*, 'Posicion: ', i
                print*, 'DPI: ', self%arreglo(i)%dpi
                print*, 'Nombre: ', trim(self%arreglo(i)%nombre)
                print*, 'Apellido: ', trim(self%arreglo(i)%apellido)
                print*, 'Direccion: ', trim(self%arreglo(i)%direccion)
                print*, 'Telefono: ', self%arreglo(i)%telefono
                print*, 'Genero: ', trim(self%arreglo(i)%genero)
                print*, '------------------------'
            end if
        end do
    end subroutine listar_tecnico

subroutine tablaGraph(self)
  class(TablaHash), intent(in) :: self
  integer :: indice = 0
  character(len=100) :: dotFile, dotFilePath, pngFilePath
  character(len=200) :: command
 character(len=100) :: nombre, apellido, direccion, genero, index, dpi, telefono
    integer :: i

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

do i = 0, size(self%arreglo)-1
    write(index, '(I0)') indice
    write(dpi, '(I0)') self%arreglo(i)%dpi
    write(nombre, '(A)') self%arreglo(i)%nombre
    write(apellido, '(A)') self%arreglo(i)%apellido
    write(direccion, '(A)') self%arreglo(i)%direccion
    write(telefono, '(I0)') self%arreglo(i)%telefono
    write(genero, '(A)') self%arreglo(i)%genero

    write(10, '(A)') "      <tr>"
    write(10, '(A)') "        <td>Posicion: " // trim(index) // "</td>"
    write(10, '(A)') "        <td>Valor: " // trim(dpi) // "</td>"
    write(10, '(A)') "        <td>Nombre: " // trim(nombre) // "</td>"
    write(10, '(A)') "        <td>Apellido: " // trim(apellido) // "</td>"
    write(10, '(A)') "        <td>Direccion: " // trim(direccion) // "</td>"
    write(10, '(A)') "        <td>Telefono: " // trim(telefono) // "</td>"
    write(10, '(A)') "        <td>Genero: " // trim(genero) // "</td>"
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
end subroutine tablaGraph
    
end module modulo_tabla_hash

program main
use modulo_tabla_hash
implicit none
type(TablaHash) :: tabla
integer(8) :: dpi, telefono
character(len=50) :: nombre, apellido, direccion, genero

print*, 'Ingrese DPI: '

end program main