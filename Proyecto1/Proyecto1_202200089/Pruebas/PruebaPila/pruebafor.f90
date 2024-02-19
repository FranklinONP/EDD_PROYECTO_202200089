program loop
    implicit none
    integer :: i

    do i = 1, 10
        print*, "Iteración: ", i
    end do

end program loop



subroutine colar(this, id, ig, ip, nombre) 
    class(List_of_lists), intent(inout) :: this
    integer, intent(out) :: id
    integer, intent(out) :: ig, ip
    character(len=*), intent(out) :: nombre

    type(node), pointer :: current


    current => this%head

        if(current%encolar .eqv. .true.) then
            id = current%index
            ig = current%ig
            ip = current%ip
            nombre = current%name
            current%encolar = .false.
            print *, 'El nodo ',current%index,'Con img G=',ig,' e img P=',ip,'  ha sido encolado'
            print*, '----------------------------------------------- ================================='
            exit
        end if

end subroutine colar