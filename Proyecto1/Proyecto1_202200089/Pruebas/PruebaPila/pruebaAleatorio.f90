program generador_aleatorio_con_semilla
    implicit none
    integer, dimension(1000) :: numeros_usados
    integer :: aleatorio, i, entero
    real :: x

    ! Initialize the array to zeros
    numeros_usados = 0

    do i = 1, 10
        call random_number(x)
        entero = INT(x * 1000) + 1

        ! Check if the number has already been used
        if (numeros_usados(entero) == 0) then
            print *, "Número generado:", entero
            numeros_usados(entero) = 1
        else
            ! Generate a new number until an unused one is found
            do
                call random_number(x)
                entero = INT(x * 1000) + 1
                if (numeros_usados(entero) == 0) then
                    print *, "Número generado:", entero
                    numeros_usados(entero) = 1
                    exit
                end if
            end do
        end if
    end do

end program generador_aleatorio_con_semilla

