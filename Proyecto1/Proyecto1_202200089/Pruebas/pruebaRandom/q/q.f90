program main
    implicit none
    integer, dimension(100) :: numbers
    integer :: i, num, j, flag

    ! Inicializar el arreglo con ceros
    numbers = 0

    do i = 1, 100
        print *, "Ingrese un número:"
        read *, num

        ! Verificar si el número ya está en el arreglo
        flag = 0
        do j = 1, i-1
            if (numbers(j) == num) then
                flag = 1
                exit
            end if
        end do

        ! Si el número no está en el arreglo, lo guardamos
        if (flag == 0) then
            numbers(i) = num
        else
            print *, "El número ya está en el arreglo, ingrese otro."
        end if
    end do
end program main
