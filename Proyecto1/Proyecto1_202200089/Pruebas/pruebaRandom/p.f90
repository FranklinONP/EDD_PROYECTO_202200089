program random_number
    implicit none
    integer :: i,j
    real :: r
    call random_seed()  ! Inicializa el generador de números aleatorios
    call random_number(r)  ! Genera un número aleatorio entre 0 y 1
    i = int(1 + 2*r)  ! Escala, desplaza y convierte a entero para obtener un número entre 1 y 2
    print*, "Número aleatorio entero entre 1 y 2: ", i
    j=int(1+4*r)  ! Escala, desplaza y convierte a entero para obtener un número entre 1 y 4
    print*, "Número aleatorio entero entre 1 y 4: ", j
end program random_number
    