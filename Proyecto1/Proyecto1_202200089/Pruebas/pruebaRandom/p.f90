program random_number
    implicit none
    character(len=6) :: nombres(50)
    character(len=20) :: desicion
    integer :: i,j,m
    real :: r
    call random_seed()  ! Inicializa el generador de números aleatorios
    call random_number(r)  ! Genera un número aleatorio entre 0 y 1
    i = int(1 + 2*r)  ! Escala, desplaza y convierte a entero para obtener un número entre 1 y 2
    print*, "Número aleatorio entero entre 1 y 2: ", i
    j=int(1+4*r)  ! Escala, desplaza y convierte a entero para obtener un número entre 1 y 4
    print*, "Número aleatorio entero entre 1 y 4: ", j

    nombres = [ &
    'Carlos', 'Maria ', 'Juan  ', 'Ana   ', 'Jose  ', &
    'Carmen', 'Franci', 'Isabel', 'Manuel', 'Laura ', &
    'Antoni', 'Lucia ', 'Jesus ', 'Marta ', 'David ', &
    'Sofia ', 'Pedro ', 'Sara  ', 'Javier', 'Paula ', &
    'Daniel', 'Andrea', 'Rafael', 'Sandra', 'Miguel', &
    'Beatr ', 'Fernan', 'Alicia', 'Pablo ', 'Teresa', &
    'Luis  ', 'Elena ', 'Sergio', 'Rosa  ', 'Jorge ', &
    'Silvia', 'Albert', 'Julia ', 'Ricard', 'Patric', &
    'Angel ', 'Cristi', 'Mario ', 'Raquel', 'Diego ', &
    'Susana', 'Alvaro', 'Gabrie', 'Adrian', 'Nomb50']

    desicion='no'

    do m=1,j
        call random_number(r)
        i = int(1 + 50*r)
        print*, nombres(i)
        print*, "¿Desea otro nombre? (si/no)"
    end do

end program random_number
