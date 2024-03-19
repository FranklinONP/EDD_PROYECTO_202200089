PROGRAM main
    IMPLICIT NONE
    CALL generate_graphviz(5, 5, 'red', 'graph.dot')
    PRINT *, 'Archivo Graphviz generado: graph.dot'
    STOP
END PROGRAM main

SUBROUTINE generate_graphviz(n, m, color, filename)
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: n, m
    CHARACTER(LEN=*), INTENT(IN) :: color, filename
    INTEGER :: i, j
    CHARACTER(LEN=100) :: line

    OPEN(UNIT=10, FILE=filename, STATUS='unknown')

    WRITE(10, '(A)') 'digraph G {'
    WRITE(10, '(A)') '  node [shape=plaintext];'
    WRITE(10, '(A)') '  graph [rankdir=LR];'

    DO i = 1, n
        DO j = 1, m
            WRITE(line, '(A,I0,A,I0,A,A,A)') '  "', i, ',', j, &
            & '" [label=< <TABLE BORDER="0" CELLBORDER="1" CELLSPACING="0"><TR><TD BGCOLOR="', color, '"> </TD></TR></TABLE> >];'
            WRITE(10, '(A)') TRIM(line)
        END DO
    END DO

    WRITE(10, '(A)') '}'

    CLOSE(10)
END SUBROUTINE generate_graphviz
