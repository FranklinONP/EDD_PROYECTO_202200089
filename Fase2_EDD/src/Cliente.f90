module clienteTemporal
    use abb_m
    !use listaAlbumes
    use avl_m
    implicit none
    type :: cliente
        
        character(len=100) :: nombre
        integer :: dpi
        character(len=100) :: password
        type(abb) :: tree
        type(avl) :: avl
        !type(List_of_lists) :: listaAlbumes
    end type cliente
end module clienteTemporal
