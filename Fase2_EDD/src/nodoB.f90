module module_ordinal_user
    !use module_abbtree_layers
    implicit none

    type ordinal_user
    character(:), allocatable :: name
    integer(kind=8), allocatable :: DPI
    character(:), allocatable :: password
    contains
    end type
    contains
end module  module_ordinal_user