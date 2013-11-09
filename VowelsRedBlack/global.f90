module global
    use node_definition
    implicit none
    private

    logical, parameter :: red = .true., black = .false.
    type(node), pointer :: root, placeholder
    integer :: current_depth, max_depth

    public :: red, black, root, placeholder, current_depth, max_depth

end module global
