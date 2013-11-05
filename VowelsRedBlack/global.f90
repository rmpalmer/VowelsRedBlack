module global
    use node_definition
    implicit none
    private

    logical, parameter :: red = .true., black = .false.
    type(node), pointer :: root, placeholder

    public :: red, black, root, placeholder

end module global
