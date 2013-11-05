module node_definition
    implicit none
    private

    type node
        integer :: item
        logical :: color
        type(node), pointer :: left
        type(node), pointer :: right
        type(node), pointer :: parent
    end type node

    public :: node

end module node_definition
