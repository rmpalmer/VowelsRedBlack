module group

    implicit none
    private

    public :: create_node, insert_in_tree, rotate_left, rotate_right, rebalance_tree, traverse, display_sideways

  contains

    subroutine insert_in_tree(new)
        !
        use node_definition
        use global
        type(node), pointer :: new
        type(node), pointer :: v, w
        v => root
        nullify(w)
        do
            if (.not. associated(v)) then
                exit
            end if
            w => v
            if (new%item < v%item) then
                v => v%left
            else
                v => v%right
            end if
        end do
        new%parent => w
        if (.not. associated(w)) then
            root => new
        else if (new%item < w%item) then
            w%left => new
        else
            w%right => new
        end if
        !
    end subroutine insert_in_tree

    subroutine rotate_left(pivot)
        use node_definition
        use global
        type(node), pointer :: pivot
        type(node), pointer :: y, x
        !
        x => pivot
        y => x%right
        x%right => y%left
        if (associated(y%left)) then
            y%left%parent => x
        end if
        y%parent => x%parent
        if (.not. associated(x%parent)) then
            root => y
        else if (associated(x, x%parent%left)) then
            x%parent%left => y
        else
            x%parent%right => y
        end if
        y%left => x
        x%parent => y
    end  subroutine rotate_left

    subroutine rotate_right(pivot)
        use node_definition
        use global
        type(node), pointer :: pivot
        type(node), pointer :: y, x
        !
        x => pivot
        y => x%left
        x%left => y%right
        if (associated(y%right)) then
            y%right%parent => x
        end if
        y%parent => x%parent
        if (.not. associated(x%parent)) then
            root => y
        else if (associated(x, x%parent%right)) then
            x%parent%right => y
        else
            x%parent%left => y
        end if
        y%right => x
        x%parent => y
    end subroutine rotate_right

    subroutine rebalance_tree(current)
        use node_definition
        use global
        type(node), pointer :: current
        type(node), pointer :: x, y
        logical :: red_uncle
        logical :: iterating
        !
        x => current
        do
            iterating = .not. associated(x, root)
            if (iterating) then
                iterating = x%parent%color .eqv. red
            end if
            if (iterating) then
                iterating = associated(x%parent%parent)
            end if
            if (.not. iterating) then
                exit
            end if
            if (associated(x%parent, x%parent%parent%left)) then
                y => x%parent%parent%right
                red_uncle = associated(y)
                if (red_uncle) then
                    red_uncle = y%color .eqv. red
                end if
                if (red_uncle) then
                    x%parent%color = black
                    y%color = black
                    x%parent%parent%color = red
                    x => x%parent%parent
                else
                    if (associated(x, x%parent%right)) then
                        x => x%parent
                        call rotate_left(x)
                    end if
                    x%parent%color = black
                    x%parent%parent%color = red
                    call rotate_right(x%parent%parent)
                end if
            else
                y => x%parent%parent%left
                red_uncle = associated(y)
                if (red_uncle) then
                    red_uncle = y%color .eqv. red
                end if
                if (red_uncle) then
                    x%parent%color = black
                    y%color = black
                    x%parent%parent%color = red
                    x => x%parent%parent
                else
                    if (associated(x, x%parent%left)) then
                        x => x%parent
                        call rotate_right(x)
                    end if
                    x%parent%color = black
                    x%parent%parent%color = red
                    call rotate_left(x%parent%parent)
                end if
            end if
        end do
        root%color = black
        !
    end subroutine rebalance_tree

    recursive subroutine traverse(current)
        use node_definition
        type(node), pointer :: current
        !
        if (associated(current%left)) then
            call traverse(current%left)
        end if
        print *, current%item
        if (associated(current%right)) then
            call traverse(current%right)
        end if
    end subroutine traverse

    subroutine create_node(number)
        use node_definition
        use global
        integer :: number
        !
        allocate(placeholder)
        placeholder%item = number
        placeholder%color = red
        nullify(placeholder%left)
        nullify(placeholder%right)
        call insert_in_tree(placeholder)
    end subroutine create_node

    recursive subroutine visit(current)
        use node_definition
        use global
        type(node), pointer :: current
        !
        current_depth = current_depth + 1
        if (current_depth > max_depth) then
            max_depth = current_depth
        end if
        if (associated(current%left)) then
            call visit(current%left)
            current_depth = current_depth - 1
        end if
        if (associated(current%right)) then
            call visit(current%right)
            current_depth = current_depth - 1
        end if
    end subroutine visit

    subroutine display_sideways
        use node_definition
        use global
        type(node), pointer :: ptr
        integer :: max_nodes, &
                   node_no, &
                   level, &
                   mask, &
                   dmask, &
                   nlevels
        logical :: printed
        character(len=10) :: layout
        !
        current_depth = 0
        max_depth = 0
        call visit(root)
        max_nodes = 2**max_depth
        write (*,'(''max_nodes: '',i4)') max_nodes
        do node_no = 1, max_nodes-1
            ptr => root
            mask = max_nodes/2
            printed = .false.
            do level=1,1000,1
                if(iand(node_no,mask) /= 0) then
                    ptr => ptr%right
                else
                    if ((modulo(node_no, mask) == 0) .or. (mask == 1)) then
                        write(unit=*,fmt=*)
                        dmask = 2*mask
                        if (node_no /= max_nodes/2) then
                            write(unit=layout,fmt="(a,i3,a)") "(tr",10*(level-1)+1, ",a)"
                            if (iand(node_no,dmask) /= 0) then
                                write(unit=*,fmt=layout,advance="no") "\"
                            else
                                write(unit=*,fmt=layout,advance="no") "/"
                            end if
                        end if
                        write(unit=*,fmt="(i3)",advance="no") ptr%item
                        printed = .true.
                    end if
                    ptr => ptr%left
                end if
                mask = mask / 2
                if ((level >= max_depth) .or. (.not. associated(ptr)) .or. printed) then
                    exit
                end if
            end do
            if (.not. printed) then
                do nlevels = level, max_depth
                    print *
                end do
            end if
        end do
    end subroutine display_sideways

end module group
