program main
    use node_definition
    use global
    use group
    implicit none
    !
    print *,"begin"
    nullify(root)
    call create_node(0)
    root => placeholder
    nullify(root%parent)
    root%color = black

    call create_node(4)
    call rebalance_tree(placeholder)
    call create_node(3)
    call rebalance_tree(placeholder)
    call create_node(2)
    call rebalance_tree(placeholder)
 !   call create_node(4)
 !   call rebalance_tree(placeholder)
 !   call create_node(-3)
 !   call rebalance_tree(placeholder)
 !   call create_node(4)
 !   call rebalance_tree(placeholder)
 !   call create_node(1)
 !   call rebalance_tree(placeholder)
 !   call create_node(-2)
 !   call rebalance_tree(placeholder)
 !   call create_node(9)
 !   call rebalance_tree(placeholder)
    call traverse(root)
    print *,'now display'
    call display_sideways()
    print *,"end"
    !
end program main
