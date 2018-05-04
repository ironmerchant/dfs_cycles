module search_routines
  use class_Graph
  use class_Stack
  implicit none
contains

  function process_dfs_neighbors(G, vertex_u) result(out_vertex)
      Class(Graph) :: G
      integer :: vertex_w, out_vertex, vertex_u, p_vertex_u, p_vertex_w
      type(StackIterator) :: adj_list
      out_vertex = -1

      ! get adjacency list iterator
      adj_list = G%get_adj_list(vertex_u)
      p_vertex_u = G%get_predecessor(vertex_u)

      ! for each vertex in adjacency_list
      do while (adj_list%has_next())

        ! get the next item as w
        vertex_w = adj_list%next()
        p_vertex_w = G%get_predecessor(vertex_w)

        ! if u has a gray neighbour, there is a cycle
        if ((G%get_color(vertex_w) == GRAY) .and. (p_vertex_u == p_vertex_w)) then
            print *, "A cycle!"

        ! else if u has a white neighbour w, then bail
        else if (G%get_color(vertex_w) == WHITE) then
          out_vertex = vertex_w
          exit
        end if
      enddo
  end function process_dfs_neighbors

  subroutine dfs(g)
    Class(Graph) :: G
    type(Stack) :: S
    integer :: initial_vertex = 1, vertex_u = -1, vertex_w = -1, predecessor

    !all vertices coloured white previously when G was initialized
    !create a stack S
    call S%init()

    !push v onto S
    call S%push(initial_vertex)

    !while S is non-empty
    do while (S%has_items() .eqv. .True.)

      ! Get top of S as u
      vertex_u = S%pop()
      call G%color_vertex(vertex_u, GRAY)

      predecessor = G%get_predecessor(vertex_u)
      print *, vertex_u

      ! get the next vertex to traverse down
      vertex_w = process_dfs_neighbors(G, vertex_u)
      if (vertex_w .ne. -1) then
        call G%color_vertex(vertex_w, GRAY)
        call G%set_predecessor(vertex_w, vertex_u)
        call S%push(vertex_w)
      else
        !  else colour u black
        call G%color_vertex(vertex_u, BLACK)
      end if
    enddo
  end subroutine dfs
end module search_routines
