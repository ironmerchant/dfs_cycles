program myprog
  use class_Graph
  use class_Stack
  use utility
  use search_routines

  type(Graph) :: g
  integer :: num_vertices = 30, i, num_edges, percent_edges = 50
  integer, allocatable :: edges(:,:)

  print *, "Hello main@"
  num_edges = int(percent_edges * (num_vertices**2 - num_vertices) / 200)
  allocate(edges(num_edges, 2))

  edges = reshape( [(random_int(1, num_vertices), random_int(1, num_vertices), i = 1, num_edges)], [num_edges, 2])

  call g%init(num_vertices, edges, num_edges)
  call dfs(g)

end program myprog
