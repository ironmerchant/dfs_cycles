module class_Graph
  use class_Stack
  use utility

  integer :: WHITE = 0, GRAY = 1, BLACK = 2

  type Vertex
    integer :: value, predecessor, color
    real :: distance
    logical :: is_leaf
  end type Vertex

  type, public :: Graph
    type(Vertex), private, allocatable, dimension(:) :: vertices
    class(Stack), private, allocatable, dimension(:) :: adj_list

    contains

      procedure :: init => graph_init
      procedure :: color_vertex => graph_color_vertex
      procedure :: get_color => graph_get_color
      procedure :: get_adj_list => graph_get_adj_list
      procedure :: set_predecessor => graph_set_predecessor
      procedure :: get_predecessor => graph_get_predecessor
  end type

contains

  subroutine graph_init(self, num_vertices, edges, num_edges)
    use, intrinsic :: ieee_arithmetic

    ! define the parameters
    class(Graph), intent(inout) :: self
    integer, intent(in) :: num_edges
    integer, intent(in) :: num_vertices
    integer, intent(in) :: edges(:, :) ! a 2xV array

    ! define the data variables
    integer :: i
    real :: inf

    ! define infinity
    inf = ieee_value(inf, ieee_positive_inf)

    ! allocate the vertices and the adjacency list
    allocate(self%vertices(num_vertices))
    allocate(self%adj_list(num_vertices))

    ! initialized the vertices
    do i = 1, num_vertices
      self%vertices(i)%value = i
      self%vertices(i)%distance = inf
      self%vertices(i)%predecessor = 0
      self%vertices(i)%color = WHITE
      self%vertices(i)%is_leaf = .false.
    enddo

    ! initialize the adjacency lists
    do i = 1, num_vertices
      call self%adj_list(i)%init()
    enddo

    ! create the adjacency list
    do i = 1, num_edges
      call self%adj_list(edges(i, 1))%push(edges(i, 2))
    enddo

  end subroutine graph_init

  subroutine graph_color_vertex(self, vertex_index, color)
    Class(Graph), intent(inout) :: self
    integer, intent(in) :: vertex_index, color

    self%vertices(vertex_index)%color = color

  end subroutine graph_color_vertex

  function graph_get_color(self, vertex_index) result(out_color)
    Class(Graph), intent(inout) :: self
    integer, intent(in) :: vertex_index
    integer :: out_color

    out_color = self%vertices(vertex_index)%color

  end function

  function graph_get_adj_list(self, vertex_index) result(out_stackiterator)
    Class(Graph), intent(inout) :: self
    integer, intent(in) :: vertex_index
    type(StackIterator) :: out_stackiterator

    call out_stackiterator%create(self%adj_list(vertex_index))

  end function graph_get_adj_list

  subroutine graph_set_predecessor(self, vertex_index, vertex_parent_index)
    Class(Graph), intent(inout) :: self
    integer, intent(in) :: vertex_index, vertex_parent_index

    self%vertices(vertex_index)%predecessor = vertex_parent_index

  end subroutine graph_set_predecessor

  function graph_get_predecessor(self, vertex_index) result(out_vertex)
    Class(Graph), intent(inout) :: self
    integer, intent(in) :: vertex_index
    integer :: out_vertex

    out_vertex = self%vertices(vertex_index)%predecessor

  end function graph_get_predecessor

end module class_Graph
