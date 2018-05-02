module class_StackIterator
  use class_Stack

  type, public :: StackIterator
    integer, private :: index
    type(link), pointer, private :: current
    contains
      procedure :: create => stack_iterator_create
      procedure :: next => stack_iterator_next
      procedure :: has_next => stack_iterator_has_next
      procedure :: get_index => stack_iterator_get_index
  end type StackIterator

contains

  subroutine stack_iterator_create(self, in_stack)
    Class(StackIterator), intent(inout) :: self
    Class(Stack), intent(inout) :: in_stack
    self%current => in_stack%get_head()
    self%index = 0
  end subroutine stack_iterator_create

  function stack_iterator_next(self) result(out_vertex_label)
    Class(StackIterator), intent(inout) :: self
    integer :: out_vertex_label
    out_vertex_label = self%current%i
    self%current => self%current%previous
    self%index = self%index + 1
  end function stack_iterator_next

  function stack_iterator_has_next(self) result(out_bool)
    Class(StackIterator), intent(inout) :: self
    logical :: out_bool
    out_bool = associated(self%current%previous)
  end function stack_iterator_has_next

  function stack_iterator_get_index(self) result(current_index)
     Class(StackIterator), intent(inout) :: self
     integer :: current_index
     current_index = self%index
  end function stack_iterator_get_index

end module class_StackIterator
