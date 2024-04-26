module P1
  implicit none
  private

  public :: say_hello
contains
  subroutine say_hello
    print *, "Hello, P1!"
  end subroutine say_hello
end module P1
