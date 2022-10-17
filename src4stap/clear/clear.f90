subroutine clear (a,n)
  implicit none
  double precision :: a(*)
  integer :: n
  ! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
  ! .                                                                   .
  ! .   p r o g r a m                                                   .
  ! .        to clear array a                                           .
  ! .                                                                   .
  ! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

  a(1:n) = 0.0d0
  
  return
end

