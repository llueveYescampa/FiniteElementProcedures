subroutine loadv (r,neq)
  implicit none
  double precision :: r(neq)
  integer :: neq

  ! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
  ! .                                                                   .
  ! .   p r o g r a m                                                   .
  ! .        to obtain the load vector                                  .
  ! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

  include 'tapes.h'
  
  read (iload) r
  return
end

