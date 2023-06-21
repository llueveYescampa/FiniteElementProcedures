subroutine loadv (r,neq)
include 'common.h'
  real (kind=dbl), intent(inout) :: r(neq)
  integer, intent(in)            :: neq

  ! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
  ! .                                                                   .
  ! .   p r o g r a m                                                   .
  ! .        to obtain the load vector                                  .
  ! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

  include 'tapes.h'

  read (iload) r
  return
end ! end of subroutine loadv() !
