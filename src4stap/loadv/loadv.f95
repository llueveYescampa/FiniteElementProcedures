subroutine loadv (r,neq)
include 'common.h'
  real (kind=dbl) :: r(neq)
  integer         :: neq

  ! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
  ! .                                                                   .
  ! .   p r o g r a m                                                   .
  ! .        to obtain the load vector                                  .
  ! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

  include 'tapes.h'

  read (iload) r
  return
end
