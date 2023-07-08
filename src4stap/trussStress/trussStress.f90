subroutine trussStress(itemp, u, ng, npar)
include 'common.h'
  integer, intent(in) :: itemp(*), ng, npar(*)
  real (kind=dbl), intent(in) :: u(*)
  ! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
  ! .                                                                   .
  ! .   p r o g r a m                                                   .
  ! .        to set up storage and call the truss element subroutine    .
  ! .                                                                   .
  ! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

  integer :: n102, n103, n104, n105, nlast
  include 'itwo.h'

  n102= 1    + npar(3)*itwo           ! e
  n103=n102  + npar(3)*itwo           ! area
  n104=n103  + 6*npar(2)              ! lm
  n105=n104  + 6*npar(2)*itwo         ! xyz
  nlast=n105 + npar(2)                ! matp

  call russ3 (itemp(1),itemp(n102),itemp(n103),itemp(n104),itemp(n105), u, ng, npar)

  return
end ! end of subroutine trussStress() !
