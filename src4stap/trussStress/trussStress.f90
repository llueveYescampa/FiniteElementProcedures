subroutine trussStress(temp, u, ng, npar)
  implicit none
  integer :: temp(*), ng, npar(*)
  double precision :: u(*)
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

  call russ3 (temp(1),temp(n102),temp(n103),temp(n104),temp(n105), u, ng, npar)

  return
end
