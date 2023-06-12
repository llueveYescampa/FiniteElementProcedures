subroutine elemntStress(temp, u, ng, npar)
include 'common.h'
  integer ::  temp(*), ng, npar(*)
  real (kind=dbl) :: u(*)

  ! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
  ! .                                                                   .
  ! .   p r o g r a m                                                   .
  ! .        to call the appropriate element subroutine                 .
  ! .                                                                   .
  ! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

  elementSelection: select case (npar(1))
  case (1)
    call trussStress(temp,u, ng , npar)
 !     other element types would be called here, identifying each
 !     element type by a different npar(1) parameter
 !case(2)
 !case(3)
  end select elementSelection
  return
end