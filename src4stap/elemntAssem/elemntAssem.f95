subroutine elemntAssem(itemp, maxa, rk, npar)
include 'common.h'
  integer, intent(in) :: itemp(*), maxa(*), npar(*)
  real (kind=dbl), intent(in) :: rk(*)

  ! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
  ! .                                                                   .
  ! .   p r o g r a m                                                   .
  ! .        to call the appropriate element subroutine                 .
  ! .                                                                   .
  ! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

  elementSelection: select case (npar(1))
  case (1)
    call trussAssem(itemp,maxa, rk, npar)
 !     other element types would be called here, identifying each
 !     element type by a different npar(1) parameter
 !case(2)
 !case(3)
  end select elementSelection
  return
end ! end of subroutine elemntAssem() !
