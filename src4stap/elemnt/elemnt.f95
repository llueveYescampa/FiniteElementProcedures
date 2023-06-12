subroutine elemnt(mht, id,x,y,z, npar)
include 'common.h'
  integer         :: id(3,*), mht(*), npar(*)
  real (kind=dbl) :: x(*),y(*),z(*)

  ! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
  ! .                                                                   .
  ! .   p r o g r a m                                                   .
  ! .        to call the appropriate element subroutine                 .
  ! .                                                                   .
  ! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

  elementSelection: select case (npar(1))
  case (1)
    call truss(mht,id,x,y,z, npar)
 !     other element types would be called here, identifying each
 !     element type by a different npar(1) parameter
 !case(2)
 !case(3)
  end select elementSelection
  return
end
