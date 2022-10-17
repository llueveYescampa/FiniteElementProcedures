subroutine elcal(mht, npar, id,x,y,z, numeg )
  implicit none
  integer          :: mht(*), id(3,*), numeg, npar(*)
  double precision :: x(*),y(*),z(*)

  ! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
  ! .                                                                   .
  ! .   p r o g r a m                                                   .
  ! .        to loop over all element groups for reading,               .
  ! .        generating and storing the element data                    .
  ! .                                                                   .
  ! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

  include 'tapes.h'

  integer :: n

  rewind ielmnt
  write (iout,'(//," e l e m e n t   g r o u p   d a t a",//)')

! loop over all element groups

  do n=1,numeg
    if (n /= 1) write (iout,'(" ")')
    read (iin, '(10i5)' ) npar(1:10)
    call elemnt(mht, id,x,y,z, npar)
  enddo
  return
end
