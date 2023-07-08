subroutine truss(mht, id,x,y,z, npar)
include 'common.h'
  integer, intent(in)         :: id(3,*), mht(*), npar(*)
  real (kind=dbl), intent(in) :: x(*),y(*),z(*)
  ! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
  ! .                                                                   .
  ! .   p r o g r a m                                                   .
  ! .        to set up storage and call the truss element subroutine    .
  ! .                                                                   .
  ! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

  include 'tapes.h'
  integer :: numIntegers

  real (kind=dbl), dimension(:), allocatable :: e
  real (kind=dbl), dimension(:), allocatable :: area
  real (kind=dbl), dimension(:,:), allocatable :: xyz
  integer, dimension(:,:), allocatable :: lm
  integer, dimension(:), allocatable :: matp
  include 'itwo.h'

  allocate ( e(npar(3)) )
  allocate ( area(npar(3)) )
  allocate ( lm(6,npar(2)) )
  allocate ( xyz(6,npar(2)) )
  allocate ( matp(npar(2)) )


  !                  e()         area()          lm           xyz            matp
  numIntegers = npar(3)*itwo + npar(3)*itwo +  6*npar(2) + 6*npar(2)*itwo + npar(2)

  call russ1 (mht,id,x,y,z,e, area, lm, xyz, matp, npar)
  write (ielmnt) numIntegers,npar(1:10), e(1:npar(3)), area(1:npar(3)), lm(1:6, 1:npar(2)), xyz(1:6, 1:npar(2)), matp(1:npar(2))

  deallocate (matp)
  deallocate (xyz)
  deallocate (lm)
  deallocate (area)
  deallocate (e)

  return
end ! end of subroutine truss() !
