subroutine addres (maxa,mht, neq,nwk, mk)
include 'common.h'
  integer, intent(in)    :: mht(*),neq
  integer, intent(inout) :: maxa(*), mk, nwk
  ! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
  ! .                                                                   .
  ! .   p r o g r a m                                                   .
  ! .        to calculate addresses of diagonal elements in banded      .
  ! .        matrix whose column heights are known                      .
  ! .                                                                   .
  ! .        mht  = active column heights                               .
  !.         maxa = addresses of diagonal elements                      .
  ! .                                                                   .
  ! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

  integer :: i,neqp1

! clear array maxa

  neqp1=neq + 1
  maxa(1:neqp1)=0

  maxa(1)=1
  maxa(2)=2
  mk=0
  if (neq /=  1) then
    do i=2,neq
      if (mht(i) >  mk) mk=mht(i)
      maxa(i+1)=maxa(i) + mht(i) + 1
    enddo
  endif

  mk = mk + 1
  nwk=maxa(neqp1) - maxa(1)
  return
end ! end of subroutine addres() !
