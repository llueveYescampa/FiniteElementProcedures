subroutine colht (mht,nd,lm)
use iso_fortran_env
include 'common.h'
  integer :: mht(*), nd, lm(*)
  ! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
  ! .                                                                   .
  ! .   p r o g r a m                                                   .
  ! .        to calculate column heights                                .
  ! .                                                                   .
  ! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

  integer :: ls,i,ii,me

  ls=huge(ls)  ! Largest integer

  do i=1,nd
    if (lm(i) /=  0) then
      if ( lm(i)-ls  <  0) then
        ls=lm(i)
      endif
    endif
  enddo

  do i=1,nd
    ii=lm(i)
    if (ii /=  0) then
      me = ii - ls
      if (me  >   mht(ii)) mht(ii) = me
    endif
  enddo

  return
end
