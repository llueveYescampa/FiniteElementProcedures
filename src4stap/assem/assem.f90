subroutine assem (k, maxa, numeg, npar )
  implicit none
  double precision :: k(*)
  integer :: maxa(*), numeg, npar(*)
  ! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
  ! .                                                                   .
  ! .   p r o g r a m                                                   .
  ! .        to call element subroutines for assemblage of the          .
  ! .        structure stiffness matrix                                 .
  ! .                                                                   .
  ! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
  include 'tapes.h'

  integer :: n, numest
  integer, dimension(:), allocatable :: temp
   
  rewind ielmnt
  do n=1,numeg
    read (ielmnt) numest
    allocate ( temp(numest) )
    backspace(ielmnt)
    read (ielmnt) numest, npar(1:10), temp(1:numest)
    !read (ielmnt) numest,npar,(inx(i),i=1,numest)
    call elemntAssem(temp, maxa, k, npar)
    deallocate (temp)
  enddo
  
  return
end
