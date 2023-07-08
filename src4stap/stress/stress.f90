subroutine stress (u, numeg, npar)
include 'common.h'
  integer, intent(in)         :: numeg
  integer, intent(inout)      :: npar(*)
  real (kind=dbl), intent(in) :: u(*)
  ! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
  ! .                                                                   .
  ! .   p r o g r a m                                                   .
  ! .         to call the element subroutine for the calculation of     .
  ! .         stresses                                                  .
  ! .                                                                   .
  ! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

  include 'tapes.h'

!     loop over all element groups

  integer                            :: ng, numest
  integer, dimension(:), allocatable :: itemp

  rewind ielmnt
  do ng=1,numeg
    read (ielmnt) numest
    allocate ( itemp(numest) )
    backspace(ielmnt)
    read (ielmnt) numest, npar(1:10), itemp(1:numest)
    call elemntStress(itemp, u, ng, npar)
    deallocate (itemp)
  enddo

  return
end ! end of subroutine stress() !
