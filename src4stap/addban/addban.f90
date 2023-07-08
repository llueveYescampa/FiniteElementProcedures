subroutine addban (a,maxa,s,lm,nd)
include 'common.h'
  real (kind=dbl), intent(in)    :: s(*)
  real (kind=dbl), intent(inout) :: a(*)
  integer, intent(in)            :: maxa(*),lm(*), nd
  ! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
  ! .                                                                   .
  ! .   p r o g r a m                                                   .
  ! .        to assemble upper triangular element stiffness into        .
  ! .        compacted global stiffness                                 .
  ! .                                                                   .
  ! .         a = global stiffness                                      .
  ! .         s = element stiffness                                     .
  ! .         nd = degrees of freedom in element stiffness              .
  ! .                                                                   .
  ! .                   s(1)        s(2)        s(3)        . . .       .
  ! .         s   =                 s(nd+1)     s(nd+2)     . . .       .
  ! .                                           s(2*nd)     . . .       .
  ! .                                                       . . .       .
  ! .                                                                   .
  ! .                                                                   .
  ! .                   a(1)        a(3)        a(6)        . . .       .
  ! .         a   =                 a(2)        a(5)        . . .       .
  ! .                                           a(4)        . . .       .
  ! .                                                       . . .       .
  ! .                                                                   .
  ! .                                                                   .
  ! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

  integer :: i,ii,ij,j,jj,kk,ks,kss,mi,ndi

  ndi=0
  do i=1,nd
    ii=lm(i)
    if (ii >  0) then
      mi=maxa(ii)
      ks=i
      do j=1,nd
        jj=lm(j)
        if (jj >  0) then
          ij=ii - jj
          if (ij >=  0) then
            kk=mi + ij
            kss=ks
            if (j >=  i) kss=j + ndi
            a(kk)=a(kk) + s(kss)
          endif
        endif
        ks=ks + nd - j
      enddo
    endif
    ndi=ndi + nd - i
  enddo
  return
end ! end of subroutine addban() !
