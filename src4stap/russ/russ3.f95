subroutine russ3 (e,area,lm,xyz,matp, u, ng, npar)
include 'common.h'
  real (kind=dbl) :: e(*),area(*),xyz(6,*),u(*)
  integer         :: lm(6,*) ,matp(*), ng, npar(*)
! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
! .                                                                   .
! .              s t r e s s  c a l c u l a t i o n s                 .
! .                       for truss element                           .
! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
  include 'tapes.h'

  integer         :: i,iprint,j,l,mtype,n,nd
  real (kind=dbl) :: st(6),d(3), p,str,xl2

  include 'format4Russ3.h'
  nd=6

  iprint=0
  do n=1,npar(2)
    iprint=iprint + 1
    if (iprint > 50) iprint=1
    if (iprint == 1) write (iout,2060) ng
    mtype=matp(n)
    xl2=0.0
    do l=1,3
      d(l) = xyz(l,n) - xyz(l+3,n)
      xl2=xl2 + d(l)*d(l)
    enddo
    do l=1,3
      st(l)=(d(l)/xl2)*e(mtype)
      st(l+3)=-st(l)
    enddo
    str=0.0
    do l=1,3
      i=lm(l,n)
      if (i > 0) str=str + st(l)*u(i)
      j=lm(l+3,n)
      if (j >  0) str=str + st(l+3)*u(j)
    enddo
    p=str*area(mtype)
    write (iout,"(1x,i5,11x,e13.6,4x,e13.6)") n,p,str
  enddo
  return
end
