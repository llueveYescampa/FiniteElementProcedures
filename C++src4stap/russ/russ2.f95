subroutine russ2 (e,area,lm,xyz,matp, maxa, rkk, npar)
include 'common.h'
  real (kind=dbl), intent(in) :: e(*),area(*),xyz(6,*), rkk(*)
  integer, intent(in)         :: lm(6,*) ,matp(*), maxa(*), npar(*)
  ! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
  ! .                                                                           .
  ! .      a s s e m b l e  s t u c t u r e  s t i f f n e s s  m a t r i x     .
  ! .                           for truss element                               .
  ! .                                                                           .
  ! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .........
  real (kind=dbl) :: s(21),st(6),d(3)
  real (kind=dbl) :: xl,xl2,xx,yy
  integer         :: k,kl,l,mtype,n,nd

  nd=6
  do n=1,npar(2)
    mtype=matp(n)
    xl2=0.
    do l=1,3
      d(l)=xyz(l,n) - xyz(l+3,n)
      xl2=xl2 + d(l)*d(l)
    enddo
    xl=sqrt(xl2)
    xx=e(mtype)*area(mtype)*xl
    do l=1,3
      st(l)=d(l)/xl2
      st(l+3)=-st(l)
    enddo
    kl=0
    do l=1,6
      yy=st(l)*xx
      do k=l,6
        kl=kl + 1
        s(kl)=st(k)*yy
      enddo
    enddo
    call addban (rkk,maxa,s,lm(1,n),nd)
  enddo
  return
end ! end of subroutine russ2() !
