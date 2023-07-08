subroutine colsol (a,v,maxa,nn,kkk)
include 'common.h'
  real (kind=dbl), intent(inout) :: a(*),v(*)
  integer, intent(in)            :: maxa(*), nn,kkk
  ! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
  ! .                                                                   .
  ! .   p r o g r a m                                                   .
  ! .        to solve finite element static equilibrium equations in    .
  ! .        core, using compacted storage and column reduction scheme  .
  ! .                                                                   .
  ! .  - - input variables - -                                          .
  ! .        a(nwk)    = stiffness matrix stored in compacted form      .
  ! .        v(nn)     = right-hand-side load vector                    .
  ! .        maxa(nnm) = vector containing addresses of diagonal        .
  ! .                    elements of stiffness matrix in a              .
  ! .        nn        = number of equations                            .
  ! .        nwk       = number of elements below skyline of matrix     .
  ! .        nnm       = nn + 1                                         .
  ! .        kkk       = input flag                                     .
  ! .            eq. 1   triangularization of stiffness matrix          .
  ! .            eq. 2   reduction and back-substitution of load vector .
  ! .        iout      = unit used for output                           .
  ! .                                                                   .
  ! .  - - output - -                                                   .
  ! .        a(nwk)    = d and l - factors of stiffness matrix          .
  ! .        v(nn)     = displacement vector                            .
  ! .                                                                   .
  ! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

  include 'tapes.h'

  integer         :: ic,j,k,kh,ki,kk,kl,klt,kn,ku,l,n,nd
  real (kind=dbl) :: b,c

  include 'format4Colsol.h'

  !     perform l*d*l(t) factorization of stiffness matrix

  if ( (kkk-2) < 0) then ! 40,150,150
    do n=1,nn
      kn=maxa(n)
      kl=kn + 1
      ku=maxa(n+1) - 1
      kh=ku - kl

      if (kh > 0 )  then
        k=n - kh
        ic=0
        klt=ku
        do j=1,kh
          ic=ic + 1
          klt=klt - 1
          ki=maxa(k)
          nd=maxa(k+1) - ki - 1
          if (nd > 0) then
            kk=min0(ic,nd)
            c=0.
            do l=1,kk
              c = c + a(ki+l)*a(klt+l)
            enddo
            a(klt)=a(klt) - c
          endif
          k=k + 1
        enddo
      endif

      if (kh >=  0 )  then
        k=n
        b=0.
        do kk=kl,ku
          k=k - 1
          ki=maxa(k)
          c=a(kk)/a(ki)
          b=b + c*a(kk)
          a(kk)=c
        enddo
        a(kn)=a(kn) - b
      endif

      if (a(kn) <=  0) then ! 120,120,140
        write (iout,2000) n,a(kn)
        stop
      endif
    enddo
  else

  !     reduce right-hand-side load vector
    do n=1,nn
      kl=maxa(n) + 1
      ku=maxa(n+1) - 1
      if (ku-kl >= 0) then
        k=n
        c=0.
        do kk=kl,ku
          k=k - 1
          c = c + a(kk)*v(k)
        enddo
        v(n)=v(n) - c
      endif
    enddo
  !     back-substitute
    do n=1,nn
      k=maxa(n)
      v(n)=v(n)/a(k)
    enddo
    if (nn /= 1) then ! go to 900
      n=nn
      do l=2,nn
        kl=maxa(n) + 1
        ku=maxa(n+1) - 1
        if ( (ku-kl) >=  0 ) then
          k=n
          do  kk=kl,ku
            k=k - 1
            v(k)=v(k) - a(kk)*v(n)
          enddo
       endif
        n=n - 1
      enddo
    endif
  endif
  return
end ! end of subroutine colsol() !
