subroutine russ1 (mht,id,x,y,z,e,area,lm,xyz,matp, npar)
include 'common.h'
  real (kind=dbl) ::  x(*),y(*),z(*),e(*),area(*),xyz(6,*)
  integer         :: id(3,*),lm(6,*),mht(*) ,matp(*), npar(*)
  ! . . .. . . . . . . . . . . . . . . . . . . . .
  ! .                                            .
  ! .    read and generate element information   .
  ! .            for truss element               .
  ! .                                            .
  ! . .  . . . . . . . . . . . . . . . . . . . . .

  include 'tapes.h'

  integer :: i,ii,j,jj,kg,l,m,mtyp,mtype,n,nd,kkk

  include 'format4Russ1.h'

  nd=6
  kkk=0

  write (iout,2000) npar(1),npar(2)
  if (npar(3).eq.0) npar(3)=1
  write (iout,2010) npar(3)
  write (iout,2020)
  do i=1,npar(3)
    read (iin,"(i5,2f10.0)") n,e(n),area(n)
    write (iout,"(/,i5,4x,e12.5,2x,e14.6)") n,e(n),area(n)
  enddo
  !  read element information

  write (iout,2040)
  n=1
  m=0
  do
    if (n > m) then
      read (iin,"(5i5)") m,ii,jj,mtyp,kg
      if (kg == 0) kg=1
    endif
    if (m == n) then
      i=ii
      j=jj
      mtype=mtyp
      kkk=kg
    endif
    !  save element information

    xyz(1,n)=x(i)
    xyz(2,n)=y(i)
    xyz(3,n)=z(i)

    xyz(4,n)=x(j)
    xyz(5,n)=y(j)
    xyz(6,n)=z(j)

    matp(n)=mtype

    do  l=1,6
      lm(l,n)=0
    enddo

    do l=1,3
      lm(l,n)=id(l,i)
      lm(l+3,n)=id(l,j)
    enddo
    !  update column heights and bandwidth

    call colht (mht,nd,lm(1,n))
    write (iout,"(i5,6x,i5,4x,i5,7x,i5)") n,i,j,mtype
    if (n /= npar(2)) then
      n = n + 1
      i = i + kkk
      j = j + kkk
    else
      exit
    endif
  enddo
  return
end
