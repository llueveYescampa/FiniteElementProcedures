subroutine loads (r,id,nload,neq, modex)
include 'common.h'
  integer         :: neq,nload, id(3,*), modex
  real (kind=dbl) :: r(neq)

  ! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
  ! .                                                                   .
  ! .   p r o g r a m                                                   .
  ! .      . to read nodal load data                                    .
  ! .      . to calculate the load vector r for each load case and      .
  ! .        write onto unit iload                                      .
  ! .                                                                   .
  ! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

  include 'tapes.h'

  integer :: i,ii,l,li,ln
  real (kind=dbl), dimension(:), allocatable ::fload
  integer, dimension(:),         allocatable :: nod
  integer, dimension(:),         allocatable :: idirn

  allocate ( nod(nload) )
  allocate ( idirn(nload) )
  allocate ( fload(nload) )

  write (iout,"(//,'    node       direction      load',/, '   number',19x,'magnitude')")
  read (iin,"(2i5,f10.0)") (nod(i),idirn(i),fload(i),i=1,nload)
  write (iout,"(' ',i6,9x,i4,7x,e12.5)") (nod(i),idirn(i),fload(i),i=1,nload)

  if (modex /= 0) then
    do i=1,neq
      r(i)=0.0
    enddo
    do l=1,nload
      ln=nod(l)
      li=idirn(l)
      ii=id(li,ln)
      if (ii > 0) then
        r(ii)=r(ii) + fload(l)
      endif
    enddo
    write (iload) r
  endif

  deallocate (nod)
  deallocate (idirn)
  deallocate (fload)
  return
end
