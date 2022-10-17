subroutine writed (disp,id,numnp)
  implicit none
  integer :: id(3,*), numnp
  double precision :: disp(*)
  ! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
  ! .                                                                   .
  ! .   p r o g r a m                                                   .
  ! .      to print displacements                                       .
  ! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

  include 'tapes.h'
  
  double precision :: d(3)
  integer :: i,ii,ic,il,kk

  ! print displacements

  write (iout,'(///," d i s p l a c e m e n t s",//,"  node ",10x,"x-displacement    y-displacement    z-displacement")')
  ic=4

  do ii=1,numnp
    ic=ic + 1
    if (ic >=  56) then
      write (iout,'(///," d i s p l a c e m e n t s",//,"  node ",10x,"x-displacement    y-displacement    z-displacement")')
      ic=4
    endif

    do i=1,3
      d(i)=0.
    enddo

    do i=1,3
      kk=id(i,ii)
      il=i
      if (kk /=  0) d(il)=disp(kk)
    enddo

    write (iout,'(1x,i3,8x,3e18.6)') ii,d
  enddo
  return
end

