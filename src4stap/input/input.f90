subroutine input (id,x,y,z,numnp,neq)
  implicit none
  double precision :: x(*),y(*),z(*)
  integer :: id(3,*),numnp,neq
  ! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
  ! .                                                                   .
  ! .   p r o g r a m                                                   .
  ! .      .to read, generate, and print nodal point input data         .
  ! .      .to calculate equation numbers and store them in id arrray   .
  ! .                                                                   .
  ! .           n=element number                                        .
  ! .           id=boundary condition codes (0=free,1=deleted)          .
  ! .           x,y,z= coordinates                                      .
  ! .           kn= generation code                                     .
  ! .                    i.e. increment on nodal point number           .
  ! .                                                                   .
  ! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

  ! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
  ! .   the program stap is used in single precision arithmetic on cray .
  ! .   equipment and double precision arithmetic on ibm machines,      .
  ! .   engineering workstations and pcs. deactivate above line (also   .
  ! .   occurring in other subroutines) for single precision arithmetic..
  ! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
  include 'tapes.h'

  double precision :: dx,dy,dz,xnum
  integer :: j,k,kk,kn,i,knold,n,nold,num,numn

  include 'format4Input.h'
  
  ! read and generate nodal point data
  write (iout,"(//,' n o d a l   p o i n t   d a t a',/)")
  write (iout,"(' input nodal data',//)")
  write (iout,2020)
  knold=0
  nold=0

  do
    read (iin,"(4i5,3f10.0,i5)") n,id(1:3,n),x(n),y(n),z(n),kn
    write (iout,"(i5,6x,3i5,6x,3f13.3,3x,i6)") n,id(1:3,n),x(n),y(n),z(n),kn
    if (knold /=   0) then
      num=(n-nold)/knold
      numn=num - 1
      if (numn.ge.1) then
        xnum=num
        dx=(x(n)-x(nold))/xnum
        dy=(y(n)-y(nold))/xnum
        dz=(z(n)-z(nold))/xnum
        k=nold
        do j=1,numn
          kk=k
          k=k + knold
          x(k)=x(kk) + dx
          y(k)=y(kk) + dy
          z(k)=z(kk) + dz
          !do i=1,3
            id(1:3,k)=id(1:3,kk)
          !enddo
        enddo
      endif
    endif
    nold=n
    knold=kn
    if (n == numnp) exit
  end do
  ! write complete nodal data
  write (iout,"(//,' generated nodal data',//)")
  write (iout,2020)
  do n=1,numnp
    write (iout,"(i5,6x,3i5,6x,3f13.3,3x,i6)") n,id(1:3,n),x(n),y(n),z(n),kn
  enddo
  !number unknowns
  neq=0
  do n=1,numnp
    do i=1,3
      if (id(i,n) ==  0) then ! 110,120,110
        neq=neq + 1
        id(i,n)=neq
      else ! go to 100
        id(i,n)=0
      endif
    enddo
  enddo
  ! write equation numbers
  write (iout,2040) (n,id(1:3,n),n=1,numnp)
  return
end
