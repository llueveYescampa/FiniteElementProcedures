program stap
Use, intrinsic :: iso_fortran_env, Only : iostat_end
include 'common.h'
  integer           :: argc
  character(len=32) :: argv

  ! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
  ! .                                                                   .
  ! .                              s t a p                              .
  ! .                                                                   .
  ! .            an in-core solution static analysis program            .
  ! .                                                                   .
  ! . . . . . . . . . . . . . .  . . .  . . . . . . . . . . . . . . . . .
  include 'tapes.h'
  !
      integer :: ioerr
      integer :: i,ktr,l,ll,mm,neq1,nlcase,nload,nnl, modex
      integer ::  numnp,neq,nwk,mk, numeg, npar(10)
      real (kind=sgl) :: tim(5), hed(20), tt

  !!!!!!!!!!!!!!!!!!!! Dynamically allocated variables !!!!!!!!!!!!!!!!!!!
      integer,         dimension(:,:), allocatable :: id
      integer,         dimension(:),   allocatable :: mht
      integer,         dimension(:),   allocatable :: maxa
      real (kind=dbl), dimension(:),   allocatable :: x
      real (kind=dbl), dimension(:),   allocatable :: y
      real (kind=dbl), dimension(:),   allocatable :: z
      real (kind=dbl), dimension(:),   allocatable ::r
      real (kind=dbl), dimension(:),   allocatable ::k
      real (kind=dbl), dimension(:),   allocatable ::u
  !!!!!!!!!!!!!!!!!!!! Dynamically allocated variables !!!!!!!!!!!!!!!!!!!

  ! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
  ! .   the following two lines are used to determine the maximum high  .
  ! .   speed storage that can be used for solution. to change the high .
  ! .   speed storage available for execution, change the value of mtot .
  ! .   and correspondingly common a(mtot).                             .
  ! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

  !
  !     the following scratch files are used
  !        ielmnt = unit storing element data
  !        iload  = unit storing load vectors
  !        iin    = unit used for input
  !        iout   = unit used for output
  !
  !     on some machines these files must be explicitly opened
  !
  include 'format4Stab.h'
  argc = command_argument_count()
  if (argc == 0) then
    call get_command_argument(0, argv)
    print*, 'Use: ',  argv, 'filename'
    stop
  else
    call get_command_argument(1, argv)
    iin = 3
    open(iin, file = argv, status = 'old', iostat=ioerr)
    if (ioerr > 0) then
      print*, 'the ' ,  argv, 'input file was not found'
      print*, 'Bye ...'
      stop
    endif
  endif
  iout = 6

  ielmnt = 1
  iload = 2
  open(ielmnt, file = 'elementData.dat', status = 'unknown', form='unformatted')
  open(iload,  file = 'loadData2.dat'  , status = 'unknown', form='unformatted')


  do  ! while
    !     * * * * * * * * * * * * * * * * * * * * * *
    !     * * *   i n p u t   p h a s e   * * *
    !     * * * * * * * * * * * * * * * * * * * * * *
    call getSeconds (tim(1))


    !     r e a d   c o n t r o l   i n f o r m a t i o n


    read (iin,"(20a4,/,4i5)", iostat = ioerr) hed,numnp,numeg,nlcase,modex
    if (  (numnp == 0) .or. ioerr == iostat_end  ) exit
    write (iout,2000) hed,numnp,numeg,nlcase,modex

    !!!!!!!!!!!!!!!!!!!! Dynamically allocating variables !!!!!!!!!!!!!!!!!!!
    allocate ( id(3,numnp) )
    allocate ( x(numnp) )
    allocate ( y(numnp) )
    allocate ( z(numnp) )
    !!!!!!!!!!!!!!!!!!!! Dynamically allocating variables !!!!!!!!!!!!!!!!!!!

    !     r e a d   n o d a l   p o i n t   d a t a

    call input (id,x,y,z,numnp,neq)
    neq1=neq + 1

    !     c a l c u l a t e   a n d   s t o r e   l o a d   v e c t o r s


    !!!!!!!!!!!!!!!!!!!! Dynamically allocating variables !!!!!!!!!!!!!!!!!!!
    allocate ( r(neq) )
    !!!!!!!!!!!!!!!!!!!! Dynamically allocating variables !!!!!!!!!!!!!!!!!!!
    write (iout,"(//,' l o a d   c a s e   d a t a')")
    rewind iload
    do l=1,nlcase
      read (iin,"(2i5)") ll,nload
      write (iout,2010) ll,nload
      if (ll /= l) then
        write (iout,"(' *** error *** load cases are not in order')")
        stop
      endif
      call loads (r,id,nload,neq, modex)
    enddo

    !!!!!!!!!!!!!!! Deallocate dnamically allocated variables !!!!!!!!!!!!!!
    deallocate (r)
    !!!!!!!!!!!!!!! Deallocate dnamically allocated variables !!!!!!!!!!!!!!

    !     r e a d , g e n e r a t e   a n d   s t o r e
    !     e l e m e n t   d a t a
    !     clear storage

    !!!!!!!!!!!!!!!!!!!! Dynamically allocating variables !!!!!!!!!!!!!!!!!!!
    allocate ( mht(neq) )
    do i=1,neq
      mht(i) = 0
    enddo
    !!!!!!!!!!!!!!!!!!!! Dynamically allocating variables !!!!!!!!!!!!!!!!!!!

    call elcal(mht,npar, id,x,y,z, numeg)
    !!!!!!!!!!!!!!! Deallocate dnamically allocated variables !!!!!!!!!!!!!!
    deallocate (x)
    deallocate (y)
    deallocate (z)
    !!!!!!!!!!!!!!! Deallocate dnamically allocated variables !!!!!!!!!!!!!!


    call getSeconds (tim(2))
    ! * * * * * * * * * * * * * * * * * * * * * *
    ! * * *   s o l u t i o n   p h a s e   * * *
    ! * * * * * * * * * * * * * * * * * * * * * *

    !  a s s e m b l e   s t i f f n e s s   m a t r i x
    allocate ( maxa(neq+1) )

    call addres (maxa,mht, neq,nwk, mk)
    !!!!!!!!!!!!!!! Deallocate dnamically allocated variables !!!!!!!!!!!!!!
    deallocate (mht)
    !!!!!!!!!!!!!!! Deallocate dnamically allocated variables !!!!!!!!!!!!!!

    mm=nwk/neq

    allocate ( k(nwk) )
    allocate ( u(neq) )

    ! write total system data

    write (iout,2025) neq,nwk,mk,mm

    !     in data check only mode we skip all further calculations

    if (modex > 0) then ! go to 100
      ! clear storage
      nnl=nwk + neq
      call clear (k,nwk)
      call clear (u,neq)

      call assem (k, maxa, numeg, npar)

      call getSeconds (tim(3))
      ! t r i a n g u l a r i z e   s t i f f n e s s   m a t r i x
      ktr=1
      call colsol (k,u,maxa,neq,ktr)
      call getSeconds (tim(4))
      ktr=2
      rewind iload
      do l=1,nlcase
        call loadv (u,neq)
        !   c a l c u l a t i o n   o f   d i s p l a c e m e n t s
        call colsol (k,u,maxa,neq,ktr)
        write (iout,"(//,' load case ',i3)") l
        call writed (u,id,numnp)

        !   c a l c u l a t i o n   o f   s t r e s s e s
        call stress ( u, numeg, npar ) !   <<<<<<<<====================
      enddo
      call getSeconds (tim(5))
    else
      call getSeconds (tim(3))
      call getSeconds (tim(4))
      call getSeconds (tim(5))
    endif
    ! print solution times
    tt=0.
    do i=1,4
      tim(i)=tim(i+1) - tim(i)
      tt=tt + tim(i)
    enddo
    write (iout,2030) hed,(tim(i),i=1,4),tt
    ! read next analysis case
    !!!!!!!!!!!!!!! Deallocate dnamically allocated variables !!!!!!!!!!!!!!
    deallocate (u)
    deallocate (k)
    deallocate (maxa)
    deallocate (id)
    !!!!!!!!!!!!!!! Deallocate dnamically allocated variables !!!!!!!!!!!!!!
  enddo ! while
  close(ielmnt)
  close(iload)
  close(iin)
end ! end of stap Program !
