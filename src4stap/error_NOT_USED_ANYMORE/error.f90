subroutine error (n,i)
  implicit none
  integer n,i
! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
! .                                                                   .
! .   p r o g r a m                                                   .
! .        to print messages when high-speed storage is exceeded      .
! .                                                                   .
! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
  include 'tapes.h'
  errorMessages: select case (i)
  case(1)
    write (iout,"(//,' not enough storage for id array and nodal point coordinates')")
  case(2)
    write (iout,"(//,' not enough storage for definition of load vectors')")
  case(3)
    write (iout,"(//,' not enough storage for element data input')")
  case(4)
    write (iout,"(//,' not enough storage for assemblage of global structure stiffness,')")
    write (iout,"(' and displacement and stress solution phase')")
  end select errorMessages

  write (iout,"(//,' *** error ***  storage exceeded by ', i9)") n
  stop
end

