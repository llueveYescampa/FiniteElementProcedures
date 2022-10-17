subroutine elemntAssem(temp, maxa, k, npar)
  implicit none
  integer :: temp(*), maxa(*), npar(*)
  double precision :: k(*)
  
  ! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
  ! .                                                                   .
  ! .   p r o g r a m                                                   .
  ! .        to call the appropriate element subroutine                 .
  ! .                                                                   .
  ! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

  elementSelection: select case (npar(1))
  case (1)
    call trussAssem(temp,maxa, k, npar)
 !     other element types would be called here, identifying each
 !     element type by a different npar(1) parameter
 !case(2)
 !case(3) 
  end select elementSelection
  return
end
