subroutine getSeconds (tim)
  implicit none
  real :: tim
  call cpu_time(tim)
  return
end
