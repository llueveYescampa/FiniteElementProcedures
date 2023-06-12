subroutine getSeconds (tim)
include 'common.h'
  real (kind=sgl) :: tim
  call cpu_time(tim)
  return
end
