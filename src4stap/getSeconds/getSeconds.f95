subroutine getSeconds (tim)
include 'common.h'
  real (kind=sgl), intent(inout) :: tim

  call cpu_time(tim)
  return
end ! end of subroutine getSeconds() !
