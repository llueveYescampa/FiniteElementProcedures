function getSeconds ()
include 'common.h'
  real (kind=sgl) getSeconds

  call cpu_time(getSeconds)
  return
end ! end of function getSeconds () !
