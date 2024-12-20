module gpuutil
  use iso_c_binding
  use cublas
  use cudafor
  use omp_lib

  implicit none

  type(cublasHandle) :: handle
  integer(c_int)   :: cublas_error

  contains 
  subroutine getproperties()
  implicit none
  integer :: istat,nDevices
  integer :: i
  type (cudaDeviceProp) :: prop


  print *,"Getting device properties"
  istat = cudaGetDeviceCount(nDevices)
  do i = 0, nDevices-1
     istat = cudaGetDeviceProperties(prop, i)
     write(0,"(' Device Number: ',i0)") i
     write(0,"('   Device name: ',a)") trim(prop%name)
     write(0,"('   Memory Clock Rate (KHz): ', i0)") &
       prop%memoryClockRate
     write(0,"('   Memory Bus Width (bits): ', i0)") &
       prop%memoryBusWidth
     write(0,"('   Peak Memory Bandwidth (GB/s): ', f6.2)") &
       2.0*prop%memoryClockRate*(prop%memoryBusWidth/8)/10.0**6
     write(0,*)
  enddo
  cublas_error=cublasCreate(handle)

  end subroutine getproperties

end module gpuutil
