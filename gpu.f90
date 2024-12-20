subroutine mgpu(NDIM,AA,CC)
  use iso_c_binding
  use mywrapper
  use gpuutil
  use cublas
  use cudafor
  use omp_lib
  implicit none


  double precision, dimension(NDIM,NDIM) :: aa
  double precision, dimension(NDIM,NDIM) :: cc

  double precision, dimension(:,:), allocatable, device :: aa_dev
  double precision, dimension(:,:), allocatable, device :: cc_dev


  double precision::wcStart,wcEnd
  integer(4)::get_walltime,istat
  external get_walltime

  integer :: status, NDIM, i
  double precision :: alpha, beta

!  print *,"In MGPU"

  alpha = 1d0
  beta = 0d0
  
!allocate device memory
allocate(aa_dev(ndim,ndim),cc_dev(ndim,ndim))

!aa_dev=transpose(aa)
!istat=get_walltime(wcStart)
aa_dev=aa
!istat=get_walltime(wcEnd)
!print '(1x,A22,ES10.3,A8)',"Elapsed time for copying:",wcEnd-wcStart,"seconds"
cc_dev=0.0d0


!print *, "Entering GPU Routine: Input A",aa
!print *, "Entering GPU Routine: Input C",cc
!print *, "Size of cc_dev: ",size(cc_dev)
!print *, "Alpha, Beta: ",alpha,beta
!!$omp target enter data map(to:aa,bb,cc)
!!$omp target data use_device_ptr(aa,bb,cc)
istat=get_walltime(wcStart)
       call cublasDgemm('N', 'N',  &
       NDIM, NDIM,NDIM, alpha, aa_dev, &
       NDIM, bb_dev, NDIM, beta,cc_dev, NDIM);
       cublas_error = cudaDeviceSynchronize()
istat=get_walltime(wcEnd)
!print *, "cuda synchronized", cublas_error
!print *,"Done calling GPU's DGEMM"
!print '(1x,A22,ES10.3,A8)',"Elapsed time on GPU:",wcEnd-wcStart,"seconds"
print '(1x,ES10.3)',wcEnd-wcStart

!!$omp end target data
    cc=cc_dev
!   cc=transpose(cc)
!   print *, "cublasdgemm call finished", cublas_error, cc
   if(cublas_error .ne. CUBLAS_STATUS_SUCCESS ) then
      print *, "failed", cublas_error, cc(1,1)
      call exit(1)
   endif

! wait for call to finish
!!$omp target exit data map(from:cc)
end subroutine mgpu

