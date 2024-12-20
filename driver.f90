program main
  use mywrapper
  use gpuutil
  implicit none

  integer, parameter :: MAXDIM = 8192
  integer            :: NDIM 
  double precision,  dimension(MAXDIM,MAXDIM) :: aa
  double precision,  dimension(MAXDIM,MAXDIM) :: bb
  double precision,  dimension(MAXDIM,MAXDIM) :: cc_gpu
  double precision,  dimension(MAXDIM,MAXDIM) :: cc_host

  integer :: status, i, j, error, istat
  double precision  :: alpha, beta
  double precision::wcStart,wcEnd
  integer(4)::get_walltime
  external get_walltime

  error = 0
  alpha = 1d0
  beta = 0d0

call getproperties()


!  print '(1x,A10,I8)',"NDIM*NDIM= ",NDIM*NDIM
  write(*,fmt='(1x,A40)') "NDIM | CPU time (sec) | GPU time  (sec)"
do NDIM=8,MAXDIM,8 
  write(*,fmt='(1x,I8)',advance="no") NDIM

aa=0.0D0
cc_host=0.0D0
cc_gpu = 0.0d0
  do i=1,NDIM
   aa(i,i)=1.0D0
   do j=1,NDIM
     bb(i,j) = ((i-1)*2+j)*2.0d0
  enddo
 enddo

!call copytrans(bb,ndim)
!print *,"calculating on host"
!print *,"Calling host's DGEMM"
istat=get_walltime(wcStart)
call dgemm('N','N',NDIM,NDIM,NDIM,1.0d0,AA,NDIM,BB,NDIM,0.0d0,CC_HOST,NDIM)
istat=get_walltime(wcEnd)
!print *,"Done calling host's DGEMM"
!print '(1x,A22,ES10.3,A8)',"Elapsed time on CPU:",wcEnd-wcStart,"seconds"
write(*,fmt='(1x,ES10.3)',advance="no") wcEnd-wcStart
!if ( NDIM.lt.5) then
!print '(1X,A10)',"Input A: "
!print '(1X,4F8.2)',AA
!print '(1X,A10)',"Input B: "
!print '(1X,4F8.2)',BB
!print '(1X,A10)',"Result C: "
!print '(1X,4F8.2)',cc_host
!endif
!print *
!print *,"Getting GPU propierties"
call inittrans(NDIM)
call calctrans<<<(ndim-1)/512+1,512>>>(ndim)
!bb=0.0d0
!call outtrans(bb,ndim)
!if ( NDIM.lt.5) then
!        print '(1X,A10)',"GPU B: "
!        print '(1X,4F8.2)',BB
!endif
!print *,"Calling GPU's DGEMM"
!print '(1X,A15,3I8)',"Size of AA,BB,CC: ",SIZE(AA),SIZE(BB),SIZE(CC_GPU)
!istat=get_walltime(wcStart)
call mgpu(NDIM,AA,CC_GPU)

!call mgpu2(NDIM,AA,BB,CC_GPU)
!call mgpu3(NDIM,AA)

!istat=get_walltime(wcEnd)

!print *,"Done calling GPU's DGEMM"
!print '(1x,A12,ES10.3,A8)',"Elapsed time:",wcEnd-wcStart,"seconds"
!print '(1X,A10)',"Result: "
!if ( NDIM.lt.5) then
!        print '(1X,4F8.2)',cc_gpu
!endif

! get value from gpu

! error checking
  do i=1,NDIM
   do j=1,NDIM
     if( abs( cc_gpu(i,j) - cc_host(i,j) ) > 0.00001 ) then
        error=error+1
     endif
  enddo
 enddo
  if( error > 0 ) then
!     print *, "Failed! Error = ",error 
!     call exit(1)
  else
!     print *, "Everything's ok! ",error 

  endif
 
enddo

!  deallocate( aa, stat=status )
!  deallocate( bb, stat=status )
!  deallocate( cc_host, stat=status )
!  deallocate( cc_gpu, stat=status )

end program main

