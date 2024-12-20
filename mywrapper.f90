module mywrapper
  use iso_c_binding
  use cublas
  use cudafor
  use omp_lib

  implicit none
  double precision, dimension(:,:), allocatable, device :: bb_dev

  contains 
  subroutine copytrans(B,N)
  implicit none
  double precision,dimension(n,n)::B
  integer n
  allocate (bb_dev(n,n))
!  bb_dev=transpose(b)
  bb_dev=b
  end subroutine copytrans

  subroutine inittrans(N)
  implicit none
  integer N
  allocate (bb_dev(n,n))
  end subroutine inittrans

  attributes(global) subroutine calctrans(N)
  implicit none
  integer, value ::  N
  integer i,j
  do i=1,n
   do j=1,n
    bb_dev(i,j)=((i-1)*2+j)*2.0d0
   enddo
  enddo
  end subroutine calctrans

  subroutine outtrans(B,N)
  implicit none
  double precision,dimension(n,n)::B
  integer n
  print '(1X,A15,I8)',"Size of BB: ",size(B)
  print '(1X,A15,I8)',"Size of BB_dev: ",size(bb_dev)
  B=bb_dev
  end subroutine outtrans

end module mywrapper
