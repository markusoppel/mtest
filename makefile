CC=nvcc
FC=nvfortran
MOD=mywrapper.mod gpuutil.mod
FFLAGS= -cuda -mp=gpu -cudalib=cublas -Mmkl -mcmodel=medium
LDFLAGS= 
matrix.x: gpu.o driver.o timing.o mywrapper.mod gpuutil.mod
	$(FC) $(FFLAGS)  $(LDFLAGS) driver.f90  gpu.f90 timing.o gpuutil.o mywrapper.o -o $@
timing.o: timing.c timing.h
	$(CC) -c timing.c
mywrapper.mod: mywrapper.f90
	$(FC) $(FFLAGS) -c $<
gpuutil.mod: gpuutil.f90
	$(FC) $(FFLAGS) -c $<
%.o: %.f90 $(MOD)
	$(FC) $(FFLAGS) -c  $<
clean:
	rm -rf *.o *.x *.mod
