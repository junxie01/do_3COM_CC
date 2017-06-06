FC=gfortran
FFLAG=-I/home/junxie/opt/fftw/include -L/home/junxie/opt/fftw/lib -lfftw3
FFLAG=-lfftw3 -fbounds-check
objects1=do_3COM_CC_1list.o sacio.o rot9com.o globe_data.o do_ncc.o
objects2=do_3COM_CC_2list.o sacio.o rot9com.o globe_data.o do_ncc.o
all:sacio.mod globe_data.mod ../bin/do_3COM_CC_1list ../bin/do_3COM_CC_2list
%.o:%.f90
	$(FC) $(FFLAG) $< -c 
sacio.mod:sacio.f90
	$(FC) $< -c
globe_data.mod:globe_data.f90
	$(FC) $< -c
../bin/do_3COM_CC_1list:$(objects1)
	$(FC) $^ -o $@ $(FFLAG) -lm
../bin/do_3COM_CC_2list:$(objects2)
	$(FC) $^ -o $@ $(FFLAG) -lm
clean:
	-rm *.o *.mod
