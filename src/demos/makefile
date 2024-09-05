#####################################################################
# This makefile can compile CPTEC-MBUFRTOOLS IN DIFERENT COMPILERS  
#####################################################################
#   Selection of compiler: 

# F90=f90  -g -assume byterecl -V -show cod -O0      #makefile_dec
# F90=g95 -ffree-form -g0 -ftrace=full                 #makefile_g95 
# F90=ifort -assume byterecl -CB  -free -g -tpp1 -O0 #makefile_ifort
# F90=pgf90 -g                                       #makefile_pgf90
 F90=gfortran -ffree-form -g0                #makefile_gfortran  

##################################################################### 
# SOURCES FILES
#-----------------------------------
MBUFRF        = ../shared/mbufr-adt/mbufr.f90

#
#
# 
all: demo1.exe 

demo1.exe : demo1.f90 mbufr.o
	$(F90) -o $@ demo1.f90 mbufr.o
mbufr.o  : $(MBUFRF)
	$(F90) -c $(MBUFRF)

	
clean:
	\rm *.mod *.o *.exe *.l

