# name of the compiler 
FC = mpif90 -fopenmp -O3
# CC = mpicc -fdefault-real-8
CC = mpicc
LINKER = mpif90 -fopenmp

# compiler options

FORMAT_FREE = -ffree-form
FORMAT_FIXED = -ffixed-form

# Objects list

OBJS_BASE = parallel_class.o hdf5io_class.o

OBJS_MAIN = main.o  

OBJS = ${OBJS_BASE} ${OBJS_MAIN}

# hdf libraries 
HDF_INCLUDE_PATH = -I/usr/local/include -I/usr/local/lib 
HDF_LIBPATH = -L/usr/local/lib -lhdf5 -lhdf5_fortran -lhdf5_hl -lhdf5hl_fortran
HDF_LIBS = ${HDF_LIBPATH} 

OPTS = ${HDF_INCLUDE_PATH}

# Linkage rule
main :: ${OBJS} 
	${LINKER}  ${OBJS} ${HDF_LIBS} -o io.e

clean ::
	rm *.o *.mod; rm io.e

# Compilation rules
hdf5io_class.o : hdf5io_class.f parallel_class.o
	${FC} ${OPTS} ${FORMAT_FREE} -c hdf5io_class.f -o hdf5io_class.o

parallel_class.o : parallel_class.f
	${FC} ${OPTS} ${FORMAT_FREE} -c parallel_class.f -o parallel_class.o

parallel_pipe_class.o : parallel_class.o parallel_pipe_class.f
	${FC} ${OPTS} ${FORMAT_FREE} -c parallel_pipe_class.f -o parallel_pipe_class.o


hdf5io_pipe_class.o : hdf5io_pipe_class.f parallel_pipe_class.o
	${FC} ${OPTS} ${FORMAT_FREE} -c hdf5io_class.f -o hdf5io_class.o

main.o : main.f parallel_class.o hdf5io_class.o parallel_pipe_class.o
	${FC} ${OPTS} ${FORMAT_FREE} -c main.f -o main.o
