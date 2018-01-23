# HDF5 File Writer in OpenPMD Standard 
This program contains sample codes for parallelly writing 2D/3D mesh field data and particle data into HDF5 files using the [OpenPMD standard](https://github.com/openPMD/openPMD-standard). The output file can be visualized using [OpenPMD viewer](https://github.com/openPMD/openPMD-viewer).

# Compile the Code

The makefile is set to use gcc and gfortran with MPI. The HDF5-Parallel library (version 1.8.13) is also required for compiling the code. 

To compile the programs, execute:

make

The program name is io.e

The command to execute the program is:

mpirun -np nproc ./io.e

where nproc is the number of processors to be used.
