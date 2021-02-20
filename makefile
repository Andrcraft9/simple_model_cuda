
FC = mpifort

# Definitions and Options:
FCGCC = -fopenmp -cpp -dM -ffree-line-length-0
FCINTEL = -assume byterecl -openmp -fpp -allow nofpp_comments
# Debug: (from book Introduction to Programming with Fortran)
# ?: -fPIC -Warray-temporaries
FCGCC_DEBUG = -g -O -fcheck=all -finit-real=nan  -fbacktrace -pedantic-errors -Wunderflow -ffpe-trap=zero,overflow,underflow
FCINTEL_DEBUG = -g -O -check all -traceback
# Fast:
# ?: --ffast-math -auto -stack_temp
FCGCC_FAST = -Ofast
FCINTEL_FAST = -O3

# PGI OPTIONS
# -Mcuda=rdc
FC = /opt/nvidia/hpc_sdk/Linux_x86_64/2021/compilers/bin/nvfortran
FCGCC = -openmp -cpp -dM -cuda -gpu=cc60
FCGCC_DEBUG = -g -C -gopt -Mneginfo=all
FCGCC_FAST = -fast
PROF = /opt/nvidia/hpc_sdk/Linux_x86_64/2021/cuda/bin/nvprof

#FCD = $(FC) $(FCGCC) $(FCGCC_DEBUG)
FCD = $(FC) $(FCGCC) $(FCGCC_FAST)

#BASE = \
	base/kind.f90 \
	base/basinpar.f90 \
	base/parallel.f90 \
	base/errors.f90 \
	base/decomposition.f90 \
	base/data_types.f90 \
	base/kernel_interface.f90
BASE = \
	base/kind.f90 \
	base/basinpar.f90 \
	base/gpuparallel.f90 \
	base/errors.f90 \
	base/decomposition.f90 \
	base/data_types.f90 \
	base/kernel_interface.f90

DATA = \
	data/grid.f90 \
	data/ocean.f90

CONTROL = \
	control/init_data.f90

SOLVER = \
	kernels/solver.f90 \
	kernels/solver_gpu.cuf

clean:
	find . -name "*.o" -delete
	find . -name "*.mod" -delete
	find . -name "*.inst.f90" -delete
	find . -name "*.pomp.f90" -delete

compile:
	$(FCD) $(FCFLAGS) -o model $(BASE) $(DATA) $(CONTROL) $(SOLVER) model.f90

run_example:
	$(PROF) ./model 4096 4096 1 1 100