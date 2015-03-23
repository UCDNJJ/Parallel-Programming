R_HOME := $(shell R RHOME)

TARGETS := paralleldice.so

export R_LIBS_USER=/usr/lib64/R/
export PKG_LIBS='-lgomp'
export PKG_CXXFLAGS= -fopenmp -lgomp -DTHRUST_DEVICE_SYSTEM=THRUST_DEVICE_SYSTEM_OMP -I/usr/local/cuda-5.5/targets/x86_64-linux/include

all: $(TARGETS) 

$(TARGETS): thrustdice.cpp ompdice.cpp
	R CMD SHLIB thrustdice.cpp
	R CMD SHLIB ompdice.cpp

clean:
	rm -rf *o

.PHONY: all clean
