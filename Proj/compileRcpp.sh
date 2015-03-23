export R_LIBS_USER=/usr/lib64/R/
export PKG_LIBS="-lgomp"
export PKG_CXXFLAGS="-fopenmp -I/home/ricardo/R/x86_64-pc-linux-gnu-library/3.0/Rcpp/include"
R CMD SHLIB dice.cpp
export PKG_CXXFLAGS="-fopenmp -I/home/ricardo/R/x86_64-pc-linux-gnu-library/3.0/Rcpp/include -lgomp -DTHRUST_DEVICE_SYSTEM=THRUST_DEVICE_SYSTEM_OMP -I /usr/local/cuda-5.5/targets/x86_64-linux/include"
R CMD SHLIB thrustdice.cpp
export PKG_CXXFLAGS="-fopenmp -I/home/ricardo/R/x86_64-pc-linux-gnu-library/3.0/Rcpp/include"
R CMD SHLIB ompdice.cpp
cp dice.so ~
cp thrustdice.so ~
cp ompdice.so ~
