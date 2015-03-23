export R_LIBS_USER=/home/ricardo/R/x86_64-pc-linux-gnu-library/3.0/
export PKG_LIBS="-lgomp"
export PKG_CXXFLAGS="-fopenmp -I/home/ricardo/R/x86_64-pc-linux-gnu-library/3.0/Rcpp/include"
R CMD SHLIB Problem2.cpp
cp Problem2.so ~
