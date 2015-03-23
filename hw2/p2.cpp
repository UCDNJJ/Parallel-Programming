//Computes Mandelbrot fractal matrix

//args

//xl: left limit
//xr: right limit
//yt: top limit
//yb: bottom limit
//inc: distance between ticks on X, Y axes
//maxiters: maximum number of iterations

//return value:

//We aim to return an R list which will be used as inputs to R's image() function
//: a matrix of 1s and 0s, 1 meaning sequence remains in bounds, absolute value
//<= 1, plus the tick marks vectors for the X and Y axes

#include <iostream>
#include <numeric>
#include <R.h>
#include <Rcpp.h>
#include <Rinternals.h>
#include <Rmath.h>
#include <omp.h>
#include <stdlib.h>
#include <string>
#include <vector>

using namespace Rcpp;

RcppExport SEXP rmandel(SEXP nth,SEXP xl,SEXP xr,SEXP yb,SEXP yt,SEXP inc,SEXP maxiters,SEXP sched,SEXP chunksize){
  
  int c_nth = as<int>(nth), c_maxiters = as<int>(maxiters), c_chunksize = as<int>(chunksize);
  double c_xl = as<double>(xl), c_xr = as<double>(xr), c_yb = as<double>(yb),
         c_yt = as<double>(yt), c_inc = as<double>(inc);
  std::string c_sched = as<std::string>(sched);
  
  std::vector<float>xticks[c_nth];
  std::vector<float>yticks[c_nth];
  
  NumericMatrix m[c_nth];
  NumericMatrix output;
  
  #pragma omp parallel num_threads(c_nth)
  {
      int me = omp_get_thread_num();
      std::cout << me << std::endl;
      
      //getmyxinterval <- function(i)
      float width = (c_xr - c_xl)/c_nth;      
      float myxl = c_xl + (me * width);
      float myxr = myxl + width;
      float myyb = c_yb;
      float myyt = c_yt;
     
      for(; myxl < myxr; myxl += c_inc)
          xticks[me].push_back(myxl);
      
      for(; myyb < myyt; myyb += c_inc)
          yticks[me].push_back(myyb);
      
      m[me](xticks[me].size(), yticks[me].size());
      
      float xti, ytj;
      
      //loops may need to go from 1 to x;
      //#pragma omp for schedule(sched, chunksize)
      for(unsigned int i = 0; i < xticks[me].size(); i++){
          xti = xticks[me][i];
          for(unsigned int j = 0; j < yticks[me].size(); j++){
              ytj = yticks[me][j];
              std::complex<double>cpt (xti, ytj);
              std::complex<double> z = cpt;
              #pragma omp for schedule(static, c_chunksize)
              for(int k = 1; k <= c_maxiters; k++){
                  z = (z*z) + cpt;
                  if(abs(z) > 2.0) continue;
                  //rcpp matrix
                  if(k == c_maxiters) m[me](i,j) = 1;
                         
              }
              
          }
      }
      
    #pragma omp barrier

    #pragma omp single
    {
        int totalSize_x = 0;
        for(int i = 0; i < c_nth; i++){
            totalSize_x += m[i].nrow();
        }
        
        output(totalSize_x, m[0].ncol());
        
        //don't want to keep reseting to row 0, want to fill matrix
        int j = 0;
        for(int i = 0; i < c_nth; i++){
            for(; j < m[i].nrow(); j++){
                for(int k = 0; k < m[0].ncol(); k++){
                    output(j,k) = m[i](j,k);
                }
            }
            
        }
    }
  }
  
  return R_NilValue;
}