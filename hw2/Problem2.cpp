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

#include <Rcpp.h>
#include <omp.h>
#include <string>

using namespace Rcpp;

RcppExport SEXP mandel(SEXP nth, SEXP xl, SEXP xr, 
        SEXP yb, SEXP yt, SEXP inc, 
        SEXP x_count, SEXP y_count, SEXP maxiters, 
        SEXP sched, SEXP chunksize) {
    // Convert to C-land variables
    const int c_nth = as<int>(nth),
        c_x_count = as<int>(x_count), 
        c_y_count = as<int>(y_count),
        c_maxiters = as<int>(maxiters),
        c_chunksize = as<int>(chunksize);
    const double c_xl = as<double>(xl), c_xr = as<double>(xr),
           c_yb = as<double>(yb), c_yt = as<double>(yt),
           c_inc = as<double>(inc);
    const std::string c_sched = as<std::string>(sched);
    omp_sched_t schedule;

    // Set scheduling as passed in
    if(c_sched == "static") {
        schedule = omp_sched_static;
    } else if(c_sched == "dynamic") {
        schedule = omp_sched_dynamic;
    } else if(c_sched == "guided") {
        schedule = omp_sched_guided;
    } else {
        schedule = omp_sched_auto;
    }
    omp_set_schedule(schedule, c_chunksize);

    // Create matrix
    NumericMatrix m(c_x_count, c_y_count);

    // Parallelize given the parameters for both for loops
    #pragma omp parallel for num_threads(c_nth) collapse(2) schedule(runtime)
    for(int i = 0; i < c_x_count; i++){
        for(int j = 0; j < c_y_count; j++){
            // Get point
            const std::complex<double> cpt(c_xl + i*c_inc, c_yb + j*c_inc);
            // Run for max iters
            std::complex<double> z = cpt;
            for(int k = 1; k <= c_maxiters; k++){
                z = (z*z) + cpt;
                if(abs(z) > 2.0) break;
                if(k == c_maxiters) m(i, j) = 1;
            }
        }
    }

    return m;
}
