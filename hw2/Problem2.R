library(Rcpp)
dyn.load("Problem2.so")

rmandel <- function(nth, xl, xr, yb, yt, inc, maxiters, sched, chunksize) {
    g <- list()
    g$x <- seq(xl, xr, inc)
    g$y <- seq(yb, yt, inc)
    time <- system.time(g$z <- .Call("mandel", nth, xl, xr, yb, yt, inc, length(g$x), length(g$y), maxiters, sched, chunksize))
    nrx <- length(g$x) 
    nrz <- nrow(g$z)
    if(nrx < nrz){
        g$z <- g$z[-(1:(nrz-nrx)),]
    } else if(nrz < nrx) {
        g$x <- g$x[-(1:(nrx-nrz))]
    }
    print(time)
    image(g)
}

mandelopt <- function(nth, xl, xr, yb, yt, inc, maxiters) {
    rmandel(nth, xl, xr, yb, yt, inc, maxiters, "dynamic", 1000);
}
