library(Rcpp)
dyn.load("Problem2.so")

mandelopt <- function(nth, xl, xr, yb, yt, inc, maxiters) {
    g <- list()
    g$x <- seq(xl, xr, inc)
    g$y <- seq(yb, yt, inc)
    time <- system.time(g$z <- .Call("mandel", nth, xl, xr, yb, yt, inc, length(g$x), length(g$y), maxiters, "dynamic", 1000))
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


nth <- 8
xl <- -2
xr <- 0.8
yb <- -2 + 0.6
yt <- 0.8 + 0.6
inc <- 0.014
maxiters <- 10000
mandelopt(nth, xl, xr, yb, yt, inc, maxiters);