squaretolist <- function (m) {
  as.list(apply(m, 1, function(v) {
    one_positions <- which(v == 1)
    if(length(one_positions)) one_positions else NA
  }))
}

squaretothin <- function(m) {
  thin <- which(m == 1, arr.ind=TRUE)
  colnames(thin) <- NULL
  thin[order(thin[,1], thin[,2]),]
}

thintosquare <- function(thin, nvert) {
  ms = matrix(nrow=nvert, ncol=nvert, 0)
  for (i in 1:nrow(thin)) {
    pos = thin[i,]
    ms[pos[1], pos[2]] = 1
  }
  return(ms)
}

thintolist <- function(thin, nvert)
{
  ms = thintosquare(thin,nvert)
  return(squaretolist(ms))
}

listtosquare <- function(inlist) {
  nvert = length(inlist)
  m = matrix(nrow=nvert, ncol=nvert, 0)
  for(i in 1:nvert) {
    m[i, inlist[[i]] ] = 1
  }
  m
}

listtothin <- function(inlist) {
  squaretothin(listtosquare(inlist))
}

# s <- rbind(
#   c(1, 1, 0, 1, 0), 
#   c(0, 0, 0, 1, 0), 
#   c(0, 0, 0, 1, 0), 
#   c(1, 0, 0, 1, 0), 
#   c(0, 1, 0, 1, 0))
# m <- thintosquare(listtothin(thintolist(squaretothin(s), nrow(s))), nrow(s))
# print(m)