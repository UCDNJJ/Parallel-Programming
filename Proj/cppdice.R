dyn.load("dice.so")
library(Rcpp)

cppGetEventProb = function(nrolls,
                        ndicePerRoll,
                        nsidesPerDie,
                        eventList,
                        orderMatters=FALSE)
{
  .Call("getEventProb", nrolls, ndicePerRoll, nsidesPerDie, eventList, orderMatters)
}
