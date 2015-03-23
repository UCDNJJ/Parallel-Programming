\name{paralleldice-package}
\alias{paralleldice-package}
\alias{paralleldice}
\docType{package}
\title{
Calculate probabilities of various dice-rolling events using parallelized backend
}
\description{
This package parallelizes the dice package by Dylan Arena which provides utilities to calculate the probabilities of various dice-rolling events.
An example is calculating the probability of rolling a four-sided die six times and getting a 4, a 3, and either a 1 or 2 among the six rolls (in any order); the probability of rolling two six-sided dice three times and getting a 10 on the first roll, followed by a 4 on the second roll, followed by anything but a 7 on the third roll; or the probabilities of each possible sum of rolling five six-sided dice, dropping the lowest two rolls, and summing the remaining dice. The parallelizations are implemented using Snow, Thrust, and OpenMP.
}
\details{
\tabular{ll}{
Package: \tab paralleldice\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2015-03-20\cr
License: \tab GPL (>= 2)\cr
}
Although initially conceived as a utility for role-playing game calculations, functions in the \code{paralleldice} package can be used to answer questions in any dice-rolling context (e.g., calculating probabilities for the game of craps, solving problems for an introductory probability course, etc.)

The \code{paralleldice} package requires the \code{gtools} and \code{dice} packages.

For a complete list of functions, use \code{library(help="paralleldice")}.
}
\author{
Nelson Johansen, Ricardo Matsui, Michael Polyakov 

Maintainer: Ricardo Matsui <rmmatsui@ucdavis.edu>
}
\references{
The parallelizations of the getSumProbs and getEventProb functions originated from the CRAN dice package:
\url{http://cran.r-project.org/web/packages/dice/dice.pdf}


\keyword{ package }
\seealso{
\code{\link[pkg:dice-package]{http://cran.r-project.org/web/packages/dice/dice.pdf}}
}
\examples{
getEventProb(nrolls = 6,
             ndicePerRoll = 1,
             nsidesPerDie = 4,
             eventList = list(4, 3, c(1,2)),
             orderMatters = FALSE)

getEventProb(nrolls = 3,
             ndicePerRoll = 2,
             nsidesPerDie = 6,
             eventList = list(10, 4, c(2:6, 8:12)),
             orderMatters = TRUE)

getSumProbs(ndicePerRoll = 5,
            nsidesPerDie = 6,
            nkept = 3,
            dropLowest = TRUE)
}
