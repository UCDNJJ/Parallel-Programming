library(dice)
library(ggplot2)
# source('snowdice.R')
# source('optdice.R')
# source('cppdice.R')
# source('thrustdice.R')
# source('ompdice.R')

delta <- 0.00001

# Helper to record into the dataframe
recordTime <- function(df, i, rolls, start, backend){
  time <- Sys.time() - start
  units(time) <- "secs"
  df$time[i] <- time 
  df$rolls[i] <- rolls
  df$backend[i] <- backend
  df
}

# Stops execution if the results did not match
stopIfDifferent <- function(a, b){
  print(a)
  print(b)
  stopifnot(abs(a - b) < delta)
}

# Benchmarks each backend with different number of dice
speedTest <- function(rollsToTest, argfun, backends){
  backend_count <- length(which(sapply(backends, function(x){x[1]}) == T))
  # Set up data frame
  df <- data.frame(time=numeric(length(rollsToTest)*backend_count), 
                   rolls=numeric(length(rollsToTest)*backend_count), 
                   backend=character(length(rollsToTest)*backend_count), 
                   stringsAsFactors = FALSE)
  i <- 1
  # Test each problem size
  for(rolls in rollsToTest){
    # Generate args from parameter
    args <- argfun(rolls)
    
    # Test original version
    if(backends$original){
      start <- Sys.time()
      original <- do.call(getEventProb, args)
      df <- recordTime(df, i, rolls, start, "original")
      i <- i + 1
    }
    
    # Test snow version
    if(backends$snow){
      start <- Sys.time()
      result <- do.call(snowGetEventProb, args)
      if(!backends$original){
        original <- result
      }
      df <- recordTime(df, i, rolls, start, "snow")
      i <- i + 1
      stopIfDifferent(original, result)
    }
    
    # Test thrust version
    if(backends$thrust){
      start <- Sys.time()
      result <- do.call(thrustGetEventProb, args)
      df <- recordTime(df, i, rolls, start, "thrust")
      i <- i + 1
      stopIfDifferent(original, result)
    }
    
    # Test thrust version
    if(backends$omp){
      start <- Sys.time()
      result <- do.call(ompGetEventProb, args)
      df <- recordTime(df, i, rolls, start, "omp")
      i <- i + 1
      stopIfDifferent(original, result)
    }
    
    if(backends$cpp){
      start <- Sys.time()
      result <- do.call(cppGetEventProb, args)
      df <- recordTime(df, i, rolls, start, "cpp")
      i <- i + 1
      stopIfDifferent(original, result)
    }
    
    # Test optimized version
    if(backends$optimized){
      start <- Sys.time()
      result <- do.call(optGetEventProb, args)
      df <- recordTime(df, i, rolls, start, "optimized")
      i <- i + 1
      stopIfDifferent(original, result)
    }
    
    print('----')
  }
  # Plot results
  p <- ggplot(df, aes(y=time, x=rolls, group=backend, colour=backend))
  p <- p + geom_line()
  p
}

# Run test
# speedTest(rollsToTest=1:10, function(rolls){
#   list(nrolls=3, ndicePerRoll=rolls, nsidesPerDie=6, eventList=rep(list(rolls:(5*rolls)), 3), orderMatters=F)
# }, list(snow=T, original=F, thrust=T, cpp=F, optimized=T, omp=T))

