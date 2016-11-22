createPerformanceSamples <- function(dmuData, samplesNo){
  result <- c()
  varCount <- dmuData$inputCount + dmuData$outputCount
  dmuCount <- length(dmuData$altIDs)
  for(i in 1:samplesNo)
  {
    result[[i]] <-c()
  }
  
  for(i in 1:dmuCount){
    constraints <- createConstrints(dmuData, i)
    samples <- createSamples(constraints, samplesNo)
    result <- parseSamples(result, samples, i)
  }
  
  for(i in 1:varCount)
  {
    if(i %in% dmuData$ordinalFactors)
    {
      constraints <- createordinalConstrints(dmuData, i)
      samples <- createSamples(constraints, samplesNo)
      for(j in 1:samplesNo)
      {
        result[[j]][,i] <- samples[j,]
      }
    }
  }
  return(result)
}

createordinalConstrints <- function(dmuData, subjectFactor)
{
  eps = 1.1
  dmuCount <- length(dmuData$altIDs)
  currConstr <- numeric(dmuCount)
  constr <- c()
  dir <- c()
  rhs <- c()
  
  criteriumValues <- array(dim=c(2, dmuCount))
  criteriumValues[1,] <- 1:dmuCount
  criteriumValues[2,] <- dmuData$data[,subjectFactor]
  criteriumValues<- criteriumValues[,order(criteriumValues[2,])]
  
  
  currConstr[criteriumValues[1,1]] <- -1
  dir <- c(dir,  c("<="))
  rhs <- c(rhs, c(-0.01))
  constr <- rbind(constr, currConstr)
  
  for(i in 2:dmuCount)
  {
    constraint <- numeric(dmuCount)
    
    if(criteriumValues[2,i] == criteriumValues[2,(i-1)])
    {
      constraint[criteriumValues[1,i]] <- 1
      constraint[criteriumValues[1,(i-1)]] <- -1
      dir <- c(dir,  c("="))
      
    }
    else
    {
      constraint[criteriumValues[1,i]] <- -1
      constraint[criteriumValues[1,(i-1)]] <- eps
      dir <- c(dir,  c("<="))
    }
    rhs <- c(rhs, c(0))
    constr <- rbind(constr, constraint)
  }
  constraint <- numeric(dmuCount)
  constraint[criteriumValues[1,dmuCount]] <- 1
  dir <- c(dir,  c("<="))
  rhs <- c(rhs, c(10000))
  constr <- rbind(constr, constraint)
  
  rownames(constr) <- NULL
  constraints <- list(constr=constr, dir=dir, rhs=rhs)
  return (constraints)
}

createConstrints <- function(dmuData, subjectDmu) {
  varCount <- dmuData$inputCount + dmuData$outputCount
  constr <- c()
  dir <- c()
  rhs <- c()
  
  for(i in 1 : (dmuData$inputCount + dmuData$outputCount)) {
    currConstr <- numeric(varCount)
    currConstr[i] = -1
    rownames(currConstr) <- NULL
    constr <- rbind(constr, currConstr)
    currConstr[i] = 1
    constr <- rbind(constr, currConstr)
    dir <- c(dir,  c("<=", "<="))
    minValue <- -dmuData$minData[subjectDmu, i]
    maxValue <- dmuData$maxData[subjectDmu, i]
    interval <- c(minValue, maxValue)
    rhs <- c(rhs, interval)
    
  }
  rownames(constr) <- NULL
  constraints <- list(constr=constr, dir=dir, rhs=rhs)
  return (constraints)
}

createSamples <- function(constraints, samplesNo) {
  state <- har.init(constraints)
  result <- har.run(state, n.samples=samplesNo)
  samples <- result$samples
  return(samples)
}

parseSamples = function(result, samples, subjectDmu)
{
  varCount<- ncol(samples)
  
  for(i in 1: nrow(samples))
  {
    if(length(result) < nrow(samples))
    {
      result[[i]] <- samples[i,]
    }
    else
    {
      result[[i]] <- rbind(result[[i]], samples[i,])
    }
    
  }
  return(result)
}

