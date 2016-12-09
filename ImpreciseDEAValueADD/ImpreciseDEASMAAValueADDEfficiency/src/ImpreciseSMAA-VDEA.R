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
      constraints <- createOrdinalConstraints(dmuData, i)
      samples <- createSamples(constraints, samplesNo)
      for(j in 1:samplesNo)
      {
        result[[j]][,i] <- samples[j,]
      }
    }
    else if(i %in% dmuData$multipleFunctionCriteria)
    {
      for(j in 1:samplesNo)
      {
        dmuData$data <- result[[j]]
        
        constraints <- createMultipleFunctionConstraints(dmuData, i)
        samples <- createSamples(constraints, 1)
        result[[j]][,i] <- samples
      }
    }
  }
  
  for(j in 1:samplesNo)
  {
    dmuData$data <- result[[j]]
    result[[j]] <- transformToUtilityValues(dmuData)$data
  }
  return(result)
}

createOrdinalConstraints <- function(dmuData, subjectFactor)
{
  isInput <- subjectFactor <= dmuData$inputCount
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
  
  sign <- -1
  if(isInput)
  {
    sign <- 1
  }
  currConstr[criteriumValues[1,1]] <- sign * 1
  dir <- c(dir,  c("<="))
  if(isInput)
  {
    rhs <- c(rhs, c(0.99))
  }
  else
  {
    rhs <- c(rhs, c(-0.01))
  }
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
      if(isInput)
      {
        constraint[criteriumValues[1,i]] <- eps
        constraint[criteriumValues[1,(i-1)]] <- -1
      }
      else
      {
        constraint[criteriumValues[1,i]] <- -1
        constraint[criteriumValues[1,(i-1)]] <- eps
      }
      dir <- c(dir,  c("<="))
    }
    rhs <- c(rhs, c(0))
    constr <- rbind(constr, constraint)
  }
  constraint <- numeric(dmuCount)
  constraint[criteriumValues[1,dmuCount]] <- -sign
  dir <- c(dir,  c("<="))
  if(isInput)
  {
    rhs <- c(rhs, c(-0.01))
  }
  else
  {
    rhs <- c(rhs, c(0.99))
  }
  constr <- rbind(constr, constraint)
  
  rownames(constr) <- NULL
  constraints <- list(constr=constr, dir=dir, rhs=rhs)
  return (constraints)
}

createMultipleFunctionConstraints <- function(dmuData, subjectFactor)
{
  isInput <- subjectFactor <= dmuData$inputCount
  eps = 1.1
  dmuCount <- length(dmuData$altIDs)
  constr <- c()
  dir <- c()
  rhs <- c()
  
  criteriumValues <- array(dim=c(2, dmuCount))
  criteriumValues[1,] <- 1:dmuCount
  criteriumValues[2,] <- dmuData$data[,subjectFactor]
  criteriumValues<- criteriumValues[,order(criteriumValues[2,])]
  
  for(i in 1:dmuCount)
  {
    constraint <- numeric(dmuCount)
    values <- c(calculateFunctionValue(dmuData,i,subjectFactor, 1), calculateFunctionValue(dmuData,i,subjectFactor, 2))
    values <- sort(values)
    dir <- c(dir,  c("<=","<="))
    rhs <- c(rhs, c(-values[1], values[2]))
    constraint[i] <- -1
    constr <- rbind(constr, constraint)
    constraint[i] <- 1
    constr <- rbind(constr, constraint)
    
    if(i > 1)
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
        if(isInput)
        {
          constraint[criteriumValues[1,i]] <- eps
          constraint[criteriumValues[1,(i-1)]] <- -1
        }
        else
        {
          constraint[criteriumValues[1,i]] <- -1
          constraint[criteriumValues[1,(i-1)]] <- eps
        }
        dir <- c(dir,  c("<="))
      }
      rhs <- c(rhs, c(0))
      constr <- rbind(constr, constraint)
    }
  }
  
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

calculateFunctionValue <- function(dmuData, i, j, funcId)
{
  inputs <- dmuData$inputCount
  points <- dmuData$functionShapes[[j]][[funcId]]
  x1 <- dmuData$boundaries$low[j]
  x2 <- dmuData$boundaries$up[j]
  
  if(j <= inputs) {
    y1 <- 1
    y2 <- 0
  }
  else
  {
    y1 <- 0
    y2 <- 1
  }
  
  if(is.null(nrow(points)))
  {
    tmp <-points
    points <- array(dim=c(1,2))
    points[1,] <-tmp
    
  }
  
  for(p in 1:nrow(points))
  {
    if(points[p,1] == dmuData$data[i,j])
    {
      return (points[p,2])
    }
    if(points[p,1] < dmuData$data[i,j])
    {
      x1 <- points[p,1]
      y1 <- points[p,2]
    }
    else
    {
      x2 <- points[p,1]
      y2 <- points[p,2]
      break
    }
  }
  
  if(j <= inputs) {
    return(((x2 - dmuData$data[i,j]) / (x2 - x1)) * (y1 - y2) + y2)
  }
  return(dmuData$data[i,j] <- ((dmuData$data[i,j] - x1) / (x2 - x1)) * (y2 - y1) + y1)
  
}

transformToUtilityValues = function(dmuData) {
  boundaries <- dmuData$boundaries
  dmuCount <- nrow(dmuData$data)
  inputs <- dmuData$inputCount
  outputs <- dmuData$outputCount
  for(i in 1:dmuCount) {   
    for(j in 1:(inputs + outputs)) {
      if(!j %in% dmuData$ordinalFactors
         && !j %in% dmuData$multipleFunctionCriteria)
      {
        if(length(dmuData$functionShapes[[j]]) > 0)
        {
          dmuData$data[i,j] <- calculateFunctionValue(dmuData, i, j, 1)
        }
        else
        {
          if(j <= inputs) {
            dmuData$data[i,j] <- (boundaries$up[j] - dmuData$data[i,j]) / (boundaries$up[j] - boundaries$low[j])
          } else if (j <= inputs + outputs) {
            dmuData$data[i,j] <- (dmuData$data[i,j] - boundaries$low[j]) / (boundaries$up[j] - boundaries$low[j])
          }
        }
      }
    }
  }
  return (dmuData)
}

