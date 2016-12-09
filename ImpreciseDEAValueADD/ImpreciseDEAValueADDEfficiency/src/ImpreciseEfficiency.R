#problem :
# v1 v2 ..vm | u1 u2 .. un u(n+1) 
#result[i] :
#efficiency_i
recalculatePerformance = function(dmuData, subjectDmu, direction="opt") {
  dmuCount <- nrow(dmuData$data) 
  weightsCount <- dmuData$inputCount + dmuData$outputCount
  if(direction == "opt")
  {
    for(i in 1:dmuCount) {
      if(i == subjectDmu)
      {
        for(j in 1:dmuData$inputCount) {
          dmuData$data[i,j] <- dmuData$minData[i,j]
        }
        
        for(j in (dmuData$inputCount + 1): weightsCount) {
          dmuData$data[i,j] <- dmuData$maxData[i,j]
        }
      }
      else
      {
        for(j in 1:dmuData$inputCount) {
          dmuData$data[i,j] <- dmuData$maxData[i,j]
        }
        
        for(j in (dmuData$inputCount + 1): weightsCount) {
          dmuData$data[i,j] <- dmuData$minData[i,j]
        }
      }
    }
  }
  else
  {
    for(i in 1:dmuCount) {
      if(i == subjectDmu)
      {
        for(j in 1:dmuData$inputCount) {
          dmuData$data[i,j] <- dmuData$maxData[i,j]
        }
        for(j in (dmuData$inputCount + 1): weightsCount) {
          dmuData$data[i,j] <- dmuData$minData[i,j]
        }
      }
      else
      {
        for(j in 1:dmuData$inputCount) {
          dmuData$data[i,j] <- dmuData$minData[i,j]
        }
        
        for(j in (dmuData$inputCount + 1): weightsCount) {
          dmuData$data[i,j] <- dmuData$maxData[i,j]
        }
      }
    }
  }
  return(dmuData)
}

recalculateCriterionPerformance = function(dmuData, subjectDmu, criterion, direction="opt") {
  dmuCount <- nrow(dmuData$data) 
  weightsCount <- dmuData$inputCount + dmuData$outputCount
  if(direction == "opt")
  {
    for(i in 1:dmuCount) {
      if(i == subjectDmu)
      {
        if(criterion <= dmuData$inputCount) {
          dmuData$data[i,criterion] <- dmuData$minData[i,criterion]
        }
        else {
          dmuData$data[i,criterion] <- dmuData$maxData[i,criterion]
        }
      }
      else
      {
        if(criterion <= dmuData$inputCount) {
          dmuData$data[i,criterion] <- dmuData$maxData[i,criterion]
        }
        
        else {
          dmuData$data[i,criterion] <- dmuData$minData[i,criterion]
        }
      }
    }
  }
  else
  {
    for(i in 1:dmuCount) {
      if(i == subjectDmu)
      {
        if(criterion <= dmuData$inputCount) {
          dmuData$data[i,criterion] <- dmuData$maxData[i,criterion]
        }
        else {
          dmuData$data[i,criterion] <- dmuData$minData[i,criterion]
        }
      }
      else
      {
        if(criterion <= dmuData$inputCount) {
          dmuData$data[i,criterion] <- dmuData$minData[i,criterion]
        }
        
        else {
          dmuData$data[i,criterion] <- dmuData$maxData[i,criterion]
        }
      }
    }
  }
  return(dmuData)
}

transformToUtilitiyValues = function(dmuData, subjectDmu, direction = "opt")
{
  dmuCount <- nrow(dmuData$data) 
  weightsCount <- dmuData$inputCount + dmuData$outputCount
  boundaries <- dmuData$boundaries
  inputs <- dmuData$inputCount
  outputs <- dmuData$outputCount
  
  for(j in 1:(inputs + outputs)) {
    if(j %in% dmuData$ordinalFactors)
    {
      #dmuData$data[,j] <- createFunctionModel(dmuData,subjectDmu,j, direction)
      dmuData$data[,j] <- dmuData$minData[,j]
    }
    else if(length(dmuData$functionShapes[[j]]) > 0)
    {
      dmuData <- recalculateCriterionPerformance(dmuData, subjectDmu, j, direction)
      if(!j %in% dmuData$multipleFunctionCriteria)
      {
        for(i in 1:dmuCount) {
          dmuData$data[i,j] <- calculateFunctionValue(dmuData, i, j, 1)
        }
      }
    }
    else
    {
      for(i in 1:dmuCount) {
        if(direction == "opt")
        {
          if(i == subjectDmu)
          {
            if(j <= dmuData$inputCount) {
              dmuData$data[i,j] <- (boundaries$up[j] - dmuData$minData[i,j])/(boundaries$up[j] - boundaries$low[j])
            } else {
              dmuData$data[i,j] <- (dmuData$maxData[i,j] - boundaries$low[j])/(boundaries$up[j] - boundaries$low[j])
            }
          }
          else
          {
            if(j <= dmuData$inputCount) {
              dmuData$data[i,j] <- (boundaries$up[j] - dmuData$maxData[i,j])/(boundaries$up[j] - boundaries$low[j])
            } else {
              dmuData$data[i,j] <- (dmuData$minData[i,j] - boundaries$low[j])/(boundaries$up[j] - boundaries$low[j])
            }
          }
        }
        else
        {
          if(i == subjectDmu)
          {
            if(j <= dmuData$inputCount) {
              dmuData$data[i,j] <- (boundaries$up[j] - dmuData$maxData[i,j])/(boundaries$up[j] - boundaries$low[j])
            } else {
              dmuData$data[i,j] <- (dmuData$minData[i,j] - boundaries$low[j])/(boundaries$up[j] - boundaries$low[j])
            }
          }
          else
          {
            if(j <= dmuData$inputCount) {
              dmuData$data[i,j] <- (boundaries$up[j] - dmuData$minData[i,j])/(boundaries$up[j] - boundaries$low[j])
            } else {
              dmuData$data[i,j] <- (dmuData$maxData[i,j] - boundaries$low[j])/(boundaries$up[j] - boundaries$low[j])
            }
          }
        }
      }
    }
  }
  return(dmuData)
}


calculateEfficiency = function (dmuData, subjectDmu) {
  modelEff <- createProblemModel(subjectDmu, dmuData)
  
  #result <- array(0, dim=2)
  #weights <- get.variables(modelEff)
  #result[1] <- calculateEfficiencyForWeights(dmuData, subjectDmu, weights)
  result <- get.objective(modelEff)
  rm(modelEff)
  
  return (result)
}

calculateEfficiencyForWeights = function (dmuData, subjectDmuIdx, weights) {
  outputs <- 0
  offset <-(dmuData$inputCount + dmuData$outputCount) - length(dmuData$ordinalFactors)
  idx <- 1
  for(i in 1:(dmuData$inputCount + dmuData$outputCount)) {
    if(i %in% dmuData$ordinalFactors)
    {
      outputs <- outputs + weights[offset+subjectDmuIdx]
      offset <- offset + nrow(dmuData$data)
    }
    else
    {
      outputs <- outputs + weights[i] * dmuData$data[subjectDmuIdx, i]
      idx <- idx + 1
    }
  }
  return (outputs)
}

calculateEfficiencyForAll = function (dmuData, transformToUtilities=TRUE, normalize=FALSE) {
  dmuCount = nrow(dmuData$data)
  result <- c() #array(0, dim=c(dmuCount,1))
  
  
  for(i in 1:dmuCount) {
    if(transformToUtilities)
    {
      dmuData <- transformToUtilitiyValues(dmuData, i, "opt")
    }
    else
    {
      dmuData <- recalculatePerformance(dmuData, i, "opt")
    }
    result <- rbind(result, calculateEfficiency(dmuData, i))
  }
  
  if(normalize==TRUE) {
    maxEff <- max(result)
    result <- result/maxEff
  }
  return (result)
}

calculatePesEfficiencyForAll = function (dmuData, transformToUtilities=TRUE, normalize=FALSE) {
  dmuCount = nrow(dmuData$data)
  result <- c() #array(0, dim=c(dmuCount,1))
  
  for(i in 1:dmuCount) {
    if(transformToUtilities)
    {
      dmuData <- transformToUtilitiyValues(dmuData, i, "pes")
    }
    else
    {
      dmuData <-recalculatePerformance(dmuData, i, "pes")
    }
    result <- rbind(result, calculateEfficiency(dmuData, i))
  }
  
  if(normalize==TRUE) {
    maxEff <- max(result)
    result <- result/maxEff
  }
  
  return (result)
}

createProblemModel = function (subjectDmuIdx, dmuData) {
  ordinalCount <- length(dmuData$ordinalFactors)
  variablesCount <- dmuData$inputCount + dmuData$outputCount + 
    (ordinalCount + length(dmuData$multipleFunctionCriteria)) * nrow(dmuData$data) + 1
  lprec <- make.lp(0, variablesCount)
  lp.control(lprec,sense='min')
  
  createProblemObjective(lprec, subjectDmuIdx, dmuData)
  createConstraints(lprec, subjectDmuIdx, dmuData)
  solve(lprec)
  return (lprec)
}

createProblemObjective = function(model, subjectDmuIdx, dmuData) {
  ordinalCount <- length(dmuData$ordinalFactors)
  varCount <- dmuData$inputCount + dmuData$outputCount + 
    (ordinalCount + length(dmuData$multipleFunctionCriteria)) * nrow(dmuData$data) + 1
  objective <-  array(0, dim=varCount)
  objective[varCount] <- 1
  set.objfn(model, objective)
}

createConstraints = function(model, subjectDmuIdx, dmuData) {
  ordinalCount <- length(dmuData$ordinalFactors)
  varCount <- dmuData$inputCount + dmuData$outputCount + 
    (ordinalCount + length(dmuData$multipleFunctionCriteria)) * nrow(dmuData$data) + 1
  
  createSubjectConstraints(model, subjectDmuIdx, dmuData)
  
  for(i in dmuData$ordinalFactors)
  {
    createMonotonicityConstraints(model, i, dmuData, varCount)
  }
  for(i in dmuData$multipleFunctionCriteria)
  {
    createMonotonicityConstraints(model, i, dmuData, varCount)
  }
  
  if(dmuData$withWeightConstraints == TRUE) {
    source("weightConstraints.R")
    createCustomWeightConstraints(model, dmuData, varCount)
  }
}

#subject DMU constraints
createSubjectConstraints = function(model, subjectDmuIdx, dmuData) {
  dmuCount <- nrow(dmuData$data)
  ordinalCount <- length(dmuData$ordinalFactors)
  
  critCount <- dmuData$inputCount + dmuData$outputCount
  varCount <-  critCount + (ordinalCount + length(dmuData$multipleFunctionCriteria)) * dmuCount + 1
  
  for(i in 1:dmuCount)
  {
    if(i != subjectDmuIdx)
    {
      offset <- critCount
      
      constraint <- array(0, dim = varCount)
      constraint[varCount] <- -1
      for(j in 1:(critCount))
      {
        if(!(j %in% dmuData$ordinalFactors)
          && !(j %in% dmuData$multipleFunctionCriteria)){
            constraint[j] <- dmuData$data[i,j] - dmuData$data[subjectDmuIdx,j]
        }
        else
        {
          constraint[offset + i] <- 1
          constraint[offset + subjectDmuIdx] <- -1
          offset <- offset + dmuCount
        }
      }
      add.constraint(model, constraint, "<=", 0)
    }
  }
  
  if(ordinalCount + length(dmuData$multipleFunctionCriteria) > 0)
  {
    for(i in 1:dmuCount)
    {
      offset <- critCount
      for(j in 1:critCount)
      {
        if(j %in% dmuData$ordinalFactors)
        {
          constraint <- array(0, dim = varCount)
          constraint[offset + i] <- -1
          constraint[j] <- 1
          offset <- offset + dmuCount
          
          add.constraint(model, constraint, ">=", 0)
        }
        else if(j %in% dmuData$multipleFunctionCriteria)
        {
          values=c(0,0)
          values[1] <- calculateFunctionValue(dmuData, i, j, 1)
          values[2] <- calculateFunctionValue(dmuData, i, j, 2)
          values <- sort(values)
          
          constraint <- array(0, dim = varCount)
          constraint[offset + i] <- 1
          constraint[j] <- -values[1]
          add.constraint(model, constraint, ">=", 0)
          
          constraint <- array(0, dim = varCount)
          constraint[offset + i] <- 1
          constraint[j] <- -values[2]
          add.constraint(model, constraint, "<=", 0)
          
          offset <- offset + dmuCount
        }
      }
    }
  }
  constraint = array(0, dim = varCount)
  constraint[1:critCount] <- 1
  add.constraint(model, constraint, "=", 1)
}

createMonotonicityConstraints = function(model, subjectCriterion, dmuData, varCount) {
  eps <- 0.0001
  alpha <- 1.05
  isInput <- subjectCriterion <= dmuData$inputCount
  
  data <- dmuData$data
  dmuCount <- nrow(data)
  
  criteriumValues <- array(dim=c(2, dmuCount))
  criteriumValues[1,] <- 1:dmuCount
  criteriumValues[2,] <- data[,subjectCriterion]
  criteriumValues<- criteriumValues[,order(criteriumValues[2,])]
  
  idx <- sum(dmuData$ordinalFactors <= subjectCriterion) + sum(dmuData$multipleFunctionCriteria <= subjectCriterion) - 1

  
  offset <- dmuData$inputCount+dmuData$outputCount + dmuCount * idx
  
  if(subjectCriterion %in% dmuData$ordinalFactors)
  {
    constraint <- array(0, dim = c(varCount))
    if(isInput)
    {
      constraint[criteriumValues[1,dmuCount] + offset] <- 1
    }
    else
    {
      constraint[criteriumValues[1,1] + offset] <- 1
    }
    add.constraint(model, constraint, ">=", eps)
  } 
  for(i in 2:dmuCount)
  {
    constraint <- array(0, dim = c(varCount))
    if(criteriumValues[2,i - 1] == criteriumValues[2,i])
    {
      constraint[criteriumValues[1,i] + offset] <- 1
      constraint[criteriumValues[1,i - 1] + offset] <- -1
      add.constraint(model, constraint, "=", 0)
    }
    else
    {
      if(isInput)
      {
        constraint[criteriumValues[1,i] + offset] <- -alpha
        constraint[criteriumValues[1,i - 1] + offset] <- 1
      }
      else
      {
        constraint[criteriumValues[1,i] + offset] <- 1
        constraint[criteriumValues[1,i - 1] + offset] <- -alpha
      }
      add.constraint(model, constraint, ">=", 0)  
    }
  }
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