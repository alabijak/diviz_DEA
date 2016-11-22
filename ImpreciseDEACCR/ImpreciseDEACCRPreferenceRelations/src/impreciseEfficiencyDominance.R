#problem
# v1 v2 .. vm | u1 u2 .. un

recalculatePerformance = function(dmuData, subjectDmu, relativeDmu, direction) {
  tolerance <- dmuData$tolerance
  dmuCount <- nrow(dmuData$data) 
  weightsCount <- dmuData$inputCount + dmuData$outputCount
  
  if(direction == "max")
  {
    for(j in 1:dmuData$inputCount) {
      dmuData$data[subjectDmu,j] <- dmuData$minData[subjectDmu,j]
    }
    
    for(j in (dmuData$inputCount + 1): weightsCount) {
      dmuData$data[subjectDmu,j] <- dmuData$maxData[subjectDmu,j]
    }
    
    for(j in 1:dmuData$inputCount) {
      dmuData$data[relativeDmu,j] <- dmuData$maxData[relativeDmu,j]
    }
    
    for(j in (dmuData$inputCount + 1): weightsCount) {
      dmuData$data[relativeDmu,j] <- dmuData$minData[relativeDmu,j]
    }
  }
  else
  {
    for(j in 1:dmuData$inputCount) {
      dmuData$data[subjectDmu,j] <- dmuData$maxData[subjectDmu,j]
    }
    
    for(j in (dmuData$inputCount + 1): weightsCount) {
      dmuData$data[subjectDmu,j] <- dmuData$minData[subjectDmu,j]
    }
    
    for(j in 1:dmuData$inputCount) {
      dmuData$data[relativeDmu,j] <- dmuData$minData[relativeDmu,j]
    }
    
    for(j in (dmuData$inputCount + 1): weightsCount) {
      dmuData$data[relativeDmu,j] <- dmuData$maxData[relativeDmu,j]
    }
  }
  
  return(dmuData)
}


#result[i] :
#dominanceMatrix : N(necesssary)/P(possible)/0(not dominating) minDom: min_dominance, maxDom : max_dominance

calculateDominance = function(dmuData, subjectDmu, relativeDmu) {
  result <- array(0, dim=3)
  oldData <- dmuData$data
  dmuData <- recalculatePerformance(dmuData, subjectDmu, relativeDmu, "min")
  modelMin <-createProblemModel("minEff", subjectDmu, dmuData, relativeDmu)
  dmuData$data <- oldData
  dmuData = recalculatePerformance(dmuData, subjectDmu, relativeDmu, "max")
  modelMax <- createProblemModel("maxEff", subjectDmu, dmuData, relativeDmu)
 # print(modelMin)
#  print(modelMax)
  result[2] <- get.objective(modelMin)
  result[3] <- -get.objective(modelMax)
  if(result[2] >= 0.999) {
    result[1] <- 'N'
  } else if(result[3] >= 0.999){
    result[1] <- 'P'
  } else {
    result[1] <- 0
  }
  rm(modelMin)
  rm(modelMax)
  return (result)
}

calculateDominanceForAll = function (dmuData) {
  dmuCount <- nrow(dmuData$data)
  result <- c()
  result$dominanceMatrix <- array(0, dim=c(dmuCount, dmuCount))
  result$minDom <- array(100, dim=c(dmuCount))
  result$maxDom <- array(0, dim=c(dmuCount))
  for(i in 1:dmuCount) {
    for(j in 1:dmuCount) {
      tempResult <- calculateDominance(dmuData, i, j)
      result$dominanceMatrix[i,j] <- tempResult[1]
      if(i != j ) {
      result$minDom[i] <- min(result$minDom[i], tempResult[2])
      result$maxDom[i] <- max(result$maxDom[i], tempResult[3])        
      }
    }
  }
  return (result)
}

createProblemModel = function (problemName, subjectDmuIdx, dmuData, relativeDmu) {
  ordinalCount <- length(dmuData$ordinalFactors)
  variablesCount <- dmuData$inputCount + dmuData$outputCount + 2*ordinalCount;
  lprec <- make.lp(0, variablesCount)
  
  createProblemObjective(problemName, lprec, subjectDmuIdx,dmuData)
  createConstraints(problemName, lprec, subjectDmuIdx, dmuData, relativeDmu)
  solve(lprec)
  return (lprec)
}

createProblemObjective = function(problemName, model, subjectDmuIdx, dmuData) {
  if (problemName == "maxEff") {
    sign = -1
  }
  else if(problemName == "minEff") {
    sign = 1
  }
  ordinalCount <- length(dmuData$ordinalFactors)
  variablesCount = dmuData$inputCount + dmuData$outputCount + 2*ordinalCount
  objective <- array(0, dim=variablesCount)
  for (i in (dmuData$inputCount + 1):(dmuData$inputCount + dmuData$outputCount)) {
    if(!(i %in% dmuData$ordinalFactors))
    {
      objective[i] <- sign * dmuData$data[subjectDmuIdx, i]
    }
    else
    {
      idx = which(dmuData$ordinalFactors == i)
      objective[dmuData$inputCount + dmuData$outputCount + 2*(idx-1) + 1] <- sign
    }
  }
  set.objfn(model, objective)
}

createConstraints = function(problemName, model, subjectDmuIdx, dmuData, relativeDmu) {
  ordinalCount <- length(dmuData$ordinalFactors)
  variablesCount = dmuData$inputCount + dmuData$outputCount + 2*ordinalCount
  createSubjectConstraints(model, subjectDmuIdx, dmuData)
  createOtherConstraints(problemName, model, subjectDmuIdx, dmuData, relativeDmu)
  createMonotonicityConstraints(model, subjectDmuIdx, relativeDmu, dmuData)
  if(dmuData$withWeightConstraints == TRUE) {
    source("weightConstraints.R")
    createCustomWeightConstraints(model, dmuData, variablesCount)
  }
}

#subject DMU constraints - sum of inputs has to be equal 1
createSubjectConstraints = function(model, subjectDmuIdx, dmuData) {
  ordinalCount <- length(dmuData$ordinalFactors)
  varCount <-  dmuData$inputCount + dmuData$outputCount + 2*ordinalCount
  result <- array(0, dim = varCount)
  
  for (i in 1 : dmuData$inputCount){
    if(!(i %in% dmuData$ordinalFactors))
    {
      result[i] <- dmuData$data[subjectDmuIdx, i]  
    }
    else
    {
      idx = which(dmuData$ordinalFactors == i)
      result[dmuData$inputCount + dmuData$outputCount + 2*(idx-1) + 1] <- 1
    }
   
  }
  add.constraint(model, result, "=", 1)
}

createOtherConstraints = function(problemName, model, subjectDmuIdx, dmuData, relativeDmu) {
  ordinalCount <- length(dmuData$ordinalFactors)
  varCount <-  dmuData$inputCount + dmuData$outputCount + 2*ordinalCount
  
  result <- array(0, dim =varCount)
  
  for(i in 1 : (dmuData$inputCount + dmuData$outputCount)) {
    sign <- 1
    if (i <= dmuData$inputCount) {
      sign <- -1
    }
    
    if(!(i %in% dmuData$ordinalFactors))
    {
      result[i] <- sign*dmuData$data[relativeDmu, i]  
    }
    else
    {
      idx = which(dmuData$ordinalFactors == i)
      result[dmuData$inputCount + dmuData$outputCount + 2*(idx-1) + 2] <- sign
    }
  } 
  sign <- "="
  add.constraint(model, result, sign, 0)
  
}

createMonotonicityConstraints = function(model, subjectDmuIdx, relativeDmuIdx, dmuData)
{
  eps = 1.1
  ordinalCount <- length(dmuData$ordinalFactors)
  varCount <- dmuData$inputCount + dmuData$outputCount + 2 * ordinalCount
  
  for(factor in dmuData$ordinalFactors)
  {
    idx = which(dmuData$ordinalFactors == factor)
    offset <- dmuData$inputCount + dmuData$outputCount + (idx-1)*2
    criteriumValues <- array(dim=c(2, 2))
    criteriumValues[1,] <- 1:2
    criteriumValues[2,1] <- dmuData$data[subjectDmuIdx,factor]
    criteriumValues[2,2] <- dmuData$data[relativeDmuIdx,factor]
    criteriumValues<- criteriumValues[,order(criteriumValues[2,])]
    
    constraint <-  array(0, dim = varCount)
    constraint[offset + criteriumValues[1,1]] <- 1
    add.constraint(model, constraint, ">=", 0.01)
    
    if(criteriumValues[2,1] == criteriumValues[2,2])
    {
      constraint[offset + criteriumValues[1,2]] <- 1
      constraint[offset + criteriumValues[1,1]] <- -1
      add.constraint(model, constraint, "=", 0)
    }
    else
    {
      constraint[offset + criteriumValues[1,2]] <- 1
      constraint[offset + criteriumValues[1,1]] <- -eps
      add.constraint(model, constraint, ">=", 0)
    }
    
    constraint[offset + criteriumValues[1,2]] <- 1
    add.constraint(model, constraint, "<=", 100000)
  }
  
}

