#problem  w1 w2 ... wq 
#result : efficiencyMin | efficiencymax
calculateMinMaxEfficiency = function (dmuData, subjectDmu,transformToUtilities=TRUE) {
  orgData<- dmuData
  
  source("ImpreciseEfficiency.R")
  if(transformToUtilities)
  {
    dmuData <- transformToUtilitiyValues(orgData, subjectDmu, "pes")
  }
  else
  {
    dmuData <-recalculatePerformance(orgData, subjectDmu, "pes")
  }
  source("impreciseMinmaxEfficiency.R")
  minModelEff <- createProblemModel("min", subjectDmu, dmuData)
  
  source("ImpreciseEfficiency.R")
  if(transformToUtilities)
  {
    dmuData <- transformToUtilitiyValues(orgData, subjectDmu, "opt")
  }
  else
  {
    dmuData <-recalculatePerformance(orgData, subjectDmu, "opt")
  }
  source("impreciseMinmaxEfficiency.R")
  maxModelEff <- createProblemModel("max", subjectDmu, dmuData)
  
  
  result <- array(0, dim=2)
  result[1] <- get.objective(minModelEff)
  result[2] <- get.objective(maxModelEff)
  rm(minModelEff)
  rm(maxModelEff)
  return (result)
}

calculateMinMaxEfficiencyForAll = function (dmuData, transformToUtilities=TRUE, normalize=FALSE) {
  dmuCount = nrow(dmuData$data)
  result <- array(0, dim=c(dmuCount,2))
  for(i in 1:dmuCount) {
    result[i,] <- calculateMinMaxEfficiency(dmuData, i, transformToUtilities)
  }
  if(normalize==TRUE) {
    maxEff <- max(result)
    result <- result/maxEff
  }
  return (result)
}

calculateWeights = function(dmuData, subjectDmu) {
  modelEff <- createProblemModel(subjectDmu, dmuData)
  weights <- get.variables(modelEff)
  return (weights)
}

createProblemModel = function (dir, subjectDmuIdx, dmuData) {
  ordinalCount <- length(dmuData$ordinalFactors)
  weightsCount <- dmuData$inputCount + dmuData$outputCount
  varCount <- weightsCount + (ordinalCount + length(dmuData$multipleFunctionCriteria)) * nrow(dmuData$data)
  
  lprec <- make.lp(0, varCount)
  if(dir == "max")
    lp.control(lprec, sense="max")
  
  createProblemObjective(dir,lprec, subjectDmuIdx, dmuData)
  createConstraints(lprec, subjectDmuIdx, dmuData)
  solve(lprec)
  return (lprec)
}

createProblemObjective = function(dir, model, subjectDmuIdx, dmuData) {
  dmuCount <- nrow(dmuData$data)
  ordinalCount <- length(dmuData$ordinalFactors)
  weightsCount <- dmuData$inputCount + dmuData$outputCount
  varCount <- weightsCount + (ordinalCount + length(dmuData$multipleFunctionCriteria)) * dmuCount
  
  objective <-  array(0, dim=varCount)
  offset <- weightsCount
  for(i in 1:weightsCount) {
    if(!(i %in% dmuData$ordinalFactors)
       && !(i %in% dmuData$multipleFunctionCriteria)){
      objective[i] <- dmuData$data[subjectDmuIdx, i]
    }
    else
    {
      objective[offset+subjectDmuIdx]<- 1
      offset <- offset + dmuCount
    }
  }
  set.objfn(model, objective)
}

createConstraints = function(model, subjectDmuIdx, dmuData) {
  createOtherConstraints(model, subjectDmuIdx, dmuData)
  
  ordinalCount <- length(dmuData$ordinalFactors)
  weightsCount <- dmuData$inputCount + dmuData$outputCount
  varCount <- weightsCount + (ordinalCount + length(dmuData$multipleFunctionCriteria)) * nrow(dmuData$data)
  
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

createOtherConstraints = function(model, subjectDmuIdx, dmuData) {
  dmuCount <- nrow(dmuData$data)
  ordinalCount <- length(dmuData$ordinalFactors)
  weightsCount <- dmuData$inputCount + dmuData$outputCount
  varCount <- weightsCount + (ordinalCount + length(dmuData$multipleFunctionCriteria)) * dmuCount
  
  result <- array(0, dim = c(varCount))
  result[1:weightsCount] <- 1
  add.constraint(model, result, "=" , 1)
  
  if(ordinalCount + length(dmuData$multipleFunctionCriteria) > 0)
  {
    for(i in 1:dmuCount)
    {
      offset <- weightsCount
      for(j in 1:weightsCount)
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
}

transformToUtilityValues = function(dmuData) {
  boundaries <- dmuData$boundaries
  dmuCount <- nrow(dmuData$data)
  inputs <- dmuData$inputCount
  outputs <- dmuData$outputCount
  for(i in 1:dmuCount) {
    for(j in 1:(inputs + outputs)) {
      if(j <= inputs) {
        dmuData$data[i,j] <- (boundaries$up[j] - dmuData$data[i,j]) / (boundaries$up[j] - boundaries$low[j])
      } else if (j <= inputs + outputs) {
        dmuData$data[i,j] <- (dmuData$data[i,j] - boundaries$low[j]) / (boundaries$up[j] - boundaries$low[j])
      }
    }
  }
  return (dmuData)
}