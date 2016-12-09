#result : lowerBound, upperBound, d_opt, d_pes

calculateEfficiencyBounds = function (dmuData, subjectDmu, transformToUtilities=TRUE) {
  result <- array(0, dim=4)
  orgData <- dmuData
  
  source("impreciseEfficiencyBounds.R")
  maxDist <- calculateMaxDistance(dmuData, subjectDmu, transformToUtilities)
  
  source("ImpreciseEfficiency.R")
  if(transformToUtilities)
  {
    dmuData <- transformToUtilitiyValues(orgData, subjectDmu, "opt")
  }
  else
  {
    dmuData <-recalculatePerformance(orgData, subjectDmu, "opt")
  }
  minDist <- calculateEfficiency(dmuData, subjectDmu)
  
  source("impreciseMinmaxEfficiency.R")
  minmaxEff <- calculateMinMaxEfficiency(orgData, subjectDmu, transformToUtilities)
  result[1] <- minmaxEff[1]
  result[2] <- minmaxEff[2]
  result[3] <- minDist
  result[4] <- maxDist
  return (result)
}

calculateEfficiencyBoundsForAll = function (dmuData, transformToUtilities=TRUE) {
  dmuCount = nrow(dmuData$data)
  result <- array(0, dim=c(dmuCount,4))
  
  for(i in 1:dmuCount) {
    result[i,] <- calculateEfficiencyBounds(dmuData, i, transformToUtilities)
  }
  return (result)
}

#problem :
# u01 u02 .. u0n | u1 u2 .. un | z1 z2 .. zk | d
#result[i] :  
#max_d

calculateMaxDistance = function (dmuData, subjectDmuIdx, transformToUtilities=TRUE) {
  source("ImpreciseEfficiency.R")
  if(transformToUtilities)
  {
    dmuData <- transformToUtilitiyValues(dmuData, subjectDmuIdx, "pes")
  }
  else
  {
    dmuData <-recalculatePerformance(dmuData, subjectDmuIdx, "pes")
  }
  
  source("impreciseEfficiencyBounds.R")
  modelMax <- createProblemModel(subjectDmuIdx, dmuData)
  result <- get.objective(modelMax)
  rm(modelMax)
  return (result)
}

createProblemModel = function (subjectDmuIdx, dmuData) {
  dmuCount <- nrow(dmuData$data)
  ordinalCount <- length(dmuData$ordinalFactors)
  variablesCount <- dmuData$inputCount + dmuData$outputCount + (ordinalCount + length(dmuData$multipleFunctionCriteria) + 1) *  dmuCount + 1
  lprec <- make.lp(0, variablesCount)
  lp.control(lprec, sense="max")
  
  createProblemObjective(lprec, subjectDmuIdx, dmuData)
  createConstraints(lprec, subjectDmuIdx, dmuData)
  setVariablesTypes(lprec, dmuData)
  solve(lprec)
  return (lprec)
}

createProblemObjective = function(model, subjectDmuIdx, dmuData) {
  dmuCount <- nrow(dmuData$data)
  ordinalCount <- length(dmuData$ordinalFactors)
  variablesCount <- dmuData$inputCount + dmuData$outputCount + (ordinalCount + length(dmuData$multipleFunctionCriteria) + 1) * dmuCount + 1
  
  objective <- array(0, dim=variablesCount)
  objective[variablesCount] <- 1
  # objective[1:(2 * (dmuData$inputCount + dmuData$outputCount))] <- 0
  
  set.objfn(model, objective)
}

createConstraints = function(model, subjectDmuIdx, dmuData) {
  
  dmuCount <- nrow(dmuData$data)
  ordinalCount <- length(dmuData$ordinalFactors)
  varCount <- dmuData$inputCount + dmuData$outputCount + (ordinalCount + length(dmuData$multipleFunctionCriteria) + 1) * dmuCount + 1
  
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

createSubjectConstraints = function(model, subjectDmuIdx, dmuData) {
  CONST <- 10000
  
  dmuCount <- nrow(dmuData$data)
  ordinalCount <- length(dmuData$ordinalFactors)
  weightsCount <- (dmuData$inputCount + dmuData$outputCount) 
  
  varCount <- weightsCount + (ordinalCount + length(dmuData$multipleFunctionCriteria) + 1) * dmuCount + 1
  
  for (i in 1 : dmuCount) 
  {
    offset <- weightsCount
    
    constraint <- array(0, dim = varCount)
    constraint[varCount - dmuCount - 1 + i] <- CONST
    constraint[varCount] = -1
    
    for(j in 1 : weightsCount) {
      if(!(j %in% dmuData$ordinalFactors)
         && !(j %in% dmuData$multipleFunctionCriteria)){
        constraint[j] <- dmuData$data[i,j] - dmuData$data[subjectDmuIdx,j]
      }
      else
      {
        constraint[offset + i] <- 1
        constraint[offset + subjectDmuIdx] <- constraint[offset + subjectDmuIdx] - 1
        offset <- offset + dmuCount
      }
    }
    
    add.constraint(model, constraint, ">=", 0)
  }
  
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
          #source("ImpreciseEfficiency.R")
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
  constraint[1:weightsCount]<-1
  add.constraint(model, constraint, "=", 1)
  
  
  constraint <- array(0, dim = varCount)
  constraint[(varCount - dmuCount):(varCount - 1)] <- 1
  add.constraint(model, constraint, "<=", dmuCount-1)
} 

setVariablesTypes = function (model, dmuData) {
  dmuCount <- nrow(dmuData$data)
  ordinalCount <- length(dmuData$ordinalFactors)
  varCount <- dmuData$inputCount + dmuData$outputCount + 
    (ordinalCount + length(dmuData$multipleFunctionCriteria) + 1) * dmuCount + 1
  
  set.type(model, columns = (varCount - dmuCount):(varCount-1), type="binary")
}


