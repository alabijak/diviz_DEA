
#result : N(necesssary)/P(possible)/0(not dominating)


calculateEfficiencyDominanceForAll = function(dmuData, transformToUtilities=TRUE){
  dmuCount = nrow(dmuData$data)
  result <- array(0, dim=c(dmuCount,dmuCount))
  for(i in 1:dmuCount) {
    for(j in 1:dmuCount) {
      if(i != j) {
        result[i,j] = calculateEfficiencyDominance(dmuData, i, j, transformToUtilities)
      } else {
        result[i,j] = 'N'
      }
    }
  }
  return (result)
}

calculateEfficiencyDominance = function(dmuData, subjectDmu, relativeDmu, transformToUtilities=TRUE) {
  orgData <- dmuData
  
  source("ImpreciseEfficiency.R")
  if(transformToUtilities == TRUE) {
    dmuData <- transformToUtilityValues(orgData, subjectDmu, "pes")
  }
  else{
    dmuData <- recalculatePerformance(orgData, subjectDmu, "pes")
  }
  source("efficiencyDominance.R")
  modelNecessary <- createProblemModel(dmuData, subjectDmu, relativeDmu, "N")
  
  source("ImpreciseEfficiency.R")
  if(transformToUtilities == TRUE) {
    dmuData <- transformToUtilityValues(orgData, subjectDmu, "opt")
  }
  else{
    dmuData <- recalculatePerformance(orgData, subjectDmu, "opt")
  }
  source("efficiencyDominance.R")
  modelPossible <- createProblemModel(dmuData, subjectDmu, relativeDmu, "P")
  necessary = get.objective(modelNecessary)
  possible = get.objective(modelPossible)
  result = 0
  
  if(necessary >= 0) {
    result = "N"
  } else if (possible >= 0) {
    result = "P"
  }
  
  return (result)
}

createProblemModel = function (dmuData, subjectDmuIdx, relativeDmuIdx, dominanceType) {
  variablesCount <- dmuData$inputCount + dmuData$outputCount + 
    2 * (length(dmuData$ordinalFactors) + length(dmuData$multipleFunctionCriteria)) + 1
  lprec <- make.lp(0, variablesCount)
  
  if(dominanceType == "N")
    lp.control(lprec, sense="min")
  else
    lp.control(lprec, sense="max")
  
  createProblemObjective(lprec, dmuData)
  createConstraints(lprec, subjectDmuIdx, relativeDmuIdx, dmuData, dominanceType)
  solve(lprec)
  return (lprec)
}

createProblemObjective = function(model,dmuData) {
  variablesCount <- dmuData$inputCount + dmuData$outputCount + 
    2 * (length(dmuData$ordinalFactors) + length(dmuData$multipleFunctionCriteria)) + 1
  
  objective <-  array(0, dim=variablesCount)
  objective[variablesCount] <- 1
  set.objfn(model, objective)
}

createConstraints = function(model, subjectDmuIdx, relativeDmuIdx, dmuData, dominanceType) {
  createSubjectConstraints(model, subjectDmuIdx, relativeDmuIdx, dmuData, dominanceType)
  createOtherConstraints(model, subjectDmuIdx, relativeDmuIdx, dmuData)
  
  for(i in dmuData$ordinalFactors)
  {
    createMonotonicityConstraints(model, i, subjectDmuIdx, relativeDmuIdx, dmuData)
  }
  for(i in dmuData$multipleFunctionCriteria)
  {
    createMonotonicityConstraints(model, i, subjectDmuIdx, relativeDmuIdx, dmuData)
  }
  
  if(dmuData$withWeightConstraints == TRUE) {
    variablesCount <- dmuData$inputCount + dmuData$outputCount + 
      2 * (length(dmuData$ordinalFactors) + length(dmuData$multipleFunctionCriteria)) + 1
    source("weightConstraints.R")
    createCustomWeightConstraints(model, dmuData, variablesCount)
  }
}

#subject DMU constraints
createSubjectConstraints = function(model, subjectDmuIdx, relativeDmuIdx, dmuData, dominanceType) {
  data <- dmuData$data
  weightsCount <- dmuData$inputCount + dmuData$outputCount
  
  varCount <- weightsCount + 
    2 * (length(dmuData$ordinalFactors) + length(dmuData$multipleFunctionCriteria)) + 1
  
  
  sign = "<="
  if(dominanceType == "P") {
    sign = ">="
  }
  
  constraint <- array(0, dim = varCount)
  constraint[varCount] <- -1
  offset <- weightsCount
  for(j in 1:weightsCount) {
    if(!(j %in% dmuData$ordinalFactors)
       && !(j %in% dmuData$multipleFunctionCriteria)){
      constraint[j] <- data[subjectDmuIdx, j] - data[relativeDmuIdx, j]
    }
    else
    {
      constraint[offset + 1] <- 1
      constraint[offset + 2] <- -1
      offset <- offset + 2
    }
  } 
  add.constraint(model, constraint, sign, 0)
  
}

createOtherConstraints = function(model, subjectDmuIdx, relativeDmuIdx, dmuData) {
  weightsCount <- dmuData$inputCount + dmuData$outputCount
  
  varCount <- weightsCount + 
    2 * (length(dmuData$ordinalFactors) + length(dmuData$multipleFunctionCriteria)) + 1
  
  if(length(dmuData$ordinalFactors) + length(dmuData$multipleFunctionCriteria) > 0)
  {
    offset <- weightsCount
    for(j in 1:weightsCount)
    {
      if(j %in% dmuData$ordinalFactors)
      {
        constraint <- array(0, dim = c(2, varCount))
        constraint[1, offset + 1] <- -1
        constraint[2, offset + 2] <- -1
        constraint[,j] <- 1
        offset <- offset + 2
        
        add.constraint(model, constraint[1,], ">=", 0)
        add.constraint(model, constraint[2,], ">=", 0)
      }
      else if(j %in% dmuData$multipleFunctionCriteria)
      {
        values=c(0,0)
        values[1] <- calculateFunctionValue(dmuData, subjectDmuIdx, j, 1)
        values[2] <- calculateFunctionValue(dmuData, subjectDmuIdx, j, 2)
        values <- sort(values)
        
        constraint <- array(0, dim = varCount)
        constraint[offset + 1] <- 1
        constraint[j] <- -values[1]
        add.constraint(model, constraint, ">=", 0)
        
        constraint <- array(0, dim = varCount)
        constraint[offset + 1] <- 1
        constraint[j] <- -values[2]
        add.constraint(model, constraint, "<=", 0)
        
        values=c(0,0)
        values[1] <- calculateFunctionValue(dmuData, relativeDmuIdx, j, 1)
        values[2] <- calculateFunctionValue(dmuData, relativeDmuIdx, j, 2)
        values <- sort(values)
        
        constraint <- array(0, dim = varCount)
        constraint[offset + 2] <- 1
        constraint[j] <- -values[1]
        add.constraint(model, constraint, ">=", 0)
        
        constraint <- array(0, dim = varCount)
        constraint[offset + 2] <- 1
        constraint[j] <- -values[2]
        add.constraint(model, constraint, "<=", 0)
        
        offset <- offset + 2
      }
    }
  }
  
  constraint <- array(0, dim = varCount)
  constraint[1:weightsCount] <- 1
  add.constraint(model, constraint, "=", 1)
  set.bounds(model, lower = -Inf, columns = varCount)
}

createMonotonicityConstraints = function(model, subjectCriterion, subjectDmu, relativeDmu, dmuData, varCount) {
  varCount <- dmuData$inputCount + dmuData$outputCount + 
    2 * (length(dmuData$ordinalFactors) + length(dmuData$multipleFunctionCriteria)) + 1
  
  eps <- 0.0001
  alpha <- 1.05
  isInput <- subjectCriterion <= dmuData$inputCount
  
  data <- dmuData$data
  
  criteriumValues <- array(dim=c(2, 2))
  criteriumValues[1,] <- 1:2
  criteriumValues[2,1] <- data[subjectDmu,subjectCriterion]
  criteriumValues[2,2] <- data[relativeDmu,subjectCriterion]
  criteriumValues<- criteriumValues[,order(criteriumValues[2,])]
  
  idx <- sum(dmuData$ordinalFactors <= subjectCriterion) + sum(dmuData$multipleFunctionCriteria <= subjectCriterion) - 1
  
  offset <- dmuData$inputCount+dmuData$outputCount + 2 * idx
  
  constraint <- array(0, dim = c(varCount))
  if(!subjectCriterion %in% dmuData$multipleFunctionCriteria)
  {
    if(isInput)
    {
      constraint[criteriumValues[1,2] + offset] <- 1
    }
    else
    {
      constraint[criteriumValues[1,1] + offset] <- 1
    }
    add.constraint(model, constraint, ">=", eps)
  }
  
  constraint <- array(0, dim = c(varCount))
  if(criteriumValues[2,1] == criteriumValues[2,2])
  {
    constraint[criteriumValues[1,2] + offset] <- 1
    constraint[criteriumValues[1,1] + offset] <- -1
    add.constraint(model, constraint, "=", 0)
  }
  else
  {
    if(isInput)
    {
      constraint[criteriumValues[1,2] + offset] <- -alpha
      constraint[criteriumValues[1,1] + offset] <- 1
    }
    else
    {
      constraint[criteriumValues[1,2] + offset] <- 1
      constraint[criteriumValues[1,1] + offset] <- -alpha
    }
    add.constraint(model, constraint, ">=", 0)  
  }
  
}
