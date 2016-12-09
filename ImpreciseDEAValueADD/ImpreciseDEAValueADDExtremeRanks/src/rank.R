#problem :
# u01 u02 .. u0n | u1 u2 .. un | z1 z2 .. zk
#result[i] :  
#minRank_i, maxRank_i

calculateRank = function (dmuData, transformToUtilites=TRUE) {
  
  dmuCount = nrow(dmuData$data)
  result <- array(0, dim=c(dmuCount, 2))
  orgData <- dmuData
  source("ImpreciseEfficiency.R")
  for(i in 1:nrow(dmuData$data)) {
    if(transformToUtilites == TRUE) {
      dmuData <- transformToUtilityValues(orgData, i, "opt")
    }
    else {
      dmuData <- recalculatePerformance(orgData,i,"opt")
    }
    #print(dmuData$data[13,])
    source("rank.R")
    modelMin <-createProblemModel("minRank", i, dmuData)
    
    source("ImpreciseEfficiency.R")
    if(transformToUtilites == TRUE) {
      dmuData <- transformToUtilityValues(orgData, i, "pes")
    }
    else {
      dmuData <- recalculatePerformance(orgData,i,"pes")
    }
    source("rank.R")
    modelMax <- createProblemModel("maxRank", i, dmuData)
    result[i,1] <- as.integer(get.objective(modelMin) + 1)
    result[i,2] <- as.integer(get.objective(modelMax) + 1)
    rm(modelMin)
    rm(modelMax)
  }
  return (result)
}

createProblemModel = function (problemName, subjectDmuIdx, dmuData) {
  dmuCount <- nrow(dmuData$data)
  ordinalCount <- length(dmuData$ordinalFactors)
  variablesCount <- dmuData$inputCount + dmuData$outputCount + 
    (ordinalCount + length(dmuData$multipleFunctionCriteria) + 1) * nrow(dmuData$data)
  lprec <- make.lp(0, variablesCount)
  
  if(problemName == "minRank")
    lp.control(lprec, sense="min")
  else
    lp.control(lprec, sense="max")
  
  createProblemObjective(problemName, lprec, subjectDmuIdx, dmuData)
  createConstraints(problemName, lprec, subjectDmuIdx, dmuData)
  setVariablesTypes(lprec, dmuData)
  solve(lprec)
  return (lprec)
}

createProblemObjective = function(problemName, model, subjectDmuIdx, dmuData) {
  dmuCount <- nrow(dmuData$data)
  ordinalCount <- length(dmuData$ordinalFactors)
  variablesCount <- dmuData$inputCount + dmuData$outputCount + 
    (ordinalCount + length(dmuData$multipleFunctionCriteria) + 1) * dmuCount
  
  #objectiveVariables <- array(1, dim=dmuCount)
  #objectiveVariables[subjectDmuIdx] = 0;
  #restOfVariables <- array(0, dim=2 * (dmuData$inputCount + dmuData$outputCount))
  #objective <- append(restOfVariables, objectiveVariables)
  
  objective <- array(0,variablesCount)
  objective[(variablesCount - dmuCount + 1) : variablesCount] <- 1
  objective[(variablesCount - dmuCount + subjectDmuIdx)] <- 0
  
  set.objfn(model, objective)
}

createConstraints = function(problemName, model, subjectDmuIdx, dmuData) {
  dmuCount <- nrow(dmuData$data)
  ordinalCount <- length(dmuData$ordinalFactors)
  variablesCount <- dmuData$inputCount + dmuData$outputCount + 
    (ordinalCount + length(dmuData$multipleFunctionCriteria) + 1) * dmuCount
  
  if(problemName == "minRank"){
    createSubjectConstraintsForMin(model, subjectDmuIdx, dmuData)
  }
  else {
    createSubjectConstraintsForMax(model, subjectDmuIdx, dmuData)
  }
  createOtherConstraints(model, subjectDmuIdx, dmuData)
  
  for(i in dmuData$ordinalFactors)
  {
    createMonotonicityConstraints(model, i, dmuData, variablesCount)
  }
  for(i in dmuData$multipleFunctionCriteria)
  {
    createMonotonicityConstraints(model, i, dmuData, variablesCount)
  }
  
  if(dmuData$withWeightConstraints == TRUE) {
    source("weightConstraints.R")
    createCustomWeightConstraints(model, dmuData, variablesCount)
  }
}


#subject DMU's constraints- worstRank
createSubjectConstraintsForMax = function(model, subjectDmuIdx, dmuData) {
  CONST <- 1000
  dmuCount <- nrow(dmuData$data)
  ordinalCount <- length(dmuData$ordinalFactors)
  variablesCount <- dmuData$inputCount + dmuData$outputCount + 
    (ordinalCount + length(dmuData$multipleFunctionCriteria) + 1) * dmuCount
  
  critCount <- dmuData$inputCount + dmuData$outputCount
  
  for(i in 1:dmuCount)
  {
    offset <- critCount
    constraint <- array(0, dim=variablesCount)
    constraint[variablesCount - dmuCount + i] <- CONST
    for(j in 1:critCount)
    {
      if(!(j %in% dmuData$ordinalFactors)
         && !(j %in% dmuData$multipleFunctionCriteria)){
        constraint[j] <- -dmuData$data[i,j] + dmuData$data[subjectDmuIdx,j]
      }
      else
      {
        constraint[offset + i] <- -1
        constraint[offset + subjectDmuIdx] <- 1
        offset <- offset + dmuCount
      }
    }
    add.constraint(model, constraint, "<=", CONST)
  }
}


#subject DMU's constraints - bestRank
createSubjectConstraintsForMin = function(model, subjectDmuIdx, dmuData) {
  
  CONST <- 1000
  dmuCount <- nrow(dmuData$data)
  ordinalCount <- length(dmuData$ordinalFactors)
  variablesCount <- dmuData$inputCount + dmuData$outputCount + 
    (ordinalCount + length(dmuData$multipleFunctionCriteria) + 1) * dmuCount
  
  critCount <- dmuData$inputCount + dmuData$outputCount
  
  for(i in 1:dmuCount)
  {
    if(i != subjectDmuIdx)
    {
      offset <- critCount
      constraint <- array(0, dim=variablesCount)
      constraint[variablesCount - dmuCount + i] <- -CONST
      for(j in 1:critCount)
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
}

#wagi "sztucznego" DMU musza byc takie same jak wagi DMU obliczanego, suma wag = 1
createOtherConstraints = function(model, subjectDmuIdx, dmuData) {
  dmuCount <- nrow(dmuData$data)
  ordinalCount <- length(dmuData$ordinalFactors)
  varCount <- dmuData$inputCount + dmuData$outputCount + 
    (ordinalCount + length(dmuData$multipleFunctionCriteria) + 1) * dmuCount
  critCount <- dmuData$inputCount + dmuData$outputCount
  
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

setVariablesTypes = function (model, dmuData) {
  dmuCount <- nrow(dmuData$data)
  ordinalCount <- length(dmuData$ordinalFactors)
  varCount <- dmuData$inputCount + dmuData$outputCount + 
    (ordinalCount + length(dmuData$multipleFunctionCriteria) + 1) * dmuCount
  
  set.type(model, columns = (varCount - dmuCount + 1):varCount, type="binary")
}