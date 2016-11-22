#problem :
# v1 v2 ..vm | u1 u2 .. un | z1 z2 .. zk
#result[i] :  
#minRank_i, maxRank_i

calculateRank = function (dmuData) {
  dmuCount = nrow(dmuData$data)
  result <- array(0, dim=c(dmuCount, 2))
  
  for(i in 1:nrow(dmuData$data)) {
    modelMin <-createProblemModel("minRank", i, dmuData)
    modelMax <- createProblemModel("maxRank", i, dmuData)
    result[i,1] <- as.integer(get.objective(modelMin) + 1)
    result[i,2] <- as.integer(-get.objective(modelMax) + 1)
    rm(modelMin)
    rm(modelMax)
  }
  return (result)
}

createProblemModel = function (problemName, subjectDmuIdx, dmuData) {
  if(problemName == "minRank"){
    source("impreciseMinRank.R")
    dmuData <- recalculatePerformance(dmuData, subjectDmuIdx)
  }
  else {
    source("impreciseMaxRank.R")
    dmuData <- recalculatePerformance(dmuData, subjectDmuIdx)
  }
  dmuCount <- nrow(dmuData$data)
  ordinalCount <- length(dmuData$ordinalFactors)
  variablesCount = dmuData$inputCount + dmuData$outputCount + dmuCount * (ordinalCount+1);
  lprec <- make.lp(0, variablesCount)
  createProblemObjective(problemName, lprec, subjectDmuIdx, dmuData)
  createConstraints(problemName, lprec, subjectDmuIdx, dmuData)
  createMonotonicityConstraints(lprec, subjectDmuIdx, dmuData)
  setVariablesTypes(lprec, dmuData)
  solve(lprec)
  return (lprec)
}

createProblemObjective = function(problemName, model, subjectDmuIdx, dmuData) {
  dmuCount <- nrow(dmuData$data)
  ordinalCount <- length(dmuData$ordinalFactors)
  if(problemName == "minRank") {
    value = 1
  }
  else {
    value = -1
  }
  objectiveVariables <- array(value, dim=dmuCount)
  objectiveVariables[subjectDmuIdx] = 0;
  restOfVariables <- array(0, dim=(dmuData$inputCount + dmuData$outputCount))
  ordinalVariables <- array(0, dim=(dmuCount * ordinalCount))
  objective <- append(restOfVariables, objectiveVariables)
  objective <- append(objective, ordinalVariables)
  set.objfn(model, objective)
}

createConstraints = function(problemName, model, subjectDmuIdx, dmuData) {
  ordinalCount <- length(dmuData$ordinalFactors)
  dmuCount <- nrow(dmuData$data)
  varCount <- dmuData$inputCount + dmuData$outputCount + dmuCount*(ordinalCount+1)
   createSubjectConstraints(model, subjectDmuIdx, dmuData)
   if(problemName == "minRank"){
      source("impreciseMinRank.R")
      createOtherConstraints(model, subjectDmuIdx, dmuData)
   }
   else {
     source("impreciseMaxRank.R")
     createOtherConstraints(model, subjectDmuIdx, dmuData)
   }
   if(dmuData$withWeightConstraints == TRUE) {
     source("weightConstraints.R")
     createCustomWeightConstraints(model, dmuData, varCount)
   }
}

#subject DMU constraints
createSubjectConstraints = function(model, subjectDmuIdx, dmuData) {
  CONST <- 1000000
  dmuCount <- nrow(dmuData$data)
  ordinalCount <- length(dmuData$ordinalFactors)
  varCount <- dmuCount*(ordinalCount+1) + dmuData$inputCount + dmuData$outputCount
  constrCount <- 2 #one for inputs, one for outputs
  result <- array(0, dim = c(constrCount,varCount))
  
  #for (i in 1 : constrCount) {
  for(j in 1 : (dmuData$inputCount+dmuData$outputCount)) {
    if(j <= dmuData$inputCount)
    {
      constrNo <- 1
    }
    else
    {          
      constrNo <- 2
    }
    
    if(!(j %in% dmuData$ordinalFactors))
    {
      result[constrNo, j] <- dmuData$data[subjectDmuIdx, j]
    }
    else
    {
      idx = which(dmuData$ordinalFactors == j)
      result[constrNo, dmuData$inputCount + dmuData$outputCount + idx*dmuCount + subjectDmuIdx] <- 1
    }
  }
  add.constraint(model, result[1,], "=", 1)
  add.constraint(model, result[2,], "=", 1)
  #}
}

createMonotonicityConstraints = function(model, subjectDmuIdx, dmuData)
{
  eps = 1.1
  dmuCount = nrow(dmuData$data)
  ordinalCount <- length(dmuData$ordinalFactors)
  varCount <- dmuData$inputCount + dmuData$outputCount + dmuCount * (ordinalCount+1)
  
  for(factor in dmuData$ordinalFactors)
  {
    idx = which(dmuData$ordinalFactors == factor)
    offset <- dmuData$inputCount + dmuData$outputCount + idx*dmuCount
    criteriumValues <- array(dim=c(2, dmuCount))
    criteriumValues[1,] <- 1:dmuCount
    criteriumValues[2,] <- dmuData$data[,factor]
    criteriumValues<- criteriumValues[,order(criteriumValues[2,])]
    
    constraint <-  array(0, dim = varCount)
    constraint[offset + criteriumValues[1,1]] <- 1
    add.constraint(model, constraint, ">=", 0.01)
    
    for(i in 2:dmuCount)
    {
      constraint <- array(0, dim = varCount)      
      if(criteriumValues[2,i] == criteriumValues[2,(i-1)])
      {
        constraint[offset + criteriumValues[1,i]] <- 1
        constraint[offset + criteriumValues[1,(i-1)]] <- -1
        add.constraint(model, constraint, "=", 0)
      }
      else
      {
        constraint[offset + criteriumValues[1,i]] <- 1
        constraint[offset + criteriumValues[1,(i-1)]] <- -eps
        add.constraint(model, constraint, ">=", 0)
      }
    }
    
    constraint[offset + criteriumValues[1,dmuCount]] <- 1
    add.constraint(model, constraint, "<=", 100000)
  }
  
}

setVariablesTypes = function (model, dmuData) {
  dmuCount <- nrow(dmuData$data)
  varCount <- dmuCount + dmuData$inputCount + dmuData$outputCount
  set.type(model, columns = (dmuData$inputCount + dmuData$outputCount + 1):varCount, type="binary")
}
