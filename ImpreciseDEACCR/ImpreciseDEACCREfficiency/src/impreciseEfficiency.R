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

calculateEfficiency = function (dmuData, subjectDmu) {
  modelEff <- createProblemModel(subjectDmu, dmuData)
  result <- get.objective(modelEff)
  rm(modelEff)
  
  return (result)
}

calculateEfficiencyForAll = function (dmuData) {
  if(length(dmuData$ordinalFactors) == 0)
  {
    source("efficiencyIntervals.R")
    return(calculateEfficiencyForAll(dmuData))
  }
  
  source("impreciseEfficiency.R")
  dmuCount = nrow(dmuData$data)
  result <- array(0, dim=c(dmuCount,1))
  
  
  for(i in 1:dmuCount) {
    dmuData <-recalculatePerformance(dmuData, i, "opt")
    result[i] <- calculateEfficiency(dmuData, i)
  }
  return (result)
}


createProblemModel = function (subjectDmuIdx, dmuData) {
  dmuCount = nrow(dmuData$data)
  ordinalCount <- length(dmuData$ordinalFactors)
  variablesCount = dmuData$inputCount + dmuData$outputCount + dmuCount * ordinalCount
  lprec <- make.lp(0, variablesCount)
  lp.control(lprec, sense="max")
  createProblemObjective(lprec, subjectDmuIdx, dmuData)
  createConstraints(lprec, subjectDmuIdx, dmuData)
  solve(lprec)
  return (lprec)
}

createProblemObjective = function(model, subjectDmuIdx, dmuData) {
  dmuCount = nrow(dmuData$data)
  ordinalCount <- length(dmuData$ordinalFactors)
  varCount = dmuData$inputCount + dmuData$outputCount + dmuCount * ordinalCount
  
  objective <-  array(0, dim=varCount)
  for(i in (dmuData$inputCount+1):(dmuData$inputCount+dmuData$outputCount)) {
    if(!(i %in% dmuData$ordinalFactors))
    {
      objective[i] <- dmuData$data[subjectDmuIdx, i]
    }
    else
    {
      idx = which(dmuData$ordinalFactors == i)
      objective[dmuData$inputCount + dmuData$outputCount + (idx-1)*dmuCount + subjectDmuIdx] <- 1
    }
      
  }
  set.objfn(model, objective)
  #add.constraint(model, objective, ">=", -1)
}

createConstraints = function(model, subjectDmuIdx, dmuData) {
  dmuCount = nrow(dmuData$data)
  ordinalCount <- length(dmuData$ordinalFactors)
  varCount = dmuData$inputCount + dmuData$outputCount + dmuCount * ordinalCount
  
  createSubjectConstraints(model, subjectDmuIdx, dmuData)
  createOtherConstraints(model, subjectDmuIdx, dmuData)
  createMonotonicityConstraints(model, subjectDmuIdx, dmuData)
  if(dmuData$withWeightConstraints == TRUE) {
    source("weightConstraints.R")
    createCustomWeightConstraints(model, dmuData, varCount)
  }
}

#subject DMU constraints
createSubjectConstraints = function(model, subjectDmuIdx, dmuData) {
  dmuCount = nrow(dmuData$data)
  ordinalCount <- length(dmuData$ordinalFactors)
  varCount <- dmuData$inputCount + dmuData$outputCount + dmuCount * ordinalCount
  result <- array(0, dim = varCount)
  
  for (i in 1 : dmuData$inputCount){
    if(!(i %in% dmuData$ordinalFactors))
    {
      result[i] <- dmuData$data[subjectDmuIdx, i]
    }
    else
    {
      idx = which(dmuData$ordinalFactors == i)
      result[dmuData$inputCount + dmuData$outputCount + (idx-1)*dmuCount + subjectDmuIdx] <- 1
    }
  }
  add.constraint(model, result, "=", 1)
}

createOtherConstraints = function(model, subjectDmuIdx, dmuData) {
  data <- dmuData$data
  dmuCount = nrow(data)
  inputCount = dmuData$inputCount
  outputCount = dmuData$outputCount
  ordinalCount <- length(dmuData$ordinalFactors)
  varCount <- inputCount + outputCount + dmuCount * ordinalCount
  constrCount <- dmuCount
  result <- array(0, dim = c(constrCount,varCount))
  
  for (i in 1:constrCount) {
    #if(i != subjectDmuIdx) {
      for(j in 1 : (inputCount+outputCount)) {
        if (j <= inputCount) {
          if(!(j %in% dmuData$ordinalFactors))
          {
            result[i,j] <- -data[i, j]
          }
          else
          {
            idx = which(dmuData$ordinalFactors == j)
            result[i,dmuData$inputCount + dmuData$outputCount + (idx-1)*dmuCount + i] <- -1
          }
        } else if (j <= inputCount + outputCount) {
          if(!(j %in% dmuData$ordinalFactors))
          {
            result[i,j] <- data[i, j]
          }
          else
          {
            idx = which(dmuData$ordinalFactors == j)
            result[i,dmuData$inputCount + dmuData$outputCount + (idx-1)*dmuCount + i] <- 1
          }
        } 
      } 
      
      sign = "<="
      add.constraint(model, result[i,], sign, 0)
    }
  #}  
}

createMonotonicityConstraints = function(model, subjectDmuIdx, dmuData)
{
  eps = 1.1
  dmuCount = nrow(dmuData$data)
  ordinalCount <- length(dmuData$ordinalFactors)
  varCount <- dmuData$inputCount + dmuData$outputCount + dmuCount * ordinalCount
  
  for(factor in dmuData$ordinalFactors)
  {
    idx = which(dmuData$ordinalFactors == factor)
    criteriumValues <- array(dim=c(2, dmuCount))
    criteriumValues[1,] <- 1:dmuCount
    criteriumValues[2,] <- dmuData$data[,factor]
    criteriumValues<- criteriumValues[,order(criteriumValues[2,])]
    
    #constraint <-  array(0, dim = varCount)
    #constraint[dmuData$inputCount + dmuData$outputCount + criteriumValues[1,1]] <- 1
    #add.constraint(model, constraint, ">=", 0.01)
    
    for(i in 2:dmuCount)
    {
      constraint <- array(0, dim = varCount)
      offset <- dmuData$inputCount + dmuData$outputCount + (idx-1)*dmuCount
      
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
    
    #constraint[offset + criteriumValues[1,dmuCount]] <- 1
    #add.constraint(model, constraint, "<=", 100000)
  }
  
}
