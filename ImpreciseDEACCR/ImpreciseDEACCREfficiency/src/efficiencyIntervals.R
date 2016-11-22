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
  result <- -get.objective(modelEff)
  rm(modelEff)
  
  return (result)
}

calculateEfficiencyForAll = function (dmuData) {
  dmuCount = nrow(dmuData$data)
  result <- array(0, dim=c(dmuCount,1))
  
  
  for(i in 1:dmuCount) {
    dmuData <-recalculatePerformance(dmuData, i, "opt")
    result[i] <- calculateEfficiency(dmuData, i)
  }
  
  return (result)
}

calculatePesEfficiencyForAll = function (dmuData) {
  dmuCount = nrow(dmuData$data)
  result <- array(0, dim=c(dmuCount,1))
  
  for(i in 1:dmuCount) {
    dmuData <-recalculatePerformance(dmuData, i, "pes")
    result[i] <- calculateEfficiency(dmuData, i)
  }
  
  return (result)
}

createProblemModel = function (subjectDmuIdx, dmuData) {
  variablesCount = dmuData$inputCount + dmuData$outputCount;
  lprec <- make.lp(0, variablesCount)
  createProblemObjective(lprec, subjectDmuIdx, dmuData)
  createConstraints(lprec, subjectDmuIdx, dmuData)
  solve(lprec)
  return (lprec)
}

createProblemObjective = function(model, subjectDmuIdx, dmuData) {
  sign = -1;
  varCount = dmuData$inputCount + dmuData$outputCount
  objective <-  array(0, dim=varCount)
  for(i in (dmuData$inputCount+1):(varCount)) {
    objective[i] <- sign * dmuData$data[subjectDmuIdx, i]
  }
  set.objfn(model, objective)
  add.constraint(model, objective, ">=", -1)
}

createConstraints = function(model, subjectDmuIdx, dmuData) {
  createSubjectConstraints(model, subjectDmuIdx, dmuData)
  createOtherConstraints(model, subjectDmuIdx, dmuData)
  if(dmuData$withWeightConstraints == TRUE) {
    source("weightConstraints.R")
    createWeightConstraints(model, dmuData)
  }
}

#subject DMU constraints
createSubjectConstraints = function(model, subjectDmuIdx, dmuData) {
  varCount <-  dmuData$inputCount + dmuData$outputCount
  result <- array(0, dim = varCount)
  
  for (i in 1 : dmuData$inputCount){
    result[i] <- dmuData$data[subjectDmuIdx, i]  
  }
  add.constraint(model, result, "=", 1)
}

createOtherConstraints = function(model, subjectDmuIdx, dmuData) {
  data <- dmuData$data
  dmuCount = nrow(data)
  inputCount = dmuData$inputCount
  outputCount = dmuData$outputCount
  varCount <- inputCount + outputCount
  constrCount <- dmuCount
  result <- array(0, dim = c(constrCount,varCount))
  
  for (i in 1:constrCount) {
    if(i != subjectDmuIdx) {
      for(j in 1 : varCount) {
        if (j <= inputCount) {
          result[i, j] <- -data[i, j]
        } else if (j <= inputCount + outputCount) {
          result[i, j] <- data[i, j]
        } 
      } 
      
      sign = "<="
      add.constraint(model, result[i,], sign, 0)
    }
  }  
}

