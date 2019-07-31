#result : lowerBound, upperBound, d_opt, d_pes

calculateEfficiencyBounds = function (dmuData, subjectDmu, nodeId) {
  result <- array(0, dim=4)
  source("efficiencyBounds-hierarchy.R")
  maxDist <- calculateMaxDistance(dmuData, subjectDmu, nodeId)
  minDist <- calculateMinDistance(dmuData, subjectDmu, nodeId)
  
  source("minmaxEfficiency-hierarchy.R")
  minmaxEff <- calculateMinMaxEfficiency(dmuData, subjectDmu, nodeId)
  result[1] <- minmaxEff[1]
  result[2] <- minmaxEff[2]
  result[3] <- minDist
  result[4] <- maxDist
  return (result)
}

calculateEfficiencyBoundsForAll = function (dmuData, transformToUtilities=TRUE) {
  dmuCount = nrow(dmuData$data)
  result <- c()
  if(transformToUtilities == TRUE) {
    dmuData <- transformToUtilityValues(dmuData)
  }
  nodeResult <- c()
  for(i in 1:dmuCount) {
    nodeResult <- rbind(nodeResult, calculateEfficiencyBounds(dmuData, i, dmuData$hierarchyNode))
  }
  return (nodeResult)
}

calculateMaxDistance = function (dmuData, subjectDmuIdx, nodeId) {
  modelMax <- createProblemModel(subjectDmuIdx, dmuData, nodeId)
  result <- get.objective(modelMax)
  rm(modelMax)
  return (result)
}

createProblemModel = function (subjectDmuIdx, dmuData, nodeId) {
  dmuCount <- nrow(dmuData$data)
  critCount <- length(dmuData$hierarchy[[nodeId]]$criteria)
  variablesCount <- dmuCount +  2 * critCount + 1
  
  lprec <- make.lp(0, variablesCount)
  lp.control(lprec,sense='max')

  createProblemObjective(lprec, variablesCount)
  createConstraints(lprec, subjectDmuIdx, dmuData, nodeId)
  setVariablesTypes(lprec, dmuData, nodeId)
  
  solve(lprec)
  return (lprec)
}

createProblemObjective = function(model, varCount) {
  objective <- array(0, dim=varCount)
  objective[varCount] <- 1
  
  set.objfn(model, objective)
}

createConstraints = function(model, subjectDmuIdx, dmuData, nodeId) {
  
  critCount <- length(dmuData$hierarchy[[nodeId]]$criteria)
  varCount <- nrow(dmuData$data) +  2 * critCount + 1
  
  createSubjectConstraints(model, subjectDmuIdx, dmuData, nodeId)
  createOtherConstraints(model, subjectDmuIdx, dmuData, nodeId)
  if(dmuData$withWeightConstraints == TRUE) {
    source("weightConstraintsHierarchicalDEA.R")
    createWeightsConstraints(model, dmuData, varCount)
  }
}


#subject DMU's constraints- worstRank
createSubjectConstraints = function(model, subjectDmuIdx, dmuData, nodeId) {
  CONST <- 100
  dmuCount <- nrow(dmuData$data)
  
  weightsCount <- length(dmuData$hierarchy[[nodeId]]$criteria)
  varCount <- dmuCount +  2 * weightsCount + 1
  
  constrCount <- dmuCount
  result <- array(0, dim = c(constrCount,varCount))
  for (i in 1:constrCount) {
    #if(i != subjectDmuIdx) {
    for(j in 1 : varCount) {
      if(i == j - 2 * weightsCount) {
        result[i,j] <- CONST
      } else if (j <= weightsCount) {
        critNo <- which(dmuData$critIDs == dmuData$hierarchy[[nodeId]]$criteria[j])
        result[i, j] <- -dmuData$data[subjectDmuIdx, critNo]
      } else if (j <= 2 * weightsCount) {
        critNo <- which(dmuData$critIDs == dmuData$hierarchy[[nodeId]]$criteria[j - weightsCount])
        result[i, j] <- dmuData$data[i, critNo]
      } else if (j == varCount){
        result[i, j] <- -1 
      } 
    } 
    add.constraint(model, result[i,], ">=", 0)
  }
  #}   
}


#wagi "sztucznego" DMU musza byc takie same jak wagi DMU obliczanego, suma wag = 1
createOtherConstraints = function(model, subjectDmuIdx, dmuData, nodeId) {
  data <- dmuData$data
  inputCount <- dmuData$inputCount
  outputCount <- dmuData$outputCount
  dmuCount = nrow(data)
  weightsCount <- length(dmuData$hierarchy[[nodeId]]$criteria)
  varCount <- dmuCount +  2 * weightsCount + 1
  constrCount <- weightsCount + 2
  result <- array(0, dim = c(constrCount,varCount))
  sign <- "="
  for (i in 1:(constrCount-2)) {
    critNo <- which(dmuData$critIDs == dmuData$hierarchy[[nodeId]]$criteria[i])
    result[i,i] <- -1
    result[i, i + weightsCount] <- 1
    add.constraint(model, result[i,], sign, 0)
  }
  for(i in 1:weightsCount) {
    result[constrCount - 1, i] <- 1
  }
  add.constraint(model, result[constrCount-1,], sign, 1)
  
  for(i in 1:dmuCount) {
    result[constrCount, i + 2 * weightsCount] <- 1
  }
  add.constraint(model, result[constrCount,], "<=", dmuCount-1)
}

setVariablesTypes = function (model, dmuData, nodeId) {
  dmuCount <- nrow(dmuData$data)
  weightsCount <- length(dmuData$hierarchy[[nodeId]]$criteria)
  varCount <- dmuCount +  2 * weightsCount + 1
  set.type(model, columns = (varCount - dmuCount):(varCount-1), type="binary")
}

calculateMinDistance = function (dmuData, subjectDmu, nodeId) {
  modelEff <- createMinDistanceModel(subjectDmu, dmuData, nodeId)
  
  result <- get.objective(modelEff)
  rm(modelEff)
  return (result)
}

createMinDistanceModel = function (subjectDmuIdx, dmuData, nodeId) {
  variablesCount = length(dmuData$hierarchy[[nodeId]]$criteria) + 1
  lprec <- make.lp(ncol=variablesCount)
  lp.control(lprec,sense='min')
  
  createProblemObjective(lprec, variablesCount)
  createMinDistanceConstraints(lprec, subjectDmuIdx, dmuData, nodeId)
  solve(lprec)
  return (lprec)
}

createMinDistanceConstraints = function(model, subjectDmuIdx, dmuData, nodeId) {
  createMinDistancwSubjectConstraints(model, subjectDmuIdx, dmuData, nodeId)
  
  if(dmuData$withWeightConstraints == TRUE) {
    source("weightConstraintsHierarchicalDEA.R")
    createWeightsConstraints(model, dmuData, length(dmuData$hierarchy[[nodeId]]$criteria) + 1)
  }
}

createMinDistancwSubjectConstraints = function(model, subjectDmuIdx, dmuData, nodeId) {
  varCount <-  length(dmuData$hierarchy[[nodeId]]$criteria) + 1
  dmuCount = nrow(dmuData$data)
  
  for(i in 1:dmuCount)
  {
    if(i != subjectDmuIdx)
    {
      constraint <- array(0, dim = varCount)
      constraint[varCount] <- -1
      for(j in 1:(varCount - 1))
      {
        critNo = which(dmuData$critIDs == dmuData$hierarchy[[nodeId]]$criteria[j])
        constraint[j]<-dmuData$data[i,critNo] - dmuData$data[subjectDmuIdx,critNo] 
      }
      add.constraint(model, constraint, "<=", 0)
    }
  }
  constrint = array(0, dim = varCount)
  constraint[varCount] <- 0
  for(j in 1:(varCount - 1))
  {
    constraint[j]<-1
  }
  add.constraint(model, constraint, "=", 1)
}

transformToUtilityValues = function(dmuData)
{
  dmuCount <- nrow(dmuData$data) 
  weightsCount <- ncol(dmuData$data)
  boundaries <- dmuData$boundaries
  for(i in 1:dmuCount) {
    for(j in 1:weightsCount) {
      if(j <= dmuData$inputCount)
        dmuData$data[i,j] <- (boundaries$up[j] - dmuData$data[i,j])/(boundaries$up[j] - boundaries$low[j])
      else 
        dmuData$data[i,j] <- (dmuData$data[i,j] - boundaries$low[j])/(boundaries$up[j] - boundaries$low[j])
    }
  }
  return(dmuData)
}




