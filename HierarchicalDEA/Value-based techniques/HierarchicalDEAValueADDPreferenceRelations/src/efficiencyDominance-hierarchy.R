
#result : N(necesssary)/P(possible)/0(not dominating)


calculateEfficiencyDominanceForAll = function(dmuData, transformToUtilities=TRUE){
  dmuCount = nrow(dmuData$data)
  
  if(transformToUtilities == TRUE) {
    dmuData <- transformToUtilityValues(dmuData)
  }
  
  nodeResult <- array(0, dim=c(dmuCount,dmuCount))
  
  for(i in 1:dmuCount) {
    for(j in 1:dmuCount) {
      if(i != j) {
        nodeResult[i,j] = calculateEfficiencyDominance(dmuData, i, j, dmuData$hierarchyNode)
      } else {
        nodeResult[i,j] = 'N'
      }
    }
  }
  
  return (nodeResult)
}

calculateEfficiencyDominance = function(dmuData, subjectDmu, relativeDmu, nodeId) {
  
  modelNecessary <- createProblemModel(dmuData, subjectDmu, relativeDmu, "N", nodeId)
  modelPossible <- createProblemModel(dmuData, subjectDmu, relativeDmu, "P", nodeId)
  necessary = get.objective(modelNecessary)
  possible = -get.objective(modelPossible)
  result = 0
  
  if(necessary >= 0) {
    result = "N"
  } else if (possible >= 0) {
    result = "P"
  }
  
  return (result)
}

createProblemModel = function (dmuData, subjectDmuIdx, relativeDmuIdx, dominanceType, nodeId) {
  weightsCount <- length(dmuData$hierarchy[[nodeId]]$criteria)
  variablesCount = 2 *  weightsCount + 1
  lprec <- make.lp(0, variablesCount)
  createProblemObjective(lprec, dmuData, dominanceType, nodeId)
  createConstraints(lprec, subjectDmuIdx, relativeDmuIdx, dmuData, dominanceType, nodeId)
  solve(lprec)
  return (lprec)
}

createProblemObjective = function(model,dmuData, dominanceType, nodeId) {
  weightsCount <- length(dmuData$hierarchy[[nodeId]]$criteria)
  varCount <- 2 *  weightsCount + 1
  objective <-  array(0, dim=varCount)
  sign = 1
  if(dominanceType == "P") {
    sign = -1
  }
  objective[varCount] <- sign
  set.objfn(model, objective)
}

createConstraints = function(model, subjectDmuIdx, relativeDmuIdx, dmuData, dominanceType, nodeId) {
  weightsCount <- length(dmuData$hierarchy[[nodeId]]$criteria)
  varCount <- 2 *  weightsCount + 1
  
  createSubjectConstraints(model, subjectDmuIdx, relativeDmuIdx, dmuData, dominanceType, nodeId)
  createOtherConstraints(model, subjectDmuIdx, dmuData, nodeId)
  if(dmuData$withWeightConstraints == TRUE) {
    source("weightConstraintsHierarchicalDEA.R")
    createWeightsConstraints(model, dmuData, varCount)
  }
}

#subject DMU constraints
createSubjectConstraints = function(model, subjectDmuIdx, relativeDmuIdx, dmuData, dominanceType, nodeId) {
  data <- dmuData$data
  
  dmuCount = nrow(data)
  weightsCount <- length(dmuData$hierarchy[[nodeId]]$criteria)
  varCount <- 2 *  weightsCount + 1
  
  constrCount <- 1
  result <- array(0, dim = varCount)
  
  sign = 1
  if(dominanceType == "P") {
    sign = -1
  }
  
  for(j in 1:varCount) {
    if (j <= weightsCount) {
      critNo <- which(dmuData$critIDs == dmuData$hierarchy[[nodeId]]$criteria[j])
      result[j] <- sign * data[subjectDmuIdx, critNo]
    } else if (j <= varCount - 1) {
      critNo <- which(dmuData$critIDs == dmuData$hierarchy[[nodeId]]$criteria[j - weightsCount])
      result[j] <- -sign * data[relativeDmuIdx, critNo]
    } else {
      result[j] <- -sign
    }
  } 
  sign = "<="
  add.constraint(model, result, sign, 0)
  
}

#wagi "sztucznego" DMU musza byc takie same jak wagi DMU obliczanego, suma wag = 1
createOtherConstraints = function(model, subjectDmuIdx, dmuData, nodeId) {
  data <- dmuData$data
  dmuCount = nrow(data)
  
  weightsCount <- length(dmuData$hierarchy[[nodeId]]$criteria)
  varCount <- 2 *  weightsCount + 1
  
  constrCount <- weightsCount + 1
  result <- array(0, dim = c(constrCount,varCount))
  sign <- "="
  for (i in 1:(constrCount-1)) {
    result[i,i] <- -1
    result[i, i + weightsCount] <- 1
    add.constraint(model, result[i,], sign, 0)
  }
  for(i in 1:weightsCount) {
    result[constrCount, i] <- 1
  }
  add.constraint(model, result[constrCount,], sign, 1)
  set.bounds(model, lower = -Inf, columns = varCount)
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

