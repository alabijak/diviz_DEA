#problem :
# u01 u02 .. u0n | u1 u2 .. un | z1 z2 .. zk
#result[i] :  
#minRank_i, maxRank_i

calculateRank = function (dmuData, transformToUtilities=TRUE) {
  if(transformToUtilities == TRUE) {
    dmuData <- transformToUtilityValues(dmuData)
  }
  dmuCount = nrow(dmuData$data)
  nodeResult <- array(0, dim=c(dmuCount, 2))
  for(i in 1:nrow(dmuData$data)) {
    modelMin <- createProblemModel("minRank", i, dmuData, dmuData$hierarchyNode)
    modelMax <- createProblemModel("maxRank", i, dmuData, dmuData$hierarchyNode)
    
    nodeResult[i,1] <- as.integer(get.objective(modelMin) + 1)
    nodeResult[i,2] <- as.integer(-get.objective(modelMax) + 1)
    rm(modelMin)
    rm(modelMax)
  }
  return (nodeResult)
}

createProblemModel = function (problemName, subjectDmuIdx, dmuData, nodeId) {
  dmuCount <- nrow(dmuData$data)
  critCount <- length(dmuData$hierarchy[[nodeId]]$criteria)
  varCount <- nrow(dmuData$data) +  2 * critCount
  lprec <- make.lp(0, varCount)
  createProblemObjective(problemName, lprec, subjectDmuIdx, dmuData, nodeId)
  createConstraints(problemName, lprec, subjectDmuIdx, dmuData, nodeId)
  setVariablesTypes(lprec, dmuData, nodeId)
  solve(lprec)
  return (lprec)
}

createProblemObjective = function(problemName, model, subjectDmuIdx, dmuData, nodeId) {
  dmuCount <- nrow(dmuData$data)
  critCount <- length(dmuData$hierarchy[[nodeId]]$criteria)
  if(problemName == "minRank") {
    value = 1
  }
  else {
    value = -1
  }
  objectiveVariables <- array(value, dim=dmuCount)
  objectiveVariables[subjectDmuIdx] = 0
  restOfVariables <- array(0, dim=2 * critCount)
  objective <- append(restOfVariables, objectiveVariables)
  set.objfn(model, objective)
}

createConstraints = function(problemName, model, subjectDmuIdx, dmuData, nodeId) {
  critCount <- length(dmuData$hierarchy[[nodeId]]$criteria)
  varCount <- nrow(dmuData$data) + 2 * critCount
  if(problemName == "minRank"){
    createSubjectConstraintsForMin(model, subjectDmuIdx, dmuData, nodeId)
  }
  else {
    createSubjectConstraintsForMax(model, subjectDmuIdx, dmuData, nodeId)
  }
  createOtherConstraints(model, subjectDmuIdx, dmuData, nodeId)
  if(dmuData$withWeightConstraints == TRUE) {
    source("weightConstraintsHierarchicalDEA.R")
    createWeightsConstraints(model, dmuData, varCount)
  }
}


#subject DMU's constraints- worstRank
createSubjectConstraintsForMax = function(model, subjectDmuIdx, dmuData, nodeId) {
  CONST <- 100
  dmuCount <- nrow(dmuData$data)
  critCount <- length(dmuData$hierarchy[[nodeId]]$criteria)
  varCount <- nrow(dmuData$data) + 2 * critCount
  
  constrCount <- dmuCount
  result <- array(0, dim = c(constrCount,varCount))
  
  for (i in 1 : constrCount) {
    if(i != subjectDmuIdx) {
      for(j in 1 : varCount) {
        if(i == j - 2 * critCount) {
          result[i,j] <- CONST
        } else if (j <= critCount) {
          critNo <- which(dmuData$critIDs == dmuData$hierarchy[[nodeId]]$criteria[j])
          result[i, j] <- dmuData$data[subjectDmuIdx, critNo]
        } else if (j <= 2 * critCount) {
          critNo <- which(dmuData$critIDs == dmuData$hierarchy[[nodeId]]$criteria[j - critCount])
          result[i, j] <- -dmuData$data[i, critNo]
        } else {
          result[i, j] <- 0
        }
      } 
      add.constraint(model, result[i,], "<=", CONST)
    }
  }   
}


#subject DMU's constraints - bestRank
createSubjectConstraintsForMin = function(model, subjectDmuIdx, dmuData, nodeId) {
  CONST <- 100
  dmuCount <- nrow(dmuData$data)
  weightsCount <- length(dmuData$hierarchy[[nodeId]]$criteria)
  varCount <- nrow(dmuData$data) + 2 * weightsCount
  
  constrCount <- dmuCount
  result <- array(0, dim = c(constrCount,varCount))
  
  for (i in 1 : constrCount) {
    if(i != subjectDmuIdx) {
      for(j in 1 : varCount) {
        if(i == j - 2 * weightsCount) {
          result[i,j] <- -CONST
        } else if (j <= weightsCount) {
          critNo <- which(dmuData$critIDs == dmuData$hierarchy[[nodeId]]$criteria[j])
          result[i, j] <- dmuData$data[i, critNo]
        } else if (j <= 2 * weightsCount) {
          critNo <- which(dmuData$critIDs == dmuData$hierarchy[[nodeId]]$criteria[j - weightsCount])
          result[i, j] <- -dmuData$data[subjectDmuIdx, critNo]
        } else {
          result[i, j] <- 0
        }
      } 
      add.constraint(model, result[i,], "<=", 0)
    }
  }   
}

#wagi "sztucznego" DMU musza byc takie same jak wagi DMU obliczanego, suma wag = 1
createOtherConstraints = function(model, subjectDmuIdx, dmuData, nodeId) {
  data <- dmuData$data
  dmuCount = nrow(data)
  weightsCount <- length(dmuData$hierarchy[[nodeId]]$criteria)
  varCount <- nrow(dmuData$data) + 2 * weightsCount
  
  constrCount <- weightsCount + 1
  result <- array(0, dim = c(constrCount,varCount))
  sign <- "="
  for (i in 1:(constrCount-1)) {
    critNo <- which(dmuData$critIDs == dmuData$hierarchy[[nodeId]]$criteria[i])
    result[i,i] <- -data[subjectDmuIdx, critNo]
    result[i, i + weightsCount] <- data[subjectDmuIdx, critNo]
    add.constraint(model, result[i,], sign, 0)
  }
  for(i in 1:weightsCount) {
    result[constrCount, i] <- 1
  }
  add.constraint(model, result[constrCount,], sign, 1)
}

setVariablesTypes = function (model, dmuData, nodeId) {
  dmuCount <- nrow(dmuData$data)
  weightsCount <- length(dmuData$hierarchy[[nodeId]]$criteria)
  varCount <- nrow(dmuData$data) + 2 * weightsCount
  
  set.type(model, columns = (varCount - dmuCount + 1):varCount, type="binary")
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

