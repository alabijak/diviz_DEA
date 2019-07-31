calculateMinMaxEfficiency = function (dmuData, subjectDmu,nodeId) {
  minModelEff <- createProblemModel("min", subjectDmu, dmuData, nodeId)
  maxModelEff <- createProblemModel("max", subjectDmu, dmuData, nodeId)
  
  result <- array(0, dim=2)
  result[1] <- get.objective(minModelEff)
  result[2] <- -get.objective(maxModelEff)
  rm(minModelEff)
  rm(maxModelEff)
  return (result)
}

calculateWeights = function(dmuData, subjectDmu) {
  modelEff <- createProblemModel(subjectDmu, dmuData)
  weights <- get.variables(modelEff)
  return (weights)
}

createProblemModel = function (dir, subjectDmuIdx, dmuData, nodeId) {
  variablesCount <- length(dmuData$hierarchy[[nodeId]]$criteria)
  lprec <- make.lp(0, variablesCount)
  
  createProblemObjective(dir,lprec, subjectDmuIdx, dmuData, nodeId)
  createConstraints(lprec, subjectDmuIdx, dmuData, nodeId)
  solve(lprec)
  return (lprec)
}

createProblemObjective = function(dir, model, subjectDmuIdx, dmuData, nodeId) {
  sign <- 1
  if(dir == "max") {
    sign <- -1
  } 
  varCount <- length(dmuData$hierarchy[[nodeId]]$criteria)
  objective <-  array(0, dim=varCount)
  for(i in 1:varCount) {
    critNo = which(dmuData$critIDs == dmuData$hierarchy[[nodeId]]$criteria[i])
    objective[i] <- sign * dmuData$data[subjectDmuIdx, i]
  }
  set.objfn(model, objective)
}

createConstraints = function(model, subjectDmuIdx, dmuData, nodeId) {
  createOtherConstraints(model, subjectDmuIdx, dmuData, nodeId)
  varCount <- length(dmuData$hierarchy[[nodeId]]$criteria)
  
  if(dmuData$withWeightConstraints == TRUE) {
    source("weightConstraintsHierarchicalDEA.R")
    createWeightsConstraints(model, dmuData, varCount)
  }
}

createOtherConstraints = function(model, subjectDmuIdx, dmuData, nodeId) {
  varCount <- length(dmuData$hierarchy[[nodeId]]$criteria)
  result <- array(1, dim = varCount)
  add.constraint(model, result, "=" , 1)
}
