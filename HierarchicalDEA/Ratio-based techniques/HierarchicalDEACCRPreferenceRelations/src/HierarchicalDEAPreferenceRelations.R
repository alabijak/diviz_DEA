calculatePreferenceRelationsForAll = function(dmuData)
{
  dmuCount <-nrow(dmuData$data)
  necessaryDominances <- list()
  possibleDominances <- list()
  dominanceMatrix <- array("0", dim = c(dmuCount, dmuCount))
  for(i in 1:dmuCount)
  {
    for(j in 1:dmuCount)
    {
      if(i == j)
      {
        dominanceMatrix[i,j] <- 'N'
      }
      else if(checkNecessaryRelation(dmuData, i,j))
      {
        dominanceMatrix[i,j] <- 'N'
      }
      else if(checkPossibleRelation(dmuData, i,j))
      {
        dominanceMatrix[i,j] <- 'P'
      }
    }
  }
  return(dominanceMatrix)
}

checkNecessaryRelation = function(dmuData, subjectDmu, relativeDmu)
{
  model <- createProblemModel(dmuData,subjectDmu,relativeDmu, "min")
  
  result <- get.objective(model)
  if(result >= 0.99999999)
  {
    return(TRUE)
  }
  return(FALSE)
}

checkPossibleRelation = function(dmuData, subjectDmu, relativeDmu)
{
  model <- createProblemModel(dmuData,subjectDmu,relativeDmu, "max")
  
  result <- get.objective(model)
  if(result >= 0.99999999)
  {
    return(TRUE)
  }
  return(FALSE)
  
}

createProblemModel = function(dmuData, subjectDmuId,relativeDmuId,direction) 
{
  vCount <- ncol(dmuData$data)
  lprec <- make.lp(0, vCount)
  lp.control(lprec, sense=direction)
  createProblemObjective(lprec, dmuData, subjectDmuId)
  createConstraints(lprec, dmuData, subjectDmuId,relativeDmuId)
  solve(lprec)
  return(lprec)
}

createProblemObjective = function(model, dmuData, subjectDmuId)
{
  vCount <- ncol(dmuData$data)
  
  objective <- array(0, dim=vCount)
  for(i in 1: vCount)
  {
    objective[i] <- dmuData$data[subjectDmuId,i]
  }
  set.objfn(model, objective)
  
}

createConstraints = function(model, dmuData, subjectDmuId,relativeDmuId)
{
  createSubjectConstraints(model, dmuData, subjectDmuId, relativeDmuId)
  if(dmuData$withWeightConstraints)
  {
    source("weightConstraintsHierarchicalDEA.R")
    createWeightsConstraints(model, dmuData, ncol(dmuData$data))
  }
}

createSubjectConstraints = function(model, dmuData, subjectDmuId, relativeDmu)
{
  vCount <- ncol(dmuData$data)
  
  constraint = array(0, dim=vCount)
  for(i in 1 : vCount)
  {
      constraint[i] <- dmuData$data[relativeDmu,i]
  }
  add.constraint(model, constraint, "=", 1)
  
}

findChildren = function(dmuData, critId)
{
  result = NULL
  for(i in 1 : length(dmuData$criteria$ids))
  {
    actParent <- dmuData$criteria$parents[i][1]
    if(actParent == critId)
    {
      children = findChildren(dmuData,dmuData$criteria$ids[i])
      if(!is.null(children))
      {
        result = rbind(result, children)
      }
      else
      {
        result = rbind(result, c(dmuData$criteria$ids[i]))
      }
    }
  }
  if(is.null(result))
  {
    result = rbind(result, c(critId))
  }
  return(result)
}

calculateWeights = function(model, dmuData)
{
  var <- get.variables(model)
  criteria = data$criteria
  weights = array(0, dim=length(criteria$ids))
  for(i in 1:length(criteria$ids))
  {
    criteriumId = criteria$ids[i]
    children = findChildren(dmuData, criteriumId)
    
    for(j in 1 : length(children))
    {
      leafs = dmuData$criteria$leafs
      id = which(leafs == children[j])
      weights[i] = weights[i] + var[id]
    }
    
    
    id = which(dmuData$criteria$ids == criteriumId)
    children = findChildren(dmuData, dmuData$criteria$parents[id])
    sum = 0
    for(j in 1 : length(children))
    {
      leafs = dmuData$criteria$leafs
      id = which(leafs == children[j])
      sum = sum + var[id]
    }
    weights[i] = weights[i] / sum
  }
  return(weights)
}
