calculateRanksForAll = function (dmuData) {
  dmuCount = nrow(dmuData$data)
  result <- array(0, dim=c(dmuCount, 2))
  
  for(i in 1:nrow(dmuData$data)) {
    modelMin <-createProblemModel(dmuData, i, "min")
    modelMax <- createProblemModel(dmuData, i, "max")
    result[i,1] <- round(get.objective(modelMin)) + 1
    result[i,2] <- round(get.objective(modelMax)) + 1
    rm(modelMin)
    rm(modelMax)
  }
  return (result)
}


createProblemModel = function(dmuData, subjectDmuId, direction) 
{
  vCount <- ncol(dmuData$data) + nrow(dmuData$data)
  lprec <- make.lp(0, vCount)
  lp.control(lprec, sense=direction)
  setVariablesTypes(lprec, dmuData)
  createProblemObjective(lprec, dmuData, subjectDmuId)
  createConstraints(lprec, dmuData, subjectDmuId, direction)
  solve(lprec)
  return(lprec)
}

createProblemObjective = function(model, dmuData, subjectDmuId)
{
  vCount <- ncol(dmuData$data) + nrow(dmuData$data)
  
  objective <- array(0, dim=vCount)
  for(i in (ncol(dmuData$data) + 1) : vCount)
  {
    if(i != (ncol(dmuData$data) + subjectDmuId))
    {
      objective[i] <- 1
    }
  }
  set.objfn(model, objective)
  
}

createConstraints = function(model, dmuData, subjectDmuId, direction)
{
  createSubjectConstraints(model, dmuData, subjectDmuId, direction)
  if(dmuData$withWeightConstraints)
  {
	source("weightConstraintsHierarchicalDEA.R")
	createWeightsConstraints(model, dmuData, ncol(dmuData$data) + nrow(dmuData$data))
	}
}

createSubjectConstraints = function(model, dmuData, subjectDmuId, direction)
{
  CONST <- 10000
  vCount <- ncol(dmuData$data) + nrow(dmuData$data)
  #constraint = array(0, dim=vCount)
  #for(j in 1 : dmuData$inputCount)
  #{
  #  constraint[j] <- dmuData$data[subjectDmuId, j]
  #}
  #add.constraint(model, constraint, "=", 1)
  
  constraint = array(0, dim=vCount)
  for(j in 1:ncol(dmuData$data))
  {
    constraint[j] <- dmuData$data[subjectDmuId, j]
  }
  add.constraint(model, constraint, "=", 1)
  
  for(i in 1 : nrow(dmuData$data))
  {
    if(i != subjectDmuId)
    {
      constraint = array(0, dim=vCount)
      for(j in 1 : ncol(dmuData$data))
      {
        #if(j <= dmuData$inputCount){
        #  constraint[j] <- -dmuData$data[i,j]
        #}else{
          constraint[j] <- dmuData$data[i,j]
        #}
      }
      constraint[ncol(dmuData$data) + i] <- -CONST
      if(direction == "min")
      {
        add.constraint(model, constraint, "<=", 1)
      }
      else
      {
        add.constraint(model, constraint, ">=", (-CONST + 1))
      }
    }
  }
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

setVariablesTypes = function (model, dmuData) {
  dmuCount <- nrow(dmuData$data)
  varCount <- dmuCount + ncol(dmuData$data)
  set.type(model, columns = (ncol(dmuData$data) + 1):varCount, type="binary")
}
