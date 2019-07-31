calculateEfficiencyForAll = function(dmuData)
{
  dmuCount <-nrow(dmuData$data)
  result <- array(0, dim=c(dmuCount))
  for(i in 1:dmuCount)
  {
    model <- createProblemModel(dmuData,i)
    
    result[i] = get.objective(model)
    # print(get.variables(model))
    
  }
  return(result)
}

createProblemModel = function(dmuData, subjectDmuId) 
{
  vCount <- ncol(dmuData$data)
  lprec <- make.lp(0, vCount)
  lp.control(lprec, sense="max")
  createProblemObjective(lprec, dmuData, subjectDmuId)
  createConstraints(lprec, dmuData, subjectDmuId)
  solve(lprec)
  return(lprec)
}

createProblemObjective = function(model, dmuData, subjectDmuId)
{
  vCount <- ncol(dmuData$data)
  
  objective <- array(0, dim=vCount)
  for(i in 1 : vCount)
  {
    objective[i] <- dmuData$data[subjectDmuId,i]
  }
  set.objfn(model, objective)
  
}

createConstraints = function(model, dmuData, subjectDmuId)
{
  createSubjectConstraints(model, dmuData, subjectDmuId)
  if(dmuData$withWeightConstraints)
  {
    source("weightConstraintsHierarchicalDEA.R")
    createWeightsConstraints(model, dmuData, ncol(dmuData$data))
  }
}

createSubjectConstraints = function(model, dmuData, subjectDmuId)
{
  vCount <- ncol(dmuData$data)
#   constraint = array(0, dim=vCount)
#   for(i in 1 : dmuData$inputCount)
#   {
#     constraint[i] <-dmuData$data[subjectDmuId,i]
#   }
#   add.constraint(model, constraint, "=", 1)
  for(i in 1 : nrow(dmuData$data))
  {
    constraint = array(0, dim=vCount)
    for(j in 1 : vCount)
    {
#       if(j <= dmuData$inputCount){
#         constraint[j] <- -dmuData$data[i,j]
#       }
      #else
      #{
        constraint[j] <- dmuData$data[i,j]
      #}
    }
    add.constraint(model, constraint, "<=", 1)
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
