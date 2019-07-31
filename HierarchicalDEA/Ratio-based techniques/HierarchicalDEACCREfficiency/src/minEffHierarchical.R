#problem :
# v1 v2 ..vm | u1 u2 .. un u(n+1) 
#result[i] :
#efficiency_i

calculateEfficiencyForAll = function(dmuData)
{
  dmuCount <-nrow(dmuData$data)
  result <- array(0, dim=c(dmuCount))
  for(i in 1:dmuCount)
  {
    source("minEffHierarchical.R")
    model <- createProblemModel(dmuData,i)
    
    result[i] = get.objective(model)
    #var <- get.variables(model)
    #weights = calculateWeights(model, dmuData)
  }
  return(result)
}

createProblemModel = function(dmuData, subjectDmuId) 
{
  vCount <- ncol(dmuData$data) + nrow(dmuData$data)
  lprec <- make.lp(0, vCount)
  lp.control(lprec, sense="min")
  setVariablesTypes(lprec, dmuData)
  
  createProblemObjective(lprec, dmuData, subjectDmuId)
  createConstraints(lprec, dmuData)
  solve(lprec)
  return(lprec)
}

createProblemObjective = function(model, dmuData, subjectDmuId)
{
  vCount <- ncol(dmuData$data) + nrow(dmuData$data)
  
  objective <- array(0, dim=vCount)
  for(i in 1 : ncol(dmuData$data))
  {
    objective[i] <- dmuData$data[subjectDmuId,i]
  }
  set.objfn(model, objective)
  
}

createConstraints = function(model, dmuData)
{
  createSubjectConstraints(model, dmuData)
  if(dmuData$withWeightConstraints)
  {
	source("weightConstraintsHierarchicalDEA.R")
	createWeightsConstraints(model, dmuData, ncol(dmuData$data) + nrow(dmuData$data))
  }
}

createSubjectConstraints = function(model, dmuData)
{
  CONST <- 10000
  vCount <- ncol(dmuData$data) + nrow(dmuData$data)
  for(i in 1 : nrow(dmuData$data))
  {
    constraint = array(0, dim=vCount)
    for(j in 1 : ncol(dmuData$data))
    {
        constraint[j] <- dmuData$data[i,j]
    }
    
    constraint[ncol(dmuData$data) + i] <- -CONST
      
    
    add.constraint(model, constraint, ">=", (-CONST+1))
  }
  constraint = array(0, dim=vCount)
  for(i in (ncol(dmuData$data) + 1) : vCount)
  {
    constraint[i] <- 1
  }
  add.constraint(model, constraint, ">=", 1)
}

setVariablesTypes = function (model, dmuData) {
  dmuCount <- nrow(dmuData$data)
  varCount <- dmuCount + ncol(dmuData$data)
  set.type(model, columns = (ncol(dmuData$data) + 1):varCount, type="binary")
}