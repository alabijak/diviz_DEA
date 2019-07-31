createWeightsConstraints = function(model, dmuData, varCount)
{
  constraintTemplates = dmuData$weightConstraints
  constraintCount = length(dmuData$weightConstraints)
  
  critCount <- length(dmuData$hierarchy)
  for(i in 1 : constraintCount)
  {
    operator = constraintTemplates[[i]]$operator
    rhs = constraintTemplates[[i]]$rhs
    constraint = array(0, dim=varCount)
    criteriumId <- -1
    for(k in 1:critCount)
    {
      if(constraintTemplates[[i]]$constraint[k] != 0)
      {
        #children = findChildren(dmuData, dmuData$criteria$ids[k])
        children <- dmuData$hierarchy[[k]]$criteria

        for(j in 1 : length(children))
        {
          #leafs = dmuData$criteria$leafs
          id = which(dmuData$critIDs == children[j])
          constraint[id] <- constraint[id] + constraintTemplates[[i]]$constraint[k]
        }
        criteriumId = k
      }
      
    }
    
     children = dmuData$hierarchy[[dmuData$hierarchy[[criteriumId]]$parent]]$criteria #findChildren(dmuData, dmuData$criteria$parents[criteriumId])
    for(j in 1 : length(children))
    {
      #leafs = dmuData$criteria$leafs
      id = which(dmuData$critIDs == children[j])
      constraint[id] <- constraint[id] - rhs
    }
    
    add.constraint(model, constraint, operator, 0)
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
