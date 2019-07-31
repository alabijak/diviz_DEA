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
        children <- dmuData$hierarchy[[k]]$criteria

        for(j in 1 : length(children))
        {
          id = which(dmuData$critIDs == children[j])
          constraint[id] <- constraint[id] + constraintTemplates[[i]]$constraint[k]
        }
        criteriumId = k
      }
      
    }
    
    children = dmuData$hierarchy[[dmuData$hierarchy[[criteriumId]]$parent]]$criteria
    for(j in 1 : length(children))
    {
      id = which(dmuData$critIDs == children[j])
      constraint[id] <- constraint[id] - rhs
    }
    
    add.constraint(model, constraint, operator, 0)
  }
}