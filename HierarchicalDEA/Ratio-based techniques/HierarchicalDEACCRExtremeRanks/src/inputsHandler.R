checkAndExtractInputs <- function(xmcdaData, programExecutionResult) { # TODO
  altIDs<- c()
  for(i in as.list(xmcdaData$alternatives$getIDs()))
  {
    altIDs <- c(altIDs, i$toString())
  }
  
  performanceTables <- getNumericPerformanceTableList(xmcdaData)
  performance <- performanceTables[[1]]
  
  hierarchy <- parseHierarchy(xmcdaData, colnames(performance))
  
  weightConstraints <- list()
  withWeightConstraints <- FALSE
  constraints <-as.list(xmcdaData$criteriaLinearConstraintsList)
  for(constraint in constraints) {
    for(i in 1:constraint$size()) {
      weightConstraints[[i]] <- getWeightConstraint(constraint$get(as.integer(i-1)), hierarchy)
    }
    withWeightConstraints <- TRUE
  }
  
  hierarchyNode <- "root"
  parameters <- as.list(xmcdaData$programParametersList)
  if(length(parameters) > 0)
  {
    parameters <- parameters[[1]]
    param <- parameters$getParameter("hierarchyNode")
    if(!is.null(param)){
      hierarchyNode <- param$getValues()$get(as.integer(0))$getValue()
      if(!hierarchyNode %in% names(hierarchy))
        stop(paste("Given hierarchy node: \"", hierarchyNode,"\" does not exist in hierarchy", sep=""))
    }
  }
  node <- hierarchy[[hierarchyNode]]
  performance <- performance[, c(node$criteria)]
    
  result <- list(data=performance,
                 inputCount=0,
                 outputCount=ncol(performance),
                 weightConstraints = weightConstraints,
                 withWeightConstraints = withWeightConstraints,
                 altIDs = altIDs,
                 hierarchy = hierarchy,
                 critIDs=node$criteria,
                 hierarchyNode=hierarchyNode)
  if(result$withWeightConstraints){
    result <- removeUnnecessaryConstraints(result)
    result$withWeightConstraints <- length(result$weightConstraints) > 0
  }
  return (result)
}



parseHierarchy <- function(xmcdaData, criteriaIds){
  xmcdaHierarchy <- as.list(xmcdaData$criteriaHierarchiesList)[[1]]
  hierarchy <- list()
  for(root in as.list(xmcdaHierarchy$getRootNodes())){
    hierarchy <- parseNode(root, "root", hierarchy, criteriaIds)
  }
  hierarchy[["root"]] <-list(id = "root", parent = "-1", criteria = criteriaIds, isLeaf=FALSE)
  
  return(hierarchy)
}

parseNode = function(node, parent, hierarchy, allCriteriaIds)
{
  criteriaIds <- c()
  id <- node$getCriterion()$id()
  
  subNodes = as.list(node$getChildren())
  if(length(subNodes) > 0)
  {
    for(subNode in subNodes)
    {
      hierarchy <- parseNode(subNode, id, hierarchy, allCriteriaIds)
      criteriaIds <- append(criteriaIds, hierarchy[[length(hierarchy)]]$criteria)
    }
  }
  else{
    criteriaIds <- append(criteriaIds, id)
  }
  isLeaf <- length(subNodes) == 0
  if(isLeaf && !id %in% allCriteriaIds)
    stop(paste("Leaf hierarchy node does not exist in inputs/outputs scales file, criteron:", id))
  hierarchy[[id]] <- list(id = id, parent = parent, criteria = criteriaIds, isLeaf=isLeaf)
  return(hierarchy)
}

getWeightConstraint <- function(constraint, hierarchy) {
  varCount <- length(hierarchy)
  elements <- as.list(constraint$getElements())
  operator <- constraint$getOperator()$toString()
  rhs <- constraint$getRhs()
  weightConstraint <- array(0,dim=varCount)  
  
  for(element in elements) {
    if(is.null(element$getUnknown()))
      stop(paste("Invalid constraint definition. Constraint contains criterion which does not exist in hierarchy."))
    critID <- element$getUnknown()$id()
    critIdx <- which(names(hierarchy) == critID) 
    if(length(critIdx) != 1)
      stop(paste("Invalid constraint definition. Constraint contains criterion which does not exist in hierarchy."))
    value <- as.double(element$getCoefficient()$getValue())
    weightConstraint[critIdx] <- value
  }
  if(operator == "EQ") {
    operator <- "="
  }
  if(operator == "LEQ") {
    operator <- "<="
  }
  if(operator == "GEQ") {
    operator <- ">="
  }
  weightConstraintData <- list(constraint = weightConstraint, operator=operator, rhs=as.double(rhs$getValue()))
  if(!validateContraint(hierarchy, weightConstraint))
    stop("Invalid constraint definition. All criteria in one constraint must have common parent node.")
  return (weightConstraintData)
}

removeUnnecessaryConstraints <- function(dmuData){
  toRemove <- c()
  
  critCount <- length(dmuData$hierarchy)
  nodeId <- dmuData$hierarchyNode
  node <- dmuData$hierarchy[[nodeId]]
  
  for(i in 1:length(dmuData$weightConstraints))
  {
    template <- dmuData$weightConstraints[[i]]
    for(k in which(template$constraint != 0))
    {
      children <- dmuData$hierarchy[[k]]$criteria
      
      if(dmuData$hierarchy[[k]]$id == nodeId || !all(children %in% node$criteria)){
        toRemove <- c(toRemove, i)
        break
      }
    }
  }
  toRemove <- rev(toRemove)
  for(idx in toRemove){
    dmuData$weightConstraints[[idx]] <- NULL
  }
  return(dmuData)
}

validateContraint <- function(hierarchy, constraint){
  parent <- "-1"
  for(k in which(constraint != 0)) {
    if(parent == "-1"){
      parent <- hierarchy[[k]]$parent
    }
    else if(parent != hierarchy[[k]]$parent)
      return(FALSE)
  }
  return(TRUE)
}
