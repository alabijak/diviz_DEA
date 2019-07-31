checkAndExtractInputs <- function(xmcdaData) {
  altIDs<- c()
  for(i in as.list(xmcdaData$alternatives$getIDs()))
  {
    altIDs <- c(altIDs, i$toString())
  }
  
  critIDs <- c()
  preferenceDirs <- c()
  criteriaScales <- as.list(xmcdaData$criteriaScalesList)[[1]]
  criteria = as.list(criteriaScales$keySet())
  for(i in criteria)
  {
    critIDs <- c(critIDs,i$id())
    scale <-criteriaScales$get(i)$get(as.integer(0))
    preferenceDirs<- c(preferenceDirs, scale$getPreferenceDirection()$toString())
  }
  
  hierarchy <- parseHierarchy(xmcdaData, critIDs)
  
  weightConstraints <- NULL
  withWeightConstraints <- FALSE
  constraints <-as.list(xmcdaData$criteriaLinearConstraintsList)
  for(constraint in constraints) {
    currentConstraints <- as.list(constraint)
    for(i in 1:length(currentConstraints)) {
      weightConstraints[[i]] <- getWeightConstraint(currentConstraints[[i]], hierarchy)
    }
    withWeightConstraints <- TRUE
  }
  
  transformToUtilities <- TRUE
  boundariesProvided <- FALSE
  hierarchyNode <- "root"
  samplesNo <- 100
  intervalsNo <- 10
  parameters <- as.list(xmcdaData$programParametersList)
  if(length(parameters) > 0)
  {
    parameters <- parameters[[1]]
    param <- parameters$getParameter("transformToUtilities")
    if(!is.null(param))
      transformToUtilities <-param$getValues()$get(as.integer(0))$getValue()
    param <- parameters$getParameter("boundariesProvided")
    if(!is.null(param))
      boundariesProvided <-param$getValues()$get(as.integer(0))$getValue()
    param <- parameters$getParameter("samplesNb")
    if(!is.null(param))
      samplesNo <- param$getValues()$get(as.integer(0))$getValue()
    param <- parameters$getParameter("intervalsNb")
    if(!is.null(param))
      intervalsNo <- param$getValues()$get(as.integer(0))$getValue()
    param <- parameters$getParameter("hierarchyNode")
    if(!is.null(param)){
      hierarchyNode <- param$getValues()$get(as.integer(0))$getValue()
      if(!hierarchyNode %in% names(hierarchy))
        stop(paste("Given hierarchy node: \"", hierarchyNode,"\" does not exist in hierarchy", sep=""))
    }
  }  
  
  node <- hierarchy[[hierarchyNode]]
  
  orderedCriteria <- orderCriteriaByPreference(critIDs, preferenceDirs, node$criteria)
  performance <-  getNumericPerformanceTableList(xmcdaData)[[1]][, c(orderedCriteria$critIDs)]
  
  result <- list(data = performance,
                 inputCount = orderedCriteria$inputCount,
                 outputCount = orderedCriteria$outputCount,
                 weightConstraints = weightConstraints,
                 withWeightConstraints = withWeightConstraints,
                 altIDs = altIDs,
                 hierarchy = hierarchy,
                 critIDs = colnames(performance),
                 transformToUtilities = transformToUtilities,
                 boundariesProvided = boundariesProvided,
                 hierarchyNode = hierarchyNode,
                 samplesNo = samplesNo,
                 intervalsNo = intervalsNo)
  
  if(boundariesProvided == TRUE) {
    low <- array(0,dim=c(length(critIDs)))
    up <- array(0,dim=c(length(critIDs)))
    
    for(i in 1:ncol(performance))
    {
      crit <- colnames(performance)[i]
      if(xmcdaData$criteriaScalesList$get(as.integer(0))$get(xmcdaData$criteria$get(crit))$get(as.integer(0)) %instanceof% "org.xmcda.QuantitativeScale")
      {
        scale <- .jcast(xmcdaData$criteriaScalesList$get(as.integer(0))$get(xmcdaData$criteria$get(crit))$get(as.integer(0)), "org.xmcda.QuantitativeScale")
        low[i] <- scale$getMinimum()$getValue()
        up[i] <- scale$getMaximum()$getValue()
      }
    }
    
    result$boundaries <- list(up=up, low=low)
  } else {
    result$boundaries <- createBoundaries(result)
  }
  
  
  if(result$withWeightConstraints){
    result <- removeUnnecessaryConstraints(result)
    result$withWeightConstraints <- length(result$weightConstraints) > 0
  }
  if(result$withWeightConstraints){
    result$weightConstraints <- transformConstraints(result$weightConstraints, length(hierarchy))
  }
  
  result <- removeUnnecessaryHierarchyNodes(result)
  
  return (result)
}

createBoundaries <- function (dmuData) {
  eps <- 0.01
  varCount <- ncol(dmuData$data)
  up <- c()
  low <- c()
  for(i in 1:varCount) {
    up <- cbind(up, max(dmuData$data[,i]) + eps)
    low <- cbind(low, min(dmuData$data[,i]) - eps)
  }
  result <- list(up = up, low = low)
  return (result)
}

orderCriteriaByPreference <- function(criteriaIDs, preferenceDirs, nodeCriteria) {
  inputIds <- c()
  outputIds <- c()
  for(crit in nodeCriteria){
    idx <- which(criteriaIDs == crit)
    if(preferenceDirs[idx] == "MIN"){
      inputIds <- c(inputIds, crit)
    }
    else{
      outputIds <- c(outputIds, crit)
    }
  }
  
  result <- list (critIDs = c(inputIds, outputIds), 
                  inputCount = length(inputIds), 
                  outputCount = length(outputIds))
  return (result)
}


parseHierarchy <- function(xmcdaData, criteriaIds){
  xmcdaHierarchy <- as.list(xmcdaData$criteriaHierarchiesList)[[1]]
  hierarchy <- list()
  children <- c()
  for(root in as.list(xmcdaHierarchy$getRootNodes())){
    hierarchy <- parseNode(root, "root", hierarchy, criteriaIds)
    children <- c(children, root$getCriterion()$id())
  }
  hierarchy[["root"]] <-list(id = "root", parent = "-1", criteria = criteriaIds, isLeaf=FALSE, children = children)
  
  return(hierarchy)
}

parseNode = function(node, parent, hierarchy, allCriteriaIds)
{
  criteriaIds <- c()
  children <- c()
  id <- node$getCriterion()$id()
  
  subNodes = as.list(node$getChildren())
  if(length(subNodes) > 0)
  {
    for(subNode in subNodes)
    {
      hierarchy <- parseNode(subNode, id, hierarchy, allCriteriaIds)
      criteriaIds <- append(criteriaIds, hierarchy[[length(hierarchy)]]$criteria)
      children <- c(children, subNode$getCriterion()$id())
    }
  }
  else{
    criteriaIds <- append(criteriaIds, id)
  }
  isLeaf <- length(subNodes) == 0
  if(isLeaf && !id %in% allCriteriaIds)
    stop(paste("Leaf hierarchy node does not exist in inputs/outputs scales file, criteron:", id))
  hierarchy[[id]] <- list(id = id, parent = parent, criteria = criteriaIds, isLeaf=isLeaf, children=children)
  return(hierarchy)
}

getWeightConstraint <- function(constraint, hierarchy) {
  varCount <- length(hierarchy)
  elements <- as.list(constraint$getElements())
  operator <- constraint$getOperator()$toString()
  rhs <- constraint$getRhs()
  rhs <- as.double(rhs$getValue())
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
    operator <- "<="
    rhs <- -rhs
    weightConstraint <- -weightConstraint
  }
  weightConstraintData <- list(constraint = weightConstraint, operator=operator, rhs=rhs)
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

transformConstraints = function(constraints, varCount){
  transformed = array(dim = c(length(constraints), varCount + 2))
  for(i in 1:length(constraints)){
    transformed[i ,1:varCount] <- constraints[[i]]$constraint
    transformed[i, varCount+1] <- constraints[[i]]$operator
    transformed[i, varCount+2] <- constraints[[i]]$rhs
  }
  return(transformed)
}

removeUnnecessaryHierarchyNodes <- function(dmuData){
  selectedRoot <- dmuData$hierarchyNode
  toRemove <- removeFromNode(dmuData$hierarchy, "root", selectedRoot)
  names <- names(dmuData$hierarchy)
  ids <- c()
  for(n in toRemove){
    ids <- c(ids, which(names == n))
    dmuData$hierarchy[[n]] <- NULL
  }
  if(!is.null(ids))
    dmuData$weightConstraints <- dmuData$weightConstraints[,-ids]
  return(dmuData)
}

removeFromNode <- function(hierarchy, node, selectedRoot){
  toRemove <- c()
  if(node != selectedRoot){
    for(c in hierarchy[[node]]$children){
      toRemove <- append(toRemove, removeFromNode(hierarchy, c, selectedRoot))
    }
    toRemove <- append(toRemove, node) 
  }
  return(toRemove)
}