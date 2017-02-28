checkAndExtractInputs <- function(xmcdaData, programExecutionResult) { # TODO
  altIDs<- c() #<- getAlternativesIDs(dataTree$altTree)[[1]]
  for(i in as.list(xmcdaData$alternatives$getIDs()))
  {
    altIDs <- c(altIDs, i$toString())
  }
  
  critIDs <- c()
  preferenceDirs <- c() #getValues(dataTree$criteriaTree, "preferenceDirection") 
  ordinalFactors <- c()
  for(i in as.list(xmcdaData$criteria))
  {
    critIDs <- c(critIDs,i$id())
    scale <-xmcdaData$criteriaScalesList$get(as.integer(0))$get(i)$get(as.integer(0))
    preferenceDirs<- c(preferenceDirs, scale$getPreferenceDirection()$toString())
    if(scale %instanceof% "org.xmcda.QualitativeScale"){
      ordinalFactors <- c(ordinalFactors, i$id())
    }
  }
  
  #inputs have to be first, then oputputs
  orderedCriteria <- orderCriteriaByPreference(critIDs, preferenceDirs, ordinalFactors)
  
  performanceTables <- getNumericPerformanceTableList(xmcdaData)
  performance <- performanceTables[[1]] #getPerformanceTables(dataTree$performanceTree, altIDs, orderedCriteria$critIDs)[[1]]
  performance <- orderPerformanceByCriteria(performance, orderedCriteria$critIDs)
  
  maxPerformance <- NULL
  if(length(performanceTables) > 1){
    maxPerformance <- performanceTables[[2]]
    maxPerformance <- orderMaxPerformanceByCriteria(maxPerformance, orderedCriteria$critIDs, performance)
  }
  
  weightConstraints <- NULL
  withWeightConstraints <- FALSE
  constraints <-as.list(xmcdaData$criteriaLinearConstraintsList)
  for(constraint in constraints) {
    for(i in 0:(constraint$size()-1)) {
      weightConstraints <- rbind(weightConstraints,getWeightConstraint(constraint$get(i),
                                                                       orderedCriteria$critIDs))
    }
    withWeightConstraints <- TRUE
  }
  
  tolerance <- 0.0
  transformToUtilities <- TRUE
  boundariesProvided <- FALSE
  functionShapeProvided <- FALSE
  samplesNo <- 100
  parameters <- as.list(xmcdaData$programParametersList)
  if(length(parameters) > 0)
  {
    parameters <- parameters[[1]]
    param <- parameters$getParameter("tolerance")
    if(!is.null(param))
      tolerance <-param$getValues()$get(as.integer(0))$getValue()
    param <- parameters$getParameter("transformToUtilities")
    if(!is.null(param))
      transformToUtilities <-param$getValues()$get(as.integer(0))$getValue()
    param <- parameters$getParameter("boundariesProvided")
    if(!is.null(param))
      boundariesProvided <-param$getValues()$get(as.integer(0))$getValue()
    param <- parameters$getParameter("functionShapeProvided")
    if(!is.null(param))
      functionShapeProvided <-param$getValues()$get(as.integer(0))$getValue()
    param <- parameters$getParameter("samplesNb")
    if(!is.null(param))
      samplesNo <-param$getValues()$get(as.integer(0))$getValue()
  }  
  if(is.null(maxPerformance))
  { 
    maxPerformance = array(dim=c(nrow(performance), ncol(performance)))
    for(i in 1:nrow(performance))
    {
      for(j in 1:ncol(performance))
      {
        if(j %in% orderedCriteria$ordinalCriteria)
        {
          maxPerformance[i,j] = performance[i,j]
        }
        else
        {
          tmp = performance[i,j]
          performance[i,j] = tmp * (1-tolerance)
          maxPerformance[i,j] = tmp * (1+tolerance)
        }
      }
    }
  }
  
  
  
  result <- list(data=performance,
                 minData=performance,
                 maxData=maxPerformance,
                 inputCount=orderedCriteria$inputCount,
                 outputCount=orderedCriteria$outputCount,
                 weightConstraints = weightConstraints,
                 withWeightConstraints = withWeightConstraints,
                 altIDs = altIDs,
                 ordinalFactors = orderedCriteria$ordinalCriteria,
                 transformToUtilities = transformToUtilities,
                 samplesNo = samplesNo)
  
  if(boundariesProvided == TRUE) {
    low <- array(0,dim=c(length(critIDs)))
    up <- array(0,dim=c(length(critIDs)))
    
    for(i in 1:(orderedCriteria$inputCount + orderedCriteria$outputCount))
    {
      crit <- orderedCriteria$critIDs[[i]]
      if(xmcdaData$criteriaScalesList$get(as.integer(0))$get(xmcdaData$criteria$get(crit))$get(as.integer(0)) %instanceof% "org.xmcda.QuantitativeScale")
      {
        scale <- .jcast(xmcdaData$criteriaScalesList$get(as.integer(0))$get(xmcdaData$criteria$get(crit))$get(as.integer(0)), "org.xmcda.QuantitativeScale")
        low[i] <- scale$getMinimum()$getValue()
        up[i] <- scale$getMaximum()$getValue()
      }
    }
    
    result$boundaries = list(up=up, low=low)
  } else {
    result$boundaries = createBoundaries(result)
  }
  
  functionShapes <- NULL
  if(functionShapeProvided)
  {
    if(xmcdaData$criteriaFunctionsList$size() > 0)
    {
      shapes <- xmcdaData$criteriaFunctionsList$get(as.integer(0))
      for(i in 1:length(orderedCriteria$critIDs))
      {
        crit<-xmcdaData$criteria$get(orderedCriteria$critIDs[[i]])
        functionShapes[[i]] <- list()
        if(!is.null(shapes$get(crit)) &&
           !xmcdaData$criteriaScalesList$get(as.integer(0))$get(crit)$get(as.integer(0)) %instanceof% "org.xmcda.QualitativeScale")
        {
          id <- 1
          for(currentShape in as.list(shapes$get(crit)))
          {
            
            values <- array(dim=c(0,2))
            for(j in 1:length(as.list(currentShape)))
            {
              seg<-currentShape$get(as.integer(j-1))
              values <- rbind(values, c(seg$getHead()$getAbscissa()$getValue(), seg$getHead()$getOrdinate()$getValue()))
              
              if(j == length(as.list(currentShape)))
              {
                values <- rbind(values, c(seg$getTail()$getAbscissa()$getValue(), seg$getTail()$getOrdinate()$getValue()))
              }
            }
            
            values <- values[order(values[,1]),]
            functionShapes[[i]][[id]]<- values
            id <- id + 1
          }
        }
        
      }
    }
  }
  result$functionShapes <- functionShapes 
  result$multipleFunctionCriteria <- c()
  if(!is.null(functionShapes))
  {
    for(i in 1:length(orderedCriteria$critIDs))
    {
      if(!is.null(functionShapes[[i]]) && length(functionShapes[[i]] ) == 2)
      {
        result$multipleFunctionCriteria<- c(result$multipleFunctionCriteria, i)
      }
    }
  }
  return (result)
}

createBoundaries <- function (dmuData) {
  eps <- 0.01
  varCount <- dmuData$inputCount + dmuData$outputCount
  up <- c()
  low <- c()
  for(i in 1:varCount) {
    up <- cbind(up, max(dmuData$maxData[,i]) + eps)
    low <- cbind(low, min(dmuData$minData[,i]) - eps)
  }
  result <- list(up = up, low = low)
  return (result)
}

test <- function(xmcdaData)
{
  
}

orderCriteriaByPreference <- function(criteriaIDs, preferenceDirs, ordinalCriteria) {
  sortedPref <- sort(preferenceDirs, decreasing=TRUE, index.return = TRUE)
  orderedCriteriaIDs <- array(0, dim=length(criteriaIDs))
  inputCount <- 0
  outputCount <- 0
  ordinalCriteriaIdx <- c()
  for(i in 1:length(sortedPref$ix)) {
    if(sortedPref$x[i] == "MIN") {
      inputCount <- inputCount + 1
    } else {
      outputCount <- outputCount + 1
    }
    critIdx <- sortedPref$ix[i]
    orderedCriteriaIDs[i] <- criteriaIDs[critIdx]  
    
    if(criteriaIDs[critIdx] %in% ordinalCriteria){
      ordinalCriteriaIdx<- c(ordinalCriteriaIdx, i)
    }
  }
  result <- list (critIDs = orderedCriteriaIDs, 
                  inputCount = inputCount, 
                  outputCount = outputCount, 
                  ordinalCriteria = ordinalCriteriaIdx)
  return (result)
}

#switch column order to match pattern -> inputs first, then outputs
orderPerformanceByCriteria <- function(performance, orderedCriteriaIDs) {
  orderedPerformance <- array(0, dim=c(nrow(performance), ncol(performance)))
  for(i in 1:length(orderedCriteriaIDs)) {
    orderedPerformance[,i] <- performance[,orderedCriteriaIDs[i]]
  }
  return (orderedPerformance)
}

orderMaxPerformanceByCriteria <- function(performance, orderedCriteriaIDs, minPerf) {
  orderedPerformance <- array(0, dim=c(nrow(minPerf), ncol(minPerf)))
  for(i in 1:length(orderedCriteriaIDs)) {
    if(orderedCriteriaIDs[i] %in% colnames(performance))
    {
      orderedPerformance[,i] <- performance[,orderedCriteriaIDs[i]]
    }
    else
    {
      orderedPerformance[,i] <- minPerf[,i]
    }
  }
  return (orderedPerformance)
}

getWeightConstraint <- function(constraint, critIDs) {
  varCount <- length(critIDs)
  elements <- as.list(constraint$getElements())
  operator <- constraint$getOperator()$toString()
  rhs <- constraint$getRhs()
  weightConstraint <- array(0,dim=varCount)  
  
  for(i in 1:length(elements)) {
    element <- elements[[i]]
    critID <- element$getUnknown()$id()
    critIdx <- which(critIDs == critID) 
    value <- as.double(element$getCoefficient()$getValue())
    if(operator == "GEQ"){
      weightConstraint[critIdx] <- -value
    }
    else {
      weightConstraint[critIdx] <- value
    }
  }
  if(operator == "EQ") {
    operator <- "="
  }
  else {
    operator <- "<="
  }
  
  
  if(operator == "GEQ"){
    rhsValue <- -as.double(rhs$getValue())
  }
  else {
    rhsValue <- rhs$getValue()
  }
  return (c(weightConstraint, operator, rhs$getValue()))
}
