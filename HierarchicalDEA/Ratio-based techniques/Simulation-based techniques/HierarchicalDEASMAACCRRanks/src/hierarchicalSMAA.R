createWeightsSamplesForSMAA <- function (dmuData, samplesNo) {
  baseCons <- createBaseConstraints(dmuData)
  if(dmuData$withWeightConstraints == TRUE) {
    dmuData$weightConstraints <- rbind(dmuData$weightConstraints, baseCons)
    
  } else {
    dmuData$weightConstraints <- baseCons
  }
  weightsCons <- parseWeights(dmuData)
  samples <- createWeightsSamples(weightsCons, samplesNo)
  return (samples)
}

createBaseConstraints <- function (dmuData) {
  varCount <- length(dmuData$hierarchy) + 2
  normWeights <- array(0, dim=c(length(dmuData$hierarchy), varCount))
  
  normWeights[,varCount - 1] <- "<="
  for(i in 1:(varCount - 2)) {
    normWeights[i, i] <- -1
  }
  
  normWeights<- createAdditionalConditions(dmuData, normWeights, varCount, dmuData$hierarchyNode)
  
  condition = array(0, dim=c(varCount))
  condition[varCount - 1] = "="
  condition[varCount] = 1
  critNo = which(names(dmuData$hierarchy) == dmuData$hierarchyNode)
  condition[critNo] <- 1
  normWeights <- rbind(normWeights, condition)
  
  row.names(normWeights) <- NULL
  return (normWeights)
}

createAdditionalConditions = function(dmuData, normWeights, varCount, critId)
{
  children <- dmuData$hierarchy[[critId]]$children
  condition = array(0, dim=c(varCount))
  condition[varCount - 1] = "="
  condition[varCount] = 1
  for(i in children)
  {
    critNo = which(names(dmuData$hierarchy) == i)
    
    condition[critNo] = 1
    normWeights <- createAdditionalConditions(dmuData, normWeights, varCount, i)
  }
  if(!is.null(children))
  {
    normWeights <- rbind(normWeights, condition)
  }
  return(normWeights)
}

#parse to har format
parseWeights <- function (dmuData) {
  varCount <- length(dmuData$hierarchy)
  weights <- dmuData$weightConstraints
  constrCount <- nrow(weights)
  constr <- c()
  dir <- c()
  rhs <- c()
  for(i in 1:constrCount) {
    currConstr <- c()
    for(j in 1:varCount) {
      currConstr <- c(currConstr, as.numeric(weights[i,j]))
    }
    rownames(currConstr) <- NULL
    constr <- rbind(constr, currConstr)
    dir <- c(dir,  gsub("\\s","",as.character(weights[i,varCount + 1])))
    rhs <- c(rhs, as.numeric(weights[i, varCount + 2]))
  }
  rownames(constr) <- NULL
  constraints <- list(constr=constr, dir=dir, rhs=rhs)
  return (constraints)
}

createWeightsSamples <- function(constraints, samplesNo) {
  state <- har.init(constraints)
  result <- har.run(state, n.samples=samplesNo)
  samples <- result$samples
  return(samples)
}

parseSampleToWeightsConstraints <- function(sample) {
  varCount <- length(sample)
  weights <- list()
  operator <- '='
  for(i in 1:varCount) {
    weight <- array(0, dim=c(varCount))
    weight[i] <- 1
    rhs <- sample[i]
    weights <- rbind(weights, list(weights=weight, rhs=rhs, operator=operator))
  }
  
  return (weights)
}

