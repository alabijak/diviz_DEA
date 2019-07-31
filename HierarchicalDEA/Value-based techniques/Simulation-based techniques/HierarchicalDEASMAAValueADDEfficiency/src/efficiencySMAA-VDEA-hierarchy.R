calculateEfficiencyIntervals <- function (dmuData, samplesNo=10, intervalsNo=10, transformToUtilites=TRUE) {
  result <- calculateEfficiencySMAAForAll(dmuData, samplesNo, transformToUtilites)
  intervals <- c()
  return(createIntervals(result, intervalsNo))
}

calculateEfficiencySMAAForAll <- function(dmuData, samplesNo=10, transformToUtilites=TRUE) {
  if(transformToUtilites == TRUE) {
    dmuData <- transformToUtilityValues(dmuData)
  }
  dmuCount = nrow(dmuData$data)
  
  result <- c()
  source("hierarchicalSMAA.R")
  samples <- createWeightsSamplesForSMAA(dmuData, samplesNo)
  
  for(i in 1:dmuCount) {
    result <- rbind(result, calculateEfficiencySMAA(dmuData, i, samples))
  }
  
  return (result)
}

calculateEfficiencySMAA <- function (dmuData, subjectDmuIdx, samples) {
  samplesNo <- nrow(samples)
  effResults  <- array(0, dim=samplesNo)
  for(i in 1:samplesNo) {
    effResults[i] <- calculateEfficiencyForWeights(dmuData, subjectDmuIdx, samples[i,])
  }
  
  return (effResults)
}

createIntervals <- function (efficiencyResults, intervalsNo) {
  dmuCount <- NROW(efficiencyResults)
  samplesNo <- ncol(efficiencyResults)
  
  avgRes  <- array(0, dim=dmuCount)
  minRes <- array(1, dim=dmuCount)
  maxRes <- array(0, dim=dmuCount)
  
  intervals  <- array(0, dim=c(dmuCount,intervalsNo))
  intervalLenght <- 1/intervalsNo
  for(i in 1:samplesNo) {
    for(j in 1:dmuCount){
      if(efficiencyResults[j,i] == 0) {
        intervalIdx = 1
      } else {
        intervalIdx <- ceiling(efficiencyResults[j,i]/intervalLenght)
      }
      intervals[j,intervalIdx] = intervals[j,intervalIdx] + 1
      avgRes[j] <- avgRes[j] + efficiencyResults[j,i]
      maxRes[j] <- max(maxRes[j], efficiencyResults[j,i])
      minRes[j] <- min(minRes[j], efficiencyResults[j,i])
    }
  }
  avgRes <- avgRes / samplesNo
  intervals <- intervals / samplesNo
  result <- list(intervals = intervals, avgEff = avgRes, minEff = minRes, maxEff = maxRes)
  return (result)
}

# calculateEfficiencyForWeights = function (dmuData, subjectDmuIdx, weights) {
#   outputs <- 0
#   for(i in 1:ncol(dmuData$data)) {
#     outputs <- outputs + weights[i] * dmuData$data[subjectDmuIdx, i]
#   }
#   return (outputs)
# }

calculateEfficiencyForWeights = function (dmuData, subjectDmuIdx, weights) {
  efficiency <- 0
  for(i in 1:length(dmuData$critIDs))
  {
    act <- dmuData$critIDs[i]
    weight <- 1
    for(j in dmuData$hierarchy)
    {
      if(act %in% j$criteria)
      {
        critIdx <- which(names(dmuData$hierarchy) == j$id)
        weight <- weight * weights[critIdx]
      }
    }
    
    
    efficiency <- efficiency + weight * dmuData$data[subjectDmuIdx, i]
  }
  return(efficiency)
}

transformToUtilityValues = function(dmuData) {
  boundaries <- dmuData$boundaries
  dmuCount <- nrow(dmuData$data)
  inputs <- dmuData$inputCount
  outputs <- dmuData$outputCount
  for(i in 1:dmuCount) {
    for(j in 1:(inputs + outputs)) {
      if(j <= inputs) {
        dmuData$data[i,j] <- (boundaries$up[j] - dmuData$data[i,j]) / (boundaries$up[j] - boundaries$low[j])
      } else if (j <= inputs + outputs) {
        dmuData$data[i,j] <- (dmuData$data[i,j] - boundaries$low[j]) / (boundaries$up[j] - boundaries$low[j])
      }
    }
  }
  return (dmuData)
}
