calculateEfficiencyIntervals <- function (dmuData, samplesNo=10, intervalsNo=10, transformToUtilities=TRUE) {
  result <- calculateEfficiencySMAAForAll(dmuData, samplesNo, transformToUtilities)
  intervals <- createIntervals(result, intervalsNo)
  return(intervals)
}

calculateEfficiencySMAAForAll <- function(dmuData, samplesNo=10, transformToUtilities=TRUE) {
  
  dmuCount = nrow(dmuData$data)
  result <- c()
  source("smaa.R")
  samples <- createWeightsSamplesForSMAA(dmuData, samplesNo, vdea=TRUE)
  
  source("ImpreciseSMAA-VDEA.R")
  performanceSamples <- createPerformanceSamples(dmuData, samplesNo)
  
  for(i in 1:dmuCount) {
    result <- rbind(result, calculateEfficiencySMAA(dmuData, i, samples, performanceSamples))
  }
  return (result)
}

calculateEfficiencySMAA <- function (dmuData, subjectDmuIdx, samples, performanceSamples) {
  samplesNo <- nrow(samples)
  effResults  <- array(0, dim=samplesNo)
  for(i in 1:samplesNo) {
    dmuData$data <- performanceSamples[[i]]
    effResults[i] <- calculateEfficiencyForWeights(dmuData, subjectDmuIdx, samples[i,])
  }
  #print(effResults)
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

calculateEfficiencyForWeights = function (dmuData, subjectDmuIdx, weights) {
  outputs <- 0
  for(i in 1:(dmuData$inputCount + dmuData$outputCount)) {
    outputs <- outputs + weights[i] * dmuData$data[subjectDmuIdx, i]
  }
  return (outputs)
}

