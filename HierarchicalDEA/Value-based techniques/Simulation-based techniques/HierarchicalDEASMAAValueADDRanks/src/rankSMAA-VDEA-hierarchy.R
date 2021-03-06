calculateRankSMAAForAll <- function(dmuData, samplesNo=10, transformToUtilities=TRUE) {
  dmuCount = nrow(dmuData$data)
  result <- c()
  source("efficiencySMAA-VDEA-hierarchy.R")
  effResults <- calculateEfficiencySMAAForAll(dmuData, samplesNo, transformToUtilities)

  result <- calculateRankSMAA(effResults)
  return(createSummary(result, dmuCount, samplesNo))
}

calculateRankSMAA <- function (effResults) {
  maxRank <- NROW(effResults) + 1
  dmuCount <- ncol(effResults)
  for(i in 1:dmuCount) {
    effResults[,i] <- maxRank - rank(effResults[,i], ties.method="min") 
  }
  return (effResults)
}

createSummary <- function (ranks, intervalsNo, samplesNo) {
  dmuCount <- NROW(ranks)
  intervals  <- array(0, dim=c(dmuCount,intervalsNo))
  avgRank <- array(0, dim=dmuCount)
  for(i in 1:dmuCount) {
    for(j in 1:length(ranks[i,])){
      intervals[i, ranks[i,j]] = intervals[i,ranks[i,j]] + 1 
    }
	for(j in 1:dmuCount){
		avgRank[i] <- avgRank[i] + j * (intervals[i,j])
	}
  }
  avgRank <- avgRank / samplesNo
  intervals <- intervals / samplesNo
  result <- list(ranks = intervals, avgRank = avgRank)
  return (result)
}

