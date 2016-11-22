#changes performance table to best values for sbject DMU and worst for others
recalculatePerformance = function(dmuData, subjectDmu) {
  tolerance <- dmuData$tolerance
  dmuCount <- nrow(dmuData$data) 
  weightsCount <- dmuData$inputCount + dmuData$outputCount
  
  for(i in 1:dmuCount) {
    if(i == subjectDmu)
    {
      for(j in 1:dmuData$inputCount) {
        dmuData$data[i,j] <- dmuData$minData[i,j]
      }
      for(j in (dmuData$inputCount + 1): weightsCount) {
        dmuData$data[i,j] <- dmuData$maxData[i,j]
      }
    }
    else
    {
      for(j in 1:dmuData$inputCount) {
        dmuData$data[i,j] <- dmuData$maxData[i,j]
      }
      for(j in (dmuData$inputCount + 1): weightsCount) {
        dmuData$data[i,j] <- dmuData$minData[i,j]
      }
    }
  }
  return(dmuData)
}

#other DMU's constraints - bestRank
createOtherConstraints = function(model, subjectDmuIdx, dmuData) {
  CONST <- 1000
  dmuCount <- nrow(dmuData$data)
  varCount <- dmuCount + dmuData$inputCount + dmuData$outputCount
  constrCount <- dmuCount
  result <- array(0, dim = c(constrCount,varCount))
  
  for (i in 1 : constrCount) {
    if(i != subjectDmuIdx) {
      for(j in 1 : varCount) {
        if(i == j - (dmuData$inputCount + dmuData$outputCount)) {
          result[i,j] <- -CONST
        } else if (j <= dmuData$inputCount) {
          result[i, j] <- -dmuData$data[i, j]
        } else if (j <= dmuData$inputCount + dmuData$outputCount) {
          result[i, j] <- dmuData$data[i, j]
        } else {
          result[i, j] <- 0
        }
      } 
      add.constraint(model, result[i,], "<=", 0)
    }
  }   
}