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
  ordinalCount <- length(dmuData$ordinalFactors)
  varCount <- dmuCount*(ordinalCount+1) + dmuData$inputCount + dmuData$outputCount
  constrCount <- dmuCount
  
  for (i in 1 : constrCount) {
    if(i != subjectDmuIdx) {
      result <- array(0, dim = varCount)
      for(j in 1 : (dmuData$inputCount+dmuData$outputCount)) {
        sign <- 1
        if (j <= dmuData$inputCount) {
          sign <- -1
        }
        if(!(j %in% dmuData$ordinalFactors))
        {
          result[j] <- sign*dmuData$data[i, j]
        }
        else
        {
          idx = which(dmuData$ordinalFactors == j)
          result[dmuData$inputCount + dmuData$outputCount + idx*dmuCount + i] <- sign*1
        }
        
        result[i+dmuData$inputCount + dmuData$outputCount] <- -CONST
      } 
      add.constraint(model, result, "<=", 0)
    }
  }   
}