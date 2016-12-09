# TODO depending on whether the file was generated from a description based on
# XMCDA v2 or v3, only one list is correct, either XMCDA_v2_TAG_FOR_FILENAME
# or XMCDA_v3_TAG_FOR_FILENAME: check them to determine which one should be
# adapted.

XMCDA_v2_TAG_FOR_FILENAME <- list(
  # output name -> XMCDA v2 tag
  efficiencyDistribution = "performanceTable",
  maxEfficiency = "alternativesValues",
  minEfficiency = "alternativesValues",
  avgEfficiency = "alternativesValues",
  messages = "methodMessages"
)

XMCDA_v3_TAG_FOR_FILENAME <- list(
  # output name -> XMCDA v3 tag
  efficiencyDistribution = "performanceTable",
  maxEfficiency = "alternativesValues",
  minEfficiency = "alternativesValues",
  avgEfficiency = "alternativesValues",
  messages = "programExecutionResult"
)

xmcda_v3_tag <- function(outputName){
  return (XMCDA_v3_TAG_FOR_FILENAME[[outputName]])
}

xmcda_v2_tag <- function(outputName){
  return (XMCDA_v2_TAG_FOR_FILENAME[[outputName]])
}


convert <- function(results, alternatives, programExecutionResult) { # TODO
  altIDs <- c()
  for(i in as.list(xmcdaData$alternatives$getIDs()))
  {
    altIDs <- c(altIDs, i$toString())
  }
  
  eff <- results$minEff
  names(eff) <- altIDs
  
  xmcdaMin<-.jnew("org/xmcda/XMCDA")
  putAlternativesValues(xmcdaMin,eff)
  
  eff <- results$maxEff
  names(eff) <- altIDs
  
  xmcdaMax<-.jnew("org/xmcda/XMCDA")
  putAlternativesValues(xmcdaMax,eff)
  
  eff <- results$avgEff
  names(eff) <- altIDs
  
  xmcdaAvg <-.jnew("org/xmcda/XMCDA")
  putAlternativesValues(xmcdaAvg,eff)
  
  distribution <- convertDistribution(results$intervals, as.list(alternatives))
  
  return(list(maxEfficiency=xmcdaMax, minEfficiency=xmcdaMin, avgEfficiency=xmcdaAvg, efficiencyDistribution=distribution))
  
}

generateColNames<-function(res)
{
  cols <- ncol(res)
  interval <- 1/cols
  colsNames <- c()
  colsNames <- rbind(colsNames, paste("[0.0 - ", interval, "]", sep=""))
  for(i in 1:(cols-1)) {
    colsNames <- rbind(colsNames, paste("(", i * interval, " - ", (i+1) * interval,"]", sep=""))    
  }
  
  return(colsNames)
}

convertDistribution <- function(results, alternatives)
{
  colnames<- generateColNames(results)
  columns<-c()
  for(i in 1:ncol(results))
  {
    col <- .jnew("org/xmcda/Criterion", colnames[[i]])
    columns <- c(columns, col)
  }
  
  table <- .jnew("org/xmcda/PerformanceTable")
  for(i in 1:nrow(results))
  {
    for(j in 1:ncol(results))
    {
      table$put(alternatives[[i]], columns[[j]], .jnew("java/lang/Double", results[i, j]))
    }
  }
  xmcdaDist <-.jnew("org/xmcda/XMCDA")
  xmcdaDist$performanceTablesList$add(table)
  return(xmcdaDist)
}
