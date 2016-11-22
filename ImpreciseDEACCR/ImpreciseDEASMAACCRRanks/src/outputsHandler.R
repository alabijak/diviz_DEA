# TODO depending on whether the file was generated from a description based on
# XMCDA v2 or v3, only one list is correct, either XMCDA_v2_TAG_FOR_FILENAME
# or XMCDA_v3_TAG_FOR_FILENAME: check them to determine which one should be
# adapted.

XMCDA_v2_TAG_FOR_FILENAME <- list(
  # output name -> XMCDA v2 tag
  rankAcceptabilityIndices = "performanceTable",
  avgRank = "alternativesValues",
  messages = "methodMessages"
)

XMCDA_v3_TAG_FOR_FILENAME <- list(
  # output name -> XMCDA v3 tag
  rankAcceptabilityIndices = "performanceTable",
  avgRank = "alternativesValues",
  messages = "programExecutionResult"
)

xmcda_v3_tag <- function(outputName){
  return (XMCDA_v3_TAG_FOR_FILENAME[[outputName]])
}

xmcda_v2_tag <- function(outputName){
  return (XMCDA_v2_TAG_FOR_FILENAME[[outputName]])
}


convert <- function(results, alternatives, programExecutionResult) {
  
  distribution <- convertDistribution(results$ranks, alternatives)
  
  names <- c()
  for(i in as.list(alternatives$getIDs()))
  {
    names <-c(names, i$toString())
  }
  
  xmcdaAvg <-.jnew("org/xmcda/XMCDA")
  names(results$avgRank) <- names
  putAlternativesValues(xmcdaAvg, results$avgRank)
  
  
  return (list(avgRank=xmcdaAvg, rankAcceptabilityIndices = distribution))
}

convertDistribution <- function(results, alternatives)
{
  altIDs <- as.list(alternatives$getIDs())
  colnames<- c(paste("Rank", 1:ncol(results)))
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
      table$put(alternatives$get(altIDs[[i]]$toString()), columns[[j]], .jnew("java/lang/Double", results[i, j]))
    }
  }
  xmcdaDist <-.jnew("org/xmcda/XMCDA")
  xmcdaDist$performanceTablesList$add(table)
  return(xmcdaDist)
}
