# TODO depending on whether the file was generated from a description based on
# XMCDA v2 or v3, only one list is correct, either XMCDA_v2_TAG_FOR_FILENAME
# or XMCDA_v3_TAG_FOR_FILENAME: check them to determine which one should be
# adapted.

XMCDA_v2_TAG_FOR_FILENAME <- list(
  # output name -> XMCDA v2 tag
  pairwiseOutrankingIndices = "performanceTable",
  messages = "methodMessages"
)

XMCDA_v3_TAG_FOR_FILENAME <- list(
  # output name -> XMCDA v3 tag
  pairwiseOutrankingIndices = "performanceTable",
  messages = "programExecutionResult"
)

xmcda_v3_tag <- function(outputName){
  return (XMCDA_v3_TAG_FOR_FILENAME[[outputName]])
}

xmcda_v2_tag <- function(outputName){
  return (XMCDA_v2_TAG_FOR_FILENAME[[outputName]])
}


convert <- function(results, alternatives, programExecutionResult) {
  return (list(pairwiseOutrankingIndices=convertDistribution(results, alternatives)))
}

generateColNames<-function(res, altIDs)
{
  cols <- ncol(res)
  interval <- 1/cols
  colNames <- c()
  for(i in 1:cols) {
    colNames <- c(colNames, paste("geq", altIDs[[i]]$toString()))    
  }
  
  return(colNames)
}

convertDistribution <- function(results, alternatives)
{
  altIDs <- as.list(alternatives$getIDs())
  colnames<- generateColNames(results, altIDs)
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
