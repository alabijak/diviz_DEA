# TODO depending on whether the file was generated from a description based on
# XMCDA v2 or v3, only one list is correct, either XMCDA_v2_TAG_FOR_FILENAME
# or XMCDA_v3_TAG_FOR_FILENAME: check them to determine which one should be
# adapted.

XMCDA_v2_TAG_FOR_FILENAME <- list(
  # output name -> XMCDA v2 tag
  bestRank = "alternativesValues",
  worstRank = "alternativesValues",
  messages = "methodMessages"
)

XMCDA_v3_TAG_FOR_FILENAME <- list(
  # output name -> XMCDA v3 tag
  bestRank = "alternativesValues",
  worstRank = "alternativesValues",
  messages = "programExecutionResult"
)

xmcda_v3_tag <- function(outputName){
  return (XMCDA_v3_TAG_FOR_FILENAME[[outputName]])
}

xmcda_v2_tag <- function(outputName){
  return (XMCDA_v2_TAG_FOR_FILENAME[[outputName]])
}


convert <- function(results, altIDs, programExecutionResult) { 
  
  ranks <- results[,1]
  names(ranks) <- altIDs
  
  xmcdaRes<-.jnew("org/xmcda/XMCDA")
  putAlternativesValues(xmcdaRes,ranks)
  
  ranks <- results[,2]
  names(ranks) <- altIDs
  
  xmcdaWorstRank<-.jnew("org/xmcda/XMCDA")
  putAlternativesValues(xmcdaWorstRank,ranks)
  
  return(list(bestRank=xmcdaRes, worstRank=xmcdaWorstRank))
  #return(xmcdaRes)
}
