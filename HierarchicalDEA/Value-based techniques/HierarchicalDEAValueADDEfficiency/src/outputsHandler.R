# TODO depending on whether the file was generated from a description based on
# XMCDA v2 or v3, only one list is correct, either XMCDA_v2_TAG_FOR_FILENAME
# or XMCDA_v3_TAG_FOR_FILENAME: check them to determine which one should be
# adapted.

XMCDA_v2_TAG_FOR_FILENAME <- list(
  # output name -> XMCDA v2 tag
  minEfficiency = "alternativesValues",
  maxEfficiency = "alternativesValues",
  minDistance = "alternativesValues",
  maxDistance = "alternativesValues",
  messages = "methodMessages"
)

XMCDA_v3_TAG_FOR_FILENAME <- list(
  # output name -> XMCDA v3 tag
  minEfficiency = "alternativesValues",
  maxEfficiency = "alternativesValues",
  minDistance = "alternativesValues",
  maxDistance = "alternativesValues",
  messages = "programExecutionResult"
)

xmcda_v3_tag <- function(outputName){
  return (XMCDA_v3_TAG_FOR_FILENAME[[outputName]])
}

xmcda_v2_tag <- function(outputName){
  return (XMCDA_v2_TAG_FOR_FILENAME[[outputName]])
}


convert <- function(results, altIDs, programExecutionResult) {
  eff <- c()
  for(i in results[,2])
  {
    eff <- c(eff,round(i, digits = 10))
  }
  names(eff) <- altIDs
  
  xmcdaEff<-.jnew("org/xmcda/XMCDA")
  putAlternativesValues(xmcdaEff,eff)
  
  eff <- c()
  for(i in results[,1])
  {
    eff <- c(eff,round(i,10))
  }
  names(eff) <- altIDs
  
  xmcdaMinEff <-.jnew("org/xmcda/XMCDA")
  putAlternativesValues(xmcdaMinEff,eff)
  
  eff <- c()
  for(i in results[,3])
  {
    eff <- c(eff,round(i,10))
  }
  names(eff) <- altIDs
  
  xmcdaMinDist<-.jnew("org/xmcda/XMCDA")
  putAlternativesValues(xmcdaMinDist,eff)
  
  eff <- c()
  for(i in results[,4])
  {
    eff <- c(eff,round(i,10))
  }
  names(eff) <- altIDs
  
  xmcdaMaxDist<-.jnew("org/xmcda/XMCDA")
  putAlternativesValues(xmcdaMaxDist,eff)
  
  return(list(maxEfficiency=xmcdaEff, minEfficiency=xmcdaMinEff, minDistance=xmcdaMinDist, maxDistance=xmcdaMaxDist))

}
