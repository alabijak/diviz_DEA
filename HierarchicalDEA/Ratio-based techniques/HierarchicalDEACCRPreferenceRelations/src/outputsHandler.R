# TODO depending on whether the file was generated from a description based on
# XMCDA v2 or v3, only one list is correct, either XMCDA_v2_TAG_FOR_FILENAME
# or XMCDA_v3_TAG_FOR_FILENAME: check them to determine which one should be
# adapted.

XMCDA_v2_TAG_FOR_FILENAME <- list(
  # output name -> XMCDA v2 tag
  necessaryDominance = "alternativesComparisons",
  possibleDominance = "alternativesComparisons",
  messages = "methodMessages"
)

XMCDA_v3_TAG_FOR_FILENAME <- list(
  # output name -> XMCDA v3 tag
  necessaryDominance = "alternativesMatrix",
  possibleDominance = "alternativesMatrix",
  messages = "programExecutionResult"
)

xmcda_v3_tag <- function(outputName){
  return (XMCDA_v3_TAG_FOR_FILENAME[[outputName]])
}

xmcda_v2_tag <- function(outputName){
  return (XMCDA_v2_TAG_FOR_FILENAME[[outputName]])
}


convert <- function(preferenceMatrix, alternatives, programExecutionResult) {
  
  necessaryResults <-.jnew("org/xmcda/XMCDA")
  possibleResults <-.jnew("org/xmcda/XMCDA")
  
  nMatrix <-J("org.xmcda.Factory")$alternativesMatrix()  #.jnew("org/xmcda/AlternativesMatrix")
  pMatrix <- J("org.xmcda.Factory")$alternativesMatrix()  
  
  int1 <-.jnew("java/lang/Integer", as.integer(1))
  qValues <- .jnew("org/xmcda/QualifiedValues")
  qValue <- .jnew("org/xmcda/QualifiedValue", .jcast(int1))
  qValues$add(qValue)
  for(i in 1:nrow(preferenceMatrix))
  {
    for(j in 1:ncol(preferenceMatrix))
    {
      initial <- .jcast(alternatives$get(alternatives$getIDs()$get(as.integer(i - 1))))
      terminal <- .jcast(alternatives$get(alternatives$getIDs()$get(as.integer(j - 1))))
      
      if(preferenceMatrix[i, j] == 'N')
      {
        nMatrix$put(.jnew("org/xmcda/utils/Coord", initial, terminal), qValues)
        pMatrix$put(.jnew("org/xmcda/utils/Coord", initial, terminal), qValues)
      }
      else if(preferenceMatrix[i, j] == 'P')
      {
        pMatrix$put(.jnew("org/xmcda/utils/Coord", initial, terminal), qValues)
      }
    }
  }
    
  necessaryResults$alternativesMatricesList$add(nMatrix)
  possibleResults$alternativesMatricesList$add(pMatrix)
  
  return(list(necessaryDominance = necessaryResults, possibleDominance=possibleResults))
}
