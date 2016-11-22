# usage:
# R --slave --vanilla --file=DEACCREfficiencyCLI_XMCDAv2.R --args "[inDirectory]" "[outDirectory]"

rm(list=ls())

# tell R to use the rJava package and the RXMCDA3 package
library(lpSolveAPI)
library(rJava)
library(XMCDA3)

# cf. http://stackoverflow.com/questions/1815606/rscript-determine-path-of-the-executing-script
script.dir <- function() {
  cmdArgs <- commandArgs(trailingOnly = FALSE)
  needle <- "--file="
  match <- grep(needle, cmdArgs)
  if (length(match) > 0) {
    # Rscript
    return(dirname(normalizePath(sub(needle, "", cmdArgs[match]))))
  } else {
    # 'source'd via R console
    return(dirname(normalizePath(sys.frames()[[1]]$ofile)))
  }
}

# load the R files in the script's directory
script.wd <- setwd(script.dir())

source("utils.R")
source("inputsHandler.R")
source("outputsHandler.R")
# restore the working directory so that relative paths passed as
# arguments work as expected
if (!is.null(script.wd)) setwd(script.wd)

# get the in and out directories from the arguments

inDirectory <- commandArgs(trailingOnly=TRUE)[1]
outDirectory <- commandArgs(trailingOnly=TRUE)[2]

# filenames

unitsFile <- "units.xml"
inputsOutputsFile <- "inputsOutputs.xml"
inputsOutputsScalesFile <- "inputsOutputsScales.xml"
performanceTableFile <- "performanceTable.xml"
maxPerformanceTableFile <- "maxPerformanceTable.xml"
weightsLinearConstraintsFile <- "weightsLinearConstraints.xml"
methodParametersFile <- "methodParameters.xml"
bestRankFile <- "bestRank.xml"
worstRankFile <- "worstRank.xml"
messagesFile <- "messages.xml"

# the Java xmcda object for the output messages

xmcdaMessages<-.jnew("org/xmcda/XMCDA")
xmcdaData <- .jnew("org/xmcda/XMCDA")

loadXMCDAv3(xmcdaData, inDirectory, unitsFile, mandatory = TRUE, xmcdaMessages, "alternatives")
loadXMCDAv3(xmcdaData, inDirectory, inputsOutputsFile, mandatory = TRUE, xmcdaMessages, "criteria")
loadXMCDAv3(xmcdaData, inDirectory, inputsOutputsScalesFile, mandatory = TRUE, xmcdaMessages, "criteriaScales")
loadXMCDAv3(xmcdaData, inDirectory, performanceTableFile, mandatory = TRUE, xmcdaMessages, "performanceTable")
loadXMCDAv3(xmcdaData, inDirectory, maxPerformanceTableFile, mandatory = FALSE, xmcdaMessages, "performanceTable")
loadXMCDAv3(xmcdaData, inDirectory, weightsLinearConstraintsFile, mandatory = FALSE, xmcdaMessages, "criteriaLinearConstraints")
loadXMCDAv3(xmcdaData, inDirectory, methodParametersFile, mandatory = FALSE, xmcdaMessages, "programParameters")

# if we have problem with the inputs, it is time to stop

if (xmcdaMessages$programExecutionResultsList$size() > 0){
  if (xmcdaMessages$programExecutionResultsList$get(as.integer(0))$isError()){
    writeXMCDA(xmcdaMessages, paste(outDirectory,messagesFile, sep="/"))
    stop(paste("An error has occured while loading the input files. For further details, see ", messagesFile, sep=""))
  }
}

# let's check the inputs and convert them into our own structures

inputs<-checkAndExtractInputs(xmcdaData, programExecutionResult)

if (xmcdaMessages$programExecutionResultsList$size()>0){
  if (xmcdaMessages$programExecutionResultsList$get(as.integer(0))$isError()){
    writeXMCDA(xmcdaMessages, paste(outDirectory,messagesFile, sep="/"))
    stop(paste("An error has occured while checking and extracting the inputs. For further details, see ", messagesFile, sep=""))
  }
}

# here we know that everything was loaded as expected

# now let's call the calculation method

source("impreciseRank.R")
results <- handleException(
  function() return(
    calculateRank(inputs)
  ),
  xmcdaMessages,
  humanMessage = "The calculation could not be performed, reason: "
)



if (is.null(results)){
  writeXMCDA(xmcdaMessages, paste(outDirectory,messagesFile, sep="/"))
  stop("Could not calculate DEACCREfficiency.")
}


# fine, now let's put the results into XMCDA structures

xResults = convert(results, inputs$altIDs, xmcdaMessages)

if (is.null(xResults)){
  writeXMCDA(xmcdaMessages, paste(outDirectory,messagesFile, sep="/"))
  stop("Could not convert DEACCREfficiency results into XMCDA")
}

# and last, write them onto the disk

for (i in 1:length(xResults)){
  outputFilename = paste(outDirectory, paste(names(xResults)[i],".xml",sep=""), sep="/")
  tmp <- handleException(
    function() return(
      writeXMCDA(xResults[[i]], outputFilename, xmcda_v3_tag(names(xResults)[i]))
    ),
    xmcdaMessages,
    humanMessage = paste("Error while writing ", outputFilename,", reason :")
  )

  if (is.null(tmp)){
    writeXMCDA(xmcdaMessages, paste(outDirectory,messagesFile, sep="/"))
    stop("Error while writing ",outputFilename,sep="")
  }
}

# then the messages file
# TODO faire autrement qu'en ajoutant une info vide pour faire un <status>ok</status>

tmp <- handleException(
  function() return(
    putProgramExecutionResult(xmcdaMessages, infos="")
  ),
  xmcdaMessages
)

if (is.null(tmp)){
  writeXMCDA(xmcdaMessages, paste(outDirectory,messagesFile, sep="/"))
  stop("Could not add methodExecutionResult to tree.")
}

tmp <- handleException(
  function() return(
    writeXMCDA(xmcdaMessages, paste(outDirectory,messagesFile, sep="/"))
  ),
  xmcdaMessages
)

if (is.null(tmp)){
  writeXMCDA(xmcdaMessages, paste(outDirectory,messagesFile, sep="/"))
  stop("Error while writing messages file.")
}
