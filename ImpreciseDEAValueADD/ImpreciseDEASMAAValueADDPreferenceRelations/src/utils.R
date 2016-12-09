loadXMCDAv3 <- function(xmcdaData, inDirectory, filename, mandatory, programExecutionResult, tag){
  if (!file.exists(paste(inDirectory,filename,sep="/")))
  {
    if (mandatory)
    {
      putProgramExecutionResult(programExecutionResult, errors = paste("Could not find the mandatory file ", filename, sep=""))
    }
  } else {
    handleException(
      function() return(
        readXMCDA(file=paste(inDirectory,filename,sep="/"), xmcda=xmcdaData, tag=tag)
      ),
      programExecutionResult,
      humanMessage = paste("Unable to read & parse the file ", filename, ", reason: ", sep="")
    )
  }
}

loadXMCDAv2 <- function(xmcdaData_v2, inDirectory, filename, mandatory, programExecutionResult, tag){
  if (!file.exists(paste(inDirectory,filename,sep="/")))
  {
    if (mandatory)
    {
      putProgramExecutionResult(programExecutionResult, errors = paste("Could not find the mandatory file ", filename, sep=""))
    }
  } else {
    handleException(
      function() return(
        xmcdaData_v2 <- readXMCDAv2_and_update(xmcda_v2 = xmcdaData_v2, file=paste(inDirectory,filename,sep="/"), tag=tag)
      ),
      programExecutionResult,
      humanMessage = paste("Unable to read & parse the file ", filename, ", reason: ", sep="")
    )
  }
}

readXMCDAv2_and_update <- function(xmcda_v2 = NULL, file, tag){
  if (is.null(xmcda_v2)) xmcda_v2<-.jnew("org/xmcda/v2_2_1/XMCDA")
  
  parser2<-.jnew("org/xmcda/parsers/xml/xmcda_2_2_1/XMCDAParser")
  
  new_xmcda <- parser2$readXMCDA(file,.jarray(c(tag)))
  
  new_content <- new_xmcda$getProjectReferenceOrMethodMessagesOrMethodParameters()
  
  xmcda_v2$getProjectReferenceOrMethodMessagesOrMethodParameters()$addAll(new_content)
  
  return(xmcda_v2)
}

writeXMCDAv2 <- function(xmcda, filename){
  xmcda_v2 <- .jnew("org/xmcda/v2_2_1/XMCDA")
  
  converter<-.jnew("org/xmcda/converters/v2_2_1_v3_0/XMCDAConverter")
  
  xmcda_v2 <- converter$convertTo_v2(xmcda)
  
  parser2<-.jnew("org/xmcda/parsers/xml/xmcda_2_2_1/XMCDAParser")
  
  parser2$writeXMCDA(xmcda_v2, paste(filename, sep="/"))
}

convertConstraints <- function(xmcdaDatav2, xmcdaData) {
  parameters <- xmcdaDatav2$getProjectReferenceOrMethodMessagesOrMethodParameters()
  for(param in as.list(parameters))
  {
    if(param$getValue() %instanceof% "org.xmcda.v2_2_1.CriteriaLinearConstraints")
    {
      constraints_v3 <- .jnew("org/xmcda/CriteriaLinearConstraints")
      for(const in as.list(param$getValue()$getConstraint()))
      {
        constr_v3<-.jnew("org/xmcda/LinearConstraint")
        rhs <- const$getRhs()
        converter<-.jnew("org/xmcda/converters/v2_2_1_v3_0/QualifiedValueConverter")
        operator <- toupper(const$getOperator())
        rhs_v3 <- converter$convertTo_v3(rhs, xmcdaData)
        
        constr_v3$setRhs(rhs_v3)
        constr_v3$setOperator(.jfield("org.xmcda.LinearConstraint$Operator", NULL,operator))
        #elements_v3 <- c()
        elements_v3 <- .jnew("java.util.ArrayList")
        
        for(element in as.list(const$getElement()))
        {
          critID <- element$getCriterionID()
          coeff <- element$getCoefficient();
          coeff_v3 <-converter$convertTo_v3(coeff, xmcdaData)
          
          element_v3 <-.jnew("org/xmcda/LinearConstraint$Element")
          element_v3$setCoefficient(coeff_v3)
          
          for(crit in as.list(xmcdaData$criteria))
          {
            if(crit$id() == critID)
            {
              element_v3$setUnknown(crit)
              break 
            }
          }
          
          elements_v3$add(element_v3)
        }
        
        constr_v3$setElements(elements_v3)
        constraints_v3$add(constr_v3)
      }
      
      xmcdaData$criteriaLinearConstraintsList$add(constraints_v3)
    }
  }
  return (xmcdaData)
}
