generateConfigurationDesignList <- function(noOfDesign, systemInformationData, preparedComponentList){
  configurationList <- NULL;
  compList <- systemInformationData@compList;
  dpList <- systemInformationData@dpList;
  
  numberOfComp <- nrow(compList);
  numberOfDP <-  nrow(dpList); 
  columnName <- c(rownames(compList), rownames(dpList));
  
  for(i in 1:noOfDesign){
    configurationList <- c(configurationList, generateConfigurationDesign(compList, preparedComponentList))};
  
  configurationList <- matrix(configurationList, ncol = (numberOfComp + numberOfDP), byrow = TRUE);
  configurationList <- unique(configurationList);
  colnames(configurationList) <- columnName
  configurationList <- data.frame(configurationList);
  
  configurationList[,1:numberOfComp] <- lapply(configurationList[,1:numberOfComp], as.character)
  configurationList[,(numberOfComp+1):(numberOfComp+numberOfDP)] <- lapply(configurationList[,(numberOfComp+1):(numberOfComp+numberOfDP)], as.character)
  configurationList[,(numberOfComp+1):(numberOfComp+numberOfDP)] <- lapply(configurationList[,(numberOfComp+1):(numberOfComp+numberOfDP)], as.numeric)
  
  return(configurationList);
}



#configuration design by combining prepared components
generateConfigurationDesign <- function(compList, preparedComponents){
  codeList <- NULL;
  dpDesignList <- NULL;
  
  for(aComp in rownames(compList)){
    preparedDesign <- preparedComponents[[aComp]]@preparedCompDesign;
    
    selectedComponentCode <- sample(rownames(preparedDesign), 1);
    dpDesign <- preparedDesign[selectedComponentCode,];
    
    codeList <- c(codeList, selectedComponentCode);
    dpDesignList <- c(dpDesignList, dpDesign);}
  
  result <- c(codeList, dpDesignList);
  
  return(result);
}


#configuration design by combining prepared components
checkDesignFeasibility <- function(configurations, constraintData){
  
  dataLength <- nrow(constraintData);
  
  for(i in 1:dataLength){
    dpA <- configurations[, (constraintData[i,1])];
    dpB <- configurations[, (constraintData[i,3])];
    const <- paste("dpA", as.character(constraintData[i,2]), "dpB");
    
    result <- eval(parse(text = const));
    configurations <- configurations[which(result == TRUE),];};
  
  return(configurations);
}

