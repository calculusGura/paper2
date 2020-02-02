#generate Configuration Design according to each OE
generateBaseDesigns <- function(oeData, systemInformation){
  
  qfd <- systemInformation@qfd;  
  iterations <- ncol(oeData);
  noOfDPs <- ncol(qfd);
  baseDesigns <- matrix(data = 0, nrow = iterations, ncol = noOfDPs);
  colnames(baseDesigns) <- colnames(qfd);
  
  for(i in 1:iterations){
    baseDesigns[i,] <- calculateRequiredDesign(oeData[,i], qfd);
    baseDesigns[i,] <- adjustDesignByConstraint(baseDesigns[i,], systemInformation);
    };
  
  rownames(baseDesigns) <- colnames(oeData);
  
  return(baseDesigns);
}




#adjust 
adjustDesignByConstraint <- function(design, productData){
  browser();
  constraintData <- productData@constraint;
  dataLength <- nrow(constraintData);
  for(i in 1:dataLength){
    dpA <- design[(constraintData[i,1])];
    dpB <- design[(constraintData[i,3])];
    const <- paste("dpA", as.character(constraintData[i,2]), "dpB");

    if(eval(parse(text = const))){return(design);}
    else{
      switch(constraintData[i,2],
             ">=" = {design[(constraintData[i,1])] <- dpB;},
             "==" = {design[(constraintData[i,1])] <- dpB;},
             "<=" = {design[(constraintData[i,3])] <- dpA;})
      return(design);}
  }  
}




generatePreparedComponents <- function(baseDesigns, productData){

  compDPMatrix <- productData@compDPMatrix
  numberOfBelongedDPs <- colSums(compDPMatrix);
  numberOfDesign <- nrow(baseDesigns); 
  preparedComponentList <- list();
  start <- 1;
  count <- 1;
  
  browser();  
  for(belongedDP in numberOfBelongedDPs){
    end <- start + belongedDP - 1;
    compData <- productData@compList[count,];
    compDesignList <- as.matrix(baseDesigns[,start:end], byrow=TRUE);
    colnames(compDesignList) <- rownames(compDPMatrix)[start:end];
    rownames(compDesignList) <- c(paste0("OE", 1:numberOfDesign));
    compDesignList <- unique(compDesignList);
    
    preparedComponent <- new("PreparedComponentData", 
                             code = rownames(compData), 
                             name = compData[,1],
                             baseCost = compData[,3],
                             preparedCompDesign = compDesignList);
    
    preparedComponent <- buildCompCostMatrix(preparedComponent, productData);
    preparedComponentList <- c(preparedComponentList, preparedComponent);
    start <- end + 1; 
    count <- count + 1;}
  
  names(preparedComponentList) <- colnames(compDPMatrix);
  
  return(preparedComponentList);
}



buildCompCostMatrix <- function(preparedComponent, productData){
  preparedCompDesign <- preparedComponent@preparedCompDesign;
  compCost <- preparedComponent@baseCost;
  dpData <- productData@dpList[colnames(preparedComponent@preparedCompDesign),];
  #dpImportance <- matrix(dpData[,2], nrow= nrow(preparedCompDesign), ncol=nrow(dpData), byrow=TRUE);
  dpSensitivity <- matrix(dpData[,3], nrow= nrow(preparedCompDesign), ncol=nrow(dpData), byrow=TRUE);

  preparedCompDesign <- (preparedCompDesign - 1);
  #preparedCompDesign <- preparedCompDesign * dpImportance * dpSensitivity;
  preparedCompDesign <- preparedCompDesign * dpSensitivity;
  preparedCompDesign <- rowMeans(preparedCompDesign);
  
  costMatrix <- matrix(preparedCompDesign, ncol = length(preparedCompDesign));
  costMatrix <- costMatrix[rep(1, ncol(costMatrix)), ]; 
  colnames(costMatrix) <- names(preparedCompDesign);
  rownames(costMatrix) <- names(preparedCompDesign);

  for(i in 1:nrow(costMatrix)){
    costMatrix[i, ] <- abs(costMatrix[i, ] - costMatrix[i, i]);}

  preparedCompDesign <- (1 + preparedCompDesign);
  costMatrix <- costMatrix + diag(preparedCompDesign);
  costMatrix <- costMatrix * compCost;

  preparedComponent@costMatrix <- costMatrix;
  
  return(preparedComponent);
}




generateConfigurationDesignList <- function(iteration, productData, preparedComponentList){
  configurations <- NULL;
  compList <- productData@compList;
  dpList <- productData@dpList;
  
  numberOfComp <- nrow(compList);
  numberOfDP <-  nrow(dpList); 
  columnName <- c(rownames(compList), rownames(dpList));
  
  for(i in 1:iteration){
    configurations <- c(configurations, generateConfigurationDesign(compList, preparedComponentList))};
  
  configurations <- matrix(configurations, ncol = (numberOfComp + numberOfDP), byrow = TRUE);
  configurations <- unique(configurations);
  colnames(configurations) <- columnName
  configurations <- data.frame(configurations);
  
  configurations[,1:numberOfComp] <- lapply(configurations[,1:numberOfComp], as.character)
  configurations[,(numberOfComp+1):(numberOfComp+numberOfDP)] <- lapply(configurations[,(numberOfComp+1):(numberOfComp+numberOfDP)], as.character)
  configurations[,(numberOfComp+1):(numberOfComp+numberOfDP)] <- lapply(configurations[,(numberOfComp+1):(numberOfComp+numberOfDP)], as.numeric)
  
  return(configurations);
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

