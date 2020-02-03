generatePreparedComponents <- function(baseDesigns, systemInformation){
  
  compDPMatrix <- systemInformation@compDPMatrix
  numberOfBelongedDPs <- colSums(compDPMatrix);
  numberOfDesign <- nrow(baseDesigns); 
  preparedComponentList <- list();
  start <- 1;
  count <- 1;
  
  browser();
  for(belongedDP in numberOfBelongedDPs){
    end <- start + belongedDP - 1;
    compData <- systemInformation@compList[count,];
    compDesignList <- as.matrix(baseDesigns[,start:end], byrow=TRUE);
    colnames(compDesignList) <- rownames(compDPMatrix)[start:end];
    rownames(compDesignList) <- c(paste0("OE", 1:numberOfDesign));
    compDesignList <- unique(compDesignList);
    
    preparedComponent <- new("PreparedComponentData", 
                             code = rownames(compData), 
                             name = compData[,1],
                             baseCost = compData[,3],
                             preparedCompDesign = compDesignList);
    
    preparedComponent <- buildCompCostMatrix(preparedComponent, systemInformation);
    preparedComponentList <- c(preparedComponentList, preparedComponent);
    start <- end + 1; 
    count <- count + 1;
  }
  
  
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