evaluateBuildCost <- function(configurationDesign, preparedComponentList){
  
  costResult <- NULL;
  
  for(aComp in preparedComponentList){
    compResult <- configurationDesign[,aComp@code];
    compResult <- sapply(compResult, function(x) findCostOnMatrix(aComp@costMatrix, x, x));
    costResult <- cbind(costResult, compResult);}
  
  costResult <- rowSums(costResult);
  costResult <- matrix(costResult);
  colnames(costResult) <- "IBC"
  
  return(costResult);
}


findCostOnMatrix <- function(costMatrix, designA, designB){
  result <- costMatrix[designA, designB];
  return(result);
}

