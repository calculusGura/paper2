#importing csv data of Component - DP relationships
generateSystemStructureModel <- function(csvData){
  dataLength <- nrow(csvData);
  compList <- list();
  dpList <- list();
  compCodeList <- list();
  dpCodeList <- list();
  
  for(i in 1: dataLength){
    if(csvData[i,1] !=""){
      compList <- c(compList, c(as.character(csvData[i,1]), csvData[i,2], csvData[i,3]))};
    dpList <- c(dpList, c(as.character(csvData[i,4]), csvData[i,5], csvData[i,6]));
  }  

  compList <- matrix(unlist(compList), ncol=3, byrow=TRUE);
  colnames(compList) <- c("name", "lifecycle", "cost");
  compCodeList <- paste0("C",1:nrow(compList));
  rownames(compList) <- compCodeList;
  compList <- data.frame(compList);
  compList$name <- as.character(compList$name);
  compList$lifecycle <- as.numeric(as.character(compList$lifecycle));
  compList$cost <- as.numeric(as.character(compList$cost));
 
  dpList <- matrix(unlist(dpList), ncol=3, byrow=TRUE);
  colnames(dpList) <- c("name", "importance", "costSens");
  dpCodeList <- paste0("D",1:nrow(dpList));
  rownames(dpList) <- dpCodeList;
  dpList <- data.frame(dpList);
  dpList$name <- as.character(dpList$name);
  dpList$importance <- as.numeric(as.character(dpList$importance));
  dpList$costSens <- as.numeric(as.character(dpList$costSens));
  
  compDPMatrix <- matrix(0,nrow = nrow(dpList), ncol = nrow(compList));
  rownames(compDPMatrix) <- dpCodeList;
  colnames(compDPMatrix) <- compCodeList;
  count <- 0;
  for(i in 1: dataLength){
    if(csvData[i,1] !=""){count <- count + 1;}
    compDPMatrix[i, count] <- 1;
  }  
  
  productData <- new("SystemInformation", compList = compList, dpList = dpList, compDPMatrix= compDPMatrix);
  
  return(productData);
}


#importing the csv data of QFD (including function Data)
generateQFDModel <- function(csvData, systemData){
  dataLength <- nrow(csvData);
  dpList <- systemData@dpList;
  dpCodeList <- list();
  
  funcCodeList <- paste0("F",1:dataLength);
  funcList <- csvData[,1:2];
  rownames(funcList) <- funcCodeList;
  colnames(funcList) <- c("name", "characteristic");
  funcList$name <- as.character(funcList$name);
  funcList$characteristic <- as.numeric(as.character(funcList$characteristic));

  qfd <- as.matrix(csvData[, 3:ncol(csvData)]);
  rownames(qfd) <- funcCodeList;
  for(i in 1:ncol(qfd)){
    colnames(qfd)[i] <- rownames(dpList)[which(dpList$name == colnames(qfd)[i])]}

  systemData@funcList <- funcList;
  systemData@qfd <- qfd;
  
  return(systemData);
}


#importing the csv data of Constraints (relationship between DPs)
generateConstraintModel <- function(csvData, productData){
  dataLength <- nrow(csvData);
  dpList <- productData@dpList;
  constraintList <- NULL;
 
  for(i in 1: dataLength){
    dp1 <- rownames(dpList)[apply(dpList, 1, function(x) any(x==csvData[i,1]))];
    dp2 <- rownames(dpList)[apply(dpList, 1, function(x) any(x==csvData[i,3]))];
    constraintList <- c(constraintList, dp1, as.character(csvData[i,2]), dp2);
  }  
  
  constraintList <- data.frame(matrix(constraintList, ncol=3, byrow=TRUE));
  colnames(constraintList) <- c("dpA", "relationship", "dpB");
  constraintList$dpA <- as.character(constraintList$dpA);
  constraintList$relationship <- as.character(constraintList$relationship);
  constraintList$dpB <- as.character(constraintList$dpB);
  productData@constraint <- constraintList;
  
  return(productData);
}


#importing the csv data of QFD (including function Data)
generateOEsModel <- function(csvData, productData){

  label <- csvData[,1];
  label <- as.matrix(label[-1])
  label <- apply(label, 1, function(x) 
    rownames(productData@funcList)[which(productData@funcList[1] == as.character(x))])
    
  csvData <- csvData[,-1];
  duration <- as.matrix(csvData[1,c(TRUE,FALSE)]);
  rownames(duration) <- "duration";
  csvData <- csvData[-1,];
  
  requiredFunc <- as.matrix(csvData[,c(TRUE,FALSE)]);
  rownames(requiredFunc) <- label;
  
  funcImportance <- as.matrix(csvData[,!c(TRUE,FALSE)]);
  rownames(funcImportance) <- label;
  colnames(funcImportance) <- colnames(requiredFunc);

  oeData <- new("OEData", duration = duration, requiredFunc = requiredFunc, funcImportance=funcImportance);
  return(oeData);
}





