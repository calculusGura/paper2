evaluateAchivedFunction <- function(configurationDesign, systemInformation){
  
  qfd <- systemInformation@qfd
  startIndex <- grep("D1", colnames(configurationDesign));
  endIndex <- startIndex + nrow(productData@dpList) - 1;
  
  iterationNo <- nrow(configurationDesign);
  afResult <- NULL;

  for(i in 1:iterationNo){
    temp <- calculateAchievedFunction(configurationDesign[i,startIndex:endIndex], qfd);  
    afResult <- rbind(afResult, t(temp));
  }
  colnames(afResult) <- rownames(qfd);

  return(afResult);
}



evaluateUtility <- function (achivedFunctionByDesigns, productData, oeData){

  utilities <- NULL;
  requiredFunc <- oeData@requiredFunc;
  functionImportance <- oeData@funcImportance;
  
  numberOfOEs <- ncol(requiredFunc);
  numberOfFunc <-  ncol(achivedFunctionByDesigns);
  numberOfDesigns <- nrow(achivedFunctionByDesigns);
  
  for(i in 1:numberOfOEs){
    rqFc <- matrix(requiredFunc[,i], ncol = numberOfFunc);
    fcIm <- matrix(functionImportance[,i], ncol = numberOfFunc);
    rqFc <- rqFc[rep(1, numberOfDesigns), ];
    fcIm <- fcIm[rep(1, numberOfDesigns), ]; 

    performanceResult <- achivedFunctionByDesigns / rqFc;
    performanceResult <- evaluateFunctionPerformance(performanceResult, productData@funcList[,2]);
    performanceResult <- (performanceResult - 1);

    performanceResult <- performanceResult * fcIm;
    performanceResult <- (performanceResult + 1);
    utilities <- cbind(utilities, rowMeans(performanceResult));
  }
  
  colnames(utilities) <- paste0("OE",1:numberOfOEs);
  
  performance <- as.matrix(rowMeans(achivedFunctionByDesigns), ncol=1);
  colnames(performance) <-  "Perform";
  
  result <- cbind(performance, utilities)
  
  return(result) 
}



evaluateFunctionPerformance <- function(scoreSheet, funcChar1){
  
  scoreSheet1 <- scoreSheet;
  scoreSheet2 <- scoreSheet;

  scoreSheet1[scoreSheet1>=1] <- 0;
  scoreSheet2[scoreSheet2<1] <- 0;
  
  funcChar1 <- (1/(1-funcChar1));
  funcChar1 <- matrix(funcChar1, ncol=length(funcChar1));
  funcChar1 <- funcChar1[rep(1, nrow(scoreSheet)), ];

  scoreSheet1 <- (scoreSheet1 * funcChar1) - (funcChar1 - 1);
  
  scoreSheet1[scoreSheet1<=0] <- 0;
  scoreSheet2[scoreSheet2!=0] <- 1;
  
  result <- scoreSheet1 + scoreSheet2;
  result[which(is.nan(result))] <- ifelse(scoreSheet[which(is.nan(result))] >= 1, 1, 0);
  
  return(result);
}


