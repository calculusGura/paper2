evaluateAchivedFunction <- function(configurationList, systemInformation){

  qfd <- systemInformation@qfd
  startIndex <- grep("D1", colnames(configurationList))[1] #to beside other DPs start with D1 such as D11, D12..;
  endIndex <- startIndex + nrow(systemInformation@dpList) - 1;
  
  iterationNo <- nrow(configurationList);
  achivedFunctions <- NULL;

  for(i in 1:iterationNo){
    configurationDesign <- configurationList[i,startIndex:endIndex];
    temp <- calculateAchievedFunction(configurationDesign, qfd); #qfdCalculation
    achivedFunctions <- rbind(achivedFunctions, t(temp)); 
  }
  colnames(achivedFunctions) <- rownames(qfd);

  return(achivedFunctions);
}



evaluateUtility <- function (achivedFunctionByDesigns, systemInformation, oeData){

  utilities <- NULL;
  requiredFunc <- oeData@requiredFunc;
  functionImportance <- oeData@funcImportance;
  
  numberOfOEs <- ncol(requiredFunc);
  numberOfFunc <-  ncol(achivedFunctionByDesigns);
  numberOfDesigns <- nrow(achivedFunctionByDesigns);
  

  for(i in 1:numberOfOEs){
    browser();
    rqFc <- matrix(requiredFunc[,i], ncol = numberOfFunc);
    fcIm <- matrix(functionImportance[,i], ncol = numberOfFunc);
    rqFc <- rqFc[rep(1, numberOfDesigns), ];
    fcIm <- fcIm[rep(1, numberOfDesigns), ]; 

    performanceResult <- achivedFunctionByDesigns / rqFc;
    performanceResult <- evaluateFunctionPerformance(performanceResult, systemInformation@funcList[,2]);
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
  browser();
  scoreSheet1 <- scoreSheet;
  scoreSheet2 <- scoreSheet;

  scoreSheet1[scoreSheet1>=1] <- 0;
  scoreSheet2[scoreSheet2<1] <- 0;
  
  funcChar1 <- (1/(1-funcChar1));
  funcChar1 <- matrix(funcChar1, ncol=length(funcChar1));
  funcChar1 <- funcChar1[rep(1, nrow(scoreSheet)), ];

  scoreSheet1 <- (scoreSheet1 * funcChar1) - (funcChar1 - 1);
  browser();
  scoreSheet1[scoreSheet1<=0] <- 0;
  scoreSheet2[scoreSheet2!=0] <- 1;
  
  result <- scoreSheet1 + scoreSheet2;
  result[which(is.nan(result))] <- ifelse(scoreSheet[which(is.nan(result))] >= 1, 1, 0);
  browser();
  return(result);
}


