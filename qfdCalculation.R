#calculate the level of design parameta from operation environment (via QFD)
#oe ()
#qfd as matrix
calculateRequiredDesign <- function(oeData, qfd){
  numberOfDP <- ncol(qfd);
  requiredDesign <- matrix(data=0, ncol = numberOfDP);
  
  diffFuncs <- as.matrix(oeData) - 1; #the difference between initial OE and current OE
  diffFuncs <- diffFuncs[,rep(1,numberOfDP)]; #resize for caluating with qfd

  browser();  
  #required DP Levels for each functions
  requiredDP <- diffFuncs / qfd; 
  requiredDP <- requiredDP + 1;
  requiredDP[is.nan(requiredDP) | is.infinite(requiredDP)] <- 0;
  
  #calculate required DP levels to maximize the achived function.
  for(i in 1:numberOfDP){
    requiredDesign[,i] <- calcuateOptimalDPvalue(qfd[,i], requiredDP[,i]);
  }
  
  return(requiredDesign);
} 



calcuateOptimalDPvalue <- function(qfd, requiredDPvalue){
  
  result <- NULL;
  dpValues <- NULL;
  achievedFuncList <- NULL;
  
  #narrow down to only related function to the DP
  requiredDPvalue <- as.matrix(requiredDPvalue[qfd != 0]); 
  qfd <- as.matrix(qfd[qfd != 0]); 
  
  #enumerate the required values from max to min pby 0.01
  candidateDPvalues <- seq(min(requiredDPvalue), max(requiredDPvalue), 0.01);
  
  #the list of achived functions by each candidates of DP level
  for(i in candidateDPvalues){
    aCandidateDP <- i;
    diffDP <- aCandidateDP - 1;
    achievedFunc <- (diffDP * qfd) + 1;
    achievedFuncList <- c(achievedFuncList, achievedFunc);
  } 
  
  achievedFuncList <- matrix(achievedFuncList, nrow=length(qfd));
  
  #find optimal value ????
  ##difference between requiend and achieved
  result <- (requiredDPvalue[,rep(1,length(candidateDPvalues))]) - achievedFuncList;
  ##neutralize the suplus achived function 
  result[result<0] <- 0;
  ##find the minized
  result <- colSums(result);
  optimalDPValue <- candidateDPvalues[which.min(result)];
  
  return(optimalDPValue);
}






#calculate the level of function from configuration design (via QFD)
#design as ()
#qfd as matrix
calculateAchievedFunction <- function(design, qfd){
  
  numberOfFM <- nrow(qfd);
  achievedFunction <- matrix(data=0, nrow = numberOfFM);

  design <- (design - 1); #the difference from standard design
  #design <- rbind(design, design[rep(1, (numberOfFM-1)), ]); 
  design <- design[rep(1, numberOfFM), ]; 
  
  temp <- qfd;
  temp[which(temp!=0)] <- 1
  
  achievedFunction <- design * qfd;
  achievedFunction <- achievedFunction + temp;   #restore the actual level of functions
  achievedFunction <- apply(achievedFunction, 1, function(x) geoMean(x[x!=0]));

  return(achievedFunction);
} 










