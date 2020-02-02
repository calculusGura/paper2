#calculate the level of design parameta from operation environment (via QFD)
#oe ()
#qfd as matrix
calculateRequiredDesign <- function(oe, qfd){
  
  numberOfDP <- ncol(qfd);
  requiredDesign <- matrix(data=0, ncol = numberOfDP);
  
  oe <- (oe - 1); #the difference from initial OE
  
  for(i in 1:numberOfDP){
    temp <- oe / qfd[,i];
    browser();
    if(sum(temp) != 0)
    {requiredDesign[1,i] <- max(temp[temp != 0])};
  }
  
  requiredDesign <- requiredDesign + 1;  #restore the actual level of DPs
  browser();
  return(requiredDesign);
} 