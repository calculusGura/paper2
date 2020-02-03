#generate Configuration Design according to each OE
generateBaseDesigns <- function(oeData, systemInformation){
  
  qfd <- systemInformation@qfd;  
  iterations <- ncol(oeData);
  noOfDPs <- ncol(qfd);
  baseDesigns <- matrix(data = 0, nrow = iterations, ncol = noOfDPs);
  colnames(baseDesigns) <- colnames(qfd);
  
  for(i in 1:iterations){
    baseDesigns[i,] <- calculateRequiredDesign(oeData[,i], qfd);
    baseDesigns[i,] <- adjustDesignByConstraints(baseDesigns[i,], systemInformation);
  };
  
  rownames(baseDesigns) <- colnames(oeData);
  
  return(baseDesigns);
}



#adjust 
adjustDesignByConstraints <- function(configurationDesign, systemInformation){
  browser();
  constraintData <- systemInformation@constraint;
  dataLength <- nrow(constraintData);
  for(i in 1:dataLength){
    dpA <- design[(constraintData[i,1])];
    dpB <- design[(constraintData[i,3])];
    const <- paste("dpA", as.character(constraintData[i,2]), "dpB");
    
    if(eval(parse(text = const))){return(design);}
    else{
      switch(constraintData[i,2],
             ">=" = {configurationDesign[(constraintData[i,1])] <- dpB;},
             "==" = {configurationDesign[(constraintData[i,1])] <- dpB;},
             "<=" = {configurationDesign[(constraintData[i,3])] <- dpA;})
      return(configurationDesign);}
  }  
}