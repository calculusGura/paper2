generateLifecycleScenario <- function(oeData, lifeCycle){

  oes <- oeData@duration;
  names <- colnames(oes);
  scenario <- matrix(NA, ncol=lifeCycle);
  sum <- 0;
  
  duration <- oes[1];
  scenario[1:duration] <- colnames(oes)[1];
  sum <- sum + duration;
  names <- names[-1];
  
  while((sum <= lifeCycle) | (length(names) = 0)){
    nextOE <- sample(names,1);
    duration <- oes[,nextOE];

    startYr <- sum + 1;
    endYr <- sum + duration;
    scenario[startYr:endYr] <- nextOE;

    sum <- sum + duration;
    names <- names[-which(names == nextOE)];
      
  }

  return(scenario[1:lifeCycle]) 
}


generateLifecycleScenarioList <- function(oeData, lifeCycle, numberOfScenarios){
  
  scenarioList <- NULL;
  
  for(i in 1:numberOfScenarios ){
  aScenario <- generateLifecycleScenario(oeData, lifeCycle);
  scenarioList <- rbind(scenarioList, aScenario);
  }
  
  colnames(scenarioList) <- paste0("yr",1:lifeCycle);
  rownames(scenarioList) <- paste0("sc",1:numberOfScenarios);
  return(scenarioList) 
}
