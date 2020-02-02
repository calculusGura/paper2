#setwd("C:/Users/buzzs/Dropbox/paper2"); ##windows
#setwd("/home/ubuntu/paper2");  ##aws ssh
#setwd("/Users/Jeong/Dropbox/paper2"); ##osx

setwd("/home/buzzsongs/paper2"); ##chrome os

rm(list = ls())

if (!require(EnvStats)) install.packages("EnvStats");
if (!require(ggplot2)) install.packages("ggplot2");
library("EnvStats");
library("ggplot2");


source("dataClasses.R");
source("generateInformationModel.R");

csvData1 <- read.csv("./productData/productData1.csv");
systemInformationData <- generateSystemStructureModel(csvData1);

csvData2 <- read.csv("./productData/QFD1.csv");
systemInformationData <- generateQFDModel(csvData2, systemInformationData);

csvData3 <- read.csv("./productData/constraint1.csv");
systemInformationData <- generateConstraintModel(csvData3, systemInformationData);

csvData4 <- read.csv("./productData/oes1.csv");
oeData <- generateOEsModel(csvData4, systemInformationData);



source("configurationDesign.R");
#source("qfdCalculation.R");
#baseDesigns <- generateBaseDesigns(oeData@requiredFunc, systemInformationData);
preparedComponentList <- generatePreparedComponents(baseDesigns, systemInformationData);


configurations <- generateConfigurationDesignList(1000, systemInformationData, preparedComponentList);
configurations <- checkDesignFeasibility(configurations, systemInformationData@constraint);






source("performanceEvaluation.R");
source("costEvaluation.R");
achivedFunction <- evaluateAchivedFunction(configurations, productData);
initialBuildCost <- evaluateBuildCost(configurations, preparedComponentList);
utilities <- evaluateUtility(achivedFunction, productData, oeData);


result <- cbind(configurations, achivedFunction, initialBuildCost, utilities);

source("scenario.R");

scenarioList <- generateLifecycleScenarioList(oeData, 20, 1000);


