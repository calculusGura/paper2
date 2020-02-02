setClass("SystemInformation",
         representation(compList = "data.frame",
                        dpList = "data.frame",
                        funcList = "data.frame",
                        compDPMatrix = "matrix",
                        qfd = "matrix",
                        constraint = "data.frame"));


setClass("PreparedComponentData",
         representation(code = "character",
                        name = "character", 
                        baseCost = "numeric",
                        preparedCompDesign = "matrix",
                        costMatrix = "matrix"));


setClass("OEData",
         representation(duration = "matrix",
                        requiredFunc = "matrix", 
                        funcImportance = "matrix"));
                        
