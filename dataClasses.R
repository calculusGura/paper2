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


#setGeneric("test", function(object) standardGeneric("test"))
#setMethod("test", "PreparedComponentData", function(object) object@code)
#showMethowd("test")




setClass("OEData",
         representation(duration = "matrix",
                        requiredFunc = "matrix", 
                        funcImportance = "matrix"));
                        
