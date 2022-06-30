

# 
# knnsense() arguments: 
#   data: dataframe, raw datasets
#   yName:  column name for outcome variable; vector indicates
#   regression, factor classification 
#   sensNames:  sensitive variables to be excluded from the ML analysis
#   possible algorithm-specific options
#   deweightPars： list of CVars=CVals
#   CVars: proxies variables chosen.
#   CVals: deweight values chosen for each CVars
#   holdout:  size of holdout set, if any
#   
# values: 
#   $testacc : overall probability of misclassification on test set
#   





qeFairKNN <- function(data, yName, deweightPars=NULL,sensNames=NULL,  holdout=floor(min(1000,0.1*nrow(data))),holdIdxs=NULL,kmax=25)
{
  nonSensNames <- setdiff(names(data),sensNames)
  data1 <- data[nonSensNames]
  if (!is.null(deweightPars)){
  CVars <- names(deweightPars)
  CVals <- as.numeric(deweightPars)
  expand <- ExpandVars(data,Cvar=CVars,Weight=CVals)
  expandVars <- expand$var
  expandVals <- expand$val
  }
  
  
  classif <- is.factor(data1[[yName]])
  if (classif) 
    classNames <- levels(yName)
  trainRow1 <- getRow1(data1, yName)
  if (!is.null(holdout) & is.null(holdIdxs)){
    holdIdxs <- splitData(holdout, data1)
  }
  if (!is.null(holdout) & !is.null(holdIdxs)){
    data1 <- data1[-holdIdxs,]
  }
  xyc <- getXY(data1, yName, xMustNumeric = TRUE, classif = TRUE, 
               makeYdumms = TRUE)
  x <- xyc$x
  xm <- as.matrix(x)
  factorsInfo <- xyc$factorsInfo
  if (!is.null(factorsInfo)) 
    attr(xm, "factorsInfo") <- factorsInfo
  y <- xyc$y
  if (classif) {
    xy <- xyc$xy
    y <- xyc$yDumms
    classNames <- xyc$classNames
  }
  if (!is.null(deweightPars)) {
    
    testdf <- data[nonSensNames][holdIdxs,]
    testset <- getXY(testdf, yName, xMustNumeric = TRUE, classif = TRUE, 
                     makeYdumms = TRUE)
    getx <- testset$x
    gety <- testset$yDumms
    ycol <- which(names(testdf) == yName)
    testout <- kNN(xm, y, newx = getx, kmax, scaleX = TRUE, 
                   classif = classif, smoothingFtn = mean, expandVars = expandVars, expandVals = expandVals)
    testout$testAcc <- mean(testout$ypreds != encode(testdf[,ycol]))
    testout$classif <-classif
    testout$holdIdxs <- holdIdxs
    testout$baseAcc <- 1 - max(table(data[, ycol]))/nrow(data)
    testout$newx <- getx
    testout$corrs <- corrsens_modified(data,yName,testout,sensNames)
  } else{
    testdf <- data[nonSensNames][holdIdxs,]
    testset <- getXY(testdf, yName, xMustNumeric = TRUE, classif = TRUE, 
                     makeYdumms = TRUE)
    getx <- testset$x
    gety <- testset$yDumms
    ycol <- which(names(testdf) == yName)
    testout <- kNN(xm, y, newx = getx, kmax, scaleX = TRUE, 
                   classif = classif, smoothingFtn = mean)
    testout$holdIdxs <- holdIdxs
    testout$testAcc <- mean(testout$ypreds != encode(testdf[,ycol]))
    testout$classif <-classif
    testout$baseAcc <- 1 - max(table(data[, ycol]))/nrow(data)
    testout$newx <- getx
    testout$corrs <- corrsens_modified(data,yName,testout,sensNames)
    
    
  }
  # else {
  #   testout <- kNN(xm, y, newx = xm, 25, scaleX = TRUE, 
  #                  classif = classif, smoothingFtn = mean, expandVars = expandVars, expandVals = expandVals)
  # }

  testout
  }



encode<-function(x){
  as.numeric(factor(x))
}
