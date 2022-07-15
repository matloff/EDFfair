
# qeFair*() arguments:

#    data:  dataframe, training set; class labels col is a factor; other
#       columns may be factors
#    yName:  column name for outcome variable; vector indicates
#       regression, factor classification 
#    sensNames:  sensitive variables to be excluded from the ML analysis
#    holdout:  size of holdout set, if any
#    possible algorithm-specific options

# value:

#    see individual functions below


#########################  qeFairKNN()  #################################

qeFairKNN <- function(data,yName,
   deweightPars,
   sensNames=NULL,
   yesYVal=NULL,k=25,scaleX=TRUE,
   holdout=floor(min(1000,0.1*nrow(data))))
{
   require(qeML)

   scaling <- if(scaleX) 'scale' else 'none'
   prepData(scaling=scaling)

   nonSensNames <- setdiff(names(data),sensNames)
   data1 <- data[nonSensNames]

   classif <- is.factor(yName)
   if (classif) classNames <- levels(yName)

   deweightNames <- names(deweightPars)
   deweightVals <- unlist(deweightPars)
   expandVars <- deweightNames
   expandVals <- 1/deweightVals 

   knnout <- qeKNN(data1,yName,k,yesYVal=yesYVal,
      expandVars=expandVars,expandVals=expandVals,,
      holdout=NULL)

   srout <- list(knnout=knnout)
   srout$factorsInfo <- knnout$factorsInfo
   srout$classif <- classif
   srout$deweightNames <- deweightNames
   srout$deweightVals <- deweightVals
   srout$sensNames <- sensNames
   srout$trainRow1 <- trainRow1
   class(srout) <- c('qeFairKNN')
   srout$scalePars <- scalePars
   srout$yesYVal <- yesYVal
   if (!is.null(yesYVal)) {
      lvlsY <- levels(data1[,yName])
      noYVal <- lvlsY[3 - which(lvlsY==yesYVal)]
      srout$noYVal <- noYVal
   }
   if (!is.null(holdout)){
      idxs <- sample(1:nrow(data1),holdout)
      srout$holdIdxs <- idxs
      trn <- data1[-idxs,]
      tst <- data1[idxs,]
      ycol <- which(colnames(tst) == yName)
      if (classif) tst[,ycol] <- as.integer(tst[,ycol] == yesYVal)
      predictHoldoutFair(srout)
      srout$corrs <- corrsens(data,yName,srout,sensNames)
   }
   srout
}

# predict() arguments:

#    object:  output from q*()
#    newx:  data frame of points to be predicted
#    needsSetup:  TRUE for new external data points, FALSE for holdout
 
# value:  R list with components as follows:
 
#    classification case:

#       ypreds:  R factor instance of predicted class labels, one element f
#          for each row of newx 
#       conditprobs:  vector/matrix of class probabilities; in the 2-class
#          case, a vector, the probabilities of Y = 1
 
#    regression case:

#       vector of predicted values

predict.qeFairKNN <- function(object,newx,needsSetup=TRUE)
{

   # remove the sensitive variables, if any
   sens <- object$sensNames
   nonsens <- setdiff(colnames(newx),sens)
   newx <- newx[,nonsens]

   if (needsSetup && !is.null(object$factorsInfo)) 
      newx <- factorsToDummies(newx,TRUE,object$factorsInfo)

   if (needsSetup) {
      sps <- object$scalePars
      newx <- scale(newx,center=sps$ctr,scale=sps$scl)
   }
   knnout <- object$knnout

   # have already scaled and dealt with factors, so turn that off
   if (needsSetup) {
      knnout$scalePars <- NULL
      knnout$factorsInfo <- NULL
   }
   
   
   preds <- predict(knnout,newx)
   as.vector(preds)
}
 

