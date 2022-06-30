
# qeFair*() arguments:

#    data:  dataframe, training set; class labels col is a factor; other
#       columns may be factors
#    yName:  column name for outcome variable; vector indicates
#       regression, factor classification 
#    selectProbs:  probabilities that specified features will be selected
#    sensNames:  sensitive variables to be excluded from the ML analysis
#    possible algorithm-specific options
#    holdout:  size of holdout set, if any

# value:

#    see individual functions below

# predict() arguments:

#    object:  output from q*()
#    newx:  data frame of points to be predicted
#    possible options
 
# value:  R list with components as follows:
 
#    classification case:

#       ypreds:  R factor instance of predicted class labels, one element f
#          for each row of newx 
#       conditprobs:  vector/matrix of class probabilities; in the 2-class
#          case, a vector, the probabilities of Y = 1
 
#    regression case:

#       vector of predicted values

#########################  qeFairRF()  #################################

# selectProbs: 
 
qeFairRF <- function(data,yName,deweightPars,sensNames=NULL,
   nTree=500,minNodeSize=10,mtry = floor(sqrt(ncol(data))),
   holdout=floor(min(1000,0.1*nrow(data))))
{
   require(qeML)

   prepData(1)

   rfout <- qeRFranger(data2,'y',
      nTree=nTree,minNodeSize=minNodeSize,mtry=mtry,
      deweightNames=deweightNames,deweightVal=deweightVals,
      holdout=holdout)

   srout <- list(rfout=rfout)
   srout$classif <- rfout$classif
   srout$deweightNames <- deweightNames
   srout$deweightVals <- deweightVals
   srout$sensNames <- sensNames
   srout$trainRow1 <- trainRow1
   srout$factorsInfo <- factorsInfo
   class(srout) <- c('qeFairRF')
   srout$holdIdxs <- rfout$holdIdxs
   srout$holdoutPreds <- rfout$holdoutPreds
   srout$testAcc <- rfout$testAcc
   srout$baseAcc <- rfout$baseAcc
   srout$confusion <- rfout$confusion

   if (!is.null(sensNames) && !is.null(holdout)) {
      srout$corrs <- corrsens(data,yName,srout,sensNames)
      if (srout$classif)
         srout$sensConfusion <- calcSensConfusion(data,data1,yName,
            srout$holdIdxs,srout$holdoutPreds,sensNames)
   }

   srout
}

predict.qeFairRF <- function(object,newx)
{
   newx <- prepNewx(object,newx)
   rfout <- object$rfout
   classif <- object$classif
   predict(rfout,newx)
}
 

