

# qeFair*() arguments:

#    data:  dataframe, training set; class labels col is a factor; other
#       columns may be factors
#    yName:  column name for outcome variable; vector indicates
#       regression, factor classification 
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
 
#       ypreds:  R factor instance of predicted class labels, one element f
#          for each row of newx 
#       conditprobs:  vector/matrix of class probabilities; in the 2-class
#          case, a vector, the probabilities of Y = 1
 
#########################  qeFairSVM ()  #################################

qeFairSVM <- function(data,yName,expansion,sensNames=NULL,
   holdout=floor(min(1000,0.1*nrow(data))))
{
   require(qeML)

   ycol <- which(names(data) == yName)
   y <- data[,ycol]
   if (!is.factor(y)) stop('for classification problems')
   if (length(levels(y)) > 2) stop('dichtomous Y only')

   nonSensNames <- setdiff(names(data),sensNames)
   data1 <- data[nonSensNames]

   # standard qe*-series code for ML methods needing numeric X
   trainRow1 <- getRow1(data1,yName)  # sample row, to supply names etc.
   if (!is.null(holdout)) splitData(holdout,data1)
   # mainly, call factorsToDummies if needed; the saved factorsInfo will
   # be in xyc, as will the new X in matrix form
   xyc <- getXY(data1,yName,xMustNumeric=TRUE,classif=FALSE,
      makeYdumms=TRUE)
   x <- xyc$x
   colnamesX <- colnames(x)
   xm <- as.matrix(x)

   # need to update lambdas re X dummies
   expandDeweightPars <- expandLambdas(data1,yName,expandPars)

   # scale X data
   xm <- scale(xm)
   scalePars <- list(
      ctr=attr(xm,'scaled:center'),
      scl=attr(xm,'scaled:scale'))

   factorsInfo <- xyc$factorsInfo
   if (!is.null(factorsInfo)) attr(xm,'factorsInfo') <- factorsInfo
   y <- xyc$y
   if (!is.numeric(y)) stop('Y must be numeric')

   xpx <- t(xm) %*% xm
   xpx <- xpx / nrow(xm)  # now all diags are 1.0
   xpy <- t(xm) %*% y  
   xpy <- xpy / nrow(xm)  # retain scale

   # compute diag perturbation
   lambdaVars <- names(lambdas)
   d <- rep(0,ncol(xm))
   names(d) <- colnames(x)
   d[lambdaVars] <- unlist(lambdas)

   # solve for beta-hats
   bhat <- solve(xpx + diag(d)) %*% xpy
   if (any(is.na(bhat))) {
      subs <- which(is.na(bhat))
      print('NA values in bhat:')
      print(names(bhat)[subs])
      stop('exiting due to bhat NAs')
   }
   bhat <- as.vector(bhat)

   srout <- list(bhat=bhat,
      ctr=attr(xm,'scaled:center'),scl=attr(xm,'scaled:scale'),ybar=ybar)
   srout$classif <- FALSE
   srout$lambdas <- lambdas
   srout$sensNames <- sensNames
   srout$factorsInfo <- factorsInfo
   srout$trainRow1 <- trainRow1
   class(srout) <- c('qeFairSVM ')
   if (!is.null(holdout)) {
      predictHoldoutFair(srout)
      srout$holdIdxs <- holdIdxs
   } else srout$holdIdxs <- NULL
   if (!is.null(sensNames)) {
      data2 <- data1
      srout$corrs <- corrsens(data,yName,srout,sensNames)
   }
      
   srout
}

predict.qeFairSVM  <- function(object,newx)
{
   if (!regtools::allNumeric(newx)) 
      newx <- qeML:::setTrainFactors(object,newx)
   xyc <- getXY(newx,NULL,TRUE,FALSE,object$factorsInfo,makeYdumms=TRUE)
   if (is.vector(newx)) {
      nr <- 1
   } else{
      nr <- nrow(newx)
   } 
   newx <- matrix(xyc$x,nrow=nr)
   newx <- scale(newx,center=object$ctr,scale=object$scl)

   preds <- newx %*% object$bhat + object$ybar
   preds
}
 
