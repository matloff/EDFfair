
# qeFair*() arguments:

#    data:  dataframe, training set; class labels col is a factor; other
#       columns may be factors
#    yName:  column name for outcome variable; vector indicates
#       regression, factor classification 
#    deweightPars: deweighting parameters, e.g. lambdas in Ridge
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

#########################  qeFairRidgeLin()  #################################

# lambdas: ridge-type L2 regularizers
 
qeFairRidgeLin <- function(data,yName,deweightPars,sensNames=NULL,
   holdout=floor(min(1000,0.1*nrow(data))))
{

   require(qeML)

   scaling <- 'scale'
   prepData(scaling=scaling)

   # since we are not calling qeLin(), need to do our own holding-out
   if (!is.null(holdout)) splitData(holdout,data1)

   # center Y
   ybar <- mean(y)
   y <- y - ybar

   xpx <- t(xm) %*% xm
   xpx <- xpx / nrow(xm)  # now all diags are 1.0
   xpy <- t(xm) %*% y  
   xpy <- xpy / nrow(xm)  # retain scale

   # compute diag perturbation
   lambdaVars <- names(deweightPars)
   d <- rep(0,ncol(xm))
   names(d) <- colnames(x)
   d[lambdaVars] <- unlist(deweightPars)

   # solve for beta-hats
   bhat <- solve(xpx + diag(d)) %*% xpy
   if (any(is.na(bhat))) {
      subs <- which(is.na(bhat))
      print('NA values in bhat:')
      print(names(bhat)[subs])
      stop('exiting due to bhat NAs')
   }
   bhat <- as.vector(bhat)

   fairLogOut$classif <- FALSE
   fairLogOut$deweightPars <- deweightPars
   fairLogOut$sensNames <- sensNames
   fairLogOut$factorsInfo <- factorsInfo
   fairLogOut$trainRow1 <- trainRow1
   fairLogOut$scaling <- scaling
   fairLogOut$scalePars <- scalePars
   class(fairLogOut) <- c('qeFairRidgeLin')
   if (!is.null(holdout)) {
      predictHoldoutFair(fairLogOut)
      srout$holdIdxs <- holdIdxs
      # srout$holdoutPreds <- predict(srout,trnx)
   } else fairLogOut$holdIdxs <- NULL

   if (!is.null(sensNames) && !is.null(holdout)) {
      data2 <- data1
      srout$corrs <- corrsens(data,yName,srout,sensNames)
   }

   srout
}

predict.qeFairRidgeLin <- function(object,newx)
{

   newx <- prepNewx(object,newx)
   if (is.vector(newx)) {
      nr <- 1
   } else{
      nr <- nrow(newx)
   } 
   ### sps <- object$scalePars
   preds <- newx %*% object$bhat + object$ybar
   preds
}
 
qeFairRidgeLog <- function(data,yName,lambdas,sensNames=NULL,
   start=NULL,nIters=10,
   holdout=floor(min(1000,0.1*nrow(data))))
{
   require(qeML)

   if (length(setdiff(names(lambdas),names(data))) > 0)
      stop('invalid feature name')

   nonSensNames <- setdiff(names(data),sensNames)
   data1 <- data[nonSensNames]

   # standard qe*-series code for ML methods needing numeric X; here we
   # have a classification problem, so getXY() will also create a dummy
   # for each level of the factor Y
   trainRow1 <- getRow1(data1,yName)
   classif <- TRUE
   if (!is.null(holdout)) splitData(holdout,data1)
   xyc <- getXY(data1,yName,xMustNumeric=TRUE,classif=classif,
      makeYdumms=TRUE)
   xy <- xyc$xy
   x <- xyc$x
   colnamesX <- colnames(x)
   xm <- as.matrix(x)
   xm <- scale(xm)

   yDumms <- xyc$yDumms  # dummies version of Y; xy is X+this
   y <- xyc$y  # original R factor version of Y
   classNames <- xyc$classNames
   nClass <- length(classNames)
   ncxy <- ncol(xy)
   nx <- ncol(x)
   nydumms <- ncxy - nx  # redundant, same as nClass
   empirClassProbs <- colMeans(yDumms)

   # need to update lambdas re X dummies
   lambdas <- Pars(data1,yName,lambdas)

   factorsInfo <- xyc$factorsInfo
   if (!is.null(factorsInfo)) attr(xm,'factorsInfo') <- factorsInfo
   y <- xyc$y

   doGlmFairRidge <- function(colI) {
      tmpDF <- cbind(x, yDumms[, colI])
      names(tmpDF)[nx + 1] <- "yDumm"
      bhat <- glmFitLambda(xm,yDumms[,colI],start=start,family=binomial(),
         lambdas,nIters) 
      if (any(is.na(bhat))) {
         subs <- which(is.na(bhat))
         print('NA values in bhat:')
         print(names(bhat)[subs])
         stop('exiting due to bhat NAs')
      }
      bhat     
   }

   bhats <- sapply(1:nydumms, doGlmFairRidge)

   srout <- list(bhats=bhats,classNames=levels(y),scalePars=scalePars)
   srout$classif <- classif
   srout$yName <- yName
   srout$lambdas <- lambdas
   srout$sensNames <- sensNames
   srout$factorsInfo <- factorsInfo
   srout$trainRow1 <- trainRow1
   class(srout) <- c('qeFairRidgeLog')
   if (!is.null(holdout)) { 
      predictHoldoutFair(srout)
      srout$holdIdxs <- holdIdxs
   } else srout$holdIdxs <- NULL
   if (!is.null(sensNames)) {
      srout$corrs <- corrsens(data,yName,srout,sensNames)
      srout$sensConfusion <- calcSensConfusion(data,data1,yName,
         srout$holdIdxs,srout$holdoutPreds,sensNames)
   }
   srout
}

predict.qeFairRidgeLog <- function(object,newx)
{
   if (!regtools::allNumeric(newx)) 
      newx <- qeML:::setTrainFactors(object,newx)
   classif <- TRUE
   xyc <- getXY(newx,NULL,TRUE,FALSE,object$factorsInfo,makeYdumms=TRUE)
   if (is.vector(newx)) {
      nr <- 1
   } else{
      nr <- nrow(newx)
   } 
   newx <- matrix(xyc$x,nrow=nr)
   newx <- scale(newx,center=object$ctr,scale=object$scl)
   newx <- cbind(1,newx)

   preds <- newx %*% object$bhats 
   preds <- 1 / (1 + exp(-preds))
   rs <- rowSums(preds)
   preds <- (1/rs) * preds
   qeML:::collectForReturn(object,preds)
}
 
## qeFairRidgeScutari <- function(data,yName,lambda,
##    holdout=floor(min(1000,0.1*nrow(data))))
## {
##    require(qeML)
## 
##    if (length(setdiff(names(lambdas),names(data))) > 0)
##       stop('invalid feature name')
## 
##    # standard qe*-series code for ML methods needing numeric X; here we
##    # have a classification problem, so getXY() will also create a dummy
##    # for each level of the factor Y
##    trainRow1 <- getRow1(data,yName)
##    classif <- is.factor(data[[yName]])
##    if (!is.null(holdout)) splitData(holdout,data)
##    xyc <- getXY(data,yName,xMustNumeric=TRUE,classif=classif,
##       makeYdumms=TRUE)
##    xy <- xyc$xy
##    x <- xyc$x
##    colnamesX <- colnames(x)
##    xm <- as.matrix(x)
##    xm <- scale(xm)
## 
##    yDumms <- xyc$yDumms  # dummies version of Y; xy is X+this
##    y <- xyc$y  # original R factor version of Y
##    classNames <- xyc$classNames
##    nClass <- length(classNames)
##    ncxy <- ncol(xy)
##    nx <- ncol(x)
##    nydumms <- ncxy - nx  # redundant, same as nClass
##    empirClassProbs <- colMeans(yDumms)
## 
##    # need to update lambdas re X dummies
##    lambdas <- expandLambdas(data,yName,lambdas)
## 
##    factorsInfo <- xyc$factorsInfo
##    if (!is.null(factorsInfo)) attr(xm,'factorsInfo') <- factorsInfo
##    y <- xyc$y
## 
##    doGlmFairRidge <- function(colI) {
##       tmpDF <- cbind(x, yDumms[, colI])
##       names(tmpDF)[nx + 1] <- "yDumm"
##       bhat <- glmFitLambda(xm,yDumms[,colI],start=start,family=binomial(),
##          lambdas,nIters) 
##       bhat     
##    }
## 
##    bhats <- sapply(1:nydumms, doGlmFairRidge)
## 
##    srout <- list(bhats=bhats,classNames=levels(y),
##       ctr=attr(xm,'scaled:center'),scl=attr(xm,'scaled:scale'))
##    srout$classif <- classif
##    srout$yName <- yName
##    srout$factorsInfo <- factorsInfo
##    srout$trainRow1 <- trainRow1
##    class(srout) <- c('qeFairRidgeScutari')
##    if (!is.null(holdout)) { 
##       predictHoldoutFair(srout)
##       srout$holdIdxs <- holdIdxs
##    } else srout$holdIdxs <- NULL
##    srout
## }
## 
## predict.qeFairRidgeScutari <- function(object,newx)
## {
##    if (!regtools::allNumeric(newx)) 
##       newx <- qeML:::setTrainFactors(object,newx)
##    classif <- TRUE
##    xyc <- getXY(newx,NULL,TRUE,FALSE,object$factorsInfo,makeYdumms=TRUE)
##    if (is.vector(newx)) {
##       nr <- 1
##    } else{
##       nr <- nrow(newx)
##    } 
##    newx <- matrix(xyc$x,nrow=nr)
##    newx <- scale(newx,center=object$ctr,scale=object$scl)
##    newx <- cbind(1,newx)
## 
##    preds <- newx %*% object$bhats 
##    preds <- 1 / (1 + exp(-preds))
##    rs <- rowSums(preds)
##    preds <- (1/rs) * preds
##    qeML:::collectForReturn(object,preds)
## }

# call to glm.fit() with lambdas

# arguments:

#    x,y,start,family: as in glm.fit()
#    lambdas: as in qeFairRidgeLin() above
#    nIters: number of iterations

#    glm.fit() being fit first, without lambdas, with the resulting
#    beta-hats then being used for initial values for our algorithm here

glmFitLambda <- function(x,y,start=NULL,family=binomial(),lambdas,nIters) 
{
   # x should already be scaled
   if (is.null(attr(x,'scaled:center')))
      stop('x must already be scaled')
   xm <- cbind(1,x)
   colnames(xm)[1] <- 'const'
   xm <- as.matrix(xm)


   z <- glm.fit(x=xm,y=y,family=family)  
   preds <- xm %*% coef(z)
   preds <- as.vector(1 / (1 + exp(-preds)))
   wts <- 1 / (preds * (1-preds))

   lambdaVars <- names(lambdas)
   d <- rep(0,ncol(xm))
   names(d) <- colnames(xm)
   d[lambdaVars] <- unlist(lambdas)
   for (i in 1:nIters) {
      xw <- sqrt(wts) * xm
      xpx <- t(xw) %*% xw
      # xpx <- t(xm) %*% diag(wts) %*% xm
      xpy <- t(xm) %*% (wts * y)
      scaleToMax1 <- max(diag(xpx))
      xpx <- xpx / scaleToMax1
      xpy <- xpy / scaleToMax1
      bhat <- solve(xpx + diag(d)) %*% xpy
      bhat <- as.vector(bhat)
      if (i < nIters) {
         preds <- as.vector(xm %*% bhat)
         preds <- 1 / (1 + exp(-preds))
         wts <- 1 / (preds * (1-preds))
      }

   }

   bhat

}
 
qeFairRidgeLog <- function(data,yName,deweightPars,sensNames=NULL,
   negativeYVal=0,holdout=floor(min(1000,0.1*nrow(data))))
{

   require(qeML)

   # data prep
   data <- na.exclude(data)
   scaling <- 'scale'
   prepData(scaling=scaling)
   dataNonSens <- data2
   yCol <- ncol(dataNonSens)
   names(dataNonSens)[yCol] <- yName

##    nonSensNames <- setdiff(names(data),sensNames)
##    dataNonSens <- data[,nonSensNames]
##    yCol <- which(yName == names(dataNonSens))
##    x <- dataNonSens[,-yCol]
##    xd <- factorsToDummies(x,omitLast=TRUE)
##    factorsInfo <- attr(xd, "factorsInfo")
##    xd <- scale(xd)
##    namesX <- colnames(xd)
##    xd <- as.data.frame(xd)
##    y <- dataNonSens[,yCol]
##    dataNonSens <- cbind(xd,y)
##    names(dataNonSens)[yCol] <- yName
##    trainRow1 <- getRow1(dataNonSens,yName)
## 
##    # deweighting pars
##    deweightPars <- expandDeweightPars(data, yName, deweightPars)
##    deweightNames <- names(deweightPars)
##    deweightVals <- unlist(deweightPars)


   # set up the lambdas
   p <- ncol(dataNonSens) - 1
   dataExtended <- rbind(dataNonSens,dataNonSens[1:p,])
   n <- nrow(dataNonSens)
   newRows <- (n+1):(n+p)
   tmp <- rep(0,p)
   names(tmp) <- namesX
   tmp[deweightNames] <- deweightVals
   newx <- as.data.frame(diag(tmp))
   names(newx) <- namesX
   newy <- rep(negativeYVal,p)
   y <- as.character(dataNonSens[,yCol])
   yExtended <- as.factor(c(y,newy))
   dataExtended[newRows,-yCol] <- newx
   dataExtended[,yCol] <- yExtended
   names(dataExtended)[yCol] <- yName

   if (!is.null(holdout)) splitData(holdout,dataExtended)

   fairLogOut <- qeLogit(dataExtended,yName,holdout=NULL)

   fairLogOut$classif <- FALSE
   fairLogOut$deweightPars <- deweightPars
   fairLogOut$sensNames <- sensNames
   fairLogOut$factorsInfo <- factorsInfo
   fairLogOut$trainRow1 <- trainRow1
   fairLogOut$scaling <- scaling
   # fairLogOut$scalePars <- scalePars
   class(fairLogOut) <- c('qeFairRidgeLog')

   if (!is.null(holdout)) {      
      predictHoldoutFair(fairLogOut)
      fairLogOut$holdIdxs <- holdIdxs
   }

   if (!is.null(sensNames) && !is.null(holdout)) {
      data2 <- data1
      fairLogOut$corrs <- corrsens(data,yName,fairLogOut,sensNames)
   }

   fairLogOut

}

predict.qeFairRidgeLog <- function(object,newx)
{

   newx <- prepNewx(object,newx)
   if (is.vector(newx)) {
      nr <- 1
   } else{
      nr <- nrow(newx)
   } 
   ### sps <- object$scalePars
   tmp <- object$glmOuts[[1]]
   class(tmp) <- c('glm','lm')
   preds <- predict(tmp,newx,type='response')
browser()


   newx %*% object$bhat + object$ybar
   preds
}
