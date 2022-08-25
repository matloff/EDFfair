########################  prepData  #############################

# common prep for all qeFair*(); here the S and C sets are as in the
# paper, S being the set of sensitive variables, to be excluded from the
# analysis, and C being the set of variables that we will use but
# deweight, due to assumed correlation with S

# to be called from a qeFair*() function that has 'data', 'yName', 
# 'sensNames' and 'deweightPars' as (some of) its arguments

# the argument zzz is just a placeholder; apparently gtools:defmacro()
# needs at least one argument other than expr

# value:  this is a macro, setting the following

#    nonSensNames:  original data names, minus those in S
#    data1:  original 'data' without S columns
#    x:  "X" part of data1, run through factorsToDummies()
#    factorsInfo:  info on factors, attribute of output of
#       factorsToDummies(); note that omitLast is set to FALSE
#    xm: scaled version of x; uses mmscale() instead of scale(), to avoid
#       inordinate influence of dummies that are mostly 1s or mostly 0s
#    minmax:  attributes produced by mmscale()
#    deweightNames, deweightVals: names of the C variables,
#       and their deweighting parameters AFTER factorsToDummies()
#       if any variables in S are factors
#    y:  "Y" vector/factor from original 'data'
#    data2: as.data.frame(cbind(x,y))
#    trainRow1:  needed for names used in later prediction, standard qeML

# note that prepData() does not call splitData(); this is assumed to be
# done by the qeML function

prepData <- defmacro(zzz,scaling='mmscale',expr=
   {
      ycol <- which(names(data) == yName)
      y <- data[,ycol]

      nonSensNames <- setdiff(names(data),sensNames)
      data1 <- data[nonSensNames]

      ycol <- which(names(data1) == yName)
      x <- data1[,-ycol]
      x <- factorsToDummies(x,omitLast=TRUE)
      factorsInfo <- attr(x,'factorsInfo')

      # scaling
      if (!is.null(scaling)) {
         if (scaling == 'mmscale') {
            xm <- mmscale(x)
            scalePars <- attr(xm,'minmax')
         } else {  # assumed to be ordinary R scale()
            xm <- scale(x)
            scalePars <- list(
                  ctr=attr(xm,'scaled:center'),
                  scl=attr(xm,'scaled:scale')
               )
         }
      }

      xm.df <- as.data.frame(xm)
      data2 <- cbind(xm.df,y)
      names(data2)[1:ncol(xm)] <- colnames(x)

      deweightPars <- expandDeweightPars(data,yName,deweightPars)
      deweightNames <- names(deweightPars)
      deweightVals <- unlist(deweightPars)
   
      trainRow1 <- getRow1(data1,yName)
   }
)

########################  predictHoldoutFair  #############################

# modified version of qeML::predictHoldout()

# globals:  trn, tst, yName, preds

# res is the return value of qeFair*()

predictHoldoutFair <- defmacro(res,
   expr={
      ycol <- which(names(tst) == yName);
      ycolData <- which(names(data) == yName);
      tstx <- tst[,-ycol,drop=FALSE];
      attr(tstx,'noNeedPrepNewx') <- TRUE
      preds <- predict(res,tstx);
      res$holdoutPreds <- preds;
      if (res$classif) {
         if (is.numeric(preds)) {
            probs <- preds
            tmp <- round(preds)
            # predClasses <- res$yLevels[tmp+1]
            predClasses <- ifelse(tmp,yesYVal,noYVal)
         }
         preds <- list(probs=probs,predClasses=predClasses)
         res$testAcc <- mean(preds$predClasses != tst[,ycol])
         res$baseAcc <- 1 - max(table(data[,ycolData])) / nrow(data)
         # res$confusion <- regtools::confusion(tst[,ycol],preds$predClasses)
         doOneConfMatrix <- function(sensName) 
         {
            tmp <- sensName
            sens <- data[[tmp]][idxs]
            table(tst[,ycol],preds$predClasses,sens)
         }
         # res$sensConfusion <- lapply(sensNames,doOneConfMatrix)
      } else {
         res$testAcc <- mean(abs(preds - tst[,ycol]))
         trnYcol <- which(names(trn) == yName)
         res$baseAcc <-  mean(abs(tst[,ycol] - mean(trn[,trnYcol])))
      }
   }
)

########################  calcSensConfusion  #############################

# calculate the confusion matrix for each factor in sensNames (assumed
# to all be factors)

calcSensConfusion <- function(data,dataNoSens,yName,idxs,preds,sensNames) 
{
   ycol <- which(names(dataNoSens) == yName)
   tst <- dataNoSens[idxs,]

   doOneConfMatrix <- function(sensName) 
   {
      tmp <- sensName
      sens <- data[[tmp]][idxs]
      table(tst[,ycol],preds$predClasses,sens)
   }

   lapply(sensNames,doOneConfMatrix)
         
}

########################  nonoyesyes  #############################

# calculate conditional probabilities in a 2x2 table output by
# calcSensConfusion(); say table is rbind(c(u,v),c(w,x)); then output
# c(x/(w+x),x/(v+x)); tableNum is the third coordinate of the 3-dim
# array output by calcSensConfusion()

nonoyesyes <- function(cSCout,tableNum) 
{
   ary <- cSCout[[1]]
   tbl <- ary[,,tableNum]
   x <- tbl[2,2]; w <- tbl[2,1]; v <- tbl[1,2]; u <- tbl[1,1]
   l <- list(
      NoPredNoActual=u/(u+w),
      NoActualNoPred=u/(u+v),
      YesPredYesActual=x/(v+x),
      YesActualYesPred=x/(x+w))     
   c(l[[1]],l[[2]],l[[3]],l[[4]])
}

########################  dispTreat  #############################

# main privacy criterion, based on avoiding disparate treatment (not
# disparate impact); ideally, E(Y | X,S) should be independent of S, and
# we need a measure of how close we meet that ideal 

# our measure is based on the fact the disparate treatment means that
# individuals of similar X should have similar predicted Y

# for now, we require that Y (coded 0,1) be binary or scalar numeric,
# and that S be categorical; the procedure is this:

# at each X_i, we find the k-nearest neighbors of X_i (including X_i);
# in each neighborhood, we partition the points according to their
# values of S; let m denote the number of levels of S; then the
# partitioning gives us m groups (some possibly empty); then calculate
# mean predicted Y for each group; then compute the ratio of mean in
# the various groups, relative to a reference level, for now levels(S)[1];
# finally compute the mean ratios across all neighborhoods; the closer
# these are to 1.0, the fairer the analysis

dispTreat <- function(data,yName,sName,classif,qeFairOut,k,nSam,yYesName)
{
   ycol <- which(names(data) == yName)
   yvec <- data[,ycol]
   if (is.factor(yvec) && length(levels(yvec)) == 2)
      if (length(yYesName) > 0) {
         whichYes <- which(yvec == yYesName)
         yvec <- as.character(yvec)
         yvec[whichYes] <- '1'
         yvec[-whichYes] <- '0'
         yvec <- as.factor(yvec)
         data[,ycol] <- yvec
      } else stop('empty yYesName')
   scol <- which(names(data) == sName)
   data <- data[sample(1:nrow(data),nSam),]
   dataXS <- data[,-c(ycol)]
   dataX <- data[,-c(ycol,scol)]
   dataXdumms <- factorsToDummies(dataX)
   Y <- data[[yName]]
   S <- data[[sName]]
   if (is.numeric(S)) S <- as.factor(S)
   sLevels <- levels(S)
   nSlevels <- length(sLevels)
   n <- nrow(data)

   predictGroup <- function(grp)
   {
      grpIdxs <- bySlevel[[grp]]
      preds <- predict(qeFairOut,dataX[grpIdxs,])
      if (classif) preds <- preds$probs[,2]
      preds
   }

   findNbhd<- function(xrow) 
   {
      require(FNN)
      xrow <- matrix(xrow,nrow=1)
      tmp <- FNN::get.knnx(data=dataXdumms,query=xrow,k=k)
      tmp$nn.index[,1:k]
   }

   ratioMeans <- matrix(0,nrow=n,ncol=nSlevels-1)
   for (i in 1:nrow(data)) {
      nearIdxs <- findNbhd(dataXdumms[i,])  # indices of the neighborhood
      bySlevel <- NULL
      bySlevel <- 
         split(nearIdxs,S[nearIdxs])  # one set of indices for each S value
      checkEmpty <- sapply(bySlevel,length)
      if (any(checkEmpty == 0)) next
      preds <- lapply(1:length(bySlevel),predictGroup)  
      for (g in 2:nSlevels) {
         if (sum(preds[[g]]) > 0) {
            tmp <- mean(preds[[1]]) / mean(preds[[g]])
            ratioMeans[i,g-1] <- tmp
         }
      }
   }

   colMeans(ratioMeans,na.rm=TRUE)

}

#################### expandDeweightPars() ############################

# if a variable in the sensitive set S is an R factor, it will
# eventually be converted to dummies, and its deweighting parameter
# should be expand to each of them

expandDeweightPars <- function(data,yName,deweightPars) 
{
   yCol <- which(names(data) == yName)
   dataX <- data[,-yCol]
   colnamesX <- colnames(dataX)
   newPars <- list()
   parNames <- names(deweightPars)
   # for each factor in dataX, is it in deweightPars?; if so, expand
   # deweightPars
   dataXNames <- names(dataX)
   for (i in 1:ncol(dataX)) {
      xName <- dataXNames[i]
      if (xName %in% parNames) {
         if (is.numeric(dataX[,i])) {
            newPars[xName] <- deweightPars[xName]
         } else {
            lvls <- levels(dataX[,i])
            lvls <- lvls[-length(lvls)]  # omit redundant dummy
            expandedNames <- paste0(xName,'.',lvls)
            newPars[expandedNames] <- deweightPars[[colnamesX[i]]]
         }
      }
   }
   deweightPars <- newPars
   deweightPars
}

######################### prepNewx() ################################

# remove sensitive vars, apply factorsToDummies(), scaling, etc., in
# preparation for prediction

prepNewx <- function(object,newx) 
{
   nonSensNames <- setdiff(names(newx),object$sensNames)
   newx <- newx[nonSensNames]
   newx <- qeML:::setTrainFactors(object,newx)
   newx <- 
      factorsToDummies(newx,omitLast=TRUE,factorsInfo=object$factorsInfo)
   cnames <- colnames(newx)
   sps <- object$scalePars
   if (object$scaling != 'none') {
      newx <- 
         if(object$scaling == 'mmscale') {
            mmscale(newx,sps)
         } else {
            scale(newx,center=sps$ctr,scale=sps$scl)
         }
   }
   
   colnames(newx) <- cnames
   newx
}
