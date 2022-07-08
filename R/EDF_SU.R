

 
qeSU <- function(data,yName,deweightPars,sensNames,suFtn='frrm',
   yesYVal=0,holdout=floor(min(1000,0.1*nrow(data))))
{

   require(qeML)
   require(fairml)

   data <- na.exclude(data)

   unfairness <- deweightPars$unfairness
   suFtn <- get(suFtn)

   if (!is.null(holdout)) {
      holdIdxs <- sample(1:nrow(data),holdout)
      trn <- data[-holdIdxs,]
      tst <- data[holdIdxs,]
   } else {
      trn <- data
      holdIdxs <- NULL
   } 

   # where are they in the columns?
   allNames <- names(trn)
   findidx <- function(sn) grep(sn,allNames)
   sensCols <- sapply(sensNames,findidx)
   yCol <- which(names(trn) == yName)
   xCols <- setdiff(1:ncol(trn),union(yCol,sensCols))

   # fairml quirk: integer isn't considered numeric
   for (i in 1:ncol(trn)) {
      trnCol <- trn[,i]
      if (is.integer(trnCol)) trn[,i] <- as.double(trnCol)
   }
   suOut <- suFtn(trn[,yCol],trn[,xCols],trn[,sensCols],unfairness)

   suOut$unfairness <- unfairness
   suOut$sensNames <- sensNames
   suOut$holdIdxs <- holdIdxs

   classif <- identical(suFtn,fgrrm)
   suOut$classif <- classif
   attr(data,'classif') <- classif
   suOut$holdIdxs <- holdIdxs
   suOut$yesYVal <- yesYVal
   suOut$noYVal <- setdiff(levels(data[,yCol]),yesYVal)

   suOut$sensCols <- sensCols
   suOut$xCols <- xCols
   suOut$yCol <- yCol

   class(suOut) <- c('qeSU',class(suOut))

   if (!is.null(holdout)) {
      preds <- predict(suOut,tst[,xCols],tst[,sensCols])
      suOut$holdoutPreds <- preds
      if (!classif) 
         suOut$testAcc <- mean(abs(tst[,yCol] - preds))
      else {
         predClasses <- round(preds)
         predClasses <- 
            ifelse(predClasses,suOut$yesYVal,suOut$noYVal)
         suOut$testAcc <- mean(predClasses != tst[,yCol])
      }
      suOut$holdIdxs <- holdIdxs
   } else suOut$holdIdxs <- NULL

   if (!is.null(sensNames) && !is.null(holdout)) {
      suOut$corrs <- corrsens(data,yName,suOut,sensNames)
   }

   suOut
}

predict.qeSU <- function(object,newx,newsens)
{
   processNewx <- is.null(attr(newx,'noNeedPrepNewx'))
   if (processNewx) newx <- prepNewx(object,newx)

   # fairml quirk: integer isn't considered numeric
   for (i in 1:ncol(newx)) {
      newxCol <- newx[,i]
      if (is.integer(newxCol)) newx[,i] <- as.double(newxCol)
   }
   class(object) <- class(object)[-1]
   predict(object,newx,newsens)
}
 
