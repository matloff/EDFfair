

 
qeSU <- function(data,yName,deweightPars,sensNames,suFtn='frrm',
   holdout=floor(min(1000,0.1*nrow(data))))
{

   require(qeML)
   require(fairml)

   unfairness <- deweightPars$unfairness
   suFtn <- get(suFtn)

   if (!is.null(holdout)) {
      holdIdxs <- sample(1:nrow(data),holdout)
      trn <- data[-holdIdxs,]
      tst <- data[holdIdxs,]
   } else {
      trn <- data
   }

   # where are they in the columns?
   allNames <- names(trn)
   findidx <- function(sn) grep(sn,allNames)
   sensCols <- sapply(sensNames,findidx)
   yCol <- which(names(trn) == yName)
   xCols <- setdiff(1:ncol(trn),union(yCol,sensCols))

   suOut <- suFtn(trn[,yCol],trn[,xCols],trn[,sensCols],unfairness)

   suOut$unfairness <- unfairness
   suOut$sensNames <- sensNames
   suOut$holdIdxs <- holdIdxs

   suOut$classif <- FALSE
   attr(data,'classif') <- FALSE

   suOut$sensCols <- sensCols
   suOut$xCols <- xCols
   suOut$yCol <- yCol

   class(suOut) <- c('qeSU',class(suOut))

   if (!is.null(holdout)) {
      preds <- predict(suOut,tst[,xCols],tst[,sensCols])
      suOut$holdoutPreds <- preds
      suOut$testAcc <- mean(abs(tst[,yCol] - preds))
      suOut$holdIdxs <- holdIdxs
   } else suOut$holdIdxs <- NULL

   if (!is.null(sensNames) && !is.null(holdout)) {
      suOut$corrs <- corrsens(data,yName,suOut,sensNames)
   }

   suOut
}

predict.qeSU <- function(object,newx,newsens)
{
   class(object) <- class(object)[-1]
   predict(object,newx,newsens)
}
 
