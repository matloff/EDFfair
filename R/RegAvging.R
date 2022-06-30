
# counterfactual, How would Group A fare if subjected the policies for
# Group B? 

# arguments:
# 
#    data: a data frame
#    yName: name of Y column
#    qeFtn: a ftn from the qeML pkg
#    grpName: name of the grouping column
#    yYes: for binary Y, name of the positive factor level
#    grpIntervals: for continuous grouping variable, break into
#       this mean intervals

# value: matrix of the counterfactual means

# 'data' will be grouped according to 'grpName'; then 
# within each group 'qeFtn' will be applied with 'yName' as Y, resulting
# in 'qeObj'; then each 'qeObj' will be used to predict in all the other
# groups, with the resulting average Y values returned

# for now, default values will be used for qeFtn()

regAvg <- function(data,yName,qeFtn,grpName,
   yYes=NULL,grpIntervals=NULL,naRM=TRUE,fPos=FALSE,stdErr=FALSE) 
{
   if(is.factor(data[[yName]])) {
      if (length(levels(data[[yName]])) > 2)
         stop('Y must be binary or continuous')
      if (is.null(yYes)) stop('null yYes, binary Y')
      classif <- TRUE
   } else classif <- FALSE

   grpvar <- data[[grpName]]
   if (!is.factor(grpvar)) 
      data[[grpName]] <- cut(grpvar,grpIntervals)

   grps <- split(data,data[[grpName]])
   # get XY data by removing grouping variable
   grpsXY <- lapply(grps,function(grp) {grp[[grpName]] <- NULL; grp})
   # get X data by removing Y variable
   grpsX <- lapply(grpsXY,function(grp) {grp[[yName]] <- NULL; grp})
   # do the model fits
   qeObjs <- lapply(grpsXY,
      function(grp) qeFtn(grp,yName,holdout=NULL))

   nGrps <- length(grps)
   avgs <- matrix(nrow=nGrps,ncol=nGrps)
   rownames(avgs) <- levels(data[[grpName]])
   colnames(avgs) <- levels(data[[grpName]])

   for (i in 1:nGrps)
      for (j in 1:nGrps) {
         if (i == j && !fPos) {
            # EY = E[E(Y|X)]
            tmp <- grps[[i]][[yName]]
            if (classif) tmp <- as.numeric(tmp) - 1
            avgs[i,i] <- mean(tmp,na.rm=naRM)
         } else {
            grpsxi <- grpsX[[i]]
            tmp <- predict(qeObjs[[j]],grpsxi)

            if (stdErr) {
               # for now, just print out; later make it optional, part
               # of an R list return value; and this code should be a
               # separate function
               ai <- colMeans(grpsxi)
               cvbj <- vcov(qeObjs[[j]])
               cvbj <- cvbj[-1,-1]
               bj <- coef(qeObjs[[j]])
               bj <- bj[-1]
               cvxi <- cov(grpsxi)
               ni <- nrow(grpsxi)
               term1 <- t(ai) %*% cvbj %*% ai
               term2 <- t(bj) %*% cvxi %*% bj / ni
               cat(i,j,sqrt(term1+term2),'\n')
            }

            if (!fPos) {
               if (classif) tmp <- tmp$probs[,yYes]
               avgs[i,j] <- mean(tmp,na.rm=naRM)
            } else {
               tmp <- tmp$probs[,yYes]
               num <- mean( (tmp >= 0.5) * (1 - tmp) )
               den <- mean(tmp >= 0.5)
               avgs[i,j] <- num/den
            }
         }
      }
   
   avgs
}

