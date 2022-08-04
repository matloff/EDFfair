
# here we measure the effectiveness of a fair ML

# in the measure's basic form, where the response variable Y and the
# sensitive attribute S are continuous, the measure is simply the
# correlation rho(Ypred,S), where Ypred is the predicted value of Y

# either Y and/or S are also allowed to be dichotomous (Bernoulli), in which
# case correlation is less appealing; it is handled here via conditional
# probabilities

# in the case of dichotomous Y (= 0,1), P(Y = 1 | X) is the regression
# function E(Y | X), which is what we are using in the continuous Y
# case (the predicted Y is the conditional expectation), so there is 
# nothing new here; and, with P(Y = 1 | X) we still have a continuous
# quantity (if X is continuous)

# the case of dichotomous S (categorical case is allowed too; see below) is
# different; in order to have a continuous quantity similar in role to
# just using S, we use P(S = 1 | X), again continuous if X is

# it is assumed that variables that are directly sensitive have been
# excluded from the ML analysis that returned fittedObject; only "proxy"
# variables (and other nonsensitive variables)  have been used in the
# analysis; e.g. we are worred about S = Age so it is excluded but
# YearsOfWorkExperience has not, and is in the C set

# for now, the classification case only allows dichotomous Y, still required
# to be an R factor but only 2 levels; we allow the sensitive variable
# to be categorical, though

# squared correlations are computed, as in the usual "proportion of
# Var(Y) explained by X" view

# glm() is used for estimating the conditional probabilities; k-NN etc.
# could be used instead, but it probably doesn't matter much

# arguments:

#    "qeFair*()" refers to, e.g. qeFairRidge(); these functions divide the data 
#    into training and test sets, and fit the specified function to the
#    training set; the correlation is computed on the test set, to avoid
#    overfitting

#    data: as in qeFair*(); the full dataset, not e.g. a training set
#    yName: as in qeFair*()
#    sensNames: names in 'data' of sensitive cols S, if any; 
#       excluded in ML analysis; if no data on this, corrsens() won't be
#       called
#    fittedObject: return value from qeFair*(); latter needs to have
#       been called with non-NULL holdout

corrsens <- function(data,yName,fittedObject,sensNames=NULL) 
{
   classif <- fittedObject$classif
   if (is.null(classif)) classif <- attr(data,'classif')

   SU <- inherits(fittedObject,'qeSU')
   if (SU) {
      xCols <- fittedObject$xCols
      sensCols <- fittedObject$sensCols
      holdIdxs <- fittedObject$holdIdxs
   }

   preds <- 
      if (SU) fittedObject$holdoutPreds 
      else if (classif %% is.numeric(fittedObject$holdoutPreds))
         fittedObject$holdoutPreds
      else if(classif && is.list(fittedObject$holdoutPreds))
            fittedObject$holdoutPreds$probs[,1]
      else fittedObject$holdoutPreds

   xNames <- setdiff(names(data),c(yName,sensNames))
   allX <- setdiff(names(data),c(yName))
   holdIdxs <- fittedObject$holdIdxs

   if (SU) {
      # xCols <- attr(fittedObject,'xCols')
      # sensCols <- attr(fittedObject,'sensCols')
      xCols <- fittedObject$xCols
      sensCols <- fittedObject$sensCols
   }

   corrs <- NULL  # eventual output
   nCorrs <- 0
   # loop through all the elements of S
   for (sensNm in sensNames) {
      sens <- data[sensNm][holdIdxs,]

      # the case of factor sens is more complex, for 2 reasons:
      # 1. we must change the 1 or more dummies to probabilities, so
      # that cor() makes sense, and 2. if this is a categorical
      # variable, not Bernoulli, then we must call cor() on each of the
      # resulting dummies

      # say for example S consists of Age and Race, with say 5
      # categories for the latter; that will mean S will be a vector of
      # 6 components, 1 continuous and 5 dichotomous; also, if in the input
      # data Race is an R factor, we need to convert it to 5 dummies

      if (is.factor(sens)) {

         lvls <- levels(sens)
         sens <- as.numeric(sens)
         nLvls <- length(lvls)
         if (nLvls == 2) {  # sens is dichotomous
            # set up R formula to be used in glm() call
            frml <- paste0(sensNm,' ~ .')
            frml <- as.formula(frml)
            tmp <- glm(frml,data[allX],family=binomial())
            sensProbs <- tmp$fitted.values[holdIdxs]
            corrs <- c(corrs,cor(preds,sensProbs)^2)
            nCorrs <- nCorrs + 1
            names(corrs)[nCorrs] <- sensNm
         } else {  # sens is categorical
            # qeLogit does handle categorical Y; a glm() call is made
            # for each class, results in glmOuts
            tmp <- qeLogit(data[allX],sensNm,holdout=NULL)
            for (i in 1:length(tmp$glmOuts)) {
               glmout <- tmp$glmOuts[[i]]
               sens <- glmout$fitted.values[holdIdxs]
               corrs <- c(corrs,cor(preds,sens)^2)
               nCorrs <- nCorrs + 1
               nm <- paste0(sensNm,'.',lvls[i])
               names(corrs)[nCorrs] <- nm
            }
         }
      } else {
         if (is.matrix(preds)) preds <- as.vector(preds)
         corrs <- c(corrs,cor(preds,sens)^2)
         nCorrs <- nCorrs + 1
         names(corrs)[nCorrs] <- sensNm
      }
   }

   corrs

}

