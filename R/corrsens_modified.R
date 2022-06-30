
# Modify fitted object, as it is object from knnsense

corrsens_modified <- function(data,yName,fittedObject,sensNames=NULL) 
{
  classif <- fittedObject$classif
  if (is.null(classif)) stop('"classif" missing in fittedObject')
  
  preds <- if(classif) fittedObject$regests[,1] else fittedObject$ypreds#ORG: 
  #if(classif) preds <- fittedObject$holdoutPreds$probs[,1] else fittedObject$holdoutPreds
  #else fittedObject$ypreds
  xNames <- setdiff(names(data),c(yName,sensNames))
  xSensNames <- setdiff(names(data),c(yName))
  holdIdxs <- fittedObject$holdIdxs
  
  corrs <- NULL  # eventual output
  nCorrs <- 0
  for (sensNm in sensNames) {
    sens <- data[sensNm][holdIdxs,]
    # the case of factor sens is more complex, for 2 reasons:
    # 1. we must change the 1 or more dummies to probabilities, so
    # that cor() makes sense, and 2. if this is a categorical
    # variable, not Bernoulli, then we must call cor() on each of the
    # resulting dummies
    if (is.factor(sens)) {
      # item 1 above
      lvls <- levels(sens)
      sens <- as.numeric(sens)
      nLvls <- length(lvls)
      if (nLvls == 2) {
        frml <- paste0(sensNm,' ~ .')
        frml <- as.formula(frml)
        tmp <- glm(frml,data[xSensNames],family=binomial())
        sensProbs <- tmp$fitted.values[holdIdxs]
        corrs <- c(corrs,cor(preds,sensProbs)^2)
        nCorrs <- nCorrs + 1
        names(corrs)[nCorrs] <- sensNm
      } else {
        # item 2; want 1 col of probs for Bernoulli sens, or 1 col 
        # for each dummy in the categorical case
        tmp <- qeLogit(data[xSensNames],sensNm,holdout=NULL)
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
      corrs <- c(corrs,cor(preds,sens)^2)
      nCorrs <- nCorrs + 1
      names(corrs)[nCorrs] <- sensNm
    }
  }
  
  corrs
  
}
