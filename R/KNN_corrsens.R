# values:
#   correlation between fitted values of full model and model without s but with c
knn_corrsens <- function(data,yName,deweightPars,sensNames=NULL) 
{
  full_FO <- knnsense(data,yName=yName)
  holdIdxs <- full_FO$holdIdxs
  classif <- full_FO$classif
  if (is.null(classif)) stop('"classif" missing in fittedObject')
  #prediction from modified knn
  modified_knn <- knnsense(data, sensNames, yName,deweightPars,holdIdxs=holdIdxs)
  preds <- modified_knn$regests[,1]
  #prediction from knn with s
  Spreds <- full_FO$regests[,1]
  corrs <- NULL  # eventual output
  nCorrs <- 0
  for (sensNm in sensNames) {
    sens <- data[holdIdxs,][,sensNm]
    if (is.factor(sens)) {
      # item 1 above
      lvls <- levels(sens)
      nlvls <- length(lvls)
      for (i in 1:nlvls) {
        lvl_idx <- sens==lvls[i]
        corrs <- c(corrs,cor(preds[lvl_idx],Spreds[lvl_idx])^2)
        nCorrs <- nCorrs + 1
        nm <- paste0(sensNm,'.',lvls[i])
        names(corrs)[nCorrs] <- nm
      }
    } else {
      corrs <- c(corrs,cor(preds,Spreds)^2)
      nCorrs <- nCorrs + 1
      names(corrs)[nCorrs] <- sensNm
    }
    
  }
  
  corrs
  
}
