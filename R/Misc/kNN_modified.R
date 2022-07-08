# colnames are removed After mmscale(x), thus adding colnames after mmscale(x) and mmscale(newx)






kNN <- function (x, y, newx = x, kmax, scaleX = TRUE, PCAcomps = 0, 
                 expandVars = NULL, expandVals = NULL, smoothingFtn = mean, 
                 allK = FALSE, leave1out = FALSE, classif = FALSE, startAt1 = TRUE, 
                 saveNhbrs = FALSE, savedNhbrs = NULL) 
{
  if (PCAcomps > 0) 
    stop("PCA now must be done separately")
  if (allK) 
    stop("allK option currenttly disabled")
  if (identical(smoothingFtn, loclin) && kmax < 3) 
    stop("loclin requires k >= 3")
  if (identical(smoothingFtn, vary) && kmax < 2) 
    stop("vary requires k >= 2")
  noPreds <- is.null(newx)
  nms <- colnames(x)#modified line
  startA1adjust <- if (startAt1) 
    0
  else 1
  if (is.vector(x)) 
    x <- matrix(x, ncol = 1)
  if (hasFactors(x)) 
    stop("use factorsToDummies() to create dummies")
  if (is.data.frame(x)) 
    x <- as.matrix(x)
  ccout <- constCols(x)
  if (length(ccout) > 0) {
    warning("X data has constant columns:")
    print(ccout)
    print("deleting")
    x <- x[, -ccout]
  }
  else ccout <- NULL
  nYvals <- length(unique(y))
  if (is.vector(y)) {
    if (classif && nYvals > 2) 
      y <- factorsToDummies(as.factor(y), omitLast = FALSE)
    else y <- matrix(y, ncol = 1)
  }
  if (!is.vector(y) && !is.matrix(y)) 
    stop("y must be vector or matrix")
  if (identical(smoothingFtn, mean)) 
    smoothingFtn <- meany
  if (ncol(y) > 1 && !allK) 
    classif <- TRUE
  if (is.factor(newx) || is.data.frame(newx) && hasFactors(newx)) 
    stop("change to dummies, factorsToDummies()")
  if (is.vector(newx)) {
#    nms <- names(newx)
    newx <- matrix(newx, ncol = ncol(x))
    colnames(newx) <- nms
  }
  if (is.data.frame(newx)) {
    newx <- as.matrix(newx)
  }
  if (nrow(y) != nrow(x)) 
    stop("number of X data points not equal to that of Y")
  if (noPreds) 
    newx <- x
  kmax1 <- kmax + leave1out
  if (scaleX) {
    
    x <- mmscale(x)
    colnames(x) <- nms#modified line
    xminmax <- attr(x, "minmax")
    newx <- mmscale(newx, scalePars = xminmax)
    colnames(newx) <- nms#modified line
    
  }
  else xminmax <- NULL
  eVars <- !is.null(expandVars)
  eVals <- !is.null(expandVals)
  if (eVars || eVals) {
    if (length(expandVars) != length(expandVals)) 
      stop("expandVars and expandVals must have the same length")
    x <- multCols(x, expandVars, expandVals)
    newx <- multCols(newx, expandVars, expandVals)
  }
  if (is.null(savedNhbrs)) {
    tmp <- FNN::get.knnx(data = x, query = newx, k = kmax1)
  }
  else {
    tmp <- savedNhbrs
  }
  closestIdxs <- tmp$nn.index[, 1:(kmax + leave1out), drop = FALSE]
  if (leave1out) 
    closestIdxs <- closestIdxs[, -1, drop = FALSE]
  if (kmax1 == 1) {
    regests <- y[closestIdxs, ]
  }
  else {
    fyh <- function(newxI) smoothingFtn(closestIdxs[newxI, 
    ], x, y, newx[newxI, ])
    regests <- sapply(1:nrow(newx), fyh)
    if (ncol(y) > 1) 
      regests <- t(regests)
  }
  tmplist <- list(whichClosest = closestIdxs, regests = regests, 
                  scaleX = scaleX, classif = classif, xminmax = xminmax)
  tmplist$nhbrs <- if (saveNhbrs) 
    tmp
  else NULL
  meanx <- colMeans(x)
  covx <- cov(x)
  tried <- try(tmplist$mhdists <- mahalanobis(newx, meanx, 
                                              covx), silent = TRUE)
  if (is.null(tried) || inherits(tried, "try-error")) {
    tmplist$mhdists <- NULL
  }
  if (classif && !noPreds) {
    if (ncol(y) > 1) {
      yp <- apply(regests, 1, which.max) - startA1adjust
      if (!allK) {
        ypreds <- yp
      }
      else ypreds <- matrix(yp, nrow = kmax, byrow = TRUE)
    }
    else ypreds <- round(regests)
    tmplist$ypreds <- ypreds
  }
  tmplist$x <- x
  tmplist$y <- y
  tmplist$ccout <- ccout
  tmplist$noPreds <- noPreds
  tmplist$leave1out <- leave1out
  tmplist$startAt1adjust <- startA1adjust
  tmplist$expandVars <- expandVars
  tmplist$expandVals <- expandVals
  class(tmplist) <- "kNN"
  tmplist
}
