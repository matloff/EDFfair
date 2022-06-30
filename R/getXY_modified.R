#change omit last to false, originallt true

getXY <- function (data, yName, xMustNumeric = FALSE, classif, factorsInfo = NULL, 
                   makeYdumms = FALSE) 
{
  if (is.vector(data) && is.null(yName)) 
    data <- data.frame(data)
  if (!is.data.frame(data)) 
    stopBrowser("data must be a data frame")
  if (!is.null(yName)) {
    ycol <- which(names(data) == yName)
    y <- data[, ycol]
  }
  else y <- ycol <- NULL
  if (classif && !is.factor(y)) 
    stop("Y must be a factor")
  if (!is.null(y)) {
    x <- data[, -ycol, drop = FALSE]
  }
  else x <- data
  if (xMustNumeric) {
    xClasses <- regtools::getDFclasses(x)
    if (any(xClasses == "logical") || any(xClasses == "character")) {
      print("character or logical variables currently not allowed")
      print("change to factors")
      return(NA)
    }
    x <- regtools::factorsToDummies(x, omitLast = FALSE)# modified: omitLast = FALSE
    factorsInfo <- attr(x, "factorsInfo")
  }
  else factorsInfo <- NULL
  if (classif && !is.null(yName) && makeYdumms) {
    yDumms <- regtools::factorsToDummies(y, omitLast = FALSE, 
                                         factorsInfo = NULL)
    classNames <- levels(y)
    colnames(yDumms) <- classNames
    xy <- cbind(x, yDumms)
  }
  else {
    yDumms <- NULL
    classNames <- NULL
    xy <- NULL
  }
  if (classif && !is.null(yName) && !makeYdumms) 
    classNames <- levels(y)
  list(xy = xy, x = x, y = y, yDumms = yDumms, classNames = classNames, 
       factorsInfo = factorsInfo)
}
