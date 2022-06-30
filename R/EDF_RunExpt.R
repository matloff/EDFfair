
#  run one of the qeFair* functions many times, over many holdout sets

#  for now, ony takes a single deweighting parameter, e.g. a single
#  lambda

# args:

#    nReps:  number of holdout sets to run
#    cmdPartial:  qeFair* call, containing the string 'xoxo', to be 
#       substituted one at a time by elements in the argument xvals
#    xvals:  see cmdPartial; typically values of a deweighting
#       parameter

# example:

#   runExpt(25,"qeFairRidgeLin(pef,'wageinc',list(occ=xoxo),'sex')",c(0.1,0.5))

#   run 25 holdout sets and 2 values of lambda (0.1, 0.5) in linear
#   ridge, predicting wage income with gender sensitive and C =
#   occupation; return value:  matrix, one row for each deweighting
#   param, with the row showing mean testAcc and mean corrs 

runExpt <- function(nReps,cmdPartial,xvals,code='')
{
   res <- matrix(nrow=length(xvals),ncol=3)
   for (i in 1:length(xvals)) {
      x <- xvals[i]
      cmd <- sub('xoxo',x,cmdPartial)
      cmd <- paste0('z <- ',cmd)
      cmd <- paste0(cmd,'; c(z$testAcc,z$corrs)')
      # repOut <- replicMeans(nReps,cmd)
      repOut <- t(replicate(nReps,eval(parse(text=cmd))))
      repMeans <- colMeans(repOut)  # add std. errors later
      res[i,1] <- x
      res[i,-1] <- repMeans 
   }
   attr(res,'call') <- match.call()
   res <- as.data.frame(res)
   res$code <- code
   class(res) <- 'runExpt'
   res
}

expandRange <- function(rng,expAmt) 
{
   width <- rng[2] - rng[1]
   midpt <- mean(rng)
   newwidth <- expAmt * width
   nw2 <- newwidth / 2
   c(midpt-nw2,midpt+nw2)
}

plot.runExpt <- function(runExptOut,clrs=palette(),
                   xlim=expandRange(range(runExptOut[,2]),1.2), 
                   ylim=expandRange(range(runExptOut[,3]),3.2), 
                   legendPos='bottomright')

{
   xyzPlotEDF(runExptOut[,c(2,3,1,4)],clrs=clrs,xlim=xlim,ylim=ylim,
      legendPos=legendPos)
}

# modified version of regtools::xyzPlot()

xyzPlotEDF <- function(xyz, clrs = NULL, cexText = 1, xlim = NULL, ylim = NULL,
    xlab = NULL, ylab = NULL, legendPos = NULL, plotType = "l") 
{
    if (is.null(xlim))
        xlim <- range(xyz[, 1])
    if (is.null(ylim))
        ylim <- range(xyz[, 2])
    if (is.null(xlab))
        xlab <- "x"
    if (is.null(ylab))
        ylab <- "y"
    # how many lines will be plotted?
    nGrps <- length(unique(xyz[,4]))
    if (is.null(clrs)) {
        if (nGrps == 1)
            clrs <- "black"
        else clrs <- heat.colors(length(unique(xyz[, 4])))
    }
    if (plotType == "l")
        xyz <- xyz[order(xyz[, 1]), ]
    nr <- nrow(xyz)
    lineGrps <- if (nGrps == 1)
        list(1:nr)
    else split(1:nr, xyz[, 4])
    nGrps <- length(lineGrps)
    plot(1, xlim = xlim, ylim = ylim, xlab = xlab, ylab = ylab,cex = 0.1)
    for (i in 1:nGrps) {
        if (plotType == "l") {
            lines(xyz[lineGrps[[i]], 1:2], type = "l", col = clrs[i])
        }
        else points(xyz[lineGrps[[i]], 1:2], col = clrs[i], cex = 0.1)
    }
    for (i in 1:nGrps) {
        lns <- xyz[lineGrps[[i]], ]
        text(lns[, 1], lns[, 2], lns[, 3], cex = cexText, col = clrs[i])
    }
    if (nGrps > 1)
        legend(legendPos, legend = unique(xyz[, 4]), col = clrs,
            lty = 1)

}

