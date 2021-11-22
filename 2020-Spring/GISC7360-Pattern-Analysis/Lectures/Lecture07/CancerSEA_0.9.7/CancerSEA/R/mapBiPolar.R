#' @title Function: Map a bipolar theme broken around a neutral value
#'
#' @description \code{mapBiPolar} generates a map of a bipolar theme variable
#'
#' @details The function \code{mapBiPolar} maps a \emph{bipolar variable} with a
#'   \emph{divergent color ramp} around a specific break values. A legend is generated.
#'   Below values are coded blue and above values red. Each branch is broken
#'   into 'quantiles'. Therefore, the number of class in each branch should be
#'   proportional to the number of observations in each class.
#'
#' @param var.name A variable to be mapped in a bipolar theme. If it is in a data-frame,
#'   then the data-frame must be refered to, e.g., \code{df$var}
#' @param shape An existing spatial polygon or spatial polygon data-frame
#' @param break.value Neutral value separating the negative branch from the
#'   positive branch of the variable
#' @param neg.breaks Number of classes in the negative branch (default=4)
#' @param pos.breaks Number of classes in the positive branch (default=\code{neg.breaks})
#' @param map.title Character string with map title
#' @param legend.title Character string with legend title (default=\code{var.name})
#' @param legend.pos Location of legend in the map frame (default=\code{"bottomleft"})
#' @param legend.cex Relative font size of the legend
#' @param add.to.map Logical to start a new map frame if \code{FALSE} or overlay onto an
#'   existing map frame if \code{TRUE}
#' @export
#' @return \code{NULL}
#' @author Michael Tiefelsdorf <tiefelsdorf@utd.edu>
#' @examples
#' library(maptools)
#' data(mig); data(shpSEA); data(shpNA); data(shpStates)
#' ## Calculate In- and Out-flows and merge to shapefile
#' inFlow <- apply(mig[,9:516],2,sum)
#' outFlow <- apply(mig[,9:516],1,sum)
#' logRateFlow <- log(inFlow/outFlow)
#' flowDF <- data.frame(mig$SEQID,inFlow,outFlow,logRateFlow)
#' names(flowDF) <- c("SEQID","InFlow","OutFlow", "LogRateFlow")
#'
#' ## Merge with shpSEA
#' shpSEA <- sp::merge(shpSEA, flowDF, by.x="SEQID", by.y="SEQID")
#'
#' ## Check number of classes in logRateFlows
#' sum(shpSEA$LogRateFlow < 0)
#' sum(shpSEA$LogRateFlow >= 0)
#'
#' ## Generate map
#' boxSEA <- bbox(shpSEA)
#' plot(shpNA, col="cornsilk", axes=T, xlim=boxSEA[1,], ylim=boxSEA[2,], bg="lightskyblue1")
#' mapBiPolar(shpSEA$LogRateFlow, shpSEA, neg.breaks=5, pos.breaks=4,
#'            legend.cex=0.9, legend.pos="bottomright", legend.title="Log(Inflow/Outflow)",
#'            map.title="Internal SEA Migration Gains and Losses", add.to.map=T)
#' plot(shpStates, border="red", lwd=2, add=TRUE)
#'
mapBiPolar <- function(var.name,shape,
                       break.value=0,neg.breaks=4,pos.breaks=neg.breaks,
                       map.title="",legend.title=deparse(substitute(var.name)),
                       legend.pos="bottomleft",legend.cex=1,
                       add.to.map=F) {
   ##
   ## Plot bipolar map theme for variable "var.name"
   ##
   require(RColorBrewer); require(classInt); require(maptools)

   ## define quantile breaks and color assignment
   q.neg.breaks <- classIntervals((var.name[var.name < break.value]), n=neg.breaks, style="quantile")
   q.pos.breaks <- classIntervals((var.name[var.name > break.value]), n=pos.breaks, style="quantile")
   q.breaks <- c(q.neg.breaks$brks[-(neg.breaks+1)],break.value,q.pos.breaks$brks[-1])     # combine neg and pos over zero

   pal.neg <- brewer.pal(neg.breaks, "Blues")
   pal.pos <- brewer.pal(pos.breaks, "Reds")
   pal <- c(rev(pal.neg),pal.pos)                                                # combine palettes

   map.col <- pal[findInterval(var.name,q.breaks,rightmost.closed=T)]
   ## generate choropleth map
   plot(shape, col=map.col, border=grey(0.9), axes=T, add=add.to.map)
   legend(legend.pos, title=legend.title, legend=leglabs(round(q.breaks,digits=3)),
          cex=legend.cex, fill=pal, bty="n", ncol=1)
   title(map.title)
   box()
} # end:mapBiPolar
