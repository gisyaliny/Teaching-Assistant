#' @title Function: Maps a sequential color theme
#'
#' @description \code{mapColorRamp} generates a map with a sequential theme of an interval scaled variable
#'
#' @details The function \code{mapColorRamp} maps an \emph{interval scaled variable} by a
#'   \emph{sequetial color ramp}. Quantiles values are coded in gradually increasing
#'   intensities of oranges. A legend is generated.
#'
#' @param var.name A variable to be mapped in a bipolar theme. If it is in a data-frame,
#'   then the data-frame must be refered to, e.g., \code{df$var}
#' @param shape An existing spatial polygon or spatial polygon data-frame
#' @param breaks Number of qunatiles. It needs to range the range of 3 to 9
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
#' data(shpSEA); data(shpNA); data(shpStates); data(cancer)
#'
#' ## Merge data to shpSEA
#' shpSEA <- sp::merge(shpSEA, cancer, by.x="SEQID", by.y="SEQID")
#'
#' ## Generate map
#' boxSEA <- bbox(shpSEA)
#' plot(shpNA, col="cornsilk", axes=T, xlim=boxSEA[1,], ylim=boxSEA[2,], bg="lightskyblue1")
#' mapColorRamp(shpSEA$RAD_MD, shpSEA, breaks=8,
#'              legend.cex=0.9, legend.pos="bottomright", legend.title="Radon",
#'              map.title="Median Indoor Radon by SEA", add.to.map=T)
#' plot(shpStates, border="red", lwd=2, add=TRUE)
#'
#'

mapColorRamp <- function(var.name, shape, breaks=8,
                         map.title="", legend.title=deparse(substitute(var.name)),
                         legend.pos="bottomleft", legend.cex=1, add.to.map=F) {
  ##
  ## Plot a color ramp variable "var.name"
  ##
  require(maptools); require(RColorBrewer); require(classInt)
  if (breaks <= 2) stop("At least breaks=3 color classes need to be specified")
  if (breaks >= 10) stop("A maximum of breaks=9 color classes can be specified")
  ## define breaks and color assignment
  q.breaks <- classIntervals(var.name, n=breaks, style="quantile")
  pal <- brewer.pal(breaks, "Oranges")
  #pal.YlOrRd <- brewer.pal(n.breaks, "YlOrRd")
  map.col <- pal[findInterval(var.name, q.breaks$brks, rightmost.closed=T)]
  ## generate choropleth map
  plot(shape, col=map.col, border=grey(0.9), axes=T, add=add.to.map)
  legend(legend.pos, title=legend.title,
         legend=leglabs(round(q.breaks$brks,digits=3)),
         fill=pal, bty="n", ncol=1, cex=legend.cex)
  title(map.title)
  box()
} # end::mapColorRamp
