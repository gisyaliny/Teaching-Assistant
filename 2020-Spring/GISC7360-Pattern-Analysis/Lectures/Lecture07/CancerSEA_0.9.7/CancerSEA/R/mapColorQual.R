#' @title Function: Maps a qualitative theme of maximum 12 categories
#'
#' @description \code{mapColorQual} generates a map of a qualitative variable
#'
#' @details The function \code{mapColorQual} maps a \emph{categorical variable} with a
#'   \emph{set of distinct colors}. A legend is generated.
#'
#' @param var.name A factor variable to be mapped with a maximum of 12 categories.
#'   If it is in a data-frame, then the data-frame must be refered to, e.g., \code{df$var}
#' @param shape An existing spatial polygon or spatial polygon data-frame
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
#' data(shpSEA); data(shpNA); data(shpStates)
#'
#' ## Generate map
#' boxSEA <- bbox(shpSEA)
#' shpSEA$URBRURFAC <- factor(shpSEA$URBRUR, labels=c("rural","urban"))
#' plot(shpNA, col="cornsilk", axes=T, xlim=boxSEA[1,], ylim=boxSEA[2,], bg="lightskyblue1")
#' mapColorQual(shpSEA$URBRURFAC, shpSEA, legend.cex=0.9, legend.pos="bottomright",
#'              legend.title="Urban/Rural",
#'              map.title="SEAs Classified as Urban and Rural", add.to.map=T)
#' plot(shpStates, border="red", lwd=2, add=TRUE)
#'

mapColorQual <- function(var.name, shape,
                         map.title="", legend.title=deparse(substitute(var.name)),
                         legend.pos="bottomleft", legend.cex=1, add.to.map=F) {
  ##
  ## Plot a qualitative colors for factor "var.name"
  ##
  require(maptools); require(RColorBrewer); require(classInt)
  if (!is.factor(var.name)) stop("plotColorQual: Not a factor.")

  qualVal <- as.numeric(unclass(var.name))
  qualName <- levels(var.name)
  pal.Qual <- brewer.pal(12,"Set3")
  if(length(qualName) > 12) stop("Maximum number of categories exceeds 12.")
  map.col <- pal.Qual[qualVal]

  ## generate choropleth map
  plot(shape, col=map.col, border=grey(0.9), axes=T, add=add.to.map)
  legend(legend.pos, title=legend.title, legend=qualName,
         cex=legend.cex, fill=pal.Qual[1:length(qualName)],bty="n",
         ncol=1)
  title(map.title)
  box()
} # end::mapColorQual
