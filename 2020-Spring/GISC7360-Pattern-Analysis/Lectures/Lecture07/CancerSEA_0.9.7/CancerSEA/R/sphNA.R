#' @title Map: SpatialPolygons of North America's Land Mass
#'
#' @details Spatial polygons of the outline of Mexico and Canada. This map
#' can be used to provide a geographic reference frame of the USA.
#'
#' @docType data
#' @name shpNA
#' @usage \code{data(shpNA)}
#' @format spatial polygons in long/lat format.
#
#'@examples
#' library(maptools)
#' data(shpSEA); data(shpStates); data(shpNA)
#' SEA.box <- sp::bbox(shpSEA)                 # bounding box for map frame
#' ## Plot three layers:
#' plot(shpNA, xlim=SEA.box[1,],ylim=SEA.box[2,], col=grey(0.7),
#'      bg="powderblue",axes=T)
#' plot(shpSEA, col=grey(0.5), border="beige", add=TRUE)
#' plot(shpStates, border="red", lwd=2, add=TRUE)
#' title("SEAs and U.S. States with Neighboring Countries")
#' box()
NULL
