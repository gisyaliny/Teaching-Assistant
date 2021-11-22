#' @title Map: Adjusted SpatialPolygons of the U.S. States
#'
#' @details Spatial polygons of the U.S. States with relocated Alaska and Hawaii.
#'   It can be used in a map overlay to visually group the SEAs by state.
#'
#' @docType data
#' @name shpStates
#' @usage \code{data(shpStates)}
#' @format 51 spatial polygons in long/lat format
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
