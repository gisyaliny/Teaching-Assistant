#' @title Function: Correlation between spatial link matrices
#'
#' @description \code{corLinkMat} evaluates the correlation between two link matrices
#'
#' @details The function \code{corLinkMat(A, B)} evaluates the correlation between two
#'   Moran's \emph{I} spatial autocorrelation coefficients that are defined by
#'   different spatial link matrices or one link matrix in different coding schemes.
#'   The underlying assumptions are [a] that Moran's \emph{I} is based on a simple
#'   projection matrix \eqn{ M_[1]=I-1*(1^T*1)^-1*1^T} and [b] global spatial
#'   independence. The function transforms both matrices to symmetry
#'   by \eqn{(A+A^T)/2}.
#'
#' @param A First coded spatial link matrix
#' @param B Second coded spatial link matrix
#' @export
#' @return Scalar value correlation between A and B
#' @author Michael Tiefelsdorf <tiefelsdorf@utd.edu>
#' @references Michael Tiefelsdorf (2000). Modelling Spatial Processes. The
#'   Identification and Analysis of Spatial Relationships in Regression
#'   Residuals by Means of Moran's \emph{I}. Berlin: Springer
#' @examples
#' library(CancerSEA)
#' data(mig)
#' G <- mig[ ,-c(1:8)]                              # Extract migration matrix
#' n <- nrow(G)
#'
#' Gmin <- matrix(0,n,n)                            # minimum flow between i and j
#' Gmax <- matrix(0,n,n)                            # maximum flow between i and j
#' for (i in 1:(n-1))
#'    for (j in (i+1):n){
#'       Gmin[i,j] <- Gmin[j,i] <- min(G[i,j],G[j,i])
#'       Gmax[i,j] <- Gmax[j,i] <- max(G[i,j],G[j,i])
#'    }
#'
#' Cmin <- n/sum(Gmin)*Gmin                         # Global C-coding scheme
#' Cmax <- n/sum(Gmax)*Gmax                         # Global C-coding scheme
#'
#' Wmin <- Gmin/rowSums(Gmin)                       # Row-sum W-coding scheme
#' Wmax <- Gmax/rowSums(Gmax)                       # Row-sum W-coding scheme
#'
#' ## Spectrum of eigenvalues
#' eval <- sort(eigen(Wmin, only.values = TRUE)$values)
#' plot(eval, type="h", main="Eigenvalue Spectrum of Wmin"); abline(h=0,lwd=2)
#'
#' ## Correlation between minimum and maximum link matrices
#' corLinkMat(Cmin,Cmax)                            # norun: 0.8933488
#' corLinkMat(Wmin,Cmin)                            # norum: 0.6848093
#'
corLinkMat <- function(A,B){
  ## Correlation between Moran's Is based on link matrices A and B,
  ## A and B are transformed to symmetry
  ## Assumptions: M1 projection matrix and global spatial independence.
  getMoranStat <- function(MSM, df) {
    #MSM:     M %*% S %*% M matrix
    #         M : projection matrix
    #         S : coded symmetric spatial link matrix
    #df:      degrees of freedom
    MSM <- as.matrix(MSM)
    t1 <- sum(diag(MSM))
    t2 <- sum(diag(MSM %*% MSM))
    E <- t1 / df
    V <- 2 * (df * t2 - t1 * t1)/(df * df * (df + 2))
    return(list(Mean=E,Var=V))
  } # end::getMoranStat
  A <- (A+t(A))/2
  B <- (B+t(B))/2
  n <- nrow(A)
  M1 <- diag(1,n)-matrix(1/n, n, n)
  # calculate variances
  varA <- getMoranStat(M1%*%A%*%M1, n-1)[[2]]
  varB <- getMoranStat(M1%*%B%*%M1, n-1)[[2]]
  varAB <- getMoranStat(M1%*%(A-B)%*%M1, n-1)[[2]]
  # evaluate correlation
  corAB <- 0.5*(varA+varB-varAB) /sqrt(varA*varB)
  return(corAB)
} # end::corMat
