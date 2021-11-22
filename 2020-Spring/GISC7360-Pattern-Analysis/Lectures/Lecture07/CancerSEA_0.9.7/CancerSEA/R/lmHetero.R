#' @title Function: Multiplicately weighted regression model
#'
#' @description
#' \code{lmHetero} accounts for heteroscedasticity in regression models
#'
#' @details
#' This function estimates the parameters of a regression model whose normally
#' distributed disturbances have a variance that multiplicatively depends on a
#' set of strictly positive weights variables. That is,
#'
#' \deqn{\sigma^2_i = \exp(\gamma_0 + \gamma_1 \cdot \log(z_{i1}) + ...)}
#'
#' The weights variables z must be entered in their logarithmic forms. The
#' paramater \eqn{\exp(\gamma_0)} expresses the global variance.
#'
#' @usage
#' \code{lmHetero(formula, hetero = ~1, data, subset, na.action, contrasts = NULL, iter = TRUE)}

#'
#' @param formula Formula object (perhaps, multiple parts formula
#'  \code{y~x1+x2+...| z1+z2+...}) linking the dependent
#' variable to its set of associated independent variables x and a second
#' expression separated by \code{|} modelling the local variances with the
#' variables z. Omitting the second expression "\code{| z1+z2+...}" assumes a
#' constant variance (homoscedasticity).
#' In this function specification the parameter "\code{hetero}" should not be included
#' in the function call.
#' This is the preferred specification of the function call.
#'
#' @param hetero Optional formula specification without "\code{| z1+z2+...}":
#' A second parameter modeling the heteroscedasticity with a
#' right-handed formula by a set of variables z. That is, "\code{hetero=~z1+z2+...}".
#' Omitting the second parameter assumes "\code{hetero=~1}". This is only included
#' for backward compatibility).
#'
#' @param data An optional data frame containing the variables in the model.
#'  By default the variables are taken from the environment of the formula.
#'
#' @param subset An optional vector specifying a subset of observations to
#' be used in fitting the model.
#'
#' @param na.action A function that indicates what should happen when the data
#' contain \code{NA}s. The default is set by the "\code{na.action}" option
#'
#' @param contrasts An optional list. See the "\code{contrasts.arg}" of
#' \code{\link{model.matrix.default}}.
#'
#' @param iter Logical indicating whether the interation history should be
#' displayed. The default setting if "\code{FALSE}".
#'
#' @export
#' @return a list with 10 elements:
#' \item{CALL}{function call}
#' \item{sigma2}{global variance estimate \code{exp(gamma_0)}}
#' \item{gamma}{vector of estimated gamma coefficients}
#' \item{namesGamma}{vector of variable names expressed by Z}
#' \item{beta}{vector of estimated weight adjusted regression parameters}
#' \item{weights}{vector of weights \code{1/\sigma_i} estimates for each
#' observation. It can be used in the call \code{lm(..., weights=weights)}
#' to adjust for heteroscedasticity}
#' \item{covBeta}{covariance matrix of the estimated regression coefficients}
#' \item{covGamma}{covariance matrix of the estimated gamma coefficients}
#' \item{logLikeH1}{log-likelihood of the heteroscedastic adjusted regression
#' model}
#' \item{logLikeH0}{log-likelihood of the unadjusted regression model}
#'
#' @source The maximum likelihood estimation procedure for multiplicately
#' weighted regression is given in Greene W. H. (2000). Econometric Analysis.
#' 4th edition. Upper Saddle River: Prentice Hall. pp 516-521
#' (Note: page numbers will differ for other editions)
#'
#' @author Michael Tiefelsdorf (\email{tiefelsdorf@@utd.edu}) & Yongwan Chun
#'
#' @examples
#' library(CancerSEA)
#' ## H0 model
#' mod.lm <- lmHetero(B_WM_P2_RT~L_WM_P1_RT+RAD_MD+URBRUR, data=cancer)
#' summary(mod.lm)
#' ## Preferred function call
#' mod.lmH <- lmHetero(B_WM_P2_RT~L_WM_P1_RT+RAD_MD+URBRUR|log(POPATRISK1982), data=cancer)
#' summary(mod.lmH)
#' ## Alternative function call
#' mod.lmH <- lmHetero(B_WM_P2_RT~L_WM_P1_RT+RAD_MD+URBRUR, hetero=~log(POPATRISK1982), data=cancer)
#' summary(mod.lmH)
#' ## Use estimated weights as input for weighted lm
#' mod.lmW <- lm(B_WM_P2_RT~L_WM_P1_RT+RAD_MD+URBRUR, weights=mod.lmH$weights, data=cancer)
#' summary(mod.lmW)
#' hist(weighted.residuals(mod.lmW))

lmHetero <- function(formula, hetero=~1, data, subset, na.action,
                     contrasts = NULL, iter=FALSE, ...) {
  ################################################################################################
  ##
  ## Purpose: Calculate multiplicately weighted regression models with respect to externally
  ##          given exogenous variables
  ##
  ## Source: The ML estimation procedure for multiplicately weighted regression is given
  ##         in Greene W. H. (2000). Econometric Analysis. 4th edition. Upper Saddle River:
  ##         Prentice Hall. pp 516-521 (Note: page numbers will differ for other editions)
  ##
  ## Objective: estimate gamma values for mulitiplicative heteroscedasticity models
  ##
  ## Syntax:
  ## [1] lmHetero(y~x1+x2, hetero= ~z1+z2, data=mydata)
  ## [2] lmHetero(y~x1+x2 | z1+z2, data=mydata)
  ## Note: An expression for Z must be present.
  ## y : Dependent variable
  ## X : Independent variable(s) with added intercept
  ## Z : Weight variable(s) with intercept added.
  ##
  ##     !!! Important user input: weighte variables Z must be enter log-transformed !!!
  ##
  ## Authors: Michael Tiefelsdorf (tiefelsdorf@utd.edu) & Yongwan Chun
  ###############################################################################################

  ## Parseing the function call
  require(Formula)
  cl <- match.call()
  if (missing(data)) data <- environment(formula)
  mf <- match.call(expand.dots = FALSE)
  m <- match(c("formula", "data", "subset", "na.action"), names(mf), 0)
  mf <- mf[c(1, m)]
  mf$drop.unused.levels <- TRUE
  if (!missing(hetero)) {
    formula <- as.Formula(formula, hetero)
    cl$hetero <- NULL
    cl$formula <- formula(formula)
  }
  else {
    formula <- as.Formula(formula)
  }
  stopifnot(length(formula)[1] == 1L, length(formula)[2] %in% 1:2)
  has_dot <- function(formula) inherits(try(terms(formula), silent = TRUE), "try-error")
  if (has_dot(formula)) {
    f1 <- formula(formula, rhs = 1)
    f2 <- formula(formula, lhs = 0, rhs = 2)
    if (!has_dot(f1) & has_dot(f2))
      formula <- as.Formula(f1, update(formula(formula, lhs = 0, rhs = 1), f2))
  }
  mf$formula <- formula
  mf[[1]] <- as.name("model.frame")
  mf <- eval(mf, parent.frame())
  y <- model.response(mf, "numeric")
  mt <- terms(formula, data = data)
  mtX <- terms(formula, data = data, rhs = 1)
  X <- model.matrix(mtX, mf, contrasts)
  if (length(formula)[2] < 2L) {
    #stop("Set of weights variables is missing in function call!")
    #mtZ <- NULL
    #Z <- NULL
    zNames <- "(Intercept)"
    Z <- matrix(1, nrow=length(y), ncol=1)
  }
  else {
    mtZ <- delete.response(terms(formula, data = data, rhs = 2))
    Z <- model.matrix(mtZ, mf, contrasts)
    zNames <- colnames(Z)
    if (!all(exp(Z) > 0)) stop("All weight variables must be positive!")
    #Z[,-1] <- log(Z[,-1])
  }
  ## done::parsing and starting the estimation

  ##
  ## Calculate start values for Beta and Gamma
  ##
  nofreg <- length(y)
  Beta <- qr.solve(crossprod(X),crossprod(X,y))
  res <- y - X %*% Beta
  res <- log(res^2)

  Gamma <- qr.solve(crossprod(Z),crossprod(Z,res))
  Gamma[1] <- Gamma[1]+1.2704    # Harvey (1976) correction
  if (iter==TRUE) { cat("Beta:",Beta);cat("\n");cat("Gamma:",Gamma);cat("\n") }

  qrcz <- qr(crossprod(Z))

  ## Start interative estimation procedure
  MaxDiff <- Inf
  while (MaxDiff > 0.001) {
    Sigma2 <- exp(Z %*% Gamma)
    W <- diag(1 / Sigma2[,1])
    ## Calculate the values at the next step
    #  newBeta <- solve(t(x) %*% W %*% x, t(x) %*% W %*% y)
    xW <- crossprod(X,W)
    newBeta <- qr.solve(xW %*% X, xW %*% y)
    res2 <- (y - X %*% newBeta)^2
    newGamma <- Gamma + qr.coef(qrcz,crossprod(Z, res2 / Sigma2 - 1))
    MaxDiff <- max(abs(newGamma - Gamma))
    if (iter==TRUE) {cat("Beta:",newBeta);cat("\n");cat("Gamma:",newGamma);cat("\n")}
    Beta <- newBeta
    Gamma <- newGamma
  }  # end while

  Sigma2 <- exp(Z %*% newGamma)
  cBeta <- qr.solve(xW %*% X)      # The global covariance matrix is block diagonal
  cGamma <- 2*qr.solve(crossprod(Z))
  logLikeH1 <- -(nofreg/2)*log(2*pi) - 0.5*sum(log(Sigma2[,1])) - 0.5* sum(res2/Sigma2[,1])
  logLikeH0 <- logLik(lm(y~X))[1]
  ##
  ## Report results. As ML-estimates the values are slightly biased
  ##
  rval <- list()
     rval$CALL <- cl
     rval$sigma2 <- exp(newGamma[1])
     rval$gamma <- newGamma
     rval$namesGamma <- zNames
     rval$beta <- newBeta
     rval$weights <- 1/Sigma2[,1]
     rval$covBeta <- cBeta
     rval$covGamma <- cGamma
     rval$logLikeH1 <- logLikeH1
     rval$logLikeH0 <- logLikeH0
  #   rval$formula <- formula(formula)
  #   rval$terms <- list(regressors = mtX, hetero = mtZ, full = mt)
  #   rval$na.action <- attr(mf, "na.action")
  #   rval$levels <- .getXlevels(mt, mf)
  #   rval$contrasts <- list(regressors = attr(X, "contrasts"),
  #                        hetero = attr(Z, "contrasts"))
  class(rval) <- c("lmHetero","list")
  return(rval)
} # end::lmHetero

