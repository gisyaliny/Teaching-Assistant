#' @title Function: Summerize lmHetero model estimates
#'
#' @description
#' \code{summary.lmHetero} provides relevant summary information of a
#' heteroscedastic regression model estimated by \code{lmHetero}.
#'
#' @details Regression tables for the estimate regression coefficients and the
#' coefficients of the weights model are provided as well as a maximum
#' likelihood ratio test is provided agaist a model, which assumes homoscedasticity.
#' @usage summary(object)
#'
#' @param object An object of class \code{lmHetero}.
#' @export
#' @return the input object is returned silently.
#' @author Michael Tiefelsdorf \email{tiefelsdorf@@utd.edu}
#'
#' @examples
#' library(CancerSEA)
#' mod.lmH <- lmHetero(B_WM_P2_RT~L_WM_P1_RT+RAD_MD+URBRUR|log(POPATRISK1982), data=cancer)
#' summary(mod.lmH)
#'
#'
summary.lmHetero <- function(object, ...){
   if (!inherits(object, "lmHetero"))
      stop("Object must be of class 'lmHetero'")
   ## Regression coefficent info
   b <- object$beta
   se <- sqrt(diag(object$covBeta))
   z <- b/se
   table1 <- cbind(b, se, z, 2*(1-pnorm(abs(z))))
   colnames(table1) <- c("Estimate", "Std.Err", "z-value", "Pr(>|z|)")
   rownames(table1) <- rownames(object$beta)

   ## Gamma weights coefficient infor
   g <- object$gamma
   seg <- sqrt(diag(object$covGamma))
   zg <- g/seg
   table2 <- cbind(g, seg, zg, 2*(1-pnorm(abs(zg))))
   colnames(table2) <- c("Gamma", "Std.Err", "z-value", "Pr(>|z|)")
   rownames(table2) <- object$namesGamma

   cat("\n===============================================================")
   cat("\nMultiplicatively Weighted Heteroscedasticity ML-Regrssion Model")
   cat("\n===============================================================\n")
   cat("\nCall:\n")
   print(object$CALL)
   cat("\nRegression Coefficients:\n")
   printCoefmat(table1)
   cat("\nGamma Coefficients:\n")
   printCoefmat(table2)
   cat("\nlog-likelihood =", object$logLikeH1,"\n")

   ## Likelihood ratio test for heteroscedasticity
   df <- nrow(table2)-1                      # with df=dim(Gamma[-Intercept])
   if (df > 0L)
   {
      LR <- 2*abs(object$logLikeH1 - object$logLikeH0)     # chi^2 distributed
      table <- cbind(LR,df,pchisq(LR,df,lower.tail=FALSE))
      colnames(table) <- c("LR","  df"," Pr(Chi > LR)")
      rownames(table) <- ""
      LRTest <- c("LR"=round(LR,2),"df"=round(df,0),"Pr(Chi > LR)"=round(pchisq(LR,df,lower.tail=FALSE),5))
      cat("\nHeteroscedasticity likelihood ratio test:\n")
      print(table)
   }
   cat("\n")
   invisible(object)
} # end::summary.lmHetero
