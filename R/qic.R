#' Calculates QIC (Pan, 2001) for model generated with geeglm in the geepack package
#' 
#' This function calculates the quasi-likelihood under the independence model citerion
#' (also commonly known as quasi-likelihood information criterion) (QIC; Pan, 2001) for
#' model generated using geeglm in geepack. 
#' @usage 
#' QIC(object, ...)
#' @param object an object class "geeglm", obtained from a call to \code{geeglm} of the \code{geepack} package.
#' @param ... optionally more fitted model objects.
#' @export
#' @import MASS
#' @import geepack
#' @importFrom stats family model.matrix update
#' @r
#' @details 
#' The QIC can be used as the AIC for generalized estimating equations. The smaller the
#' QIC, the better the fit. 
#' 
#' According to Cui and Qian (2007), QICu is obtinaed by assuming the asymptotic
#'  equivalence of the working correlation and the independence correlation structures.
#'  It follows that QICu is not appropriate for being used to select an optimal 
#'  correlation structure and therefore QICu is not included here. 
#' 
#' Based on the full model including all explanatory variables and their interactions, 
#' the best correlation structure is usually selected first based on the model with the
#' smallest QIC value. Based on the best correlation structure, we can then further
#' select the best subset of covariates. The model with the smallest QIC value at this 
#' stage would be consider as the most parsimonious model with the best correlation
#' structure.
#' 
#' @return 
#' If just one object is provided, a numeric value with the corresponding QIC. 
#' 
#' If multiple objects are provided, a data.frame with rows corresponding to the
#'  objects and columns representing the QIC, log of quasi-likelihood 
#'  (\code{log.QLik}), trace or the penalty term (\code{Trace}) as well as the number
#'   of covariates (\code{px}) in the model. 
#' 
#' \code{get_QIC} is the function that does the actual calculation.
#' @author 
#' Originally by Daniel J. Hocking, more recent revisions by Thomas Fung.
#' 
#' @references
#' Cui, J, and Qian, G. (2007). Selection of Working Correlation Structure and Best
#' Model in GEE Analyses of Longitudinal Data. \emph{Communication in Statistics
#' --Simulation and Computation} \bold{36}, 987--996.
#' 
#' Pan, W. (2001). Akaike's Information Criterion in Generalized Estimating Equations.
#' \emph{Biometrics} \bold{57}, 120--125. 
#' 
#' @examples 
#' data(dietox)
#' dietox$Cu <- as.factor(dietox$Cu)
#' gee01 <- geeglm(Weight ~ Time + Cu + Cu * Time, id =Pig, data =
#'  dietox, family=gaussian,corstr="ex")
#' gee02 <- geeglm(Weight ~ Time + Cu + Cu * Time, id =Pig, data =
#'  dietox, family=gaussian,corstr="unstructured")
#' QIC(gee01)
#' QIC(gee01, gee02)
#' 
#' mf3 = formula(Weight ~ Cu + Time + I(Time^2))
#' gee3.ar = geeglm(mf3, data = dietox, id = Pig, family = Gamma, corstr = "ar1")
#' gee3.i = update(gee3.ar, corstr = "independence")
#' gee3.ex = update(gee3.ar, corstr = "exchangeable")
#' gee3.un = update(gee3.ar, corstr = "unstructured")
#' QIC(gee3.ar, gee3.i, gee3.ex, gee3.un)
#' @name QIC
NULL

#' @rdname QIC
#' @export
QIC <- function(object, ...){
  if (!missing(...)) {
    res <- sapply(list(object, ...), get_QIC)
    val <- as.data.frame(t(res))
    Call <- match.call()
    row.names(val) <- as.character(Call[-1L])
    val
  }
  else get_QIC(object)[[1L]]
}

#' @rdname QIC
#' @export
get_QIC <- function(object) {
    check.class <- class(object)
    known <- NULL
    if(identical(check.class[1], "geeglm")) {
      known[1] <- 1
    } 
    if(identical(check.class[1], "geese")) {
      stop("\nQIC function not defined for object of class geese rerun models using geeglm in geepack\n")
      known[2] <- 1
    }
    if(sum(known) < 1) {
      stop("\nFunction not defined for this object class\n")
    }
    model.indep <- update(object, corstr = "independence")
    mu.R <- object$fitted.values
    y <- object$y
    type <- family(object)$family
    quasi.R <- switch(type,
                      poisson = sum((y*log(mu.R)) - mu.R),
                      gaussian = sum(((y - mu.R)^2)/-2),
                      binomial = sum(y*log(mu.R/(1 - mu.R)) + log(1 - mu.R)),
                      Gamma = sum(-y/mu.R - log(mu.R)),
                      stop("Error: distribution not defined for this function"))
    # Trace Term (penalty for model complexity)
    omegaI <- solve(model.indep$geese$vbeta) # Omega-hat(I) via Moore-Penrose 
    Vr <- object$geese$vbeta
    trace.R <- sum(diag(omegaI %*% Vr))
    px <- dim(model.matrix(model.indep))[2]
    QIC <- 2*(trace.R - quasi.R)
    out <- list()
    out$QIC <- round(QIC,4)
    out$log.QLik <- quasi.R
    out$Trace <- trace.R
    out$px <- px
    class(out) <- "qic"
    return(out)
}

