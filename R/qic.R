#' Calculates QIC (Pan, 2001) for model generated with geeglm in the geepack package
#' 
#' This function calculates the quasilikelihood information criteria (QIC; Pan, 2001) for model generated using geeglm in geepack. The QIC is intended as an equivalent of AIC for generalized estimating equations (GEE-PA).
#' @usage 
#' qic(object)
#' @param object an object class "geeglm", obtained from a call to \code{geeglm} of the \code{geepack} package.
#' @export
#' @import MASS
#' @details 
#' Fit a mean-parametrizied COM-Poisson regression using maximum likelihood estimation 
#' via an iterative Fisher Scoring algorithm. 
#' 
#' The COM-Poisson regression model is
#' 
#' Y_i ~ CMP(mu_i, nu), 
#'           
#' where  
#'    
#' E(Y_i) = mu_i = exp(x_i^T beta),
#'       
#' and \emph{nu > 0} is the dispersion parameter. 
#' 
#' The fitted COM-Poisson distribution is over- or under-dispersed 
#' if \emph{nu < 1} and \emph{nu > 1} respectively.
#' @return 
#' A fitted model object of class \code{cmp} similar to one obtained from \code{glm} 
#' or \code{glm.nb}.
#' 
#' The function \code{summary} (i.e., \code{\link{summary.cmp}}) can be used to obtain 
#' and print a summary of the results. 
#' 
#' The function \code{plot} (i.e., \code{\link{plot.cmp}}) can be used to produce a range 
#' of diagnostic plots. 
#' 
#' The generic assessor functions \code{coef} (i.e., \code{\link{coef.cmp}}), 
#' \code{logLik} (i.e., \code{\link{logLik.cmp}}) 
#' \code{fitted} (i.e., \code{\link{fitted.cmp}}), 
#' \code{nobs} (i.e., \code{\link{nobs.cmp}}), 
#' \code{AIC} (i.e., \code{\link{AIC.cmp}}) and 
#' \code{residuals} (i.e., \code{\link{residuals.cmp}}) 
#' can be used to extract various useful features of the value
#' returned by \code{glm.cmp}.
#' 
#' An object class 'glm.cmp' is a list containing at least the following components:
#'
#' \item{coefficients}{a named vector of coefficients}
#' \item{se_beta}{approximate standard errors (using observed rather than expected 
#' information) for coefficients}
#' \item{residuals}{the \emph{response} residuals (i.e., observed-fitted)}
#' \item{fitted.values}{the fitted mean values}
#' \item{rank}{the numeric rank of the fitted linear model}
#' \item{linear.predictors}{the linear fit on log scale}
#' \item{df.residuals}{the residuals degrees of freedom}
#' \item{df.null}{the residual degrees of freedom for the null model}
#' \item{null.deviance}{The deviance for the null model. 
#' The null model will include only the intercept.}
#' \item{y}{the \code{y} vector used.}
#' \item{x}{the model matrix}
#' \item{model}{the model frame}
#' \item{call}{the matched call}
#' \item{formula}{the formula supplied}
#' \item{terms}{the \code{terms} object used}
#' \item{data}{the \code{data} argument}
#' \item{offset}{the \code{offset} vector used}
#' \item{lambdaub}{the final \code{lambdaub} used}
#' 
#' @references 
#' Fung, T., Alwan, A., Wishart, J. and Huang, A. (2019). \code{mpcmp}: Mean-parametrized
#' Conway-Maxwell Poisson Regression. R package version 0.2.0.
#' 
#' Huang, A. (2017). Mean-parametrized Conway-Maxwell-Poisson regression models for 
#' dispersed counts. \emph{Statistical Modelling} \bold{17}, 359--380.
#'   
#' @seealso 
#' \code{\link{summary.cmp}}, \code{\link{plot.cmp}}, \code{\link{fitted.cmp}} 
#' and \code{\link{residuals.cmp}}.
#' @examples 
#' ### Huang (2017) Page 368--370: Overdispersed Attendance data
#' data(attendance)
#' M.attendance <- glm.cmp(daysabs~ gender+math+prog, data=attendance)
#' M.attendance
#' summary(M.attendance)
#' plot(M.attendance)
#' 
#' ### Barbour & Brown (1974): Overdispersed Fish data
#' data(fish)
#' M.fish <- glm.cmp(species~ 1+log(area), data=fish)
#' M.fish
#' summary(M.fish)
#' 
#' ### Huang (2017) Page 371--372: Underdispersed Takeover Bids data
#' data(takeoverbids)
#' M.bids <- glm.cmp(numbids ~ leglrest + rearest + finrest + whtknght 
#'     + bidprem + insthold + size + sizesq + regulatn, data=takeoverbids)
#' M.bids
#' summary(M.bids)
#' par(mfrow=c(2,2))
#' plot(M.bids)
#' 
#' ### Huang (2017) Page 373--375: Underdispersed Cotton bolls data
#' ### Model fitting for predictor V 
#' \donttest{
#' data(cottonbolls)
#' M.bolls <- glm.cmp(nc~ 1+stages:def+stages:def2, data= cottonbolls)
#' M.bolls
#' summary(M.bolls)
#' }


qic <- function(object) {
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
    omegaI <- ginv(model.indep$geese$vbeta.naiv) # Omega-hat(I) via Moore-Penrose 
    Vr <- object$geese$vbeta
    trace.R <- sum(diag(omegaI %*% Vr))
    px <- dim(model.matrix(model.indep))[2]
    # QIC
    QIC <- 2*(trace.R - quasi.R)
    QICu <- (-2)*quasi.R + 2*px    # Approximation assuming model structured correctly
    output <- data.frame(list(QIC, QICu, quasi.R, trace.R, px))
    names(output) <- c('QIC', 'QICu', 'Log.QLik', 'Trace', 'px')
    return(output)
  }
