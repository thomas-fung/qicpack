#' qicpack: QIC Package for GEE
#'
#' @name qicpack-package
#' @aliases qicpack
#' @docType package
#' @title qicpack: QIC Package for GEE
#' @keywords package
#' @references 
#' Fung, T. (2019). \emph{qicpack}: QIC Package for GEE. R package version 0.0.1.
NULL

#' Dietox data set
#'
#' This data set gives weight of pigs measured weekly for 12 weeks in a 3x3
#' factorial experiment. Data also contains the starting weight (i.e. the weight at week
#' 1). The treatments are 3 different levels of Evit = vitamin E (dose: 0, 100, 200 mg 
#' dl-alpha-tocopheryl acetat /kg feed) in combination with 3 different levels of Cu
#' =copper (dose: 0, 35, 175 mg/kg feed) in the feed. The cumulated feed intake is
#'  also recorded. The pigs are littermates. 
#' 
#' The dietox data frame has 861 observations on 7 variables. This dataset is included 
#' in multiple packages, including \code{doBy} and \code{geepack}.  
#' 
#'#'
#' @name dietox
#' @format A data frame with 861 observations on 7 variables.
#' \describe{
#' \item{Pig}{Identifier}
#' \item{Weight}{Weight}
#' \item{Feed}{the cumulated feed intake}
#' \item{Time}{Time (in weeks) in the experiment}
#' \item{Evit}{3 different levels of vitamin E supplement}
#' \item{Cu}{3 different levles of copper supplement}
#' \item{Litter}{Identifier of litter of each pig} 
#' }
#' 
#' @docType data
#' @keywords datasets
#' @usage 
#' data(dietox)
#' @source 
#' Lauridsen, C., Højsgaard, S., Sørensen, M.T.C. (1999) Influence of Dietary Rapeseed Oli, Vitamin E, and Copper on Performance and Antioxidant and Oxidative Status of Pigs. J. Anim. Sci.77:906-916
#' 
#' @examples 
#' ## For examples see example(QIC)
NULL
