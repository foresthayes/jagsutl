#' A simple wrapper for MCMC trace to produce jags graphcs in Rmarkdown
#'
#' @param x An object of class rjags (from R2jags) or jagsUI
#'
#' @return Returns trace plots of tracked parameters
#'
#' @examples jags_result %>% util_jags_trc()
#'
#' # plot only density
#' jags_result %>% utl_jags_trc(type = "density")
#'
#'# produce a PDF
#'jags_result %>% utl_jags_trc(pdf = T)
#'
#'# Exclude b0.p from the plots
#'jags_result %>% utl_jags_trc(excl = "b0.p")
#'
#' @export
utl_jags_trc <- function(x, ..., excl = "deviance", ind = T, pdf = F ){

if(class(x) == "rjags"){params <- x$parameters.to.save}
if(class(x) == "jagsUI"){params <- x$parameters}

MCMCvis::MCMCtrace(
  x,
  ...,
  params = params,
  excl = excl,
  ind = ind,
  pdf = pdf
)
return(invisible(x))
}

