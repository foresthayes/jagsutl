#' A simple wrapper for MCMC trace to produce jags graphcs in Rmarkdown
#'
#' @param x An object of class rjags (from R2jags) or jagsUI
#'
#' @return returns trace plots of tracked parameters without deviance
#' @export
#'
#' @examples jags_result %>% util_jags_trc()
#'
utl_jags_trc <- function(x){

  # assertthat::assert_that(class(x) == "rjags" | "jagsUI")

  if(class(x) == "rjags"){params <- x$parameters.to.save}
  if(class(x) == "jagsUI"){params <- x$parameters}

  MCMCvis::MCMCtrace(
    x,
    params = params[1:(length(params)-1)],
    # type = "density",
    ind = T,
    pdf = F
  )
  return(invisible(x))
}
