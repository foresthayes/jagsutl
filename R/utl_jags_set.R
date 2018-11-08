#' Produce a table of jags model settings from a jags object
#'
#' @param x  An object of class rjags (from R2jags) or jagsUI
#' @param ... Another object or value to add to the table
#'
#' @return
#' @export
#'
#' @examples jags_result %>% utl_jags_set(., waic_value)
utl_jags_set <- function(x, waic = NA, ...){

  if(class(x) == "rjags"){
    DIC <- round(x$BUGSoutput$DIC, 0)
    ni <- x$BUGSoutput$n.iter
    nb <- x$BUGSoutput$n.burnin
    nt <- x$BUGSoutput$n.thin
  }

  if(class(x) == "jagsUI"){
    DIC <- x$DIC %>% round(0)
    ni <- x$mcmc.info$n.iter
    nb <- x$mcmc.info$n.burnin
    nt <- x$mcmc.info$n.thin
  }

  tibble::tibble(
    DIC = DIC,
    Iterations = ni,
    Burnin = nb,
    Thin = nt,
    WAIC = waic,
    ...
  ) %>%
    knitr::kable(format = "html") %>%
    kableExtra::kable_styling(bootstrap_options = c("striped", "hover"), full_width = F, position = "left") %>%
    print()

  return(invisible(x))
}
