#' Pretty jags model results using knitr
#'
#' @param x An object of class rjags (from R2jags) or jagsUI
#'
#' @return A knitr::kable with the jags summary
#' @export
#'
#' @examples jags_result %>% utl_jags_rslt_tbl
utl_jags_rslt_tbl <- function(x){

  if(class(x) == "rjags"){ df <- x$BUGSoutput$summary}
  if(class(x) == "jagsUI"){ df <- x$summary}

  df %>%
    as.data.frame() %>%
    round(3) %>%
    select(mean, sd, '2.5%', '97.5%', Rhat) %>%
    knitr::kable(format = "html") %>%
    kableExtra::kable_styling(bootstrap_options = c("striped", "hover"), full_width = F, position = "left") %>%
    print()

  return(invisible(x))
}
