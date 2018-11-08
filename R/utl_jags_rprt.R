#' Wrapper function to produce jags reports for markdown
#'
#' @param x An object of class rjags (from R2jags) or jagsUI
#' @param waic A numeric value for waic
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
utl_jags_rprt <- function(x, waic, ...){
  x %>%
    utl_jags_trc() %>%
    utl_jags_rslt_tbl() %>%
    utl_jags_set(., waic, ...)
}
