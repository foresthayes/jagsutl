#' Calcualte WAIC for a jags model using rjags
#'
#' @param data
#' @param inits
#' @param parms
#' @param model
#' @param chains
#' @param burnin
#' @param iter
#' @param thin
#'
#' @return
#' @export
#'
#' @examples

utl_jags_waic <- function(
  data,
  inits,
  parms,
  model,
  chains,
  burnin,
  iter,
  thin){

  rjags::load.module("dic")

  m <- rjags::jags.model(
    model,
    data,
    inits,
    chains
  )

  update(m, burnin)

  s <- rjags::jags.samples(
    m,
    parms,
    type = "mean",
    iter,
    thin
  )

  tmp <- s %>%
    lapply(unclass) %>%
    sapply(sum) %>%
    magrittr::extract(c("deviance", "WAIC"))

  waic <- (tmp["deviance"] + 2 * tmp["WAIC"]) %>%
    magrittr::set_names("WAIC") %>%
    print()

  return(waic)
}
