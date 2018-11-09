#' Calcualte WAIC for a jags model using rjags
#'
#' Model paremeters follow the same naming conventions as R2jags and jags UI.
#'
#' @param data
#' @param parameters.to.save
#' @param model.file
#' @param n.chains
#' @param n.burnin
#' @param n.iter
#' @param n.thin
#' @param inits
#'
#' @return
#' @export
#'
#' @examples

utl_jags_waic <- function(
                          data,
                          inits,
                          parameters.to.save,
                          model.file,
                          n.chains,
                          n.burnin,
                          n.iter,
                          n.thin){

  parameters.to.save <- c("deviance", "WAIC")

  rjags::load.module("dic")

  m <- rjags::jags.model(
    model.file,
    data,
    inits,
    n.chains
  )

  update(m, n.burnin)

  s <- rjags::jags.samples(
    m,
    parameters.to.save,
    type = "mean",
    n.iter,
    n.thin
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
