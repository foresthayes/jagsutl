# Functions require the following packages:
# MCMCvis
# knitr
# kableExtra
# dplyr

require(dplyr)

md_trace <- function(x){
  params <- x$parameters.to.save   
  MCMCvis::MCMCtrace(
    x,
    params = params[1:(length(params)-1)],
    # type = "density",
    ind = T,
    pdf = F
  )
  return(invisible(x))
}

md_result_table <- function(x){
  x$BUGSoutput$summary %>%
    as.data.frame() %>%
    round(3) %>%
    select(mean, sd, '2.5%', '97.5%', Rhat) %>%
    knitr::kable(format = "html") %>%
    kableExtra::kable_styling(bootstrap_options = c("striped", "hover"), full_width = F, position = "left") %>%
    print()
  
  return(invisible(x))
}

md_model_settings <- function(x){
  DIC <- round(x$BUGSoutput$DIC, 0)
  ni <- x$BUGSoutput$n.iter
  nb <- x$BUGSoutput$n.burnin
  
  tibble::tibble(
    DIC = DIC,
    Iterations = ni,
    Burnin = nb
  ) %>%
    knitr::kable(format = "html") %>%
    kableExtra::kable_styling(bootstrap_options = c("striped", "hover"), full_width = F, position = "left") %>%
    print()

  return(invisible(x))
}  

# Example call:
# model_result %>%
#   md_trace() %>%
#   md_result_table() %>%
#   md_model_settings()

# NOTE: in markdown use chunk options: echo = F, results = 'asis'



