#' Generate autocorrelated random numbers with negative binomial distribution
#'
#' @inheritParams stats::rnbinom
#' @param rho autocorrelation at lag 1
#' @param return_Y whether to return the intermediate Y distribution used to generate X. default FALSE.
#'
#' @return vector of length n
#' @export
#'
#' @examples
#' rnbinomAR(100, size = 1.3, mu = 3, rho = 0.8)
rnbinomAR <- function(n, size, prob, mu, rho, return_Y = FALSE) {

  if (!missing(prob) & !missing(mu)) {
    rlang::abort('Use either `prob` or `mu`')
  }

  if (!missing(prob)) {
    beta <- beta_from_p(prob)
  }
  if (missing(prob)) {
    beta <- beta_from_mu(mu, size)
  }

  # This differs from Gourieroux & Lu, as theirs (c = 1) yielded incorrect means.
  c_param = rho/beta

  # just initialise the whole vector
  if (!missing(prob)) {
    X <- rnbinom(n, size = size, prob = prob)
  }
  if (!missing(mu)) {
    X <- rnbinom(n, size = size, mu = mu)
  }

  # Initialise the intensity process
  Y <- X*NA

  # Build the sequence one step at a time according to Gourieroux & Lu Definition 1.
  for (i in 2:length(X)) {
    Y[i] <- rgamma(1, shape = size+X[i-1], scale = c_param)
    X[i] <- rpois(1, lambda = beta*Y[i])
  }

  if (return_Y) {
    return(list(X = X, Y = Y))
  } else {
    return(X)
  }
}

#' Estimate parameters of an autocorrelated negative binomial
#'
#' Wraps [stats::acf()] and [fitdistrplus::fitdistr()]
#'
#' @param X
#'
#' @return a tibble with the fitted parameter values for size, mu, and rho
#' @export
#'
fit_nbinomAR <- function(X) {
  # check the size and mu
  musize <- fitdistrplus::fitdist(X, 'nbinom')
  # check the AR
  ac_x1 <- acf(X)$acf[2]

  # return tibble
  nbinar_est <- tibble::tibble(term = c(names(musize$estimate), 'rho'),
                               estimate = c(musize$estimate, ac_x1),
                               std_error = c(musize$sd, NA))

  return(nbinar_est)

}

#' Get empirical and negative binomial distributions
#'
#' Particularly useful for checking fits look good
#'
#' @inheritParams rnbinomAR
#' @param X vector of values
#'
#' @return
#' @export
#'
nbin_emp_pmf <- function(X, size, prob, mu) {

  freq_x <- tibble::tibble(count = X) |>
    dplyr::summarise(empirical = dplyr::n()/length(X),
                     .by = count) |>
    dplyr::arrange(count)

  if (!missing(prob) & !missing(mu)) {
    rlang::abort('Use either `prob` or `mu`')
  }

  if (!missing(prob)) {
    x_dist <- tibble::tibble(count = 0:max(freq_x$count),
                             pmf = dnbinom(count,
                                           size = size,
                                           prob = prob))
  }
  if (missing(prob)) {
    x_dist <- tibble::tibble(count = 0:max(freq_x$count),
                             pmf = dnbinom(count,
                                           size = size,
                                           mu = mu))
  }

  # Do I want to be able to do this for the *given* params and the *fitted* params? Or just do it twice? I think probably do it twice, otherwise this gets VERY specific.


  # Join into one dataframe
  x_dist <- x_dist |>
    dplyr::left_join(freq_x)

  return(x_dist)
}

#' Diagnostic plot of empirical and theoretical negative binomial
#'
#' @param distdf return from [nbin_emp_pmf()]
#'
#' @return
#' @export
#'
#' @examples
nbin_gg <- function(distdf) {

  nbin_gg_check <- distdf |>
    pivot_longer(-count) |>
    ggplot(aes(x = count, y = value, color = name)) +
    geom_line() +
    labs(y = 'P(X=x)', color = '')

  return(nbin_gg_check)
}

#' Chi-square for nbin
#'
#' @param distdf return from [nbin_emp_pmf()]
#'
#' @return
#' @export
#'
#' @examples
nbin_chi <- function(distdf, grouper) {
  nbc <- distdf |>
    summarise(chi_p = chisq.test(empirical, pmf)$p.value)

  return(nbc)

}

# Helpers -----------------------------------------------------------------

p_from_beta <- function(beta) {
  beta/(beta + 1)
}

p_from_mu <- function(mu, size) {
  size / (size + mu)
}

beta_from_p <- function(p) {
  -p/(p-1)
}

mu_from_p <- function(p, size) {
  (size*(1-p))/p
}

beta_from_mu <- function(mu, size) {
  p <- p_from_mu(mu, size)
  beta_from_p(p)
}

mu_from_beta <- function(beta, size) {
  p <- p_from_beta(beta)
  mu_from_p(p, size)
}
