#' Simulate data with random effects structure.
#'
#' This is only very slightly modified from Clark (2019, May 14), which is an *excellent* visual treatment of shrinkage in mixed models. Michael Clark: Shrinkage in Mixed Effects Models. Retrieved from https://m-clark.github.io/posts/2019-05-14-shrinkage-in-mixed-models/
#' Modifications include much more control over how x works and the form of unbalancing, along with renaming and shifting some control to the user.
#' The choice of x structure is important here, and reflects sampling strategies and experimental design. I've added a lot of ability to control it, though we may still need more.
#' This has one level of random factor, 'cluster', with some number of observations within it.
#'
#' @param n_per_cluster Number of observations within a cluster
#' @param n_clusters Number of clusters (levels of random factor)
#' @param intercept True intercept of the relationship between x and y
#' @param slope True slope of the x-y relationship
#' @param sigma Observation-level sd, i.e. variance relative to the cluster mean (residual variance)
#' @param sd_rand_intercept Standard deviation of the random intercept, i.e. constant variance of the random levels around the line
#' @param sd_rand_slope Standard deviation of the random slope, i.e. how much does the slope of the x-y relationship vary between random levels
#' @param rand_si_cor Correlation between random slope and intercept.
#' @param cluster_x How to handle x-values of the cluster. If numeric and length n_clusters, it gives the x of each cluster. If a function, e.g. \(x) runif(x, min = -1, max = 2) or \(x) rnorm(x), it chooses the cluster-mean x's from those distributions. In the special case of a length-1 numeric, it gives a common value to all clusters, which is particularly useful if you want the observations to vary along x but not the clusters themselves (as the original function did).
#' @param obs_x_sd Standard deviation of observational x-variation around cluster means. If 0, all obs in a cluster have the same value. If cluster_x is length-1, then this just yields a common distribution for all obs.
#'
#' @return a dataframe of simulated data
#' @export
#'
simulate_gaussian_mm <- function(n_per_cluster,
                                 n_clusters,
                                 intercept,
                                 slope,
                                 sigma,
                                 sd_rand_intercept,
                                 sd_rand_slope,
                                 rand_si_cor,
                                 cluster_x,
                                 obs_x_sd) {

  # cluster identity
  cluster <- rep(1:n_clusters, each = n_per_cluster)
  # Number of observations in total
  N <- n_clusters * n_per_cluster

  # Get the x-values for each observation. They could be set by cluster, or vary around the cluster, or independent.
  # If cluster_x is numeric, give each cluster those values
  if (is.numeric(cluster_x)) {
    if (length(cluster_x == 1)) {
      xc = rep(cluster_x, n_clusters)
    } else if (length(cluster_x) == n_clusters) {
      xc <- cluster_x
    } else {
      rlang::abort("cluster_x is numeric but wrong length")
    }
  }

  # If cluster_x is a function (e.g. \(x) runif(x, min = -1, max = 2), rnorm), then give each cluster values chosen from that dist
  if (rlang::is_function(cluster_x)) {
    xc = cluster_x(n_clusters)
  }

  # Make x then normally distributed around those cluster means. I guess there might be a reason to do this uniform too, but for now, don't bother.
  x <- rnorm(N, mean = rep(xc, each = n_per_cluster), sd = obs_x_sd)

  # Get the vcov matrix of the random effect betas
  varmat <- matrix(c(sd_rand_intercept^2, rand_si_cor, rand_si_cor, sd_rand_slope^2), 2, 2)

  # Realised cluster deviations in y
  re = mvtnorm::rmvnorm(n_clusters, sigma = varmat)
  colnames(re) <- c('intercept', 'slope')

  # Realized y, accounting for cluster deviations in intercept and slope betas and residual (observation within cluster) error
  y <- (intercept + re[cluster, 'intercept']) + (slope + re[cluster, 'slope'])*x + rnorm(N, sd = sigma)

  # make cluster obviously a category, not numeric.
  return(tibble::tibble(x, y, cluster = as.character(cluster)))
}
