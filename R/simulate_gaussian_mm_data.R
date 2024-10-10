#' Simulate data with random effects structure.
#'
#' This is only very slightly modified from Clark (2019, May 14), which is an *excellent* visual treatment of shrinkage in mixed models. Michael Clark: Shrinkage in Mixed Effects Models. Retrieved from https://m-clark.github.io/posts/2019-05-14-shrinkage-in-mixed-models/
#' Modifications include much more control over how x works and the form of unbalancing, along with renaming and shifting some control to the user.
#' The choice of x structure is important here, and reflects sampling strategies and experimental design. I've added a lot of ability to control it, though we may still need more.
#' This has one level of random factor, 'cluster', with some number of observations within it.
#'
#' @param n_clusters Number of clusters (levels of random factor)
#' @param intercept True intercept of the relationship between x and y
#' @param slope True slope of the x-y relationship
#' @param obs_sigma Observation-level sd, i.e. variance relative to the cluster mean (residual variance)
#' @param sd_rand_intercept Standard deviation of the random intercept, i.e. constant variance of the random levels around the line
#' @param sd_rand_slope Standard deviation of the random slope, i.e. how much does the slope of the x-y relationship vary between random levels
#' @param rand_si_cor Correlation between random slope and intercept.
#' @param cluster_x How to handle x-values of the cluster. If numeric and length n_clusters, it gives the x of each cluster. If a function, e.g. \(x) runif(x, min = -1, max = 2) or \(x) rnorm(x), it chooses the cluster-mean x's from those distributions. In the special case of a length-1 numeric, it gives a common value to all clusters, which is particularly useful if you want the observations to vary along x but not the clusters themselves (as the original function did).
#' @param obs_x_sd Standard deviation of observational x-variation around cluster means. If 0, all obs in a cluster have the same value. If cluster_x is length-1, then this just yields a common distribution for all obs.
#' @param N total number of observations
#' @param cluster_N character, 'fixed', 'uneven', with fixed yielding N/n_clusters per cluster, uneven being a random number of obs in each cluster chosen by rnbinom.
#' @param nobs_mean character, 'fixed', 'x'. If `cluster_N = 'uneven'`, this controls whether the `mu` term in [stats::rnbinom()] is the same for all clusters (n_clusters/N) or varies (with x, but that may be shuffled according to `uneven_with_x`)
#' @param force_nclusters logical- force the number of realised clusters to equal n_clusters. Done by adding 1 to any clusters that randomly end up with 0.
#' @param force_N logical- force the total realised obs to equal N. Done by sampling from the realised obs in clusters given its probabilities until we get N.
#' @param nbsize `size` argument to [stats::rnbinom()]
#' @param uneven_with_x logical- keep any uneven rnbinom `mu` correlated with x (TRUE) or shuffle relative to x (FALSE)
#' @param x_is_density logical- re-calculate x directly from the number of obs in the cluster; e.g. x *is* density.
#'
#' @return a dataframe of simulated data
#' @export
#'
simulate_gaussian_mm <- function(n_clusters,
                                 N,
                                 intercept,
                                 slope,
                                 obs_sigma,
                                 sd_rand_intercept,
                                 sd_rand_slope,
                                 rand_si_cor,
                                 obs_x_sd,
                                 cluster_N = 'fixed',
                                 nobs_mean = 'fixed',
                                 force_nclusters = TRUE,
                                 force_N = TRUE,
                                 cluster_x,
                                 nbsize,
                                 uneven_with_x = FALSE,
                                 x_is_density = FALSE) {

  # we sometimes base uneven mean on x, at least right now, so get x first (noting that it gets overwritten if x = density)

  # Get the x-values for each observation. They could be set by cluster, or vary around the cluster, or independent.
  # If cluster_x is numeric, give each cluster those values
  if (is.numeric(cluster_x)) {
    if (length(cluster_x) == 1) {
      xc = rep(cluster_x, n_clusters)
    } else if (length(cluster_x) == n_clusters) {
      xc <- cluster_x
    } else {
      rlang::abort("cluster_x is numeric but wrong length")
    }
  }

  # If cluster_x is a function (e.g. \(x) runif(x, min = -1, max = 2), rnorm), then give each cluster values chosen from that dist
  if (rlang::is_function(cluster_x)) {
    xc <- cluster_x(n_clusters)
  }

  # get the number of observatons in each cluster. THis has become a beast, so making it a subfunciton
  n_per_cluster <- get_clusterobs(n_clusters = n_clusters,
                                  N = N,
                                  xvals = xc,
                                  cluster_N = cluster_N,
                                  nobs_mean = nobs_mean,
                                  force_nclusters = force_nclusters,
                                  force_N = force_N,
                                  nbsize = nbsize,
                                  uneven_with_x = uneven_with_x)

  # Now, if x is density, it needs to match the number of observations, so re-calculate it. even if there's a translation
  if (x_is_density) {
      # Get the range about right. This will work better if we're always positive. We can't actually *stretch* the range out while assuming clusters are the same physical size
      maxshift <- max(xc)/max(n_per_cluster)
      xc <- n_per_cluster*maxshift
  }

  # These next two bits are really about having variation in x within clusters and random slopes
  # Make x then normally distributed around those cluster x means. I guess there might be a reason to do this uniform too, but for now, don't bother.
  x <- rnorm(sum(n_per_cluster), mean = rep(xc, times = n_per_cluster), sd = obs_x_sd)
  # Get the vcov matrix of the random effect betas
  varmat <- matrix(c(sd_rand_intercept^2, rand_si_cor, rand_si_cor, sd_rand_slope^2), 2, 2)

  # Realised cluster deviations in y
  re = mvtnorm::rmvnorm(n_clusters, sigma = varmat)
  colnames(re) <- c('intercept', 'slope')

  # cluster identity
  cluster <- rep(1:n_clusters, times = n_per_cluster)

  # Realized y, accounting for cluster deviations in intercept and slope betas and residual (observation within cluster) error
  y <- (intercept + re[cluster, 'intercept']) + (slope + re[cluster, 'slope'])*x + rnorm(sum(n_per_cluster), sd = obs_sigma)

  # make cluster obviously a category, not numeric.
  return(tibble::tibble(x, y, cluster = as.character(cluster)))
}

#' Get the observations in each cluster
#'
#' @inheritParams simulate_gaussian_mm
#'
#' @return
#' @export
#'
#' @examples
get_clusterobs <- function(n_clusters,
                           N,
                           xvals,
                           cluster_N = 'fixed',
                           nobs_mean = 'fixed',
                           force_nclusters = TRUE,
                           force_N = TRUE,
                           nbsize,
                           uneven_with_x = FALSE) {
  # We could generalise this to reduce dependence on x (ie set the probabilities elsewhere, and then sort by x if we want it correlated, thoguh that would need to have noise somehow)
  # That approach, using a Poisson or nbinom (with a +1 adjustment) might be a really good way to control the proportion singletons.
  # This approach does keep things consistent.

  # Get the Observations per cluster.
  # If cluster_N is fixed, give each cluster the same number of obs.
  if (cluster_N == 'fixed') {
    if (!is.null(N)) {
      n_per_cluster <- rep(round(N/n_clusters), n_clusters)
    } else {
      rlang::abort("cluster_N cannot be fixed if N is not set.")
    }
    # If it's fixed, we just return here.
    return(n_per_cluster)
  }

  if (cluster_N == 'uneven') {
    if (nobs_mean == 'fixed') {
      if (!is.null(N)) {
        cmean <- rep(round(N/n_clusters), n_clusters)
      } else {
      rlang::abort("nobs_mean cannot be fixed if N is not set.")
      }
    }
    if (nobs_mean == 'x') {
      cmean <- xvals
    }

    n_per_cluster <- rnbinom(n_clusters, size = nbsize, mu = cmean)
  }

  if (force_nclusters) {
    n_per_cluster[n_per_cluster < 1] <- 1
  }

  if (force_N) {

    # if we're under, go up
    while(sum(n_per_cluster) < N) {
      adders <- sample(1:n_clusters, size = N-sum(n_per_cluster), replace = TRUE,
                       prob = proportions(n_per_cluster)) |> table()
      for (i in 1:length(adders)) {
        thisindex <- as.numeric(names(adders[i]))

        n_per_cluster[thisindex] <- n_per_cluster[thisindex]+adders[i]
      }
    }

    # If we drew from nbin or added one(s), we might be over N. Remove but not if it pushes somethign below 0
    # Try to retain the distribution, though this will tend to flatten things out.

    while(sum(n_per_cluster) > N) {
      removers <- sample(1:n_clusters, size = sum(n_per_cluster) - N, replace = TRUE,
                         prob = proportions(n_per_cluster)) |> table()
      for (i in 1:length(removers)) {
        thisindex <- as.numeric(names(removers[i]))
        thisper <- n_per_cluster[thisindex]

        n_per_cluster[thisindex] <- thisper-removers[i]

      }

      # Don't drop below 1 if we can't lose clusters, and never go below 0
      if (force_nclusters) {
        n_per_cluster[n_per_cluster < 1] <- 1
      } else {
        n_per_cluster[n_per_cluster < 0] <- 0
      }
    }
  }




  # if we have `nobs_mean = x`, nobs is correlated with x break that if desired.
  if (!uneven_with_x) {
    n_per_cluster <- sample(n_per_cluster)
  }

  return(n_per_cluster)
}
