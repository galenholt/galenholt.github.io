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
#' @param obs_sigma Observation-level sd, i.e. variance relative to the cluster mean (residual variance)
#' @param sd_rand_intercept Standard deviation of the random intercept, i.e. constant variance of the random levels around the line
#' @param sd_rand_slope Standard deviation of the random slope, i.e. how much does the slope of the x-y relationship vary between random levels
#' @param rand_si_cor Correlation between random slope and intercept.
#' @param cluster_x How to handle x-values of the cluster. If numeric and length n_clusters, it gives the x of each cluster. If a function, e.g. \(x) runif(x, min = -1, max = 2) or \(x) rnorm(x), it chooses the cluster-mean x's from those distributions. In the special case of a length-1 numeric, it gives a common value to all clusters, which is particularly useful if you want the observations to vary along x but not the clusters themselves (as the original function did).
#' @param obs_x_sd Standard deviation of observational x-variation around cluster means. If 0, all obs in a cluster have the same value. If cluster_x is length-1, then this just yields a common distribution for all obs.
#'
#' @return a dataframe of simulated data
#' @export
#'
simulate_gaussian_mm <- function(n_clusters,
                                 N,
                                 cluster_N = round(N/n_clusters),
                                 intercept,
                                 slope,
                                 obs_sigma,
                                 sd_rand_intercept,
                                 sd_rand_slope,
                                 rand_si_cor,
                                 cluster_x,
                                 obs_x_sd,
                                 uneven_exponent = 0,
                                 uneven_with_x = FALSE,
                                 x_is_density = FALSE) {

  # Get the Observations per cluster.
  # They could be set or chosen randomly, whether independent or correlated with x or exactly x.
  # If cluster_N is numeric, give each cluster those values
  if (is.integer(cluster_N)) {
    if (length(cluster_N) == 1) {
      n_per_cluster <- rep(cluster_N, n_clusters)
    } else if (length(cluster_N) == n_clusters) {
      n_per_cluster <- cluster_N
    } else {
      rlang::abort("cluster_N is numeric but wrong length")
    }
  }

  # we base the uneven on x, at least right now, so get x first (noting that it gets overwritten if x = density)

  # does somethign like this work?
  # N <- 100
  # n_clusters <- 10
  #
  # binprobs <- rnbinom(n_clusters, 1, 0.2)
  # binprobs
  # sample(1:n_clusters, size = N, replace = TRUE, prob = binprobs/sum(binprobs))
  # # yes, but does it make sense as an approach? Especially when it comes time to make it correlated with x?

  # cluster identity
  # cluster <- rep(1:n_clusters, each = n_per_cluster)
  # # Number of observations in total
  # N <- n_clusters * n_per_cluster

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

  # We could generalise this to reduce dependence on x (ie set the probabilities elsewhere, and then sort by x if we want it correlated, thoguh that would need to have noise somehow)
    # That approach, using a Poisson or nbinom (with a +1 adjustment) might be a really good way to control the proportion singletons.
  # This approach does keep things consistent.
  if (is.character(cluster_N)) {
    if (cluster_N == 'uneven') {
      # number per cluster chosen randomly (lots of potential distributions, but a distribution based on the x values has some advantages)
      # This is kind of funny to assign and then count, but it helps hold N constant vs something like n_per_cluster <- rnbinom(length(xc), size = 1, prob = proportions(xc)), which is entirely open-ended
      n_per_cluster <- sample(1:n_clusters, size = N, replace = TRUE,
                              prob = proportions(xc^uneven_exponent)) |> table()
      # Needs some checks so we don't lose clusters entirely
      missing_clusters <- as.character(1:n_clusters)[!as.character(1:n_clusters) %in% rownames(n_per_cluster)]
      n_per_cluster <- c(n_per_cluster, rep(1, length(missing_clusters)) |> setNames(missing_clusters))
      n_per_cluster <- n_per_cluster[match(sort(as.numeric(names(n_per_cluster))), as.numeric(names(n_per_cluster)))]

      # If we added one(s), we'll be over N. Remove but not if it pushes somethign below 0
      # Try to retain the distribution though.

      while(sum(n_per_cluster) > N) {
        removers <- sample(1:n_clusters, size = length(missing_clusters), replace = TRUE,
                           prob = proportions(n_per_cluster)) |> table()
        for (i in removers) {
          thisindex <- names(n_per_cluster) == names(removers[1])
          thisper <- n_per_cluster[thisindex]
          if (thisper > 1) {
            n_per_cluster[thisindex] <- thisper - 1
          }
        }
      }


      # That's correlated with x

      # Uncorrelate with x if desired
      if (!uneven_with_x) {
        n_per_cluster <- sample(n_per_cluster)
      }

    } else if (cluster_N == 'even') {
      n_per_cluster <- rep(round(N/n_clusters), n_clusters)
    } else {
      rlang::abort('Character options for cluster_N other than "uneven" and "even" need to be written.')
    }

  }

  # Now, if x is density, it needs to match the number of observations, even if htere's a translation
  if (x_is_density) {
      # Get the range about right. This will work better if we're always positive. We can't actually *stretch* the range out while assuming clusters are the same physical size
      maxshift <- max(xc)/max(n_per_cluster)
      xc <- n_per_cluster*maxshift
  }

  # These next two bits are really about having variation in x within clusters and random slopes
  # Make x then normally distributed around those cluster x means. I guess there might be a reason to do this uniform too, but for now, don't bother.
  x <- rnorm(N, mean = rep(xc, times = n_per_cluster), sd = obs_x_sd)
  # Get the vcov matrix of the random effect betas
  varmat <- matrix(c(sd_rand_intercept^2, rand_si_cor, rand_si_cor, sd_rand_slope^2), 2, 2)

  # Realised cluster deviations in y
  re = mvtnorm::rmvnorm(n_clusters, sigma = varmat)
  colnames(re) <- c('intercept', 'slope')

  # cluster identity
  cluster <- rep(1:n_clusters, times = n_per_cluster)

  # Realized y, accounting for cluster deviations in intercept and slope betas and residual (observation within cluster) error
  y <- (intercept + re[cluster, 'intercept']) + (slope + re[cluster, 'slope'])*x + rnorm(N, sd = obs_sigma)

  # make cluster obviously a category, not numeric.
  return(tibble::tibble(x, y, cluster = as.character(cluster)))
}
