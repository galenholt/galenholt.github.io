#' Simulate data with random effects structure.
#'
#' This is only very slightly modified from Clark (2019, May 14), which is an
#' *excellent* visual treatment of shrinkage in mixed models. Michael Clark:
#' Shrinkage in Mixed Effects Models. Retrieved from
#' https://m-clark.github.io/posts/2019-05-14-shrinkage-in-mixed-models/
#' Modifications include much more control over how x works and the form of
#' unbalancing, along with renaming and shifting some control to the user. The
#' choice of x structure is important here, and reflects sampling strategies and
#' experimental design. I've added a lot of ability to control it, though we may
#' still need more. This has one level of random factor, 'cluster', with some
#' number of observations within it.
#'
#' @param n_clusters Number of clusters (levels of random factor)
#' @param n_subclusters Number of subclusters (total). Can be a vector to have
#'   multiple levels of nesting
#' @param intercept True intercept of the relationship between x and y
#' @param slope True slope of the x-y relationship
#' @param obs_sigma Observation-level sd, i.e. variance relative to the cluster
#'   mean (residual variance)
#' @param sd_rand_intercept Standard deviation of the random intercept, i.e.
#'   constant variance of the random levels around the line. If using
#'   subclusters, should be a vector with a value for each scale, starting with
#'   cluster, then subcluster 1, 2, ...
#' @param sd_rand_slope Standard deviation of the random slope, i.e. how much
#'   does the slope of the x-y relationship vary between random levels. If using
#'   subclusters, should be a vector with a value for each scale, starting with
#'   cluster, then subcluster 1, 2, ...
#' @param rand_si_cor Correlation between random slope and intercept.If using
#'   subclusters, should be a vector with a value for each scale, starting with
#'   cluster, then subcluster 1, 2, ...
#' @param cluster_x How to handle x-values of the cluster. If numeric and length
#'   n_clusters, it gives the x of each cluster. If a function, e.g. \(x)
#'   runif(x, min = -1, max = 2) or \(x) rnorm(x), it chooses the cluster-mean
#'   x's from those distributions. In the special case of a length-1 numeric, it
#'   gives a common value to all clusters, which is particularly useful if you
#'   want the observations to vary along x but not the clusters themselves (as
#'   the original function did).
#' @param obs_x_sd Standard deviation of observational x-variation around
#'   cluster means. If 0, all obs in a cluster have the same value. If cluster_x
#'   is length-1, then this just yields a common distribution for all obs.
#'   *Note*, this is not set up to have differences in the x of different
#'   subclusters/nestings- the x determination is done for observations within
#'   the largest cluster.
#' @param N total number of observations
#' @param cluster_N character, 'fixed', 'uneven', with fixed yielding
#'   N/n_clusters per cluster, uneven being a random number of obs in each
#'   cluster chosen by rnbinom. Can be vector if using subclusters and want
#'   different values at different levels
#' @param nobs_mean character, 'fixed', 'x'. If `cluster_N = 'uneven'`, this
#'   controls whether the `mu` term in [stats::rnbinom()] is the same for all
#'   clusters (n_clusters/N) or varies (with x, but that may be shuffled
#'   according to `uneven_with_x`)
#' @param force_nclusters logical- force the number of realised clusters to
#'   equal n_clusters. Done by adding 1 to any clusters that randomly end up
#'   with 0. Can be vector if using subclusters and want different values at
#'   different levels
#' @param force_N logical- force the total realised obs to equal N. Done by
#'   sampling from the realised obs in clusters given its probabilities until we
#'   get N.
#' @param nbsize `size` argument to [stats::rnbinom()]
#' @param uneven_with_x logical- keep any uneven rnbinom `mu` correlated with x
#'   (TRUE) or shuffle relative to x (FALSE)
#' @param x_is_density logical- re-calculate x directly from the number of obs
#'   in the cluster; e.g. x *is* density.
#'
#' @return a dataframe of simulated data
#' @export
#'
simulate_gaussian_mm <- function(n_clusters,
                                 n_subclusters = NULL,
                                 N,
                                 intercept,
                                 slope,
                                 obs_sigma,
                                 sd_rand_intercept,
                                 sd_rand_slope,
                                 rand_si_cor,
                                 obs_x_sd,
                                 cluster_N = "fixed",
                                 nobs_mean = "fixed",
                                 force_nclusters = TRUE,
                                 force_nsubclusters = TRUE,
                                 force_N = TRUE,
                                 cluster_x,
                                 nbsize,
                                 uneven_with_x = FALSE,
                                 x_is_density = FALSE) {
  # Deal with some expansions
  if (!is.null(n_subclusters)) {
    if (length(cluster_N) == 1) {
      cluster_N <- rep(cluster_N, length(n_subclusters) + 1)
    }
    if (length(force_nclusters) == 1) {
      force_nclusters <- rep(force_nclusters, length(n_subclusters) + 1)
    }
    if (length(sd_rand_intercept) == 1) {
      rlang::warn("Using nested random effects with the same random intecept variation at each level.
                  Is that really what you want to do?")
      sd_rand_intercept <- rep(sd_rand_intercept, length(n_subclusters) + 1)
    }
    if (length(sd_rand_slope) == 1) {
      rlang::warn("Using nested random effects with the same random slope variation at each level.
                  Is that really what you want to do?")
      sd_rand_slope <- rep(sd_rand_slope, length(n_subclusters) + 1)
    }
    if (length(rand_si_cor) == 1) {
      # I should probably warn here too, but this seems less of an issue to keep
      # constant.
      rand_si_cor <- rep(rand_si_cor, length(n_subclusters) + 1)
    }
  }

  # we sometimes base uneven mean on x, at least right now, so get x first
  # (noting that it gets overwritten if x = density)

  # Get the x-values for each observation. They could be set by cluster, or vary
  # around the cluster, or independent. If cluster_x is numeric, give each
  # cluster those values
  if (is.numeric(cluster_x)) {
    if (length(cluster_x) == 1) {
      xc <- rep(cluster_x, n_clusters)
    } else if (length(cluster_x) == n_clusters) {
      xc <- cluster_x
    } else {
      rlang::abort("cluster_x is numeric but wrong length")
    }
  }

  # If cluster_x is a function (e.g. \(x) runif(x, min = -1, max = 2), rnorm),
  # then give each cluster values chosen from that dist
  if (rlang::is_function(cluster_x)) {
    xc <- cluster_x(n_clusters)
  }

  #
  #   # Ge the number of obs in each subcluster We can just treat the subclusters as
  #   # the base scale where obs get distributed, and then put them in clusters
  #   if (!is.null(n_subclusters)) {
  #     n_per_subcluster <- get_clusterobs(n_clusters = n_subclusters,
  #                                        N = N,
  #                                        xvals = xc, # This will cause trouble, b/c they're assigned to the cluster
  #                                        cluster_N = subcluster_N,
  #                                        nobs_mean = nobs_mean,
  #                                        force_nclusters = force_nsubclusters,
  #                                        force_N = force_N,
  #                                        nbsize = nbsize,
  #                                        uneven_with_x = uneven_with_x)
  #   }
  #
  #
  #   # get the number of observations in each cluster.
  #   # 1. One option with subclusters is to just cut them into n_clusters groups, and
  #   # let the chips fall where they may. But that would end up with constant
  #   # subclusters per cluster
  #   # 2. Another would be to use this to set how many obs should be in each
  #   # *cluster*, and then arrange the subclusters into groups to yield numbers as
  #   # close as possible to those.
  #     # I kind of like that approach; it would give the uneven_with_x a way to
  #     # work. But what's the algorithm to do the distribution? Randomly grab then
  #     # stop when they go over? Distribute the subclusters randomly biggest first,
  #     # moving on if they don't fit in the first choice?
  #   # 3. We could use this to choose how many *subclusters* go in each cluster,
  #   # and then divide up the subclusters
  #     # I THINK THIS IS THE WAY, AND SPIN IT AROUND- DO THIS FIRST.
  #     # Set the subclusters in each cluster
  #     # THen we can expand xc to the right length.
  #     # Still pick N in each subcluster from overall N, I think (rather than
  #     # finding an N in each cluster first). We'll just know a priori which
  #     # cluster they belong to.
  #     # Would also make generalising even easier, we could just drill down in a loop using the N from the higher level each time.
  #
  #   # In any case, will we end up with the ability to have density vary at both
  #   # scales? well, sort of, in the sense that lower cluster numbers will be more
  #   # likely to have smaller subcluster numbers. But it's not a hard connection,
  #   # nd so on average, it wouldn't happen at all except with 2. because high
  #   # subclusters can't fit in low-obs clusters.
  #   n_per_cluster <- get_clusterobs(n_clusters = n_clusters,
  #                                   N = N,
  #                                   xvals = xc,
  #                                   cluster_N = cluster_N,
  #                                   nobs_mean = nobs_mean,
  #                                   force_nclusters = force_nclusters,
  #                                   force_N = force_N,
  #                                   nbsize = nbsize,
  #                                   uneven_with_x = uneven_with_x)

  # Try to get this generic and loopy
  step_n <- c(n_clusters, n_subclusters, N)
  # We need to save the n_per so we can know which cluster each subcluster is in
  n_per <- vector(mode = "list", length = length(step_n) - 1)
  xc <- list(xc)

  # Loop over (will need to stop the loop 1 short of length)
  # Assume for now that cluster_N, nobs_mean, force, and negbin settings hold
  # constant. we can just make all those arguments able to be vectors though.

  for (l in 1:(length(step_n) - 1)) {
    n_per[[l]] <- get_clusterobs(
      n_clusters = step_n[l],
      N = step_n[l + 1],
      xvals = xc[[l]],
      cluster_N = cluster_N[l],
      nobs_mean = nobs_mean,
      force_nclusters = force_nclusters[l],
      force_N = force_N,
      nbsize = nbsize,
      uneven_with_x = uneven_with_x
    )

    # Preserve the xc steps too for later
    # The last item is the x-vals for the obs.
    xc[[l + 1]] <- rep(xc[[l]], times = n_per[[l]])
  }


  # so that gives the number of lower levels inside each higher level.
  # We do want the number of obs within the highest level sometimes too.
  # may not actually need this
  upper_id <- vector(mode = "list", length = length(n_per))
  for (l in seq_along(n_per)) {
    upper_id[[l]] <- rep(seq_along(n_per[[l]]), times = n_per[[l]])
  }

  # This would work, but maybe tibble is easier
  # agggroup <- aggregate(n_per[[3]], by = list(upper_id[[2]]), FUN = sum)
  # rep(agggroup[,1], times = agggroup$x)

  # Get the ids of every observation
  ids <- tibble::tibble(cluster = 1:n_clusters)
  for (l in seq_along(n_per)) {
    if (l == 1) {
      colname <- 'cluster'
    } else {
      colname <- glue::glue("sub_{l-1}")
    }
    ids <- ids |>
      dplyr::mutate("{colname}" := dplyr::row_number()) |>
      dplyr::bind_cols(nper = n_per[[l]]) |>
      tidyr::uncount(nper)
  }

  # and get the number of obs per cluster
  obs_per_cluster <- ids |>
    dplyr::summarise(obs_per_cluster = n(), .by = "cluster") |>
    # We need to not lose zeros if we aren't forcing
    dplyr::bind_rows(tibble(cluster = 1:step_n[1], obs_per_cluster = 0)) |>
    dplyr::summarise(obs_per_cluster = sum(obs_per_cluster), .by = 'cluster') |>
    dplyr::arrange(cluster) |>
    dplyr::pull()

  # Now, if x is density, it needs to match the number of observations, so
  # re-calculate it. even if there's a translation
  # We want this to be density *at the highest cluster level*
  if (x_is_density) {
    # Get the range about right. This will work better if we're always
    # positive. We can't actually *stretch* the range out while assuming
    # clusters are the same physical size
    maxshift <- max(xc[[1]]) / max(obs_per_cluster)
    xc[[1]] <- obs_per_cluster * maxshift
    # Keep things correct, even if not needed
    for (l in 1:(length(step_n) - 1)) {
      # Preserve the xc steps too for later
      # The last item is the x-vals for the obs.
      xc[[l + 1]] <- rep(xc[[l]], times = n_per[[l]])
    }
  }

  # These next two bits are really about having variation in x within clusters
  # and random slopes Make x then normally distributed around those cluster x
  # means. I guess there might be a reason to do this uniform too, but for now,
  # don't bother. Note- this reps by obs_per_cluster, which may be zero, and so
  # doesn't assign an x to missing clusters- we only want values for
  # observations/
  x <- rnorm(sum(obs_per_cluster),
             mean = rep(xc[[1]], times = obs_per_cluster),
             sd = obs_x_sd)

  # get the random deviations at each scale
  re <- vector(mode = "list", length = length(n_per))
  for (l in 1:(length(step_n) - 1)) {
    # Get the vcov matrix of the random effect betas (between random slope and
    # intercept, so this is always 2x2)
    varmat <- matrix(c(sd_rand_intercept[l]^2, rand_si_cor[l],
                       rand_si_cor[l], sd_rand_slope[l]^2), 2, 2)
    # Realised cluster deviations in y
    re[[l]] <- mvtnorm::rmvnorm(step_n[l], sigma = varmat)
    colnames(re[[l]]) <- c("intercept", "slope")
  }


  # cluster identity
  # cluster <- rep(1:n_clusters, times = n_per_cluster)

  # Realized y, accounting for cluster deviations in intercept and slope betas
  # and residual (observation within cluster) error
  # This is the one-level version, easier to see what we're doing.
  # y <-
  #   # Intercepts
  #   (intercept + re[cluster, 'intercept']) +
  #   # Slopes
  #   (slope + re[cluster, 'slope'])*x +
  #   # eps (residual error)
  #   rnorm(sum(n_per_cluster), sd = obs_sigma)

  # This just adds the intercepts and slopes from each level to get the overall
  # slope and intercept of each observation.
  obsintercept <- intercept
  obsslope <- slope
  for (l in seq_along(re)) {
    obsintercept <- obsintercept +
      re[[l]][dplyr::pull(ids[, l]), "intercept"]
    obsslope <- obsslope +
      re[[l]][dplyr::pull(ids[, l]), "slope"]
  }

  y <-
    # Intercepts
    obsintercept +
    # Slopes
    obsslope * x +
    # eps (residual error)
    rnorm(sum(obs_per_cluster), sd = obs_sigma)

  # ids needs to be numeric above to index into re, but we really want it to be a factor
  ids <- ids |>
    dplyr::mutate(across(everything(), as.character))

  return(dplyr::bind_cols(ids, tibble::tibble(x, y)))
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
                           cluster_N = "fixed",
                           nobs_mean = "fixed",
                           force_nclusters = TRUE,
                           force_N = TRUE,
                           nbsize,
                           uneven_with_x = FALSE) {
  # We could generalise this to reduce dependence on x (ie set the probabilities
  # elsewhere, and then sort by x if we want it correlated, thoguh that would
  # need to have noise somehow) That approach, using a Poisson or nbinom (with a
  # +1 adjustment) might be a really good way to control the proportion
  # singletons. This approach does keep things consistent.

  # Get the Observations per cluster.
  # If cluster_N is fixed, give each cluster the same number of obs.
  if (cluster_N == "fixed") {
    if (!is.null(N)) {
      n_per_cluster <- rep(round(N / n_clusters), n_clusters)
    } else {
      rlang::abort("cluster_N cannot be fixed if N is not set.")
    }
    # If it's fixed, we just return here.
    return(n_per_cluster)
  }

  if (cluster_N == "uneven") {
    if (nobs_mean == "fixed") {
      if (!is.null(N)) {
        cmean <- rep(round(N / n_clusters), n_clusters)
      } else {
        rlang::abort("nobs_mean cannot be fixed if N is not set.")
      }
    }
    if (nobs_mean == "x") {
      cmean <- xvals
    }

    n_per_cluster <- rnbinom(n_clusters, size = nbsize, mu = cmean)
  }

  if (force_nclusters) {
    n_per_cluster[n_per_cluster < 1] <- 1
  }

  if (force_N) {
    # if we're under, go up
    while (sum(n_per_cluster) < N) {
      adders <- sample(1:n_clusters,
        size = N - sum(n_per_cluster), replace = TRUE,
        prob = proportions(n_per_cluster)
      ) |> table()
      for (i in seq_along(adders)) {
        thisindex <- as.numeric(names(adders[i]))

        n_per_cluster[thisindex] <- n_per_cluster[thisindex] + adders[i]
      }
    }

    # If we drew from nbin or added one(s), we might be over N. Remove but not
    # if it pushes somethign below 0
    # Try to retain the distribution, though this will tend to flatten things out.

    while (sum(n_per_cluster) > N) {
      removers <- sample(1:n_clusters,
        size = sum(n_per_cluster) - N, replace = TRUE,
        prob = proportions(n_per_cluster)
      ) |> table()
      for (i in seq_along(removers)) {
        thisindex <- as.numeric(names(removers[i]))
        thisper <- n_per_cluster[thisindex]

        n_per_cluster[thisindex] <- thisper - removers[i]
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
