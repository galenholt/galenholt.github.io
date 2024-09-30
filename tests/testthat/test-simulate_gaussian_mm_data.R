library(ggplot2)

test_that("data generation", {

  # even spread of values, no variance
  evenspreadnovar <- simulate_gaussian_mm(n_per_cluster = 10,
                       n_clusters = 20,
                       intercept = 1,
                       slope = 0.5,
                       sigma = 0.1,
                       sd_rand_intercept = 0.2,
                       sd_rand_slope = 0,
                       rand_si_cor = 0,
                       cluster_x = 1:20,
                       obs_x_sd = 0)

  expect_equal(unique(evenspreadnovar$x), 1:20)

  # obs independent, no cluster diffs
  withr::with_seed(17, obsxnormal <- simulate_gaussian_mm(n_per_cluster = 10,
                                          n_clusters = 20,
                                          intercept = 1,
                                          slope = 0.5,
                                          sigma = 0.1,
                                          sd_rand_intercept = 0.2,
                                          sd_rand_slope = 0,
                                          rand_si_cor = 0,
                                          cluster_x = 0,
                                          obs_x_sd = 1))
  obsxnormalplot <- ggplot(obsxnormal, aes(x = x, y = y, color = factor(cluster))) + geom_point()
  vdiffr::expect_doppleganger('obsxnormal', obsxnormalplot)

  # obs independent, cluster diffs
  withr::with_seed(17, obsxnormalc <- simulate_gaussian_mm(n_per_cluster = 10,
                                                          n_clusters = 20,
                                                          intercept = 1,
                                                          slope = 0.5,
                                                          sigma = 0.1,
                                                          sd_rand_intercept = 0.2,
                                                          sd_rand_slope = 0,
                                                          rand_si_cor = 0,
                                                          cluster_x = 1:20,
                                                          obs_x_sd = 0.5))
  obsxnormalcplot <- ggplot(obsxnormalc, aes(x = x, y = y, color = factor(cluster))) + geom_point()
  vdiffr::expect_doppleganger('obsxnormalc', obsxnormalplotc)

  # obs independent, cluster runif
  withr::with_seed(17, obsxnormalu <- simulate_gaussian_mm(n_per_cluster = 10,
                                                           n_clusters = 20,
                                                           intercept = 1,
                                                           slope = 0.5,
                                                           sigma = 0.1,
                                                           sd_rand_intercept = 0.2,
                                                           sd_rand_slope = 0,
                                                           rand_si_cor = 0,
                                                           cluster_x = \(x) runif(x, min = -5, max = 10),
                                                           obs_x_sd = 0.5))
  obsxnormaluplotu <- ggplot(obsxnormalu, aes(x = x, y = y, color = factor(cluster))) + geom_point()
  vdiffr::expect_doppleganger('obsxnormalu', obsxnormalplotu)

  # obs independent, cluster rnorm
  withr::with_seed(17, obsxnormaln <- simulate_gaussian_mm(n_per_cluster = 10,
                                                           n_clusters = 20,
                                                           intercept = 1,
                                                           slope = 0.5,
                                                           sigma = 0.1,
                                                           sd_rand_intercept = 0.2,
                                                           sd_rand_slope = 0,
                                                           rand_si_cor = 0,
                                                           cluster_x = \(x) rnorm(x, mean = 0, sd = 5),
                                                           obs_x_sd = 0.5))
  obsxnormaluplotn <- ggplot(obsxnormaln, aes(x = x, y = y, color = factor(cluster))) + geom_point()
  vdiffr::expect_doppleganger('obsxnormaln', obsxnormalplotn)
})
