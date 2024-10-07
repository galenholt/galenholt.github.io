make_fullmodels <- function(simdata) {
  full_models <- list(
    cluster_rand_x = glmmTMB(y ~ x + (1|cluster), data = simdata),
    # This one ends up rank-deficient for the last cluster usually
    cluster_fixed_x = glmmTMB(y ~ x + cluster, data = simdata),
    cluster_fixed = glmmTMB(y ~ cluster, data = simdata),
    cluster_rand = glmmTMB(y ~ 1|cluster, data = simdata),
    no_cluster = glmmTMB(y ~ x, data = simdata)
  )
  return(full_models)
}

extract_terms <- function(full_models) {
  # Extraction as best I can of *just* the cluster estimates and se
  term_estimates <- imap(full_models, extract_glmmTMB_ests)
  # term_estimates <- list(
  #   cluster_rand_x = extract_glmmTMB_ests(full_models$cluster_rand_x),
  #   cluster_fixed_x = extract_glmmTMB_ests(full_models$cluster_fixed_x),
  #   cluster_rand = extract_glmmTMB_ests(full_models$cluster_rand),
  #   cluster_fixed = extract_glmmTMB_ests(full_models$cluster_fixed)
  # )

  return(term_estimates)
}

estimate_clusters <- function(full_models, simdata) {
  # Extraction as best I can of *just* the cluster estimates and se
  cluster_estimates <- list(
    cluster_rand_x = extract_cluster_terms(full_models$cluster_rand_x, simdata),
    cluster_fixed_x = extract_cluster_terms(full_models$cluster_fixed_x, simdata),
    cluster_rand = extract_cluster_terms(full_models$cluster_rand, simdata),
    cluster_fixed = extract_cluster_terms(full_models$cluster_fixed, simdata),

    # From the data
    cluster_raw = naive_clusters(simdata)
  )

  return(cluster_estimates)
}


predict_clusters <- function(full_models, simdata) {
  # Extraction of estimates from the full model for the clusters at their x values (this works here with no in-cluster x variation)
  cluster_predictions <- list(
    cluster_rand_x = predict_at_clusters(full_models$cluster_rand_x, simdata),
    cluster_fixed_x = predict_at_clusters(full_models$cluster_fixed_x, simdata),
    cluster_rand = predict_at_clusters(full_models$cluster_rand, simdata),
    cluster_fixed = predict_at_clusters(full_models$cluster_fixed, simdata),

    no_cluster = predict_at_clusters(full_models$no_cluster, simdata)
  )
  return(cluster_predictions)
}

model_from_clusters <- function(cluster_estimates) {
  cluster_models <- list(
    cluster_rand_x = glmmTMB(y ~ x, data = cluster_estimates$cluster_rand_x |>
                               rename(y = estimate)),
    cluster_fixed_x = glmmTMB(y ~ x, data = cluster_estimates$cluster_fixed_x |>
                                rename(y = estimate)),
    cluster_raw = glmmTMB(y ~ x, data = cluster_estimates$cluster_raw |>
                            rename(y = estimate)),
    # This is what we're doing in the plots, though there it's internally nested as well
    cluster_fixed = glmmTMB(y ~ x, data = cluster_estimates$cluster_fixed |>
                              rename(y = estimate))
  )
  return(cluster_models)
}


fit_x_full <- function(full_models, xvals) {
  fitted_x_lines <- list(
    # The full models
    cluster_rand_x = predict_fit(full_models$cluster_rand_x, xvals),
    cluster_fixed_x = predict_fit(full_models$cluster_fixed_x, xvals),
    no_cluster = predict_fit(full_models$no_cluster, xvals)
  )
  return(fitted_x_lines)
}

fit_x_cluster <- function(cluster_models, xvals) {
  fitted_x_lines_tocluster <- list(
    # Fits to the cluster estimates
    cluster_rand_x = predict_fit(cluster_models$cluster_rand_x, xvals),
    cluster_fixed_x = predict_fit(cluster_models$cluster_fixed_x, xvals),
    cluster_raw = predict_fit(cluster_models$cluster_raw, xvals),
    cluster_fixed = predict_fit(cluster_models$cluster_fixed, xvals)
  )
  return(fitted_x_lines_tocluster)
}

get_cluster_residuals <- function(cluster_models, cluster_estimates, simdata, ...) {
  cluster_deviations <- list(
    cluster_rand_x = cluster_residuals(cluster_models$cluster_rand_x, simdata,
                                       cluster_estimates$cluster_rand_x),
    cluster_fixed = cluster_residuals(cluster_models$cluster_fixed, simdata,
                                      cluster_estimates$cluster_fixed),
    cluster_raw = cluster_residuals(cluster_models$cluster_raw, simdata,
                                    cluster_estimates$cluster_raw)
  )
  return(cluster_deviations)
}

make_fit_figure <- function(fitted_x_lines, cluster_estimates, simdata, ...) {
  fit_with_clusters <- fitted_x_lines$cluster_rand_x |>
    ggplot(aes(x = x)) +
    geom_point(data = simdata, aes(y = y), alpha = 0.2) +
    geom_ribbon(aes(y = estimate, ymin = estimate-se, ymax = estimate + se), alpha = 0.25) +
    geom_line(aes(y = estimate)) +
    geom_point(data = cluster_estimates$cluster_rand_x,aes(y = estimate)) +
    geom_errorbar(data = cluster_estimates$cluster_rand_x, aes(y = estimate, ymin = estimate-se, ymax = estimate + se))

  return(fit_with_clusters)
}

make_fit_vs_naive <- function(fitted_x_lines, fitted_x_lines_tocluster, cluster_estimates, mod_pal, ...) {
  linedf <- bind_rows(fitted_x_lines$cluster_rand_x,
                      fitted_x_lines_tocluster$cluster_raw,
                      fitted_x_lines$no_cluster)

  pointdf <- bind_rows(cluster_estimates$cluster_rand_x,
                       cluster_estimates$cluster_raw)

  method_comparison <- ggplot(linedf,
                              aes(x = x, y = estimate, ymin = estimate-se, ymax = estimate + se,
                                  color = model, linetype = model)) +
    geom_ribbon(data = linedf |> filter(model == 'cluster_rand_x'),
                alpha = 0.25, linetype = 0) +
    geom_line() +
    geom_point(data = pointdf, position = position_dodge(width = 0.1)) +
    geom_errorbar(data = pointdf, position = position_dodge(width = 0.1)) +
    # geom_text(data = pointdf |> filter(model == 'cluster_rand_x'), aes(label = cluster), nudge_x = 0.2) +
    scale_color_manual(values = mod_pal)

  return(method_comparison)
}

make_shrinkage <- function (cluster_deviations, mod_pal) {
  shrink <- ggplot(bind_rows(cluster_deviations),
                   aes(x = x, y = cluster_resid,
                       ymin = cluster_resid-se,
                       ymax = cluster_resid + se,
                       color = model)) +
    geom_point(position = position_dodge(width = 0.1)) +
    geom_linerange(position = position_dodge(width = 0.1)) +
    geom_hline(yintercept = 0) +
    scale_color_manual(values = mod_pal)

  return(shrink)
}

compare_cluster_estimates <- function(cluster_estimates, cluster_predictions, mod_pal, ...) {
  cluster_compare <-
    bind_rows(cluster_estimates$cluster_fixed,
              cluster_estimates$cluster_rand_x,
              cluster_estimates$cluster_rand,
              cluster_predictions$no_cluster,
              cluster_estimates$cluster_raw) |>
    ggplot(aes(y = estimate, x = x, color = model, ymin = estimate-se, ymax = estimate+se)) +
    geom_pointrange(position = position_dodge(width = 0.3)) +
    scale_color_manual(values = mod_pal)

  return(cluster_compare)
}

compare_cluster_to_full <- function(fitted_x_lines, fitted_x_lines_tocluster, cluster_estimates, mod_pal, ...) {
  fit_data_v_clusters <- bind_rows(fitted_x_lines$cluster_rand_x |> mutate(fittype = 'full'),
                                   fitted_x_lines_tocluster$cluster_rand_x |> mutate(fittype = 'cluster_ests'),
                                   fitted_x_lines_tocluster$cluster_fixed |> mutate(fittype = 'cluster_ests'),
                                   fitted_x_lines$no_cluster |> mutate(fittype = 'full'),
                                   fitted_x_lines_tocluster$cluster_raw |> mutate(fittype = 'cluster_ests')) |>
    ggplot(aes(x = x, y = estimate, color = model)) +
    geom_line(aes(linetype = fittype)) +
    # facet_wrap('model_group') +
    geom_point(data = bind_rows(cluster_estimates$cluster_rand_x,
                                cluster_estimates$cluster_fixed,
                                cluster_estimates$cluster_raw),
               position = position_dodge(width = 0.05)) +
    geom_errorbar(data = bind_rows(cluster_estimates$cluster_rand_x,
                                   cluster_estimates$cluster_fixed,
                                   cluster_estimates$cluster_raw),
                  aes(ymin = estimate-se, ymax = estimate + se),
                  position = position_dodge(width = 0.05)) +
    scale_color_manual(values = mod_pal)

  return(fit_data_v_clusters)
}

compare_cluster_est_to_pred <- function(cluster_estimates, cluster_predictions, ...) {
  compare_cluster_estimate_to_prediction <-
    bind_rows(cluster_estimates$cluster_fixed, cluster_predictions$cluster_fixed,
              cluster_estimates$cluster_fixed_x, cluster_predictions$cluster_fixed_x,
              cluster_estimates$cluster_rand_x, cluster_predictions$cluster_rand_x,
              cluster_estimates$cluster_rand, cluster_predictions$cluster_rand) |>
    ggplot(aes(y = estimate, x = x, color = estimate_type, ymin = estimate-se, ymax = estimate+se)) +
    geom_pointrange(position = position_dodge(width = 0.1)) +
    facet_wrap('model')

  return(compare_cluster_estimate_to_prediction)
}

compare_effect_of_x <- function(cluster_estimates, cluster_predictions, ...) {
  compare_cluster_ests_with_out_x <-
    bind_rows(cluster_estimates$cluster_fixed, cluster_predictions$cluster_fixed,
              cluster_estimates$cluster_fixed_x, cluster_predictions$cluster_fixed_x,
              cluster_estimates$cluster_rand_x, cluster_predictions$cluster_rand_x,
              cluster_estimates$cluster_rand, cluster_predictions$cluster_rand) |>
    mutate(hasx = grepl('_x$', model),
           clusterrand = grepl('_rand', model)) |>
    ggplot(aes(y = estimate, x = x, color = hasx, ymin = estimate-se, ymax = estimate+se)) +
    geom_pointrange(position = position_dodge(width = 0.1)) +
    facet_grid(estimate_type ~ clusterrand, labeller = 'label_both')

  return(compare_cluster_ests_with_out_x)
}

compare_sep_to_fixed <- function(cluster_estimates, cluster_predictions, ...) {
  sep_fixed <-
    bind_rows(cluster_estimates$cluster_fixed,
              cluster_estimates$cluster_fixed_x,
              cluster_predictions$cluster_fixed,
              cluster_predictions$cluster_fixed_x,
              # so I can include on both facets
              cluster_estimates$cluster_raw |> mutate(estimate_type = 'cluster_term'),
              cluster_estimates$cluster_raw |> mutate(estimate_type = 'full_model_predict')) |>

    ggplot(aes(y = estimate, x = x, color = model, ymin = estimate-se, ymax = estimate+se)) +
    geom_pointrange(position = position_dodge(width = 0.3)) +
    facet_wrap('estimate_type') +
    scale_color_manual(values = mod_pal)

  return(sep_fixed)
}

make_current_plot <- function(fitted_x_lines, cluster_estimates, ...) {
  current_method <- fitted_x_lines$cluster_rand_x |>
    ggplot(aes(x = x, y = estimate, ymin = estimate-se, ymax = estimate + se)) +
    geom_ribbon(alpha = 0.25) +
    geom_line() +
    geom_point(data = cluster_estimates$cluster_fixed) +
    geom_errorbar(data = cluster_estimates$cluster_fixed)
  return(current_method)
}

make_full_method_comparison <- function(fitted_x_lines, fitted_x_lines_tocluster, cluster_estimates, mod_pal, ...) {
  linedf <- bind_rows(fitted_x_lines$cluster_rand_x,
                      fitted_x_lines_tocluster$cluster_raw,
                      fitted_x_lines$no_cluster,
                      fitted_x_lines_tocluster$cluster_fixed)

  pointdf <- bind_rows(cluster_estimates$cluster_rand_x,
                       cluster_estimates$cluster_raw,
                       cluster_estimates$cluster_fixed)

  method_comparison_all <- ggplot(linedf,
                                  aes(x = x, y = estimate, ymin = estimate-se, ymax = estimate + se,
                                      color = model, linetype = model)) +
    geom_ribbon(data = linedf |> filter(model == 'cluster_rand_x'),
                alpha = 0.25, linetype = 0) +
    geom_line() +
    geom_point(data = pointdf, position = position_dodge(width = 0.05)) +
    geom_errorbar(data = pointdf, position = position_dodge(width = 0.05)) +
    geom_text(data = pointdf |> filter(model == 'cluster_rand_x'), aes(label = cluster), nudge_x = 0.2) +
    scale_color_manual(values = mod_pal)

  return(method_comparison_all)
}

make_analysed_tibble <- function(params, mod_pal) {
  inout_data <- params |>
              purrr::pmap(simulate_gaussian_mm) |>
              tibble(simdata = _) |>
              bind_cols(params)

  # Get the full models
  inout_data <- inout_data |>
    mutate(full_models = map(simdata, \(x) make_fullmodels(x)))

  # Get the terms estimated by the models
  inout_data <- inout_data |>
    mutate(term_estimates = map(full_models, \(x) extract_terms(x)))

  # Get the cluster estimates as directly as possible
  inout_data <- inout_data |>
    mutate(cluster_estimates = map2(full_models, simdata,
                                    \(x,y) estimate_clusters(x, y)))

  # Get the predictions at each cluster and corresponding x
  inout_data <- inout_data |>
    mutate(cluster_predictions = map2(full_models, simdata,
                                      \(x,y) predict_clusters(x, y)))

  # Get the lines through the cluster estimates- what our eyes will try to draw if we plot the cluster points
  inout_data <- inout_data |>
    mutate(cluster_models = map(cluster_estimates,
                                \(x) model_from_clusters(x)))

  # Fit the actual model fits for the x range for the full models and the fits through cluster estimate models
  # the warnings here are important but are about the estimation; nothing is broken with the code here.
  inout_data <- inout_data |>
    mutate(fitted_x_lines = map(full_models,
                                \(x) fit_x_full(x, xvals = xdata)),
           fitted_x_lines_tocluster = map(cluster_models,
                                          \(x) fit_x_cluster(x, xvals = xdata)))

  # Get the cluster residuals so we can see shrinkage.
  inout_data <- inout_data |>
    mutate(cluster_deviations = pmap(inout_data, get_cluster_residuals))

  ## FIGURES

  # Need to add the palette if I want one
  inout_data <- inout_data |>
    mutate(mod_pal = list(mod_pal))


  # THe main fig we actually want
  inout_data <- inout_data |>
    mutate(fit_with_clusters = pmap(inout_data, make_fit_figure))

  # The most relevant diagnostic- the fit of and cluster estimates compared to a naive fit without clusters and direct estimation of cluster values
  inout_data <- inout_data |>
    mutate(method_comparison = pmap(inout_data, make_fit_vs_naive))

  # Shrinkage
  inout_data <- inout_data |>
    mutate(shrink = map2(cluster_deviations, mod_pal,
                         \(x,y) make_shrinkage(x, y)))

  # These are more for testing and reference

  # more complete cluster estimate comparison
  inout_data <- inout_data |>
    mutate(cluster_compare = pmap(inout_data, compare_cluster_estimates))

  # compare the to-cluster-estimate fits to teh full fits
  inout_data <- inout_data |>
    mutate(fit_data_v_clusters = pmap(inout_data, compare_cluster_to_full))

  # Now we're gettng fairly esoteric

  # Compare the estimates for the cluster with what we get from 'predict' on the models
  # and the impact of including x on cluster estimates.
  inout_data <- inout_data |>
    mutate(cluster_est_to_pred = pmap(inout_data, compare_cluster_est_to_pred)) |>
    mutate(cluster_x_effect = pmap(inout_data, compare_effect_of_x)) |>
    mutate(cluster_sep_v_fixed = pmap(inout_data, compare_sep_to_fixed))

  # these will help us see what's going on with our current method, but might need to be updated once we have nesting.
  inout_data <- inout_data |>
    mutate(current_method = pmap(inout_data, make_current_plot)) |>
    mutate(method_comparison_all = pmap(inout_data, make_full_method_comparison))


  return(inout_data)

}



# Extract the DATA to a list ----------------------------------------------

extract_unnest <- function(data, paramdf) {

  paramvals <- names(paramdf)[names(paramdf) != 'cluster_x']

  fitlines <- data |>
    select({{paramvals}}, fitted_x_lines) |>
    unnest(cols = fitted_x_lines) |>
    unnest(cols = fitted_x_lines)

  fitclusters <- data |>
    select({{paramvals}}, cluster_estimates) |>
    unnest(cols = cluster_estimates) |>
    unnest(cols = cluster_estimates)

  fitlinesc <- data |>
    select({{paramvals}}, fitted_x_lines_tocluster) |>
    unnest(cols = fitted_x_lines_tocluster) |>
    unnest(cols = fitted_x_lines_tocluster)

  shrink <- data |>
    select({{paramvals}}, cluster_deviations) |>
    unnest(cols = cluster_deviations) |>
    unnest(cols = cluster_deviations)

  points <- data |>
    select({{paramvals}}, simdata) |>
    unnest(cols = simdata)

  set_v_est <- data |>
    select(intercept, slope, obs_sigma, sd_rand_intercept, term_estimates) |>
    mutate(group = row_number()) |>
    pivot_longer(cols = c(intercept, slope, obs_sigma, sd_rand_intercept)) |>
    unnest(term_estimates) |> unnest(term_estimates) |>
    filter(model == 'cluster_rand_x' & term == name) |>
    select(-name, -model, set_value = value) |>
    mutate(term = factor(term, levels = c('intercept', 'slope', 'sd_rand_intercept', 'obs_sigma')),
           type = case_when(term %in% c('intercept', 'slope') ~ 'fixed',
                            term == 'sd_rand_intercept' ~ 'random',
                            term == 'obs_sigma' ~ 'residual'))

  groupattach <- data |>
    select({{paramvals}}) |>
    mutate(group = row_number())

  set_v_est <- set_v_est |>
    left_join(groupattach)

  return(tibble::lst(fitlines, fitclusters, fitlinesc, shrink, points, set_v_est))
}
