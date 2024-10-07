# Helper functions for random error and mixed models

#' Just get the mean and sd of each cluster
#'
#' @param alldata
#'
#' @return
#' @export
#'
#' @examples
naive_clusters <- function(alldata) {
  alldata |>
    summarise(estimate = mean(y),
              se = sd(y)/sqrt(n()),
              x = unique(x),
              .by = cluster) |>
    mutate(estimate_type = 'separate',
           model = 'cluster_raw')
}


#' Get full-model predictions for each cluster (and x value if present)
#'
#' @param model
#' @param alldata
#'
#' @return
#' @export
#'
#' @examples
predict_at_clusters <- function(model, alldata) {
  clusterdata <- make_clusterdata(alldata)
  mname <- deparse(substitute(model)) |>
    stringr::str_remove_all('.*\\$')

  fitcluster <- clusterdata |>
    add_predictions(model, se.fit = TRUE) |>
    rename(estimate = fit, se = se.fit) |>
    mutate(estimate_type = 'full_model_predict',
           model = mname)

  return(fitcluster)
}

#' Get model predictions across an x-range at the mean random
#' This is a bit silly with how close it is to the above, but whatever.
#'
#' @param model
#' @param alldata
#'
#' @return
#' @export
#'
#' @examples
predict_fit <- function(model, xsteps) {

  mname <- deparse(substitute(model)) |>
    stringr::str_remove_all('.*\\$')

  # If we have clusters as fixed effects, we can't get the 'pure' effect of x using `predict`, so have to hand-roll
  if (length(ranef(model)$cond) == 0 && any(grepl('cluster', attributes(terms(model))$term.labels))) {
    intercepts <- predict(model, newdata = tibble(x = 0, cluster = unique(model$frame$cluster)))
    intercept <- mean(intercepts) # should maybe be weighted?

    int_se <- summary(model)$coef$cond['(Intercept)', 'Std. Error']
    slope <- fixef(model)$cond['x']
    slope_se <- summary(model)$coef$cond['x', 'Std. Error']

    fitline <- xsteps |>
      mutate(estimate = intercept + slope*x,
             se = ifelse(x == 0, int_se, slope_se),
             model = mname)
    rlang::warn('SE for fits with fixed clusters are not correct')

  } else {
    fitline <- xsteps |>
      add_predictions(model, se.fit = TRUE, re.form = NA) |>
      rename(estimate = fit, se = se.fit) |>
      mutate(model = mname)
  }

  return(fitline)
}


#' Extract cluster coefficients from full models and adjust by slope/intercept if needed.
#' Tends to be the same as the predictions from the full model, but with different se.
#'
#' @param model
#' @param alldata
#'
#' @return
#' @export
#'
#' @examples
extract_cluster_terms <- function(model, alldata) {

  mname <- deparse(substitute(model)) |>
    stringr::str_remove_all('.*\\$')

  if (length(ranef(model)$cond) == 0) {
    clustib <- cluster_estimates_fixed(model, alldata)
  } else {
    clustib <- cluster_estimates_rand(model, alldata)
  }

  clustib <- clustib |>
    mutate(estimate_type = 'cluster_term',
           model = mname)

  return(clustib)
}

#' Extraction from re models
#'
#' @param model
#' @param alldata
#'
#' @return
#' @export
#'
#' @examples
cluster_estimates_rand <- function(model, alldata) {
  clusterdata <- make_clusterdata(alldata)
  clustib <- tibble(cluster = row.names(ranef(model)$cond$cluster),
                    intercept = fixef(model)$cond["(Intercept)"],
                    slope = fixef(model)$cond['x'],
                    cluster_deviation = ranef(model)$cond$cluster[[1]],
                    sd = sqrt(c(attributes(ranef(model)$cond$cluster)$condVar))) |>
    mutate(slope = ifelse(is.na(slope), 0, slope)) |>
    left_join(clusterdata, by = 'cluster') |>
    mutate(estimate = intercept + slope*x + cluster_deviation,
           se = sd/sqrt(n))

  # Note- checking against as.data.frame(ranef(model)), and these are the same numbers (including the condsd)

  return(dplyr::select(clustib, cluster, x, estimate, se))
}

#' Extraction from only fixed models
#'
#' @param model
#' @param alldata
#'
#' @return
#' @export
#'
#' @examples
cluster_estimates_fixed <- function(model, alldata) {
  clusterdata <- make_clusterdata(alldata)
  sumvals <- summary(model)$coefficients$cond
  # unique(clusterdata$cluster)
  row.names(sumvals) <- stringr::str_remove_all(row.names(sumvals), 'cluster')
  missingc <- setdiff(unique(clusterdata$cluster), row.names(sumvals))
  # We get the intercept elsewhere
  sumvals[row.names(sumvals) == '(Intercept)', 'Estimate'] <- 0
  row.names(sumvals)[row.names(sumvals) == '(Intercept)'] <- missingc


  clustib <- as_tibble(sumvals, rownames = 'cluster') |>
    select(cluster, estimate = Estimate, se = `Std. Error`) |>
    mutate(intercept = fixef(model)$cond['(Intercept)'],
           slope = fixef(model)$cond['x'],
           slope = ifelse(is.na(slope), 0, slope)) |>
    filter(cluster != 'x') |>
    left_join(clusterdata) |>
    mutate(estimate = intercept + estimate + slope*x)

  return(dplyr::select(clustib, cluster, x, estimate, se))

}

cluster_residuals <- function(model, alldata, cluster_ests) {

  if (length(ranef(model)$cond) == 0) {
    resids <- predict_fit(model, make_clusterdata(alldata)) |>
      rename(line_est = estimate, line_se = se) |>
      select(-model) |>
      left_join(cluster_ests) |>
      mutate(cluster_resid = estimate-line_est)
  } else {
    resids <- predict_fit(model, make_clusterdata(alldata)) |>
      # Extract these directly from ranef to make sure we're not assuming anything
      left_join(as_tibble(ranef(model)$cond$cluster, rownames = 'cluster')) |>
      rename(cluster_resid = `(Intercept)`)

      # As a check that this is correct, we get the same thing from this:
      # rename(line_est = estimate, line_se = se) |>
      # left_join(cluster_estimates$cluster_rand_x) |>
      # mutate(cluster_resid = estimate-line_est) |>
  }

  return(resids)

}

#' Extract the term estimates from glmmTMB
#'
#' @param model
#'
#' @return
#' @export
#'
#' @examples
extract_glmmTMB_ests <- function(model, mname) {

  # mname <- deparse(substitute(model)) |>
  #   stringr::str_remove_all('.*\\$')

  ms <- summary(model)

  tib <- confint(model) |>
    as_tibble(rownames = 'term') |>
    mutate(term = case_when(term == '(Intercept)' ~ 'intercept',
                            term == 'x' ~ 'slope',
                            term == 'Std.Dev.(Intercept)|cluster' ~ 'sd_rand_intercept',
                            .default = term)) |>
    rename(cil = `2.5 %`, ciu = `97.5 %`, estimate = Estimate)

  # If the model only has fixed, just use the residual sd.
  if (length(ranef(model)$cond) == 0 && any(grepl('cluster', attributes(terms(model))$term.labels))) {
    resid_sig <- tibble(term = 'obs_sigma', estimate = sd(resid(model)))
  } else {
    resid_sig <- tibble(term = 'obs_sigma', estimate = attributes(ms$varcor$cond)$sc)
  }

  tib <- bind_rows(tib, resid_sig) |>
    mutate(model = mname)

  return(tib)

}

#' Helper
#'
#' @param alldata
#'
#' @return
#' @export
#'
#' @examples
make_clusterdata <- function(alldata) {
  alldata |>
    select(x, cluster) |>
    mutate(n = n(), .by = cluster) |>
    distinct()
}

#' Add predictions to a dataframe. More passthrough than modelr::add_predictions, espcially needed for se.fit and re.form
#'
#' @param df
#' @param model
#' @param se.fit
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
add_predictions <- function(df, model, se.fit = TRUE, ...) {
  preds <- predict(object = model,
                   newdata = df,
                   se.fit = se.fit, ...) |>
    as_tibble()

  df <- bind_cols(df, preds)
  return(df)
}
