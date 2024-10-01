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
           model = 'separate')
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
  mname <- deparse(substitute(model))
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

  mname <- deparse(substitute(model))

  # If we have clusters as fixed effects, we can't get the 'pure' effect of x using `predict`, so have to hand-roll
  if (length(ranef(model)$cond) == 0 && any(grepl('cluster', attributes(terms(model))$term.labels))) {
    intercept <- fixef(model)$cond['(Intercept)']
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
  clusterdata <- make_clusterdata(alldata)
  mname <- deparse(substitute(model))

  if (length(ranef(model)$cond) == 0) {
    clustib <- cluster_estimates_fixed(model, clusterdata)
  } else {
    clustib <- cluster_estimates_rand(model, clusterdata)
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
                    se = sqrt(c(attributes(ranef(model)$cond$cluster)$condVar))) |>
    mutate(slope = ifelse(is.na(slope), 0, slope)) |>
    left_join(clusterdata, by = 'cluster') |>
    mutate(estimate = intercept + slope*x + cluster_deviation)

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
