#' Adjust zeros to make relativizing, logging, etc smoother
#'
#' @param data dataframe
#' @param adjust_col column to adjust (numeric)
#' @param amount how much to adjust, default 0. Either numeric or `'auto'`. Auto
#'   shifts by `0.1*min(abs(data[data != 0]))`. If all nonzero data is positive
#'   or negative, the zeros are shifted only up or down. Otherwise, they are
#'   shifted randomly up and down `abs(amount)`
#' @param onlyzeros logical, default `FALSE`. Shift all data or only zeros
#'   (`TRUE`). Nonzero data is shifted away from 0 in whatever direction it
#'   already is if `FALSE`
#'
#' @return dataframe with no zeros in `adjust_col`

adjust_zeros <- function(data, adjust_col, amount, onlyzeros = FALSE) {
  # handle 'auto' adjustment
  if (grepl("auto", amount)) {
    amount <- min(abs(data[[adjust_col]])[data[[adjust_col]] != 0],
      na.rm = TRUE
    ) * 0.1
  }

  # a function to do the adjust in a mutate
  adjfun <- function(x) {
    adjust_dir <- dplyr::case_when(all(x >= 0, na.rm = TRUE) ~ "pos",
      all(x <= 0, na.rm = TRUE) ~ "neg",
      .default = "both"
    )
    if (onlyzeros) {
      zeroamount <- 0
    } else {
      zeroamount <- amount
    }

    adjx <- dplyr::case_when(
      x > 0 ~ x + zeroamount,
      x < 0 ~ x - zeroamount,
      x == 0 & adjust_dir == "pos" ~ x + amount,
      x == 0 & adjust_dir == "neg" ~ x - amount,
      x == 0 & adjust_dir == "both" ~ x + sample(c(amount, -1 * amount), 1)
    )
  }

  # mutate
  data <- data |>
    dplyr::mutate(dplyr::across(tidyselect::all_of(adjust_col), adjfun))

  return(data)
}
