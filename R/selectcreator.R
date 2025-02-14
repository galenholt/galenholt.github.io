#' Helper to handle different ways we might choose columns
#'
#' Tidyselect has a robust way of selecting columns, but we often need to do
#' that selection indirectly- ie pass the selecting in as an argument to an
#' outer function. There are a number of ways to do this, but this is reasonably
#' robust and general, relying on using `tidyselect` itself. Allows passing
#' character vectors, bare names, or tidyselect syntax, provided this function
#' is called at the right place in the call stack
#'
#' @param selectvals the selection of columns. Can be character, bare names, or
#'   `tidyselect` syntax, e.g. `tidyselect::starts_with()`. If `tidyselect`, it can include
#'   objects, but they need to be `!!`. For example, if `prefix = 'ABC'`, to
#'   select columns starting with that prefix, should call `selectvals =
#'   tidyselect::starts_with(!!prefix)`
#' @param data a tibble or dataframe to find the columns
#' @param failmissing logical, default `TRUE`: fail if the requested grouping or
#'   aggregation columns not exist. If `FALSE`, proceed with those that do exist
#'   and silently drop those that don't
#' @return a character vector of column names. The intention is to use them in
#'   `dplyr::group_by` or `select` with `dplyr::select(dplyr::across({{output}}))`. The underlying
#'   `eval_select` returns a named integer vector giving column indices, but we
#'   return only the names because in use the indices may not be stable
#'   throughout the calling function(s)
#' @export
#'
selectcreator <- function(selectvals, data, failmissing = TRUE) {

  # Simple if they're characters
  if (is.character(selectvals)) {
    if (failmissing) {
      s1g <- rlang::expr(tidyselect::all_of(selectvals))
    } else {
      s1g <- rlang::expr(tidyselect::any_of(selectvals))
    }

  }

  # Deal with quosures and tidyselect
  if (is.language(selectvals)) {

    # If it directly evaluates as a character vector, just get it and wrap as above
    if (is.character(rlang::get_expr(selectvals))) {
      charvec <- rlang::get_expr(selectvals)
      if (failmissing) {
        s1g <- rlang::expr(tidyselect::all_of(charvec))
      } else {
        s1g <- rlang::expr(tidyselect::any_of(charvec))
      }
    }

    # If it's not a character, it *could* still evaluate to one, but it could
    # also evaluate to tidyselect, and we can't treat those the same.
    if (!is.character(rlang::get_expr(selectvals))) {
      # I can't figure out a way to ask what it evaluates to without evaluating
      # it and potentially triggering an error if it's tidyselect.

      # So, set it to s1g, in case it's tidyselect, but also do a trycatch to
      # find the character version and overwrite s1g if so. Done in that order
      # to avoid interrupted promises from evaluating the tidy
      s1g <- selectvals

      # touching it again breaks the quosures, so get s1g here, I guess, and overwrite later if needed.
      # s1g <- s1g |>
      #   tidyselect::eval_select(data, strict = failmissing) |>
      #   names()

      charvec <- tryCatch(rlang::eval_tidy(selectvals), error = function(c) FALSE)

      # if it's a character, we have it now, process as above.
      if (is.character(charvec)) {
        if (failmissing) {
          s1g <- rlang::expr(tidyselect::all_of(charvec))
        } else {
          s1g <- rlang::expr(tidyselect::any_of(charvec))
        }
      }
    }

  }

  # a secondary check in case s1g evals to just a character vector. This
  # typically only happens if a character vector is passed in but wrapped in
  # rlang::enquo, since the first conditional wraps incoming bare character
  # vectors in tidyselect already
  if (is.character(rlang::get_expr(s1g))) {
    charvec <- rlang::get_expr(s1g)
    if (failmissing) {
      s1g <- rlang::expr(tidyselect::all_of(charvec))
    } else {
      s1g <- rlang::expr(tidyselect::any_of(charvec))
    }
  }

  # Returning names instead of the indices is safer in case cols get reshuffled
  # at some point as of R 4.2, this throws a warning when s1g evals to a
  # character vector, and needs tidyselect::all_of( or tidyselect::any_of(
  # wrapping- see secondary conditional above

  # The `suppressWarnings` here is because I get an interrupted promise
  # evaluatation message if I ever touch selectvals or s1g after I set them to
  # each other for the tidyselect case. I could probably work around it with a
  # different order of operations, but the check whether it evals to character
  # is necessary and does it.
  suppressWarnings(s1g <- s1g |>
                     tidyselect::eval_select(data, strict = failmissing) |>
                     names())

  return(s1g)
}

