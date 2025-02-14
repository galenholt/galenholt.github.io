#' Compare values to a baseline
#'
#' This is used to automate comparison to baselines of various sorts without needing to do all the data wrangling by hand.
#'
#' @param val_df dataframe of values
#' @param compare_col column or columns to use for grouping- needed if the baseline is one level in this column. Otherwise ignored
#' @param base_lev value for baseline. Can be a level in `compare_col`, or a scalar, or a vector of `length(unique(val_df$compare_col))`. A dataframe would be useful but not yet implemented.
#' @param values_col column containing the values. Can be character or tidyselect or bare names, though the last are fragile.
#' @param comp_fun function to use for comparison comparing the first two arguments (`difference` and `relative` included here as wrappers for `-` and `/`). Can be character, or list-format, (both safe) or bare name (brittle)
#' @param ... additional arguments to `comp_fun`
#' @param failmissing logical, default TRUE. Use `tidyselect::any_of` or `tidyselect::all_of` when `values_col` or `compare_col` are character. see [selectcreator()]
#' @param names_to character, as in [tidyr::pivot_longer()], used for auto-pivotting
#' @param values_to character, as in [tidyr::pivot_longer()], used for auto-pivotting
#' @param group_cols character, columns to group the baselining by. Often spatial or env_obj or similar.
#' @param zero_adjust numeric (default 0) or `"auto"`, adjustment to data to
#'   avoid zeros by adding `zero_adjust` to `abs(data)`, e.g shifting all data
#'   away from zero, either positively or negatively. Zeros themselves are
#'   shifted up or down randomly. Used for avoiding x/0, NaN, and Inf when
#'   relativiszing and taking logs, primarily. Auto shifts by
#'   `0.1*min(abs(data[data != 0]))`.
#' @param onlyzeros logical, default `FALSE`. Should all values be adjusted away from zero (`TRUE`) or only adjust zero values (`FALSE`)?
#'
#' @return a tibble matching `val_df` (or a long version thereof if `val_df` is wide), with an added column for the reference level (named `ref_values_col`) and a column of the compared values (named `comp_fun_values_col`)
#' @export
#'
baseline_compare <- function(val_df, compare_col, base_lev, values_col,
                             group_cols = NULL, comp_fun, ...,
                             failmissing = TRUE,
                             names_to = 'name', values_to = 'value',
                             zero_adjust = 0, onlyzeros = FALSE) {


  # need to get the values_col into characters as soon as possible
  values_col <- selectcreator(rlang::enquo(values_col), val_df, failmissing)

  # move data away from zero if desired
  val_df <- adjust_zeros(val_df, values_col, zero_adjust, onlyzeros)

  # generate the standard format with the reference
  val_df <- create_base(val_df = val_df, compare_col = compare_col,
                        base_lev = base_lev, values_col = values_col,
                        group_cols = group_cols, failmissing = failmissing,
                        names_to = names_to, values_to = values_to)

  # We have to generate valcols both in create_base and here, unfortunately.And
  # Deal with the expected change to multiple valcols
  if (length(values_col) > 1) {values_col <- values_to}

  # Not actually sure we need this again, except the edge case where values_to might be tidyselect
  valcols <- selectcreator(rlang::enquo(values_col), val_df, failmissing)


  # Defining a y in a list has to be done externally, while auto-generating y is
  # done in the `across`. There's probably a slick way to do either, (or
  # generate one version from the other). But for now, we already need the
  # conditionals for names and functionlister, so just do different mutates too

  # The new deal where lists of anonymous functions have to be quosures
  if (rlang::is_quosure(comp_fun)) {
    nameparser = paste0('{.fn}_{.col}')
    val_df <- val_df |>
      dplyr::mutate(dplyr::across({{valcols}},
                                  !!comp_fun, ...,
                                  .names = nameparser))
  }

  if (is.list(comp_fun)) {
    # make comp_fun a named list whether it comes in that way or as a character vector
    comp_fun <- functionlister(comp_fun)
    nameparser = paste0('{.fn}_{.col}')
    val_df <- val_df |>
      dplyr::mutate(dplyr::across({{valcols}},
                                  comp_fun, ...,
                                  .names = nameparser))
  }

  if (is.character(comp_fun)) {
    if (length(comp_fun) > 1) {rlang::abort('baselining with more than one function not supported.')}
    comp_fun <- functionlister(comp_fun)
    nameparser = paste0(names(comp_fun), '_{.col}')
    # this assumes a single function to solve the across dots deprecation
    # simply. We were making that assumption anyway, but this may not
    # generalise.
    val_df <- val_df |>
      dplyr::mutate(dplyr::across({{valcols}},
                                  \(x) comp_fun[[1]](x, y = .data[[stringr::str_c('ref_', valcols)]], ...),
                                  .names = nameparser))
  }

  if (is.function(comp_fun)) {
    # if comp_fun is a bare function, leave it alone but get its name
    # https://stackoverflow.com/questions/1567718/getting-a-function-name-as-a-string
    funname <- as.character(substitute(comp_fun))
    # https://cran.r-project.org/web/packages/dplyr/vignettes/programming.html
    nameparser <- paste0(funname,'_{.col}')
    val_df <- val_df |>
      dplyr::mutate(dplyr::across({{valcols}},
                                  \(x) comp_fun(x, y = .data[[stringr::str_c('ref_', valcols)]], ...),
                                  .names = nameparser))
  }


  return(val_df)

}

#' Creates a baseline column in several different ways. Helper for [baseline_compare()] but useful elsewhere too
#'
#' @inheritParams baseline_compare
#'
#' @return a tibble matching `val_df` (or a long version thereof if `val_df` is wide), with an added column for the reference level (named `ref_values_col`)
#' @export
#'
create_base <- function(val_df, compare_col, base_lev, values_col, group_cols = NULL,
                             failmissing = TRUE, names_to, values_to) {


# Create the ref_ column --------------------------------------------------
  # Should probably split this out into a separate function.

  # Get the scenario columns however they were passed
  compcols <- selectcreator(rlang::enquo(compare_col), val_df, failmissing)

  valcols <- selectcreator(rlang::enquo(values_col), val_df, failmissing)

  # save for later but allow changes to valcols itself
  orig_valcols <- valcols

  # Enforce long data for scenarios, let there by arbitrary numbers of values
  # columns. e.g. could be flow, stage height etc or aggregation 1, aggregation
  # 2, etc... It's FAR easier to write the mutate if I make these long. Will
  # just use the default "name" and "value" to keep general
  if (length(valcols) > 1) {
    rlang::warn("data with multiple values columns. Making long and will return long with column names in 'name' column")
    val_df <- val_df |> tidyr::pivot_longer({{valcols}}, names_to = names_to, values_to = values_to)
    valcols <- values_to
  }

  # Get the grouping columns- assume everything but the scene cols and values
  # cols- otherwise we'd end up duplicating values. Maybe that's OK, but needs
  # more thought and clear tests
    # including 'geometry' col because we drop it and match on polyID
  if (is.null(group_cols)) {

    # Deal with potentially missed grouping factors- are values duplicated?
    ndups <- val_df |>
      dplyr::group_by(dplyr::across(tidyselect::any_of(c(compcols, 'geometry')))) |>
      dplyr::summarise(nvals = dplyr::n())
    ndups <- max(ndups$nvals, na.rm = TRUE)

    if (ndups == 1) {groupcols <- NULL}
    if (ndups > 1) {
      nonnumnames <- names(val_df)[!purrr::map_lgl(val_df, is.numeric)]
      nonnumnames <- nonnumnames[!(nonnumnames %in% c(compcols, valcols, 'geometry'))]
      rlang::warn(glue::glue("`group_cols` argument not passed, but multiple
                             data points in {stringr::str_flatten_comma(compcols)}.
                             Trying to group by non-numeric columns {stringr::str_flatten_comma(nonnumnames)},
                             but FAR better to be explicit."))

      groupcols <- nonnumnames
    }


  } else {
    groupcols <- selectcreator(rlang::enquo(group_cols), val_df, failmissing)
  }



  # Deal with the most-common situation- baselev as a single lev in the scenario column
  if (is.character(base_lev) &&
      length(compcols) == 1 &&
      (base_lev %in% dplyr::pull(val_df, var = {{compcols}}))) {
    refdf <- dplyr::filter(val_df, .data[[compcols]] == base_lev) |>
      dplyr::rename_with(.fn = ~stringr::str_c('ref_', .),
                         .cols = tidyselect::all_of(valcols)) |>
      dplyr::select({{groupcols}}, tidyselect::starts_with('ref_')) |>
      sf::st_drop_geometry()

    # handle the special case of no groups, so a single reference
    if (nrow(refdf) == 1 & ncol(refdf) == 1) {
      val_df <- dplyr::bind_cols(val_df, refdf)
    } else {
      val_df <- dplyr::left_join(val_df, refdf, by = groupcols, relationship = "many-to-many")
    }

  }

  # The easiest thing to do is if base_lev is a scalar
  if ((length(base_lev) == 1) & (is.numeric(base_lev))) {
    val_df <- val_df |>
      dplyr::mutate(refcol = base_lev) |>
      dplyr::rename_with(.fn = ~stringr::str_c('ref_', valcols),
                         .cols = tidyselect::all_of('refcol'))
  }

  # we might also want to feed base_lev as a dataframe (e.g. as historical
  # average or something). Need a real use-case I think to build it
  if (is.data.frame(base_lev)) {
    rlang::abort("a baseline dataframe is a good idea but not yet implemented")
  }

  # and we might want to feed numeric values for each of several valcols. I
  # think mods to this to expand_grid to groupcols would end up looking like the
  # passed dataframe thing above needs to look
  if ((((length(orig_valcols) > 1) &
        (length(base_lev) == length(orig_valcols)))) &
      (is.numeric(base_lev))) {
    refdf <- tibble::tibble(name = orig_valcols, refcol = base_lev) |>
      dplyr::rename_with(.fn = ~stringr::str_c('ref_', valcols),
                         .cols = "refcol")  |>
      dplyr::rename_with(.fn = ~names_to,
                         .cols = "name") |>
      sf::st_drop_geometry()

    val_df <- val_df |>
      dplyr::left_join(refdf, by = names_to)
  }

  return(val_df)

}
