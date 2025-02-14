#' Function cleanup to allow passing character, bare names, and list-formatted functions
#'
#' @param funs a function to evaluate inside something else, could be character, bare name, or list
#' @param forcenames force-name the functions. Often used if the names don't auto-populate correctly
#'
#' @return a named list of functions
#' @export
#'
functionlister <- function(funs, forcenames = NULL) {

  # if this is nested, there's no good way to get names for the cases with bare
  # functions, so allow forcing.
  if (is.null(forcenames)) {
    funnames <- as.character(substitute(funs))
    # if bare names are c(name, name), the c gets included, so cut it
    if(funnames[1] == "c") {funnames <- funnames[2:length(funnames)]}
  } else {
    funnames <- forcenames
  }

  # I could parse character-wrapped lists here, e.g. I'm not sure if this is too
  # much complication, and we should just let the parsing in general_aggregate
  # handle the characters or not.
  if (length(funs) == 1 && is.character(funs) && grepl('^list', funs)) {
    funs <- rlang::eval_tidy(rlang::parse_expr(funs))
  }


  # the list specification of ~ functions
  if (is.list(funs)) {
    funlist <- funs
    # also catches c(barename, barename2), so deal with that.
    if (is.function(funs[[1]])) {
      names(funlist) <- funnames
    }

  } else if (is.character(funs)) {
    funlist <- try(mget(funs, inherits = TRUE), silent = TRUE)
    # Handle the situation of nothing to mget- try just returning the character and hope it works in an `eval` in `general_aggregate`
    if (inherits(funlist, 'try-error')) {
      funlist <- funs
    }
  } else if (is.function(funs)) {
    funlist <- list(funs)
    names(funlist) <- funnames
  } else {
    rlang::abort("funs is of unsupported type (bare names, character, or list")
  }



  return(funlist)
}
