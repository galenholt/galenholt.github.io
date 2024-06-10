#' Vectorised version of ggsave
#'
#' Passes all arguments to [ggplot2::ggsave()] but is vectorised over `device` and `filename` should not have an extension
#'
#' @param filename As in [ggplot2::ggsave()], but without extension, and no extension guessing.
#' @param plot As in [ggplot2::ggsave()]
#' @param device As in [ggplot2::ggsave()], but must be character and can be a character vector
#' @param ... All other arguments to [ggplot2::ggsave()]
#'
#' @return invisible vector of filenames
#' @export

ggsave_multi <- function(filename, plot = last_plot(), device, ...) {

  extnames <- lapply(device, \(x) paste0(filename, '.', x))
  lapply(extnames, \(x) ggsave(
    filename = x, plot = plot,
    ...
  ))
  # similar to the ggsave return
  return(invisible(unlist(extnames)))
}
