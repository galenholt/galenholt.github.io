#' Quick helper to create a quarto profile to only render the active file. Use this in most instances, not the similar [make_simple_yaml()]
#'
#' @param renderfile file to render, default 'auto' uses the active file in Rstudio
#'
#' @return
#' @export
#'
#' @examples
make_simpleyml <- function(renderfile = 'auto') {

  if (renderfile == 'auto') {
    if (rstudioapi::isAvailable()) {
      projpath <- rstudioapi::getActiveProject()
      docpath <- rstudioapi::documentPath()
      projdir <- sub(".*/([^/]+)$", "\\1", projpath)
      reldocpath <- sub(paste0(".*", projdir, "/"), "", docpath)
      renderfile <- reldocpath
    } else {
      rlang::inform("Rstudio not running, do not want new profiles created while rendering, so skipping")
      return(invisible())
    }


  }


  simple_yaml <- list()
  simple_yaml$project <- list()
  simple_yaml$project$render <- list(renderfile)
  yaml::write_yaml(simple_yaml, '_quarto-singlefile.yml')
}
