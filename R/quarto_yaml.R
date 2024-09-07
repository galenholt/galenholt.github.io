#' Switches among several _quarto.yml. *ALMOST ALWAYS BETTER TO USE [make_simpleyml()] with a profile*
#'
#' @param yamfile
#' @param yamdir
#'
#' @return
#' @export
#'
#' @examples
use_quarto_yaml <- function(yamfile = 'project', yamdir = '_yml') {
  yamfiles <- list.files(yamdir)
  whichyam <- grepl(yamfile, yamfiles)
  if (sum(whichyam) > 1) {stop('too many matching yaml files')}

  file.copy(file.path(yamdir, yamfiles[whichyam]), '_quarto.yml', overwrite = TRUE)

  return(invisible())
}

#' Rebuilds _quarto.yml to be a single file. *ALMOST ALWAYS BETTER TO USE [make_simpleyml()] with a profile*
#'
#' @param proj_yaml_file
#' @param yamdir
#' @param simple_file
#'
#' @return
#' @export
#'
#' @examples
make_simple_yaml <- function(proj_yaml_file = NULL,
                             yamdir = getwd(),
                             simple_file = NULL) {
  # By default, assume yaml is in working directory/_quarto.yml
  if (is.null(proj_yaml_file)) {
    proj_yaml_file <- file.path(yamdir, '_quarto.yml')
  }

  proj_yaml <- yaml::read_yaml(proj_yaml_file)

  simple_yaml <- list()
  simple_yaml$project <- proj_yaml$project
  # kill the type in case it's complex (e.g. website, book)
  simple_yaml$project$type <- NULL
  # kill render options- only rendering a single doc should allow rendering that doc and no others
  simple_yaml$project$render <- NULL

  if (is.null(simple_file)) {
    simple_file <- file.path(yamdir, '_quarto_simple.yml')
  }

  yaml::write_yaml(simple_yaml, simple_file)

}

#' Sets up a main and subsidiary yaml. *ALMOST ALWAYS BETTER TO USE [make_simpleyml()] with a profile*
#'
#' @param yamdir
#' @param copy_orig
#' @param make_simple
#' @param leave_orig
#'
#' @return
#' @export
#'
#' @examples
make_multi_yaml <- function(yamdir = '_yml',
                            copy_orig = TRUE,
                            make_simple = TRUE,
                            leave_orig = TRUE) {


  if (!dir.exists(yamdir)) {dir.create(yamdir)}

  if (copy_orig) {
    file.copy('_quarto.yml', file.path(yamdir, '_quarto_project.yml'))
    if (!leave_orig) {file.remove('_quarto.yml')}
  }

  if (make_simple) {
    make_simple_yaml(file.path(yamdir, '_quarto_project.yml'), yamdir = yamdir)
  }
}
