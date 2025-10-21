#' Create qmds needed for re-rendering supplement to generate separate
#' bibliographies. Still needs some copy-paste, but easier than before
#'
#' @param mainfile path to the original quarto with text and supp
#' @param breaksection title to break on, e.g., 'Supplement' (doesn't need to be complete, it's regex)
#' @param ref_file the .bib file. Needs to be the full path from the project, not the file as it is in quarto yaml
#' @param cslfile as in quarto yaml (not the full path, because this gets appended into output yaml)
#' @param out_dir directory to save the output quartos
#' @param is_numeric_csl flag for whether the csl generates numeric citation styles because they're harder to manage.
#'
#' @returns TRUE, invisibly
#' @export
#'
create_multibib <- function(mainfile, breaksection, ref_file, cslfile, out_dir, is_numeric_csl = FALSE) {
  # read file
  all_lines <- readLines(mainfile)

  # which line is the supplement?
  whichsupp <- which(grepl(breaksection, all_lines))

  # Extract all the references to everything
  all_ats <- purrr::map(all_lines, \(x) stringr::str_extract_all(x, '@[^;.\\] )]+?(?=[;.\\] )])'))

  # We're going to unlist to make parsing easier, so need to split now
  main_ats <- unlist(all_ats[1:(whichsupp-1)])

  supp_ats <- unlist(all_ats[whichsupp:length(all_ats)])

  # extract available references
  refs <- readLines(ref_file)

  # Now we want to find the ids. There's likely a package that does this

  # just the lines starting with @
  refs <- refs[which(grepl('^@', refs))]
  # remove the type
  refs <- sub("^@[^\\{]*\\{", "@", refs)
  refs <- sub(',', '', refs)

  main_refs <- main_ats[main_ats %in% refs] |> unique()
  supp_refs <- supp_ats[supp_ats %in% refs] |> unique()
  # Now make two dummy quartos. I'd like to do this with an <include>. The catch
  # is, that doesn't work because it's essentially pasted in and we're right back
  # to the only one bib in a document problem.

  # Paths are a pain.
  maintext <- c('---',
                paste0('bibliography: ', sub(".*/", "", ref_file)),
                paste0('csl: ', cslfile),
                '---',
                '::: {.hidden}',
                paste(main_refs, collapse = '; '),
                ':::')

  supptext <- c('---',
                paste0('bibliography: ', sub(".*/", "", ref_file)),
                paste0('csl: ', cslfile),
                '---',
                '::: {.hidden}',
                paste(supp_refs, collapse = '; '),
                ':::')

  if (!is_numeric_csl) {
    writeLines(maintext, file.path(out_dir, 'main_refs.qmd'))
    writeLines(supptext, file.path(out_dir, 'supp_refs.qmd'))
  }

  if (is_numeric_csl) {
    # A different approach for numbered refs. The only way I can figure to get the
    # sequence right is to actually re-render the supplement, and we want all refs
    # to figs etc to work. So, this will take a lot longer, since we'll need to
    # render the whole doc.

    # We'll just strip all citations out of the top, render it otherwise, and then throw the top away.

    top_lines <- all_lines[1:(whichsupp-1)]
    supp_lines <- all_lines[whichsupp:length(all_lines)]

    top_refless <- purrr::map(top_lines, \(x) stringr::str_remove_all(x, paste0(refs, collapse = '|')))

    writeLines(unlist(c(top_refless, supp_lines)), file.path(out_dir, 'suppcitesonly.qmd'))
  }

  return(invisible(TRUE))
}
