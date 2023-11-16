find_missing_pages <- function(files = NULL, pattern = "_quarto.*.yml") {

  if (is.null(files)) {
    files <- list.files(pattern = pattern)
  }

  qmd_in_proj <- list.files(pattern = "*.qmd", recursive = TRUE)

  qmd_in_yml <- vector(mode = 'character')
  for (f in files) {
    qmd_in_file <- readLines(f) |>
      purrr::keep(\(x) grepl(".qmd", x)) |>
      gsub('\\s', '', x = _) |>
      gsub('-', '', x = _) |>
      gsub('href:', '', x = _)

    qmd_in_yml <- c(qmd_in_yml, qmd_in_file)
  }

  qmd_in_yml <- unique(qmd_in_yml)

  # The values in the `render` section are still there, but that's ok

  not_present <- which(!(qmd_in_proj %in% qmd_in_yml))

  missing_pages <- qmd_in_proj[not_present]

  no_file <- which(!(qmd_in_yml %in% qmd_in_proj))

  broken_links <- qmd_in_yml[no_file]


  return(list(missing_pages = missing_pages, no_file_present = broken_links))
}
