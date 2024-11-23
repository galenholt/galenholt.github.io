#' Helper to download and unzip and throw out extras
#'
#' @param dirname
#' @param datadir
#' @param sourceurl
#' @param existing_dirs
#'
#' @return
#' @export
#'
#' @examples
zipdownload <- function(dirname, datadir, sourceurl,
                      existing_dirs = list.files(datadir)) {

  if (!(dirname %in% existing_dirs)) {
    if (!dir.exists(file.path(datadir, dirname))) {
      dir.create(file.path(datadir, dirname), recursive = TRUE)
    }
    zippath <- file.path(datadir, paste0(dirname, '.zip'))

    # Curl only tested wiht ftp, but probably works everywhere. download.file failing with ftp
    # if (grepl('ftp:', sourceurl)) {
      curl::curl_download(sourceurl, destfile = zippath)
    # }
    # download.file(sourceurl, destfile = zippath)

    unzip(zippath, exdir = file.path(datadir, dirname))

    file.remove(zippath)
  }
}
