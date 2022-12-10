zip_load <- function(dirname, datadir, sourceurl,  
                      existing_dirs = list.files(datadir)) {
  print(existing_dirs)
  if (!(dirname %in% existing_dirs)) {
    
    zippath <- file.path(datadir, paste0(dirname, '.zip'))
    download.file(sourceurl, destfile = zippath)
    
    unzip(zippath, exdir = file.path(datadir, dirname))
    
    file.remove(zippath)
  }
}