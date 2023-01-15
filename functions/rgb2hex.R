#' Convert R, G, B columns to hex values
#'
#' @param R 
#' @param G 
#' @param B 
#' @param maxval 
#'
#' @return
#' @export
#'
#' @examples
rgb2hex <- function(R, G, B, maxval = 255) {
  rgbobj <- colorspace::RGB(R/maxval, G/maxval, B/maxval)
  hexval <- colorspace::hex(rgbobj)
  return(hexval)
}