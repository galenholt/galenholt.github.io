#' A standard ggplot theme for HydroBOT
#'
#' @param base_size set the base_size as in other `ggplot` themes
#' @param ... other arguments to [ggplot2::theme()]
#'
#' @return a ggplot theme
#' @export
#'
#' @examples
#' ggplot2::ggplot(iris,
#'   ggplot2::aes(x = Sepal.Length, y = Sepal.Width, color = Species)) +
#'   ggplot2::geom_point() +
#'   theme_clean()
theme_clean <- function(base_size = 10, ...) {

  ggplot2::theme_bw(base_size = base_size) +
    ggplot2::theme(strip.background = ggplot2::element_blank(),
                   panel.grid = ggplot2::element_blank(),
                   ...)

}
