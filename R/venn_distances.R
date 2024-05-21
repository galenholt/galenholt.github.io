#' Find area of a circle-circle intersection given two radii and distance between centers
#'
#' Implementation of [https://mathworld.wolfram.com/Circle-CircleIntersection.html](https://mathworld.wolfram.com/Circle-CircleIntersection.html)
#'
#' @param d distance between centers
#' @param radii length-2 vector of the radii of the circles. Cannot use this and radius1, radius2
#' @param radius1 radius of circle 1. Cannot be used with radii
#' @param radius2 radius of circle 2. Cannot be used with radii
#'
#' @return
#' @export
#'
#' @examples
calc_a <- function(d, radii, radius1, radius2) {

  if (!missing(radii) & (!missing(radius1) | !missing(radius2))) {
    rlang::abort('either use radii or radius1, radius2')
  }

  # unlike the others, this is easier to have separate scalars
  if (!missing(radii)) {
    radius1 <- radii[1]
    radius2 <- radii[2]
  }


  A <- radius1^2*acos((d^2 + radius1^2 - radius2^2) / (2*d*radius1)) +
    radius2^2*acos((d^2 - radius1^2 + radius2^2) / (2*d*radius2)) -
    0.5*sqrt((d+radius1-radius2) * (d-radius1+radius2) * (-d+radius1+radius2) * (d+radius1+radius2))

  return(A)
}

#' Function to optimize to find distance between circle centers
#'
#' @inheritParams calc_a
#' @param area area of overlap of the circles
#'
#' @return
#' @export
#'
#' @examples
opt_d <- function(d, area, radii, radius1, radius2) {

  if (!missing(radii) & (!missing(radius1) | !missing(radius2))) {
    rlang::abort('either use radii or radius1, radius2')
  }

  if (missing(radii)) {
    radii <- c(radius1, radius2)
  }

  if (d < abs(diff(radii)) | (d > sum(radii))) {
    rlang::abort("Optimizer throws NaN when one circle is wholly contained or they are wholly separate. Use at least `lower = abs(radius1 - radius2)`.")
  }

  dcheck <- calc_a(d, radii)

  # log it to get closer than just abs?
  checker <- log(abs(dcheck-area))

  return(checker)
}


#' Find the distance between centers given radii and overlap area
#'
#' @inheritParams opt_d
#'
#' @return
#' @export
#'
#' @examples
find_d <- function(area, radii, radius1, radius2) {
  if (!missing(radii) & (!missing(radius1) | !missing(radius2))) {
    rlang::abort('either use radii or radius1, radius2')
  }

  if (missing(radii)) {
    # radii <- cbind(radius1, radius2)
    radii <- c(radius1, radius2)
  }

  # weirdly complex to work with vectors
  # bestmin <- vector(mode = 'double', length = nrow(radii))

  # for (i in 1:length(bestmin)) {
  #   bestd <- optimize(opt_d,
  #                     lower = abs(diff(radii[i, ])),
  #                     upper =  sum(radii[i, ]),
  #                     area = area[i],
  #                     radii = radii[i, ])
  #
  #   bestmin[i] <- bestd$minimum
  # }

    bestd <- optimize(opt_d,
                      lower = abs(diff(radii)),
                      upper =  sum(radii),
                      area = area,
                      radii = radii)

    bestmin <- bestd$minimum


  return(bestmin)
}
