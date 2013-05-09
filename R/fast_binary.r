#' Compatibility of two binary characters
#'
#' This is a fast and convenient function for calculating the compatibility 
#' of two binary characters with no missing state scores.
#'
#' @param cp n x 2 matrix where n is the number of taxa
#' @export
#' @examples cp <- matrix(c(0, 0, 1, 0, 1, 1), ncol = 2)
#'           comp <- fast.comp(cp)  # 1
#'
#'           ncp <- matrix(c(0, 0, 1, 1, 0, 1, 1, 0), ncol = 2)
#'           nocmp <- fast.comp(cp)  # 0
fast.comp <- function(cp) {
  comp <- ifelse(nrow(unique(cp)) > 3, 0, 1)
  comp
}
