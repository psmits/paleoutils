#' Bionomial name maker
#'
#' Convenience function to make a bionomial name from seperate
#' genus and species information.
#'
#' @param genus string genus name
#' @param spec string species name
#' @param sep seperator between genus and species text
#' @return
#' @export
#' @author Peter D Smits psmits@uchicago.edu
#' @examples
binom.make <- function(genus, species, sep = ' ') {
  paste(genus, species, sep = sep)
}
