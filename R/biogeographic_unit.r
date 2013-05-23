#' Calculate BC value for network
#'
#' Biogeographic connectedness is defined as (O - N) / (LN - N)
#' O is the number of edges
#' N is the number of taxa
#' L is the number of localities
#'
#' THIS IS FROM A PAPER THAT I NEED TO ADD THE FORMAL CITATION 
#' SIDOR ET AL. PNAS 2013
#'
#' @param graph object of class igraph
#' @param l.small logical inidicating if the localities are the smaller bipartite projection
#' @keywords
#' @export
#' @author Peter D Smits <psmits@uchicago.edu>
#' @examples
bc <- function(graph, l.small = TRUE) {
  oo <- length(E(graph))
  bip <- bipartite.projection(graph)

  len <- lapply(bip, function(x) length(V(x)))
  ws <- which.min(unlist(len))
  wm <- which.max(unlist(len))
  if (l.small) {
    ll <- length(V(bip[[ws]]))
    nn <- length(V(bip[[wm]]))
  } else if (!l.small) {
    ll <- length(V(bip[[wm]]))
    nn <- length(V(bip[[ws]]))
  }

  bc <- (oo - nn) / (ll * nn - nn)

  bc
}


