#' Resample biogeographic network statistic
#'
#' Process similar to that described in Sidor et al. 2013. 
#' This only performs a single resample.
#'
#' TODO add the bipartite check
#'
#' @param graph object of class igraph (bipartite)
#' @param fun function describing the biogeographic structure of the network
#' @param aa removal probability
#' @param bb addition probability
#' @param l.small logical if smaller part of bipartite projection is locality information
#' @return
#' @export
#' @keywords
#' @author Peter D Smits <psmits@uchicago.edu>
#' @references
#' @examples
biogeo.re <- function(graph, fun, aa = 0.05, bb = 0.05, l.small = TRUE) {
  bip <- bipartite.projection(graph)
  len <- lapply(bip, function(x) vcount(x))
  ws <- which.min(unlist(len))
  wm <- which.max(unlist(len))

  if(l.small) {
    taxa <- V(bip[[wm]])$name
    loc <- V(bip[[ws]])$name
  } else if(!l.small) {
    taxa <- V(bip[[ws]])$name
    loc <- V(bip[[wm]])$name
  }

  # get neighbors of each locality
  pres <- abse <- list()
  for(ii in seq(length(loc))) {
    pres[[ii]] <- neighbors(graph, loc[ii])
    abse[[ii]] <- seq(length(taxa))[-pres[[ii]]]
  }

  # for the taxa that are attached, do they stay attached?
  rms <- lapply(pres, function(x) {
                oo <- ifelse(runif(length(x)) <= aa, TRUE, FALSE)
                x[oo]})
  for(ii in seq(length(loc))) {
    ll <- which(V(graph)$name == loc[ii])
    rr <- cbind(rms[[ii]], ll)
    tr <- apply(rr, 1, function(x) E(graph, path = x))
    graph <- delete.edges(graph, tr)
  }

  # for the taxa that aren't attached, do they get attached?
  ae <- lapply(abse, function(x) {
               oo <- ifelse(runif(length(x)) <= bb, TRUE, FALSE)
               x[oo]})
  for(jj in seq(length(loc))) {
    ll <- which(V(graph)$name == loc[jj])
    uu <- cbind(ae[[jj]], ll)
    graph <- add.edges(graph, as.vector(t(uu)))
  }

  # get rid of any taxa that have no neighbors
  pp <- lapply(taxa, function(x) neighbors(graph, x))
  rn <- which(V(graph)$name %in% taxa[which(unlist(lapply(pp, length)) == 0)])
  graph <- delete.vertices(graph, rn)

  # biogeographic summary statistic
  out <- fun(graph)

  out
}

#' Bootstrap biogeographic network statistic
#'
#' Process similar to that described in Sidor et al. 2013. 
#' This produces the full bootstrap distribution
#'
#' TODO add the bipartite check
#'
#' @param graph object of class igraph (bipartite)
#' @param fun function describing the biogeographic structure of the network
#' @param aa removal probability
#' @param bb addition probability
#' @param nsim number of bootstrap replicates
#' @param l.small logical if smaller part of bipartite projection is locality information
#' @return
#' @export
#' @keywords
#' @author Peter D Smits <psmits@uchicago.edu>
#' @references
#' @examples
biogeo.boot <- function(graph, fun, aa = 0.05, bb = 0.05, nsim = 1000, l.small = TRUE){
  out <- array(dim = nsim)
  for(ii in seq(nsim)) {
    out[ii] <- biogeo.re(graph = graph, fun = fun, aa = aa, bb = bb, l.small = l.small)
  }

  out
}
