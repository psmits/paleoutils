#' Create a bipartite graph of taxonomic occurence
#'
#'
#' @param pbdb object of class pbdb
#' @param weight logical indicating if resulting graph object is weighted
#' @keywords
#' @export
#' @author Peter D Smits <psmits@uchicago.edu>
#' @examples
library(igraph)
library(reshape2)
taxon.bipartite <- function(pbdb, weight = FALSE) {
  nn <- melt(cbind(rownames(tt$occurence), tt$occurence))

  if(!weight) {
    nn.g <- graph.data.frame(nn[, 2:1], directed = FALSE)
  } else if(weight) {
    nn.g <- graph.data.frame(nn[, c(2:1, 3)], directed = FALSE)
  }

  V(nn.g)$type <- V(nn.g)$name %in% rownames(tt$occurence)

  nn.g
}
