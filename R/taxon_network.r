#' Create a bipartite graph of taxonomic occurence
#'
#'
#'
#'
#'
library(igraph)
library(reshape2)
taxon.bipartite <- function(pbdb, group = 'formation', weight = FALSE) {
  nn <- melt(cbind(rownames(tt$occurence), tt$occurence))

  if(!weight) {
    nn.g <- graph.data.frame(nn[, 1:2])
  } else if(weight) {
    nn.g <- graph.data.frame(nn)
  }

  V(nn.g)$type <- V(nn.g)$name %in% rownames(tt$occurence)

  nn.g
}
