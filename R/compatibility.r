#' Compatibility
#'
#' Calculate the compatibility between any two unordered characters. 
#' Compatibility is a tree-free method of determining the plausibility 
#' of of two characters having evolved with homoplasy. This function does this 
#' as a hybrid of various methods devised in the 80s to account for any 
#' number of character states for unordered characters.
#'
#' @param ch1 either a vector of scored states for a character or a two column 
#' matrix representing two characters
#' @param ch2 a vector of scored states for a character. optional if ch1 is a 
#' two column matrix.
#' @param st1 number of states in the first character (optional)
#' @param st2 number of states in the second character (optional)
#' @param t1 type of the first character (0 = unordered/binary, 1 = ordered) 
#' DOES NOTHING. DO NOT CHANGE
#' @param t2 type of the second character
#' @param UNKNOWN value of unknown character state scores
#' @param INAP value of inapplicable character state scores
#' @keywords
#' @export
#' @examples
Compatibility <- function(ch1, ch2, 
                          st1, st2, 
                          t1 = 0, t2 = 0, 
                          UNKNOWN = -11, INAP = -22) {
  #  Returns:
  #    value of 0 or 1
  #    0 is incompatible
  #    1 is compatible
  
  # make sure the input is in the right format
  # weirdness?
  if (!missing(ch2) && identical(length(dim(ch1)), 1)) {
    stop("ch2 exists but ch1 is not a 1-d vector! one or the other please")
  }
  # ch1 is a pair?
  if (missing(ch2) && identical(dim(ch1)[2], 2)) {
    charpair <- ch1
  } else if (missing(ch2) && !identical(dim(ch1)[2], 2)) {
    stop("char pair is not of the right shape. 2 column matrix please.")
  }
  # ch1 and ch2 both exist? are they the same length?
  if (!missing(ch2) && identical(length(ch1), length(ch2))) {
    charpair <- cbind(ch1, ch2)
  } else if (!missing(ch2) && !identical(length(ch1), length(ch2))) {
    stop("ch1 and ch2 are not of equal length")
  }
  
  # handle states
  if (missing(st1)) st1 <- max(charpair[, 1]) + 1
  if (missing(st2)) st2 <- max(charpair[, 2]) + 1
  
  # let's start executing useful code
  if (any(charpair == UNKNOWN | charpair == INAP)) {  
    rmz <- which(charpair == UNKNOWN | charpair == INAP, arr.ind = T)[, 1]
    charpair <- charpair[-rmz, ]
  }
  
  all.pairs <- unique(charpair)
  
  if (st1 <= 2 && st2 <= 2) {
    comp <- ifelse(nrow(all.pairs) > 3, 0, 1)
  } else if (st1 == 1 || st2 == 1) {
    comp <- 1
  } else {  # multistate

    # all state pair combinations
    sp1 <- combn(st1, 2) - 1
    sp2 <- combn(st2, 2) - 1
    
    # if ordered multistate
    if (t1 == 1) {
      sp1 <- sp1[, sp1[1, ] == sp1[2, ] - 1]
    }
    if (t2 == 1) {
      sp2 <- sp2[, sp2[1, ] == sp2[2, ] - 1]
    }
    
    pai <- c() # one of the few times you need to intialize
    swing <- matrix(, ncol = 2)
    for (ii in seq(ncol(sp1))) {
      for (jj in seq(ncol(sp2))) {
        c1 <- which(all.pairs[, 1] == sp1[1, ii] | 
                    all.pairs[, 1] == sp1[2, ii], 
                    arr.ind = T)
        c2 <- which(all.pairs[, 2] == sp2[1, jj] | 
                    all.pairs[, 2] == sp2[2, jj], 
                    arr.ind = T)
        pa <- all.pairs[intersect(c1, c2), ]
        p <- nrow(as.matrix(pa))
        n <- ifelse(p > 3, 0, 1)
        pai <- c(pai, n)
        if (any(c(t1, t2) == 0) & p == 3) {
          u1 <- sp1[, ii]
          u2 <- sp2[, jj]
          swing <- na.omit(rbind(swing, FindSwing(pa, u1, u2)))
        }
      }
    }
    
    circuit <- FALSE
    if (any(c(t1, t2) == 0) & nrow(swing) > 3) {
      swing <- unique(swing)
      state.diagram <- matrix(0, nrow = st1, ncol = st2)
      state.diagram[swing + 1] <- 1
      ci <- state.diagram
      if (any(rowSums(state.diagram) < 2) | any(colSums(state.diagram) < 2)) {
        ci <- state.diagram[rowSums(state.diagram) >= 2, 
                            colSums(state.diagram) >= 2]
      }
      circuit <- (sum(ci) == sum(state.diagram)) | 
        (sum(ci) %% 2 == 0 && sum(ci) >= 4)
    }
    
    comp <- ifelse(any(pai == 0), 0, 1)
    comp <- ifelse(circuit, 0, comp)
  }
  
  comp
}


#' Find the swing between three adjacent state pairs
#'
#' When determining compatibility between unordered multistate characters it is 
#' necessary to determine the position of the swing pair when there are three 
#' observed. This function is used internally.
#'
#' @param pa 3x2 matrix of state pairs
#' @param u1 state identities (values) of the first column
#' @param u2 state identities (values) of the second column
FindSwing <- function (pa, u1, u2) {
  
  if (any(pa[, 1] == u1[1] & pa[, 2] == u2[1]) & 
    any(pa[, 1] == u1[2] & pa[, 2] == u2[2])) {
    # heterogeneous swing
    # one of the maxes and one of the mins
    # can't be both
    p1 <- ifelse(sum(pa[, 1] == u1[1]) > sum(pa[, 1] == u1[2]), 1, 2)
    p2 <- ifelse(sum(pa[, 2] == u2[1]) > sum(pa[, 2] == u2[2]), 1, 2)
    
    swing <- pa[pa[, 1] == u1[p1] & pa[, 2] == u2[p2], ]
    
  } else {
    # homogeneous swing
    # either both of the maxes or both of the mins
    # can't be both 
    o <- list()
    o[[1]] <- pa[, 1] == max(u1) & pa[, 2] == max(u2)
    o[[2]] <- pa[, 1] == min(u1) & pa[, 2] == min(u2)
    y <- ifelse(any(o[[1]]), 1, 2)
    
    swing <- pa[o[[y]], ]
  }
  
  swing
}

