fodds <- function(x, mdata) { 
  z <- 1:length(x)
  data.frame(mdata, Win  = unlist(lapply(z, function(z) 
    sum(x[[z]][row(x[[z]]) >  col(x[[z]])]))),
    Draw = unlist(lapply(z, function(z) 
      sum(x[[z]][row(x[[z]]) == col(x[[z]])]))),
    Lose = unlist(lapply(z, function(z) 
      sum(x[[z]][row(x[[z]]) <  col(x[[z]])])))) 
  }