cscores <- function(x, mdata, matchview = TRUE) { 
  z <- 1:length(x)
  if(matchview == TRUE) {
    y <- lapply(z, function(z) x[[z]][1:5,1:5])
    mxtrnames <- rep(list(c(gsub(' ', '_', paste(
      gsub(' ', '', paste('H', row(y[[1]])-1)),
      gsub(' ', '', paste('A', col(y[[1]])-1)))),
      'H_UP5', 'A_UP5')), length(z))
    cslist <- lapply(z, function(z) data.frame(
      No = z,matrix(c(y[[z]], sum(x[[z]][row(x[[z]])-col(x[[z]])>=5]),
                      sum(x[[z]][col(x[[z]])-row(x[[z]])>=5])), ncol=27, 
                    dimnames = list(NULL,mxtrnames[[z]]))))
    
    csdf <- Reduce(function(x, y) merge(x, y, all = T), 
                   cslist, accumulate = F); data.frame(mdata,csdf[-1])
  } else { 
      lapply(z, function(z) x[[z]][1:11,1:11]) } 
  }