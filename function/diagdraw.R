diagdraw <- function(x) { 
  z <- 1:length(x); thetap <- result$effects$p * result$effects$theta
  
  for(i in 1:length(z)) { dimnames(x[[i]]) <-
    list(seq(0,(nrow(x[[i]])-1),1),seq(0,(nrow(x[[i]])-1),1)) }; rm(i)
  tplist <- rep(list(diag(thetap,nrow(x[[1]]),nrow(x[[1]]))),length(z)) 
  y <- lapply(z, function(z) x[[z]] + tplist[[z]] * diag(x[[z]])/sum(diag(x[[z]])))
  lapply(z, function(z) y[[z]]/sum(y[[z]]))
  }