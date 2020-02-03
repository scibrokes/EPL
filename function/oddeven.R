oddeven <- function(x, mdata) { z <- 1:length(x)
oelist <- lapply(z, function (z) { data.frame(No=z,                   
                                              Odd  = sum(x[[z]][(row(x[[z]]) + col(x[[z]])) %% 2==1]),
                                              Even = sum(x[[z]][(row(x[[z]]) + col(x[[z]])) %% 2==0]))})
oedf <- Reduce(function(x, y) merge(x, y, all = T), 
               oelist, accumulate = F); data.frame(mdata,oedf[-1]) }