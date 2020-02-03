tgoal <- function(x, mdata) { z <- 1:length(x)
tglist <- lapply(z, function (z) { data.frame(No=z,                     
                                              UP7 = sum(x[[z]][row(x[[z]]) + col(x[[z]]) > 8]),
                                              T4_6 = sum(x[[z]][row(x[[z]]) + col(x[[z]]) > 5 & 
                                                                  row(x[[z]]) + col(x[[z]]) < 9]),
                                              T2_3 = sum(x[[z]][row(x[[z]]) + col(x[[z]]) > 3 & 
                                                                  row(x[[z]]) + col(x[[z]]) < 6]),
                                              T0_1 = sum(x[[z]][row(x[[z]]) + col(x[[z]]) < 4]))})
tgdf <- Reduce(function(x, y) merge(x, y, all = T), 
               tglist, accumulate = F); data.frame(mdata,tgdf[-1]) }