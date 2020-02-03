teamgoal <- function(x, mdata, matchview = T) { z <- 1:length(x)
ou1 <- lapply(z, function (z) { data.frame( No = seq(1,24,4),
                                            ovodds = gsub(' ', '', paste('O',rev(seq(0.5,5.5,1)))),
                                            Over = t(data.frame(
                                              O5.50 = sum(x[[z]][row(x[[z]]) > 7]),
                                              O4.50 = sum(x[[z]][row(x[[z]]) > 6]), 
                                              O3.50 = sum(x[[z]][row(x[[z]]) > 5]),
                                              O2.50 = sum(x[[z]][row(x[[z]]) > 4]),
                                              O1.50 = sum(x[[z]][row(x[[z]]) > 3]),
                                              O0.50 = sum(x[[z]][row(x[[z]]) > 2]))),
                                            unodds = gsub(' ', '', paste('U',rev(seq(0.5,5.5,1)))),
                                            Under = t(data.frame(
                                              U5.50 = sum(x[[z]][row(x[[z]]) <= 7]),
                                              U4.50 = sum(x[[z]][row(x[[z]]) <= 6]), 
                                              U3.50 = sum(x[[z]][row(x[[z]]) <= 5]),
                                              U2.50 = sum(x[[z]][row(x[[z]]) <= 4]),
                                              U1.50 = sum(x[[z]][row(x[[z]]) <= 3]),
                                              U0.50 = sum(x[[z]][row(x[[z]]) <= 2]))))})

ou2 <- lapply(z, function (z) { data.frame( No = seq(3,21,4),
                                            ovodds = gsub(' ', '', paste('O',rev(seq(1,5,1)))),
                                            Over = t(data.frame(
                                              O5.00 = sum(ou1[[z]]['O5.50','Over'],ou1[[z]]['O4.50','Over'])/2,
                                              O4.00 = sum(ou1[[z]]['O4.50','Over'],ou1[[z]]['O3.50','Over'])/2,
                                              O3.00 = sum(ou1[[z]]['O3.50','Over'],ou1[[z]]['O2.50','Over'])/2,
                                              O2.00 = sum(ou1[[z]]['O2.50','Over'],ou1[[z]]['O1.50','Over'])/2,
                                              O1.00 = sum(ou1[[z]]['O1.50','Over'],ou1[[z]]['O0.50','Over'])/2)),
                                            unodds = gsub(' ', '', paste('U',rev(seq(1,5,1)))),
                                            Under = t(data.frame(
                                              U5.00 = sum(ou1[[z]]['O5.50','Under'],ou1[[z]]['O4.50','Under'])/2,
                                              U4.00 = sum(ou1[[z]]['O4.50','Under'],ou1[[z]]['O3.50','Under'])/2,
                                              U3.00 = sum(ou1[[z]]['O3.50','Under'],ou1[[z]]['O2.50','Under'])/2,
                                              U2.00 = sum(ou1[[z]]['O2.50','Under'],ou1[[z]]['O1.50','Under'])/2,
                                              U1.00 = sum(ou1[[z]]['O1.50','Under'],ou1[[z]]['O0.50','Under'])/2)))})

ou3 <- lapply(z, function (z) { data.frame( No = seq(2,21,2),
                                            ovodds = gsub(' ', '', paste('O',rev(seq(0.75,5.5,0.5)))),
                                            Over = t(data.frame(
                                              O5.25 = sum(ou1[[z]]['O5.50','Over'],ou2[[z]]['O5.00','Over'])/2,
                                              O4.75 = sum(ou2[[z]]['O5.00','Over'],ou1[[z]]['O4.50','Over'])/2,
                                              O4.25 = sum(ou1[[z]]['O4.50','Over'],ou2[[z]]['O4.00','Over'])/2,
                                              O3.75 = sum(ou2[[z]]['O4.00','Over'],ou1[[z]]['O3.50','Over'])/2,
                                              O3.25 = sum(ou1[[z]]['O3.50','Over'],ou2[[z]]['O3.00','Over'])/2,
                                              O2.75 = sum(ou2[[z]]['O3.00','Over'],ou1[[z]]['O2.50','Over'])/2,
                                              O2.25 = sum(ou1[[z]]['O2.50','Over'],ou2[[z]]['O2.00','Over'])/2,
                                              O1.75 = sum(ou2[[z]]['O2.00','Over'],ou1[[z]]['O1.50','Over'])/2,
                                              O1.25 = sum(ou1[[z]]['O1.50','Over'],ou2[[z]]['O1.00','Over'])/2,
                                              O0.75 = sum(ou2[[z]]['O1.00','Over'],ou1[[z]]['O0.50','Over'])/2)),
                                            unodds = gsub(' ', '', paste('U',rev(seq(0.75,5.5,0.5)))),
                                            Under = t(data.frame(
                                              U5.25 = sum(ou1[[z]]['O5.50','Under'],ou2[[z]]['O5.00','Under'])/2,
                                              U4.75 = sum(ou2[[z]]['O5.00','Under'],ou1[[z]]['O4.50','Under'])/2,
                                              U4.25 = sum(ou1[[z]]['O4.50','Under'],ou2[[z]]['O4.00','Under'])/2,
                                              U3.75 = sum(ou2[[z]]['O4.00','Under'],ou1[[z]]['O3.50','Under'])/2,
                                              U3.25 = sum(ou1[[z]]['O3.50','Under'],ou2[[z]]['O3.00','Under'])/2,
                                              U2.75 = sum(ou2[[z]]['O3.00','Under'],ou1[[z]]['O2.50','Under'])/2,
                                              U2.25 = sum(ou1[[z]]['O2.50','Under'],ou2[[z]]['O2.00','Under'])/2,
                                              U1.75 = sum(ou2[[z]]['O2.00','Under'],ou1[[z]]['O1.50','Under'])/2,
                                              U1.25 = sum(ou1[[z]]['O1.50','Under'],ou2[[z]]['O1.00','Under'])/2,
                                              U0.75 = sum(ou2[[z]]['O1.00','Under'],ou1[[z]]['O0.50','Under'])/2)))})

a1 <- lapply(z, function(z) merge(ou2[[z]],ou3[[z]],all=T))
teamou <- lapply(z, function(z) merge(ou1[[z]],a1[[z]],all=T))
teamou <- lapply(z, function(z) teamou[[z]][order(teamou[[z]]$No),])
teamou <- lapply(z, function(z) teamou[[z]][-1])
rm(ou1, ou2, ou3, a1)

if(matchview == T) { 
  fnames <- function(x, mdata) { z <- 1:length(x)
  y <- lapply(z, function(z) data.frame(t(data.frame(
    t(x[[z]][1:2]),t(x[[z]][3:4])))))
  rnames <- rep(list(as.character(factor(y[[1]]$ovodds))),length(z))
  oulist <- lapply(z, function(z) { data.frame(No = z,
                                               t(structure(y[[z]], row.names=rnames[[z]])[-1])) })
  oudf <- Reduce(function(x, y) merge(x, y, all = T), 
                 oulist, accumulate = F); data.frame(mdata,oudf[-1]) }
  fnames(teamou,mdata)
} else {
  snames <- function(x) { z <- 1:length(x) 
  x <- lapply(z, function(z) { x <- data.frame(
    Odds = t(t(c(gsub('O', '', x[[z]]$ovodds)))),x[[z]][-c(1,3)])}); x }
  snames(teamou) } }