handicap <- function(x, mdata, matchview = T) { z <- 1:length(x)
hmhdp1 <- lapply(z, function (z) { data.frame( No = seq(1,15,4),
                                               hmodds = c('HN3.50', 'HN2.50','HN1.50','HN0.50'),
                                               Home = t(data.frame(
                                                 HN3.50 = sum(x[[z]][row(x[[z]]) - col(x[[z]])>= 4]),
                                                 HN2.50 = sum(x[[z]][row(x[[z]]) - col(x[[z]])>= 3]),
                                                 HN1.50 = sum(x[[z]][row(x[[z]]) - col(x[[z]])>= 2]),            
                                                 HN0.50 = sum(x[[z]][row(x[[z]]) - col(x[[z]])>= 1]))),
                                               awodds = c('AP3.50', 'AP2.50','AP1.50','AP0.50'),
                                               Away = t(data.frame(
                                                 AP3.50 = sum(x[[z]][row(x[[z]]) - col(x[[z]])< 4]),
                                                 AP2.50 = sum(x[[z]][row(x[[z]]) - col(x[[z]])< 3]),
                                                 AP1.50 = sum(x[[z]][row(x[[z]]) - col(x[[z]])< 2]),            
                                                 AP0.50 = sum(x[[z]][row(x[[z]]) - col(x[[z]])< 1]))))})

awhdp1 <- lapply(z, function (z) { data.frame( No = seq(17,29,4),
                                               hmodds = c('HP0.50', 'HP1.50','HP2.50','HP3.50'),
                                               Home = t(data.frame(
                                                 HP0.50 = sum(x[[z]][col(x[[z]]) - row(x[[z]])< 1]),
                                                 HP1.50 = sum(x[[z]][col(x[[z]]) - row(x[[z]])< 2]),
                                                 HP2.50 = sum(x[[z]][col(x[[z]]) - row(x[[z]])< 3]),            
                                                 HP3.50 = sum(x[[z]][col(x[[z]]) - row(x[[z]])< 4]))),
                                               awodds = c('AN0.50', 'AN1.50','AN2.50','AN3.50'),
                                               Away = t(data.frame(
                                                 AN0.50 = sum(x[[z]][col(x[[z]]) - row(x[[z]])>= 1]),
                                                 AN1.50 = sum(x[[z]][col(x[[z]]) - row(x[[z]])>= 2]),
                                                 AN2.50 = sum(x[[z]][col(x[[z]]) - row(x[[z]])>= 3]),            
                                                 AN3.50 = sum(x[[z]][col(x[[z]]) - row(x[[z]])>= 4]))))})

hmhdp2 <- lapply(z, function (z) { data.frame( No = seq(3,15,4),
                                               hmodds = c('HN3.00', 'HN2.00','HN1.00','H0.00'),
                                               Home = t(data.frame(
                                                 HN3.00 = sum(hmhdp1[[z]]['HN3.50','Home'],hmhdp1[[z]]['HN2.50','Home'])/2,
                                                 HN2.00 = sum(hmhdp1[[z]]['HN2.50','Home'],hmhdp1[[z]]['HN1.50','Home'])/2,
                                                 HN1.00 = sum(hmhdp1[[z]]['HN1.50','Home'],hmhdp1[[z]]['HN0.50','Home'])/2,
                                                 H0.00  = sum(hmhdp1[[z]]['HN0.50','Home'],awhdp1[[z]]['HP0.50','Home'])/2)),
                                               awodds = c('AP3.00', 'AP2.00','AP1.00','A0.00'),
                                               Away = t(data.frame(
                                                 AP3.00 = sum(hmhdp1[[z]]['HN3.50','Away'],hmhdp1[[z]]['HN2.50','Away'])/2,
                                                 AP2.00 = sum(hmhdp1[[z]]['HN2.50','Away'],hmhdp1[[z]]['HN1.50','Away'])/2,
                                                 AP1.00 = sum(hmhdp1[[z]]['HN1.50','Away'],hmhdp1[[z]]['HN0.50','Away'])/2,
                                                 A0.00  = sum(hmhdp1[[z]]['HN0.50','Away'],awhdp1[[z]]['HP0.50','Away'])/2)))})

awhdp2 <- lapply(z, function (z) { data.frame( No = seq(15,29,4),
                                               hmodds = c('H0.00', 'HP1.00','HP2.00','HP3.00'),
                                               Home = t(data.frame(
                                                 H0.00  = sum(awhdp1[[z]]['HP0.50','Home'],hmhdp1[[z]]['HN0.50','Home'])/2,
                                                 HP1.00 = sum(awhdp1[[z]]['HP1.50','Home'],awhdp1[[z]]['HP0.50','Home'])/2,
                                                 HP2.00 = sum(awhdp1[[z]]['HP2.50','Home'],awhdp1[[z]]['HP1.50','Home'])/2,            
                                                 HP3.00 = sum(awhdp1[[z]]['HP3.50','Home'],awhdp1[[z]]['HP2.50','Home'])/2)),
                                               awodds = c('A0.00', 'AN1.00','AN2.00','AN3.00'),							   
                                               Away = t(data.frame(
                                                 A0.00  = sum(awhdp1[[z]]['HP0.50','Away'],hmhdp1[[z]]['HN0.50','Away'])/2,
                                                 AN1.00 = sum(awhdp1[[z]]['HP1.50','Away'],awhdp1[[z]]['HP0.50','Away'])/2,
                                                 AN2.00 = sum(awhdp1[[z]]['HP2.50','Away'],awhdp1[[z]]['HP1.50','Away'])/2,            
                                                 AN3.00 = sum(awhdp1[[z]]['HP3.50','Away'],awhdp1[[z]]['HP2.50','Away'])/2)))})

hmhdp3 <- lapply(z, function (z) { data.frame( No = seq(2,15,2),
                                               hmodds = c('HN3.25','HN2.75', 'HN2.25','HN1.75','HN1.25','HN0.75','HN0.25'),
                                               Home = t(data.frame(
                                                 HN3.25 = sum(hmhdp1[[z]]['HN3.50','Home'],hmhdp2[[z]]['HN3.00','Home'])/2,
                                                 HN2.75 = sum(hmhdp2[[z]]['HN3.00','Home'],hmhdp1[[z]]['HN2.50','Home'])/2,
                                                 HN2.25 = sum(hmhdp1[[z]]['HN2.50','Home'],hmhdp2[[z]]['HN2.00','Home'])/2,
                                                 HN1.75 = sum(hmhdp2[[z]]['HN2.00','Home'],hmhdp1[[z]]['HN1.50','Home'])/2,
                                                 HN1.25 = sum(hmhdp1[[z]]['HN1.50','Home'],hmhdp2[[z]]['HN1.00','Home'])/2,
                                                 HN0.75 = sum(hmhdp2[[z]]['HN1.00','Home'],hmhdp1[[z]]['HN0.50','Home'])/2,
                                                 HN0.25 = sum(hmhdp1[[z]]['HN0.50','Home'],hmhdp2[[z]]['H0.00','Home'])/2)),
                                               awodds = c('AP3.25', 'AP2.75','AP2.25','AP1.75','AP1.25','AP0.75','AP0.25'),
                                               Away = t(data.frame(
                                                 AP3.25 = sum(hmhdp1[[z]]['HN3.50','Away'],hmhdp2[[z]]['HN3.00','Away'])/2,
                                                 AP2.75 = sum(hmhdp2[[z]]['HN3.00','Away'],hmhdp1[[z]]['HN2.50','Away'])/2,
                                                 AP2.25 = sum(hmhdp1[[z]]['HN2.50','Away'],hmhdp2[[z]]['HN2.00','Away'])/2,
                                                 AP1.75 = sum(hmhdp2[[z]]['HN2.00','Away'],hmhdp1[[z]]['HN1.50','Away'])/2,
                                                 AP1.25 = sum(hmhdp1[[z]]['HN1.50','Away'],hmhdp2[[z]]['HN1.00','Away'])/2,
                                                 AP0.75 = sum(hmhdp2[[z]]['HN1.00','Away'],hmhdp1[[z]]['HN0.50','Away'])/2,
                                                 AP0.25 = sum(hmhdp1[[z]]['HN0.50','Away'],hmhdp2[[z]]['H0.00','Away'])/2)))})

awhdp3 <- lapply(z, function (z) { data.frame( No = seq(16,28,2),
                                               hmodds = c('HP0.25','HP0.75', 'HP1.25','HP1.75','HP2.25','HP2.75','HP3.25'),
                                               Home = t(data.frame(
                                                 HP0.25 = sum(awhdp1[[z]]['HP0.50','Home'],awhdp2[[z]]['H0.00','Home'])/2,
                                                 HP0.75 = sum(awhdp2[[z]]['HP1.00','Home'],awhdp1[[z]]['HP0.50','Home'])/2,
                                                 HP1.25 = sum(awhdp1[[z]]['HP1.50','Home'],awhdp2[[z]]['HP1.00','Home'])/2,
                                                 HP1.75 = sum(awhdp2[[z]]['HP2.00','Home'],awhdp1[[z]]['HP1.50','Home'])/2,
                                                 HP2.25 = sum(awhdp1[[z]]['HP2.50','Home'],awhdp2[[z]]['HP2.00','Home'])/2,
                                                 HP2.75 = sum(awhdp2[[z]]['HP3.00','Home'],awhdp1[[z]]['HP2.50','Home'])/2,
                                                 HP3.25 = sum(awhdp1[[z]]['HP3.50','Home'],awhdp2[[z]]['HP3.00','Home'])/2)),
                                               awodds = c('AN0.25', 'AN0.75','AN1.25','AN1.75','AN2.25','AN2.75','AN3.25'),
                                               Away = t(data.frame(
                                                 AN0.25 = sum(awhdp1[[z]]['HP0.50','Away'],awhdp2[[z]]['H0.00','Away'])/2,
                                                 AN0.75 = sum(awhdp2[[z]]['HP1.00','Away'],awhdp1[[z]]['HP0.50','Away'])/2,
                                                 AN1.25 = sum(awhdp1[[z]]['HP1.50','Away'],awhdp2[[z]]['HP1.00','Away'])/2,
                                                 AN1.75 = sum(awhdp2[[z]]['HP2.00','Away'],awhdp1[[z]]['HP1.50','Away'])/2,
                                                 AN2.25 = sum(awhdp1[[z]]['HP2.50','Away'],awhdp2[[z]]['HP2.00','Away'])/2,
                                                 AN2.75 = sum(awhdp2[[z]]['HP3.00','Away'],awhdp1[[z]]['HP2.50','Away'])/2,
                                                 AN3.25 = sum(awhdp1[[z]]['HP3.50','Away'],awhdp2[[z]]['HP3.00','Away'])/2)))})

a1 <- lapply(z, function(z) merge(hmhdp2[[z]],hmhdp3[[z]],all=T))
a2 <- lapply(z, function(z) merge(awhdp2[[z]],awhdp3[[z]],all=T))
a3 <- lapply(z, function(z) merge(hmhdp1[[z]],awhdp1[[z]],all=T))
a4 <- lapply(z, function(z) merge(a1[[z]],a2[[z]],all=T))
asnhdp <- lapply(z, function(z) merge(a3[[z]],a4[[z]],all=T))
asnhdp <- lapply(z, function(z) asnhdp[[z]][order(asnhdp[[z]]$No),])
asnhdp <- lapply(z, function(z) asnhdp[[z]][-1])
rm(hmhdp1, hmhdp2, hmhdp3, awhdp1, awhdp2, awhdp3, a1, a2, a3, a4)

if(matchview == T) { 
  fnames <- function(x, mdata) { z <- 1:length(x)
  y <- lapply(z, function(z) data.frame(t(data.frame(
    t(x[[z]][1:2]),t(x[[z]][3:4])))))
  rnames <- rep(list(as.character(factor(y[[1]]$hmodds))),length(z))
  ahlist <- lapply(z, function(z) { data.frame(No = z,
                                               t(structure(y[[z]], row.names=rnames[[z]])[-1])) })
  ahdf <- Reduce(function(x, y) merge(x, y, all = T), 
                 ahlist, accumulate = F); data.frame(mdata,ahdf[-1]) }
  fnames(asnhdp,mdata)
} else {
  snames <- function(x) { z <- 1:length(x) 
  x <- lapply(z, function(z) { x <- data.frame(
    Odds = t(t(c(gsub('H', '-', substr(x[[z]][substring(
      x[[z]]$hmodds, 2) == 'N',]$hmodds, 1, 5)),0 , gsub('H', '',
                                                         substr(x[[z]][substring(x[[z]]$hmodds,2) == 'P',]$hmodds, 1, 5)
      )))),x[[z]][-c(1,3)])}); x }
  snames(asnhdp) } }