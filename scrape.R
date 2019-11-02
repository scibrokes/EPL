library(BBmisc)
pkg <- c('rvest','tidyverse','plyr','dplyr','stringr','xml2','XML', 'tidyr', 
         'RCurl', 'devtools', 'RSelenium', 'lubridate', 'zoo', 'magrittr', 
         'purrr', 'PerformanceAnalytics', 'openxlsx')
lib(pkg)

lnk <- 'https://188yingqiu.com/en-gb/asia'
lnk2a <- 'https://www.scorespro.com/soccer/england/premier-league/results/'
lnk2b <- 'https://www.scorespro.com/soccer/england/premier-league/fixtures/'

res <- lnk2a %>% 
  read_html %>% 
  html_table(fill=TRUE) %>% 
  bind_rows %>% 
  .[-1, c(1, 3:5)]

fix <-lnk2b %>% 
  read_html %>% 
  html_table(fill=TRUE) %>% 
  bind_rows %>% 
  .[-1, c(1, 3:5)]

res <- bind_rows(res, fix)
rm(fix)

names(res) <- c('KODate', 'Home', 'HG', 'Away')
score <- res$HG %>% 
  str_split_fixed(' - ', 3) %>% 
  .[,1:2] %>% data.frame %>% 
  mutate_if(is.factor, as.numeric)
names(score) <- c('FTHG', 'FTAG')

res <- bind_cols(res[,-3], score)
rm(score)
index <- res$KODate %>% str_replace_all('\\.', '/')
index <- substr(index, 1, 8) %>% dmy
res$KODate <- index

res %<>% mutate(Home = as.factor(Home), 
                Away = as.factor(Away))
rm(index)
res %<>% arrange(index)

## =============================================================
## https://www.scorespro.com/soccer/england/premier-league/results/

res <- readLines('fixture.txt') %>% 
  str_split('\t') %>% llply(., function(x) x %>% data.frame %>% t %>% data.frame) %>% 
  bind_rows %>% 
  .[c(1:2, 4:6)]
names(res) <- c('Round', 'KODate', 'Home', 'HG', 'Away')

score <- res$HG %>% 
  str_split_fixed(' - ', 3) %>% 
  .[,1:2] %>% data.frame

names(score) <- c('FTHG', 'FTAG')
score %<>% mutate(FTHG = as.numeric(as.character(FTHG)), 
                  FTAG = as.numeric(as.character(FTAG)))
score$FTHG <- score$FTHG
score$FTAG <- score$FTAG

res <- bind_cols(res[,-4], score)
rm(score)
index <- res$KODate %>% str_replace_all('\\.', '/')
index <- substr(index, 1, 8) %>% dmy
res$KODate <- index

res %<>% mutate(Home = as.factor(Home), 
                Away = as.factor(Away))
rm(index)

res %<>% 
  mutate(Round = na.locf(Round) %>% 
           str_replace_all('Round ', '') %>% 
           as.numeric)
res <- res[!is.na(res$Home),]
res %<>% arrange(KODate) %>% data.frame

## =========================================================

install_github('scibrokes/Rmodel')
require('Rmodel')

source('function/compileIndex2.R')
source('function/bvp.R')
source('function/compileOdds (need to review).R')

l1=FTHG~1; l2=FTAG~1; l1l2= ~c(Home,Away)+c(Away,Home); l3=~1;
data = na.omit(res); maxit=300; xi=-0.000007; fordate=NULL; fun="glm"; inflated=TRUE

result <- compileIndex2(data=res, xi = -0.000007, fordate = ymd('2019-11-03'))
res2 <- subset(res, KODate == ymd('2019-11-03'))


# =======================================================
diagdraw <- function(x) { z <- 1:length(x); thetap <- result$effects$p * result$effects$theta
for(i in 1:length(z)) { dimnames(x[[i]]) <-
  list(seq(0,(nrow(x[[i]])-1),1),seq(0,(nrow(x[[i]])-1),1)) }; rm(i)
tplist <- rep(list(diag(thetap,nrow(x[[1]]),nrow(x[[1]]))),length(z)) 
y <- lapply(z, function(z) x[[z]] + tplist[[z]] * diag(x[[z]])/sum(diag(x[[z]])))
lapply(z, function(z) y[[z]]/sum(y[[z]])) }

# --------------------------------------------------------------
fodds <- function(x, mdata) { z <- 1:length(x)
data.frame(mdata, Win  = unlist(lapply(z, function(z) 
  sum(x[[z]][row(x[[z]]) >  col(x[[z]])]))),
  Draw = unlist(lapply(z, function(z) 
    sum(x[[z]][row(x[[z]]) == col(x[[z]])]))),
  Lose = unlist(lapply(z, function(z) 
    sum(x[[z]][row(x[[z]]) <  col(x[[z]])])))) }
# --------------------------------------------------------------
cscores <- function(x, mdata, matchview = TRUE) { z <- 1:length(x)
if(matchview == TRUE) {
  y <- lapply(z, function(z) x[[z]][1:5,1:5])
  mxtrnames <- rep(list(c(gsub(' ', '_', paste(
    gsub(' ', '', paste('H', row(y[[1]])-1)),
    gsub(' ', '', paste('A', col(y[[1]])-1)))),
    'H_UP5', 'A_UP5')), length(z))
  cslist <- lapply(z, function(z) data.frame(No = z,matrix(c(y[[z]], 
                                                             sum(x[[z]][row(x[[z]])-col(x[[z]])>=5]),
                                                             sum(x[[z]][col(x[[z]])-row(x[[z]])>=5])), ncol=27, 
                                                           dimnames = list(NULL,mxtrnames[[z]]))))
  csdf <- Reduce(function(x, y) merge(x, y, all = T), 
                 cslist, accumulate = F); data.frame(mdata,csdf[-1])
} else { lapply(z, function(z) x[[z]][1:11,1:11]) } }
# --------------------------------------------------------------
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
# --------------------------------------------------------------
goalline <- function(x, mdata, matchview = T) { z <- 1:length(x)
ou1 <- lapply(z, function (z) { data.frame( No = seq(1,33,4),
                                            ovodds = gsub(' ', '', paste('O',rev(seq(0.5,8.5,1)))),
                                            Over = t(data.frame(
                                              O8.50 = sum(x[[z]][row(x[[z]]) + col(x[[z]])> 10]),
                                              O7.50 = sum(x[[z]][row(x[[z]]) + col(x[[z]])> 9]), 
                                              O6.50 = sum(x[[z]][row(x[[z]]) + col(x[[z]])> 8]),
                                              O5.50 = sum(x[[z]][row(x[[z]]) + col(x[[z]])> 7]),
                                              O4.50 = sum(x[[z]][row(x[[z]]) + col(x[[z]])> 6]), 
                                              O3.50 = sum(x[[z]][row(x[[z]]) + col(x[[z]])> 5]),
                                              O2.50 = sum(x[[z]][row(x[[z]]) + col(x[[z]])> 4]),
                                              O1.50 = sum(x[[z]][row(x[[z]]) + col(x[[z]])> 3]),
                                              O0.50 = sum(x[[z]][row(x[[z]]) + col(x[[z]])> 2]))),
                                            unodds = gsub(' ', '', paste('U',rev(seq(0.5,8.5,1)))),
                                            Under = t(data.frame(
                                              U8.50 = sum(x[[z]][row(x[[z]]) + col(x[[z]])<= 10]),
                                              U7.50 = sum(x[[z]][row(x[[z]]) + col(x[[z]])<= 9]), 
                                              U6.50 = sum(x[[z]][row(x[[z]]) + col(x[[z]])<= 8]),
                                              U5.50 = sum(x[[z]][row(x[[z]]) + col(x[[z]])<= 7]),
                                              U4.50 = sum(x[[z]][row(x[[z]]) + col(x[[z]])<= 6]), 
                                              U3.50 = sum(x[[z]][row(x[[z]]) + col(x[[z]])<= 5]),
                                              U2.50 = sum(x[[z]][row(x[[z]]) + col(x[[z]])<= 4]),
                                              U1.50 = sum(x[[z]][row(x[[z]]) + col(x[[z]])<= 3]),
                                              U0.50 = sum(x[[z]][row(x[[z]]) + col(x[[z]])<= 2]))))})

ou2 <- lapply(z, function (z) { data.frame( No = seq(3,33,4),
                                            ovodds = gsub(' ', '', paste('O',rev(seq(1,8,1)))),
                                            Over = t(data.frame(
                                              O8.00 = sum(ou1[[z]]['O8.50','Over'],ou1[[z]]['O7.50','Over'])/2,
                                              O7.00 = sum(ou1[[z]]['O7.50','Over'],ou1[[z]]['O6.50','Over'])/2,
                                              O6.00 = sum(ou1[[z]]['O6.50','Over'],ou1[[z]]['O5.50','Over'])/2,
                                              O5.00 = sum(ou1[[z]]['O5.50','Over'],ou1[[z]]['O4.50','Over'])/2,
                                              O4.00 = sum(ou1[[z]]['O4.50','Over'],ou1[[z]]['O3.50','Over'])/2,
                                              O3.00 = sum(ou1[[z]]['O3.50','Over'],ou1[[z]]['O2.50','Over'])/2,
                                              O2.00 = sum(ou1[[z]]['O2.50','Over'],ou1[[z]]['O1.50','Over'])/2,
                                              O1.00 = sum(ou1[[z]]['O1.50','Over'],ou1[[z]]['O0.50','Over'])/2)),
                                            unodds = gsub(' ', '', paste('U',rev(seq(1,8,1)))),
                                            Under = t(data.frame(
                                              U8.00 = sum(ou1[[z]]['O8.50','Under'],ou1[[z]]['O7.50','Under'])/2,
                                              U7.00 = sum(ou1[[z]]['O7.50','Under'],ou1[[z]]['O6.50','Under'])/2,
                                              U6.00 = sum(ou1[[z]]['O6.50','Under'],ou1[[z]]['O5.50','Under'])/2,
                                              U5.00 = sum(ou1[[z]]['O5.50','Under'],ou1[[z]]['O4.50','Under'])/2,
                                              U4.00 = sum(ou1[[z]]['O4.50','Under'],ou1[[z]]['O3.50','Under'])/2,
                                              U3.00 = sum(ou1[[z]]['O3.50','Under'],ou1[[z]]['O2.50','Under'])/2,
                                              U2.00 = sum(ou1[[z]]['O2.50','Under'],ou1[[z]]['O1.50','Under'])/2,
                                              U1.00 = sum(ou1[[z]]['O1.50','Under'],ou1[[z]]['O0.50','Under'])/2)))})

ou3 <- lapply(z, function (z) { data.frame( No = seq(2,33,2),
                                            ovodds = gsub(' ', '', paste('O',rev(seq(0.75,8.5,0.5)))),
                                            Over = t(data.frame(
                                              O8.25 = sum(ou1[[z]]['O8.50','Over'],ou2[[z]]['O8.00','Over'])/2,
                                              O7.75 = sum(ou2[[z]]['O8.00','Over'],ou1[[z]]['O7.50','Over'])/2,
                                              O7.25 = sum(ou1[[z]]['O7.50','Over'],ou2[[z]]['O7.00','Over'])/2,
                                              O6.75 = sum(ou2[[z]]['O7.00','Over'],ou1[[z]]['O6.50','Over'])/2,
                                              O6.25 = sum(ou1[[z]]['O6.50','Over'],ou2[[z]]['O6.00','Over'])/2,
                                              O5.75 = sum(ou2[[z]]['O6.00','Over'],ou1[[z]]['O5.50','Over'])/2,
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
                                            unodds = gsub(' ', '', paste('U',rev(seq(0.75,8.5,0.5)))),
                                            Under = t(data.frame(
                                              U8.25 = sum(ou1[[z]]['O8.50','Under'],ou2[[z]]['O8.00','Under'])/2,
                                              U7.75 = sum(ou2[[z]]['O8.00','Under'],ou1[[z]]['O7.50','Under'])/2,
                                              U7.25 = sum(ou1[[z]]['O7.50','Under'],ou2[[z]]['O7.00','Under'])/2,
                                              U6.75 = sum(ou2[[z]]['O7.00','Under'],ou1[[z]]['O6.50','Under'])/2,
                                              U6.25 = sum(ou1[[z]]['O6.50','Under'],ou2[[z]]['O6.00','Under'])/2,
                                              U5.75 = sum(ou2[[z]]['O6.00','Under'],ou1[[z]]['O5.50','Under'])/2,
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
ouhdp <- lapply(z, function(z) merge(ou1[[z]],a1[[z]],all=T))
ouhdp <- lapply(z, function(z) ouhdp[[z]][order(ouhdp[[z]]$No),])
ouhdp <- lapply(z, function(z) ouhdp[[z]][-1])
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
  fnames(ouhdp,mdata)
} else {
  snames <- function(x) { z <- 1:length(x) 
  x <- lapply(z, function(z) { x <- data.frame(
    Odds = t(t(c(gsub('O', '', x[[z]]$ovodds)))),x[[z]][-c(1,3)])}); x }
  snames(ouhdp) } }
# --------------------------------------------------------------
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
# --------------------------------------------------------------
oddeven <- function(x, mdata) { z <- 1:length(x)
oelist <- lapply(z, function (z) { data.frame(No=z,                   
                                              Odd  = sum(x[[z]][(row(x[[z]]) + col(x[[z]])) %% 2==1]),
                                              Even = sum(x[[z]][(row(x[[z]]) + col(x[[z]])) %% 2==0]))})
oedf <- Reduce(function(x, y) merge(x, y, all = T), 
               oelist, accumulate = F); data.frame(mdata,oedf[-1]) }
# --------------------------------------------------------------
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
# --------------------------------------------------------------
htft <- function(ht, ft, mdata) {data.frame(mdata,
                                            HH = ht$Win  * ft$Win, HD = ht$Win  * ft$Draw, HA = ht$Win  * ft$Lose,
                                            DH = ht$Draw * ft$Win, DD = ht$Draw * ft$Draw, DA = ht$Draw * ft$Lose,
                                            AH = ht$Lose * ft$Win, AD = ht$Lose * ft$Draw, AA = ht$Lose * ft$Lose) }
# --------------------------------------------------------------
halfmostgoal <- function(htmxt, ftmxt, mdata) { z <- 1:length(htmxt)
dfmxt <- lapply(z, function(z) { ftmxt[[z]] - htmxt[[z]] })
handicap(x = dfmxt, mdata = mdata) }
# --------------------------------------------------------------
# hmdf <- ldply(res[is.na(res$FTHG),]$Home, function(x) 
#   result$rating[row.names(result$rating) == x, 'Defence'] %>% 
#               as.character %>% 
#               as.numeric)
# awdf <- ldply(res[is.na(res$FTAG),]$Away, function(x) 
#   result$rating[row.names(result$rating) == x, 'Defence'] %>% 
#               as.character %>% 
#               as.numeric)
# hmof <- ldply(res[is.na(res$FTHG),]$Home, function(x) 
#   result$rating[row.names(result$rating) == x, 'Offence'] %>% 
#                 as.character %>%
#                 as.numeric)
# awof <- ldply(res[is.na(res$FTAG),]$Away, function(x) 
#   result$rating[row.names(result$rating) == x, 'Offence'] %>% 
#                 as.character %>% 
#                 as.numeric)

hmdf <- ldply(res2$Home, function(x) 
  result$rating[row.names(result$rating) == x, 'Defence'] %>% 
    as.character %>% 
    as.numeric)
awdf <- ldply(res2$Away, function(x) 
  result$rating[row.names(result$rating) == x, 'Defence'] %>% 
    as.character %>% 
    as.numeric)
hmof <- ldply(res2$Home, function(x) 
  result$rating[row.names(result$rating) == x, 'Offence'] %>% 
    as.character %>% 
    as.numeric)
awof <- ldply(res2$Away, function(x) 
  result$rating[row.names(result$rating) == x, 'Offence'] %>% 
    as.character %>% 
    as.numeric)

names(hmdf) <- 'Home.Def'
names(awdf) <- 'Away.Def'
names(hmof) <- 'Home.Off'
names(awof) <- 'Away.Off'
mbase <- data.frame(hmof, awdf, hmdf, awof, result$effects[,-1])
rm(hmof, awof, hmdf, awdf)


if(homeavd == FALSE) { lmb1 = (1 - mbase$p) * mbase$Home.Off * mbase$Away.Def
} else { lmb1 = (1 - mbase$p) * mbase$Home.Off * mbase$Away.Def * mbase$Home }
lmb2 <- (1 - mbase$p) * mbase$Away.Off * mbase$Home.Def
lmb3 <- (1 - mbase$p) * mbase$Effect
lmb1 <- lmb1[!is.na(lmb1)]; lmb3 <- lmb3[!is.na(lmb3)]; lmb3 <- lmb3[!is.na(lmb3)]
mbase$Home.Off <- mbase$Home.Def <- mbase$Away.Off <- mbase$Away.Def <- NULL
mbase$Home <- mbase$Effect <- mbase$p <- mbase$theta <- NULL

nl <- 1:length(lmb1)
mxt <- lapply(nl, function(z){outer(0:20, 0:20, function(x, y) 
  bvp(x, y, lambda = c(lmb1[z], lmb2[z], lmb3[z])))})
rm(lmb1, lmb2, lmb3, nl)

FTmxt <- diagdraw(mxt); rm(mxt)
FTWDW <- fodds(FTmxt,mbase)
FTCS <- cscores(FTmxt, mbase)
FTAH <- handicap(FTmxt, mbase)
FTOU <- goalline(FTmxt, mbase)
FTTG <- tgoal(FTmxt, mbase)
FTOE <- oddeven(FTmxt, mbase)
FTTOU <- teamgoal(FTmxt, mbase)

## 
data.frame(res2, FTAH)[c('KODate', 'Home', 'Away', 'HP0.25', 'AN0.25')]
read.xlsx('res2.xlsx', detectDates = TRUE)

(data.frame(res2, FTAH)[c('KODate', 'Home', 'Away', 'HP0.25', 'AN0.25')]$AN0.25 %>% 
    as.character %>% 
    as.numeric %>% .[1] * 1.94) - 
  (1 - (data.frame(res2, FTAH)[c('KODate', 'Home', 'Away', 'AN0.25', 'HP0.25')]$AN0.25 %>% 
          as.character %>% 
          as.numeric %>% .[1]))
#[1]0.3400971




## https://srdas.github.io/MLBook/Gambling.html#linking-the-kelly-criterion-to-portfolio-optimization



