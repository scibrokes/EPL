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

if(!require('Rmodel')) {
  install_github('scibrokes/Rmodel')
}
library('Rmodel')

source('function/compileIndex2.R')
source('function/bvp.R')
source('function/compileOdds (need to review).R')

l1=FTHG~1; l2=FTAG~1; l1l2= ~c(Home,Away)+c(Away,Home); l3=~1;
data = na.omit(res); maxit=300; xi=-0.000007; fordate=NULL; fun="glm"; inflated=TRUE


res <- read.xlsx('res2.xlsx', detectDates = TRUE)
names(res) <- res[1,]
res <- res[-1,]
res %<>% mutate(Round = as.numeric(Round), KODate = ymd(KODate), Home = factor(Home), 
                Away = factor(Away), FTHG = as.numeric(FTHG), FTAG = as.numeric(FTAG), 
                Handicap = factor(Handicap), Home.f = as.numeric(Home.f), 
                Away.f = as.numeric(Away.f), AH.f1 = as.numeric(AH.f1), 
                AH.PL1 = as.numeric(AH.PL1), OU = factor(OU), Over = as.numeric(Over), 
                Under = as.numeric(Under), OU.f1 = as.numeric(OU.f1), 
                OU.PL1 = as.numeric(OU.PL1))
res$`NA` <- NULL

result <- compileIndex2(data=res, xi = -0.000007, fordate = ymd('2019-12-21'))
res2 <- subset(res, KODate == ymd('2019-12-21'))


## ==================================================================
source('res.R')

## ==================================================================
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



