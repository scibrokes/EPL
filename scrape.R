library(BBmisc)
<<<<<<< HEAD
pkg <- c('rvest','tidyverse','plyr','dplyr','stringr','xml2','XML', 
         'RCurl', 'devtools', 'RSelenium', 'lubridate', 'zoo')
=======
pkg <- c('rvest','tidyverse','plyr','dplyr','stringr','xml2','XML', 'tidyr', 
         'RCurl', 'devtools', 'RSelenium', 'lubridate', 'zoo', 'magrittr', 
         'purrr', 'PerformanceAnalytics', 'openxlsx')
>>>>>>> aae6d845c2334bf650f93a53ec21415d0efcf3b4
lib(pkg)

lnk <- 'https://188yingqiu.com/en-gb/asia'
lnk2a <- 'https://www.scorespro.com/soccer/england/premier-league/results/'
lnk2b <- 'https://www.scorespro.com/soccer/england/premier-league/fixtures/'

res <- lnk2a %>% 
  read_html %>% 
  html_table(fill=TRUE) %>% 
  bind_rows %>% 
<<<<<<< HEAD
  tbl_df %>% 
  .[-1, 1:5]
=======
  .[-1, c(1, 3:5)]
>>>>>>> aae6d845c2334bf650f93a53ec21415d0efcf3b4

fix <-lnk2b %>% 
  read_html %>% 
  html_table(fill=TRUE) %>% 
  bind_rows %>% 
<<<<<<< HEAD
  tbl_df %>% 
  .[-1, 1:5]

res <- lnk2a %>% 
  read_html %>% 
  html_table(fill=TRUE) %>% 
  bind_rows %>% 
  tbl_df %>% 
  .[-1, c(1,3:5)]

fix <-lnk2b %>% 
  read_html %>% 
  html_table(fill=TRUE) %>% 
  bind_rows %>% 
  tbl_df %>% 
  .[-1, c(1,3:5)]
=======
  .[-1, c(1, 3:5)]
>>>>>>> aae6d845c2334bf650f93a53ec21415d0efcf3b4

res <- bind_rows(res, fix)
rm(fix)

<<<<<<< HEAD
res %<>% rename('index' = X1, 'Home' = X3, 
                HG = 'X4', 'Away' = X5)
score <- res$HG %>% 
  str_split_fixed(' - ', 3) %>% 
  .[,1:2] %>% data.frame %>% 
  tbl_df %>% mutate_if(is.factor, as.numeric) %>% 
  rename(HG = X1, AG = X2)

res <- bind_cols(res[,-3], score)
rm(score)
index <- res$index %>% str_replace_all('\\.', '/')
index <- substr(index, 1, 8) %>% dmy
res$index <- index
=======
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
>>>>>>> aae6d845c2334bf650f93a53ec21415d0efcf3b4

res %<>% mutate(Home = as.factor(Home), 
                Away = as.factor(Away))
rm(index)
res %<>% arrange(index)

<<<<<<< HEAD
## 
## https://www.scorespro.com/soccer/england/premier-league/results/

res <- readLines('fixture.txt') %>% 
  str_split('\t') %>% llply(., function(x) x %>% data.frame %>% t %>% data.frame) %>% 
  bind_rows %>% 
  .[c(1:2, 4:6)] %>% tbl_df
names(res) <- c('Round', 'index', 'Home', 'HG', 'Away')

score <- res$HG %>% 
  str_split_fixed(' - ', 3) %>% 
  .[,1:2] %>% data.frame %>% 
  tbl_df %>% mutate_if(is.factor, as.numeric) %>% 
  rename(HG = X1, AG = X2)

res <- bind_cols(res[,-4], score)
rm(score)
index <- res$index %>% str_replace_all('\\.', '/')
index <- substr(index, 1, 8) %>% dmy
res$index <- index
=======
## =============================================================
## https://www.scorespro.com/soccer/england/premier-league/results/

res <- readLines('fixture.txt') %>% 
  str_split('\t|nn') %>% llply(., function(x) x %>% data.frame %>% t %>% data.frame) %>% 
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
>>>>>>> aae6d845c2334bf650f93a53ec21415d0efcf3b4

res %<>% mutate(Home = as.factor(Home), 
                Away = as.factor(Away))
rm(index)

res %<>% 
  mutate(Round = na.locf(Round) %>% 
           str_replace_all('Round ', '') %>% 
<<<<<<< HEAD
           as.numeric) %>% 
  na.omit
res %<>% arrange(index)

## =========================================================

install_github('scibrokes/Rmodel')
require('Rmodel')

res %<>% rename(KODate = index, FTHG = HG, FTAG = AG)
compileIndex2(data=res)

=======
           as.numeric)
res <- res[!is.na(res$Home),]
res %<>% arrange(KODate) %>% data.frame

resb <- read.xlsx('res2.xlsx', detectDates = TRUE)
names(resb) <- resb[1,]
resb <- resb[-1,]
resb %<>% mutate(Round = as.numeric(res$Round), KODate = ymd(res$KODate), Home = factor(res$Home), 
                 Away = factor(res$Away), FTHG = as.numeric(res$FTHG), FTAG = as.numeric(res$FTAG), 
                 Handicap = factor(Handicap), Home.f = as.numeric(Home.f), 
                 Away.f = as.numeric(Away.f), AH.f1 = as.numeric(AH.f1), 
                 AH.PL1 = as.numeric(AH.PL1), OU = factor(OU), Over = as.numeric(Over), 
                 Under = as.numeric(Under), OU.f1 = as.numeric(OU.f1), 
                 OU.PL1 = as.numeric(OU.PL1))
resb$`NA` <- NULL

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

result <- compileIndex2(data=res, xi = -0.000007, fordate = ymd('2019-12-15'))
res2 <- data.frame(subset(res, KODate == ymd('2019-12-15')), 
                   subset(resb, KODate == ymd('2019-12-15'))[,-c(1:6)])


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
>>>>>>> aae6d845c2334bf650f93a53ec21415d0efcf3b4



