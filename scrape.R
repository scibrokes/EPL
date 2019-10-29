library(BBmisc)
pkg <- c('rvest','tidyverse','plyr','dplyr','stringr','xml2','XML', 
         'RCurl', 'devtools', 'RSelenium', 'lubridate', 'zoo')
lib(pkg)

lnk <- 'https://188yingqiu.com/en-gb/asia'
lnk2a <- 'https://www.scorespro.com/soccer/england/premier-league/results/'
lnk2b <- 'https://www.scorespro.com/soccer/england/premier-league/fixtures/'

res <- lnk2a %>% 
  read_html %>% 
  html_table(fill=TRUE) %>% 
  bind_rows %>% 
  tbl_df %>% 
  .[-1, 1:5]

fix <-lnk2b %>% 
  read_html %>% 
  html_table(fill=TRUE) %>% 
  bind_rows %>% 
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

res <- bind_rows(res, fix)
rm(fix)

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

res %<>% mutate(Home = as.factor(Home), 
                Away = as.factor(Away))
rm(index)
res %<>% arrange(index)

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

res %<>% mutate(Home = as.factor(Home), 
                Away = as.factor(Away))
rm(index)

res %<>% 
  mutate(Round = na.locf(Round) %>% 
           str_replace_all('Round ', '') %>% 
           as.numeric) %>% 
  na.omit
res %<>% arrange(index)

## =========================================================

install_github('scibrokes/Rmodel')
require('Rmodel')

res %<>% rename(KODate = index, FTHG = HG, FTAG = AG)
compileIndex2(data=res)




