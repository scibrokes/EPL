compileOdds <- function(res, resb, fordate = '2019-12-14') {
  
  source('function/diagdraw.R')
  source('function/fodds.R')
  source('function/cscores.R')
  source('function/handicap.R')
  source('function/goalline.R')
  source('function/tgoal.R')
  source('function/oddeven.R')
  source('function/teamgoal.R')
  source('function/htft.R')
  source('function/halfmostgoal.R')
  
  result <- compileIndex2(data=res, xi = -0.000007, fordate = ymd(fordate))
  res2 <- data.frame(subset(res, KODate == ymd('2019-12-15')), 
                     subset(resb, KODate == ymd('2019-12-15'))[,-c(1:6)])
  
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
  
  ## AH and OU
  res2$AH.prob <- llply(seq(res2$Handicap), function(i) {
    FTAH[i, as.character(res2$Handicap[i])] %>% 
      as.character %>% as.numeric
  }) %>% unlist
  
  res2$OU.prob <- llply(seq(res2$OU), function(i) {
    FTOU[i, as.character(res2$OU[i])] %>% 
      as.character %>% as.numeric
  }) %>% unlist
  
  res3 <- (data.frame(res2, FTAH)[c('KODate', 'Home', 'Away', 'HP0.25', 'AN0.25')]$AN0.25 %>% 
    as.character %>% 
    as.numeric %>% .[1] * 1.94) - 
  (1 - (data.frame(res2, FTAH)[c('KODate', 'Home', 'Away', 'AN0.25', 'HP0.25')]$AN0.25 %>% 
          as.character %>% 
          as.numeric %>% .[1]))
  
  tmp
}

