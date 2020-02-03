htft <- function(ht, ft, mdata) {data.frame(mdata,
                                            HH = ht$Win  * ft$Win, HD = ht$Win  * ft$Draw, HA = ht$Win  * ft$Lose,
                                            DH = ht$Draw * ft$Win, DD = ht$Draw * ft$Draw, DA = ht$Draw * ft$Lose,
                                            AH = ht$Lose * ft$Win, AD = ht$Lose * ft$Draw, AA = ht$Lose * ft$Lose) }