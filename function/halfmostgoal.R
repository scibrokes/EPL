halfmostgoal <- function(htmxt, ftmxt, mdata) { z <- 1:length(htmxt)
dfmxt <- lapply(z, function(z) { ftmxt[[z]] - htmxt[[z]] })
handicap(x = dfmxt, mdata = mdata) }