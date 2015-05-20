# ---- Equity data ----
loadCM = function(cmfile){
  tryCatch({
    tv = read.csv(cmfile, as.is = T, na.strings = 'zzzz')
    tv = tv[, -c(13,14)]
    tv = tv[ tv$SERIES == 'EQ', ]
    tv = tv[order(tv$TIMESTAMP, tv$SYMBOL), ]
    tv$TIMESTAMP = as.POSIXct(strptime(tv$TIMESTAMP, '%d-%b-%Y'))
    tv = cbind(DATE = tv$TIMESTAMP, tv[-11])
    return(tv)
    }, 
    error = function(e) NULL)
}



# ---- Deliverable quantity data ----
loadDQ = function(dqfile){
  tryCatch({
    datura = 
      read.csv(dqfile, header = F, as.is=T, skip=4, na.strings="zzzz")[ ,-c(1:2)]
    datura$DATE = substring(dqfile, first = nchar(dqfile)-12+1, last = nchar(dqfile)-4)
    colnames(datura)[-6] = c('SYMBOL', 'TYPE', 'TradedQ', 'DeliveryQ', 'DQratio')
    datura$DATE = as.POSIXct(strptime(datura$DATE, '%d%m%Y'))
    datura = datura[ datura$TYPE == 'EQ', ]
    datura = datura[order(datura$DATE, datura$SYMBOL), ]
    datura = cbind(DATE = datura$DATE, datura[-c(6)])
    return(datura)
  }, 
  error = function(e) NULL)
}


# ---- Futures data ----
loadFO = function(fofile){
  tryCatch({
    elmy = read.csv(fofile, as.is = T)
    elmy = elmy[ (elmy$INSTRUMENT == 'FUTSTK'), -c(1, 4:5, 16)]
    elmy$MONTH = 
      with(elmy, paste0('MONTH', ave(seq_along(SYMBOL), SYMBOL, FUN = seq_along)) )
    brain = reshape(elmy[-2], idvar = c('SYMBOL', 'TIMESTAMP'), 
                    timevar = 'MONTH', direction = 'wide')
    brain$TIMESTAMP = as.POSIXct(strptime(brain$TIMESTAMP, format = "%d-%b-%Y"))
    brain = cbind(DATE = brain[ ,2], brain[ ,-2])
    brain = brain[1:11]
    return(brain)
  }, 
  error = function(e) NULL)
}

