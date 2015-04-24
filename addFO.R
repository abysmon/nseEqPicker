cnx100 = read.csv('cnx100.csv', as.is = T)

foread = function(foname){
  fodata = read.csv(foname, as.is = T)
  fodata = fodata[ (fodata$INSTRUMENT == 'FUTSTK'), c(2:3, 14:15)]
  fodata = tidyr::spread(fodata, key = EXPIRY_DT, value = CHG_IN_OI)
  colnames(fodata)[3:5] = c('Month1', 'Month2', 'Month3')
  return(fodata)
}

fofiles = c(list.files('D:/dataset/nseFO/2014/', '*.csv$', full.names = T, recursive = T),
            list.files('D:/dataset/nseFO/2015/', '*.csv$', full.names = T, recursive = T))
# lists = do.call(rbind, lapply(list.files('D:/dataset/nseFO/links', full.names = T), function(x){read.csv(x, as.is = T)}))

fostk = do.call(rbind, lapply(fofiles, foread))
fostk$TIMESTAMP = as.POSIXct(strptime(fostk$TIMESTAMP, format = "%d-%b-%Y"))
fostk = cbind(fostk[,2], fostk[,-2])
colnames(fostk)[1] = "Date"
write.csv(fostk, file = 'fostk.all.csv', row.names = F)


fodata = read.csv('D:/dataset/nseFO/2015/fo10APR2015bhav.csv', as.is = T)
fodata = fodata[ (fodata$INSTRUMENT == 'FUTSTK'), -c(1, 4:5, 16)]
fodata$EXPIRY_DT =  as.POSIXct(strptime(fodata$EXPIRY_DT, "%d-%b-%Y"))
fodata$TIMESTAMP =  as.POSIXct(strptime(fodata$TIMESTAMP, "%d-%b-%Y"))
fodata$Gap = fodata$EXPIRY_DT - fodata$TIMESTAMP
fodata$Gap = as.numeric(fodata$Gap)



