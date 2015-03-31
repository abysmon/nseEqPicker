cnx100 = read.csv('cnx100.csv', as.is = T)
fodata = read.csv('D:/dataset/2015-03-09/fo/fo13MAR2015bhav.csv', as.is = T)
length(unique(fodata$SYMBOL[ fodata$INSTRUMENT == c('FUTSTK','OPTSTK')]))
futstk=unique(fodata$SYMBOL[ fodata$INSTRUMENT == c('FUTSTK')])
optstk=unique(fodata$SYMBOL[ fodata$INSTRUMENT == c('OPTSTK')])
futstk=futstk[order(futstk)]
optstk=optstk[order(optstk)]
