# ---- Equity 2014-15 ----

cmlist = c(list.files('D:/dataset/nseCM/2014/', '^cm', full.names = T), 
           list.files('D:/dataset/nseCM/2015/', '^cm', full.names = T))

CM = do.call(rbind, lapply(cmlist, function(f){
  read.csv(f, as.is = T, na.strings = 'zzzz')}))

CM = CM[, -c(13,14)]
CM = CM[ CM$SERIES == 'EQ', ]
CM = CM[order(CM$TIMESTAMP, CM$SYMBOL), ]
colnames(CM)[11] = "Date"
CM$Date = as.POSIXct(strptime(CM$Date, '%d-%b-%Y'))

write.csv(CM, file = 'FO.only/CM.all.csv', row.names = F)


# ---- Deliverable quantity 2014-15 ----
dqlist = c(list.files('D:/dataset/nseDQ/2014/', '^MTO', full.names = T), 
           list.files('D:/dataset/nseDQ/2015/', '^MTO', full.names = T))

readDAT = function(filedat){
  data = read.csv(filedat, header = F, as.is = T, skip = 4, 
                  na.strings = "zzzz")[ ,-c(1:2)]
  data$Date = substring(filedat, first = nchar(filedat)-12+1, last = nchar(filedat)-4)
  return(data)
}

DQ = do.call(rbind, lapply(dqlist, readDAT))
colnames(DQ)[-6] = c('TICKER', 'TYPE', 'TradedQ', 'DeliveryQ', 'DQratio')
DQ$Date = as.POSIXct(strptime(DQ$Date, '%d%m%Y'))
DQ = DQ[ DQ$TYPE == 'EQ', ]
DQ = DQ[order(DQ$Date, DQ$TICKER), ]

write.csv(DQ, file = 'FO.only/DQ.all.csv', row.names = F)


# ---- Futures 2014-15 ----
folist = c(list.files('D:/dataset/nseFO/2014/', '^fo', full.names = T), 
           list.files('D:/dataset/nseFO/2015/', '^fo', full.names = T))

# using base R
foread = function(foname){
  fodata = read.csv(foname, as.is = T)
  fodata = fodata[ (fodata$INSTRUMENT == 'FUTSTK'), -c(1, 4:5, 16)]
  fodata$MONTH = with(
    fodata, paste0('MONTH', ave(seq_along(SYMBOL), SYMBOL, FUN = seq_along)) )
  res = reshape(fodata[-2], idvar=c('SYMBOL', 'TIMESTAMP'), 
                  timevar = 'MONTH', direction = 'wide')
  return(res)
}

# # using base R
# # weird result
# foread = function(foname){
#   fodata = read.csv(foname, as.is = T)
#   fodata = fodata[ (fodata$INSTRUMENT == 'FUTSTK'), -c(1, 4:5, 16)]
#   keys = c('SYMBOL','TIMESTAMP');
#   res = aggregate(fodata[ ,!(names(fodata) %in% keys)], 
#                   fodata[ ,names(fodata) %in% keys], identity)
#   return(res)
# }

# using base R
# foread = function(foname){
#   fodata = read.csv(foname, as.is = T)
#   fodata = fodata[ (fodata$INSTRUMENT == 'FUTSTK'), -c(1, 4:5, 16)]
#   keys = c('SYMBOL','TIMESTAMP')
#   mnum = ave(1:nrow(fodata), fodata[ ,keys], FUN = seq)
#   mdata = lapply(1:max(mnum), function(x) setNames(fodata[mnum == x, ], ifelse(names(fodata) %in% keys, names(fodata), paste0(names(fodata), '.Month', x))) )
#   res = Reduce(function(x,y) merge(x, y, by = keys, all = T), mdata )
#   res = res[, -c(3, 13, 23)]
#   return(res)
# }

# using data.table
# require(data.table) # atleast v 1.9.5
# foread = function(foname){
#   fodata = read.csv(foname, as.is = T)
#   fodata = fodata[ (fodata$INSTRUMENT == 'FUTSTK'), -c(1, 4:5, 16)]
#   res = dcast(setDT(fodata)[ ,Month:=paste0('Month', 1:.N), by = SYMBOL],
#                SYMBOL+TIMESTAMP ~ Month, value.var = names(fodata)[3:11])
#   setnames(res, sub('([^_]+)_(.*)', '\\2.\\1', colnames(res)))
#   res1 <- setDF(res[ ,names(output), with = FALSE])
#   return(res)
# }


folist = folist[order(folist)]
FO = do.call(rbind, lapply(folist, foread))
FO$TIMESTAMP = as.POSIXct(strptime(FO$TIMESTAMP, format = "%d-%b-%Y"))
FO = cbind(FO[,2], FO[,-2])
colnames(FO)[1] = "Date"

write.csv(FO, file = 'FO.only/FO.all.csv', row.names = F)


# ---- Keep FO stks data only ----
CM = CM[order(CM$Date, CM$SYMBOL), ]
DQ = DQ[order(DQ$Date, DQ$TICKER), ]

identical(CM$SYMBOL, DQ$TICKER)
identical(CM$Date, DQ$Date)
identical(CM$TOTTRDQTY, DQ$TradedQ)

CD = cbind(CM[11], CM[-c(2,11)], DQ[4])
# rm(CM, DQ)
# gc()
.SD
# get the latest FO stk list
fodata = read.csv('D:/dataset/nseFO/2015/fo10APR2015bhav.csv', as.is = T)
fodata = fodata[ (fodata$INSTRUMENT == 'FUTSTK'), -c(1, 4:5, 16)]
fostks = unique(fodata$SYMBOL)

# only for latest month
FO.ds = FO[1:11]
# only for stks in latest data
FO.ds2 = FO.ds[ FO.ds$SYMBOL %in% fostks, ]

# only for deriv segment stks in latest data
CD.ds = CD[ CD$SYMBOL %in% fostks, ]

# merge the CM+DQ data with FO data
FCD = merge(CD.ds, FO.ds2, by=c("Date","SYMBOL"), all = F)


# ---- add returns and ranking ----
# FCD$logReturns = (log(FCD$CLOSE, base = 10) - log(FCD$PREVCLOSE, base = 10))*100
FCD$arReturns = (FCD$CLOSE/FCD$PREVCLOSE-1)*100

# FCD[FCD$logReturns < -20, c(1,2,22,23)]
FCD[FCD$arReturns < -20, c(1,2,22)]


# ---- manual adjustment for stock splits ----
# works only for a single split
splitstks = unique(FCD$SYMBOL[FCD$arReturns < -20])
splitstks = splitstks[order(splitstks)]

res = FCD
for(idx in seq_along(splitstks)){
  vaping = FCD[ FCD$SYMBOL == splitstks[idx], ]
  exidx = which.min(vaping$arReturns)
  mx = round(vaping$PREVCLOSE[exidx]/vaping$CLOSE[exidx], digits = 0)
  prices = c(3:8, 13:17)
  volumes = c(9,12)
  vaping[1:(exidx-1), prices] = vaping[1:(exidx-1), prices]/mx # prices get divided
  vaping[1:(exidx-1), volumes] = vaping[1:(exidx-1), volumes]*mx # volumes get multiplied
  vaping$PREVCLOSE[exidx] = vaping$PREVCLOSE[exidx]/mx
  
  res = res[!res$SYMBOL %in% splitstks[idx], ]
  res = rbind(res, vaping)
}

res$arReturns = (res$CLOSE/res$PREVCLOSE-1)*100
splits = unique(res$SYMBOL[res$arReturns < -20])
splits = splits[order(splits)]
res[res$arReturns < -20, c(1,2,22)]
qplot(Date, CLOSE, data = vaping, col = arReturns)

# write.csv(FO.ds2, file = 'FO.only/FO.ds.csv', row.names = F)
# write.csv(CD.ds, file = 'FO.only/CD.ds.csv', row.names = F)
write.csv(FCD, file = 'FO.only/FCD.csv', row.names = F)


# ---- Post processing ----
res = read.csv('FO.only/FCD.splitadj.csv', as.is = T)
# completely remove the stk having partly missing entries
mis = unique(res[ is.na(res$CLOSE.MONTH1), 2])
res = res[ !res$SYMBOL %in% mis, ]
NROW(res)/length(unique(res$Date))
length(unique(res$SYMBOL))


# ---- Rank the top stks by Closing Arithmetic returns ----
Threshold = 10
res = transform(res, Rank = ave(arReturns, Date, FUN = function(x) rank(-x, ties.method = "first")))
# nse$Rank = ave(nse$Returns, nse$Date, 
#                FUN = function(x) rank(-x, ties.method = "first"))
res$TopPick = ifelse(res$Rank < (Threshold+1), "Top", "Others")
res = res[order(res$Date, res$Rank), ] # sort the data
write.csv(res, file = 'FO.only/FCD.splitadj.csv', row.names = F)


# ---- file list check ----
listcm = strptime(substring(cmlist, 25, 33), "%d%b%Y")
listfo = strptime(substring(folist, 25, 33), "%d%b%Y")
listdq = strptime(substring(dqlist, 27, 34), "%d%m%Y")

listcm[!listcm %in% listdq]
listcm[!listcm %in% listfo]


fodata = read.csv('D:/dataset/nseFO/2015/fo10APR2015bhav.csv', as.is = T)
fodata = fodata[ (fodata$INSTRUMENT == 'FUTSTK'), -c(1, 4:5, 16)]

ave(seq_along(fodata$SYMBOL), fodata$SYMBOL, FUN = seq_along)
setnames(res2, sub('([^_]+)_(.*)', '\\2.\\1', colnames(res2)))
res1 <- setDF(res2[,names(output), with=FALSE])
