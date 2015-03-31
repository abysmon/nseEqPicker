# ---- Bhavcopy 2014-15 ----
csvlist = c(list.files('D:/dataset/nsebhavcopy/2014/', '*.csv', full.names = T), 
            list.files('D:/dataset/nsebhavcopy/2015/', '*.csv', full.names = T))

bhavc = do.call(rbind, lapply(csvlist, function(f){ 
  read.csv(f, as.is = T, na.strings = 'zzzz')}))

bhavc = bhavc[, -c(13,14)]
# sum(is.na(bhavc))
bhavc$TIMESTAMP = as.POSIXct(strptime(bhavc$TIMESTAMP, '%d-%b-%Y'))
bhavc = bhavc[ bhavc$SERIES == 'EQ', ]
bhavc = bhavc[order(bhavc$TIMESTAMP, bhavc$SYMBOL), ]
# length(unique(bhavc$TIMESTAMP))
# unique(bhavc$SERIES)

# ---- Deliverable quantity 2014-15 ----
dqdir = 'D:/dataset/NSE Delivery Quantity/'
dqfile = c(list.files(paste0(dqdir, '2014'), full.names = T, pattern = '^MTO'), 
           list.files(paste0(dqdir, '2015'), full.names = T, pattern = '^MTO'))

readDAT = function(filedat){
  data = read.csv(filedat, header = F, as.is = T, skip = 4, 
                  na.strings = "zzzz")[ ,-c(1:2)]
  data$DATE = substr(filedat, start = nchar(filedat)-12+1, stop = nchar(filedat)-4)
  return(data)
}

dqdata = do.call(rbind, lapply(dqfile, readDAT))
colnames(dqdata)[-6] = c('TICKER', 'TYPE', 'TradedQ', 'DeliveryQ', 'DQ/TQ')
dqdata$DATE = as.POSIXct(strptime(dqdata$DATE, '%d%m%Y'))
dqdata = dqdata[ dqdata$TYPE == 'EQ', ]
dqdata = dqdata[order(dqdata$DATE, dqdata$TICKER), ]

# all.equal(dqdata$TICKER, bhavc$SYMBOL)
# all.equal(dqdata$DATE, bhavc$TIMESTAMP)
# all.equal(dqdata$TradedQ, bhavc$TOTTRDQTY)
# x=dqdata$TradedQ- bhavc$TOTTRDQTY
# which(x != 0)

# ---- price, vol, delivery ----
nse = cbind(bhavc, dqdata$DeliveryQ)
nse = nse[ ,c(11, 1, 3:10, 12:13)]
colnames(nse) = c("Date","Ticker","Open","High","Low","Close",
                       "Last","PrevClose","TradedQuantity","TradedValue",
                       "TotalTrades","DeliveryQuantity")

nse$VWAP = nse$TradedValue/nse$TradedQuantity
nse$DQShare = nse$DeliveryQuantity/nse$TradedQuantity*100
nse$Returns = (nse$Close/nse$PrevClose-1)*100
nse$TradeSize = nse$TradedQuantity/nse$TotalTrades

write.csv(nse, file = 'nse.unadj.eq.csv', row.names = F)

# ---- CNX500 only ----
cnx500 = read.csv('cnx500list12Mar2015.csv', as.is = T)
nse.cnx500 = nse[ nse$Ticker %in% cnx500$Symbol, ]

# ---- Removing the splits ----
x = unique(nse$Returns[nse$Returns < -20])
x[order(x)]
splitstks = unique(nse$Ticker[nse$Returns < -20])
splitstks = splitstks[order(splitstks)]
nse.cnx500 = nse.cnx500[ !(nse.cnx500$Ticker %in% splitstks), ]

# ---- Rank the top stks by Closing Arithmetic returns ----
Threshold = 10
nse.cnx500 = transform(nse.cnx500, Rank = ave(Returns, Date, 
                                FUN = function(x) rank(-x, ties.method = "first")))
# nse$Rank = ave(nse$Returns, nse$Date, 
#                FUN = function(x) rank(-x, ties.method = "first"))
nse.cnx500$TopPick = ifelse(nse.cnx500$Rank < (Threshold+1), "Top", "Others")
nse.cnx500 = nse.cnx500[order(nse.cnx500$Date, nse.cnx500$Rank), ] # sort the data
