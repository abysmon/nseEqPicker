# ---- Deliverable quantity 2014-15 ----
# url = 'http://www.nseindia.com/archives/equities/mto/MTO_04032015.DAT'

datadir = 'D:/dataset/NSE Delivery Quantity/'
dqfile = c(list.files(paste0(datadir, '2014'), full.names = T, pattern = '^MTO'), 
           list.files(paste0(datadir, '2015'), full.names = T, pattern = '^MTO'))

readDAT = function(filedat){
  data = read.csv(filedat, header = F, as.is = T, skip = 4, 
                  na.strings = "zzzz")[ ,-c(1:2)]
  data$DATE = substr(filedat, start = nchar(filedat)-12+1, stop = nchar(filedat)-4)
  return(data)
}

dqdata = do.call(rbind, lapply(dqfile, readDAT))
colnames(dqdata)[-6] = c('TICKER', 'TYPE', 'TradedQ', 'DeliveryQ', 'DQ/TQ')

dqdata$TradedQ = as.numeric(dqdata$TradedQ)
dqdata$DeliveryQ = as.numeric(dqdata$DeliveryQ)

dqdata = dqdata[ !(dqdata$DATE == '14032014'), ]

prob = read.csv(filedat, header = F, as.is = T, skip = 7)[ ,-c(1:2)]
prob$DATE = '14032014'
colnames(prob)[-6] = c('TICKER', 'TYPE', 'TradedQ', 'DeliveryQ', 'DQ/TQ')

dqdata = rbind(dqdata, prob)
dqdata$DATE = as.POSIXct(strptime(dqdata$DATE, '%d%m%Y'))

write.csv(dqdata, file = 'dq.csv', row.names = F)


# ---- Bhavcopy 2014-15 ----
csvdir1 = 'D:/dataset/nsebhavcopy/2014/'
csvdir2 = 'D:/dataset/nsebhavcopy/2015/'
csvlist1 = list.files(csvdir1, '*.csv', full.names = T)
csvlist2 = list.files(csvdir2, '*.csv', full.names = T)

nse2014 = do.call(rbind, lapply(csvlist1, function(f){ read.csv(f, as.is = T)}))
nse2015 = do.call(rbind, lapply(csvlist2, function(f){ read.csv(f, as.is = T)}))
nse2014 = nse2014[, -c(13,14)]
nse2015 = nse2015[, -c(13,14)]

nse2014$TIMESTAMP = as.POSIXct(strptime(nse2014$TIMESTAMP, '%d-%b-%Y'))
nse2015$TIMESTAMP = as.POSIXct(strptime(nse2015$TIMESTAMP, '%d-%b-%Y'))

nse = rbind(nse2014, nse2015)
write.csv(nse, file = 'nse2014-15.csv', row.names = F)

nse2014.cnx100 = nse2014[ nse2014$SYMBOL %in% cnx100$Symbol, ]
nse2014.cnx100 = nse2014.cnx100[ nse2014.cnx100$SERIES == 'EQ', ]

nse2015.cnx100 = nse2015[ nse2015$SYMBOL %in% cnx100$Symbol, ]
nse2015.cnx100 = nse2015.cnx100[ nse2015.cnx100$SERIES == 'EQ', ]

nse.cnx100 = rbind(nse2014.cnx100, nse2015.cnx100)
nse.cnx100 = na.omit(nse.cnx100)
nse.cnx100 = nse.cnx100[order(nse.cnx100$TIMESTAMP, nse.cnx100$SYMBOL), ]

# nse.cnx100$TIMESTAMP = as.POSIXct(strptime(nse.cnx100$TIMESTAMP, '%d%m%Y'))
write.csv(nse.cnx100, file = 'nse.cnx100.csv', row.names = F)


# ---- OHLCV for CNX100 ----
require(httr)
url = 'http://www.nseindia.com/content/indices/ind_cnx100list.csv'
# temp = tempfile()
# GET(url, user_agent("Mozilla/5.0"), write_disk(temp))
# unlink(temp)
GET(url, user_agent("Mozilla/5.0"), write_disk('cnx100.csv'))
cnx100 = read.csv('cnx100.csv', as.is = T)

dq.cnx100 = dqdata[ dqdata$TICKER %in% cnx100$Symbol, ]
dq.cnx100 = dq.cnx100[ dq.cnx100$TYPE == 'EQ', ]

dq.cnx100$DATE = as.POSIXct(strptime(dq.cnx100$DATE, '%d%m%Y'))
dq.cnx100 = na.omit(dq.cnx100)
dq.cnx100 = dq.cnx100[order(dq.cnx100$DATE, dq.cnx100$TICKER), ]

write.csv(dq.cnx100, file = 'dq.cnx100.csv', row.names = F)


# ---- price, vol, delivery for CNX100 ----
eqcnx100 = cbind(nse.cnx100, dq.cnx100$DeliveryQ)
eqcnx100 = eqcnx100[ ,c(11, 1:10, 12:13)]
colnames(eqcnx100) = c("Date","Ticker","Series","Open","High","Low","Close",
                       "Last","PrevClose","TradedQuantity","TradedValue",
                       "TotalTrades","DeliveryQuantity")

eqcnx100$VWAP = eqcnx100$TradedValue/eqcnx100$TradedQuantity
eqcnx100$DQShare = eqcnx100$DeliveryQuantity/eqcnx100$TradedQuantity*100
eqcnx100$Returns = (eqcnx100$Close/eqcnx100$PrevClose-1)*100
eqcnx100$TradeSize = eqcnx100$TradedQuantity/eqcnx100$TotalTrades
eqcnx100$VWAPReturns = diff(log(eqcnx100$VWAP))*100

write.csv(eqcnx100, file = 'eqcnx100.csv', row.names = F)


# ---- manual adjustment for stock splits ----
eqcnx100adj = read.csv('eqcnx100adj.csv', as.is = T)
eqcnx100adj = eqcnx100adj[ ,c(1:2,7,9, 14:18)]
eqcnx100adj$Date = as.POSIXct(strptime(eqcnx100adj$Date, '%m/%d/%Y'))

eqcnx100adj[eqcnx100adj$Date=='2014-01-01',9] = 
  eqcnx100adj[eqcnx100adj$Date=='2014-01-01',7]

# ---- Rank the top 5 by VWAP returns----
require(dplyr)

Threshold = 10
# eqcnx100adj = group_by(eqcnx100adj, Date)
# eqcnx100adj = arrange(eqcnx100adj, Date, desc(VWAPReturn), Ticker)
eqcnx100adj = eqcnx100adj[order(eqcnx100adj$Date, -eqcnx100adj$VWAPReturn, 
                                eqcnx100adj$Ticker), ]
eqcnx100adj$Rank = 101-ave(eqcnx100adj$VWAPReturn, eqcnx100adj$Date, FUN = rank)
eqcnx100adj$TopPick = ifelse(eqcnx100adj$Rank<(Threshold+1),"Top","Others")


# ---- Add new data ----
# DQ portion
readDAT = function(filedat){
  data = read.csv(filedat, header = F, as.is = T, skip = 4)[ ,-c(1:2)]
  data$DATE = substr(filedat, start = nchar(filedat)-12+1, stop = nchar(filedat)-4)
  return(data)
}

datadir = 'D:/dataset/newdata/'
dqfile = list.files(datadir, full.names = T, pattern = '^MTO')

dqdata = do.call(rbind, lapply(dqfile, readDAT))
colnames(dqdata)[-6] = c('TICKER', 'TYPE', 'TradedQ', 'DeliveryQ', 'DQ/TQ')
# dqdata$TradedQ = as.integer(dqdata$TradedQ)
# dqdata$DeliveryQ = as.integer(dqdata$DeliveryQ)

# Bhavcopy portion
csvlist = list.files(datadir, '*.csv', full.names = T)
nsenew = do.call(rbind, lapply(csvlist, function(f){ read.csv(f, as.is = T)}))
nsenew = nsenew[, -c(13,14)]

# merge them
dqdata = dqdata[ dqdata$TYPE == 'EQ', ]
nsenew = nsenew[ nsenew$SERIES == 'EQ', ]

write.csv(dqdata, file = 'dqdata.csv', row.names = F)
write.csv(nsenew, file = 'nsenew.csv', row.names = F)

all.equal(dqdata$TradedQ, nsenew$TOTTRDQTY)
bcdq.new = cbind(nsenew, nsenew$DeliveryQ)

