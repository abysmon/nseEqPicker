# ---- Deliverable quantity 2014-15 ----
dqdata = read.csv('dq.csv', as.is = T)
dqdata = dqdata[ dqdata$TYPE == 'EQ', ]
# dqdata$DATE = as.POSIXct(strptime(dqdata$DATE, '%m/%d/%Y'))

# ---- Bhavcopy 2014-15 ----

nse = read.csv('nse2014-15.csv', as.is = T)
nse = nse[ nse$SERIES == 'EQ', ]
# nse$TIMESTAMP = as.POSIXct(strptime(nse$TIMESTAMP, '%Y-%m-%d'))

# ---- Merging Bhavcopy and Deliverable quantity 2014-15 ----
require(data.table)

length(unique(dqdata$DATE))
length(unique(nse$TIMESTAMP))

length(unique(dqdata$TICKER))
length(unique(nse$SYMBOL))

unique(dqdata$TYPE)
unique(nse$SERIES)

# bdq = merge(nse, dqdata, by.x = c('SYMBOL', 'TIMESTAMP'), 
#       by.y = c('TICKER', 'DATE'))

dq.table = data.table(dqdata, key = c('TICKER', 'DATE'))
bc.table = data.table(nse, key = c('SYMBOL', 'TIMESTAMP'))

bcdq.table = bc.table[dq.table]
# all.equal(bcdq.table$TOTTRDQTY,bcdq.table$TradedQ)
bcdq.table = na.omit(bcdq.table)

bcdq.table[ ,TYPE:=NULL]
bcdq.table[ ,SERIES:=NULL]
# bcdq.table = bcdq.table[, which(!grepl("foo", colnames(bcdq.table))), with=FALSE]
all.equal(bcdq.table$TOTTRDQTY, bcdq.table$TradedQ)
write.csv(bcdq.table, file = 'bcdq.csv', row.names = F)
# DQ for 14MAR2014 needed correction, some other data got inserted there
# cnx500 = read.csv('cnx500list12Mar2015.csv', as.is = T)

bcdq.table = read.csv('bcdq.csv', as.is = T)
identical(bcdq.table$TOTTRDQTY, bcdq.table$TradedQ)
bcdq.table = bcdq.table[ ,c(10,1:9,11,13)]

# ---- Load and append the new data ----
bcdq.new = read.csv('newd.csv', as.is = T)
bcdq.table = rbind(bcdq.table, bcdq.new)

# Rename the columns
colnames(bcdq.table) = c("Date","Ticker","Open","High","Low","Close",
                       "Last","PrevClose","TradedQuantity","TradedValue",
                       "TotalTrades","DeliveryQuantity")

# ---- Add derived metrics to the data ----
bcdq.table$VWAP = bcdq.table$TradedValue/bcdq.table$TradedQuantity
bcdq.table$DQShare = bcdq.table$DeliveryQuantity/bcdq.table$TradedQuantity*100
bcdq.table$Returns = (bcdq.table$Close/bcdq.table$PrevClose-1)*100
bcdq.table$TradeSize = bcdq.table$TradedQuantity/bcdq.table$TotalTrades
# eqcnx100$VWAPReturns = diff(log(eqcnx100$VWAP))*100

write.csv(bcdq.table, file = 'nse.unadj.csv', row.names = F)


# ---- CNX500 only ----
cnx500 = read.csv('cnx500list12Mar2015.csv', as.is = T)
nse.cnx500 = bcdq.table[ bcdq.table$Ticker %in% cnx500$Symbol, ]

write.csv(nse.cnx500, file = 'cnx500.unadj.csv', row.names = F)

# ---- Removing the splits ----
x = unique(nse.cnx500$Returns[nse.cnx500$Returns < -20])
x[order(x)]
x = unique(nse.cnx500$Ticker[nse.cnx500$Returns < -20])
x = x[order(x)]

nse.cnx500 = nse.cnx500[ !(nse.cnx500$Ticker %in% x), ]
nse.cnx500$Date = as.POSIXct(strptime(nse.cnx500$Date, '%m/%d/%Y'))

# ---- Rank the top stks by Closing Arithmetic returns ----
Threshold = 10
nse.cnx500$Rank = ave(nse.cnx500$Returns, nse.cnx500$Date, FUN = rank)
nse.cnx500$TopPick = ifelse(nse.cnx500$Rank < (Threshold+1), "Top", "Others")

# sort the data
nse.cnx500 = nse.cnx500[order(nse.cnx500$Date, nse.cnx500$Rank), ]

# ---- Fit classifiers, as many as possible ----
require(caret)
require(nnet)
require(randomForest)


cutoff = which(nse.cnx500$Date == '2015-01-15')[1]
trainSet = nse.cnx500[1:(cutoff-1), -10]
testSet = nse.cnx500[cutoff:NROW(nse.cnx500), -10]

rfCtrl = trainControl(method = "adaptive_cv", repeats = 3, verboseIter = T)
rfModel3 = train(factor(TopPick) ~. - Date - Ticker, data = trainSet, 
                   trControl = rfCtrl, verbose = FALSE)
pred.rf = predict(rfModel, testSet)

nnCtrl = trainControl(method = "adaptive_cv", repeats = 5, verboseIter = F)
nnetModel3 = train(factor(TopPick) ~. - Date - Ticker, data = trainSet, 
                   method = 'nnet', trControl = nnCtrl, verbose = FALSE)
pred.nnet3 = predict(nnetModel3, testSet)


# ---- Time to validate ----
results = cbind(testSet$TopPick, as.character(pred.lda), as.character(pred.lda2),
                as.character(pred.qda), as.character(pred.nnet), 
                as.character(pred.nnet3), as.character(pred.rf),
                as.character(pred.svm), as.character(pred.svm.x))
colnames(results) = c('testSet', 'pred.lda', 'pred.lda2', 'pred.qda', 
                      'pred.nnet', 'pred.nnet3', 'pred.rf', 
                      'pred.svm', 'pred.svm.tuned')
results = as.data.frame(results)

# we want only performance of Top 5 recommendation
intSet = results[results$testSet=='Top', ]
accuracy = 100* c(sum(intSet$testSet == intSet$pred.lda)/NROW(intSet),
                  sum(intSet$testSet == intSet$pred.lda2)/NROW(intSet),
                  sum(intSet$testSet == intSet$pred.qda)/NROW(intSet),
                  sum(intSet$testSet == intSet$pred.nnet)/NROW(intSet),
                  sum(intSet$testSet == intSet$pred.nnet3)/NROW(intSet),
                  sum(intSet$testSet == intSet$pred.rf)/NROW(intSet),
                  sum(intSet$testSet == intSet$pred.svm)/NROW(intSet),
                  sum(intSet$testSet == intSet$pred.svm.tuned)/NROW(intSet))
accuracy = as.data.frame(accuracy)
accuracy$model = c('lda', 'lda2','qda','nnet', 'nnet-tuned','rf','svm', 'svm.tuned')

ggplot(accuracy, aes(x = model, y = accuracy)) + 
  geom_bar(stat = 'identity', fill = 'cyan4')
