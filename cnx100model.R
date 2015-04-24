# ---- Load the data ----
nse = read.csv('nse.unadj.eq.csv', as.is = T)

# ---- Keep CNX100 only ----
cnx100 = read.csv('cnx100.csv', as.is = T)
nse.cnx100 = nse[ nse$Ticker %in% cnx100$Symbol, ]
nse.cnx100$Date = as.POSIXct(strptime(nse.cnx100$Date, '%Y-%m-%d'))

# ---- manual adjustment for stock splits ----
# works only for a single split
splitstks = unique(nse.cnx100$Ticker[nse.cnx100$Returns < -20])
splitstks = splitstks[order(splitstks)]

sbin = nse.cnx100[ nse.cnx100$Ticker == 'SBIN', ]
exidx = which.min(sbin$Returns)
mx = round(sbin$PrevClose[exidx]/sbin$Close[exidx], digits = 0)
sbin[1:exidx, 3:8] = sbin[1:exidx, 3:8]/mx # prices get divided
sbin[1:exidx, c(9,12)] = sbin[1:exidx, c(9,12)]*mx # volumes get multiplied




# ---- Rank the top stocks by Arithmetic returns on Closing prices ----
Threshold = 10
nse.cnx100 = transform(nse.cnx100, Rank = ave(Returns, Date, FUN = function(x) 
  rank(-x, ties.method = "first")))

nse.cnx100$TopPick = ifelse(nse.cnx100$Rank < (Threshold+1), "Top", "Others")
nse.cnx100 = nse.cnx100[order(nse.cnx100$Date, nse.cnx100$Rank), ] # sort the data

# ---- Load the library and split data for cross-validation ----
require(caret)
require(nnet)
require(randomForest)
require(e1071) # for fast svm
require(kernlab) # for caret svm

cutoff = which(nse.cnx100$Date == '2015-01-12')[1]
trainSet = nse.cnx100[1:(cutoff-1), -17]
testSet = nse.cnx100[cutoff:NROW(nse.cnx100), -17]

# ---- Fit classifiers, as many as possible ----

# start with random Forest
rfCtrl = trainControl(method = "adaptive_cv", repeats = 3, verboseIter = T)
rfModel = train(factor(TopPick) ~. - Date - Ticker, data = trainSet, 
                trControl = rfCtrl, verbose = FALSE)
pred.rf = predict(rfModel, testSet)
table(pred.rf)

rfCtrl2 = trainControl(method = "adaptive_cv", repeats = 2, verboseIter = T, number = 5)
rfModel2 = train(factor(TopPick) ~. - Date - Ticker - Returns, data = trainSet, 
                trControl = rfCtrl2, verbose = FALSE)
pred.rf2 = predict(rfModel2, testSet)
table(pred.rf2)
# results = cbind(testSet$TopPick, as.character(pred.rf2))
# colnames(results) = c('testSet', 'pred.rf')
# results = as.data.frame(results)
# intSet = results[results$testSet=='Top', ]
# accuracy = 100* c(sum(intSet$testSet == intSet$pred.rf)/NROW(intSet))

# myModel$votes <- NULL
# myModel$predicted <- NULL
# myModel$oob.times <- NULL
# myModel$y <- NULL


ldaModel = train(factor(TopPick) ~. - Date - Ticker , data = trainSet, method = 'lda')
pred.lda = predict(ldaModel, testSet)

qdaModel = train(factor(TopPick) ~. - Date - Ticker , data = trainSet, method = 'qda')
pred.qda = predict(qdaModel, testSet)

nnetModel = train(factor(TopPick) ~. - Date - Ticker, data = trainSet, method = 'nnet')
pred.nnet = predict(nnetModel, testSet)

svmModel = e1071::svm(factor(TopPick) ~ . - Date - Ticker, data = trainSet)
pred.svm = predict(svmModel, testSet)

svmModel.tuned = tune.svm(factor(TopPick) ~ . - Date - Ticker, data = trainSet)
pred.svm.tuned = predict(svmModel.tuned$best.model, testSet)

# ---- Time to validate ----
results = cbind(testSet$TopPick, as.character(pred.lda), as.character(pred.qda), 
                as.character(pred.nnet), as.character(pred.rf),
                as.character(pred.svm), as.character(pred.svm.tuned))
colnames(results) = c('testSet', 'pred.lda', 'pred.qda', 
                      'pred.nnet', 'pred.rf', 'pred.svm', 'pred.svm.tuned')
results = as.data.frame(results)

# we want only performance of Top 5 recommendation
intSet = results[results$testSet=='Top', ]
intSet[] <- lapply(intSet, as.character) # convert whole data.frame to char

# convert only the factor columns to char
# i = sapply(intSet, is.factor)
# intSet[i] = lapply(intSet[i], as.character)

accuracy = 100* c(sum(intSet$testSet == intSet$pred.lda)/NROW(intSet),
                  sum(intSet$testSet == intSet$pred.qda)/NROW(intSet),
                  sum(intSet$testSet == intSet$pred.nnet)/NROW(intSet),
                  sum(intSet$testSet == intSet$pred.rf)/NROW(intSet),
                  sum(intSet$testSet == intSet$pred.svm)/NROW(intSet),
                  sum(intSet$testSet == intSet$pred.svm.tuned)/NROW(intSet))
accuracy = as.data.frame(accuracy)
accuracy$model = c('lda', 'qda','nnet', 
                   'rf','svm', 'svm.tuned')

ggplot(accuracy, aes(x = model, y = accuracy)) + 
  geom_bar(stat = 'identity', fill = 'cyan4')


#---- Lead the data by 1 day ----
write.csv(nse.cnx100, file = 'cnx100.unadj.csv', row.names = F)
# manually Lead the rank class by 1 day
nse.cnx100 = read.csv('cnx100.unadj.csv', as.is = T)
nse.cnx100 = na.omit(nse.cnx100)
nse.cnx100$Date = as.POSIXct(strptime(nse.cnx100$Date, '%m/%d/%Y'))

