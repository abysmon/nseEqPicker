# following from 'data.loading.R'
# ---- Load the library and split data for cross-validation ----
require(caret)
require(nnet)
require(randomForest)
require(e1071)

cutoff = which(nse.cnx500$Date == '2015-01-15')[1]
trainSet = nse.cnx500[1:(cutoff-1), -17]
testSet = nse.cnx500[cutoff:NROW(nse.cnx500), -17]

# ---- Fit classifiers, as many as possible ----
rfCtrl = trainControl(method = "adaptive_cv", repeats = 3, verboseIter = T)
rfModel = train(factor(TopPick) ~. - Date - Ticker, data = trainSet, 
                 trControl = rfCtrl, verbose = FALSE)
pred.rf = predict(rfModel, testSet)

rf.simple = randomForest(factor(TopPick) ~. - Date - Ticker, data = trainSet)
pred.rf.simple = predict(rf.simple, testSet)

nnet.simple = nnet(factor(TopPick) ~. - Date - Ticker, data = trainSet, size = 30)
pred.nnet.simple = predict(nnet.simple, testSet)

nnetModel = train(factor(TopPick) ~. - Date - Ticker, data = trainSet, method = 'nnet')
pred.nnet = predict(nnetModel, testSet)

nnCtrl = trainControl(method = "adaptive_cv", repeats = 5, verboseIter = F)
nnetModel2 = train(factor(TopPick) ~. - Date - Ticker, data = trainSet, 
                   method = 'nnet', trControl = nnCtrl, verbose = FALSE)
pred.nnet2 = predict(nnetModel2, testSet)


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
