# ---- Load the library and split data for cross-validation ----
require(randomForest)
require(caret)

trainSet = eqcnx100adj[1:24400, -10]
testSet = eqcnx100adj[24401:29000, -10]


# ---- Fit classifiers, as many as possible ----
rfModel = randomForest(x = trainSet[ ,-10], y = trainSet[ ,10])
pred.rf = predict(rfModel, testSet)

ldaModel = train(factor(Top5) ~. - Date - Ticker , data = trainSet, method = 'lda')
pred.lda = predict(ldaModel, testSet)


glmModel = glm(factor(Top5) ~. - Date - Ticker , data = trainSet, family = binomial)
pred.glm = predict(glmModel, testSet)


ldaModel2 = train(factor(Top5) ~ DQShare + Returns + TradeSize, 
                  data = trainSet, method = 'lda')
pred.lda2 = predict(ldaModel2, testSet)


qdaModel = train(factor(Top5) ~. - Date - Ticker , data = trainSet, method = 'qda')
pred.qda = predict(qdaModel, testSet)


svmModel = train(factor(Top5) ~. - Date - Ticker , data = trainSet, method = 'lssvmPoly')
pred.svm = predict(svmModel, testSet)


svmModel2 = train(factor(Top5) ~. - Date - Ticker , data = trainSet, method = 'svmRadial')
pred.svm2 = predict(svmModel2, testSet)


nnetModel = train(factor(Top5) ~. - Date - Ticker , data = trainSet, method = 'nnet')
pred.nnet = predict(nnetModel, testSet)


# ---- Time to validate ----
results = cbind(testSet$Top5, as.character(pred.lda), as.character(pred.lda2),
                as.character(pred.qda), as.character(pred.nnet))
colnames(results) = c('testSet', 'pred.lda', 'pred.lda2', 'pred.qda', 'pred.nnet')
results = as.data.frame(results)

# we want only performance of Top 5 recommendation
intSet = results[results$testSet=='Top5', ]
sum(intSet$testSet == intSet$pred.lda)/NROW(intSet)
sum(intSet$testSet == intSet$pred.lda2)/NROW(intSet)
sum(intSet$testSet == intSet$pred.qda)/NROW(intSet)
sum(intSet$testSet == intSet$pred.nnet)/NROW(intSet)


