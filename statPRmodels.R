# ---- Load the library and split data for cross-validation ----
require(e1071)
require(randomForest)
require(caret)
require(tidyr)

trainSet = eqcnx100adj[1:26500, -10]
testSet = eqcnx100adj[26501:29000, -10]

# ---- Little EDA before the plunge ----
summary(eqcnx100adj)
ggplot(eqcnx100adj, aes(x = 1, y = VWAP, col = TopPick)) + geom_boxplot()
ggplot(eqcnx100adj, aes(x = 1, y = DQShare, col = TopPick)) + geom_boxplot()
ggplot(eqcnx100adj, aes(x = 1, y = TradeSize, col = TopPick)) + geom_boxplot()
ggplot(eqcnx100adj, aes(x = 1, y = VWAPReturn, col = TopPick)) + geom_boxplot()
ggplot(eqcnx100adj, aes(x = 1, y = Returns, col = TopPick)) + geom_boxplot()


x = eqcnx100adj[,-c(3:5)] %>%
  #select(DQShare:VWAPReturn, TopPick) %>%
  gather(Features, Values, -c(TopPick, Date, Ticker))

ggplot(x, aes(x = Features, y = Values, col = TopPick)) + 
  geom_boxplot() + facet_wrap( ~ TopPick) #+ facet_grid(TopPick ~ .)


# ---- Fit classifiers, as many as possible ----
rfModel = randomForest(x = trainSet[ ,-10], y = trainSet[ ,10])
pred.rf = predict(rfModel, testSet)

ldaModel = train(factor(TopPick) ~. - Date - Ticker , data = trainSet, method = 'lda')
pred.lda = predict(ldaModel, testSet)


glmModel = glm(factor(TopPick) ~. - Date - Ticker , data = trainSet, family = binomial)
pred.glm = predict(glmModel, testSet)


ldaModel2 = train(factor(TopPick) ~ DQShare + Returns + TradeSize, 
                  data = trainSet, method = 'lda')
pred.lda2 = predict(ldaModel2, testSet)


qdaModel = train(factor(TopPick) ~. - Date - Ticker , data = trainSet, method = 'qda')
pred.qda = predict(qdaModel, testSet)


# svmModel = train(factor(TopPick) ~. - Date - Ticker , data = trainSet, method = 'lssvmPoly')
# pred.svm = predict(svmModel, testSet)
# 
# 
# svmModel2 = train(factor(TopPick) ~. - Date - Ticker , data = trainSet, method = 'svmRadial')
# pred.svm2 = predict(svmModel2, testSet)
# 
# 
# svmModel = kernlab::lssvm(factor(TopPick) ~ .,data=trainSet)
# pred.svm = predict(svmModel, testSet)


svmModel = e1071::svm(factor(TopPick) ~ . - Date - Ticker, data = trainSet)
pred.svm = predict(svmModel, testSet)

x = tune.svm(factor(TopPick) ~ . - Date - Ticker, data = trainSet)
pred.svm.x = predict(x$best.model, testSet)

nnetModel = train(factor(TopPick) ~. - Date - Ticker, data = trainSet, method = 'nnet')
pred.nnet = predict(nnetModel, testSet)

nnetModel2 = nnet(factor(TopPick) ~ DQShare + Returns + TradeSize, size = 50, maxit = 200, data = trainSet)
pred.nnet2 = predict(nnetModel2, testSet)
sum(pred.nnet2>0.3)


# ---- Visualise the fitted models ----
plot(ldaModel)
plot(ldaModel2)
plot(qdaModel)
plot(glmModel)
plot(svmModel)
plot(nnetModel)
plot(nnetModel2)


# ---- Time to validate ----
results = cbind(testSet$TopPick, as.character(pred.lda), as.character(pred.lda2),
                as.character(pred.qda), as.character(pred.nnet), 
                as.character(pred.svm), as.character(pred.svm.x))
colnames(results) = c('testSet', 'pred.lda', 'pred.lda2', 'pred.qda', 
                      'pred.nnet', 'pred.svm', 'pred.svm.tuned')
results = as.data.frame(results)

# we want only performance of Top 5 recommendation
intSet = results[results$testSet=='Top', ]
accuracy = 100* c(sum(intSet$testSet == intSet$pred.lda)/NROW(intSet),
             sum(intSet$testSet == intSet$pred.lda2)/NROW(intSet),
             sum(intSet$testSet == intSet$pred.qda)/NROW(intSet),
             sum(intSet$testSet == intSet$pred.nnet)/NROW(intSet),
             sum(intSet$testSet == intSet$pred.svm)/NROW(intSet),
             sum(intSet$testSet == intSet$pred.svm.tuned)/NROW(intSet))
accuracy = as.data.frame(accuracy)
accuracy$model = c('lda', 'lda2', 'qda', 'nnet', 'svm', 'svm.tuned')

ggplot(accuracy, aes(x = model, y = accuracy)) + 
  geom_bar(stat = 'identity', fill = 'cyan4')

