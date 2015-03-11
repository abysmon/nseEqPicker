require(randomForest)
require(caret)

trainSet = eqcnx100adj[1:24400, -10]
testSet = eqcnx100adj[24401:29000, -10]

rfModel = randomForest(x = trainSet[ ,-11], y = trainSet[ ,11])

ldaModel = train(factor(Top5) ~. - Date - Ticker , data = trainSet, method = 'lda')
pred.lda = predict(ldaModel, testSet)

glmModel = glm(factor(Top5) ~. - Date - Ticker , data = trainSet, family = binomial)
pred.glm = predict(glmModel, testSet)

ldaModel2 = train(factor(Top5) ~ DQShare + Returns + TradeSize, 
                  data = trainSet, method = 'lda')
pred.lda2 = predict(ldaModel2, testSet)

results = cbind(testSet$Top5, as.character(pred.lda), as.character(pred.lda2))
colnames(results) = c('testSet', 'pred.lda', 'pred.lda2')

