dados <- iris[sample(nrow(iris)),]

# modelo 1
train <- dados[1:120,]
test <- dados[121:150,]

model_1 <- rpart(Species ~ ., data=train, parms=list(split='gini'))

rpart.plot(model_1, extra=101)

# modelo 2
dados <- iris[sample(nrow(iris)),]
train <- dados[1:120,]
test <- dados[121:150,]

model_2 <- rpart(Species ~ ., data=train, parms=list(split='gini'))

# modelo 3
dados <- iris[sample(nrow(iris)),]
train <- dados[1:120,]
test <- dados[121:150,]

model_3 <- rpart(Species ~ ., data=train, parms=list(split='gini'))

# modelo 4
dados <- iris[sample(nrow(iris)),]
train <- dados[1:120,]
test <- dados[121:150,]

model_4 <- rpart(Species ~ ., data=train, parms=list(split='gini'))

# ploting trees
par(mfrow=c(2,2))
rpart.plot(model_1, extra=101)
rpart.plot(model_2, extra=101)
rpart.plot(model_3, extra=101)
rpart.plot(model_4, extra=101)

########## RANDOM FOREST

dados <- iris[sample(nrow(iris)),]
train <- dados[1:120,]
test <- dados[121:150,]

# Modelo 5
train1 <- train[sample(1:120, size=120, replace=T),]

model_5 <- rpart(Species ~., data=train1, parms=list(split='gini'))

# Modelo 6
train2 <- train[sample(1:120, size=120, replace=T),]

model_6 <- rpart(Species ~., data=train2, parms=list(split='gini'))

# Modelo 7
train3 <- train[sample(1:120, size=120, replace=T),]

model_7 <- rpart(Species ~., data=train3, parms=list(split='gini'))

# Modelo 8
train4 <- train[sample(1:120, size=120, replace=T),]

model_8 <- rpart(Species ~., data=train4, parms=list(split='gini'))


par(mfrow=c(2,2))
rpart.plot(model_5, extra=101)
rpart.plot(model_6, extra=101)
rpart.plot(model_7, extra=101)
rpart.plot(model_8, extra=101)


### randomforest

model_rf <- randomForest(Species~., data=train, ntree=500)
model_rf

previsao_iris <- predict(model_rf, newdata=test)
cm = table(test$Species, previsao_iris)
cm

mean(previsao_iris == test$Species)
