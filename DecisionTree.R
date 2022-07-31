dados <- iris[sample(nrow(iris)),]

n <- nrow(iris)*0.8

train <- dados[1:n,]
test <- dados[(n+1):nrow(iris),]

table(train$Species)

versicolor <- train[train$Species == 'versicolor',]
virginica <- train[train$Species == 'virginica',]
setosa <- train[train$Species == 'setosa',]


par(mfrow = c(2,2))
boxplot(versicolor$Sepal.Length, virginica$Sepal.Length, setosa$Sepal.Length, main='Sepal Length')
boxplot(versicolor$Petal.Length, virginica$Petal.Length, setosa$Petal.Length, main='Petal Length')
boxplot(versicolor$Sepal.Width, virginica$Sepal.Width, setosa$Sepal.Width, main='Sepal Width')
boxplot(versicolor$Petal.Width, virginica$Petal.Width, setosa$Petal.Width, main='Petal Width')

par(mfrow = c(1,1))
plot(train$Petal.Length, train$Petal.Width, type='n')
points(versicolor$Petal.Length, versicolor$Petal.Width, col='red', pch=16)
points(virginica$Petal.Length, virginica$Petal.Width, col='blue', pch=16)
points(setosa$Petal.Length, setosa$Petal.Width, col='green', pch=16)


max(versicolor$Petal.Width)

virginica[virginica$Petal.Width == min(virginica$Petal.Width),]


arvore_decisao <- function(arr){
  if(arr[3] < 2.5){
    return('setosa')
  }
  if(arr[4] <= 0.7){
    return('setosa')
  }
  if(arr[4] >= 1.7){
    return('virginica')
  }
  if(arr[3] >= 5.0){
    return('virginica')
  }
  else{
    return('versicolor')
  }
  
}

for (i in 1:nrow(test)){
  test[i,6] <- arvore_decisao(test[i,1:4])
}

for (i in 1:nrow(train)){
  train[i,6] <- arvore_decisao(train[i,1:4])
}

test_accuracy <- mean(test$Species == test$V6)
train_accuracy <- mean(train$Species == train$V6)

matriz_confusao <- table(test$Species, test$V6)


arvore_iris <- rpart(formula = Species ~ Petal.Length + Petal.Width,
                     data = train,
                     parms = list(split = 'gini'))

rpart.plot(arvore_iris, extra=101)

predict(arvore_iris, newdata = test[,1:4],
        type='class')

mean(predict(arvore_iris, newdata = test[,1:4],
             type='class') == test$Species)

table(predict(arvore_iris, newdata = test[,1:4],
              type='class'), test$Species)
