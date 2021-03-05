library(randomForest)

data  <- read.table("data4clustering.txt")
data  <- read.table("data4clusteringCORRELATED.txt")b

##ejemplo:
ind = sample(2, nrow(iris), replace=TRUE, prob=c(0.7,0.3))
trainData = iris[ind==1,]
testData = iris[ind==2,]

iris_rf = randomForest(Species~., data=trainData, ntree=100, proximity=T)
table(predict(iris_rf), trainData$Species))

importance(iris_rf)

irisPred = predict(iris_rf, newdata=testData)
table(irisPred, testData$Species)

plot(margin(iris_rf, testData$Species))


CM = table(irisPred, testData$Species)
accuracy = (sum(diag(CM)))/sum(CM)

## con los datos:

datos  <- data[which(!is.na(data$Mean)),c(1,4,13,22,24,25,26)]
datos  <-  datos[,3:7]

