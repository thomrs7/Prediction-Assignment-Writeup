library(data.table)
library(plyr)
library(ggplot2)
library(dplyr)
library(lubridate)
library(reshape)
library(reshape2)
library(ggplot2)
library(caret)
library(corrplot)
set.seed(1234)

#Load Data
training <- read.csv("pml-training.csv", 
                   header=T,  na.strings = c("", "NA"))

#Get Rid of null data
data_names <- names(training[,colSums(is.na(training)) == 0])
train_data <- training[,c(data_names,"classe")]
#Get rid of . . .
train_data <- train_data[8:60]


# Training/Test set
inTrain = createDataPartition(train_data$classe, p = 0.6, list = F)
train = train_data[inTrain,]
train_test = train_data[-inTrain,]

outcome = which(names(train) == "classe")

# Viz
train_melt <- melt(train[,-outcome])

ggplot(train_melt,aes(x = value)) + 
    facet_wrap(~variable,scales = "free_x") + 
    geom_histogram(color="blue")

c <- cor(train[,-outcome])
corrplot(c, order = "FPC", method = "color", type = "lower", tl.cex = 0.8, 
         tl.col = rgb(0, 0, 0))


# Featrue Selection
highCorrCols = findCorrelation(abs(cor(train[,-outcome])),0.75)
highCorrFeatures = names(train)[highCorrCols]
train = train[,-highCorrCols]


#Train
ctrlKNN = trainControl(method = "repeatedcv")
modelKNN = train(classe ~ ., train, method = "knn", 
                 trControl = ctrlKNN,
                 preProcess=c("center","scale"))

ctrlRF = trainControl(method = "oob")
modelRF = train(classe ~ ., train, method = "rf",
                trControl = ctrlRF,
                ntree = 200,
                preProcess=c("center","scale"))

fitKNN = predict(modelKNN, train_test)
fitRF = predict(modelRF, train_test)

train_test$right <- fitKNN == train_test$classe
qplot(train_test$roll_belt, train_test$roll_arm, colour=train_test$right, data=train_test,
      main=" KNN Errors")

train_test$right <- fitRF == train_test$classe
qplot(train_test$roll_belt, train_test$roll_arm, colour=train_test$right, data=train_test,
      main=" Random Forest Errors")

matrixKNN = confusionMatrix(fitKNN, train_test$classe)
matrixRF = confusionMatrix(fitRF, train_test$classe)



testing <- training <- read.csv("pml-testing.csv", 
                                header=T,  na.strings = c("", "NA"))


test_results$right <- 


test_results <- predict(modelRF, testing)


data.frame(test_results)

pml_write_files = function(x){
    n = length(x)
    for(i in 1:n){
        filename = paste0("problem_id_",i,".txt")
        write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
    }
}

pml_write_files(test_results)

