library(jstable);library(ggplot2);library(flextable);library(data.table);library(magrittr);library(RSBID);library(naniar);library(gt);library(patchwork); library(tableone); library(ggplot2); library(DataExplorer); library(caret); library(corrplot); library(FactoMineR); library(factoextra); library(smotefamily)
library(officer); 
library(rvg); library(ggcorrplot); library(dplyr); library(MLeval); library(pROC); library(ggpubr)
library(doParallel)
cl <- makePSOCKcluster(detectCores() - 1)
registerDoParallel(cl)


fit.control <- trainControl(method = "repeatedcv", number = 10, repeats = 3, classProbs = T, summaryFunction = twoClassSummary, allowParallel = T)

train_naomit <- readRDS("train.RDS")

#train_naomit <- train_naomit[1:1000,]

make.names(names(train_naomit))


print("LR")
LR <- train(ASD ~ ., data = train_naomit, method = "glm", 
            family = "binomial", trControl = fit.control, metric = "ROC")


print("RF")

RF <- train(ASD ~ ., data = train_naomit, method = "rf", trControl = fit.control, metric = "ROC")



print("SVM")

SVM <- train(ASD ~ . , data = train_naomit, method = "svmRadial", trControl = fit.control, metric = "ROC")

print("GBM")

GBM <- train(ASD ~ ., data = train_naomit,
             method = "gbm",
             metric = "ROC",
             verbose = FALSE,                    
             trControl = fit.control)

print("NN")

NN <- train(ASD ~ ., data = train_naomit,
            method = "nnet",
            metric = "ROC",
            verbose = FALSE,                    
            trControl = fit.control,
)



ML_result <- list(LR, RF,SVM, GBM, NN)

saveRDS(ML_result, "ML_result.RDS")
stopCluster(cl)

