READ_FILE <- function(file){
  tmp <- readRDS("autism.RDS")
  tmp <- tmp[,-1]
  # tmp <- head(tmp, 1000) # for testing
  
  tmp
}

NA_OMIT <- function(data){
  attr(data,"cleaning_method") <- "NA omit"
  data <- na.omit(data)
  data
}

MICE <- function(data){
  attr(data,"cleaning_method") <- "MICE"
  data
}

MISSFOREST <- function(data){
  attr(data,"cleaning_method") <- "missforest"
  data
}

ADD_DUMMY <- function(data){
  attr(data,"dummy") <- T
  data <- dummy_cols(data,
                     select_columns = colnames(data)[sapply(data, is.factor)],
                     remove_selected_columns = T,
                     remove_first_dummy = T)
  setnames(data, "ASD_Y", "ASD")
  data
}

RFE <- function(data){
  return(data)
  attr(data,"feature_selection_method") <- "RFE"
  rctrl1 <- rfeControl(method = "cv",
                       number = 3,
                       returnResamp = "all",
                       functions = caretFuncs,
                       saveDetails = TRUE)
  
  model <- rfe(ASD ~ ., data = data,
               rfeControl = rctrl1)
  cols <- c("ASD", model$optVariables)
  
  data[,..cols]
}

LASSO <- function(data){
  attr(data,"feature_selection_method") <- "LASSO"
  data
}

SPLIT_TEST_TRAIN <- function(data){
  data[,ASD := as.factor(ASD)]
  idx_train = createDataPartition(data[[1]], p = 0.7, list = F)
  list(
    train = data[idx_train,],
    test = data[-idx_train]
       )
  
  
  
}

FILTER_TRAIN <- function(data_list){
  tmp <- data_list$train
  attr(tmp, "dataset") <- "train"
  tmp
}

FILTER_TEST <- function(data_list){
  tmp <- data_list$test
  attr(tmp, "dataset") <- "test"
  tmp
}
 
TRAIN_LR <- function(data){
  attr(data, "ML") <- "LR"
  
  print(make.names(colnames(data)))
  
  print(data)
  
  trControl <- trainControl(method = 'repeatedcv',
                            number = 5,
                            repeats =  5,
                            search = 'random')
  
  m <- train(ASD ~ .,
                    data = data,
                    method = 'glmnet',
                    trControl = trControl,
                    family = 'binomial' )
  
  attr(m, "dataset") <- data
  m
}

TRAIN_RF <- function(data){
  attr(data, "ML") <- "RF"
  
  m <- randomForest(ASD ~., data = data)
  
  attr(m, "dataset") <- data
  m
}

TRAIN_SVM <- function(data){
  attr(data, "ML") <- "SVM"
  
  m <- svm(formula = ASD ~ ., 
           data = data, 
           type = 'C-classification', 
           kernel = 'linear') 
  
  attr(m, "dataset") <- data
  m
}

TRAIN_GBM <- function(data){
  attr(data, "ML") <- "GBM"
  data[,ASD := as.numeric(ASD)][,ASD := ASD -1]
  
  print(data$ASD %>% unique)
  
  m <- gbm(ASD ~. , data = data)
  
  attr(m, "dataset") <- data
  m
  

}

TRAIN_NN <- function(data){
  attr(data, "ML") <- "NN"
  
  m <- train(ASD ~ ., data = data, method = 'nnet' )
  
  attr(m, "dataset") <- data
  m
}

TEST_LR <- function(model, data){
  probs <- predict(model, data, type = "response")
  pred <- ifelse(probs > 0.5, 1, 0)
  pred
}

TEST_RF <- function(model, data){
  
}

TEST_SVM <- function(model, data){
  
}

TEST_GBM <- function(model, data){
  
}

TEST_NN <- function(model, data){
  
}