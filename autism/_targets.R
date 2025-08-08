library(targets)
library(tarchetypes)
library(tibble)
library(jstable);library(ggplot2);library(data.table);library(magrittr);library(naniar);library(gt);library(patchwork); library(tableone); library(ggplot2); library(DataExplorer); library(caret)
library(fastDummies); library(randomForest); library(e1071); library(gbm); 
source("R/functions.R")

raw_file_t <- tar_target(file, "autism.csv", format = "file")
read_file_t <- tar_target(raw_data, READ_FILE(file))
clean_data_method_t <- tar_target(clean_data_method, c("NA_OMIT"))
clean_data_t <- tar_target(
  cleaned_data,
  match.fun(clean_data_method)(raw_data),
  pattern = map(clean_data_method)
)
add_dummy_t <- tar_target(
  preprocessed_data,
  ADD_DUMMY(cleaned_data),
  pattern = map(cleaned_data),
  iteration = "list"
)
feature_selection_method_t <- tar_target(feature_selection_method, c("RFE"))
select_feature_t <- tar_target(
  feature_selected_data,
  match.fun(feature_selection_method)(preprocessed_data),
  pattern = cross(feature_selection_method, preprocessed_data),
  iteration = "list"
)
split_test_train_t <- tar_target(
  splited_data,
  SPLIT_TEST_TRAIN(feature_selected_data),
  pattern = map(feature_selected_data),
  iteration = "list"
)

filter_test_t <- tar_target(
  test_data,
  FILTER_TEST(splited_data),
  pattern = map(splited_data),
  iteration = "list"
)

filter_train_t <- tar_target(
  train_data,
  FILTER_TRAIN(splited_data),
  pattern = map(splited_data),
  iteration = "list"
)

ML_method_t <- tar_target(ML_method, c("LR", "RF", "SVM", "GBM", "NN"))
ML_train_t <- tar_target(
  trained_model,
  match.fun(paste0("TRAIN_",ML_method))(train_data),
  pattern = cross(ML_method, train_data),
  iteration = "list"
)

ML_test_t <- tar_target(
  model_test_result,
  match.fun(paste0("TEST_",ML_method))(train_data),
  pattern = map(trained_model, test_data),
  iteration = "list"
)




list(
  raw_file_t,
  read_file_t,
  clean_data_method_t,
  clean_data_t,
  add_dummy_t,
  feature_selection_method_t,
  select_feature_t,
  split_test_train_t,
  filter_train_t,
  filter_test_t,
  ML_method_t,
  ML_train_t
  
)

