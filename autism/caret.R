library(jstable);library(ggplot2);library(flextable);library(data.table);library(magrittr);library(RSBID);library(naniar);library(gt);library(patchwork); library(tableone); library(ggplot2); library(DataExplorer); library(caret); library(corrplot); library(FactoMineR); library(factoextra); library(smotefamily)
library(officer); 
library(rvg); library(ggcorrplot); library(dplyr); library(MLeval); library(pROC); library(ggpubr); library(cvms)
library(patchwork);library(ggimage); library(rsvg); library(gbm);library(flowchart)


analysis <- function(model, test){
  pr_model <- predict(model, test)
  pr_model_p <- predict(model, test_naomit, type = "prob")
  cm <- confusionMatrix(pr_model, test$ASD)
  post <- postResample(pr_model, test$ASD)
  roc <- roc(test$ASD, pr_model_p$Y)
  tmp <- cbind(test$ASD,pr_model,pr_model_p)
  colnames(tmp) <- c("obs", "pred", "N", "Y")
  tc_sum <- twoClassSummary(tmp, lev = levels(test$ASD))
  pr_sum <- prSummary(tmp, lev = levels(test$ASD))
  
  vi <- varImp(model, scale = T)
  
  
  result <- list()
  result$ML <- model$method
  result$pr_model <- pr_model
  result$pr_model_p <- pr_model_p
  result$cm <- cm
  result$post <- post
  result$roc <- roc
  result$tc_sum <- tc_sum
  result$pr_sum <- pr_sum
  result$cm_plot <- plot_confusion_matrix(as.data.frame(cm$table),
                                          target_col = "Reference",
                                          prediction_col = "Prediction",
                                          counts_col = "Freq",
                                          theme_fn = theme_classic2,
                                          add_sums = F,
                                          add_row_percentages = F,
                                          add_col_percentages = F,
                                          add_arrows = F
  ) + labs(title = model$method)
  result$vi <- vi
  
  
  result
  
  
}
theme_set(ggpubr::theme_classic2())
ppt <- read_pptx()
ppt1 <- read_pptx()
raw <- readRDS("autism.RDS")
raw <- raw[,-1]

raw_fc <- raw %>% as_fc(label = "2021/2022 National Survey of Children's Health (NSCH)")





t <- readRDS("table.RDS")
ppt1 %>% add_slide() %>% ph_with(
  t,
  location = ph_location_fullsize()
) %>% set_notes("The following table contains variables and significance according to Autism status.",
                location = notes_location_type("body"))

p <- plot_intro(raw, ggtheme = theme_classic2(), title = "")

ppt %>% add_slide() %>% ph_with(
  dml(ggobj = p),
  location = ph_location_fullsize()
) %>% set_notes("The following chart describes the composition of the dataset.",
                location = notes_location_type("body"))

t <- data.frame(
  Variable = names(raw),
  Description = c(
    "Autism ASD",
    "Age of Selected Child - In Years",
    "Sex of Selected Child",
    "Standardized Birth Weight, Grams",
    "Born 3 or More Weeks Before Due Date",
    "Ever Breast-fed",
    "Age of Mother - Years",
    "Highest Level of Education among Reported Adults",
    "Anyone in Household Use Cigarettes",
    "Health Insurance Coverage - Currently Covered",
    "Allergies",
    "Family Poverty Ratio",
    "Parental Mental Health",
    "Prenatal/Perinatal Complications"
  )
)

t <- flextable(t) %>% vline(j="Variable", border = fp_border_default()) %>% 
  width(j = "Description", width = 4)

ppt1 %>% add_slide() %>% ph_with(
  t,
  location = ph_location_fullsize()
) %>% set_notes("The following is a description of the variables in the dataset being used.",
                location = notes_location_type("body"))


p <- DataExplorer::plot_missing(raw, ggtheme = theme_classic2())

ppt %>% add_slide() %>% ph_with(
  p,
  location = ph_location_fullsize()
) %>% set_notes("Upon examination, the missing rate of PMH and EBF is considerably high. Therefore, we proceeded with the analysis after excluding the EBF variable.",
                location = notes_location_type("body"))


raw_rm <- raw[,-c("EBF")]
# create_report(raw, output_file = "./raw.html" , y = "ASD")
p <- DataExplorer::plot_missing(raw ,ggtheme = theme_classic2())

ppt %>% add_slide() %>% ph_with(
  p,
  location = ph_location_fullsize()
) %>% set_notes("PMH and EBF have been removed as follows.",
                location = notes_location_type("body"))

naomit <- na.omit(raw_rm)
p <- DataExplorer::plot_missing(naomit ,ggtheme = theme_classic2())
ppt %>% add_slide() %>% ph_with(
  p,
  location = ph_location_fullsize()
) %>% set_notes("This is the result after removing all rows containing missing values. No missing values are detected.",
                location = notes_location_type("body"))

p <- DataExplorer::plot_bar(naomit ,ggtheme = theme_classic2(), by = "ASD")
ppt %>% add_slide() %>% ph_with(
  p$page_1,
  location = ph_location_fullsize()
)%>% set_notes("The following shows the proportion of each categorical variable according to Autism status.",
               location = notes_location_type("body"))

p <- DataExplorer::plot_boxplot(naomit ,ggtheme = theme_classic2(), by = "ASD")
ppt %>% add_slide() %>% ph_with(
  p$page_1,
  location = ph_location_fullsize()
) %>% set_notes("The following shows box plots for each continuous variable according to Autism status.",
                location = notes_location_type("body"))

p <- DataExplorer::plot_density(naomit ,ggtheme = theme_classic2())
ppt %>% add_slide() %>% ph_with(
  p$page_1,
  location = ph_location_fullsize()
) %>% set_notes("The following is a density plot showing the distribution of continuous variables.",
                location = notes_location_type("body"))

p <- DataExplorer::plot_histogram(naomit ,ggtheme = theme_classic2(), geom_histogram_args = list(bins = 15) )

ppt %>% add_slide() %>% ph_with(
  p$page_1,
  location = ph_location_fullsize()
) %>% set_notes("The following is a histogram showing the distribution of continuous variables.",
                location = notes_location_type("body"))

# 
# p <- DataExplorer::plot_prcomp(naomit ,ggtheme = theme_classic2(), ncol = 3, nrow = 2)
# ppt %>% add_slide() %>% ph_with(
#   p$page_0,
#   location = ph_location_fullsize()
# )%>% add_slide() %>% ph_with(
#   p$page_1,
#   location = ph_location_fullsize()
# )%>% add_slide() %>% ph_with(
#   p$page_2,
#   location = ph_location_fullsize()
# ) %>% set_notes("The following are PCA results.",
#                 location = notes_location_type("body"))

# missforest <- missForest::missForest(raw)


# create_report(naomit, output_file = "./na_omit.html", y = "ASD")
#create_report(missforest, "missforest.html")




num <- colnames(naomit)[sapply(naomit, is.numeric)]
cat <- colnames(naomit)[sapply(naomit[,-c("ASD")], is.factor)]

p <- plot_correlation(naomit)
ppt1 %>% add_slide() %>% ph_with(
  dml(ggobj = p),
  location = ph_location_fullsize()
) %>% set_notes("The following is a visualization of the correlation matrix between all variables.",
                location = notes_location_type("body"))
# 
# p <- plot_correlation(naomit, type = "continuous")
# ppt %>% add_slide() %>% ph_with(
#   p,
#   location = ph_location_fullsize()
# ) %>% set_notes("The following is a visualization of the correlation matrix between continuous variables.",
#                 location = notes_location_type("body"))
# 
# 
# naomit.pca <- PCA(naomit[,..num], graph = FALSE)
# p<- fviz_pca_biplot(naomit.pca, label="var", habillage=naomit$ASD,
#              addEllipses=TRUE, ellipse.level=0.95, select.ind = list(cos2 = 700),col.var="red")
# 
# ppt %>% add_slide() %>% ph_with(
#   p,
#   location = ph_location_fullsize()
# ) %>% set_notes("The following shows PCA results expressed as a correlation circle plot.",
#                 location = notes_location_type("body"))
# 
naomit <- na.omit(raw_rm)
naomit <- as.data.table(predict(preProcess(naomit, method = c("center", "scale")), naomit))




#naomit_dmy[,ASD := as.factor(ASD)]

# naomit <- naomit_dmy

idx <- createDataPartition(naomit$ASD, p = 0.7, list = F)
train_naomit <- naomit[idx,]
test_naomit <- naomit[-idx,]




train_naomit = SMOTE_NC(train_naomit, "ASD" )




test_naomit <- data.frame(ASD = test_naomit$ASD, model.matrix(ASD ~., data = test_naomit)[, -1])
train_naomit <- data.frame(ASD = train_naomit$ASD, model.matrix(ASD ~., data = train_naomit)[, -1])

# normal <- preProcess(train_naomit, method = c("center", "scale"))
# train_naomit <- predict(normal, train_naomit)
# train_naomit <- as.data.table(train_naomit)

# cols <- names(train_naomit)[sapply(train_naomit, is.character)]
# train_naomit[,names(.SD) := lapply(.SD, factor), .SDcols = cols]


# test_naomit <- as.data.table(predict(preProcess(test_naomit, method = c("center", "scale")), test_naomit))
# setDT(test_naomit)

saveRDS(train_naomit, "train.RDS")
saveRDS(test_naomit, "test.RDS")
train_naomit <- readRDS("train.RDS")
test_naomit <- readRDS("test.RDS")


# fit.control <- trainControl(method = "cv", number = 10, classProbs = T, summaryFunction = twoClassSummary, allowParallel = T)
# 
# print("LR")
# LR <- train(ASD ~ . , data = train_naomit, method = "glm", 
#             family = "binomial", trControl = fit.control, metric = "ROC")
# 
# 
# print("RF")
# 
# RF <- train(ASD ~ ., data = train_naomit, method = "rf", trControl = fit.control, metric = "ROC")
# 
# 
# 
# print("SVM")
# 
# SVM <- train(ASD ~ . , data = train_naomit, method = "svmRadial", trControl = fit.control, metric = "ROC")
# 
# print("GBM")
# 
# GBM <- train(ASD ~ ., data = train_naomit,
#              method = "gbm",
#              metric = "ROC",
#              verbose = FALSE,                    
#              trControl = fit.control)
# 
# print("NN")
# 
# NN <- train(ASD ~ ., data = train_naomit,
#             method = "nnet",
#             metric = "ROC",
#             verbose = FALSE,                    
#             trControl = fit.control,
# )
# saveRDS(ML_result, "ML_result.RDS")
# 
# 
# ML_result <- list(LR, RF,SVM, GBM, NN)

ML_result <- readRDS("ML_result.RDS")


name <- c("LR", "RF", "SVM", "GBM", "NN")
# cols <- names(test_naomit)[sapply(test_naomit, is.character)]
# test_naomit[,names(.SD) := lapply(.SD, factor), .SDcols = cols]

result <- lapply(ML_result, function(x){analysis(x, test_naomit)})
#ML_result <- list(LR, GBM)
roc_list <- lapply(result, function(x){
  x$roc
})
names(roc_list) <- c("Logistic Regression",
                     "Random Forest",
                     "Support Vector Machine",
                     "Gradient Boosting Machines",
                     "Neural Network")

p <- ggroc(roc_list) + labs(color = "Algorithm") + geom_abline(intercept = 1, slope = 1, linetype = "dashed", )
p

ppt1 %>% add_slide() %>% ph_with(
  dml(ggobj = p),
  location = ph_location_fullsize()
) %>% set_notes("The following shows ROC curves derived from applying various machine learning algorithms.",
                location = notes_location_type("body"))


rank <- lapply(result, function(x){
  x$vi$importance %>% as.data.frame() %>% .[[1]] %>% unlist
  
  
}

)

rank <- as.data.frame(rank)


label <-  c("Logistic Regression",
            "Random Forest",
            "Support Vector Machine",
            "Gradient Boosting Machines",
            "Neural Network")
names(rank) <- label

rank$Variables <- rownames(result[[1]]$vi$importance)


rank <- rank[c(length((rank)), 1:(length(rank)-1))]

rank$Variables <- c(
  "Child Age",
  "Child Sex : Male",
  "Birth Weight",
  "Preterm Birth",
  "Parental Age at Birth",
  "Maternal Education Level : High school",
  "Maternal Education Level : Some college or Associate Degree",
  "Maternal Education Level : College degree or higher",
  "Household Smoking",
  "Access to Healthcare Services",
  "Allergies",
  "Family Poverty Ratio : 0-99%",
  "Family Poverty Ratio : 100-199%",
  "Family Poverty Ratio : 200-399%",
  "Parental Mental Health",
  "Prenatal/Perinatal Complications"
)
t <- flextable(rank)
ppt1 %>% add_slide() %>% ph_with(
  t,
  location = ph_location_fullsize()
)


vi_plot <- lapply(label, function(ml){
  tmp <- rank[,c("Variables", ml)]
  names(tmp) <- c("v", "w")
  
  p <- ggplot(tmp) +
    geom_bar(aes(x = reorder(v,w), y = w),  stat="identity", position="dodge") +
    coord_flip() +
    xlab("Variable Importance(%)") +
    ylab("Variables") +
    labs(title = ml)
  
  ppt1 %>% add_slide() %>% ph_with(
    dml(ggobj = p),
    location = ph_location_fullsize()
  )
  
})





eval_met <- do.call(rbind,(lapply(result, function(x){
  tmp <- c(x$post , x$tc_sum , x$pr_sum)
  as.data.frame(t(tmp))
  tmp$ML <- x$ML
  
  tmp <- as.data.table(tmp)
  cn <- c("ML", names(tmp)[-length(tmp)])
  
  setcolorder(tmp, cn)
  print(class(tmp))
  tmp
  
  
})))

eval_met <- as.data.frame(eval_met)
eval_met$ML <- c("Logistic Regression",
                 "Random Forest",
                 "Support Vector Machine",
                 "Gradient Boosting Machines",
                 "Neural Network")
t <- flextable(eval_met)
t <- vline(t, j = "ML", border = fp_border_default())
t <- colformat_double( t, digits = 3)
t <- align(t, align = "center", part = "all")


ppt1 %>% add_slide() %>% ph_with(
  t,
  location = ph_location_fullsize()
) %>% set_notes("The following shows various metrics derived from machine learning results.",
                location = notes_location_type("body"))


cm_list <- lapply(result, function(x){
  x$cm_plot
})

names(cm_list) <- c("Logistic Regression",
                    "Random Forest",
                    "Support Vector Machine",
                    "Gradient Boosting Machines",
                    "Neural Network")

cm_list <- lapply(names(cm_list), function(x){
  cm_list[[x]] + labs(title = x)
})


p <- wrap_plots(cm_list)

ppt1 %>% add_slide() %>% ph_with(
  dml(ggobj = p),
  location = ph_location_fullsize()
) %>% set_notes("The following shows confusion matrices from machine learning results.",
                location = notes_location_type("body"))

tmp <- raw_fc %>% 
  fc_filter(N = nrow(naomit), label = "Study Population",label_exc = "Missing Values", show_exc = T) %>% 
  fc_split(N = c(nrow(naomit[idx,]), nrow(naomit[-idx,])), label = c("Train", "Test")) %>% 
  fc_split(N = c(nrow(naomit[idx,][ASD == "Y"]), nrow(naomit[idx,][ASD == "N"]), nrow(naomit[-idx,][ASD == "Y"]), nrow(naomit[-idx,][ASD == "N"])),
           label = c("ASD", "no ASD")) %>% 
  fc_draw %>% 
  dml

ppt1 %>% add_slide() %>% 
  ph_with(
    tmp,
    location = ph_location_fullsize()
  )

print(ppt1, "result.pptx")



