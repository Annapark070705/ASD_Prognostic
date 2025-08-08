library(jstable)
library(ggplot2)
library(data.table)
library(magrittr)
library(naniar)
library(gt)
library(patchwork)
library(tableone)
library(DataExplorer)
library(haven)

# Set working directory if needed
# setwd("/path/to/your/local/data")

## Read data from local files
# Option 1: If you have the raw SAS files locally
a <- rbind(
  haven::read_sas("nsch_2022e_screener.sas7bdat") %>% data.table,
  haven::read_sas("nsch_2021e_screener.sas7bdat") %>% data.table, 
  fill = TRUE
)

a1 <- rbind(
  haven::read_sas("nsch_2022e_topical.sas7bdat") %>% data.table,
  haven::read_sas("nsch_2021e_topical.sas7bdat") %>% data.table, 
  fill = TRUE
)

# Create list similar to what was in pins
alist <- list(screener = a, topical = a1)

# Option 2: If you already have the data saved as RDS file locally
# alist <- readRDS("autism_data.rds")

# Option 3: If you want to save the data for future use
# saveRDS(alist, "autism_data.rds")

### Coding book: https://www.childhealthdata.org/browse/survey, 2021-2022

# Extract topical data
topical <- alist$topical

# sapply(a, function(x){attr(x, "label")}) %>% unname %>% unlist
sapply(topical, function(x){attr(x, "label")}) %>% unname %>% unlist

# a1[, `:=`(ASD = as.integer(K2Q35A == 1),
#           BirthWT = BIRTHWT_OZ_S * 28.34952,
#           Smoke_anyone = as.integer(K9Q40 == 1),
#           Mentalproblem_parents = as.integer(A1_MENTHEALTH >= 4 & A2_MENTHEALTH >= 4))]

FPL_range <- function(FPL){
  sapply(FPL, function(x){
    if(x < 100){
      return( 0)
    }else if( x < 200){
      return(1)
    }else if(x < 400){
      return(2)
    }else{
      return(3)
    }
  })
  
  
  
}

topical[,`:=` (
  C_AGE = as.integer(SC_AGE_YEARS), # child age
  C_SEX = as.factor(ifelse(SC_SEX == 1, "M", "F")), # child sex 
  B_WT_G = BIRTHWT_OZ_S * 28.3495, # child birth weight 1 oz = 28.3495 g
  PRE_B = as.factor(ifelse(K2Q05 == 1, "Y", "N")), # Was this child born more than 3 weeks before their due date?
  EBF = as.factor(ifelse(K6Q40 == 1, "Y", "N")), # Ever Breast-fed / Caution! too much NAs
  MOMAGE = MOMAGE,
  M_EDU = HIGRADE_TVIS,
  SMOKE = ifelse(K9Q40 ==1, "Y", "N"), # Anyone in Household Use Cigarettes
  IN_SMOKE = K9Q41, # Anyone Smoke Inside of Home
  ACCESS_HS = ifelse(CURRINS == 1, "Y", "N"), # all information from INSTYPE applied to CURRINS
  ALLERGIES = ifelse(ALLERGIES == 1, "Y", "N"),
  FPL = (FPL_I1 + FPL_I2 + FPL_I3 + FPL_I4 + FPL_I5 + FPL_I6) / 6,
  ASD = ifelse(K2Q35A == 1, "Y", "N"),
  PMH = ifelse((A1_MENTHEALTH == 5)|(A2_MENTHEALTH == 5) | (ACE8 == 1), 1, 0)#parental mental health, 5 = poor, 
  # PPCOM = ifelse((BLOOD_SCREEN != 1) & (CYSTFIB_SCREEN != 1)&(GENETIC_SCREEN !=1 ) & (HEART_BORN != 1 ) &(FASD !=1) &(CONCUSSION !=1), 0,1)
  
  
  
  
  
)]

cols = c("BLOOD_SCREEN", "CYSTFIB", "GENETIC_SCREEN", "HEART_BORN", "FASD", "CONCUSSION")

tmp <- replace_na_with(topical[,..cols], 2) %>% .[,PPCOM := ifelse((BLOOD_SCREEN != 1) & (CYSTFIB != 1)&(GENETIC_SCREEN !=1 ) & (HEART_BORN != 1 ) &(FASD !=1) &(CONCUSSION !=1), 0,1)]
topical <- cbind(topical, tmp)

topical[,`:=`(
  FPL_CAT = as.factor(FPL_range(FPL))
  
)]

# target <- topical[,.(ASD, C_AGE, C_SEX, B_WT_G, PRE_B, EBF, MOMAGE, M_EDU, SMOKE, IN_SMOKE, ACCESS_HS, ALLERGIES, FPL, FPL_CAT )]

## Table 3
varlist <- list(
  Base = c("HHID","ASD", "C_AGE", "C_SEX", "B_WT_G", "PRE_B", "EBF", "MOMAGE", "M_EDU", "SMOKE", "ACCESS_HS", "ALLERGIES", "FPL_CAT", "PMH", "PPCOM")
)


#Lab(Glucose~Platelet) needed

out <- topical[, .SD, .SDcols = unlist(varlist)]





factor_vars <- names(out)[sapply(out, function(x){length(table(x))}) <= 6]
out[, (factor_vars) := lapply(.SD, factor), .SDcols = factor_vars]
conti_vars <- setdiff(names(out), c("HHID", factor_vars))
out[, (conti_vars) := lapply(.SD, as.numeric), .SDcols = conti_vars]

saveRDS(out,"autism.RDS")

#out$Subject <- factor(out$Subject)
out.label <- jstable::mk.lev(out)

# vars01 <- sapply(factor_vars, function(v){identical(levels(out[[v]]), c("0", "1"))})
# for (v in names(vars01)[vars01 == T]){
#   out.label[variable == v, val_label := c("No", "Yes")]
# }

# for (v in names(name.old)){
#   out.label[variable == v, var_label := name.old[v]]
# }
out.label[variable == "ASD", `:=`(var_label = "ASD", val_label = c("no ASD", "ASD"))]
out.label[variable == "C_AGE", `:=`(var_label = "Age")]
out.label[variable == "C_SEX", `:=`(var_label = "Sex", val_label = c("Female", "Male"))]
out.label[variable == "B_WT_G", `:=`(var_label = "Birth Weight (grams)")]
out.label[variable == "PRE_B", `:=`(var_label = "Prematurity", val_label = c("No", "Yes"))]
out.label[variable == "EBF", `:=`(var_label = "Ever Breast-fed", val_label = c("No", "Yes"))]
out.label[variable == "MOMAGE", `:=`(var_label = "Parental Age at Birth (years)")]
out.label[variable == "M_EDU", `:=`(var_label = "Maternal Education Level", val_label = c(
  "Less than high school",
  "High school (including vocational, trade, or business school)",
  "Some college or Associate Degree",
  "College degree or higher"
))]
out.label[variable == "SMOKE", `:=`(var_label = "Smoke", val_label = c("No", "Yes")) ] # No data for Maternal Smoking During Pregnancy
out.label[variable == "ACCESS_HS", `:=`(var_label = "Access to Healthcare Services", val_label = c("No", "Yes"))]
out.label[variable == "PMH", `:=`(var_label = 'Family History of Mental Disorders', val_label = c("No", "Yes"))]
out.label[variable == "ALLERGIES", `:=`(var_label = "Allergies history", val_label = c("No", "Yes"))]
out.label[variable == "FPL_CAT", `:=`(var_label = "Family Poverty Ratio", val_label = c("0-99%", "100-199%", "200-399%", "≥400%"))]
out.label[variable == "PPCOM", `:=`(var_label = "Prenatal/Perinatal Complications", val_label = c("No", "Yes"))]


# out.label[variable == "Sex", `:=`(var_label = "Sex", val_label = c("Male", "Female"))]
#out.label[variable == "HIGRADE", `:=`(var_label = "Parental Highest Education Level", val_label = c("Less than high school", "High school", "More than high school"))]
#out.label[variable == "ASD", `:=`(var_label = "ASD", val_label = c("no ASD", "ASD"))]

tmp <- CreateTableOneJS(vars = colnames(out)[-c(1,2)], strata = "ASD", data = out, Labels = T, labeldata = out.label, showAllLevels = T, smd = T)$table[, c(1:4,6)]
colnames(tmp)[1] <- "Levels"
library(flextable)
flextable(cbind(Variable = rownames(tmp), data.frame(tmp, check.names = F))) %>% saveRDS("table.RDS")

p1 <- ggplot(out) + geom_boxplot(aes(attr(),ASD))


tmp <- lapply(varlist$Base[-c(1,2)], function(var){
  out.dropna <- out[!is.na(ASD),]
  
  
  cls <- class(out.dropna[,..var] %>% unlist)
  p <- NULL
  
  if(cls == "numeric"){
    
    p <- ggplot(out.dropna) + geom_boxplot(aes(.data[[var]],ASD)) +
      coord_flip()
  }else if(cls == "factor"){
    p <- ggplot(out.dropna) + geom_bar(aes(ASD, fill = .data[[var]]), position = "dodge")
    
  }else{
    
  }
  
  return(p)
  
  
})

wrap_plots(tmp)


graph <- list()
vars <- varlist$Base[-c(1,2)]
n <- length(vars)
for(i in 2:n){
  for(j in 2:i-1){
    col_i <- vars[i]
    col_j <- vars[j]
    i_class <- class(out[,..col_i] %>% unlist)
    j_class <- class(out[,..col_j] %>% unlist)
    
    if((i_class == "factor")&(j_class == "factor")){
      p <- ggplot(out) + geom_bar(aes(.data[[col_i]], fill = .data[[col_j]]), position = "dodge")
      
    }else if(i_class == "numeric" & j_class == "numeric"){
      p <- ggplot(out) + geom_point(aes(.data[[col_i]],.data[[col_j]], fill = ASD, alpha = 0.1))
      
    }else if(i_class == "factor" & j_class == "numeric"){
      p <- ggplot(out) + geom_bar(aes(.data[[col_i]],.data[[col_j]], fill = ASD))
      
    }else if(i_class == "numeric" & j_class == "factor"){
      p <- ggplot(out) + geom_bar(aes(.data[[col_j]],.data[[col_i]], fill = ASD))
    }
    
    graph[[paste0(col_i, "_", col_j)]] <- p
  }
}

