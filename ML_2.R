rm(list=ls())
ESS <- read.csv("C:/Users/Gebruiker/Documents/DataScience/Social/ESS9e03_1.csv")

library(tidyverse)
library(dplyr)
library(labelled)
library(openxlsx)
library(doParallel)
library(mice)
library(ggplot2)
library(caret)
library(Metrics)
library(randomForest)
library(gbm)
library(imbalance)
library(ROSE)
library(e1071)

num_cores <- detectCores()
registerDoParallel(cores = num_cores)

###############################################
####inspection of the data#####################
###############################################

# Show frequency table for all variables in ESS
for (col in names(ESS)) {
  print(paste("Frequency table for", col))
  print(summary(ESS[[col]]))
}

###################################################################
###selecting observations & removing irrelevant columns############
###################################################################

#1.we select observations from Germany 
#2.we filter out columns that are: 
#####irrelevant (e.g. no values for Germans, single-valued, missing values) 
#####contain too many unique values (>53) for RF imputing 

ESS <- ESS %>%
  filter(cntry == "DE") %>%
  select(-c(1:4, 7:13)) %>%
  select(-starts_with("prtv") | starts_with("prtvede1") | starts_with("prtvede2"))%>%
  select(-starts_with("prtcl") | starts_with("prtclede"))%>%
  select(-starts_with("rlgd") | starts_with(c("rlgdnade", "rlgdeade")))%>%
  select(-starts_with("marstgb"))%>%
  select(-starts_with("edlv"))%>%
  select(-starts_with("edage"))%>%
  select(-c("rshpsgb", "educgb1", "edupcgb1", "edupbgb2",
            "domain", "vteumbgb", "vteubcmb", "gndr12",
            "gndr10", "gndr11","gndr13", "gndr14", "gndr15",
            "yrbrn10", "yrbrn11", "yrbrn12",
            "yrbrn13", "yrbrn14", "yrbrn15",
            "rshipa10", "rshipa11", "rshipa12", 
            "rshipa13","rshipa14", "rshipa15", 
            "edubgb2", "edufcgb1","edufbgb2", 
            "edumcgb1","edumbgb2", "inwdds", "inwmms", "inwyys",
            "inwshh", "inwsmm", "inwdde", "inwmme",
            "inwyye", "inwehh", "inwemm", "inwehh", 
            "inwemm", "inwtm", 
            #categories too big for missForest
            "idno", "ctzshipd","lvpntyr", "maryr",
            "fcldbrn","ycldbyr", "tygrtr", "yrbrn",
            "yrbrn2","yrbrn3","yrbrn4", "nwspol",
            "netustm", "cntbrthd", 
            "livecnta", "fbrncntc", "mbrncntc", 
            "pdempyr",  "agea",   "pdjobyr",
            "njbspv", "wkhct", "wkhtot", "nacer2", 
            "isco08", "isco08p", "wkhtotp", "anctry1", 
            "anctry2", "grspnum", "netinum", 
            "frlgrsp", "frlneti"))

#####################################
#####re-coding missing values########
#####################################

#we need to recode missing values, so that all of them are in the same format 'NA' in order to apply missForest
#with the help of chatgpt we constructed the following function:


replace_values_with_na <- function(data, cols, values) {
  if (is.character(cols)) {
    cols <- which(names(data) %in% cols)
  } else if (is.list(cols)) {
    cols <- unlist(lapply(cols, function(x) which(names(data) %in% x)))
  } else if (is.null(cols) || !is.numeric(cols)) {
    stop("cols argument must be a character vector, list of character vectors, or numeric vector")
  }
  
  for (i in cols) {
    data[[i]] <- ifelse(data[[i]] %in% values, NA, data[[i]])
  }
  
  return(data)
}

      #####################################
      ####media and social trust###########
      #####################################

ESS <- ESS %>% 
  replace_values_with_na(c("nwspol", "netustm"), c(6666, 7777, 8888, 9999)) %>% 
  replace_values_with_na("netusoft", c(7, 8, 9)) %>% 
  replace_values_with_na(c("ppltrst", "pplfair", "pplhlp"), c(77, 88, 99))

      #####################################
      ####politics#########################
      #####################################
ESS <- ESS %>%
  replace_values_with_na(c("polintr", "psppsgva","actrolga", "psppipla",  
                           "cptppola", "trstprl","trstlgl", "trstplc", 
                           "trstplt", "trstprt", "trstep", "trstun",
                           "lrscale", "stflife", "stflife", "stfgov",
                           "stfdem", "stfedu", "stfeco","stfhlth", "euftf",
                           "imbgeco", "imueclt", "imwbcnt"), c(77,88,99))%>%
  replace_values_with_na(c("vote", "contplt", "wrkprty", "wrkorg", 
                           "badge", "sgnptit", "pbldmn", "bctprd",
                           "pstplonl", "clsprty","gincdif", "freehms", 
                           "hmsfmlsh", "hmsacld", "imsmetn", "imdfetn",
                           "impcntr"),c(7, 8, 9))%>%
  replace_values_with_na(c("prtvede1","prtvede2", "prtclede"), c(66, 77, 88, 99))%>%
  replace_values_with_na(c("prtdgcl"), c(6, 7, 8, 9))

      #####################################
      #####subjective well-being###########
      #####################################

ESS <- ESS %>%
  replace_values_with_na(c("happy", "sclmeet", "inprdsc",
                           "atchctr", "atcherp",
                           "rlgdgr", "rlgatnd", "pray",
                           "vteurmmb"), c(77, 88, 99))%>%
  replace_values_with_na(c("sclact", "crmvct", "aesfdrk",
                           "health", "hlthhmp", "rlgblg",
                           "rlgblge", "dscrgrp", "dscrrce",
                           "dscrntn", "dscrrlg", "dscrlng",
                           "dscretn", "dscrage", "dscrgnd",
                           "dscrsex", "dscrdsb", "dscroth",
                           "dscrdk", "dscrref", "dscrnap",
                           "dscrna", "ctzcntr", "ctzshipd", 
                           "brncntr", "blgetm", "facntr",
                           "mocntr", "admdw"), c(7, 8, 9))%>%
  replace_values_with_na(c("rlgdnm", "rlgdme"), c(66, 77, 99))%>%
  replace_values_with_na(c("rlgdnade", "rlgdeade", "cntbrthd",
                           "livecnta", "fbrncntc", "mbrncntc"), c(6666, 7777, 8888, 9999))%>%
  replace_values_with_na(c("lnghom1", "lnghom2"), c(000,777, 888, 999))

      ##################################
      #######Timing of life#############
      ##################################

ESS <- ESS %>%
  replace_values_with_na(c("evpdemp", "evlvptn", "evmar",
                           "bthcld", "ggchld", "admge",
                           "anvcld", "alvgptn", "acldnmr", 
                           "aftjbyc", "advcyc", ""), c(7, 8, 9))%>%
  replace_values_with_na(c("pdempyr", "lvpntyr", "lyptnyr",
                           "maryr", "fcldbrn", "ycldbyr", "lvptnyr",
                           "ygcdbyr"), c(6666, 7777, 8888, 9999))%>%
  replace_values_with_na(c("nbthcld", "ngchld", 
                           "plnftr"), c(66, 77, 88, 99))%>%
  replace_values_with_na(c("ageadlt", "agemage", "ageoage",
                           "iaglptn", "iagmr", "iagpnt",
                           "iagrtr", "tygledu", "tyglvp", 
                           "tygmr", "tygpnt", "tygrtr", 
                           "tolvpnt", "tochld", "towkht"), c(777, 888, 999))

      ##############################################
      ###gender, Year of birth and Household grid###
      ##############################################

ESS <- ESS %>%
  replace_values_with_na(c("hhmmb"), c(77, 88, 99))%>%
  replace_values_with_na(c("gndr","gndr2", "gndr3", "gndr4",
                           "gndr5", "gndr6", "gndr7", "gndr8",
                           "gndr9"), c(6, 7, 8, 9))%>%
  replace_values_with_na(c("yrbrn", "yrbrn2", "yrbrn3", "yrbrn4",
                           "yrbrn5", "yrbrn6", "yrbrn7", "yrbrn8",
                           "yrbrn9"), c(6666, 7777, 8888, 9999))%>%
  replace_values_with_na(c("agea"), c(999))%>%
  replace_values_with_na(c("rshipa2", "rshipa3", "rshipa4",
                           "rshipa5", "rshipa6", "rshipa7",
                           "rshipa8", "rshipa9"), c(66, 77, 88, 99))

      #############################
      ####socio-demographics#######
      #############################

ESS <- ESS %>%
  replace_values_with_na(c("rshpsts", "rshpsgb", "marsts",
                           "marstgb", "maritalb", "eisced",
                           "eduyrs", "mainact", "mnactic",
                           "wkdcorga", "iorgact", "tporgwk",
                           "hincsrca", "hinctnta",
                           "iincsrc", "eiscedp", "mnactp",
                           "eiscedf", "occf14b",
                           "eiscedm", "occm14b"), c(66, 77, 88, 99))%>%
  replace_values_with_na(c("lvgptnea", "dvrcdeva",
                           "hincfel"), c(7, 8, 9))%>%
  replace_values_with_na(c("chldhhe", "domicil", "crpdwk",
                           "pdjobev", "emplrel", "wrkctra",
                           "estsz", "jbspv", "icwhct",
                           "wrkac6m", "uemp3m", "uemp12m",
                           "uemp5yr", "mbtru", "crpdwkp",
                           "emprelp", "emprf14", "emprm14",
                           "atncrse" ), c(6, 7, 8, 9))%>%
  replace_values_with_na(c("edulvlb", "edubde1", "eduade2",
                           "eduade3", "pdjobyr", "edulvlpb",
                           "edupbde1", "edupade2", "edupade3",
                           "edulvlfb", "edufbde1",
                           "edufade2", "edufade3",
                           "edulvlmb", "edumbde1",
                           "edumade2", "edumade3"), c(6666, 7777, 8888, 9999))%>%
  replace_values_with_na(c("emplno", "njbspv","isco08",
                           "isco08p"), c(66666, 77777, 88888, 99999))%>%
  replace_values_with_na(c("wkhct", "wkhtot", "nacer2",
                           "wkhtotp"), c(666, 777, 888, 999))%>%
  replace_values_with_na(c("anctry1", "anctry2"), c(777777,888888, 999999))

      #############################
      ###Justice and fairness######
      #############################

ESS <- ESS %>%
  replace_values_with_na(c("frprtpl", "gvintcz", "poltran" ), c(7,8,9))%>%
  replace_values_with_na(c("ifredu", "ifrjob", "evfredu",
                           "evfrjob", "grsplet", "netilet"), c(66,77,88, 99))%>%
  replace_values_with_na(c("grspnum", "netinum", "frlgrsp", 
                           "frlneti"), c(666666666, 777777777, 888888888, 999999999))%>%
  replace_values_with_na(c("fvgabc", "infqbst", "grspfr",
                           "netifr", "occinfr", "topinfr", "btminfr",
                           "wltdffr", "recskil", "recexp", "recknow",
                           "recimg", "recgndr", "sofrdst", "sofrwrk",
                           "sofrpr", "sofrprv", "ppldsrv", "jstprev",
                           "pcmpinj"), c(6,7,8,9))

      ############################
      ###human values#############
      ############################

ESS <- ESS %>%
  replace_values_with_na(c("ipcrtiv", "imprich", "ipeqopt",
                           "ipshabt", "impsafe", "impdiff",
                           "ipfrule", "ipudrst", "ipmodst",
                           "ipgdtim", "impfree", "iphlppl",
                           "ipsuces", "ipstrgv", "ipadvnt",
                           "ipbhprp", "iprspot", "iplylfr",
                           "impenv", "imptrad", "impfun"), c(7,8,9))

      ################################
      ###administrative variables##### ###can be removed###
      ################################

ESS <- ESS %>%
  replace_values_with_na(c("inwdds","inwmmsn", "inwshh",
                           "inwsmm", "inwdde", "inwmme",
                           "inwehh", "inwemm","ainwehh", 
                           "ainwemm", "binwehh", "binwemm",
                           "cinwehh", "cinwemm", "dinwehh", 
                           "dinwemm", "finwehh", "finwemm",
                           "ginwehh", "ginwemm", "hinwehh",
                           "hinwemm", "iinwehh", "iinwemm"), c(99))%>%
  replace_values_with_na(c("inwyys", "inwyye"), c(9999))

#####################################
#####additional check################
#####################################

for (col in names(ESS)) {
  print(paste("Frequency table for", col))
  print(summary(ESS[[col]], na.action = "na.pass"))
}

#####################################
######imputing missing values########
#####################################

#we will be using missForest to impute missing values using a random forest algorithm

#checking & changing datatypes 
sapply(ESS, class)
ESS$cntry <- as.factor(ESS$cntry)
ESS$lnghom1 <- as.factor(ESS$lnghom1)
ESS$lnghom2 <- as.factor(ESS$lnghom2)
ESS$region<- as.factor(ESS$region)



#Calculate missing values
calculate_missing_values <- function(data) {
  missing_count <- sapply(data, function(x) {
    na_count = sum(is.na(x))
    if (na_count > 1) {
      return(na_count)
    } 
  })
  missing_count <- missing_count[!is.na(missing_count)]
  return(missing_count)
}
missing_values_no_imp <- calculate_missing_values(ESS)
print(missing_values_no_imp)


# Impute missing values using mice
imputed_data <- mice(ESS, m=1, maxit=2, method='pmm', seed=500)

# Create a complete dataset
ESS_imputed <- complete(imputed_data,1)


missing_values_imp <- calculate_missing_values(ESS_imputed)
print(missing_values_imp)

# still missing (low number)
#mainact
#marsts
#dscrgrp

#still missing (high number)
#rshipa9
#rshipa7
#yrbrn9
#yrbrn8
#yrbrn7
#yrbrn6
#gndr9
#gndr8
#gndr7
#gndr6


###################
# Models
###############
ESS_imputed <- read.csv("C:/Users/Gebruiker/Documents/DataScience/Social/ESS_imputed.csv")
ESS_imputed$cntry <- as.factor(ESS_imputed$cntry)
ESS_imputed$lnghom1 <- as.factor(ESS_imputed$lnghom1)
ESS_imputed$lnghom2 <- as.factor(ESS_imputed$lnghom2)
ESS_imputed$region<- as.factor(ESS_imputed$region)

#DELETING LEVEL 7 and 8
# Specify the levels to be removed
levels_to_remove <- c("7", "8")

# Subset the data to remove the specified levels
ESS_imputed <- subset(ESS_imputed, !(prtvede1 %in% levels_to_remove))
ESS_imputed <- subset(ESS_imputed, !(prtvede2 %in% levels_to_remove))



#correlations 
# Store your target variables separately
target_variables <- ESS_imputed_cor[, c("prtvede1", "prtvede2")]

# Exclude the target variables from your working dataframe
ESS_imputed_cor <- ESS_imputed_cor[, !(names(ESS_imputed_cor) %in% c("prtvede1", "prtvede2"))]

#Drop columns with high NA and no variance and factors
removed_columns_cor <- c('prtvede2','prtvede1','X','region','lnghom2','lnghom1','cntry','yrbrn6', 'yrbrn7', 'yrbrn8', 'yrbrn9', 'gndr6', 'gndr7', 'gndr8', 'gndr9', 'rshipa9', 'rshipa7', 'marsts','mainact','dscrgrp')

ESS_imputed_cor <- ESS_imputed %>%
  select(-all_of(removed_columns_cor))

remove_constant_cols <- function(df) {
  # Select numeric columns
  df_num <- df[sapply(df, is.numeric)]
  
  # Remove columns with zero variance
  df_num <- df_num[, sapply(df_num, function(x) var(x, na.rm = TRUE) != 0)]
  
  # Combine the numeric columns with the non-numeric columns
  df <- cbind(df[sapply(df, Negate(is.numeric))], df_num)
  
  return(df)
}
ESS_imputed_cor <- remove_constant_cols(ESS_imputed_cor)

correlation_matrix <- cor(ESS_imputed_cor, use = "pairwise.complete.obs", method = "pearson")
high_corr_columns <- which(correlation_matrix > 0.8, arr.ind = TRUE)
colnames(ESS_imputed_cor)[unique(high_corr_columns[, 2])]

# Get the pairs with high correlation
high_corr_pairs <- which(correlation_matrix > 0.9 & lower.tri(correlation_matrix), arr.ind = TRUE)

# Create an empty vector to hold the columns to be removed
columns_to_remove <- integer(0)

# Iterate through the pairs
for (i in seq_len(nrow(high_corr_pairs))) {
  # If neither column from the pair is already going to be removed
  if (!(high_corr_pairs[i, "row"] %in% columns_to_remove) && !(high_corr_pairs[i, "col"] %in% columns_to_remove)) {
    # Decide to remove one of them, let's say we choose the second one
    columns_to_remove <- c(columns_to_remove, high_corr_pairs[i, "col"])
  }
}

# Remove the selected columns from the dataframe
ESS_imputed_cor <- ESS_imputed_cor[, -columns_to_remove]
# put target back 
ESS_imputed_cor <- cbind(ESS_imputed_cor, target_variables)




####################################
# Machine learning Random forest  #
###################################


# Function to preprocess data, split, train, predict and calculate accuracy
rf_model <- function(data, target, removed_columns, num_trees, mtry_values) {
  
  # Data preprocessing
  data_processed <- data %>%
    select(-one_of(removed_columns)) %>%
    mutate(across(all_of(target), as.factor))
  
  # Create training and testing datasets
  set.seed(42)
  splitIndex <- createDataPartition(data_processed[[target]], p = 0.8, list = FALSE)
  train_set <- data_processed[splitIndex,]
  test_set <- data_processed[-splitIndex,]
  
  # Define the grid of hyperparameters
  param_grid <- expand.grid(
    ntree = num_trees,
    mtry = mtry_values
  )
  
  # Perform grid search
  best_accuracy <- 0
  best_model <- NULL
  best_params <- NULL
  
  for (i in 1:nrow(param_grid)) {
    model <- randomForest(
      as.formula(paste(target, "~ .")),
      data = train_set,
      ntree = param_grid$ntree[i],
      mtry = param_grid$mtry[i],
      importance = TRUE,
      allowParallel = TRUE
    )
    
    # Make predictions
    predictions <- predict(model, newdata = test_set)
    
    # Convert predictions to a factor with the same levels as the response variable in the test set
    predictions <- factor(predictions, levels = levels(test_set[[target]]))
    
    # Calculate the accuracy
    conf_matrix <- confusionMatrix(predictions, test_set[[target]])
    accuracy <- conf_matrix$overall["Accuracy"]
    
    # Check if the current model has better accuracy than the previous best model
    if (accuracy > best_accuracy) {
      best_accuracy <- accuracy
      best_model <- model
      best_params <- param_grid[i, ]
    }
  }
  
  cat("Best Parameters:\n")
  print(best_params)
  cat("Best Accuracy:", best_accuracy, "\n")
  
  # Calculate variable importance
  importance_table <- importance(best_model, type = 2) # type 2 for MeanDecreaseGini
  var_importance <- data.frame(Variables = row.names(importance_table), Importance = importance_table[, "MeanDecreaseGini"])
  
  # Sort by importance and select the top 10 features
  top_10_features <- var_importance %>%
    arrange(desc(Importance)) %>%
    head(10)
  
  cat("Top 10 features based on their importance:\n")
  print(top_10_features)
  
  return(list(model = best_model, top_10_features = top_10_features))
}

# Set the hyperparameter values to be tuned
num_trees <- c(500,600,700)
mtry_values <- c(11,12,13,14)
removed_columns_1 <- c('prtvede2', 'yrbrn6', 'yrbrn7', 'yrbrn8', 'yrbrn9', 'gndr6', 'gndr7', 'gndr8', 'gndr9', 'rshipa9', 'rshipa7', 'marsts','mainact','dscrgrp')
removed_columns_2 <- c('yrbrn6', 'yrbrn7', 'yrbrn8', 'yrbrn9', 'gndr6', 'gndr7', 'gndr8', 'gndr9', 'rshipa9', 'rshipa7', 'marsts','mainact','dscrgrp')

# Run the function for ESS_1 with hyperparameter tuning
model_1 <- rf_model(ESS_imputed_cor, "prtvede1", removed_columns_1, num_trees, mtry_values)

# Run the function for ESS_2 with hyperparameter tuning
model_2 <- rf_model(ESS_imputed_cor, "prtvede2", removed_columns_2, num_trees, mtry_values)




#######################################
#       Gradient boosting            #
#######################################

library(xgboost)

xgb_model <- function(data, target, removed_columns, eta_values, max_depth_values, nrounds_values) {
  
  # Data preprocessing
  data_processed <- data %>%
    select(-one_of(removed_columns)) 
    
  
  # Create training and testing datasets
  set.seed(42)
  splitIndex <- createDataPartition(data_processed[[target]], p = 0.8, list = FALSE)
  train_set <- data_processed[splitIndex,]
  test_set <- data_processed[-splitIndex,]
  
  # Define the grid of hyperparameters
  param_grid <- expand.grid(
    eta = eta_values,
    max_depth = max_depth_values,
    nrounds = nrounds_values
  )
  
  # Perform grid search
  best_accuracy <- 0
  best_model <- NULL
  best_params <- NULL
  
  for (i in 1:nrow(param_grid)) {
    params <- list(
      objective = "multi:softmax",
      eta = param_grid$eta[i],
      max_depth = param_grid$max_depth[i]
    )
    
    model <- xgboost(
      params = params,
      data = as.matrix(train_set[-target]),
      label = train_set[[target]],
      nrounds = param_grid$nrounds[i],
      verbose = 0
    )
    
    # Make predictions
    predictions <- predict(model, newdata = as.matrix(test_set[-target]))
    predictions <- ifelse(predictions > 0.5, 1, 0)  # Binarize predictions for binary classification
    
    # Calculate the accuracy
    conf_matrix <- confusionMatrix(predictions, test_set[[target]])
    accuracy <- conf_matrix$overall["Accuracy"]
    
    # Check if the current model has better accuracy than the previous best model
    if (accuracy > best_accuracy) {
      best_accuracy <- accuracy
      best_model <- model
      best_params <- param_grid[i, ]
    }
  }
  
  cat("Best Parameters:\n")
  print(best_params)
  cat("Best Accuracy:", best_accuracy, "\n")
  
  return(list(model = best_model))
}

# Set the hyperparameter values to be tuned
eta_values <- c(0.01, 0.1, 0.3)
max_depth_values <- c(6, 8, 10)
nrounds_values <- c(100, 200)

# Run the function for ESS_1 with hyperparameter tuning
model_1 <- xgb_model(ESS_imputed_cor, "prtvede1", removed_columns_1, eta_values, max_depth_values, nrounds_values)

# Run the function for ESS_2 with hyperparameter tuning
model_2 <- xgb_model(ESS_imputed_cor, "prtvede2", removed_columns_2, eta_values, max_depth_values, nrounds_values)



#######################################
#               SVM                   #
#######################################

svm_model <- function(data, target, removed_columns, cost_values, gamma_values) {
  
  # Data preprocessing
  data_processed <- data %>%
    select(-one_of(removed_columns)) %>%
    mutate(across(all_of(target), as.factor))
  
  # Create training and testing datasets
  set.seed(42)
  splitIndex <- createDataPartition(data_processed[[target]], p = 0.8, list = FALSE)
  train_set <- data_processed[splitIndex,]
  test_set <- data_processed[-splitIndex,]
  
  # Define the grid of hyperparameters
  param_grid <- expand.grid(
    cost = cost_values,
    gamma = gamma_values
  )
  
  # Perform grid search
  best_accuracy <- 0
  best_model <- NULL
  best_params <- NULL
  
  for (i in 1:nrow(param_grid)) {
    model <- svm(
      as.formula(paste(target, "~ .")),
      data = train_set,
      cost = param_grid$cost[i],
      gamma = param_grid$gamma[i],
      type = "C-classification",
      kernel = "radial"
    )
    
    # Make predictions
    predictions <- predict(model, newdata = test_set)
    
    # Calculate the accuracy
    conf_matrix <- confusionMatrix(predictions, test_set[[target]])
    accuracy <- conf_matrix$overall["Accuracy"]
    
    # Check if the current model has better accuracy than the previous best model
    if (accuracy > best_accuracy) {
      best_accuracy <- accuracy
      best_model <- model
      best_params <- param_grid[i, ]
    }
  }
  
  cat("Best Parameters:\n")
  print(best_params)
  cat("Best Accuracy:", best_accuracy, "\n")
  
  return(list(model = best_model))
}

# Set the hyperparameter values to be tuned
cost_values <- c(1,5 ,10,50, 100)
gamma_values <- c(0.00001,0.0001,0.001, 0.01, 0.1)

# Run the function for ESS_1 with hyperparameter tuning
model_1 <- svm_model(ESS_imputed_cor, "prtvede1", removed_columns_1, cost_values, gamma_values)

# Run the function for ESS_2 with hyperparameter tuning
model_2 <- svm_model(ESS_imputed_cor, "prtvede2", removed_columns_2, cost_values, gamma_values)





