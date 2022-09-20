#last update:30-07

library(tidyverse)
data <-
  read_csv("machine_learning/data_arranged_210822.csv")
names(data)


#reformat the data
data$covid_dt <- as.Date(data$covid_dt, format = "%Y/%m/%d")
#arrange the data by date
data <- data %>%
  arrange(covid_dt)
# 


cols <- names(data)[c(-3,-46,-47)]
data[cols] <- lapply(data[cols], as.factor)

names(data)
str(data)
data$cut_Date <- as.integer(data$cut_Date)
table(data$cut_Date)

names(data)


#divide the data to training and testing sets

names(data)
first_set <- data[data$cut_Date >= 1 & data$cut_Date <= 4,]
table(first_set$covid_diagnosis)
first_set <- first_set[,c(-3,-43,-45,-46,-47)]
names(first_set)
n_train <- nrow(first_set) * 0.8
train_set <- head(first_set, n = floor(n_train))
table(train_set$covid_diagnosis)


n_test <- nrow(first_set) * 0.2
test_set <- tail(first_set, n = floor(n_test))

table(test_set$covid_diagnosis)


#balance the training set
library(ROSE)
data_balanced_over <- ovun.sample(
  covid_diagnosis ~ .,
  data = train_set,
  N = 2932,
  p = 0.5,
  seed = 1,
  method = "both"
)$data


table(data_balanced_over$covid_diagnosis)
train_set <- data_balanced_over
table(train_set$covid_diagnosis)


rm(data_balanced_over, cols, first_set,result)


library(randomForest)
library(caret)
library(tidyverse)


train_model <- randomForest(
  covid_diagnosis ~ . ,
  data = train_set,
  ntree = 5000,
  mtry = 15,
  replace = T
)
#)
plot(train_model)
names(test_set)
train_model$confusion
prediction <- predict(train_model, test_set, type = 'class')
table_mat <- table(test_set$covid_diagnosis, prediction)
table_mat



###############################################################
##############Start monthly testing ###########################
############################################################

test_set1 <- data[data$cut_Date > 4, ]
names(test_set1)
yhat <- predict(train_model, test_set1[c(-3,-43,-45,-46,-47)])
y <- test_set1$covid_diagnosis
cut_date <- test_set1$cut_Date
borough <- test_set1$borough
patid <- test_set1$patid
covid_dt <- test_set1$covid_dt
table(y)
table(cut_date)
dataframe <- data.frame(covid_dt,patid, borough, y , yhat, cut_date)

dataframe$false_classification <-
  ifelse(dataframe$y == dataframe$yhat, 0, 1)

#*****************************************
#*********** Prequential error **********
#*****************************************

dataframe <- read.csv("Drift_Detection/Simulated_data_210822/resultant_data/prediction_results_220822.csv")
dataframe$covid_dt <- as.Date(dataframe$covid_dt, format = "%Y-%m-%d")
dataframe$borough <- as.factor(dataframe$borough)
lon_boroughs <- levels(dataframe$borough)
library(tidyverse)

dataframe <- arrange(dataframe, covid_dt)


prob_total <- 0
vector <- c()
sumprobx <- 0


table(dataframe$cut_date)
for (i in 5:17) {
  first_batch <- dataframe[dataframe$cut_date == i, ]

  for (x in seq(1:nrow(first_batch))) {
    
    
    probx <- ifelse(first_batch$y[x] == first_batch$yhat[x], 0, 1)
    print(probx)
    
    
    
    # sumprobx <- sumprobx + probx
    # current_error <- sumprobx / x
    # current_error <- as.numeric(format(round(current_error, 4), nsmall = 4))
    
    ######### prequential error ############

    prob_total  <- (probx + (x-1) * prob_total)/x
    prob_total <- format(round(prob_total, 3 ), nsmall = 3)
    prob_total <- as.numeric(prob_total)
    print(prob_total)
    
    #########end prequential error #########
    vector <- append(vector, prob_total)
  }
}
  



vector
plot(vector)
error_rate <- as.data.frame(vector)
error_rate <- cbind(dataframe,error_rate)

#use this in river package code
write.csv(
  error_rate,
  "Drift_Detection/Simulated_data_210822/detecttion_290822/prequential_errorrate_monthly_allLondon_fadigfactors0.998_290822.csv",
  row.names = F
)




#**************************************************************************************
#*********** holdout error (sum or wrong classification / #of examples)**********
#**************************************************************************************


vector_normal_err <- c()
sumprobx <- 0
month_error <- 0

for (i in 5:17) {
  first_batch <- dataframe[dataframe$cut_date == i, ]
  for (x in 1:nrow(first_batch)) {
    probx <- ifelse(first_batch$y[x] == first_batch$yhat[x], 0, 1)
    print(probx)
    
    sumprobx <- sumprobx + probx
    current_error <- sumprobx / x
    current_error <-
      as.numeric(format(round(current_error, 2), nsmall = 2))
    
    vector_normal_err <- append(vector_normal_err, current_error)
  }
  sumprobx <- 0
}

vector_normal_err
plot(vector_normal_err)
error_normal <- as.data.frame(vector_normal_err)

#use this in river package code
write.csv(
  error_normal,
  "Drift_Detection/Simulated_data_210822/resultant_data/normal_error_monthly_allLondon_22022.csv",
  row.names = F
)


#*******************************************************************************************
#******* Hold out Error for each month (false classification / total number of examples) ****************
#*******************************************************************************************

error <- c()



for (i in 5:17) {
  data_month <- dataframe[dataframe$cut_date == i, ]
  sum_false_classif <- sum(data_month$false_classification)
  error_month <-
    as.numeric(format(round((
      sum_false_classif / nrow(data_month)
    ), 3), nsmall = 3))
  error <- append(error, error_month)
}

plot(error)
error <- as.data.frame(error)
write.csv(
  error,
  "Drift_Detection/Simulated_data_210822/resultant_data/static_monthly_error_allLondonNEW_220822.csv",
  row.names = F
)



#*******************************************************************************************
#**************************** Monthly error for boroughs ****************
#*******************************************************************************************


table(dataframe$borough)

dataframe$borough <- as.factor(dataframe$borough)
lon_boroughs <- levels(dataframe$borough)

boroughs_monthly_error <-
  as.data.frame(matrix(nrow = 12, ncol = length(lon_boroughs)))

names(boroughs_monthly_error) <- lon_boroughs


#Draw this on python
borough <- 1
for (borough in 1:length(lon_boroughs)) {
  #inst_data <- as.data.frame(matrix(nrow=1,ncol= length(lon_boroughs)))
  
  borough_data <-
    dataframe[dataframe$borough == lon_boroughs[borough],]
  
  dataframe <- arrange(dataframe, covid_dt)
  
  current_borough <- lon_boroughs[borough]
  for (i in 5:17) {
    borough_month <- borough_data[borough_data$cut_date == i, ]
    sum_false_classif <- sum(borough_month$false_classification)
    borough_error_month <-
      as.numeric(format(round((
        sum_false_classif / nrow(borough_month)
      ), 3), nsmall = 3))
    boroughs_monthly_error[i, current_borough] <-
      borough_error_month
  }
}

boroughs_monthly_errorx <- boroughs_monthly_error[complete.cases(boroughs_monthly_error), ]
write.csv(boroughs_monthly_errorx, "Drift_Detection/Simulated_data_210822/resultant_data/boroughs_monthly_error.csv", row.names = F)

#*******************************************************************************************
#**************************** holdout error for boroughs- per example****************
#*******************************************************************************************


for (borough in 1:length(lon_boroughs)) {
  
  borough_data <-
    dataframe[dataframe$borough == lon_boroughs[borough],]
  
  borough_data <- arrange(borough_data, covid_dt)
  
  current_borough <- lon_boroughs[borough]
  vector_normal_err <- c()
  sumprobx <- 0
  month_error <- 0
  
  for (i in 5:17) {
    first_batch <- borough_data [borough_data$cut_date == i, ]
    month_error <-
      sum(first_batch$false_classification) / nrow(first_batch)
    sumprobx
    for (x in 1:nrow(first_batch)) {
      probx <- ifelse(first_batch$y[x] == first_batch$yhat[x], 0, 1)
      print(probx)
      
      sumprobx <- sumprobx + probx
      current_error <- sumprobx / x
      current_error <-
        as.numeric(format(round(current_error, 2), nsmall = 2))
      
      vector_normal_err <- append(vector_normal_err, current_error)
    }
    
    sumprobx <- 0
  }
  holdout_err <- as.data.frame(vector_normal_err)
  borough_data <- cbind(borough_data, holdout_err)
  write.csv(borough_data, paste("Drift_Detection/Simulated_data_210822/sim_boroughs_holout_error/holdout_error", current_borough,".csv"),row.names = F)
  
}





#*******************************************************************************************
#**************************** new preq error for boroughs - per example ****************
#*******************************************************************************************
#*********************THIS ONE IS THE oNE USED IN THE DISSERTATION *************************************

dataframe <- read.csv("Drift_Detection/Simulated_data_210822/resultant_data/prediction_results_220822.csv")
dataframe$covid_dt <- as.Date(dataframe$covid_dt, format = "%Y-%m-%d")
dataframe$borough <- as.factor(dataframe$borough)
lon_boroughs <- levels(dataframe$borough)
library(tidyverse)

dataframe <- arrange(dataframe, covid_dt)
str(dataframe)
prob_total<- 0 


for (borough in 1:length(lon_boroughs)) {
  
  borough_data <-
    dataframe[dataframe$borough == lon_boroughs[borough],]
  
  borough_data <- arrange(borough_data, covid_dt)
  
  current_borough <- lon_boroughs[borough]

  vector <- c()
  sumprobx <- 0
  prob_total <- 0 
  
  table(dataframe$cut_date)
  for (i in 5:17) {
    first_batch <- borough_data[borough_data$cut_date == i, ]
    print(i)

    for (x in seq(1:nrow(first_batch))) {
      
      
      probx <- ifelse(first_batch$y[x] == first_batch$yhat[x], 0, 1)
      print(probx)
      ###########new preq error##################
      prob_total  <- ((probx + ((x-1) * prob_total)))/x
       prob_total <- format(round(prob_total, 3 ), nsmall = 3)
       prob_total <- as.numeric(prob_total)
       print(prob_total)
    
      vector <- append(vector, prob_total)

    }
    
    prob_total <- sum(first_batch$false_classification)/nrow(first_batch)
  }
  
  preq_err <- as.data.frame(vector)
  preq_err$vector <- abs(preq_err$vector)
  borough_data <- cbind(borough_data, preq_err)
  write.csv(borough_data, paste("Drift_Detection/Simulated_data_210822/detecttion_290822/borough_new_preq_err_w500/preq_error", current_borough,".csv"),
            row.names = F)
  
}

table(borough_data$cut_date)

