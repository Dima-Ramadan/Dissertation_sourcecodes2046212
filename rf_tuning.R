#last update:30-08

library(tidyverse)
data <-
  read_csv("GP_allocation/resultant_data/sim_london_gp_geo_200822.csv")
names(data)


#reformat the data
data$covid_dt <- as.Date(data$covid_dt, format = "%d/%m/%Y")
#arrange the data by date
data <- data %>%
  select(-catAge, -region, -rurban, -DeathDate, -Death, -id1, -patid, -isPositive, 
         -gp_counts, -AreaCode, -Latitude, -Longitude)%>%
  arrange(covid_dt)

tail(data$covid_dt)
names(data)

#format data types and levels
data$Gender <- ifelse(data$Gender == "M", 0 , 1)

data$age <- as.integer(data$age)
data$ageCat <- ifelse(data$age < 16, 1 ,ifelse(
  data$age >= 16 & data$age <= 24 , 2, ifelse(
    data$age >= 25 & data$age <= 34 , 3, ifelse(
      data$age >= 35 & data$age <= 49, 4, ifelse(
        data$age >= 50 & data$age <= 64, 5, ifelse(
          data$age >= 65 & data$age <= 74, 6 , 7
        )
      )
    ) 
  )
)
)


cols <- names(data)[c(-4)]
data[cols] <- lapply(data[cols], as.factor)

names(data)
str(data)
data <- select(data, c(-age))

library(lubridate)

result <- data.frame(data,
                     cut_Datex = floor_date(data$covid_dt, "month"),
                     stringsAsFactors = TRUE)
result$cut_Date
result$cut_Date <- as.factor(result$cut_Datex)
table(result$cut_Date)



levels(result$cut_Date) <-
  seq(1:19) #dec 2019 - apr2022+
str(result)
table(result$cut_Date)
result <-
  result[result$cut_Date != 18 &
           result$cut_Date != 19 ,]




data <- result
data <- data %>% arrange(covid_dt)


str(data)
data$cut_Date <- as.integer(data$cut_Date)
table(data$cut_Date)

data$patid <- seq(1:nrow(data))

#data <- data[-3]
rm(result)
names(data)

data_ml <- data
#cols <- names(data_ml)[c(-1,-2,-3,-43,-44,-45,-46,-47)]

for (i in 4: 42){
  levels(data_ml[,i]) <- c("x0", "x1")
}
levels(data_ml$ageCat) <- c('x0','x1','x2','x3','x4', 'x5', 'x6')
levels(data_ml$imd_5) <- c('x0','x1','x2','x3', 'x4')
levels(data_ml$Gender) <- c('x0','x1')
#data <- result
#table(data$cut_Date)

data_ml <- read.csv("machine_learning/data_ml_210822.csv")

data_ml <- data_ml %>% arrange(covid_dt)


data_ml <- data_ml[,c(-3,-43,-45,-47)]
names(data_ml)

cols <- names(data_ml)[-43]
data_ml[cols] <- lapply(data_ml[cols], as.factor)

str(data_ml)

first_set <- data_ml[data_ml$cut_Date >= 1 & data_ml$cut_Date <= 4,]
table(first_set$covid_diagnosis)
first_set <- first_set[-43]
names(first_set)
n_train <- nrow(first_set) * 0.8
train_set <- head(first_set, n = floor(n_train))
table(train_set$covid_diagnosis)


#table(train_set$borough[train_set$covid_diagnosis == 1])



n_test <- nrow(first_set) * 0.2
test_set <- tail(first_set, n = floor(n_test))

table(test_set$covid_diagnosis)

1466 * 2
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

#write.csv(data, "machine_learning/data_arranged_210822.csv", row.names =F)
#write.csv(data_ml, "machine_learning/data_ml_210822.csv", row.names =F)


rm(data_balanced_over, cols, first_set,result)
rm(data, df)
rm(data_ml)





#***************************************************
#****************Model Training ********************
#**************************************************

library(randomForest)
library(caret)

#tunung function
customRF <- list(type = "Classification", library = "randomForest", loop = NULL)
customRF$parameters <- data.frame(parameter = c("mtry", "nodesize","ntree"), class = rep("numeric", 3), label = c("mtry","nodesize", "ntree"))
customRF$grid <- function(x, y, len = NULL, search = "grid") {}
customRF$fit <- function(x, y, wts, param, lev, last, weights, classProbs, ...) {
  randomForest(x, y, mtry = param$mtry, nodesize=param$nodesize,ntree=param$ntree, ...)
}
customRF$predict <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
  predict(modelFit, newdata)
customRF$prob <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
  predict(modelFit, newdata, type = "prob")
customRF$sort <- function(x) x[order(x[,1]),]
customRF$levels <- function(x) x$classes


seed <- 7
metric <- "Accuracy"

# train model
control <- trainControl(method="repeatedcv", number=3, repeats=1)
tunegrid <- expand.grid(.mtry=c(1:15), .nodesize=c(10,500,1000,1500), .ntree = c(5000))
set.seed(seed)
custom <- caret::train(covid_diagnosis~., data=train_set,
                method=customRF,
                metric=metric, 
                tuneGrid=tunegrid, 
                trControl=control)

custom
summary(custom$results)
plot(custom)


###############################
#*best parameters : 
#*ntree: does not affect the model performance 
#*mtry: 15
#*nodesize:


train_model <- randomForest(covid_diagnosis ~ ., 
                            data = train_set,
                            ntree = 5000,
                            mtry = 15,
                            #nodesize = 10,
                            replace = T
                            )

hist(treesize(train_model),
     main = "No. of Nodes for the Trees",
     col = "green")

#Variable Importance
varImpPlot(train_model,
           sort = T,
           n.var = 10,
           main = "Top 10 - Variable Importance")
importance(train_model)


plot(train_model)
names(test_set)
train_model$confusion
prediction <- predict(train_model, test_set, type = 'class')
table_mat <- table(test_set$covid_diagnosis, prediction)
table_mat

write_rds(train_model,"machine_learning/baseModel_ntree5000_mtry15_repT.rds")





# cols <- names(train_set)
# 
# 
# 
# #set parallel backend (Windows)
# library(parallelMap)
# library(parallel)

#install.packages("mlr")
#library(mlr)
# #getParamSet(learner)
# parallelStartSocket(cpus = detectCores())
# #parallelStop()
# row.names(train_set) <- seq(1:nrow(train_set))
# 
# # create task
# task = makeClassifTask(id = "train_set", train_set, target = "covid_diagnosis")
# # create learner
# learner = makeLearner("classif.randomForest",
#                       fix.factors.prediction = T)
# cross_val <- makeResampleDesc("CV",iters=5L)
# 
# ctrl <- makeTuneControlRandom()
# param.set <- makeParamSet(makeIntegerParam("mtry",lower = 2,upper = 15),
#                          makeIntegerParam("nodesize",lower = 10,upper = 50),
#                          makeDiscreteParam("ntree", values = c ( 100,300,500,800,1000, 1300)))
# 
# lrn_tune <- tuneParams(learner,
#                        task,
#                        measures = list(acc),
#                        control = ctrl,
#                        par.set = param.set,
#                        resampling = cross_val,
#                        show.info = T
#                        )
# 
# 
# 
# #getTuneResult






















# names(train_set)
# names(data)
# str(train_set)

# train_model <- randomForest(
#   covid_diagnosis ~ . ,
#   data = train_set,
#   ntree = 10000,
#   #oob.prox= T,
# 
#   mtry = 15,
#   # cutoff = c(0.7, 1-0.7),
#   replace = T
# )
#)
# plot(train_model)
# names(test_set)
# train_model$confusion
# prediction <- predict(train_model, test_set, type = 'class')
# table_mat <- table(test_set$covid_diagnosis, prediction)
# table_mat
# 
# 






















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


