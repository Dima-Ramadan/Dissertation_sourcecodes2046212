



#############################################
#Run this code two times to test old data and new data
# Chi-square function may need some modifications between the two datasets
#############################################

#devtools::install_github("ModelOriented/drifter")  #DALEX, iBreakDown, ingredients, ranger
#Load libraries
packages = c("lubridate",
             "tidyverse",
             "reshape2",
             "ggpubr",
             "gridExtra",
             "grid",
             "tidymodels")

package.check <- lapply(
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)





## List the files in source_files and source each one of them.
list.files("EDA_source_files/", full.names = TRUE) %>%
  map(source)


#Read data
data_drift <-
  read_csv("GP_allocation/resultant_data/GT_london_data.csv")

#data_drift <- read_csv("../london_data_DA290722.csv")

#Data Exploration

data_ex <- data_drift
remove(data_drift)
dim(data_ex) #267,600 X 50  //511946

table(data_ex$region)
names(data_ex)
data_ex <-
  data_ex %>% select(-Death,-DeathDate,-id1,-patid,-isPositive, -region) #49 features


typeof(data_ex$covid_diagnosis) #double
typeof(data_ex$covid_dt)
class(data_ex$covid_dt)

data_ex$covid_dt <- as.Date(data_ex$covid_dt, format = "%d/%m/%Y")
head(data_ex$covid_dt)

#arrange the data by date
data_ex <- data_ex %>%
  arrange(covid_dt)


#Plots theme setting
ggtheme <-
  theme(
    axis.text = element_text(size = 9),
    axis.title = element_text(size = 10, face = "bold"),
    plot.title = element_text(size = 10),
    panel.background = element_blank(),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    axis.line = element_line( colour = "black")
  )

data_ex <- data_ex[data_ex$covid_dt >= "2020-02-01" & data_ex$covid_dt <= "2021-04-01" ,]

# number of diagnoses per day
diag_day <- data_ex %>%
  arrange(covid_dt) %>%
  group_by(covid_dt) %>%
  summarise(num_diag = sum(covid_diagnosis))


date_range <-
  which(data_ex$covid_dt %in% as.Date(c("2019-12-01",
                                        "2020-01-01",
                                        "2020-02-01",
                                        "2020-03-01",
                                        "2020-04-01",
                                        "2020-05-01",
                                        "2020-06-01",
                                        "2020-07-01",
                                        "2020-08-01",
                                        "2020-09-01",
                                        "2020-10-01",
                                        "2020-11-01",
                                        "2020-12-01",
                                        "2021-01-01",
                                        "2021-02-01",
                                        "2021-03-01")))
library(RColorBrewer)
brewer.pal(n = 8, name = "RdBu")

ggplot(diag_day, aes(x = covid_dt, y = num_diag)) +
  geom_col(fill ="#92C5DE")+
  stat_smooth(aes(y =num_diag*1.5) ,method = lm, formula = y ~ poly(x, 12), se = FALSE, col= "#2166AC")+
  ylab("Counts") +
  xlab("Date") +
  ggtitle("Daily COVID-19 cases between February 2020 and April 2021") +
  scale_y_continuous(breaks = seq(0, 1500, 100)) +
  scale_x_date(date_breaks = "months" , date_labels = "%d-%m") +
  ggtheme


#930 387



#number of diagnosis per week (for concept drift)
diag_week <- data_ex %>%
  group_by(Date = floor_date(covid_dt, "week")) %>%
  summarise(num_diag_week = sum(covid_diagnosis))

brewer.pal(n = 8, name = "RdBu")

ggplot(diag_week, aes(x = Date, y = num_diag_week)) +
  geom_col(fill ="#92C5DE")+
  stat_smooth(aes(y =num_diag_week*1.2) ,method = lm, formula = y ~ poly(x, 15), se = FALSE, col ="#2166AC")+
  theme(legend.position = "none") +
  ggtheme +
  ylab("Counts") +
  xlab("Date") +
  scale_x_date(date_breaks = "month" , date_labels = "%d-%m") +
  ggtitle("Weekly Covid-19 cases between February 2020 and April 2021")




#On monthly level
diag_month <- diag_week %>%
  group_by(Date = floor_date(Date, "month")) %>%
  summarise(num_diag_month = sum(num_diag_week))
#number of diagnosis
#zero in Feb.
#1,511 in March
#9,462 in April


ggplot(diag_month, aes(x = Date, y = num_diag_month)) +
  geom_col(fill ="#92C5DE")+
  stat_smooth(aes(y =num_diag_month*1.2) ,method = lm, formula = y ~ poly(x, 10), se = FALSE, col ="#2166AC")+
  theme(legend.position = "none") +
  ggtheme +
  ylab("Counts") +
  xlab("Date") +
  scale_x_date(date_breaks = "month" , date_labels = "%d-%m") +
  ggtitle("Monthly Covid-19 cases between February 2020 and April 2021")


names(data_ex)

#data_plot <- data_ex[c(-5, -45)]
names(data_ex)
data_plot <- data_ex[c(-2, -6)]
cols <- names(data_plot)
data_plot[cols] <- lapply(data_plot[cols], as.factor)

str(data_plot)


library(scales)

#plot the features,
data_plot %>%
  select(where(is.factor)) %>%
  pivot_longer(cols = everything(),
               names_to = "variable",
               values_to = "value") %>%
  ggplot(aes(x = value)) +
  geom_bar(aes(y=(..count..)/sum(..count..)),fill = "#4393C3") +
  facet_wrap(vars(variable),
             scales = "free",
             nrow = 6)+
  xlab("")+
  ylab("")

#1175 760
data_ex %>%
  ggplot(aes(x = borough)) +
  geom_bar(aes(y=(..count..)/sum(..count..)),fill = "#2166ac") +
 guides(x = guide_axis(angle = 45)) +
  ggtitle(paste(" Simulated Residence Area"))+
  theme(
    axis.text=element_text(size=8),
    axis.title = element_text(size=8,face="bold"),
    plot.title = element_text(size=12),
    plot.caption = element_text(size=7),
    plot.margin = margin(0.8,0.8,0.8,0.8, "cm"),
    panel.background = element_blank(),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    axis.line = element_line( colour = "black"))

rm(
  data_plot,
  duplicated_id,
  duplicated_rows,
  package.check,
  diag_week_reshape,
  cols,
  date_range,
  packages
)


table(data_ex$Tamiflu_rx) #1 ==> 21, 0==> 157479
table(data_ex$SAMA_rx)    #1 ==> 200
table(data_ex$MentalHealth) #1==> 132
table(data_ex$PAD) # 1 ==> 531
table(data_ex$AminoTheophy_rx) # 1 ==> 30


#Plot features for diagnosed people only

diagnosed_people <- data_ex[data_ex$covid_diagnosis == 1, ]
data_plot <-
  diagnosed_people[c(-2, -6)]

cols <- names(data_plot)
data_plot[cols] <- lapply(data_plot[cols], as.factor)


#plot the features
data_plot %>%
  select(where(is.factor)) %>%
  pivot_longer(cols = everything(),
               names_to = "variable",
               values_to = "value") %>%
  ggplot(aes(x = value)) +
  geom_bar(aes(y=(..count..)/sum(..count..)),fill = "#4393C3") +
  facet_wrap(vars(variable),
             scales = "free",
             nrow = 6)+
  ylab("")+
  xlab("")

lapply(data_plot, table)







data_plot <- data_ex[ 47]
data_plot %>%
  ggplot(aes(x = borough)) +
  #stat_count(mapping = aes(x=borough, y=..prop.., group=1))+  #put this instead geom_bar for percentage
  geom_bar(aes(y=(..count..)/sum(..count..)),fill = "#4393C3") +
  ggtheme + guides(x = guide_axis(angle = 45)) +
  ggtitle(paste("Residence Borough Distribution For Covid-19 Cases"))


rm(data_plot, diagnosed_people)


march2020 <- data_ex[data_ex$covid_dt >= "2020-03-01" &
                       data_ex$covid_dt < "2020-04-01", ]

april2020 <- data_ex[data_ex$covid_dt >= "2020-04-01" &
                       data_ex$covid_dt <= "2020-04-30", ]

may2020 <- data_ex[data_ex$covid_dt >= "2020-05-01" &
                     data_ex$covid_dt <= "2020-05-30", ]

table(data_ex$covid_diagnosis)

nrow(data_ex[data_ex$covid_diagnosis == 1,]) #Total number of diagnosis in march2020 and april 2020 is  84085
nrow(march2020[march2020$covid_diagnosis == 1,])#Total number of diagnosis in march2020 is 15568
nrow(april2020[april2020$covid_diagnosis == 1,])#Total number of diagnosis in April 2020 is 35967
nrow(may2020[may2020$covid_diagnosis == 1,])#Total number of diagnosis in May 2020 is 27071


#Rank the important features in each month


source("EDA_source_files/Chi_square_ranking.R")
#rm(all_time_chisq)
data_ex <- data_ex[c(-45,-48,-49,-50,-51)]
names(data_ex)
all_time_chisq <-
  chisq_ranking(data_ex) #returns table for x-squares and p-values

all_time_chisq$x_squared <- as.double(all_time_chisq$x_squared)
all_time_chisq$p_value <- as.double(all_time_chisq$p_value)
all_time_chisq <- all_time_chisq[2:nrow(all_time_chisq) ,] %>%
  arrange(desc(x_squared))

all_time_indep_feat <- chisq_indep_feat(all_time_chisq)
all_time_xsquared_plot <-
  xsquared_plotting(all_time_chisq, "March - May", "2020")
all_time_xsquared_plot


march_chisq <- chisq_ranking(march2020)
march_indep_feat <- chisq_indep_feat(march_chisq)
march_xsquared_plot <-
  xsquared_plotting(march_chisq, "March", "2020")
march_xsquared_plot


april_chisq <- chisq_ranking(april2020)
april_indep_feat <- chisq_indep_feat(april_chisq)
april_xsquared_plot <-
  xsquared_plotting(april_chisq, "April", "2020")
april_xsquared_plot


# figure_chisq_rank<- grid.arrange(all_time_xsquared_plot, march_xsquared_plot,
#                                  april_xsquared_plot,
#                                     ncol = 1)


#Explore important features

#Age

source("source_files/gender_plots.R")

all_time_gender <- gender_ex(data_ex, "February 2020 - April 2021", "2020")

all_time_gender
#######################################
###########################
#######################################
source("source_files/pct_deseases_from_diagnosis.R")

pct_deseaases(data_ex, "February 2020 - April 2021", "2020")




rm(
  march_xsquared_plot,
  march_indep_feat,
  march_chisq,
  figure_chisq_rank,
  april_xsquared_plot,
  april_chisq,
  march_gender,
  april_gender,
  all_time_chisq,
  all_time_gender,
  all_time_indep_feat,
  all_time_xsquared_plot,
  april_indep_feat,
  i,
  xsquared_plotting,
  pct_deseaases,
  gender_ex,
  chisq_ranking,
  chisq_indep_feat
)
