

#20/08/2022

#This file aim is to build new GP profile from the QOF data 
#Then assign the population for each GP in London 
# The population of London was set to 1.08M But can be changed to the desired size 



install.packages("RColorBrewer")

library(tidyverse)
library(RColorBrewer)
library(openxlsx)
library(tidyverse)

path <- "additional_data/gp_data/qof-2021-prev-prac-v2.xlsx"

# importing the required library to read multiple sheets from the xlsx file
#install.packages("openxlsx")
library(openxlsx)

# getting data from sheets
sheets <- openxlsx::getSheetNames(path)
data_symp_qof <- lapply(sheets, openxlsx::read.xlsx, xlsxFile=path)

# assigning names to data frame
names(data_symp_qof) <- sheets

#Using Af sheet to extract the number of registered patients in each GP
af <- as.data.frame(data_symp_qof$AF)
names(af) <- af[6,]
af <- af[7:nrow(af), c(-8,-9,-10)]
af <- af[c("Practice code","List size")]

##################################

#Use the old GP profile data to extract the coordinates of GPs and then revese the gew encode to find the london borough where the gp is

gp_london <- read.csv("GP_allocation/resultant_data/gp_geo_london.csv")

levels(as.factor(gp_london$borough))

gp_london_modified <- gp_london[c(1,2,3,4)]

gp_london_modified<- left_join(gp_london_modified, af, by =  c("AreaCode" = "Practice code"))


gp_london_modified$`List size` <- as.integer(gp_london_modified$`List size`)

test <- gp_london_modified[!complete.cases(gp_london_modified),]
gp_london_modified <- gp_london_modified[complete.cases(gp_london_modified),]

#####################################################################
#check the distribution of residents in gp-profile QOF 
aggregated_list_size <- gp_london_modified %>%
  group_by(borough) %>%
  summarize(sum_borough = sum(`List size`))


aggregated_list_size$prop_gp <- aggregated_list_size$sum_borough / sum(aggregated_list_size$sum_borough)


ggtheme <-theme(#panel.background=element_rect(fill = "#F8fbf8", colour = "#f7f7f7"),
               #panel.grid.major = element_line(colour = "grey90"),
                axis.text=element_text(size=8),
                axis.title = element_text(size=8,face="bold"),
                plot.title = element_text(size=12),
                plot.caption = element_text(size=7),
                plot.margin = margin(0.8,0.8,0.8,0.5, "cm"),
                panel.background = element_blank(),
                panel.grid.major = element_blank(), 
                panel.grid.minor = element_blank(),
                axis.line = element_line( colour = "black"))
  
#850*500

dist_gp_prof <- ggplot(aggregated_list_size, aes(x = borough, y = (prop_gp*100) ))+
  geom_col(fill = "#2166ac")+
  guides(x = guide_axis(angle = 45))+
  scale_fill_brewer(palette = "Set1")+
  ggtheme+
  xlab ("")+
  ylab("% of residents")+
  labs(title= ("Percentage of Residents from QOF 2020-2021 Data"))+
  scale_y_continuous(limits = c(0, 5.5), breaks = seq(0, 5.5, by = 1))

dist_gp_prof 


 


#############################################

#Fom the assumed population (1080000) we will find the number of people in each borough 
####This way will allow us to control the pooulation without losing any important data ####
#****************** under sampling the population ********************

aggregated_list_size$ppl_count <- floor(aggregated_list_size$prop_gp * 1080000)


gp_modofied_LS <- gp_london_modified %>%
  full_join(aggregated_list_size, by = c("borough")) %>%
  dplyr::select(-prop_gp)

gp_modofied_LS$pct_of_ppl <- gp_modofied_LS$`List size` / gp_modofied_LS$sum_borough


gp_modified_res <- gp_modofied_LS %>%
  dplyr::select(-`List size`, -sum_borough) %>%
  mutate(gp_counts =  ceiling(ppl_count * pct_of_ppl))%>%
  dplyr::select(-ppl_count, -pct_of_ppl)

rm(af,aggregated_list_size, gp_london_modified, gp_modofied_LS, ggtheme)


# write.csv(gp_modified_res,
#           "GP_allocation/resultant_data/simulated_ppl_gpcounts_QOFData_1.08M_200822.csv",
#           row.names= FALSE)
#########################################################
##############################################



#########################################################



# getting data from sheets
path <- "additional_data/gp_data/qof-2021-prev-prac-v2.xlsx"
sheets <- openxlsx::getSheetNames(path)
#read the sheets and save each one in a list in the gp_profile_new list
gp_profile_new <- lapply(sheets, openxlsx::read.xlsx, xlsxFile=path)

# assigning names to data frame
names(gp_profile_new) <- sheets

#remove the first list (introduction sheet in excel - no useful data in it)
gp_profile_new[1] <- NULL


gp_modified_res <- read.csv("GP_allocation/resultant_data/simulated_ppl_gpcounts_QOFData_1.08M_200822.csv")
#rm(data_qof_pct, dataframe)

library(tidyverse)
data_qof_pct <- gp_modified_res 

for(i in seq(1:22)){
  dataframe <- as.data.frame(gp_profile_new [[i]]) #extract the ith list 
  names(dataframe) <- dataframe[6,]  #chane the column names
  dataframe<- dataframe[7:nrow(dataframe), c(6, 13)] #extract all the rows( any thing before the 7th row is data description in excel) and the prevelance  and the areaCode attributes
  names(dataframe)[2] <- names(gp_profile_new )[i] # assign the name of indicator to the prevelence col
  data_qof_pct <- data_qof_pct %>% left_join(dataframe, by = c("AreaCode" = "Practice code")) #ass the indocator to gp profile 
}

names(data_qof_pct)

data_qof_pct <- data_qof_pct[complete.cases(data_qof_pct), ]

#add to data variables from the old gp_profile 

gp_geo_old <- read.csv("GP_allocation/resultant_data/gp_geo_london.csv")
#rm(gp_geo_old)
gp_geo_old <- gp_geo_old[complete.cases(gp_geo_old),]
names(gp_geo_old) <- c("borough", "AreaCode", "Lat", "Long", "plus85", "lshcond",
                       "carring_respon", "imd_score", "life_exp_male", "life_exp_female",
                       "total_QOF", "CHD_QOF", "stroke_QOF", "PAD_QOF", 
                       "heart_failure_QOF", "heart_failure_LVD_QOF", 
                       "atrial_fibrillation_QOF", "hypertension_QOF",
                       "obesityPlus18_QOF", "smoke002", "diabetesPlus17_QOF",
                       "diabetesPlus40_noCVD", "mentalhealth_QOF", "dementia_QOF",
                       "pct_alzheimer_dementia", "pct_LTmentalhealth",
                       "depressionPlus18_QOF","osteoporosisPlus50_QOF",
                       "rheumatoid_rthritisPlus16_QOF",
                       "pct_LTMSK", "COPD_QOF", "Asthma_QOF", "smoke_QOF", "COPD007", 
                       "ntibiotic_prescper1000")

data_qof_pct <- gp_geo_old %>%
  dplyr::select(AreaCode, plus85, imd_score)%>%
  right_join(data_qof_pct, by = c("AreaCode"))

#filling missing values with borough average

for(i in 1:nrow(data_qof_pct)){
  data_qof_pct$imd_score[i] <-ifelse(is.na(data_qof_pct$imd_score[i]),
                                     mean(data_qof_pct$imd_score[!is.na(data_qof_pct$imd_score) &data_qof_pct$borough == data_qof_pct$borough[i]]),
                                     data_qof_pct$imd_score[i])
  
  data_qof_pct$plus85[i] <-ifelse(is.na(data_qof_pct$plus85[i]),
                                  mean(data_qof_pct$plus85[!is.na(data_qof_pct$plus85) &
                                                             data_qof_pct$borough == data_qof_pct$borough[i]]),
                                  data_qof_pct$plus85[i])
  
  data_qof_pct$diabetesPlus40_noCVD[i] <-ifelse(is.na(data_qof_pct$diabetesPlus40_noCVD[i]),
                                                mean(data_qof_pct$diabetesPlus40_noCVD[!is.na(data_qof_pct$diabetesPlus40_noCVD) &
                                                                                         data_qof_pct$borough == data_qof_pct$borough[i]]),
                                                data_qof_pct$diabetesPlus40_noCVD[i])
}


#bin the score
for (i in 1:nrow(data_qof_pct)){
  if(data_qof_pct$imd_score[i] <= 8.49){
    data_qof_pct$imd_score[i] = 1} 
  if (data_qof_pct$imd_score[i] > 8.49 & data_qof_pct$imd_score[i] <= 13.79 ){
    data_qof_pct$imd_score[i] = 2
  }
  if (data_qof_pct$imd_score[i] > 13.79 & data_qof_pct$imd_score[i] <= 21.35 ){
    data_qof_pct$imd_score[i] = 3
  }
  if (data_qof_pct$imd_score[i] > 21.35 & data_qof_pct$imd_score[i] <= 34.17 ){
    data_qof_pct$imd_score[i] = 4
  }
  if (data_qof_pct$imd_score[i] > 34.17){
    data_qof_pct$imd_score[i] = 5}
}

table(data_qof_pct$imd_score)
write.csv(data_qof_pct, "GP_allocation/resultant_data/gpprofileQOF2021_pct_pop1.08M_200822.csv", row.names = F)

rm(gp_geo_old, gp_profile_new, gp_modified_res, dataframe, i,path,sheets, data_symp_qof, gp_london)

#**************************************************************************** 

#**************************************************************************** 

data_qof_pct <- read.csv("GP_allocation/resultant_data/gpprofileQOF2021_pct_pop1.08M_200822.csv")

names(data_qof_pct)
data_qof_fselect <- data_qof_pct[c(-9,-11,-17,-27,-29)]


names(data_qof_fselect)
str(data_qof_fselect)

cols <- names(data_qof_fselect)[8:24]
data_qof_fselect[cols] <- lapply(data_qof_fselect[cols], as.numeric)

cols <- names(data_qof_fselect)[8:24]
data_qof_fselect[cols] <- lapply(data_qof_fselect[cols], function(cols) {cols/100})
data_qof_fselect[cols] <- lapply(data_qof_fselect[cols], function(cols) ceiling(cols * data_qof_fselect$gp_counts))

cols <- names(data_qof_fselect)[c(2)]

data_qof_fselect[cols] <- lapply(data_qof_fselect[cols], function(cols)cols/100)
data_qof_fselect[cols] <- lapply(data_qof_fselect[cols], function(cols) ceiling(cols * data_qof_fselect$gp_counts))


GP_profile_counts <-  data_qof_fselect %>%
  select(borough, AreaCode, Latitude, Longitude, gp_counts, everything() )

write.csv(GP_profile_counts, "GP_allocation/resultant_data/gpProfile_pplCOunts_200822.csv", row.names = F)





