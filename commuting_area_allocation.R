#******************************************************************
#*********** Find the distance between London boroughs **********
#******************************************************************

# install.packages("sf")
# install.packages("sp")
# install.packages("tmap")
# install.packages("spData")
library(tidyverse)
library(sf)
library(sp)
library(tmap)
library(spData)

#latitude and longitude for london boroughs (from spData package)
london <- lnd
View(lnd)

#find the centroid for london polygon

#lnd_pos = st_centroid(lnd)  #first way to find the centroid

lnd_pos = st_point_on_surface(lnd) #second way to find the centroid 


#Using tmap package (to plot)
tm_shape(lnd)+ #polygons geometry 
  tm_polygons()+ 
  tm_shape(lnd_pos)+ #centroid for each borough (points presentation)
  tm_symbols(col = "red", alpha = 0.5, size = 0.3) +
  tm_shape(lnd) + #add the labels from lnd (NAME variable)
  tm_text("NAME", size = 0.7,remove.overlap = T , just = c(0.6,1.5))+
  tm_layout(legend.outside = F,
            frame=FALSE,
            legend.text.size = 0.7,
            legend.title.size = 0.8,
            main.title.size = 1,
            legend.position = c("LEFT", "bottom"),
            outer.margins = 0, 
            inner.margins = 0)  #map scale.


#the distance between the boroughs' centroids (in m) // taking the earth curvature into account 
boroughs_disp <- (st_distance(lnd_pos)) 

units(boroughs_disp) <- ("km")  #convert the distance to km 
boroughs_disp <- as.data.frame(boroughs_disp) #convert to data frame 
#set the names as lnd data frame
names(boroughs_disp) <- lnd$NAME  
row.names(boroughs_disp) <- lnd$NAME

boroughs_disp <- boroughs_disp[-33, -33]

summary(boroughs_disp)

rm(london, lnd_pos)

write.csv(boroughs_disp, "commuting_area_simulation/displacement_boroughs_km_240822.csv", row.names = F)




#********************************************************
#****************Commuting calculations***************
#********************************************************

#Percent of commuters from all residence

residence <- read.csv("raw_data/place-residence-place-work-local-authorities-uk-2011.csv")



colnames(residence)[colnames(residence) == "Westminster,City of London"] <- "Westminster" 
colnames(residence)[1] <- "borough"
residence$borough[residence$borough == "Westminster,City of London"] <- "Westminster" 


#remove city of london
residence <- residence[-7,-8]

matrix_residence <-residence[,c(-1)]
matrix_residence <- as.matrix(matrix_residence)
same_borough_workers <- sum(diag(matrix_residence)) #sum the people who work in the same borough they live in (839315)
population <-  sum(matrix_residence) #sum all the population (2926149)

pct_sameBorough_workers <- same_borough_workers/population
pct_sameBorough_workers #0.29





#pct of commuters from the population = 0.71
data02 <- read.csv("GP_allocation/resultant_data/sim_london_gp_geo_200822.csv")
data02$covid_dt <- as.Date(data02$covid_dt, format = "%d/%m/%Y")
data02 <- arrange(data02, by=covid_dt)



str(data02)
set.seed(5000)
# <16 not commuters
data02$commuter <- ifelse(data02$age < 16, "F", "T" )

data02$patid <- seq(1:nrow(data02))

over_16 <- data02[data02$age >= 16, ]

over_16$over16_id <- seq(1:nrow(over_16))

over_16 <- over_16 %>%
  mutate (commuter_over16 = sample(x=c("F", "T"), size= 923063,
                            replace = T, prob= c(0.29, 0.71)))




table(over_16$commuter_over16)/nrow(over_16) #0.29 are not commuters

data02 <- data02 %>% left_join(over_16[c(29,59)], by= c("patid"))

data02$commuter <- ifelse(data02$age >= 16, data02$commuter_over16, data02$commuter)

table(data02$commuter)/nrow(data02) #0.39 are not commuters
table(data02$commuter_over16)

nrow(data02[data02$age >= 16 & data02$commuter == "T",])/ nrow(data02[data02$age>=16,]) #0.71


rm(over_16, pct_sameBorough_workers, population, same_borough_workers,matrix_residence)

###############################################
##############arrange the data ######################

data02$age <- as.integer(data02$age)

data02$catAge <- ifelse(data02$age < 16, 1 ,ifelse(
  data02$age >= 16 & data02$age <= 24 , 2, ifelse(
    data02$age >= 25 & data02$age <= 34 , 3, ifelse(
      data02$age >= 35 & data02$age <= 49, 4, ifelse(
        data02$age >= 50 & data02$age <= 64, 5, ifelse(
          data02$age >= 65 & data02$age <= 74, 6 , 7
        )
      )
    ) 
  )
)
)

names(data02)
table(data02$catAge)

(table(data02$catAge[data02$commuter=="T"],
       data02$Gender[data02$commuter=="T"])/nrow(data02[data02$commuter=="T",]))

#********************************************************
#****************Commuting areas simulation ***************
#********************************************************



#install.packages("sn")

library(sn)  #generate skewed data 

source("source_files/commuting_areas_probabilities.R")

source("source_files/assign_commuting_area.R")

#age category as an argument 
#The function will generate right skewed probabilities depending on agecat 
#prob_age1 <- skewed_prob_vec(1) #removed as commuters +16 only
prob_age2 <- skewed_prob_vec(2)
prob_age3 <- skewed_prob_vec(3)
prob_age4 <- skewed_prob_vec(4)
prob_age5 <- skewed_prob_vec(5)
prob_age6 <- skewed_prob_vec(6)
prob_age7 <- skewed_prob_vec(7)

#prbabilites of moving based on the number of commuters to each borough - from residence to work matrix

residence$total_borough <-rowSums(residence[2:ncol(residence)]) 

names(residence)[2:33] <- levels(as.factor(residence$borough))

commuting_prob_table <- residence %>%
  mutate(across(`Barking and Dagenham`:`Westminster`, ~ ./total_borough)) %>%
  select(-total_borough)

commuting_prob_table <- commuting_prob_table[, -1 ]
row.names(commuting_prob_table) <- residence$borough


#This function will distribute the people into commuting areas, it takes the patient data,
#the distance between boroughs data, commuting probability probability data, and the distance probability vector for each age range 
commuting_area <- commuting_function(data02,
                                     boroughs_disp,
                                     commuting_prob_table,
                                     prob_agecat2 = prob_age2,
                                     prob_agecat3 = prob_age3,
                                     prob_agecat4 = prob_age4,
                                     prob_agecat5 = prob_age5,
                                     prob_agecat6 = prob_age6,
                                     prob_agecat7 = prob_age7)



write.csv(commuting_area,
          "commuting_area_simulation/lon_geo_comm_250822.csv",
          row.names = FALSE)




#*************************************************************

#*************************************************************
#Similarity check -the distribution for the simulated data 

data_geo <- read.csv("commuting_area_simulation/lon_geo_comm_250822.csv", 
                     stringsAsFactors = TRUE)

names(data_geo)

data_geo <- data_geo[c(-4,-6, -9,-10,-28, -52, -54,-55,-56,-57,-58)]



str(data_geo)

cols <- names(data_geo)[-5]                        
data_geo[cols] <- lapply(data_geo[cols], function(x) as.factor(x))
str(data_geo)


rm(cols)



data_geo$covid_dt <- as.Date(data_geo$covid_dt, format = "%Y-%m-%d")
data_geo <- data_geo %>% arrange(covid_dt)




library(sf)
library(sp)
library(tmap)
library(spData)

#plot distribution of residents
sim_residence_data <- data_geo%>%
  group_by(borough)%>%
  summarise(count_resid = n()) %>%
  full_join(lnd, by = c(borough = "NAME"))%>%
  select(borough, count_resid, geometry) %>%
  mutate(pct_resid = (count_resid / sum(count_resid[-33]))*100)



sim_residence_data<- st_as_sf(sim_residence_data)
tm_shape(sim_residence_data) +
  tm_polygons(col = "pct_resid",
              legend.hist = F, style="cont", legend.show = FALSE)+
  tm_layout(legend.outside = TRUE) + #map scale.
  tm_shape(sim_residence_data) + #add the test labels from lnd (NAME variable)
  tm_text("borough", size = 0.55, col="black", fontface = 5, remove.overlap=T)+
  tm_layout(legend.outside = F,
            frame=FALSE,
            legend.text.size = 0.7,
            legend.title.size = 0.8,
            main.title.size = .01,
            legend.position = c("LEFT", "bottom"),
            outer.margins = 0, 
            inner.margins = 0,
  title.position = c('LEFT', 'top'),
  title.size = 1.2) + #map scale.
  tm_add_legend(type = "fill", 
                labels = c("<2%"," 2% to 3%", "3% to 4%", "4% to 5%",">5%", "missing"),
                col = c("#ffffd4", "#fed98e", "#fe9929", "#d95f0e", "#993404", "grey"),
                border.lwd = 0.5,
                title = "Percentage")



residence <- read.csv("raw_data/place-residence-place-work-local-authorities-uk-2011.csv")

residence$total_borough <-rowSums(residence[2:ncol(residence)]) 




#plot spatial distrobution of sumulated data.  

sim_commuting_data <- data_geo%>%
  group_by(commuting_area)%>%
  summarise(count_com = n()) %>%
  full_join(lnd, by = c(commuting_area = "NAME"))%>%
  select(commuting_area, count_com, geometry)%>%
  mutate(pct_commut = (count_com / sum(count_com[-33]))*100)

sim_commuting_data<- st_as_sf(sim_commuting_data)


tm_shape(sim_commuting_data) +
  tm_polygons(col = "pct_commut",
              legend.hist = F, style="fixed",
              breaks=c(0,2,4,6,8,10,24),
              legend.show = T,palette = "cividis", n = 6, contrast = c(0.01, 0.89))+
  tm_layout(legend.outside = TRUE) + #map scale.
  tm_shape(sim_commuting_data) + #add the test labels from lnd (NAME variable)
  tm_text("commuting_area", size = 0.55, col="#FBE0D0", fontface = 1, remove.overlap=T)+
  tm_layout(legend.outside = F,
            frame=FALSE,
            legend.text.size = 0.7,
            legend.title.size = 0.8,
            main.title.size = .01,
            legend.position = c("LEFT", "bottom"),
            outer.margins = 0, 
            inner.margins = 0,
            title.position = c('LEFT', 'top'),
            title.size = 1.2)

#plot the spatial distribution of commuters

residence <- residence[,-35]
cols <- names(residence)[2:34]
commuting_boroughs_sum <- lapply(residence[cols], function(x) sum(x))
commuting_boroughs_sum <- as.data.frame(commuting_boroughs_sum) #columns sum 


residence_sum_col <- commuting_boroughs_sum %>% 
  rownames_to_column() %>% 
  gather(variable, value, -rowname) %>% 
  spread(rowname, value)%>%
  rename(borough = variable, commuting_count = `1`)

rm(commuting_boroughs_sum, cols)
#arrange comumns names
residence_sum_col$borough = gsub("\\.", " ", residence_sum_col$borough)


#join with location geometry
real_commuting_data <- residence_sum_col%>%
  full_join(lnd, by = c(borough = "NAME"))%>%
  select(borough, commuting_count, geometry)
real_commuting_data$commuting_count[real_commuting_data$borough == "Westminster"] <- 675876
real_commuting_data <- real_commuting_data[-33, ]

real_commuting_data <- real_commuting_data%>%
  mutate(pct_commut = (commuting_count / sum(commuting_count))*100)

real_commuting_data$pct_commut[real_commuting_data$borough == "City of London"] <- NA

real_commuting_data<- st_as_sf(real_commuting_data)

tm_shape(real_commuting_data) +
tm_polygons(col = "pct_commut",
            legend.hist = F, style="fixed",
             breaks=c(0,2,4,6,8,10,24),
            legend.show = T,
            palette = "cividis", n = 6, contrast = c(0.01, 0.89))+
  tm_layout(legend.outside = TRUE) + #map scale.
  tm_shape(real_commuting_data) + #add the test labels from lnd (NAME variable)
  tm_text("borough", size = 0.55, col="#FBE0D0", fontface = 5, remove.overlap=T)+
  tm_layout(legend.outside = F,
            frame=FALSE,
            legend.text.size = 0.7,
            legend.title.size = 0.8,
            main.title.size = .01,
            legend.position = c("LEFT", "bottom"),
            outer.margins = 0, 
            inner.margins = 0, 
            title.position = c('LEFT', 'top'),
            title.size = 1.2) 





rm(residence_sum_col, real_commuting_data, sim_commuting_data, sim_residence_data, 
   real_residence_data)
#check the distribution of people on each location 
#Transpose residence dataset , col = place of residence, rows ==> place of work 





###09/08/2022 Last edit 
## Continue by checking the distribution of poeople commuting from each borough 


#finds the pct of people travelling to each borough 
pct_transpose <- function(data,col_names){
  lapply(data[col_names], function(col_names) (as.numeric(col_names) / sum(as.numeric(col_names)))*100)
}

#matrix transpose(converts row to columns and vice versa)
residence_transpose <- residence %>% 
  rownames_to_column() %>% 
  gather(variable, value, -rowname) %>% 
  spread(rowname, value)

#arrange  data
names(residence_transpose) <- residence_transpose[31, ]
residence_transpose <- residence_transpose[-31, ]
residence_transpose <- rename(residence_transpose,
                              `1borough` = "usual.residence...2011.census.merged.local.authority.district")%>%
  select(order(colnames(.)))%>%
  rename(borough = `1borough`)
residence_transpose$borough <- gsub("\\.", " ",  residence_transpose$borough)
#residence_transpose[2:34] <- order(residence_transpose[2:34])


#finds the pct from in matrix
cols <- names(residence_transpose)[2:34]
pct_real_residenceTr <-(pct_transpose(residence_transpose, cols))
pct_real_residenceTr <- as.data.frame(pct_real_residenceTr)
pct_real_residenceTr$borough <- residence_transpose$borough
pct_real_residenceTr <- pct_real_residenceTr[-7, -7]

#join with geolocation geometry
pct_real_residenceTr <- pct_real_residenceTr%>%
  full_join(lnd[c(1,8)], by = c(borough = "NAME"))



#dara arrage
pct_real_residenceTr$geometry[pct_real_residenceTr$borough == "Westminster City of London"] <- pct_real_residenceTr$geometry[33]

pct_real_residenceTr <- pct_real_residenceTr[-33, ]

#pct_real_residenceTr<- st_as_sf(pct_real_residenceTr)



data <- read.csv("raw_data/2021-04-14_Synthetic_Of_2021-04-14_GTNegAndPositives(1).csv")

#generate commting based on work place matrix from simulated data(counts)
sim_data_counts <- data_geo %>%
  group_by(borough, commuting_area) %>%
  summarise(countx = n())%>%
  spread(key = commuting_area, value = countx, fill = 0 ) %>%
  rownames_to_column() %>% 
  gather(variable, value, -rowname) %>% 
  spread(rowname, value)

head(sim_data_counts)
names(sim_data_counts) <- sim_data_counts[4,]
sim_data_counts <- sim_data_counts[-4,]

#pct of commuting based on the location of residence for simulated data
pct_sim_residenceTr <- pct_transpose(sim_data_counts, names(sim_data_counts)[2:33])

pct_sim_residenceTr <- as.data.frame(pct_sim_residenceTr) %>%
  select(order(colnames(.)))

pct_sim_residenceTr$borough <- sim_data_counts$borough
#join with geolocation geometry
pct_sim_residenceTr <- pct_sim_residenceTr%>%
  full_join(lnd[c(1,8)], by = c(borough = "NAME"))















#This function will plot the distribution of commuting area for the resident in each each borough 
#for both real and simulated data 
#It will save the images (do not run it again!)
for(i in 1: (ncol(pct_real_residenceTr)-2)){
  #Extract pct of commuters from real and simulated data
  data_real <- data.frame(commuting_loc = pct_real_residenceTr[, 33],
                          residence_loc = pct_real_residenceTr[,i], 
                          simulated_residence_loc = pct_sim_residenceTr[,i],
                          geometry = pct_real_residenceTr[,34])
  
  residence_area <- names(pct_real_residenceTr)[i] #for both simulated and real
  
  curr_location <- data_real[i,]
  data_real <- data_real[-i,]  
  # 
  #convert it ti geospatial data
  data_real<- st_as_sf(data_real)
  curr_location<- st_as_sf(curr_location)
  print(residence_area)
  

  
 map <- tm_shape(data_real) +
    tm_polygons(c("residence_loc", "simulated_residence_loc"), palette = "-Blues", n = 5,contrast = c(0.01, 0.89),
                legend.hist = F,title = c("real", "Sim"), 
                style="fixed",
                breaks=c(0,3,5, 10,15,25),
                legend.show = F)+
    tm_layout(legend.outside = TRUE) +
   
   tm_shape(curr_location) + #add the test labels from lnd (NAME variable)
   tm_polygons("residence_loc", palette = "inferno", legend.show = F)+ #map scale.
   
   tm_shape(curr_location) + #add the test labels from lnd (NAME variable)
   tm_text("commuting_loc", size = 0.8, col="#F7F7F7", fontface = 5, remove.overlap=T)+
   
    tm_layout(legend.outside = F,
              frame=FALSE,
              legend.text.size = 0.7,
              legend.title.size = 0.8,
              main.title.size = .01,
              legend.position = c("LEFT", "bottom"),
              outer.margins = 0, 
              inner.margins = 0,
              title.position = c('LEFT', 'top'),
              title.size = 1.2) 
  
 
 tmap_save(map, filename = paste("commuting_area_simulation/results_exlresidenceplaceor/figure_", residence_area, "or.png"),
                                 
                   width = 7, height = 3, dpi = 150, units = "in")
  

}


rm(cols, i, residence_area, sim_commuting_dist, real_commuting_dist, figure, 
   data_sim, data_real, pct_transpose, sim_data_counts, pct_real_residenceTr,
   pct_sim_residenceTr, residence, residence_transpose)














#