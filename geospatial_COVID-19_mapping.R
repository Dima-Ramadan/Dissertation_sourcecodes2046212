data <- read.csv("commuting_area_simulation/lon_geo_comm_250822.csv")

cols <- names(data)[-5]
data[cols] <- lapply(data[cols],  as.factor)

data$covid_dt <- as.Date(data$covid_dt, format = "%Y-%m-%d")

str(data$covid_dt)

library(tidyverse)
#arrange the data by date
data <- data %>% 
  arrange(covid_dt)

#write.csv(data, "../London_Region_DA300722/drift_detection/lon_data_0.5M.csv", row.names = FALSE)
names(data)
#library(lubridate)
data <- data[, c(-2, -4,-6,-9,-10,-28,-29,-50,-52,-56,-57,-58)]
# data$covid_dt <- as.Date(data$covid_dt, format = "%d/%m/%Y")

table(data$cut_Date)
str(data)


data_frame_age <- data.frame("population","ageCat1", "ageCat1_pct","ageCat2", "ageCat2_pct",
                             "ageCat3", "ageCat3_pct","ageCat4", "ageCat4_pct")

age_cat_summary <- data %>%
  group_by(borough, catAge) %>%
  summarise(population = n())%>%
  spread(key = catAge, value = population)%>%
  rowwise()%>%
  mutate(total_population = sum(c_across(`1`:`4`))) %>%
  mutate(agecat1 = `1`/total_population, agecat2 = `2`/total_population,
         agecat3 = `3`/total_population,
         agecat4 = `4`/total_population
         )%>%
  gather(key = ageCat, pct_population, agecat1:agecat4)



 ggplot(age_cat_summary , aes(ageCat, pct_population)) + 
  geom_col(aes(y =pct_population)) +
  facet_wrap(~borough, ncol = 4)+
   labs(x = "\n London's Boroughs",
        y = "% of age \n", 
        title = "\n Percentage of Age Category From the Borough Population \n")+ 
   theme(plot.title = element_text(hjust = 0.5),
         axis.title.x = element_text(face="bold",  size = 12),
         axis.title.y = element_text(face="bold", size = 12),
         legend.title = element_text(face="bold", size = 10))

#age_disribution_checking_to_useincruderate

#crude_rate_100000 <- (number_diag / total_population_borough) * 100,000


crude_rate_summary <- data %>%
  group_by(borough, covid_diagnosis)%>%
  summarise(diagnosis_status = n())%>%
  spread(key = covid_diagnosis, value = diagnosis_status)%>%
  rowwise()%>%
  mutate(total_population = sum(c_across(`0`:`1`))) %>%
  select(-`0`)%>%
  mutate(pct_diag_borough = `1`/ total_population)%>%
  mutate(diagnosis_rate_per_100000 = ceiling(pct_diag_borough * 100000))



library(sf)
library(sp)
library(tmap)
library(spData)

#install.packages(c("spatstat", "maps", "maptools" ))
#library(spatstat)
#library(maps)
#library(maptools)


residence_crude_rate <- crude_rate_summary %>%
  full_join(lnd, by = c(borough = "NAME"))%>%
  select(borough, diagnosis_rate_per_100000,geometry)%>%
  filter(borough != "City of London")


residence_crude_rate_sp <-  st_as_sf(residence_crude_rate)


#-inferno n-5 
lon_resid <- tm_shape(residence_crude_rate_sp) +
  tm_polygons(col = "diagnosis_rate_per_100000",
              legend.hist = FALSE, style="cont",
              title = "COVID-19 cases \n per 100,000",
              
              #  breaks = seq(110, 620, by = 100),
              
              legend.show = T,
              palette = "-cividis", n = 5, contrast = c(0.0, 1),
              # breaks = seq(1000, 2000, by = 200)
  ) +
  tm_layout(legend.outside = F,
            frame=FALSE,
            legend.text.size = 0.7,
            legend.title.size = 0.8,
#                     main.title =" Covid-19 Diagnosis Rate Per 100,000 
 #                      MAy 2020 -comm",
            main.title.size = 1,
            legend.position = c("LEFT", "bottom"),
            outer.margins = 0, 
            inner.margins = c(0,0,0,0),
            outer.bg.color = "black") + #map scale.
  tm_shape(residence_crude_rate_sp, 
  ) + #add the test labels from lnd (NAME variable)
  tm_text("borough", size = 0.6, col ="#1a1300")


tmap_save(lon_resid, "geospatial_plotting/london_resid.png")








#######################



#Incident rate: rate of new cases in a particular time span 
#incident rate = number_of_newcases / total_number of population during all time spans(mid of the year census)
#incident rate per 100000 = incident rate * 100000


#May data$cut_Date
may_crudeRate <- data %>%
  filter(covid_dt < "2020-06-01" & covid_dt>="2020-05-01") %>%
  group_by(borough, covid_diagnosis) %>%
  summarise(diagnosis_status = n()) %>%
  spread(key = covid_diagnosis, value = diagnosis_status)%>%
  rowwise()%>%
  full_join(crude_rate_summary[c(1,3)], by = c("borough")) %>%
  select(-`0`)%>%
  mutate(pct_diag_borough = `1`/ total_population)%>%
  mutate(diagnosis_rate_per_100000 = ceiling(pct_diag_borough * 100000))




may_crudeRate <- may_crudeRate %>%
  full_join(lnd, by = c(borough = "NAME"))%>%
  select(borough, diagnosis_rate_per_100000,geometry)%>%
  filter(borough != "City of London")



boroughs_of_interestmay = may_crudeRate %>%
  filter(borough %in% c("Croydon", "Enfield", 
                        "Hammersmith and Fulham","Haringey",
                        "Wandsworth"))


#breaks = seq(160, 660, by = 100),

may_crudeRate_sp <-  st_as_sf(may_crudeRate)
boroughs_of_interestmay<-  st_as_sf(boroughs_of_interestmay)

map_may <- tm_shape(may_crudeRate_sp) +
  tm_polygons(col = "diagnosis_rate_per_100000",
              legend.hist = F, style="cont",
              #  breaks=c(0,2,4,6,8,10,24),
              legend.show = F,
              palette = "-cividis", n = 5, contrast = c(0, 1),
              breaks = seq(110, 660, by = 50)
  ) +
  tm_layout(legend.outside = F,
            frame=FALSE,
            legend.text.size = 0.7,
            legend.title.size = 0.8,
            # main.title =" Covid-19 Diagnosis Rate Per 100,000 
            #December 2020",
            main.title.size = 1,
            legend.position = c("LEFT", "bottom"),
            outer.margins = 0, 
            inner.margins = 0) + #map scale.
  tm_shape(may_crudeRate_sp, 
  ) + #add the test labels from lnd (NAME variable)
  tm_text("borough", size = 0.6, col ="#1a1300")+#, col = "#5c8a8a")+
  tm_layout()+
  tm_shape(boroughs_of_interestmay) +
  tm_borders(col="red", lwd=1.8)



map_may
tmap_save(map_may, "geospatial_plotting/may_resid.png")

#######end may #######

#####################################################
##########June and July ########################
##################################################


june_crudeRate <- data %>%
  filter(covid_dt < "2020-07-01" & covid_dt>="2020-06-01") %>%
  group_by(borough, covid_diagnosis) %>%
  summarise(diagnosis_status = n()) %>%
  spread(key = covid_diagnosis, value = diagnosis_status)%>%
  rowwise()%>%
  full_join(crude_rate_summary[c(1,3)], by = c("borough")) %>%
  select(-`0`)%>%
  mutate(pct_diag_borough = `1`/ total_population)%>%
  mutate(diagnosis_rate_per_100000 = ceiling(pct_diag_borough * 100000))




june_crudeRate <- june_crudeRate %>%
  full_join(lnd, by = c(borough = "NAME"))%>%
  select(borough, diagnosis_rate_per_100000,geometry)%>%
  filter(borough != "City of London")



boroughs_of_interestjune = june_crudeRate %>%
  filter(borough %in% c("Croydon"))



june_crudeRate_sp <-  st_as_sf(june_crudeRate)
boroughs_of_interestjune<-  st_as_sf(boroughs_of_interestjune)
map_june <- tm_shape(june_crudeRate_sp) +
  tm_polygons(col = "diagnosis_rate_per_100000",
              legend.hist = F, style="cont",
              #  breaks=c(0,2,4,6,8,10,24),
              legend.show = F,
              palette = "-cividis", n = 5, contrast = c(0, 1),
              breaks = seq(100, 660, by = 50),
  ) +
  tm_layout(legend.outside = F,
            frame=FALSE,
            legend.text.size = 0.7,
            legend.title.size = 0.8,
            # main.title =" Covid-19 Diagnosis Rate Per 100,000 
            #August 2020",
            main.title.size = 1,
            legend.position = c("LEFT", "bottom"),
            outer.margins = 0, 
            inner.margins = 0) + #map scale.
  tm_shape(june_crudeRate_sp, 
  ) + #add the test labels from lnd (NAME variable)
  tm_text("borough", size = 0.6, col ="#1a1300")+#, col = "#5c8a8a")+
  tm_layout()+
  tm_shape(boroughs_of_interestjune) +
  tm_borders(col="red", lwd=1.8)




map_june
tmap_save(map_june, "geospatial_plotting/june_resid.png")




#July

#data$cut_Date
july_crudeRate <- data %>%
  filter(covid_dt < "2020-08-01" & covid_dt>="2020-07-01") %>%
  group_by(borough, covid_diagnosis) %>%
  summarise(diagnosis_status = n()) %>%
  spread(key = covid_diagnosis, value = diagnosis_status)%>%
  rowwise()%>%
  full_join(crude_rate_summary[c(1,3)], by = c("borough")) %>%
  select(-`0`)%>%
  mutate(pct_diag_borough = `1`/ total_population)%>%
  mutate(diagnosis_rate_per_100000 = ceiling(pct_diag_borough * 100000))




july_crudeRate <- july_crudeRate %>%
  full_join(lnd, by = c(borough = "NAME"))%>%
  select(borough, diagnosis_rate_per_100000,geometry)%>%
  filter(borough != "City of London")

boroughs_of_interestjul = july_crudeRate %>%
  filter(borough %in% c( "Enfield","Croydon", "Haringey",
                         "Newham"))

july_crudeRate_sp <-  st_as_sf(july_crudeRate)

boroughs_of_interestjul <- st_as_sf(boroughs_of_interestjul)


map_july <- tm_shape(july_crudeRate_sp) +
  tm_polygons(col = "diagnosis_rate_per_100000",
              legend.hist = FALSE, style="cont",
              breaks = seq(100, 660, by = 50),
              title = "COVID-19 cases \n per 100,000",
              legend.show = F,
              palette = "-cividis", n = 5, contrast = c(0.0, 0.95),
              # breaks = seq(1000, 2000, by = 200)
  ) +
  tm_layout(legend.outside = F,
            frame=FALSE,
            legend.text.size = 0.7,
            legend.title.size = 0.8,
            #main.title =" Covid-19 Diagnosis Rate Per 100,000 
            #September 2020",
            main.title.size = 1,
            legend.position = c("RIGHT", "bottom"),
            outer.margins = 0, 
            inner.margins = c(0,0,0,0),
            outer.bg.color = "black") + #map scale.
  tm_shape(july_crudeRate_sp, 
  ) + #add the test labels from lnd (NAME variable)
  tm_text("borough", size = 0.6, col ="#1a1300")+#, col = "#5c8a8a")+
  tm_shape(boroughs_of_interestjul) +
  tm_borders(col="red", lwd=1.8)

map_july
#par(oma=c(0,0,10,0)) 
#tmap_arrange(map_before, map_after, sync = T)



tmap_save(map_july, "geospatial_plotting/july_resid.png")

########end july####################

############start august####################

aug_crudeRate <- data %>%
  filter(covid_dt < "2020-09-01" & covid_dt>="2020-08-01") %>%
  group_by(borough, covid_diagnosis) %>%
  summarise(diagnosis_status = n()) %>%
  spread(key = covid_diagnosis, value = diagnosis_status)%>%
  rowwise()%>%
  full_join(crude_rate_summary[c(1,3)], by = c("borough")) %>%
  select(-`0`)%>%
  mutate(pct_diag_borough = `1`/ total_population)%>%
  mutate(diagnosis_rate_per_100000 = ceiling(pct_diag_borough * 100000))




aug_crudeRate <- aug_crudeRate %>%
  full_join(lnd, by = c(borough = "NAME"))%>%
  select(borough, diagnosis_rate_per_100000,geometry)%>%
  filter(borough != "City of London")



boroughs_of_interestaug = aug_crudeRate %>%
  filter(borough %in% c("Barnet","Brent", "Bromley",
                         "Enfield",  "Haringey",
                         "Hounslow",
                        "Newham"
                        ))



aug_crudeRate_sp <-  st_as_sf(aug_crudeRate)
boroughs_of_interestaug<-  st_as_sf(boroughs_of_interestaug)
map_aug <- tm_shape(aug_crudeRate_sp) +
  tm_polygons(col = "diagnosis_rate_per_100000",
              legend.hist = F, style="cont",
              #  breaks=c(0,2,4,6,8,10,24),
              legend.show = F,
              palette = "-cividis", n = 5, contrast = c(0, 0.95),
              breaks = seq(100, 660, by = 50),
  ) +
  tm_layout(legend.outside = F,
            frame=FALSE,
            legend.text.size = 0.7,
            legend.title.size = 0.8,
            # main.title =" Covid-19 Diagnosis Rate Per 100,000 
            #August 2020",
            main.title.size = 1,
            legend.position = c("LEFT", "bottom"),
            outer.margins = 0, 
            inner.margins = 0) + #map scale.
  tm_shape(aug_crudeRate_sp, 
  ) + #add the test labels from lnd (NAME variable)
  tm_text("borough", size = 0.6, col ="#1a1300")+#, col = "#5c8a8a")+
  tm_layout()+
  tm_shape(boroughs_of_interestaug) +
  tm_borders(col="red", lwd=1.8)




map_aug
tmap_save(map_aug, "geospatial_plotting/aug_resid.png")




#September

#data$cut_Date
sep_crudeRate <- data %>%
  filter(covid_dt < "2020-10-01" & covid_dt>="2020-09-01") %>%
  group_by(borough, covid_diagnosis) %>%
  summarise(diagnosis_status = n()) %>%
  spread(key = covid_diagnosis, value = diagnosis_status)%>%
  rowwise()%>%
  full_join(crude_rate_summary[c(1,3)], by = c("borough")) %>%
  select(-`0`)%>%
  mutate(pct_diag_borough = `1`/ total_population)%>%
  mutate(diagnosis_rate_per_100000 = ceiling(pct_diag_borough * 100000))




sep_crudeRate <- sep_crudeRate %>%
  full_join(lnd, by = c(borough = "NAME"))%>%
  select(borough, diagnosis_rate_per_100000,geometry)%>%
  filter(borough != "City of London")



boroughs_of_sep = sep_crudeRate %>%
  filter(borough %in% c("Barnet","Brent", "Bromley", "camden",
                        "Croydon", "Enfield", "Greenwich",  "Hammersmith and Fulham",
                        "Haringey","Harrow",
                        "Havering",
                        "Kensington and Chelsea", "Lewisham", "Newham", "Redbridge", 
                        "Southwark", "Sutton", "Tower Hamlets",
                        "Waltham Forest", "Wandsworth", "Westminster"))





sep_crudeRate_sp <-  st_as_sf(sep_crudeRate)
boroughs_of_sep <-  st_as_sf(boroughs_of_sep)



map_sep <- tm_shape(sep_crudeRate_sp) +
  tm_polygons(col = "diagnosis_rate_per_100000",
              legend.hist = FALSE, style="cont",
              breaks = seq(100, 660, by = 50),
              title = "COVID-19 cases \n per 100,000",
              legend.show = T,
              palette = "-cividis", n = 5, contrast = c(0, 1),
              # breaks = seq(1000, 2000, by = 200)
  ) +
  tm_layout(legend.outside = F,
            frame=FALSE,
            legend.text.size = 0.7,
            legend.title.size = 0.8,
            #main.title =" Covid-19 Diagnosis Rate Per 100,000 
            #September 2020",
            main.title.size = 1,
            legend.position = c("LEFT", "bottom"),
            outer.margins = 0, 
            inner.margins = c(0,0,0,0),
            outer.bg.color = "black") + #map scale.
  tm_shape(sep_crudeRate_sp, 
  ) + #add the test labels from lnd (NAME variable)
  tm_text("borough", size = 0.6, col ="#1a1300")+#, col = "#5c8a8a")+
  tm_shape(boroughs_of_sep) +
  tm_borders(col="red", lwd=1.8)

map_sep
#par(oma=c(0,0,10,0)) 
#tmap_arrange(map_before, map_after, sync = T)



tmap_save(map_sep, "geospatial_plotting/sep_resid.png")







#October

#data$cut_Date
oct_crudeRate <- data %>%
  filter(covid_dt < "2020-11-01" & covid_dt>="2020-10-01") %>%
  group_by(borough, covid_diagnosis) %>%
  summarise(diagnosis_status = n()) %>%
  spread(key = covid_diagnosis, value = diagnosis_status)%>%
  rowwise()%>%
  full_join(crude_rate_summary[c(1,3)], by = c("borough")) %>%
  select(-`0`)%>%
  mutate(pct_diag_borough = `1`/ total_population)%>%
  mutate(diagnosis_rate_per_100000 = ceiling(pct_diag_borough * 100000))




oct_crudeRate <- oct_crudeRate %>%
  full_join(lnd, by = c(borough = "NAME"))%>%
  select(borough, diagnosis_rate_per_100000,geometry)%>%
  filter(borough != "City of London")



boroughs_of_oct = oct_crudeRate %>%
  filter(borough %in% c("Bromley", "camden",
                        "Croydon", "Enfield","Hackney", 
                        "Haringey","Harrow",
                        "Havering",
                          "Lewisham", "Newham", "Redbridge", 
                        "Southwark", "Sutton", "Tower Hamlets",
                        "Waltham Forest", "Westminster"))








oct_crudeRate_sp <-  st_as_sf(oct_crudeRate)
boroughs_of_oct <-  st_as_sf(boroughs_of_oct)



map_oct <- tm_shape(oct_crudeRate_sp) +
  tm_polygons(col = "diagnosis_rate_per_100000",
              legend.hist = FALSE, style="cont",
              breaks = seq(100, 660, by = 50),
              
              legend.show = F,
              palette = "-cividis", n = 5, contrast = c(0.0, 1),
              # breaks = seq(1000, 2000, by = 200)
  ) +
  tm_layout(legend.outside = F,
            frame=FALSE,
            legend.text.size = 0.7,
            legend.title.size = 0.8,
            #            main.title =" Covid-19 Diagnosis Rate Per 100,000 
            #           October 2020",
            main.title.size = 1,
            legend.position = c("LEFT", "bottom"),
            outer.margins = 0, 
            inner.margins = c(0,0,0,0),
            outer.bg.color = "black") + #map scale.
  tm_shape(oct_crudeRate_sp, 
  ) + #add the test labels from lnd (NAME variable)
  tm_text("borough", size = 0.6, col ="#1a1300")+
  tm_shape(boroughs_of_oct) +
  tm_borders(col="red", lwd=1.8)

#par(oma=c(0,0,10,0)) 
#tmap_arrange(map_before, map_after, sync = T)

map_oct

tmap_save(map_oct, "geospatial_plotting/october_resid.png")


#######November ###################


#November

#data$cut_Date
nov_crudeRate <- data %>%
  filter(covid_dt < "2020-12-01" & covid_dt>="2020-11-01") %>%
  group_by(borough, covid_diagnosis) %>%
  summarise(diagnosis_status = n()) %>%
  spread(key = covid_diagnosis, value = diagnosis_status)%>%
  rowwise()%>%
  full_join(crude_rate_summary[c(1,3)], by = c("borough")) %>%
  select(-`0`)%>%
  mutate(pct_diag_borough = `1`/ total_population)%>%
  mutate(diagnosis_rate_per_100000 = ceiling(pct_diag_borough * 100000))




nov_crudeRate <- nov_crudeRate %>%
  full_join(lnd, by = c(borough = "NAME"))%>%
  select(borough, diagnosis_rate_per_100000,geometry)%>%
  filter(borough != "City of London")



boroughs_of_nov = nov_crudeRate %>%
  filter(borough %in% c("Brent", "Bromley", "camden",
                        "Croydon", "Enfield", "Hackney","Hammersmith and Fulham",
                        "Haringey","Harrow",
                        "Havering",
                        "Lewisham", "Newham", "Richmond upon Rhames",
                        "Southwark", "Sutton", "Tower Hamlets",
                        "Waltham Forest", "Westminster"))





nov_crudeRate_sp <-  st_as_sf(nov_crudeRate)
boroughs_of_nov <-  st_as_sf(boroughs_of_nov)



map_nov <- tm_shape(nov_crudeRate_sp) +
  tm_polygons(col = "diagnosis_rate_per_100000",
              legend.hist = FALSE, style="cont",
              breaks = seq(100, 660, by = 50),
              title = "COVID-19 cases \n per 100,000",
              legend.show = F,
              palette = "-cividis", n = 5, contrast = c(0, 1),
              # breaks = seq(1000, 2000, by = 200)
  ) +
  tm_layout(legend.outside = F,
            frame=FALSE,
            legend.text.size = 0.7,
            legend.title.size = 0.8,
            #main.title =" Covid-19 Diagnosis Rate Per 100,000 
            #September 2020",
            main.title.size = 1,
            legend.position = c("LEFT", "bottom"),
            outer.margins = 0, 
            inner.margins = c(0,0,0,0),
            outer.bg.color = "black") + #map scale.
  tm_shape(nov_crudeRate_sp, 
  ) + #add the test labels from lnd (NAME variable)
  tm_text("borough", size = 0.6, col ="#1a1300")+#, col = "#5c8a8a")+
  tm_shape(boroughs_of_nov) +
  tm_borders(col="red", lwd=1.8)

map_nov
#par(oma=c(0,0,10,0)) 
#tmap_arrange(map_before, map_after, sync = T)



tmap_save(map_nov, "geospatial_plotting/november_resid.png")

#######EnD November ###################

#December

data$cut_Date
last2weeks_crudeRate <- data %>%
  filter(covid_dt < "2021-01-01" & covid_dt>="2020-12-01") %>%
  group_by(borough, covid_diagnosis) %>%
  summarise(diagnosis_status = n()) %>%
  spread(key = covid_diagnosis, value = diagnosis_status)%>%
  rowwise()%>%
  full_join(crude_rate_summary[c(1,3)], by = c("borough")) %>%
  select(-`0`)%>%
  mutate(pct_diag_borough = `1`/ total_population)%>%
  mutate(diagnosis_rate_per_100000 = ceiling(pct_diag_borough * 100000))




last2_crudeRate <- last2weeks_crudeRate %>%
  full_join(lnd, by = c(borough = "NAME"))%>%
  select(borough, diagnosis_rate_per_100000,geometry)%>%
  filter(borough != "City of London")




boroughs_of_interestdec = last2_crudeRate %>%
  filter(borough %in% c("Barnet", "Brent",
                        "Camden",
                        "Enfield", "Hackney", "Haringey",
                        "Harrow", "Hounslow",
                        "Newham", "Redbridge", 
                        "Richmond upon Thames",
                        "Southwark", "Sutton", 
                        "Westminster"))

#breaks = seq(160, 660, by = 100),

last2_crudeRate_sp <-  st_as_sf(last2_crudeRate)
boroughs_of_interestdec<-  st_as_sf(boroughs_of_interestdec)
map_before <- tm_shape(last2_crudeRate_sp) +
  tm_polygons(col = "diagnosis_rate_per_100000",
              legend.hist = F, style="cont",
              #  breaks=c(0,2,4,6,8,10,24),
              legend.show = F,
              palette = "-cividis", n = 5, contrast = c(0, 1),
              breaks = seq(110, 660, by = 50)
  ) +
  tm_layout(legend.outside = F,
            frame=FALSE,
            legend.text.size = 0.7,
            legend.title.size = 0.8,
           # main.title =" Covid-19 Diagnosis Rate Per 100,000 
            #December 2020",
            main.title.size = 1,
            legend.position = c("LEFT", "bottom"),
            outer.margins = 0, 
            inner.margins = 0) + #map scale.
  tm_shape(last2_crudeRate_sp, 
  ) + #add the test labels from lnd (NAME variable)
  tm_text("borough", size = 0.6, col ="#1a1300")+#, col = "#5c8a8a")+
  tm_layout()+
  tm_shape(boroughs_of_interestdec) +
  tm_borders(col="red", lwd=1.8)





map_before
tmap_save(map_before, "geospatial_plotting/december_resid.png")




#JAnuary

#data$cut_Date
jan_crudeRate <- data %>%
  filter(covid_dt < "2021-02-01" & covid_dt>="2021-01-01") %>%
  group_by(borough, covid_diagnosis) %>%
  summarise(diagnosis_status = n()) %>%
  spread(key = covid_diagnosis, value = diagnosis_status)%>%
  rowwise()%>%
  full_join(crude_rate_summary[c(1,3)], by = c("borough")) %>%
  select(-`0`)%>%
  mutate(pct_diag_borough = `1`/ total_population)%>%
  mutate(diagnosis_rate_per_100000 = ceiling(pct_diag_borough * 100000))




jan_crudeRate <- jan_crudeRate %>%
  full_join(lnd, by = c(borough = "NAME"))%>%
  select(borough, diagnosis_rate_per_100000,geometry)%>%
  filter(borough != "City of London")



boroughs_of_interestjan = jan_crudeRate %>%
  filter(borough %in% c("Barking and Dagenham", "Brent", "Bromley",
                        "Croydon", "Enfield", "Hackney",
                        "Harrow", "Hounslow",
                        "Kensington and Chelsea",  
                        "Newham", "Redbridge", 
                        "Richmond upon Thames", "Southwark", 
                        "Sutton", 
                        "Waltham Forest",  "Westminster"))




jan_crudeRate_sp <-  st_as_sf(jan_crudeRate)
boroughs_of_interestjan <-  st_as_sf(boroughs_of_interestjan)



map_jan <- tm_shape(jan_crudeRate_sp) +
  tm_polygons(col = "diagnosis_rate_per_100000",
              legend.hist = FALSE, style="cont",
              breaks = seq(110, 660, by = 50),
              title = "COVID-19 cases \n per 100,000",
              legend.show = F,
              palette = "-cividis", n = 5, contrast = c(0, 1),
              #breaks = seq(1000, 2000, by = 200)
  ) +
  tm_layout(legend.outside = F,
            frame=FALSE,
            legend.text.size = 0.7,
            legend.title.size = 0.8,
            #main.title =" Covid-19 Diagnosis Rate Per 100,000 
            #January 2021",
            main.title.size = 1,
            legend.position = c("LEFT", "bottom"),
            outer.margins = 0, 
            inner.margins = c(0,0,0,0),
            outer.bg.color = "black") + #map scale.
  tm_shape(jan_crudeRate_sp, 
  ) + #add the test labels from lnd (NAME variable)
  tm_text("borough", size = 0.6, col = "#1a1300")+
  tm_shape(boroughs_of_interestjan) +
  tm_borders(col="red", lwd=1.8)

map_jan
#par(oma=c(0,0,10,0)) 
#tmap_arrange(map_before, map_after, sync = T)



tmap_save(map_jan, "geospatial_plotting/january_resid.png")







#February

#data$cut_Date
nxt2weeks_crudeRate <- data %>%
  filter(covid_dt < "2021-03-01" & covid_dt>="2021-02-01") %>%
  group_by(borough, covid_diagnosis) %>%
  summarise(diagnosis_status = n()) %>%
  spread(key = covid_diagnosis, value = diagnosis_status)%>%
  rowwise()%>%
  full_join(crude_rate_summary[c(1,3)], by = c("borough")) %>%
  select(-`0`)%>%
  mutate(pct_diag_borough = `1`/ total_population)%>%
  mutate(diagnosis_rate_per_100000 = ceiling(pct_diag_borough * 100000))




nxt2_crudeRate <- nxt2weeks_crudeRate %>%
  full_join(lnd, by = c(borough = "NAME"))%>%
  select(borough, diagnosis_rate_per_100000,geometry)%>%
  filter(borough != "City of London")




boroughs_of_interestfeb = nxt2_crudeRate %>%
  filter(borough %in% c("Croydon",
                        "Harrow", 
                        "Kensington and Chelsea"))


nxt2_crudeRate_sp <-  st_as_sf(nxt2_crudeRate)
boroughs_of_interestfeb<-  st_as_sf(boroughs_of_interestfeb)



map_after <- tm_shape(nxt2_crudeRate_sp) +
  tm_polygons(col = "diagnosis_rate_per_100000",
              legend.hist = FALSE, style="cont",
              
              breaks = seq(110, 660, by = 50),
              
              legend.show = F,
              palette = "-cividis", n = 5, contrast = c(0.0, 1),
              # breaks = seq(1000, 2000, by = 200)
  ) +
  tm_layout(legend.outside = F,
            frame=FALSE,
            legend.text.size = 0.7,
            legend.title.size = 0.8,
#            main.title =" Covid-19 Diagnosis Rate Per 100,000 
 #           February 2021",
            main.title.size = 1,
            legend.position = c("LEFT", "bottom"),
            outer.margins = 0, 
            inner.margins = c(0,0,0,0),
            outer.bg.color = "black") + #map scale.
  tm_shape(nxt2_crudeRate_sp, 
  ) + #add the test labels from lnd (NAME variable)
  tm_text("borough", size = 0.6, col ="#1a1300")+#, col = "#5c8a8a")+
  tm_shape(boroughs_of_interestfeb) +
  tm_borders(col="red", lwd=1.8)

#par(oma=c(0,0,10,0)) 
#tmap_arrange(map_before, map_after, sync = T)

map_after

tmap_save(map_after, "geospatial_plotting/february_resid.png")


























##########################################################################
#For commuters




crude_rate_summary_com <- data %>%
  group_by(commuting_area, covid_diagnosis)%>%
  summarise(diagnosis_status = n())%>%
  spread(key = covid_diagnosis, value = diagnosis_status)%>%
  rowwise()%>%
  mutate(total_population = sum(c_across(`0`:`1`))) %>%
  dplyr::select(-`0`)%>%
  mutate(pct_diag_borough = `1`/ total_population)%>%
  mutate(diagnosis_rate_per_100000 = ceiling(pct_diag_borough * 100000))



commuting_crude_rate_com <- crude_rate_summary_com %>%
  full_join(lnd, by = c(commuting_area = "NAME"))%>%
  dplyr::select(commuting_area, diagnosis_rate_per_100000,geometry)%>%
  filter(commuting_area!= "City of London")


commuting_crude_rate_sp <-  st_as_sf(commuting_crude_rate_com)



lon_comm <- tm_shape(commuting_crude_rate_sp) +
  tm_polygons(col = "diagnosis_rate_per_100000",
              legend.hist = FALSE, style="cont",
              title = "COVID-19 cases \n per 100,000",
              
            #  breaks = seq(110, 620, by = 100),
              
              legend.show = T,
              palette = "-cividis", n = 5, contrast = c(0.0, 1),
              # breaks = seq(1000, 2000, by = 200)
  ) +
  tm_layout(legend.outside = F,
            frame=FALSE,
            legend.text.size = 0.7,
            legend.title.size = 0.8,
            #            main.title =" Covid-19 Diagnosis Rate Per 100,000 
            #           MAy 2020 -comm",
            main.title.size = 1,
            legend.position = c("LEFT", "bottom"),
            outer.margins = 0, 
            inner.margins = c(0,0,0,0),
            outer.bg.color = "black") + #map scale.
  tm_shape(commuting_crude_rate_sp, 
  ) + #add the test labels from lnd (NAME variable)
  tm_text("commuting_area", size = 0.6, col ="#1a1300")


tmap_save(lon_comm, "geospatial_plotting/london_comm.png")



#May Commuting

may_crudeRatecom <- data %>%
  filter(covid_dt < "2020-06-01" & covid_dt>="2020-05-01") %>%
  group_by(commuting_area, covid_diagnosis) %>%
  summarise(diagnosis_status = n()) %>%
  spread(key = covid_diagnosis, value = diagnosis_status)%>%
  full_join(crude_rate_summary_com[c(1,3)], by = c("commuting_area" ))%>% 
  dplyr::select(-`0`)%>%
  mutate(pct_diag_borough = `1`/ total_population)%>%
  mutate(diagnosis_rate_per_100000 = ceiling(pct_diag_borough * 100000))


  


may_crudeRatecom <- may_crudeRatecom %>%
  full_join(lnd, by = c(commuting_area = "NAME"))%>%
  dplyr::select(commuting_area, diagnosis_rate_per_100000,geometry)%>%
  filter(commuting_area != "City of London")


may_crudeRate_spcom <-  st_as_sf(may_crudeRatecom)
map_may_com <- tm_shape(may_crudeRate_spcom) +
  tm_polygons(col = "diagnosis_rate_per_100000",
              legend.hist = FALSE, style="cont",
              
              breaks = seq(120, 620, by = 100),
              legend.show = F,
              palette = "-cividis", n = 5, contrast = c(0.0, 1),
              # breaks = seq(1000, 2000, by = 200)
  ) +
  tm_layout(legend.outside = F,
            frame=FALSE,
            legend.text.size = 0.7,
            legend.title.size = 0.8,
#                        main.title =" Covid-19 Diagnosis Rate Per 100,000 
            #           MAy 2020 -comm",
            main.title.size = 1,
            legend.position = c("LEFT", "bottom"),
            outer.margins = 0, 
            inner.margins = c(0,0,0,0),
            outer.bg.color = "black") + #map scale.
  tm_shape(may_crudeRate_spcom, 
  ) + #add the test labels from lnd (NAME variable)
  tm_text("commuting_area", size = 0.6, col ="#1a1300")



map_may_com

tmap_save(map_may_com, "geospatial_plotting/may_comm.png")
#June comm

june_crudeRatecom <- data %>%
  filter(covid_dt < "2020-07-01" & covid_dt>="2020-06-01") %>%
  group_by(commuting_area, covid_diagnosis) %>%
  summarise(diagnosis_status = n()) %>%
  spread(key = covid_diagnosis, value = diagnosis_status)%>%
  full_join(crude_rate_summary_com[c(1,3)], by = c("commuting_area" ))%>% 
  dplyr::select(-`0`)%>%
  mutate(pct_diag_borough = `1`/ total_population)%>%
  mutate(diagnosis_rate_per_100000 = ceiling(pct_diag_borough * 100000))





june_crudeRatecom <- june_crudeRatecom %>%
  full_join(lnd, by = c(commuting_area = "NAME"))%>%
  dplyr::select(commuting_area, diagnosis_rate_per_100000,geometry)%>%
  filter(commuting_area != "City of London")


june_crudeRate_spcom <-  st_as_sf(june_crudeRatecom)
map_june_com <- tm_shape(june_crudeRate_spcom) +
  tm_polygons(col = "diagnosis_rate_per_100000",
              legend.hist = FALSE, style="cont",
              
              
              breaks = seq(120, 620, by = 100),
              
              legend.show = F,
              palette = "-cividis", n = 5, contrast = c(0.0, 1),
              # breaks = seq(1000, 2000, by = 200)
  ) +
  tm_layout(legend.outside = F,
            frame=FALSE,
            legend.text.size = 0.7,
            legend.title.size = 0.8,
           # main.title =" Covid-19 Diagnosis Rate Per 100,000 
            #           MAy 2020 -comm",
            main.title.size = 1,
            legend.position = c("LEFT", "bottom"),
            outer.margins = 0, 
            inner.margins = c(0,0,0,0),
            outer.bg.color = "black") + #map scale.
  tm_shape(june_crudeRate_spcom, 
  ) + #add the test labels from lnd (NAME variable)
  tm_text("commuting_area", size = 0.6, col ="#1a1300")



map_june_com

tmap_save(map_june_com, "geospatial_plotting/june_comm.png")


#July

july_crudeRatecom <- data %>%
  filter(covid_dt < "2020-08-01" & covid_dt>="2020-07-01") %>%
  group_by(commuting_area, covid_diagnosis) %>%
  summarise(diagnosis_status = n()) %>%
  spread(key = covid_diagnosis, value = diagnosis_status)%>%
  full_join(crude_rate_summary_com[c(1,3)], by = c("commuting_area" ))%>% 
  dplyr::select(-`0`)%>%
  mutate(pct_diag_borough = `1`/ total_population)%>%
  mutate(diagnosis_rate_per_100000 = ceiling(pct_diag_borough * 100000))





july_crudeRatecom <- july_crudeRatecom %>%
  full_join(lnd, by = c(commuting_area = "NAME"))%>%
  dplyr::select(commuting_area, diagnosis_rate_per_100000,geometry)%>%
  filter(commuting_area != "City of London")


july_crudeRate_spcom <-  st_as_sf(july_crudeRatecom)
map_july_com <- tm_shape(july_crudeRate_spcom) +
  tm_polygons(col = "diagnosis_rate_per_100000",
              legend.hist = FALSE, style="cont",
              
              breaks = seq(120, 620, by = 100),
              
              legend.show = F,
              palette = "-cividis", n = 5, contrast = c(0.0, 1),
              # breaks = seq(1000, 2000, by = 200)
  ) +
  tm_layout(legend.outside = F,
            frame=FALSE,
            legend.text.size = 0.7,
            legend.title.size = 0.8,
#            main.title =" Covid-19 Diagnosis Rate Per 100,000 
            #           MAy 2020 -comm",
            main.title.size = 1,
            legend.position = c("LEFT", "bottom"),
            outer.margins = 0, 
            inner.margins = c(0,0,0,0),
            outer.bg.color = "black") + #map scale.
  tm_shape(july_crudeRate_spcom, 
  ) + #add the test labels from lnd (NAME variable)
  tm_text("commuting_area", size = 0.6, col ="#1a1300")



map_july_com

tmap_save(map_july_com, "geospatial_plotting/july_comm.png")

#August

aug_crudeRatecom <- data %>%
  filter(covid_dt < "2020-09-01" & covid_dt>="2020-08-01") %>%
  group_by(commuting_area, covid_diagnosis) %>%
  summarise(diagnosis_status = n()) %>%
  spread(key = covid_diagnosis, value = diagnosis_status)%>%
  full_join(crude_rate_summary_com[c(1,3)], by = c("commuting_area" ))%>% 
  dplyr::select(-`0`)%>%
  mutate(pct_diag_borough = `1`/ total_population)%>%
  mutate(diagnosis_rate_per_100000 = ceiling(pct_diag_borough * 100000))





aug_crudeRatecom <- aug_crudeRatecom %>%
  full_join(lnd, by = c(commuting_area = "NAME"))%>%
  dplyr::select(commuting_area, diagnosis_rate_per_100000,geometry)%>%
  filter(commuting_area != "City of London")


aug_crudeRate_spcom <-  st_as_sf(aug_crudeRatecom)
map_aug_com <- tm_shape(aug_crudeRate_spcom) +
  tm_polygons(col = "diagnosis_rate_per_100000",
              legend.hist = FALSE, style="cont",
              
              
              breaks = seq(110, 620, by = 100),
              
              legend.show = F,
              palette = "-cividis", n = 5, contrast = c(0.0, 1),
              # breaks = seq(1000, 2000, by = 200)
  ) +
  tm_layout(legend.outside = F,
            frame=FALSE,
            legend.text.size = 0.7,
            legend.title.size = 0.8,
#            main.title =" Covid-19 Diagnosis Rate Per 100,000 
            #           MAy 2020 -comm",
            main.title.size = 1,
            legend.position = c("LEFT", "bottom"),
            outer.margins = 0, 
            inner.margins = c(0,0,0,0),
            outer.bg.color = "black") + #map scale.
  tm_shape(aug_crudeRate_spcom, 
  ) + #add the test labels from lnd (NAME variable)
  tm_text("commuting_area", size = 0.6, col ="#1a1300")



map_aug_com

tmap_save(map_aug_com, "geospatial_plotting/aug_comm.png")
#September

sep_crudeRatecom <- data %>%
  filter(covid_dt < "2020-10-01" & covid_dt>="2020-09-01") %>%
  group_by(commuting_area, covid_diagnosis) %>%
  summarise(diagnosis_status = n()) %>%
  spread(key = covid_diagnosis, value = diagnosis_status)%>%
  full_join(crude_rate_summary_com[c(1,3)], by = c("commuting_area" ))%>% 
  dplyr::select(-`0`)%>%
  mutate(pct_diag_borough = `1`/ total_population)%>%
  mutate(diagnosis_rate_per_100000 = ceiling(pct_diag_borough * 100000))





sep_crudeRatecom <- sep_crudeRatecom %>%
  full_join(lnd, by = c(commuting_area = "NAME"))%>%
  dplyr::select(commuting_area, diagnosis_rate_per_100000,geometry)%>%
  filter(commuting_area != "City of London")


sep_crudeRate_spcom <-  st_as_sf(sep_crudeRatecom)
map_sep_com <- tm_shape(sep_crudeRate_spcom) +
  tm_polygons(col = "diagnosis_rate_per_100000",
              legend.hist = FALSE, style="cont",
              
              breaks = seq(120, 620, by = 100),
              
              legend.show = F,
              palette = "-cividis", n = 5, contrast = c(0.0, 1),
              # breaks = seq(1000, 2000, by = 200)
  ) +
  tm_layout(legend.outside = F,
            frame=FALSE,
            legend.text.size = 0.7,
            legend.title.size = 0.8,
#            main.title =" Covid-19 Diagnosis Rate Per 100,000 
            #           MAy 2020 -comm",
            main.title.size = 1,
            legend.position = c("LEFT", "bottom"),
            outer.margins = 0, 
            inner.margins = c(0,0,0,0),
            outer.bg.color = "black") + #map scale.
  tm_shape(sep_crudeRate_spcom, 
  ) + #add the test labels from lnd (NAME variable)
  tm_text("commuting_area", size = 0.6, col ="#1a1300")



map_sep_com

tmap_save(map_sep_com, "geospatial_plotting/sep_comm.png")

#October


oct_crudeRatecom <- data %>%
  filter(covid_dt < "2020-11-01" & covid_dt>="2020-10-01") %>%
  group_by(commuting_area, covid_diagnosis) %>%
  summarise(diagnosis_status = n()) %>%
  spread(key = covid_diagnosis, value = diagnosis_status)%>%
  full_join(crude_rate_summary_com[c(1,3)], by = c("commuting_area" ))%>% 
  dplyr::select(-`0`)%>%
  mutate(pct_diag_borough = `1`/ total_population)%>%
  mutate(diagnosis_rate_per_100000 = ceiling(pct_diag_borough * 100000))





oct_crudeRatecom <- oct_crudeRatecom %>%
  full_join(lnd, by = c(commuting_area = "NAME"))%>%
  dplyr::select(commuting_area, diagnosis_rate_per_100000,geometry)%>%
  filter(commuting_area != "City of London")


oct_crudeRate_spcom <-  st_as_sf(oct_crudeRatecom)
map_oct_com <- tm_shape(oct_crudeRate_spcom) +
  tm_polygons(col = "diagnosis_rate_per_100000",
              legend.hist = FALSE, style="cont",
              
              breaks = seq(120, 620, by = 100),
              
              legend.show = F,
              palette = "-cividis", n = 5, contrast = c(0.0, 1),
              # breaks = seq(1000, 2000, by = 200)
  ) +
  tm_layout(legend.outside = F,
            frame=FALSE,
            legend.text.size = 0.7,
            legend.title.size = 0.8,
#            main.title =" Covid-19 Diagnosis Rate Per 100,000 
            #           MAy 2020 -comm",
            main.title.size = 1,
            legend.position = c("LEFT", "bottom"),
            outer.margins = 0, 
            inner.margins = c(0,0,0,0),
            outer.bg.color = "black") + #map scale.
  tm_shape(oct_crudeRate_spcom, 
  ) + #add the test labels from lnd (NAME variable)
  tm_text("commuting_area", size = 0.6, col ="#1a1300")



map_oct_com


tmap_save(map_oct_com, "geospatial_plotting/oct_comm.png")


#November
nov_crudeRatecom <- data %>%
  filter(covid_dt < "2020-12-01" & covid_dt>="2020-11-01") %>%
  group_by(commuting_area, covid_diagnosis) %>%
  summarise(diagnosis_status = n()) %>%
  spread(key = covid_diagnosis, value = diagnosis_status)%>%
  full_join(crude_rate_summary_com[c(1,3)], by = c("commuting_area" ))%>% 
  dplyr::select(-`0`)%>%
  mutate(pct_diag_borough = `1`/ total_population)%>%
  mutate(diagnosis_rate_per_100000 = ceiling(pct_diag_borough * 100000))





nov_crudeRatecom <- nov_crudeRatecom %>%
  full_join(lnd, by = c(commuting_area = "NAME"))%>%
  dplyr::select(commuting_area, diagnosis_rate_per_100000,geometry)%>%
  filter(commuting_area != "City of London")


nov_crudeRate_spcom <-  st_as_sf(nov_crudeRatecom)
map_nov_com <- tm_shape(nov_crudeRate_spcom) +
  tm_polygons(col = "diagnosis_rate_per_100000",
              legend.hist = FALSE, style="cont",
              
              
              breaks = seq(120, 620, by = 100),
              
              legend.show = F,
              palette = "-cividis", n = 5, contrast = c(0.0, 1),
              # breaks = seq(1000, 2000, by = 200)
  ) +
  tm_layout(legend.outside = F,
            frame=FALSE,
            legend.text.size = 0.7,
            legend.title.size = 0.8,
#            main.title =" Covid-19 Diagnosis Rate Per 100,000 
            #           MAy 2020 -comm",
            main.title.size = 1,
            legend.position = c("LEFT", "bottom"),
            outer.margins = 0, 
            inner.margins = c(0,0,0,0),
            outer.bg.color = "black") + #map scale.
  tm_shape(nov_crudeRate_spcom, 
  ) + #add the test labels from lnd (NAME variable)
  tm_text("commuting_area", size = 0.6, col ="#1a1300")



map_nov_com

tmap_save(map_nov_com, "geospatial_plotting/nov_comm.png")
#December
dec_crudeRatecom <- data %>%
  filter(covid_dt < "2021-01-01" & covid_dt>="2020-12-01") %>%
  group_by(commuting_area, covid_diagnosis) %>%
  summarise(diagnosis_status = n()) %>%
  spread(key = covid_diagnosis, value = diagnosis_status)%>%
  full_join(crude_rate_summary_com[c(1,3)], by = c("commuting_area" ))%>% 
  dplyr::select(-`0`)%>%
  mutate(pct_diag_borough = `1`/ total_population)%>%
  mutate(diagnosis_rate_per_100000 = ceiling(pct_diag_borough * 100000))





dec_crudeRatecom <- dec_crudeRatecom %>%
  full_join(lnd, by = c(commuting_area = "NAME"))%>%
  dplyr::select(commuting_area, diagnosis_rate_per_100000,geometry)%>%
  filter(commuting_area != "City of London")


dec_crudeRate_spcom <-  st_as_sf(dec_crudeRatecom)
map_dec_com <- tm_shape(dec_crudeRate_spcom) +
  tm_polygons(col = "diagnosis_rate_per_100000",
              legend.hist = FALSE, style="cont",
              
              breaks = seq(120, 620, by = 100),
              
              legend.show = F,
              palette = "-cividis", n = 5, contrast = c(0.0, 1),
              # breaks = seq(1000, 2000, by = 200)
  ) +
  tm_layout(legend.outside = F,
            frame=FALSE,
            legend.text.size = 0.7,
            legend.title.size = 0.8,
          #  main.title =" Covid-19 Diagnosis Rate Per 100,000 
            #           MAy 2020 -comm",
            main.title.size = 1,
            legend.position = c("LEFT", "bottom"),
            outer.margins = 0, 
            inner.margins = c(0,0,0,0),
            outer.bg.color = "black") + #map scale.
  tm_shape(dec_crudeRate_spcom, 
  ) + #add the test labels from lnd (NAME variable)
  tm_text("commuting_area", size = 0.6, col ="#1a1300")



map_dec_com

tmap_save(map_dec_com, "geospatial_plotting/dec_comm.png")

#January 

jan_crudeRatecom <- data %>%
  filter(covid_dt < "2021-02-01" & covid_dt>="2021-01-01") %>%
  group_by(commuting_area, covid_diagnosis) %>%
  summarise(diagnosis_status = n()) %>%
  spread(key = covid_diagnosis, value = diagnosis_status)%>%
  full_join(crude_rate_summary_com[c(1,3)], by = c("commuting_area" ))%>% 
  dplyr::select(-`0`)%>%
  mutate(pct_diag_borough = `1`/ total_population)%>%
  mutate(diagnosis_rate_per_100000 = ceiling(pct_diag_borough * 100000))





jan_crudeRatecom <- jan_crudeRatecom %>%
  full_join(lnd, by = c(commuting_area = "NAME"))%>%
  dplyr::select(commuting_area, diagnosis_rate_per_100000,geometry)%>%
  filter(commuting_area != "City of London")


jan_crudeRate_spcom <-  st_as_sf(jan_crudeRatecom)
map_jan_com <- tm_shape(jan_crudeRate_spcom) +
  tm_polygons(col = "diagnosis_rate_per_100000",
              legend.hist = FALSE, style="cont",
              
              breaks = seq(120, 620, by = 100),
              title = "COVID_19 cases \n per 100,000",
              legend.show = F,
              palette = "-cividis", n = 5, contrast = c(0.0, 1),
              # breaks = seq(1000, 2000, by = 200)
  ) +
  tm_layout(legend.outside = F,
            frame=FALSE,
            legend.text.size = 0.7,
            legend.title.size = 0.8,
 #           main.title =" Covid-19 Diagnosis Rate Per 100,000 
#            #           MAy 2020 -comm",
            main.title.size = 1,
            legend.position = c("LEFT", "bottom"),
            outer.margins = 0, 
            inner.margins = c(0,0,0,0),
            outer.bg.color = "black") + #map scale.
  tm_shape(jan_crudeRate_spcom, 
  ) + #add the test labels from lnd (NAME variable)
  tm_text("commuting_area", size = 0.6, col ="#1a1300")



map_jan_com
tmap_save(map_jan_com, "geospatial_plotting/jan_comm.png")
#February



feb_crudeRatecom <- data %>%
  filter(covid_dt < "2021-03-01" & covid_dt>="2021-02-01") %>%
  group_by(commuting_area, covid_diagnosis) %>%
  summarise(diagnosis_status = n()) %>%
  spread(key = covid_diagnosis, value = diagnosis_status)%>%
  full_join(crude_rate_summary_com[c(1,3)], by = c("commuting_area" ))%>% 
  dplyr::select(-`0`)%>%
  mutate(pct_diag_borough = `1`/ total_population)%>%
  mutate(diagnosis_rate_per_100000 = ceiling(pct_diag_borough * 100000))





feb_crudeRatecom <- feb_crudeRatecom %>%
  full_join(lnd, by = c(commuting_area = "NAME"))%>%
  dplyr::select(commuting_area, diagnosis_rate_per_100000,geometry)%>%
  filter(commuting_area != "City of London")


feb_crudeRate_spcom <-  st_as_sf(feb_crudeRatecom)
map_feb_com <- tm_shape(feb_crudeRate_spcom) +
  tm_polygons(col = "diagnosis_rate_per_100000",
              legend.hist = FALSE, style="cont",
              
                breaks = seq(120, 620, by = 100),
              
              legend.show = F,
              palette = "-cividis", n = 5, contrast = c(0.0, 0.95),
              # breaks = seq(1000, 2000, by = 200)
  ) +
  tm_layout(legend.outside = F,
            frame=FALSE,
            legend.text.size = 0.7,
            legend.title.size = 0.8,
          #  main.title =" Covid-19 Diagnosis Rate Per 100,000 
            #           MAy 2020 -comm",
            main.title.size = 1,
            legend.position = c("LEFT", "bottom"),
            outer.margins = 0, 
            inner.margins = c(0,0,0,0),
            outer.bg.color = "black") + #map scale.
  tm_shape(feb_crudeRate_spcom, 
  ) + #add the test labels from lnd (NAME variable)
  tm_text("commuting_area", size = 0.6, col ="#1a1300")



map_feb_com
tmap_save(map_feb_com, "geospatial_plotting/feb_comm.png")






