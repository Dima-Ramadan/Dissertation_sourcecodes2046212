
#This file will rebuild the population based on the size that was defined while creating the gp-profile

library(tidyverse)

#read cprds data for london region
lon_health_records <-
  read.csv("GP_allocation/resultant_data/GT_london_data.csv")
#read gp-profile
gp_prof_counts <-
  read.csv("GP_allocation/resultant_data/gpProfile_pplCOunts_200822.csv")
lon_health_records$patid <- seq(1:nrow(lon_health_records))
str(lon_health_records)

#select the features that will need
gp_prof_counts <- gp_prof_counts %>%
  select(
    borough,
    AreaCode,
    Latitude,
    Longitude,
    gp_counts,
    plus85,
    imd_score,
    PC,
    PAD,
    DEM,
    RA,
    LD,
    EP,
    HF,
    MH,
    STIA,
    AF,
    COPD,
    CKD,
    CAN,
    AST,
    DEP,
    HYP,
    DM
  )

#order the features same as the GP profile

lon_health_records <- lon_health_records %>%
  select(
    Gender,
    age ,
    catAge,
    region ,
    imd_5,
    rurban,
    covid_dt,
    covid_diagnosis,
    DeathDate,
    Death,
    PalliativeCare,
    PAD,
    Dementia,
    RheumatoidArthritis,
    LearningDisability,
    Epilepsy,
    HeartFailure,
    MentalHealth,
    StrokeTIA,
    AF,
    COPD,
    CKD,
    Cancer,
    Asthma,
    Depression,
    Hypertension,
    Diabetes,
    everything()
    
  )


#initiate a list - will data indices
patients_gp <- list()


#loop through the GPs
for (j in 1:nrow(gp_prof_counts)) {
  data <-
    lon_health_records[lon_health_records$imd_5 == gp_prof_counts$imd_score[j], ]
  return_data <- lon_health_records[FALSE,]
  #loop through the health indicators
  for (i in 1:17) {
    print(names(lon_health_records)[i + 10])
    print(names(gp_prof_counts)[i + 7])
    
   #health indicators for the +16 population  
    if (names(gp_prof_counts)[i + 7] == "CKD" |
        names(gp_prof_counts)[i + 7] == "DM" |
        names(gp_prof_counts)[i + 7] == "DEP" |
        names(gp_prof_counts)[i + 7] == "EP" |
        names(gp_prof_counts)[i + 7] == "RA") {
      data_spec <- data[data$age > 16,]
      counts <- return_data[return_data$catAge > 16, ]
      exist_counts <- sum(counts[i + 10])
      
      data_sampling <- data_spec[data_spec[i + 10] == 1, ]
      rm(data_spec)
    } else{
      exist_counts <- sum(return_data[i + 10])
      
      data_sampling <- data[data[i + 10] == 1, ]
      
    }
    
    
    
    if (exist_counts < gp_prof_counts[j, i + 7]) {
      data_sampling$current_id <- seq(1:nrow(data_sampling))
      sample_vc <- as.data.frame(sample(
        x = c(1:nrow(data_sampling)),
        size = gp_prof_counts[j, i + 7] - exist_counts,
        replace = T
      ))
      names(sample_vc)[1] <- "current_id"
      sample_data <-
        left_join(sample_vc, data_sampling, by = "current_id")
      sample_data <- sample_data[-1]
      return_data <- rbind(return_data, sample_data)
      
      
    }
    data <- data[data[i + 10] != 1, ]
  }
  
  
  left_total_count <- gp_prof_counts[j, 5] - nrow(return_data)
  data_sampling <- data
  if (left_total_count > 0) {
    data_sampling$current_id <- seq(1:nrow(data_sampling))
    sample_vc <- as.data.frame(sample(
      x = c(1:nrow(data_sampling)),
      size = left_total_count,
      replace = T
    ))
    names(sample_vc)[1] <- "current_id"
    sample_data <-
      left_join(sample_vc, data_sampling, by = "current_id")
    sample_data <- sample_data[-1]
    return_data <- rbind(return_data, sample_data)
  }
  patients_gp[[j]] <- return_data
  names(patients_gp)[j] <- gp_prof_counts$AreaCode[j]
}
#



#check the results :
sim_gp_results <- lon_health_records[FALSE,]
sim_gp_results <- sim_gp_results[9:27]
names(sim_gp_results)[1] <- "AreaCode"
names(sim_gp_results)[1] <- "totalcount"

for (i in 1:length(patients_gp)) {
  x <- colSums(patients_gp[[i]][11:27])
  total_counts <- nrow(patients_gp[[i]])
  AreaCode <- names(patients_gp[i])
  x <- append(x, total_counts)
  x <- append(x, AreaCode)
  sim_gp_results <- rbind(sim_gp_results , x)
}

names(sim_gp_results)[1:17] <- names(gp_prof_counts)[8:24]
names(sim_gp_results)[19] <- "AreaCode"
names(sim_gp_results)[18] <- "totalcount"

cols <- names(sim_gp_results)[1:18]
sim_gp_results[cols] <- lapply(sim_gp_results[cols], as.integer)


cols <- names(sim_gp_results)[1:17]
sim_gp_results[cols] <-
  lapply(sim_gp_results[cols], function(cols)
    cols / sim_gp_results$totalcount)


lon_data_geo <- do.call(rbind, patients_gp)

lon_data_geo$AreaCode <- row.names(lon_data_geo)

lon_data_geo$AreaCode <- gsub("\\..*", "", lon_data_geo$AreaCode)

lon_data_gp_geo <-
  left_join(lon_data_geo, gp_prof_counts[1:5], by = c("AreaCode"))

pat_not_included <-
  lon_health_records[!lon_health_records$patid %in% lon_data_gp_geo$patid,] #10203

write.csv(
  lon_data_gp_geo,
#  "GP_allocation/resultant_data/london_gp_geo_200822.csv",
  row.names = F
)

rm(
  data,
  x,
  pat_not_included,
  lon_data_geo,
  sample_vc,
  sample_data,
  return_data,
  patients_gp,
  lon_health_records,
  data_sampling,
  cols,
  sim_gp_prof_gp,
  left_total_count,
  i,
  j,
  AreaCode,
  counts,
  gp_prof_counts
)

sim_gp_results[1:17] <- sim_gp_results[1:17] * 100
write.csv(
  sim_gp_results,
  "GP_allocation/resultant_data/sim_GPprofile_pctres_200822.csv",
  row.names = F
)
###################################################################

#Results checking


sim_gp_results <-
  read.csv("GP_allocation/resultant_data/sim_GPprofile_pctres_200822.csv")


par(mfrow = c(1, 2))
cor_sim <- cor(sim_gp_results[1:17])
corrplot::corrplot(
  cor_sim,
  method = c("square"),
  order = c("original"),
  type = c("upper"),
  #title = "simulated GP profile - % Patients ",
  
  tl.cex = 0.7,
  tl.col = "black",
  mar = c(0, 0, 1, 0)
)


real_gp_prof <-
  read.csv("GP_allocation/resultant_data/gpprofileQOF2021_pct_pop1.08M_200822.csv")
#str(real_gp_prof)

#reorder the variables
real_gp_prof <- real_gp_prof %>%
  select(
    borough,
    AreaCode,
    Latitude,
    Longitude,
    gp_counts,
    plus85,
    imd_score,
    PC,
    PAD,
    DEM,
    RA,
    LD,
    EP,
    HF,
    MH,
    STIA,
    AF,
    COPD,
    CKD,
    CAN,
    AST,
    DEP,
    HYP,
    DM
  )

cor_real <- cor(real_gp_prof[8:24])
corrplot::corrplot(
  cor_real ,
  method = c("square"),
  order = c("original"),
  type = c("upper"),
  #title = "Real GP profile - % Patients ",
  
  tl.cex = 0.7,
  tl.col = "black",
  mar = c(0, 0, 1, 0)
)


##############################################

# Heatmaps for the rows and attributes in each dataset

real_gp_prof_shuff <- real_gp_prof[c(-1,-3,-4,-5,-6,-7)]

samplecol <- sample(c(1:18), size = 18, replace = F)
real_gp_prof_shuff <- real_gp_prof_shuff[samplecol]


#transform the data to matrix for heatmap

row.names(real_gp_prof_shuff) <- real_gp_prof_shuff$AreaCode
real_gp_prof_matrix <-
  select(real_gp_prof_shuff, c(-AreaCode, -DEP, -DM, -HYP, -AST))
real_gp_prof_matrix <-
  as.matrix(real_gp_prof_matrix) #remove areacode



sim_gp_prof_matrixx <- sim_gp_results[names(real_gp_prof_shuff)]

row.names(sim_gp_prof_matrixx) <- sim_gp_prof_matrixx$AreaCode

sim_gp_prof_matrixx <-
  select(sim_gp_prof_matrixx, c(-AreaCode, -DEP, -DM, -HYP, -AST))
sim_gp_prof_matrixx <-
  as.matrix(sim_gp_prof_matrixx) #remove areacode

library(RColorBrewer)
coul <- colorRampPalette(brewer.pal(9, "Oranges"))(10)

heatmap(sim_gp_prof_matrixx,
        Colv = NA,
        Rowv = NA,
        col = coul)

heatmap(real_gp_prof_matrix,
        Colv = NA,
        Rowv = NA,
        col = coul)

#place a legend
legend(
  x = "topright",
  legend = c("low", 'mid', 'high'),
  cex = 0.8,
  fill = colorRampPalette(brewer.pal(9, "Oranges"))(3)
)


###########################################################
library(tidyverse)
#bar plots for each borough and the Gp's in it
sim_gp_results <-
  left_join(sim_gp_results, real_gp_prof[c(1, 2)], by = c("AreaCode"))
boroughs <- levels(as.factor(real_gp_prof$borough))
boroughs

library(patchwork)
#install.packages("reshape")
library(reshape)



ggtheme <-  theme(
  axis.text.x = element_text(
    angle = 90,
    vjust = 0.5,
    hjust = 1
  ),
  axis.text = element_text(size = 7),
  axis.title = element_text(size = 7),
  plot.title = element_text(size = 9),
  legend.title = element_text(size = 4),
  legend.text = element_text(size = 8),
  panel.background = element_blank(),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  axis.line = element_line(colour = "black")
)



for (borough in seq(1:32)) {
  figures_list <- list()
  i = 16
  for (i in seq(1:17)) {
    borough_areacode <-
      real_gp_prof[real_gp_prof$borough == boroughs[borough], c(2, i + 7)]
    borough_areacode$sim <-
      sim_gp_results[sim_gp_results$borough == boroughs[borough], c(i)]
    attr_name <- names(sim_gp_results)[i]
    names(borough_areacode)[2] <- paste("GT")
    names(borough_areacode)[3] <- paste("Sim")
    
    
    melt_data <- borough_areacode %>%
      melt(id.vars = "AreaCode", variable.name = "attr")
    
    fig <- ggplot(melt_data,
                  aes(
                    x = AreaCode,
                    y = value,
                    fill = variable,
                    label = value
                  )) +
      geom_col(position = position_dodge()) +
      
      annotate(
        "text",
        x = Inf,
        y = Inf,
        label = paste(boroughs[borough], "-", attr_name),
        vjust = 1,
        hjust = 1,
        size = 3
      ) +
      ggtheme 
    
    
    
    
    figures_list[[i]] <- fig
    
    if (i == 8) {
      plot <- patchwork::wrap_plots(figures_list, nrow = 4)
      #plot
      
      ggsave(
        plot,
        file = paste0(
          "GP_allocation/Figures/gpprofile_for eachgp_sim_gt2/plot_",
          boroughs[borough],
          i,
          ".png"
        ),
        width = 20,
        height = 27,
        units = "cm"
      )
      
      figures_list <- list()
    }
    
    if (i == 16) {
      figures_list[1:8] <- NULL
      plot <- patchwork::wrap_plots(figures_list, nrow = 4)
      #plot
      
      ggsave(
        plot,
        file = paste0(
          "GP_allocation/Figures/gpprofile_for eachgp_sim_gt2/plot_",
          boroughs[borough],
          i,
          ".png"
        ),
        width = 20,
        height = 27,
        units = "cm"
      )
      
      figures_list <- list()
    }
  }
  
  
}
