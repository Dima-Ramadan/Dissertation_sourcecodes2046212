
#Date: 05-07-2022
#Last update : 08-08-2022




#This function distributes the people intocommuting areas. 
#it takes (patients dataset , distance between boroughs dataset, 
#calculated probability vectors for all age categorie, commuting prob data)
#it returns a data frame with the/ new locations



commuting_function <- function(data,
                               displacement_data, 
                               commuting_prob_data,
                               prob_agecat1, 
                               prob_agecat2,
                               prob_agecat3,
                               prob_agecat4,
                               prob_agecat5,
                               prob_agecat6,
                               prob_agecat7){

  #loop throough each examplr in health records data
for (i in 1:nrow(data)){
  print(i)
  data_row <- data[i, ]
  if (data_row$commuter == "T"){
    residence_place <- data_row$borough
   
    col_index_residence <-  grep(residence_place, colnames(displacement_data))
    
    dist_resid_boroughs <- displacement_data[row.names(displacement_data) == residence_place,
                                             -col_index_residence ]
  
    
    
  #probabilities of commuting from the residence to other boroughs based on residence - work data 
  
    col_index_commProb <-  grep(residence_place, colnames(commuting_prob_table))
    
    commuting_prob <- commuting_prob_data[row.names(commuting_prob_data) == residence_place,
                                                                  -col_index_commProb]%>%
      rownames_to_column() %>% 
      gather(variable, value, -rowname) %>% 
      spread(rowname, value)
    
    names(commuting_prob)[1] <- "to_borough"
    names(commuting_prob)[2] <- "commuting_prob"
    
    #from commuting area probabilities file (function) to skew the distribution of diastances 
    #right skew distribution (the right tail for further distances from the residence place )
  
    
    
    if(data_row$catAge == 2){
      dist_prob <- prob_agecat2
      print(2)
    #  print(dist_prob)
    }
    if(data_row$catAge == 3){
      dist_prob <- prob_agecat3
      print(3)
     # print(dist_prob)
    } 
    if(data_row$catAge == 4){
      dist_prob <- prob_agecat4
      print(4)
      #print(dist_prob)
    } 
    if(data_row$catAge == 5){
      dist_prob <- prob_agecat5
      print(5)
     # print(dist_prob)
    } 
    if(data_row$catAge == 6){
      dist_prob <- prob_agecat6
      print(6)
      print(dist_prob)
    } else{
      dist_prob <- prob_agecat7
      print(7)
    #  print(dist_prob)
    } 
    
    

    
  
    #Make Transpose the columna to rows for easier manipulation and join the distance prob to the displacement arranged by order (lower to higher)
    col_dist <- dist_resid_boroughs %>% 
      rownames_to_column() %>% 
      gather(variable, value, -rowname) %>% 
      spread(rowname, value)%>%
      rename(to_borough = variable, displacement = all_of(residence_place))%>%
      arrange(displacement)%>%
      mutate(order =seq(1:31), skewed_age_prob = dist_prob[-1])%>% #remove the first prob, assuming it is the same borough
      left_join(commuting_prob , by = c("to_borough"))%>%
      mutate(total_prob = skewed_age_prob * commuting_prob)
    
    #multibly the probabilities 
    
    #choose random sample from the ordered borough and with probabilities = distance probabilities 
    commuting_area_sample <-  sample(x = c(1:31), size=1, replace = T, prob= col_dist$total_prob)
    
    # randomly selected borough 
    commute_borough <- col_dist$to_borough[col_dist$order == commuting_area_sample]
    
    data$commuting_area[i] <- commute_borough
  }else{
    data$commuting_area[i] <- data$borough[i]
  }
}  
  # rm(col_index_commProb, col_index_residence, dist_prob, residence_place, dist_resid_boroughs,
  #    data_row, commuting_prob, col_dist)
  return(data)
  
  
  
}



