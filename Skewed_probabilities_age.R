
#Date:05/07/2022
#Last edit: 15/8/2022

#This function takes one argument (age categoriy of the person) 
# and returns a probability vector for the work location dependng on AGE 
#the vector is ordered from high to low probability and should be merged with the displacement later
#from the residence place to each borough after the displacement is ordered from closer to further 
#returns right Skewed probability vector 


skewed_prob_vec <- function(agecat){
    set.seed(105140)

    
  if(agecat == 2){ #16-24
    avg_dist <- 13.4
    seq_borough <- seq(1,32, by = 1 )
    skewed_prob <- psn(seq_borough , xi = avg_dist, omega=3, alpha=0.1)
    right_skewed_prob <- 1-skewed_prob
    plot(seq_borough, right_skewed_prob, type="l",col= "#2166AC", lwd =2, ylab="probabiltyt", main= "16 - 24 Years old", 
         xlab = "boroughs distance from home")
    abline(v = avg_dist, col="#D6604D", lwd=3, lty=2)
    text(avg_dist + 5,1 , "Avg distance")
   
  }
  #448 381
  #25-34
  if(agecat == 3){
    avg_dist <- 12.7
    
    #  set.seed(100)
    seq_borough <- seq(1,32, by = 1 )
    skewed_prob <- psn(seq_borough , xi = avg_dist, omega=11, alpha=1)
    right_skewed_prob <- 1-skewed_prob
    plot(seq_borough, right_skewed_prob, type="l",col= "#2166AC", lwd =2, ylab="probabiltyt", main= "25-34 Years old", 
         xlab = "boroughs distance from home")
    abline(v = avg_dist, col="#D6604D", lwd=3, lty=2)
    text(avg_dist + 5,0.95 , "Avg distance")

  }
  
  #35-49
  if(agecat == 4){
    avg_dist <- 16.4
    #  set.seed(100)
    seq_borough <- seq(1,32, by = 1 )
    skewed_prob <- psn(seq_borough , xi = avg_dist, omega=20, alpha=1)
    right_skewed_prob <- 1-skewed_prob
    plot(seq_borough, right_skewed_prob, type="l",col= "#2166AC", lwd =2, ylab="probabiltyt", main= "35 - 49 Years old", 
         xlab = "boroughs distance from home")
    abline(v = avg_dist, col="#D6604D", lwd=3, lty=2)
   text(avg_dist + 5,0.95 , "Avg distance")

  }
  
  #50-64
  if(agecat == 5){
    avg_dist <- 15.5
    #  set.seed(100)
    seq_borough <- seq(1,32, by = 1 )
    skewed_prob <- psn(seq_borough , xi = avg_dist, omega=6, alpha=1)
    right_skewed_prob <- 1-skewed_prob
    plot(seq_borough, right_skewed_prob, type="l",col= "#2166AC", lwd =2, ylab="probabiltyt", main= "50 - 64 Years old", 
         xlab = "boroughs distance from home")
    abline(v = avg_dist, col="#D6604D", lwd=3, lty=2)
    text(avg_dist + 5,1 , "Avg distance")
    
  }
  
  #65-74
  if(agecat == 6){
    avg_dist <- 11.3
    #  set.seed(100)
    seq_borough <- seq(1,32, by = 1 )
    skewed_prob <- psn(seq_borough , xi = avg_dist, omega=6, alpha=-1.8)
    right_skewed_prob <- 1-skewed_prob
    plot(seq_borough, right_skewed_prob, type="l",col= "#2166AC", lwd =2, ylab="probabiltyt", main= "65 - 74 Years old", 
          xlab = "boroughs distance from home")
    abline(v = avg_dist, col="#D6604D", lwd=3, lty=2)
    text(avg_dist + 5,0.9 , "Average distance")
    
  }
  
  
  #75+
  if(agecat == 7){
    avg_dist <- 9.4
    #  set.seed(100)
    seq_borough <- seq(1,32, by = 1 )
    skewed_prob <- psn(seq_borough , xi = avg_dist, omega=8, alpha=-2)
    right_skewed_prob <- 1-skewed_prob
    
    plot(seq_borough, right_skewed_prob, type="l",col= "#2166AC", lwd =2, ylab="probabiltyt", main= "over 75 years", 
         xlab = "boroughs distance from home")
    abline(v = avg_dist, col="#D6604D", lwd=3, lty=2)
    text(avg_dist + 5,0.7 , "Avg distance")
    
  }
  
 return(right_skewed_prob) 
  
}















  
# col_dist <- bor_dist %>% 
#   rownames_to_column() %>% 
#   gather(variable, value, -rowname) %>% 
#   spread(rowname, value)%>%
#   rename(to_borough = variable, displacement = all_of(resed_place))%>%
#   arrange(displacement)%>%
#   mutate(order =seq(1:32), commute_prob = right_skewed_prob)
# 
#   commuting_area_sample <-  sample(x = c(1:32), size=1, replace = T, prob= col_dist$commute_prob)
# 
#   commute_borough <- col_dist$to_borough[col_dist$order == commuting_area_sample]
 










# #install.packages("sn")
# 
# library(sn)
# 
# 
# if (x$AgeCat == 1){
#   avg_dist <- 13.4
#   prob_vector <- 
#     random_dist <- sample(x)
# }
# 
# 
# 
# set.seed(100)
# x <- seq(1,33, by = 1 )
# y1 <- psn(x, xi=13.3, omega=13, alpha=3)
# y2 <- 1-y1
# plot(x, y2, type="l", ylab="density")
# 
# y <-  sample(x = c(1:33), size=32, replace = T, prob= y2)
# hist(y)
# 
# y2





# summary(max_dist)
# x = seq(4.7, 33 , length = 32)
# ran <- psn(x ,xi = 14,  omega =5, alpha= 5, tau = 5)
# plot(x, ran, type="l", ylab="density")
# 
# ran
# y <-  sample(x = c(1:32), size=5000, replace = T, prob= ran)
# hist(y)




