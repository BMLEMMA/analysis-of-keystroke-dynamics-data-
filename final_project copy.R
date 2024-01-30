#Loading in the dataset
getwd()
setwd("/Users/mac/Desktop")
x <- read.csv("tr.csv")
subject <- x$subject
session <- x$sessionIndex
H.period <- x$H.period
DD.period.t <- x$DD.period.t
UD.period.t <- x$UD.period.t
H.t <- x$H.t
DD.t.i <- x$DD.t.i

#subject "s010", H.period variable
  # "s010" "session 1" "H. period"
  mean_of_session_1_H.period <- numeric(0)
  session_1_H.period <- numeric(0)
  for(i in 1:length(x$H.period)){
    if(subject[i] == "s010"){
      if(session[i] == "1"){
        session_1_H.period <- append(session_1_H.period, H.period[i])
        mean_of_session_1_H.period <- mean(session_1_H.period)
      }
    }
  }
  mean_of_session_1_H.period
  
  # "s010" "session 2" "H.period"
  mean_of_session_2_H.period <- numeric(0)
  session_2_H.period <- numeric(0)
  for(i in 1:length(x$H.period)){
    if(subject[i] == "s010"){
      if(session[i] == "2"){
        session_2_H.period <- append(session_2_H.period, H.period[i])
        mean_of_session_2_H.period <- mean(session_2_H.period)
      }
    }
  }
  mean_of_session_2_H.period
  
  # "s010" "session 3" "H.period"
  mean_of_session_3_H.period <- numeric(0)
  session_3_H.period <- numeric(0)
  for(i in 1:length(x$H.period)){
    if(subject[i] == "s010"){
      if(session[i] == "3"){
        session_3_H.period <- append(session_3_H.period, H.period[i])
        mean_of_session_3_H.period <- mean(session_3_H.period)
      }
    }
  }
  mean_of_session_3_H.period

  # "s010" "session 4" "H.period"
  mean_of_session_4_H.period <- numeric(0)
  session_4_H.period <- numeric(0)
  for(i in 1:length(x$H.period)){
    if(subject[i] == "s010"){
      if(session[i] == "4"){
        session_4_H.period <- append(session_4_H.period, H.period[i])
        mean_of_session_4_H.period <- mean(session_4_H.period)
      }
    }
  }
  mean_of_session_4_H.period

  # "s010" "session 5" "H.period"
  mean_of_session_5_H.period <- numeric(0)
  session_5_H.period <- numeric(0)
  for(i in 1:length(x$H.period)){
    if(subject[i] == "s010"){
      if(session[i] == "5"){
        session_5_H.period <- append(session_5_H.period, H.period[i])
        mean_of_session_5_H.period <- mean(session_5_H.period)
      }
    }
  }
  mean_of_session_5_H.period

  # "s010" "session 6" "H.period"
  mean_of_session_6_H.period <- numeric(0)
  session_6_H.period <- numeric(0)
  for(i in 1:length(x$H.period)){
    if(subject[i] == "s010"){
      if(session[i] == "6"){
        session_6_H.period <- append(session_6_H.period, H.period[i])
        mean_of_session_6_H.period <- mean(session_6_H.period)
      }
    }
  }
  mean_of_session_6_H.period

  # "s010" "session 7" "H.period"
  mean_of_session_7_H.period <- numeric(0)
  session_7_H.period <- numeric(0)
  for(i in 1:length(x$H.period)){
    if(subject[i] == "s010"){
      if(session[i] == "7"){
        session_7_H.period <- append(session_7_H.period, H.period[i])
        mean_of_session_7_H.period <- mean(session_7_H.period)
      }
    }
  }
  mean_of_session_7_H.period  

  # "s010" "session 8" "H.period"
  mean_of_session_8_H.period <- numeric(0)
  session_8_H.period <- numeric(0)
  for(i in 1:length(x$H.period)){
    if(subject[i] == "s010"){
      if(session[i] == "8"){
        session_8_H.period <- append(session_8_H.period, H.period[i])
        mean_of_session_8_H.period <- mean(session_8_H.period)
      }
    }
  }
  mean_of_session_8_H.period  
  
  #combining the means into one matrix to make things easier. 
  mean_combined_H.period_s010<- matrix(data=c(mean_of_session_1_H.period, mean_of_session_2_H.period,
                                  mean_of_session_3_H.period, mean_of_session_4_H.period,
                                  mean_of_session_5_H.period, mean_of_session_6_H.period, 
                                  mean_of_session_7_H.period, mean_of_session_8_H.period))
  #scatter plot so we can see how varied the means of the different sessions in H.period are. 
 plot(mean_combined_H.period_s010, main="Means of H.period of subject s010", xlabs="Means")                  
  
  # standard deviations for s010 H.period
  sd(session_1_H.period)
  sd(session_2_H.period)
  sd(session_3_H.period)
  sd(session_4_H.period)
  sd(session_5_H.period)
  sd(session_6_H.period)
  sd(session_7_H.period)
  sd(session_8_H.period)
 
  
  stdev_combined_H.period_s010<- matrix(data=c(sd(session_1_H.period),
                                   sd(session_2_H.period),
                                   sd(session_3_H.period),
                                   sd(session_4_H.period),
                                   sd(session_5_H.period),
                                   sd(session_6_H.period),
                                   sd(session_7_H.period),
                                   sd(session_8_H.period)))
  #plot to compare the standard deviations of the different sessions in H.period:
  plot(stdev_combined_H.period_s010, xlab="Standard deviations", main="Standard deviations of H.period of subject s010")
  
  
  
  #plot to compare the means and standard deviations of H.period:
  plot(means_combined_H.period_s010, stdev_combined_H.period_s010, main="Means of H.period of subject s010", xlab="Means")
  
 
  # "s010" "all sessions" "H. period"
  mean_of_all_sessions_H.period <- numeric(0)
  all_sessions_H.period <- numeric(0)
  for(i in 1:length(x$H.period)){
    if(subject[i] == "s010"){
      all_sessions_H.period <- append(all_sessions_H.period, H.period[i])
      mean_of_all_sessions_H.period <- mean(all_sessions_H.period)
    }
  }
  mean_of_all_sessions_H.period
  
  
  
  
  
  
  
  
#subject "s010", DD.period.t variable
  # "s010" "session 1" "DD. period"
  mean_of_session_1_DD.period.t <- numeric(0)
  session_1_DD.period.t <- numeric(0)
  for(i in 1:length(x$DD.period.t)){
    if(subject[i] == "s010"){
      if(session[i] == "1"){
        session_1_DD.period.t <- append(session_1_DD.period.t, DD.period.t[i])
        mean_of_session_1_DD.period.t <- mean(session_1_DD.period.t)
      }
    }
  }
  mean_of_session_1_DD.period.t  
  
  # "s010" "session 2" DD.period"
  mean_of_session_2_DD.period.t <- numeric(0)
  session_2_DD.period.t <- numeric(0)
  for(i in 1:length(x$DD.period.t)){
    if(subject[i] == "s010"){
      if(session[i] == "2"){
        session_2_DD.period.t <- append(session_2_DD.period.t, DD.period.t[i])
        mean_of_session_2_DD.period.t <- mean(session_2_DD.period.t)
      }
    }
  }
  mean_of_session_2_DD.period.t  
  
  # "s010" "session 3" DD.period"
  mean_of_session_3_DD.period.t <- numeric(0)
  session_3_DD.period.t <- numeric(0)
  for(i in 1:length(x$DD.period.t)){
    if(subject[i] == "s010"){
      if(session[i] == "3"){
        session_3_DD.period.t <- append(session_3_DD.period.t, DD.period.t[i])
        mean_of_session_3_DD.period.t <- mean(session_3_DD.period.t)
      }
    }
  }
  mean_of_session_3_DD.period.t  
  
  # "s010" "session 4" "DD.Period"
  mean_of_session_4_DD.period.t <- numeric(0)
  session_4_DD.period.t <- numeric(0)
  for(i in 1:length(x$DD.period.t)){
    if(subject[i] == "s010"){
      if(session[i] == "4"){
        session_4_DD.period.t <- append(session_4_DD.period.t, DD.period.t[i])
        mean_of_session_4_DD.period.t <- mean(session_4_DD.period.t)
      }
    }
  }
  mean_of_session_4_DD.period.t  
  
  # "s010" "session 5" "DD.Period"
  mean_of_session_5_DD.period.t <- numeric(0)
  session_5_DD.period.t <- numeric(0)
  for(i in 1:length(x$DD.period.t)){
    if(subject[i] == "s010"){
      if(session[i] == "5"){
        session_5_DD.period.t <- append(session_5_DD.period.t, DD.period.t[i])
        mean_of_session_5_DD.period.t <- mean(session_5_DD.period.t)
      }
    }
  }
  mean_of_session_5_DD.period.t  
  
  # "s010" "session 6" "DD. Period"
  mean_of_session_6_DD.period.t <- numeric(0)
  session_6_DD.period.t <- numeric(0)
  for(i in 1:length(x$DD.period.t)){
    if(subject[i] == "s010"){
      if(session[i] == "6"){
        session_6_DD.period.t <- append(session_6_DD.period.t, DD.period.t[i])
        mean_of_session_6_DD.period.t <- mean(session_6_DD.period.t)
      }
    }
  }
  mean_of_session_6_DD.period.t  
  
  # "s010" "session 7" "DD. Period"
  mean_of_session_7_DD.period.t <- numeric(0)
  session_7_DD.period.t <- numeric(0)
  for(i in 1:length(x$DD.period.t)){
    if(subject[i] == "s010"){
      if(session[i] == "7"){
        session_7_DD.period.t <- append(session_7_DD.period.t, DD.period.t[i])
        mean_of_session_7_DD.period.t <- mean(session_7_DD.period.t)
      }
    }
  }
  mean_of_session_7_DD.period.t  
  
  # "s010" "session 8" "DD.Period"
  mean_of_session_8_DD.period.t <- numeric(0)
  session_8_DD.period.t <- numeric(0)
  for(i in 1:length(x$DD.period.t)){
    if(subject[i] == "s010"){
      if(session[i] == "8"){
        session_8_DD.period.t <- append(session_8_DD.period.t, DD.period.t[i])
        mean_of_session_8_DD.period.t <- mean(session_8_DD.period.t)
      }
    }
  }
  mean_of_session_8_DD.period.t  
  
  # "s010" "all sessions" "DD. Period"
  mean_of_all_sessions_DD.period.t <- numeric(0)
  all_sessions_DD.period.t <- numeric(0)
  for(i in 1:length(x$DD.period.t)){
    if(subject[i] == "s010"){
      all_sessions_DD.period.t <- append(all_sessions_DD.period.t, DD.period.t[i])
      mean_of_all_sessions_DD.period.t <- mean(all_sessions_DD.period.t)
    }
  }
  mean_of_all_sessions_DD.period.t
  
  
 #combining all the means into one matrix so it would be easier to plot with:
  means_combined_DD.period_s010<- matrix(data=c(mean_of_session_1_DD.period.t, mean_of_session_2_DD.period.t, 
                                           mean_of_session_3_DD.period.t, mean_of_session_4_DD.period.t,
                                           mean_of_session_5_DD.period.t, mean_of_session_6_DD.period.t, 
                                           mean_of_session_7_DD.period.t, mean_of_session_8_DD.period.t))
  
  #plot to show the difference between the means of different sessions in DD.oeriod.t of s010
  plot(means_combined_DD.period_s010, main="Means of DD.period.t of subject s010", xlab="Means")
  
  
  #standard deviations of different sessions in DD.period for s010:
  # s010 DD.period.t
  sd(session_1_DD.period.t)
  sd(session_2_DD.period.t)
  sd(session_3_DD.period.t)
  sd(session_4_DD.period.t)
  sd(session_5_DD.period.t)
  sd(session_6_DD.period.t)
  sd(session_7_DD.period.t)
  sd(session_8_DD.period.t)
  
  
  #making the standard deviations into a matrix:
  stdev_combined_DD.period_s010<- matrix(data=c(sd(session_1_DD.period.t),
                                                sd(session_2_DD.period.t),
                                                sd(session_3_DD.period.t),
                                                sd(session_4_DD.period.t),
                                                sd(session_5_DD.period.t),
                                                sd(session_6_DD.period.t),
                                                sd(session_7_DD.period.t),
                                                sd(session_8_DD.period.t)))
  
  #scatter plot to compare the differences between the standard deviation values of 
  #different sessions of DD.period for s010.
  
  plot(stdev_combined_DD.period_s010, main="Standard deviations for DD.period of subject s010", xlab="Standard deviations")
  
  
  
  #line plot to compare the mean and standard deviation of DD.period in s010:
  plot(means_combined_DD.period_s010, stdev_combined_DD.period_s010, main="Comparisions of means and standard deviations DD.period across subject s010", xlab="Means", ylab="")
  
  
  
  
  
#subject "s010", UD.period.t variable
  # "s010" "session 1" "UD. Period"
  mean_of_session_1_UD.period.t <- numeric(0)
  session_1_UD.period.t <- numeric(0)
  for(i in 1:length(x$UD.period.t)){
    if(subject[i] == "s010"){
      if(session[i] == "1"){
        session_1_UD.period.t <- append(session_1_UD.period.t, UD.period.t[i])
        mean_of_session_1_UD.period.t <- mean(session_1_UD.period.t)
      }
    }
  }
  mean_of_session_1_UD.period.t  
  
  # "s010" "session 2" 
  mean_of_session_2_UD.period.t <- numeric(0)
  session_2_UD.period.t <- numeric(0)
  for(i in 1:length(x$UD.period.t)){
    if(subject[i] == "s010"){
      if(session[i] == "2"){
        session_2_UD.period.t <- append(session_2_UD.period.t, UD.period.t[i])
        mean_of_session_2_UD.period.t <- mean(session_2_UD.period.t)
      }
    }
  }
  mean_of_session_2_UD.period.t  
  
  # "s010" "session 3" 
  mean_of_session_3_UD.period.t <- numeric(0)
  session_3_UD.period.t <- numeric(0)
  for(i in 1:length(x$UD.period.t)){
    if(subject[i] == "s010"){
      if(session[i] == "3"){
        session_3_UD.period.t <- append(session_3_UD.period.t, UD.period.t[i])
        mean_of_session_3_UD.period.t <- mean(session_3_UD.period.t)
      }
    }
  }
  mean_of_session_3_UD.period.t  
  
  # "s010" "session 4" 
  mean_of_session_4_UD.period.t <- numeric(0)
  session_4_UD.period.t <- numeric(0)
  for(i in 1:length(x$UD.period.t)){
    if(subject[i] == "s010"){
      if(session[i] == "4"){
        session_4_UD.period.t <- append(session_4_UD.period.t, UD.period.t[i])
        mean_of_session_4_UD.period.t <- mean(session_4_UD.period.t)
      }
    }
  }
  mean_of_session_4_UD.period.t  
  
  # "s010" "session 5" 
  mean_of_session_5_UD.period.t <- numeric(0)
  session_5_UD.period.t <- numeric(0)
  for(i in 1:length(x$UD.period.t)){
    if(subject[i] == "s010"){
      if(session[i] == "5"){
        session_5_UD.period.t <- append(session_5_UD.period.t, UD.period.t[i])
        mean_of_session_5_UD.period.t <- mean(session_5_UD.period.t)
      }
    }
  }
  mean_of_session_5_UD.period.t  
  
  # "s010" "session 6" 
  mean_of_session_6_UD.period.t <- numeric(0)
  session_6_UD.period.t <- numeric(0)
  for(i in 1:length(x$UD.period.t)){
    if(subject[i] == "s010"){
      if(session[i] == "6"){
        session_6_UD.period.t <- append(session_6_UD.period.t, UD.period.t[i])
        mean_of_session_6_UD.period.t <- mean(session_6_UD.period.t)
      }
    }
  }
  mean_of_session_6_UD.period.t  
  
  # "s010" "session 7" 
  mean_of_session_7_UD.period.t <- numeric(0)
  session_7_UD.period.t <- numeric(0)
  for(i in 1:length(x$UD.period.t)){
    if(subject[i] == "s010"){
      if(session[i] == "7"){
        session_7_UD.period.t <- append(session_7_UD.period.t, UD.period.t[i])
        mean_of_session_7_UD.period.t <- mean(session_7_UD.period.t)
      }
    }
  }
  mean_of_session_7_UD.period.t  
  
  # "s010" "session 8" 
  mean_of_session_8_UD.period.t <- numeric(0)
  session_8_UD.period.t <- numeric(0)
  for(i in 1:length(x$UD.period.t)){
    if(subject[i] == "s010"){
      if(session[i] == "8"){
        session_8_UD.period.t <- append(session_8_UD.period.t, UD.period.t[i])
        mean_of_session_8_UD.period.t <- mean(session_8_UD.period.t)
      }
    }
  }
  mean_of_session_8_UD.period.t  
  
  # "s010" "all sessions" 
  mean_of_all_sessions_UD.period.t <- numeric(0)
  all_sessions_UD.period.t <- numeric(0)
  for(i in 1:length(x$DD.period.t)){
    if(subject[i] == "s010"){
      all_sessions_UD.period.t <- append(all_sessions_UD.period.t, UD.period.t[i])
      mean_of_all_sessions_UD.period.t <- mean(all_sessions_UD.period.t)
    }
  }
  mean_of_all_sessions_UD.period.t
  
  
  means_combined_UD.period.t_s010<- matrix(data=c(mean_of_session_1_UD.period.t,
                                             mean_of_session_2_UD.period.t,
                                             mean_of_session_3_UD.period.t, 
                                             mean_of_session_4_UD.period.t,
                                             mean_of_session_5_UD.period.t,
                                             mean_of_session_6_UD.period.t,
                                             mean_of_session_7_UD.period.t, 
                                             mean_of_session_8_UD.period.t))
  
  #plot to compare means of the different sessions within UD.period.t for s010
  plot(means_combined_UD.period.t, main="Means of DD.period.t for subject s010")
  
  
  #standard deviations of different sessions within UD.period.t for s010:
  sd(session_1_UD.period.t)
  sd(session_2_UD.period.t)
  sd(session_3_UD.period.t)
  sd(session_4_UD.period.t)
  sd(session_5_UD.period.t)
  sd(session_6_UD.period.t)
  sd(session_7_UD.period.t)
  sd(session_8_UD.period.t)
  
  stdevs_combined_UD.period.t_s010<- matrix(data=c(sd(session_1_UD.period.t),
                                              sd(session_2_UD.period.t),
                                              sd(session_3_UD.period.t),
                                              sd(session_4_UD.period.t),
                                              sd(session_5_UD.period.t),
                                              sd(session_6_UD.period.t),
                                              sd(session_7_UD.period.t),
                                              sd(session_8_UD.period.t)))
  
  #plot to compare the standard deviations of different sessions within UD.period.t for s010.
  
  
  plot(stdevs_combined_UD.period.t, main="Standard deviations of UD.period.t of subject s010", xlab="Standard deviations")
  
  
  #plot to compare the standard deviations and the means of different sessions within UD.period.t for s010.
  plot(stdevs_combined_UD.period.t, means_combined_UD.period.t, main="Comparision of standard deviations and means of DD.period.t for subject s010")
  
  
  
  
  
  
#subject "s010", H.t variable
  # "s010" "session 1"
  mean_of_session_1_H.t <- numeric(0)
  session_1_H.t <- numeric(0)
  for(i in 1:length(x$H.t)){
    if(subject[i] == "s010"){
      if(session[i] == "1"){
        session_1_H.t <- append(session_1_H.t, H.t[i])
        mean_of_session_1_H.t <- mean(session_1_H.t)
      }
    }
  }
  mean_of_session_1_H.t 
  
  # "s010" "session 2"
  mean_of_session_2_H.t <- numeric(0)
  session_2_H.t <- numeric(0)
  for(i in 1:length(x$H.t)){
    if(subject[i] == "s010"){
      if(session[i] == "2"){
        session_2_H.t <- append(session_1_H.t, H.t[i])
        mean_of_session_2_H.t <- mean(session_2_H.t)
      }
    }
  }
  mean_of_session_2_H.t 
  
  # "s010" "session 3"
  mean_of_session_3_H.t <- numeric(0)
  session_3_H.t <- numeric(0)
  for(i in 1:length(x$H.t)){
    if(subject[i] == "s010"){
      if(session[i] == "3"){
        session_3_H.t <- append(session_3_H.t, H.t[i])
        mean_of_session_3_H.t <- mean(session_3_H.t)
      }
    }
  }
  mean_of_session_3_H.t 
  
  # "s010" "session 4"
  mean_of_session_4_H.t <- numeric(0)
  session_4_H.t <- numeric(0)
  for(i in 1:length(x$H.t)){
    if(subject[i] == "s010"){
      if(session[i] == "4"){
        session_4_H.t <- append(session_4_H.t, H.t[i])
        mean_of_session_4_H.t <- mean(session_4_H.t)
      }
    }
  }
  mean_of_session_4_H.t 
  
  # "s010" "session 5"
  mean_of_session_5_H.t <- numeric(0)
  session_5_H.t <- numeric(0)
  for(i in 1:length(x$H.t)){
    if(subject[i] == "s010"){
      if(session[i] == "5"){
        session_5_H.t <- append(session_5_H.t, H.t[i])
        mean_of_session_5_H.t <- mean(session_5_H.t)
      }
    }
  }
  mean_of_session_5_H.t 
  
  # "s010" "session 6"
  mean_of_session_6_H.t <- numeric(0)
  session_6_H.t <- numeric(0)
  for(i in 1:length(x$H.t)){
    if(subject[i] == "s010"){
      if(session[i] == "6"){
        session_6_H.t <- append(session_6_H.t, H.t[i])
        mean_of_session_6_H.t <- mean(session_6_H.t)
      }
    }
  }
  mean_of_session_6_H.t 
  
  # "s010" "session 7"
  mean_of_session_7_H.t <- numeric(0)
  session_7_H.t <- numeric(0)
  for(i in 1:length(x$H.t)){
    if(subject[i] == "s010"){
      if(session[i] == "7"){
        session_7_H.t <- append(session_7_H.t, H.t[i])
        mean_of_session_7_H.t <- mean(session_7_H.t)
      }
    }
  }
  mean_of_session_7_H.t 
  
  # "s010" "session 8"
  mean_of_session_8_H.t <- numeric(0)
  session_8_H.t <- numeric(0)
  for(i in 1:length(x$H.t)){
    if(subject[i] == "s010"){
      if(session[i] == "8"){
        session_8_H.t <- append(session_8_H.t, H.t[i])
        mean_of_session_8_H.t <- mean(session_8_H.t)
      }
    }
  }
  mean_of_session_8_H.t 
  
  # "s010" "all sessions"
  mean_of_all_sessions_H.t <- numeric(0)
  all_sessions_H.t <- numeric(0)
  for(i in 1:length(x$H.t)){
    if(subject[i] == "s010"){
      all_sessions_H.t <- append(all_sessions_H.t, H.t[i])
      mean_of_all_sessions_H.t <- mean(all_sessions_H.t)
    }
  }
  mean_of_all_sessions_H.t
  
  means_combined_H.t_s010<- matrix(data=c(mean_of_session_1_H.t, 
                                          mean_of_session_2_H.t,
                                          mean_of_session_3_H.t,
                                          mean_of_session_4_H.t,
                                          mean_of_session_5_H.t,
                                          mean_of_session_6_H.t, 
                                          mean_of_session_7_H.t, 
                                          mean_of_session_8_H.t))
  
  #plot to compare the different means of different sessions in H.t of s010.
  plot(means_combined_H.period_s010, main="Means of H.t of subject s010", xlab="Means")
  
  
  #standard deviations of the different sessions of H.t withing s010:
  sd(session_1_H.t)
  sd(session_2_H.t)
  sd(session_3_H.t)
  sd(session_4_H.t)
  sd(session_5_H.t)
  sd(session_6_H.t)
  sd(session_7_H.t)
  sd(session_8_H.t)
  
  #converting this into a matrix.
  stdevs_combined_H.t_s010<- matrix(data=c(sd(session_1_H.t),
                                           sd(session_2_H.t),
                                           sd(session_3_H.t),
                                           sd(session_4_H.t),
                                           sd(session_5_H.t),
                                           sd(session_6_H.t),
                                           sd(session_7_H.t),
                                           sd(session_8_H.t)))
  
  #plot to compare the standard deviations across the different sessions of H.t for s010.
  plot(stdevs_combined_H.t_s010, main="Standard deviations of H.t of subject s010", xlabs="Standard deviations")
  
  #plots to compare the standard deviations and
  
  plot(stdevs_combined_H.t_s010, means_combined_H.t_s010, main="Comparision of means and standard deviations of H.t for s010")
  
  
  
  
  
  
  
#subject "s010", DD.t.i variable
  # "s010" "session 1"
  mean_of_session_1_DD.t.i <- numeric(0)
  session_1_DD.t.i <- numeric(0)
  for(i in 1:length(x$DD.t.i)){
    if(subject[i] == "s010"){
      if(session[i] == "1"){
        session_1_DD.t.i <- append(session_1_DD.t.i, DD.t.i[i])
        mean_of_session_1_DD.t.i <- mean(session_1_DD.t.i)
      }
    }
  }
  mean_of_session_1_DD.t.i 
  
  # "s010" "session 2"
  mean_of_session_2_DD.t.i <- numeric(0)
  session_2_DD.t.i <- numeric(0)
  for(i in 1:length(x$DD.t.i)){
    if(subject[i] == "s010"){
      if(session[i] == "2"){
        session_2_DD.t.i <- append(session_2_DD.t.i, DD.t.i[i])
        mean_of_session_2_DD.t.i <- mean(session_2_DD.t.i)
      }
    }
  }
  mean_of_session_2_DD.t.i 
  
  # "s010" "session 3"
  mean_of_session_3_DD.t.i <- numeric(0)
  session_3_DD.t.i <- numeric(0)
  for(i in 1:length(x$DD.t.i)){
    if(subject[i] == "s010"){
      if(session[i] == "3"){
        session_3_DD.t.i <- append(session_3_DD.t.i, DD.t.i[i])
        mean_of_session_3_DD.t.i <- mean(session_3_DD.t.i)
      }
    }
  }
  mean_of_session_3_DD.t.i 
  
  # "s010" "session 4"
  mean_of_session_4_DD.t.i <- numeric(0)
  session_4_DD.t.i <- numeric(0)
  for(i in 1:length(x$DD.t.i)){
    if(subject[i] == "s010"){
      if(session[i] == "4"){
        session_4_DD.t.i <- append(session_4_DD.t.i, DD.t.i[i])
        mean_of_session_4_DD.t.i <- mean(session_4_DD.t.i)
      }
    }
  }
  mean_of_session_4_DD.t.i 
  
  # "s010" "session 5"
  mean_of_session_5_DD.t.i <- numeric(0)
  session_5_DD.t.i <- numeric(0)
  for(i in 1:length(x$DD.t.i)){
    if(subject[i] == "s010"){
      if(session[i] == "5"){
        session_5_DD.t.i <- append(session_5_DD.t.i, DD.t.i[i])
        mean_of_session_5_DD.t.i <- mean(session_5_DD.t.i)
      }
    }
  }
  mean_of_session_5_DD.t.i 
  
  # "s010" "session 6"
  mean_of_session_6_DD.t.i <- numeric(0)
  session_6_DD.t.i <- numeric(0)
  for(i in 1:length(x$DD.t.i)){
    if(subject[i] == "s010"){
      if(session[i] == "6"){
        session_6_DD.t.i <- append(session_6_DD.t.i, DD.t.i[i])
        mean_of_session_6_DD.t.i <- mean(session_6_DD.t.i)
      }
    }
  }
  mean_of_session_6_DD.t.i 
  
  # "s010" "session 7"
  mean_of_session_7_DD.t.i <- numeric(0)
  session_7_DD.t.i <- numeric(0)
  for(i in 1:length(x$DD.t.i)){
    if(subject[i] == "s010"){
      if(session[i] == "7"){
        session_7_DD.t.i <- append(session_7_DD.t.i, DD.t.i[i])
        mean_of_session_7_DD.t.i <- mean(session_7_DD.t.i)
      }
    }
  }
  mean_of_session_7_DD.t.i 
  
  # "s010" "session 8"
  mean_of_session_8_DD.t.i <- numeric(0)
  session_8_DD.t.i <- numeric(0)
  for(i in 1:length(x$DD.t.i)){
    if(subject[i] == "s010"){
      if(session[i] == "8"){
        session_8_DD.t.i <- append(session_8_DD.t.i, DD.t.i[i])
        mean_of_session_8_DD.t.i <- mean(session_8_DD.t.i)
      }
    }
  }
  mean_of_session_8_DD.t.i 
  
  # "s010" "all sessions"
  mean_of_all_sessions_DD.t.i <- numeric(0)
  all_sessions_DD.t.i <- numeric(0)
  for(i in 1:length(x$DD.t.i)){
    if(subject[i] == "s010"){
      all_sessions_DD.t.i <- append(all_sessions_DD.t.i, DD.t.i[i])
      mean_of_all_sessions_DD.t.i <- mean(all_sessions_DD.t.i)
    }
  }
  mean_of_all_sessions_DD.t.i
  
  means_combined_H.t_s010<- matrix(data=c(mean_of_session_1_DD.t.i, 
                                          mean_of_session_2_DD.t.i,
                                          mean_of_session_3_DD.t.i,
                                          mean_of_session_4_DD.t.u,
                                          mean_of_session_5_DD.t.i,
                                          mean_of_session_6_DD.t.i, 
                                          mean_of_session_7_DD.t.i, 
                                          mean_of_session_8_DD.t.i))
  
  #plot to compare the different means of different sessions in H.t of s010.
  plot(means_combined_DD.t.i_s010, main="Means of DD.t.i for subject s010", xlab="Means")
  
  
  #standard deviations of the different sessions of H.t withing s010:
  sd(session_1_DD.t.i)
  sd(session_2_DD.t.i)
  sd(session_3_DD.t.i)
  sd(session_4_DD.t.i)
  sd(session_5_DD.t.i)
  sd(session_6_DD.t.i)
  sd(session_7_DD.t.i)
  sd(session_8_DD.t.i)
  
  #converting this into a matrix.
  stdevs_combined_H.t_s010<- matrix(data=c(sd(session_1_DD.t.i),
                                           sd(session_2_DD.t.i),
                                           sd(session_3_DD.t.i),
                                           sd(session_4_DD.t.i),
                                           sd(session_5_DD.t.i),
                                           sd(session_6_DD.t.i),
                                           sd(session_7_DD.t.i),
                                           sd(session_8_DD.t.i)))
  
  #plot to compare the standard deviations across the different sessions of H.t for s010.
  plot(stdevs_combined_DD.t.i_s010, main="Standard deviations of DD.t.i for subject s010", xlab="Standard deviations")
  
  #plots to compare the standard deviations and
  
  plot(stdevs_combined_DD.t.i_s010, means_combined_DD.t.i_s010, main="Comparision of Means and standard deviations of DD.t.i of s010")
  
  
  
  
  
  
  
#subject "s024", H.period variable
# "s024" "session 1"
  s024_mean_of_session_1_H.period <- numeric(0)
  s024_session_1_H.period <- numeric(0)
  for(i in 1:length(x$H.period)){
    if(subject[i] == "s024"){
      if(session[i] == "1"){
        s024_session_1_H.period <- append(s024_session_1_H.period, H.period[i])
        s024_mean_of_session_1_H.period <- mean(s024_session_1_H.period)
      }
    }
  }
  s024_mean_of_session_1_H.period 
  
  # "s024" "session 2"
  s024_mean_of_session_2_H.period <- numeric(0)
  s024_session_2_H.period <- numeric(0)
  for(i in 1:length(x$H.period)){
    if(subject[i] == "s024"){
      if(session[i] == "2"){
        s024_session_2_H.period <- append(s024_session_2_H.period, H.period[i])
        s024_mean_of_session_2_H.period <- mean(s024_session_2_H.period)
      }
    }
  }
  s024_mean_of_session_2_H.period 
  
  # "s024" "session 3"
  s024_mean_of_session_3_H.period <- numeric(0)
  s024_session_3_H.period <- numeric(0)
  for(i in 1:length(x$H.period)){
    if(subject[i] == "s024"){
      if(session[i] == "3"){
        s024_session_3_H.period <- append(s024_session_3_H.period, H.period[i])
        s024_mean_of_session_3_H.period <- mean(s024_session_3_H.period)
      }
    }
  }
  s024_mean_of_session_3_H.period 
  
  # "s024" "session 4"
  s024_mean_of_session_4_H.period <- numeric(0)
  s024_session_4_H.period <- numeric(0)
  for(i in 1:length(x$H.period)){
    if(subject[i] == "s024"){
      if(session[i] == "4"){
        s024_session_4_H.period <- append(s024_session_4_H.period, H.period[i])
        s024_mean_of_session_4_H.period <- mean(s024_session_4_H.period)
      }
    }
  }
  s024_mean_of_session_4_H.period 
  
  # "s024" "session 5"
  s024_mean_of_session_5_H.period <- numeric(0)
  s024_session_5_H.period <- numeric(0)
  for(i in 1:length(x$H.period)){
    if(subject[i] == "s024"){
      if(session[i] == "5"){
        s024_session_5_H.period <- append(s024_session_5_H.period, H.period[i])
        s024_mean_of_session_5_H.period <- mean(s024_session_5_H.period)
      }
    }
  }
  s024_mean_of_session_5_H.period 
  
  # "s024" "session 6"
  s024_mean_of_session_6_H.period <- numeric(0)
  s024_session_6_H.period <- numeric(0)
  for(i in 1:length(x$H.period)){
    if(subject[i] == "s024"){
      if(session[i] == "6"){
        s024_session_6_H.period <- append(s024_session_6_H.period, H.period[i])
        s024_mean_of_session_6_H.period <- mean(s024_session_6_H.period)
      }
    }
  }
  s024_mean_of_session_6_H.period 
  
  # "s024" "session 7"
  s024_mean_of_session_7_H.period <- numeric(0)
  s024_session_7_H.period <- numeric(0)
  for(i in 1:length(x$H.period)){
    if(subject[i] == "s024"){
      if(session[i] == "7"){
        s024_session_7_H.period <- append(s024_session_7_H.period, H.period[i])
        s024_mean_of_session_7_H.period <- mean(s024_session_7_H.period)
      }
    }
  }
  s024_mean_of_session_7_H.period 
  
  # "s024" "session 8"
  s024_mean_of_session_8_H.period <- numeric(0)
  s024_session_8_H.period <- numeric(0)
  for(i in 1:length(x$H.period)){
    if(subject[i] == "s024"){
      if(session[i] == "8"){
        s024_session_8_H.period <- append(s024_session_8_H.period, H.period[i])
        s024_mean_of_session_8_H.period <- mean(s024_session_8_H.period)
      }
    }
  }
  s024_mean_of_session_8_H.period 
  
  # "s024" "all sessions"
  s024_mean_of_all_sessions_H.period <- numeric(0)
  s024_all_sessions_H.period <- numeric(0)
  for(i in 1:length(x$H.period)){
    if(subject[i] == "s024"){
      s024_all_sessions_H.period <- append(s024_all_sessions_H.period, H.period[i])
      s024_mean_of_all_sessions_H.period <- mean(s024_all_sessions_H.period)
    }
  }
  s024_mean_of_all_sessions_H.period
  
  
  
  
  s024_means_combined_H.period<- matrix(data=c(s024_mean_of_session_1_H.period, 
                                          s024_mean_of_session_2_H.period,
                                          s024_mean_of_session_3_H.period,
                                          s024_mean_of_session_4_H.period,
                                          s024_mean_of_session_5_H.period,
                                          s024_mean_of_session_6_H.period, 
                                          s024_mean_of_session_7_H.period, 
                                          s024_mean_of_session_8_H.period))
  
  #plot to compare the different means of different sessions in H.t of s010.
  plot(s024_means_combined_H.period)
  
  
  #standard deviations of the different sessions of H.t withing s010:
  sd(s024_session_1_H.period)
  sd(s024_session_2_H.period)
  sd(s024_session_3_H.period)
  sd(s024_session_4_H.period)
  sd(s024_session_5_H.period)
  sd(s024_session_6_H.period)
  sd(s024_session_7_H.period)
  sd(s024_session_8_H.period)
  
  #converting this into a matrix.
  s024_stdevs_combined_H.period<- matrix(data=c( sd(s024_session_1_H.period),
                                            sd(s024_session_2_H.period),
                                            sd(s024_session_3_H.period),
                                            sd(s024_session_4_H.period),
                                            sd(s024_session_5_H.period),
                                            sd(s024_session_6_H.period),
                                            sd(s024_session_7_H.period),
                                            sd(s024_session_8_H.period)))
  
  #plot to compare the standard deviations across the different sessions of H.t for s010.
  plot(s024_stdevs_combined_H.period)
  
  #plots to compare the standard deviations and
  
  plot(s024_stdevs_combined_H.period, s024_means_combined_H.period, type="l")
  
  
  
  
  
  
#subject "s024", DD.period.t variable
  # "s024" "session 1"
  s024_mean_of_session_1_DD.period.t <- numeric(0)
  s024_session_1_DD.period.t <- numeric(0)
  for(i in 1:length(x$DD.period.t)){
    if(subject[i] == "s024"){
      if(session[i] == "1"){
        s024_session_1_DD.period.t <- append(s024_session_1_DD.period.t, DD.period.t[i])
        s024_mean_of_session_1_DD.period.t <- mean(s024_session_1_DD.period.t)
      }
    }
  }
  s024_mean_of_session_1_DD.period.t 
  
  # "s024" "session 2"
  s024_mean_of_session_2_DD.period.t <- numeric(0)
  s024_session_2_DD.period.t <- numeric(0)
  for(i in 1:length(x$DD.period.t)){
    if(subject[i] == "s024"){
      if(session[i] == "2"){
        s024_session_2_DD.period.t <- append(s024_session_2_DD.period.t, DD.period.t[i])
        s024_mean_of_session_2_DD.period.t <- mean(s024_session_2_DD.period.t)
      }
    }
  }
  s024_mean_of_session_2_DD.period.t
  
  # "s024" "session 3"
  s024_mean_of_session_3_DD.period.t <- numeric(0)
  s024_session_3_DD.period.t <- numeric(0)
  for(i in 1:length(x$DD.period.t)){
    if(subject[i] == "s024"){
      if(session[i] == "3"){
        s024_session_3_DD.period.t <- append(s024_session_3_DD.period.t, DD.period.t[i])
        s024_mean_of_session_3_DD.period.t <- mean(s024_session_3_DD.period.t)
      }
    }
  }
  s024_mean_of_session_3_DD.period.t 
  
  # "s024" "session 4"
  s024_mean_of_session_4_DD.period.t <- numeric(0)
  s024_session_4_DD.period.t <- numeric(0)
  for(i in 1:length(x$DD.period.t)){
    if(subject[i] == "s024"){
      if(session[i] == "4"){
        s024_session_4_DD.period.t <- append(s024_session_4_DD.period.t, DD.period.t[i])
        s024_mean_of_session_4_DD.period.t <- mean(s024_session_4_DD.period.t)
      }
    }
  }
  s024_mean_of_session_4_DD.period.t 
  
  # "s024" "session 5"
  s024_mean_of_session_5_DD.period.t <- numeric(0)
  s024_session_5_DD.period.t <- numeric(0)
  for(i in 1:length(x$DD.period.t)){
    if(subject[i] == "s024"){
      if(session[i] == "5"){
        s024_session_5_DD.period.t <- append(s024_session_5_DD.period.t, DD.period.t[i])
        s024_mean_of_session_5_DD.period.t <- mean(s024_session_5_DD.period.t)
      }
    }
  }
  s024_mean_of_session_5_DD.period.t 
  
  # "s024" "session 6"
  s024_mean_of_session_6_DD.period.t <- numeric(0)
  s024_session_6_DD.period.t <- numeric(0)
  for(i in 1:length(x$DD.period.t)){
    if(subject[i] == "s024"){
      if(session[i] == "6"){
        s024_session_6_DD.period.t <- append(s024_session_6_DD.period.t, DD.period.t[i])
        s024_mean_of_session_6_DD.period.t <- mean(s024_session_6_DD.period.t)
      }
    }
  }
  s024_mean_of_session_6_DD.period.t 
  
  # "s024" "session 7"
  s024_mean_of_session_7_DD.period.t <- numeric(0)
  s024_session_7_DD.period.t <- numeric(0)
  for(i in 1:length(x$DD.period.t)){
    if(subject[i] == "s024"){
      if(session[i] == "7"){
        s024_session_7_DD.period.t <- append(s024_session_7_DD.period.t, DD.period.t[i])
        s024_mean_of_session_7_DD.period.t <- mean(s024_session_7_DD.period.t)
      }
    }
  }
  s024_mean_of_session_7_DD.period.t 
  
  # "s024" "session 8"
  s024_mean_of_session_8_DD.period.t <- numeric(0)
  s024_session_8_DD.period.t <- numeric(0)
  for(i in 1:length(x$DD.period.t)){
    if(subject[i] == "s024"){
      if(session[i] == "8"){
        s024_session_8_DD.period.t <- append(s024_session_8_DD.period.t, DD.period.t[i])
        s024_mean_of_session_8_DD.period.t <- mean(s024_session_8_DD.period.t)
      }
    }
  }
  s024_mean_of_session_8_DD.period.t 
  
  # "s024" "all sessions"
  s024_mean_of_all_sessions_DD.period.t <- numeric(0)
  s024_all_sessions_DD.period.t <- numeric(0)
  for(i in 1:length(x$DD.period.t)){
    if(subject[i] == "s024"){
      s024_all_sessions_DD.period.t <- append(s024_all_sessions_DD.period.t, DD.period.t[i])
      s024_mean_of_all_sessions_DD.period.t <- mean(s024_all_sessions_DD.period.t)
    }
  }
  s024_mean_of_all_sessions_DD.period.t
  
  
  
  
  s024_means_combined_DD.period.t<- matrix(data=c(s024_mean_of_session_1_DD.period.t, 
                                               s024_mean_of_session_2_DD.period.t,
                                               s024_mean_of_session_3_DD.period.t,
                                               s024_mean_of_session_4_DD.period.t,
                                               s024_mean_of_session_5_DD.period.t,
                                               s024_mean_of_session_6_DD.period.t, 
                                               s024_mean_of_session_7_DD.period.t, 
                                               s024_mean_of_session_8_DD.period.t))
  
  #plot to compare the different means of different sessions in H.t of s010.
  plot(s024_means_combined_DD.period.t)
  
  
  #standard deviations of the different sessions of H.t withing s010:
  sd(s024_session_1_DD.period.t)
  sd(s024_session_2_DD.period.t)
  sd(s024_session_3_DD.period.t)
  sd(s024_session_4_DD.period.t)
  sd(s024_session_5_DD.period.t)
  sd(s024_session_6_DD.period.t)
  sd(s024_session_7_DD.period.t)
  sd(s024_session_8_DD.period.t)
  
  #converting this into a matrix.
  s024_stdevs_combined_DD.period.t<- matrix(data=c( sd(s024_session_1_DD.period.t),
                                                 sd(s024_session_2_DD.period.t),
                                                 sd(s024_session_3_DD.period.t),
                                                 sd(s024_session_4_DD.period.t),
                                                 sd(s024_session_5_DD.period.t),
                                                 sd(s024_session_6_DD.period.t),
                                                 sd(s024_session_7_DD.period.t),
                                                 sd(s024_session_8_DD.period.t)))
  
  #plot to compare the standard deviations across the different sessions of H.t for s010.
  plot(s024_stdevs_combined_DD.period.t)
  
  #plots to compare the standard deviations and
  
  plot(s024_stdevs_combined_DD.period.t, s024_means_combined_DD.period.t, type="l")
  
  
  
  
#subject "s024", UD.period.t variable
  # "s024" "session 1"
  s024_mean_of_session_1_UD.period.t <- numeric(0)
  s024_session_1_UD.period.t <- numeric(0)
  for(i in 1:length(x$UD.period.t)){
    if(subject[i] == "s024"){
      if(session[i] == "1"){
        s024_session_1_UD.period.t <- append(s024_session_1_UD.period.t, UD.period.t[i])
        s024_mean_of_session_1_UD.period.t <- mean(s024_session_1_UD.period.t)
      }
    }
  }
  s024_mean_of_session_1_UD.period.t
  
  # "s024" "session 2"
  s024_mean_of_session_2_UD.period.t <- numeric(0)
  s024_session_2_UD.period.t <- numeric(0)
  for(i in 1:length(x$UD.period.t)){
    if(subject[i] == "s024"){
      if(session[i] == "2"){
        s024_session_2_UD.period.t <- append(s024_session_2_UD.period.t, UD.period.t[i])
        s024_mean_of_session_2_UD.period.t <- mean(s024_session_2_UD.period.t)
      }
    }
  }
  s024_mean_of_session_2_UD.period.t
  
  # "s024" "session 3"
  s024_mean_of_session_3_UD.period.t <- numeric(0)
  s024_session_3_UD.period.t <- numeric(0)
  for(i in 1:length(x$UD.period.t)){
    if(subject[i] == "s024"){
      if(session[i] == "3"){
        s024_session_3_UD.period.t <- append(s024_session_3_UD.period.t, UD.period.t[i])
        s024_mean_of_session_3_UD.period.t <- mean(s024_session_3_UD.period.t)
      }
    }
  }
  s024_mean_of_session_3_UD.period.t
  
  # "s024" "session 4"
  s024_mean_of_session_4_UD.period.t <- numeric(0)
  s024_session_4_UD.period.t <- numeric(0)
  for(i in 1:length(x$UD.period.t)){
    if(subject[i] == "s024"){
      if(session[i] == "4"){
        s024_session_4_UD.period.t <- append(s024_session_4_UD.period.t, UD.period.t[i])
        s024_mean_of_session_4_UD.period.t <- mean(s024_session_4_UD.period.t)
      }
    }
  }
  s024_mean_of_session_4_UD.period.t
  
  # "s024" "session 5"
  s024_mean_of_session_5_UD.period.t <- numeric(0)
  s024_session_5_UD.period.t <- numeric(0)
  for(i in 1:length(x$UD.period.t)){
    if(subject[i] == "s024"){
      if(session[i] == "5"){
        s024_session_5_UD.period.t <- append(s024_session_5_UD.period.t, UD.period.t[i])
        s024_mean_of_session_5_UD.period.t <- mean(s024_session_5_UD.period.t)
      }
    }
  }
  s024_mean_of_session_5_UD.period.t
  
  # "s024" "session 6"
  s024_mean_of_session_6_UD.period.t <- numeric(0)
  s024_session_6_UD.period.t <- numeric(0)
  for(i in 1:length(x$UD.period.t)){
    if(subject[i] == "s024"){
      if(session[i] == "6"){
        s024_session_6_UD.period.t <- append(s024_session_6_UD.period.t, UD.period.t[i])
        s024_mean_of_session_6_UD.period.t <- mean(s024_session_6_UD.period.t)
      }
    }
  }
  s024_mean_of_session_6_UD.period.t
  
  # "s024" "session 7"
  s024_mean_of_session_7_UD.period.t <- numeric(0)
  s024_session_7_UD.period.t <- numeric(0)
  for(i in 1:length(x$UD.period.t)){
    if(subject[i] == "s024"){
      if(session[i] == "7"){
        s024_session_7_UD.period.t <- append(s024_session_7_UD.period.t, UD.period.t[i])
        s024_mean_of_session_7_UD.period.t <- mean(s024_session_7_UD.period.t)
      }
    }
  }
  s024_mean_of_session_7_UD.period.t
  
  # "s024" "session 8"
  s024_mean_of_session_8_UD.period.t <- numeric(0)
  s024_session_8_UD.period.t <- numeric(0)
  for(i in 1:length(x$UD.period.t)){
    if(subject[i] == "s024"){
      if(session[i] == "8"){
        s024_session_8_UD.period.t <- append(s024_session_8_UD.period.t, UD.period.t[i])
        s024_mean_of_session_8_UD.period.t <- mean(s024_session_8_UD.period.t)
      }
    }
  }
  s024_mean_of_session_8_UD.period.t
  
  # "s024" "all sessions"
  s024_mean_of_all_sessions_UD.period.t <- numeric(0)
  s024_all_sessions_UD.period.t <- numeric(0)
  for(i in 1:length(x$UD.period.t)){
    if(subject[i] == "s024"){
      s024_all_sessions_UD.period.t <- append(s024_all_sessions_UD.period.t, UD.period.t[i])
      s024_mean_of_all_sessions_UD.period.t <- mean(s024_all_sessions_UD.period.t)
    }
  }
  s024_mean_of_all_sessions_UD.period.t
  
  
  s024_means_combined_UD.period.t<- matrix(data=c(s024_mean_of_session_1_UD.period.t, 
                                                  s024_mean_of_session_2_UD.period.t,
                                                  s024_mean_of_session_3_UD.period.t,
                                                  s024_mean_of_session_4_UD.period.t,
                                                  s024_mean_of_session_5_UD.period.t,
                                                  s024_mean_of_session_6_UD.period.t, 
                                                  s024_mean_of_session_7_UD.period.t, 
                                                  s024_mean_of_session_8_UD.period.t))
  
  #plot to compare the different means of different sessions in H.t of s010.
  plot(s024_means_combined_UD.period.t)
  
  
  #standard deviations of the different sessions of H.t withing s010:
  sd(s024_session_1_UD.period.t)
  sd(s024_session_2_UD.period.t)
  sd(s024_session_3_UD.period.t)
  sd(s024_session_4_UD.period.t)
  sd(s024_session_5_UD.period.t)
  sd(s024_session_6_UD.period.t)
  sd(s024_session_7_UD.period.t)
  sd(s024_session_8_UD.period.t)
  
  #converting this into a matrix.
  s024_stdevs_combined_DD.period.t<- matrix(data=c( sd(s024_session_1_UD.period.t),
                                                    sd(s024_session_2_UD.period.t),
                                                    sd(s024_session_3_UD.period.t),
                                                    sd(s024_session_4_UD.period.t),
                                                    sd(s024_session_5_UD.period.t),
                                                    sd(s024_session_6_UD.period.t),
                                                    sd(s024_session_7_UD.period.t),
                                                    sd(s024_session_8_UD.period.t)))
  
  #plot to compare the standard deviations across the different sessions of H.t for s010.
  plot(s024_stdevs_combined_UD.period.t)
  
  #plots to compare the standard deviations and
  
  plot(s024_stdevs_combined_UD.period.t, s024_means_combined_UD.period.t, type="l")
  
  
  
  
  
  
  
#subject "s024", H.t variable
  # "s024" "session 1"
  s024_mean_of_session_1_H.t <- numeric(0)
  s024_session_1_H.t <- numeric(0)
  for(i in 1:length(x$H.t)){
    if(subject[i] == "s024"){
      if(session[i] == "1"){
        s024_session_1_H.t <- append(s024_session_1_H.t, H.t[i])
        s024_mean_of_session_1_H.t <- mean(s024_session_1_H.t)
      }
    }
  }
  s024_mean_of_session_1_H.t 
  
  # "s024" "session 2"
  s024_mean_of_session_2_H.t <- numeric(0)
  s024_session_2_H.t <- numeric(0)
  for(i in 1:length(x$H.t)){
    if(subject[i] == "s024"){
      if(session[i] == "2"){
        s024_session_2_H.t <- append(s024_session_2_H.t, H.t[i])
        s024_mean_of_session_2_H.t <- mean(s024_session_2_H.t)
      }
    }
  }
  s024_mean_of_session_2_H.t 
  
  # "s024" "session 3"
  s024_mean_of_session_3_H.t <- numeric(0)
  s024_session_3_H.t <- numeric(0)
  for(i in 1:length(x$H.t)){
    if(subject[i] == "s024"){
      if(session[i] == "3"){
        s024_session_3_H.t <- append(s024_session_3_H.t, H.t[i])
        s024_mean_of_session_3_H.t <- mean(s024_session_3_H.t)
      }
    }
  }
  s024_mean_of_session_3_H.t 
  
  # "s024" "session 4"
  s024_mean_of_session_4_H.t <- numeric(0)
  s024_session_4_H.t <- numeric(0)
  for(i in 1:length(x$H.t)){
    if(subject[i] == "s024"){
      if(session[i] == "4"){
        s024_session_4_H.t <- append(s024_session_4_H.t, H.t[i])
        s024_mean_of_session_4_H.t <- mean(s024_session_4_H.t)
      }
    }
  }
  s024_mean_of_session_4_H.t 
  
  # "s024" "session 5"
  s024_mean_of_session_5_H.t <- numeric(0)
  s024_session_5_H.t <- numeric(0)
  for(i in 1:length(x$H.t)){
    if(subject[i] == "s024"){
      if(session[i] == "5"){
        s024_session_5_H.t <- append(s024_session_5_H.t, H.t[i])
        s024_mean_of_session_5_H.t <- mean(s024_session_5_H.t)
      }
    }
  }
  s024_mean_of_session_5_H.t 
  
  # "s024" "session 6"
  s024_mean_of_session_6_H.t <- numeric(0)
  s024_session_6_H.t <- numeric(0)
  for(i in 1:length(x$H.t)){
    if(subject[i] == "s024"){
      if(session[i] == "6"){
        s024_session_6_H.t <- append(s024_session_6_H.t, H.t[i])
        s024_mean_of_session_6_H.t <- mean(s024_session_6_H.t)
      }
    }
  }
  s024_mean_of_session_6_H.t 
  
  # "s024" "session 7"
  s024_mean_of_session_7_H.t <- numeric(0)
  s024_session_7_H.t <- numeric(0)
  for(i in 1:length(x$H.t)){
    if(subject[i] == "s024"){
      if(session[i] == "7"){
        s024_session_7_H.t <- append(s024_session_7_H.t, H.t[i])
        s024_mean_of_session_7_H.t <- mean(s024_session_7_H.t)
      }
    }
  }
  s024_mean_of_session_7_H.t 
  
  # "s024" "session 8"
  s024_mean_of_session_8_H.t <- numeric(0)
  s024_session_8_H.t <- numeric(0)
  for(i in 1:length(x$H.t)){
    if(subject[i] == "s024"){
      if(session[i] == "8"){
        s024_session_8_H.t <- append(s024_session_8_H.t, H.t[i])
        s024_mean_of_session_8_H.t <- mean(s024_session_8_H.t)
      }
    }
  }
  s024_mean_of_session_8_H.t 
  
  # "s024" "all sessions"
  s024_mean_of_all_sessions_H.t <- numeric(0)
  s024_all_sessions_H.t <- numeric(0)
  for(i in 1:length(x$H.t)){
    if(subject[i] == "s024"){
      s024_all_sessions_H.t <- append(s024_all_sessions_H.t, H.t[i])
      s024_mean_of_all_sessions_H.t <- mean(s024_all_sessions_H.t)
    }
  }
  s024_mean_of_all_sessions_H.t
  
  
  
  
  s024_means_combined_H.t<- matrix(data=c(s024_mean_of_session_1_H.t, 
                                                  s024_mean_of_session_2_H.t,
                                                  s024_mean_of_session_3_H.t,
                                                  s024_mean_of_session_4_H.t,
                                                  s024_mean_of_session_5_H.t,
                                                  s024_mean_of_session_6_H.t, 
                                                  s024_mean_of_session_7_H.t, 
                                                  s024_mean_of_session_8_H.t))
  
  #plot to compare the different means of different sessions in H.t of s010.
  plot(s024_means_combined_H.t)
  
  
  #standard deviations of the different sessions of H.t withing s010:
  sd(s024_session_1_H.t)
  sd(s024_session_2_H.t)
  sd(s024_session_3_H.t)
  sd(s024_session_4_H.t)
  sd(s024_session_5_H.t)
  sd(s024_session_6_H.t)
  sd(s024_session_7_H.t)
  sd(s024_session_8_H.t)
  
  #converting this into a matrix.
  s024_stdevs_combined_H.t<- matrix(data=c( sd(s024_session_1_H.t),
                                                    sd(s024_session_2_H.t),
                                                    sd(s024_session_3_H.t),
                                                    sd(s024_session_4_H.t),
                                                    sd(s024_session_5_H.t),
                                                    sd(s024_session_6_H.t),
                                                    sd(s024_session_7_H.t),
                                                    sd(s024_session_8_H.t)))
  
  #plot to compare the standard deviations across the different sessions of H.t for s010.
  plot(s024_stdevs_combined_H.t)
  
  #plots to compare the standard deviations and
  
  plot(s024_stdevs_combined_H.t, s024_means_combined_H.t, type="l")
  
  
  
  
  
  
#subject "s024", DD.t.i variable
  # "s024" "session 1"
  s024_mean_of_session_1_DD.t.i <- numeric(0)
  s024_session_1_DD.t.i <- numeric(0)
  for(i in 1:length(x$DD.t.i)){
    if(subject[i] == "s024"){
      if(session[i] == "1"){
        s024_session_1_DD.t.i <- append(s024_session_1_DD.t.i, DD.t.i[i])
        s024_mean_of_session_1_DD.t.i <- mean(s024_session_1_DD.t.i)
      }
    }
  }
  s024_mean_of_session_1_DD.t.i 
  
  # "s024" "session 2"
  s024_mean_of_session_2_DD.t.i <- numeric(0)
  s024_session_2_DD.t.i <- numeric(0)
  for(i in 1:length(x$DD.t.i)){
    if(subject[i] == "s024"){
      if(session[i] == "2"){
        s024_session_2_DD.t.i <- append(s024_session_2_DD.t.i, DD.t.i[i])
        s024_mean_of_session_2_DD.t.i <- mean(s024_session_2_DD.t.i)
      }
    }
  }
  s024_mean_of_session_2_DD.t.i 
  
  # "s024" "session 3"
  s024_mean_of_session_3_DD.t.i <- numeric(0)
  s024_session_3_DD.t.i <- numeric(0)
  for(i in 1:length(x$DD.t.i)){
    if(subject[i] == "s024"){
      if(session[i] == "3"){
        s024_session_3_DD.t.i <- append(s024_session_3_DD.t.i, DD.t.i[i])
        s024_mean_of_session_3_DD.t.i <- mean(s024_session_3_DD.t.i)
      }
    }
  }
  s024_mean_of_session_3_DD.t.i 
  
  # "s024" "session 4"
  s024_mean_of_session_4_DD.t.i <- numeric(0)
  s024_session_4_DD.t.i <- numeric(0)
  for(i in 1:length(x$DD.t.i)){
    if(subject[i] == "s024"){
      if(session[i] == "4"){
        s024_session_4_DD.t.i <- append(s024_session_4_DD.t.i, DD.t.i[i])
        s024_mean_of_session_4_DD.t.i <- mean(s024_session_4_DD.t.i)
      }
    }
  }
  s024_mean_of_session_4_DD.t.i 
  
  # "s024" "session 5"
  s024_mean_of_session_5_DD.t.i <- numeric(0)
  s024_session_5_DD.t.i <- numeric(0)
  for(i in 1:length(x$DD.t.i)){
    if(subject[i] == "s024"){
      if(session[i] == "5"){
        s024_session_5_DD.t.i <- append(s024_session_5_DD.t.i, DD.t.i[i])
        s024_mean_of_session_5_DD.t.i <- mean(s024_session_5_DD.t.i)
      }
    }
  }
  s024_mean_of_session_5_DD.t.i 
  
  # "s024" "session 6"
  s024_mean_of_session_6_DD.t.i <- numeric(0)
  s024_session_6_DD.t.i <- numeric(0)
  for(i in 1:length(x$DD.t.i)){
    if(subject[i] == "s024"){
      if(session[i] == "6"){
        s024_session_6_DD.t.i <- append(s024_session_6_DD.t.i, DD.t.i[i])
        s024_mean_of_session_6_DD.t.i <- mean(s024_session_6_DD.t.i)
      }
    }
  }
  s024_mean_of_session_6_DD.t.i 
  
  # "s024" "session 7"
  s024_mean_of_session_7_DD.t.i <- numeric(0)
  s024_session_7_DD.t.i <- numeric(0)
  for(i in 1:length(x$DD.t.i)){
    if(subject[i] == "s024"){
      if(session[i] == "7"){
        s024_session_7_DD.t.i <- append(s024_session_7_DD.t.i, DD.t.i[i])
        s024_mean_of_session_7_DD.t.i <- mean(s024_session_7_DD.t.i)
      }
    }
  }
  s024_mean_of_session_7_DD.t.i 
  
  # "s024" "session 8"
  s024_mean_of_session_8_DD.t.i <- numeric(0)
  s024_session_8_DD.t.i <- numeric(0)
  for(i in 1:length(x$DD.t.i)){
    if(subject[i] == "s024"){
      if(session[i] == "8"){
        s024_session_8_DD.t.i <- append(s024_session_8_DD.t.i, DD.t.i[i])
        s024_mean_of_session_8_DD.t.i <- mean(s024_session_8_DD.t.i)
      }
    }
  }
  s024_mean_of_session_8_DD.t.i 
  
  # "s024" "all sessions"
  s024_mean_of_all_sessions_DD.t.i <- numeric(0)
  s024_all_sessions_DD.t.i <- numeric(0)
  for(i in 1:length(x$DD.t.i)){
    if(subject[i] == "s024"){
      s024_all_sessions_DD.t.i <- append(s024_all_sessions_DD.t.i, DD.t.i[i])
      s024_mean_of_all_sessions_DD.t.i <- mean(s024_all_sessions_DD.t.i)
    }
  }
  s024_mean_of_all_sessions_DD.t.i

  
  
  s024_means_combined_DD.t.i<- matrix(data=c(s024_mean_of_session_1_DD.t.i, 
                                                  s024_mean_of_session_2_DD.t.i,
                                                  s024_mean_of_session_3_DD.t.i,
                                                  s024_mean_of_session_4_DD.t.i,
                                                  s024_mean_of_session_5_DD.t.i,
                                                  s024_mean_of_session_6_DD.t.i, 
                                                  s024_mean_of_session_7_DD.t.i, 
                                                  s024_mean_of_session_8_DD.t.i))
  
  #plot to compare the different means of different sessions in H.t of s010.
  plot(s024_means_combined_DD.t.i)
  
  
  #standard deviations of the different sessions of H.t withing s010:
  sd(s024_session_1_DD_t.i)
  sd(s024_session_2_DD_t.i)
  sd(s024_session_3_DD_t.i)
  sd(s024_session_4_DD_t.i)
  sd(s024_session_5_DD_t.i)
  sd(s024_session_6_DD_t.i)
  sd(s024_session_7_DD_t.i)
  sd(s024_session_8_DD_t.i)
  
  #converting this into a matrix.
  s024_stdevs_combined_DD.period.t<- matrix(data=c( sd(s024_session_1_DD_t.i),
                                                    sd(s024_session_2_DD_t.i),
                                                    sd(s024_session_3_DD_t.i),
                                                    sd(s024_session_4_DD_t.i),
                                                    sd(s024_session_5_DD_t.i),
                                                    sd(s024_session_6_DD_t.i),
                                                    sd(s024_session_7_DD_t.i),
                                                    sd(s024_session_8_DD_t.i)))
  
  #plot to compare the standard deviations across the different sessions of H.t for s010.
  plot(s024_stdevs_combined_DD_t.i)
  
  #plots to compare the standard deviations and
  
  plot(s024_stdevs_combined_DD_t.i, s024_means_combined_DD_t.i, type="l")
  
  
  
  
  
#subject "s047", H.period variable
  # "s047" "session 1"
  s047_mean_of_session_1_H.period <- numeric(0)
  s047_session_1_H.period <- numeric(0)
  for(i in 1:length(x$H.period)){
    if(subject[i] == "s047"){
      if(session[i] == "1"){
        s047_session_1_H.period <- append(s047_session_1_H.period, H.period[i])
        s047_mean_of_session_1_H.period <- mean(s047_session_1_H.period)
      }
    }
  }
  s047_mean_of_session_1_H.period 
  
  # "s047" "session 2"
  s047_mean_of_session_2_H.period <- numeric(0)
  s047_session_2_H.period <- numeric(0)
  for(i in 1:length(x$H.period)){
    if(subject[i] == "s047"){
      if(session[i] == "2"){
        s047_session_2_H.period <- append(s047_session_2_H.period, H.period[i])
        s047_mean_of_session_2_H.period <- mean(s047_session_2_H.period)
      }
    }
  }
  s047_mean_of_session_2_H.period 
  
  # "s047" "session 3"
  s047_mean_of_session_3_H.period <- numeric(0)
  s047_session_3_H.period <- numeric(0)
  for(i in 1:length(x$H.period)){
    if(subject[i] == "s047"){
      if(session[i] == "3"){
        s047_session_3_H.period <- append(s047_session_3_H.period, H.period[i])
        s047_mean_of_session_3_H.period <- mean(s047_session_3_H.period)
      }
    }
  }
  s047_mean_of_session_3_H.period 
  
  # "s047" "session 4"
  s047_mean_of_session_4_H.period <- numeric(0)
  s047_session_4_H.period <- numeric(0)
  for(i in 1:length(x$H.period)){
    if(subject[i] == "s047"){
      if(session[i] == "4"){
        s047_session_4_H.period <- append(s047_session_4_H.period, H.period[i])
        s047_mean_of_session_4_H.period <- mean(s047_session_4_H.period)
      }
    }
  }
  s047_mean_of_session_4_H.period 
  
  # "s047" "session 5"
  s047_mean_of_session_5_H.period <- numeric(0)
  s047_session_5_H.period <- numeric(0)
  for(i in 1:length(x$H.period)){
    if(subject[i] == "s047"){
      if(session[i] == "5"){
        s047_session_5_H.period <- append(s047_session_5_H.period, H.period[i])
        s047_mean_of_session_5_H.period <- mean(s047_session_5_H.period)
      }
    }
  }
  s047_mean_of_session_5_H.period 
  
  # "s047" "session 6"
  s047_mean_of_session_6_H.period <- numeric(0)
  s047_session_6_H.period <- numeric(0)
  for(i in 1:length(x$H.period)){
    if(subject[i] == "s047"){
      if(session[i] == "6"){
        s047_session_6_H.period <- append(s047_session_6_H.period, H.period[i])
        s047_mean_of_session_6_H.period <- mean(s047_session_6_H.period)
      }
    }
  }
  s047_mean_of_session_6_H.period 
  
  # "s047" "session 7"
  s047_mean_of_session_7_H.period <- numeric(0)
  s047_session_7_H.period <- numeric(0)
  for(i in 1:length(x$H.period)){
    if(subject[i] == "s047"){
      if(session[i] == "7"){
        s047_session_7_H.period <- append(s047_session_7_H.period, H.period[i])
        s047_mean_of_session_7_H.period <- mean(s047_session_7_H.period)
      }
    }
  }
  s047_mean_of_session_7_H.period 
  
  # "s047" "session 8"
  s047_mean_of_session_8_H.period <- numeric(0)
  s047_session_8_H.period <- numeric(0)
  for(i in 1:length(x$H.period)){
    if(subject[i] == "s047"){
      if(session[i] == "8"){
        s047_session_8_H.period <- append(s047_session_8_H.period, H.period[i])
        s047_mean_of_session_8_H.period <- mean(s047_session_8_H.period)
      }
    }
  }
  s047_mean_of_session_8_H.period 
  
  # "s047" "all sessions"
  s047_mean_of_all_sessions_H.period <- numeric(0)
  s047_all_sessions_H.period <- numeric(0)
  for(i in 1:length(x$H.period)){
    if(subject[i] == "s047"){
      s047_all_sessions_H.period <- append(s047_all_sessions_H.period, H.period[i])
      s047_mean_of_all_sessions_H.period <- mean(s047_all_sessions_H.period)
    }
  }
  s047_mean_of_all_sessions_H.period
  
  
  
  
  s047_means_combined_H.period<- matrix(data=c(s047_mean_of_session_1_H.period, 
                                                  s047_mean_of_session_2_H.period,
                                                  s047_mean_of_session_3_H.period,
                                                  s047_mean_of_session_4_H.period,
                                                  s047_mean_of_session_5_H.period,
                                                  s047_mean_of_session_6_H.period, 
                                                  s047_mean_of_session_7_H.period, 
                                                  s047_mean_of_session_8_H.period))
  
  #plot to compare the different means of different sessions in H.t of s010.
  plot(s047_means_combined_H.period)
  
  
  #standard deviations of the different sessions of H.t withing s010:
  sd(s047_session_1_H.period)
  sd(s047_session_2_H.period)
  sd(s047_session_3_H.period)
  sd(s047_session_4_H.period)
  sd(s047_session_5_H.period)
  sd(s047_session_6_H.period)
  sd(s047_session_7_H.period)
  sd(s047_session_8_H.period)
  
  #converting this into a matrix.
  s047_stdevs_combined_UD.period.t<- matrix(data=c( sd(s047_session_1_H.period),
                                                    sd(s047_session_2_H.period),
                                                    sd(s047_session_3_H.period),
                                                    sd(s047_session_4_H.period),
                                                    sd(s047_session_5_H.period),
                                                    sd(s047_session_6_H.period),
                                                    sd(s047_session_7_H.period),
                                                    sd(s047_session_8_H.period)))
  
  plot(s047_stdevs_combined_H.period)
  
  #plots to compare the standard deviations and
  
  plot(s047_stdevs_combined_H.period, s047_means_combined_H.period.t, type="l")
  
  
  
  
  
  
#subject "s047", DD.period.t variable
  # "s047" "session 1"
  s047_mean_of_session_1_DD.period.t <- numeric(0)
  s047_session_1_DD.period.t <- numeric(0)
  for(i in 1:length(x$DD.period.t)){
    if(subject[i] == "s047"){
      if(session[i] == "1"){
        s047_session_1_DD.period.t <- append(s047_session_1_DD.period.t, DD.period.t[i])
        s047_mean_of_session_1_DD.period.t <- mean(s047_session_1_DD.period.t)
      }
    }
  }
  s047_mean_of_session_1_DD.period.t 
  
  # "s047" "session 2"
  s047_mean_of_session_2_DD.period.t <- numeric(0)
  s047_session_2_DD.period.t <- numeric(0)
  for(i in 1:length(x$DD.period.t)){
    if(subject[i] == "s047"){
      if(session[i] == "2"){
        s047_session_2_DD.period.t <- append(s047_session_2_DD.period.t, DD.period.t[i])
        s047_mean_of_session_2_DD.period.t <- mean(s047_session_2_DD.period.t)
      }
    }
  }
  s047_mean_of_session_2_DD.period.t 
  
  # "s047" "session 3"
  s047_mean_of_session_3_DD.period.t <- numeric(0)
  s047_session_3_DD.period.t <- numeric(0)
  for(i in 1:length(x$DD.period.t)){
    if(subject[i] == "s047"){
      if(session[i] == "3"){
        s047_session_3_DD.period.t <- append(s047_session_3_DD.period.t, DD.period.t[i])
        s047_mean_of_session_3_DD.period.t <- mean(s047_session_3_DD.period.t)
      }
    }
  }
  s047_mean_of_session_3_DD.period.t 
  
  # "s047" "session 4"
  s047_mean_of_session_4_DD.period.t <- numeric(0)
  s047_session_4_DD.period.t <- numeric(0)
  for(i in 1:length(x$DD.period.t)){
    if(subject[i] == "s047"){
      if(session[i] == "4"){
        s047_session_4_DD.period.t <- append(s047_session_4_DD.period.t, DD.period.t[i])
        s047_mean_of_session_4_DD.period.t <- mean(s047_session_4_DD.period.t)
      }
    }
  }
  s047_mean_of_session_4_DD.period.t 
  
  # "s047" "session 5"
  s047_mean_of_session_5_DD.period.t <- numeric(0)
  s047_session_5_DD.period.t <- numeric(0)
  for(i in 1:length(x$DD.period.t)){
    if(subject[i] == "s047"){
      if(session[i] == "5"){
        s047_session_5_DD.period.t <- append(s047_session_5_DD.period.t, DD.period.t[i])
        s047_mean_of_session_5_DD.period.t <- mean(s047_session_5_DD.period.t)
      }
    }
  }
  s047_mean_of_session_5_DD.period.t 
  
  # "s047" "session 6"
  s047_mean_of_session_6_DD.period.t <- numeric(0)
  s047_session_6_DD.period.t <- numeric(0)
  for(i in 1:length(x$DD.period.t)){
    if(subject[i] == "s047"){
      if(session[i] == "6"){
        s047_session_6_DD.period.t <- append(s047_session_6_DD.period.t, DD.period.t[i])
        s047_mean_of_session_6_DD.period.t <- mean(s047_session_6_DD.period.t)
      }
    }
  }
  s047_mean_of_session_6_DD.period.t 
  
  # "s047" "session 7"
  s047_mean_of_session_7_DD.period.t <- numeric(0)
  s047_session_7_DD.period.t <- numeric(0)
  for(i in 1:length(x$DD.period.t)){
    if(subject[i] == "s047"){
      if(session[i] == "7"){
        s047_session_7_DD.period.t <- append(s047_session_7_DD.period.t, DD.period.t[i])
        s047_mean_of_session_7_DD.period.t <- mean(s047_session_7_DD.period.t)
      }
    }
  }
  s047_mean_of_session_7_DD.period.t 
  
  # "s047" "session 8"
  s047_mean_of_session_8_DD.period.t <- numeric(0)
  s047_session_8_DD.period.t <- numeric(0)
  for(i in 1:length(x$DD.period.t)){
    if(subject[i] == "s047"){
      if(session[i] == "8"){
        s047_session_8_DD.period.t <- append(s047_session_8_DD.period.t, DD.period.t[i])
        s047_mean_of_session_8_DD.period.t <- mean(s047_session_8_DD.period.t)
      }
    }
  }
  s047_mean_of_session_8_DD.period.t 
  
  # "s047" "all sessions"
  s047_mean_of_all_sessions_DD.period.t <- numeric(0)
  s047_all_sessions_DD.period.t <- numeric(0)
  for(i in 1:length(x$DD.period.t)){
    if(subject[i] == "s047"){
      s047_all_sessions_DD.period.t <- append(s047_all_sessions_DD.period.t, DD.period.t[i])
      s047_mean_of_all_sessions_DD.period.t <- mean(s047_all_sessions_DD.period.t)
    }
  }
  s047_mean_of_all_sessions_DD.period.t
  
  
  
  
  s047_means_combined_DD.period.t<- matrix(data=c(s047_mean_of_session_1_DD.period.t, 
                                                  s047_mean_of_session_2_DD.period.t,
                                                  s047_mean_of_session_3_DD.period.t,
                                                  s047_mean_of_session_4_DD.period.t,
                                                  s047_mean_of_session_5_DD.period.t,
                                                  s047_mean_of_session_6_DD.period.t, 
                                                  s047_mean_of_session_7_DD.period.t, 
                                                  s047_mean_of_session_8_DD.period.t))
  
  #plot to compare the different means of different sessions in H.t of s010.
  plot(s047_means_combined_DD.period.t)
  
  
  #standard deviations of the different sessions of H.t withing s010:
  sd(s047_session_1_DD.period.t)
  sd(s047_session_2_DD.period.t)
  sd(s047_session_3_DD.period.t)
  sd(s047_session_4_DD.period.t)
  sd(s047_session_5_DD.period.t)
  sd(s047_session_6_DD.period.t)
  sd(s047_session_7_DD.period.t)
  sd(s047_session_8_DD.period.t)
  
  #converting this into a matrix.
  s047_stdevs_combined_UD.period.t<- matrix(data=c( sd(s047_session_1_DD.period.t),
                                                    sd(s047_session_2_DD.period.t),
                                                    sd(s047_session_3_DD.period.t),
                                                    sd(s047_session_4_DD.period.t),
                                                    sd(s047_session_5_DD.period.t),
                                                    sd(s047_session_6_DD.period.t),
                                                    sd(s047_session_7_DD.period.t),
                                                    sd(s047_session_8_DD.period.t)))
  
  plot(s047_stdevs_combined_DD.period.t)
  
  #plots to compare the standard deviations and
  
  plot(s047_stdevs_combined_DD.period.t, s047_means_combined_DD.period.t, type="l")
  
  
  
#subject "s047", UD.period.t variable
  # "s047" "session 1"
  s047_mean_of_session_1_UD.period.t <- numeric(0)
  s047_session_1_UD.period.t <- numeric(0)
  for(i in 1:length(x$UD.period.t)){
    if(subject[i] == "s047"){
      if(session[i] == "1"){
        s047_session_1_UD.period.t <- append(s047_session_1_UD.period.t, UD.period.t[i])
        s047_mean_of_session_1_UD.period.t <- mean(s047_session_1_UD.period.t)
      }
    }
  }
  s047_mean_of_session_1_UD.period.t
  
  # "s047" "session 2"
  s047_mean_of_session_2_UD.period.t <- numeric(0)
  s047_session_2_UD.period.t <- numeric(0)
  for(i in 1:length(x$UD.period.t)){
    if(subject[i] == "s047"){
      if(session[i] == "2"){
        s047_session_2_UD.period.t <- append(s047_session_2_UD.period.t, UD.period.t[i])
        s047_mean_of_session_2_UD.period.t <- mean(s047_session_2_UD.period.t)
      }
    }
  }
  s047_mean_of_session_2_UD.period.t
  
  # "s047" "session 3"
  s047_mean_of_session_3_UD.period.t <- numeric(0)
  s047_session_3_UD.period.t <- numeric(0)
  for(i in 1:length(x$UD.period.t)){
    if(subject[i] == "s047"){
      if(session[i] == "3"){
        s047_session_3_UD.period.t <- append(s047_session_3_UD.period.t, UD.period.t[i])
        s047_mean_of_session_3_UD.period.t <- mean(s047_session_3_UD.period.t)
      }
    }
  }
  s047_mean_of_session_3_UD.period.t
  
  # "s047" "session 4"
  s047_mean_of_session_4_UD.period.t <- numeric(0)
  s047_session_4_UD.period.t <- numeric(0)
  for(i in 1:length(x$UD.period.t)){
    if(subject[i] == "s047"){
      if(session[i] == "4"){
        s047_session_4_UD.period.t <- append(s047_session_4_UD.period.t, UD.period.t[i])
        s047_mean_of_session_4_UD.period.t <- mean(s047_session_4_UD.period.t)
      }
    }
  }
  s047_mean_of_session_4_UD.period.t
  
  # "s047" "session 5"
  s047_mean_of_session_5_UD.period.t <- numeric(0)
  s047_session_5_UD.period.t <- numeric(0)
  for(i in 1:length(x$UD.period.t)){
    if(subject[i] == "s047"){
      if(session[i] == "5"){
        s047_session_5_UD.period.t <- append(s047_session_5_UD.period.t, UD.period.t[i])
        s047_mean_of_session_5_UD.period.t <- mean(s047_session_5_UD.period.t)
      }
    }
  }
  s047_mean_of_session_5_UD.period.t
  
  # "s047" "session 6"
  s047_mean_of_session_6_UD.period.t <- numeric(0)
  s047_session_6_UD.period.t <- numeric(0)
  for(i in 1:length(x$UD.period.t)){
    if(subject[i] == "s047"){
      if(session[i] == "6"){
        s047_session_6_UD.period.t <- append(s047_session_6_UD.period.t, UD.period.t[i])
        s047_mean_of_session_6_UD.period.t <- mean(s047_session_6_UD.period.t)
      }
    }
  }
  s047_mean_of_session_6_UD.period.t
  
  # "s047" "session 7"
  s047_mean_of_session_7_UD.period.t <- numeric(0)
  s047_session_7_UD.period.t <- numeric(0)
  for(i in 1:length(x$UD.period.t)){
    if(subject[i] == "s047"){
      if(session[i] == "7"){
        s047_session_7_UD.period.t <- append(s047_session_7_UD.period.t, UD.period.t[i])
        s047_mean_of_session_7_UD.period.t <- mean(s047_session_7_UD.period.t)
      }
    }
  }
  s047_mean_of_session_7_UD.period.t
  
  # "s047" "session 8"
  s047_mean_of_session_8_UD.period.t <- numeric(0)
  s047_session_8_UD.period.t <- numeric(0)
  for(i in 1:length(x$UD.period.t)){
    if(subject[i] == "s047"){
      if(session[i] == "8"){
        s047_session_8_UD.period.t <- append(s047_session_8_UD.period.t, UD.period.t[i])
        s047_mean_of_session_8_UD.period.t <- mean(s047_session_8_UD.period.t)
      }
    }
  }
  s047_mean_of_session_8_UD.period.t
  
  # "s047" "all sessions"
  s047_mean_of_all_sessions_UD.period.t <- numeric(0)
  s047_all_sessions_UD.period.t <- numeric(0)
  for(i in 1:length(x$UD.period.t)){
    if(subject[i] == "s047"){
      s047_all_sessions_UD.period.t <- append(s047_all_sessions_UD.period.t, UD.period.t[i])
      s047_mean_of_all_sessions_UD.period.t <- mean(s047_all_sessions_UD.period.t)
    }
  }
  s047_mean_of_all_sessions_UD.period.t
  
  
  s047_means_combined_UD.period.t<- matrix(data=c(s047_mean_of_session_1_UD.period.t, 
                                             s047_mean_of_session_2_UD.period.t,
                                             s047_mean_of_session_3_UD.period.t,
                                             s047_mean_of_session_4_UD.period.t,
                                             s047_mean_of_session_5_UD.period.t,
                                             s047_mean_of_session_6_UD.period.t, 
                                             s047_mean_of_session_7_UD.period.t, 
                                             s047_mean_of_session_8_UD.period.t))
  
  #plot to compare the different means of different sessions in H.t of s010.
  plot(s047_means_combined_UD.period.t)
  
  
  #standard deviations of the different sessions of H.t withing s010:
  sd(s047_session_1_UD.period.t)
  sd(s047_session_2_UD.period.t)
  sd(s047_session_3_UD.period.t)
  sd(s047_session_4_UD.period.t)
  sd(s047_session_5_UD.period.t)
  sd(s047_session_6_UD.period.t)
  sd(s047_session_7_UD.period.t)
  sd(s047_session_8_UD.period.t)
  
  #converting this into a matrix.
  s047_stdevs_combined_UD.period.t<- matrix(data=c( sd(s047_session_1_UD.period.t),
                                                    sd(s047_session_2_UD.period.t),
                                                    sd(s047_session_3_UD.period.t),
                                                    sd(s047_session_4_UD.period.t),
                                                    sd(s047_session_5_UD.period.t),
                                                    sd(s047_session_6_UD.period.t),
                                                    sd(s047_session_7_UD.period.t),
                                                    sd(s047_session_8_UD.period.t)))
  
  #plot to compare the standard deviations across the different sessions of H.t for s010.
  plot(s047_stdevs_combined_UD.period.t)
  
  #plots to compare the standard deviations and
  
  plot(s047_stdevs_combined_UD.period.t, s047_means_combined_UD.period.t, type="l")
  
  
  
  
  
  
  
#subject "s047", H.t variable
  # "s047" "session 1"
  s047_mean_of_session_1_H.t <- numeric(0)
  s047_session_1_H.t <- numeric(0)
  for(i in 1:length(x$H.t)){
    if(subject[i] == "s047"){
      if(session[i] == "1"){
        s047_session_1_H.t <- append(s047_session_1_H.t, H.t[i])
        s047_mean_of_session_1_H.t <- mean(s047_session_1_H.t)
      }
    }
  }
  s047_mean_of_session_1_H.t 
  
  # "s047" "session 2"
  s047_mean_of_session_2_H.t <- numeric(0)
  s047_session_2_H.t <- numeric(0)
  for(i in 1:length(x$H.t)){
    if(subject[i] == "s047"){
      if(session[i] == "2"){
        s047_session_2_H.t <- append(s047_session_2_H.t, H.t[i])
        s047_mean_of_session_2_H.t <- mean(s047_session_2_H.t)
      }
    }
  }
  s047_mean_of_session_2_H.t 
  
  # "s047" "session 3"
  s047_mean_of_session_3_H.t <- numeric(0)
  s047_session_3_H.t <- numeric(0)
  for(i in 1:length(x$H.t)){
    if(subject[i] == "s047"){
      if(session[i] == "3"){
        s047_session_3_H.t <- append(s047_session_3_H.t, H.t[i])
        s047_mean_of_session_3_H.t <- mean(s047_session_3_H.t)
      }
    }
  }
  s047_mean_of_session_3_H.t 
  
  # "s047" "session 4"
  s047_mean_of_session_4_H.t <- numeric(0)
  s047_session_4_H.t <- numeric(0)
  for(i in 1:length(x$H.t)){
    if(subject[i] == "s047"){
      if(session[i] == "4"){
        s047_session_4_H.t <- append(s047_session_4_H.t, H.t[i])
        s047_mean_of_session_4_H.t <- mean(s047_session_4_H.t)
      }
    }
  }
  s047_mean_of_session_4_H.t 
  
  # "s047" "session 5"
  s047_mean_of_session_5_H.t <- numeric(0)
  s047_session_5_H.t <- numeric(0)
  for(i in 1:length(x$H.t)){
    if(subject[i] == "s047"){
      if(session[i] == "5"){
        s047_session_5_H.t <- append(s047_session_5_H.t, H.t[i])
        s047_mean_of_session_5_H.t <- mean(s047_session_5_H.t)
      }
    }
  }
  s047_mean_of_session_5_H.t 
  
  # "s047" "session 6"
  s047_mean_of_session_6_H.t <- numeric(0)
  s047_session_6_H.t <- numeric(0)
  for(i in 1:length(x$H.t)){
    if(subject[i] == "s047"){
      if(session[i] == "6"){
        s047_session_6_H.t <- append(s047_session_6_H.t, H.t[i])
        s047_mean_of_session_6_H.t <- mean(s047_session_6_H.t)
      }
    }
  }
  s047_mean_of_session_6_H.t 
  
  # "s047" "session 7"
  s047_mean_of_session_7_H.t <- numeric(0)
  s047_session_7_H.t <- numeric(0)
  for(i in 1:length(x$H.t)){
    if(subject[i] == "s047"){
      if(session[i] == "7"){
        s047_session_7_H.t <- append(s047_session_7_H.t, H.t[i])
        s047_mean_of_session_7_H.t <- mean(s047_session_7_H.t)
      }
    }
  }
  s047_mean_of_session_7_H.t 
  
  # "s047" "session 8"
  s047_mean_of_session_8_H.t <- numeric(0)
  s047_session_8_H.t <- numeric(0)
  for(i in 1:length(x$H.t)){
    if(subject[i] == "s047"){
      if(session[i] == "8"){
        s047_session_8_H.t <- append(s047_session_8_H.t, H.t[i])
        s047_mean_of_session_8_H.t <- mean(s047_session_8_H.t)
      }
    }
  }
  s047_mean_of_session_8_H.t 
  
  # "s047" "all sessions"
  s047_mean_of_all_sessions_H.t <- numeric(0)
  s047_all_sessions_H.t <- numeric(0)
  for(i in 1:length(x$H.t)){
    if(subject[i] == "s047"){
      s047_all_sessions_H.t <- append(s047_all_sessions_H.t, H.t[i])
      s047_mean_of_all_sessions_H.t <- mean(s047_all_sessions_H.t)
    }
  }
  s047_mean_of_all_sessions_H.t

  
  
  s047_means_combined_H.t<- matrix(data=c(s047_mean_of_session_1_H.t, 
                                             s047_mean_of_session_2_H.t,
                                             s047_mean_of_session_3_H.t,
                                             s047_mean_of_session_4_H.t,
                                             s047_mean_of_session_5_H.t,
                                             s047_mean_of_session_6_H.t, 
                                             s047_mean_of_session_7_H.t, 
                                             s047_mean_of_session_8_H.t))
  
  #plot to compare the different means of different sessions in H.t of s010.
  plot(s047_means_combined_H.t)
  
  
  #standard deviations of the different sessions of H.t withing s010:
  sd(s047_session_1_H.t)
  sd(s047_session_2_H.t)
  sd(s047_session_3_H.t)
  sd(s047_session_4_H.t)
  sd(s047_session_5_H.t)
  sd(s047_session_6_H.t)
  sd(s047_session_7_H.t)
  sd(s047_session_8_H.t)
  
  #converting this into a matrix.
  s047_stdevs_combined_DD.period.t<- matrix(data=c( sd(s047_session_1_H.t),
                                                    sd(s047_session_2_H.t),
                                                    sd(s047_session_3_H.t),
                                                    sd(s047_session_4_H.t),
                                                    sd(s047_session_5_H.t),
                                                    sd(s047_session_6_H.t),
                                                    sd(s047_session_7_H.t),
                                                    sd(s047_session_8_H.t)))
  
  #plot to compare the standard deviations across the different sessions of H.t for s010.
  plot(s047_stdevs_combined_H.t)
  
  #plots to compare the standard deviations and
  
  plot(s047_stdevs_combined_H.t, s047_means_combined_H.t, type="l")
  
  
  
  
  
#subject "s047", DD.t.i variable
  # "s047" "session 1"
  s047_mean_of_session_1_DD.t.i <- numeric(0)
  s047_session_1_DD.t.i <- numeric(0)
  for(i in 1:length(x$DD.t.i)){
    if(subject[i] == "s047"){
      if(session[i] == "1"){
        s047_session_1_DD.t.i <- append(s047_session_1_DD.t.i, DD.t.i[i])
        s047_mean_of_session_1_DD.t.i <- mean(s047_session_1_DD.t.i)
      }
    }
  }
  s047_mean_of_session_1_DD.t.i 
  
  # "s047" "session 2"
  s047_mean_of_session_2_DD.t.i <- numeric(0)
  s047_session_2_DD.t.i <- numeric(0)
  for(i in 1:length(x$DD.t.i)){
    if(subject[i] == "s047"){
      if(session[i] == "2"){
        s047_session_2_DD.t.i <- append(s047_session_2_DD.t.i, DD.t.i[i])
        s047_mean_of_session_2_DD.t.i <- mean(s047_session_2_DD.t.i)
      }
    }
  }
  s047_mean_of_session_2_DD.t.i 
  
  # "s047" "session 3"
  s047_mean_of_session_3_DD.t.i <- numeric(0)
  s047_session_3_DD.t.i <- numeric(0)
  for(i in 1:length(x$DD.t.i)){
    if(subject[i] == "s047"){
      if(session[i] == "3"){
        s047_session_3_DD.t.i <- append(s047_session_3_DD.t.i, DD.t.i[i])
        s047_mean_of_session_3_DD.t.i <- mean(s047_session_3_DD.t.i)
      }
    }
  }
  s047_mean_of_session_3_DD.t.i 
  
  # "s047" "session 4"
  s047_mean_of_session_4_DD.t.i <- numeric(0)
  s047_session_4_DD.t.i <- numeric(0)
  for(i in 1:length(x$DD.t.i)){
    if(subject[i] == "s047"){
      if(session[i] == "4"){
        s047_session_4_DD.t.i <- append(s047_session_4_DD.t.i, DD.t.i[i])
        s047_mean_of_session_4_DD.t.i <- mean(s047_session_4_DD.t.i)
      }
    }
  }
  s047_mean_of_session_4_DD.t.i 
  
  # "s047" "session 5"
  s047_mean_of_session_5_DD.t.i <- numeric(0)
  s047_session_5_DD.t.i <- numeric(0)
  for(i in 1:length(x$DD.t.i)){
    if(subject[i] == "s047"){
      if(session[i] == "5"){
        s047_session_5_DD.t.i <- append(s047_session_5_DD.t.i, DD.t.i[i])
        s047_mean_of_session_5_DD.t.i <- mean(s047_session_5_DD.t.i)
      }
    }
  }
  s047_mean_of_session_5_DD.t.i 
  
  # "s047" "session 6"
  s047_mean_of_session_6_DD.t.i <- numeric(0)
  s047_session_6_DD.t.i <- numeric(0)
  for(i in 1:length(x$DD.t.i)){
    if(subject[i] == "s047"){
      if(session[i] == "6"){
        s047_session_6_DD.t.i <- append(s047_session_6_DD.t.i, DD.t.i[i])
        s047_mean_of_session_6_DD.t.i <- mean(s047_session_6_DD.t.i)
      }
    }
  }
  s047_mean_of_session_6_DD.t.i 
  
  # "s047" "session 7"
  s047_mean_of_session_7_DD.t.i <- numeric(0)
  s047_session_7_DD.t.i <- numeric(0)
  for(i in 1:length(x$DD.t.i)){
    if(subject[i] == "s047"){
      if(session[i] == "7"){
        s047_session_7_DD.t.i <- append(s047_session_7_DD.t.i, DD.t.i[i])
        s047_mean_of_session_7_DD.t.i <- mean(s047_session_7_DD.t.i)
      }
    }
  }
  s047_mean_of_session_7_DD.t.i 
  
  # "s047" "session 8"
  s047_mean_of_session_8_DD.t.i <- numeric(0)
  s047_session_8_DD.t.i <- numeric(0)
  for(i in 1:length(x$DD.t.i)){
    if(subject[i] == "s047"){
      if(session[i] == "8"){
        s047_session_8_DD.t.i <- append(s047_session_8_DD.t.i, DD.t.i[i])
        s047_mean_of_session_8_DD.t.i <- mean(s047_session_8_DD.t.i)
      }
    }
  }
  s047_mean_of_session_8_DD.t.i 
  
  # "s047" "all sessions"
  s047_mean_of_all_sessions_DD.t.i <- numeric(0)
  s047_all_sessions_DD.t.i <- numeric(0)
  for(i in 1:length(x$DD.t.i)){
    if(subject[i] == "s047"){
      s047_all_sessions_DD.t.i <- append(s047_all_sessions_DD.t.i, DD.t.i[i])
      s047_mean_of_all_sessions_DD.t.i <- mean(s047_all_sessions_DD.t.i)
    }
  }
  s047_mean_of_all_sessions_DD.t.i
  
  
  s047_means_combined_DD.t.i<- matrix(data=c(s047_mean_of_session_1_DD.t.i, 
                                             s047_mean_of_session_2_DD.t.i,
                                             s047_mean_of_session_3_DD.t.i,
                                             s047_mean_of_session_4_DD.t.i,
                                             s047_mean_of_session_5_DD.t.i,
                                             s047_mean_of_session_6_DD.t.i, 
                                             s047_mean_of_session_7_DD.t.i, 
                                             s047_mean_of_session_8_DD.t.i))
  
  #plot to compare the different means of different sessions in H.t of s010.
  plot(s047_means_combined_DD.t.i)
  
  
  #standard deviations of the different sessions of H.t withing s010:
  sd(s047_session_1_DD_t.i)
  sd(s047_session_2_DD_t.i)
  sd(s047_session_3_DD_t.i)
  sd(s047_session_4_DD_t.i)
  sd(s047_session_5_DD_t.i)
  sd(s047_session_6_DD_t.i)
  sd(s047_session_7_DD_t.i)
  sd(s047_session_8_DD_t.i)
  
  #converting this into a matrix.
  s047_stdevs_combined_DD.period.t<- matrix(data=c( sd(s047_session_1_DD_t.i),
                                                    sd(s047_session_2_DD_t.i),
                                                    sd(s047_session_3_DD_t.i),
                                                    sd(s047_session_4_DD_t.i),
                                                    sd(s047_session_5_DD_t.i),
                                                    sd(s047_session_6_DD_t.i),
                                                    sd(s047_session_7_DD_t.i),
                                                    sd(s047_session_8_DD_t.i)))
  
  #plot to compare the standard deviations across the different sessions of H.t for s010.
  plot(s047_stdevs_combined_DD_t.i)
  
  #plots to compare the standard deviations and
  
  plot(s047_stdevs_combined_DD_t.i, s047_means_combined_DD_t.i, type="l")
  
  
  
  
  
  
#subject "s053", H.period variable
  # "s053" "session 1"
  s053_mean_of_session_1_H.period <- numeric(0)
  s053_session_1_H.period <- numeric(0)
  for(i in 1:length(x$H.period)){
    if(subject[i] == "s053"){
      if(session[i] == "1"){
        s053_session_1_H.period <- append(s053_session_1_H.period, H.period[i])
        s053_mean_of_session_1_H.period <- mean(s053_session_1_H.period)
      }
    }
  }
  s053_mean_of_session_1_H.period 
  
  # "s053" "session 2"
  s053_mean_of_session_2_H.period <- numeric(0)
  s053_session_2_H.period <- numeric(0)
  for(i in 1:length(x$H.period)){
    if(subject[i] == "s053"){
      if(session[i] == "2"){
        s053_session_2_H.period <- append(s053_session_2_H.period, H.period[i])
        s053_mean_of_session_2_H.period <- mean(s053_session_2_H.period)
      }
    }
  }
  s053_mean_of_session_2_H.period 
  
  # "s053" "session 3"
  s053_mean_of_session_3_H.period <- numeric(0)
  s053_session_3_H.period <- numeric(0)
  for(i in 1:length(x$H.period)){
    if(subject[i] == "s053"){
      if(session[i] == "3"){
        s053_session_3_H.period <- append(s053_session_3_H.period, H.period[i])
        s053_mean_of_session_3_H.period <- mean(s053_session_3_H.period)
      }
    }
  }
  s053_mean_of_session_3_H.period 
  
  # "s053" "session 4"
  s053_mean_of_session_4_H.period <- numeric(0)
  s053_session_4_H.period <- numeric(0)
  for(i in 1:length(x$H.period)){
    if(subject[i] == "s053"){
      if(session[i] == "4"){
        s053_session_4_H.period <- append(s053_session_4_H.period, H.period[i])
        s053_mean_of_session_4_H.period <- mean(s053_session_4_H.period)
      }
    }
  }
  s053_mean_of_session_4_H.period 
  
  # "s053" "session 5"
  s053_mean_of_session_5_H.period <- numeric(0)
  s053_session_5_H.period <- numeric(0)
  for(i in 1:length(x$H.period)){
    if(subject[i] == "s053"){
      if(session[i] == "5"){
        s053_session_5_H.period <- append(s053_session_5_H.period, H.period[i])
        s053_mean_of_session_5_H.period <- mean(s053_session_5_H.period)
      }
    }
  }
  s053_mean_of_session_5_H.period 
  
  # "s053" "session 6"
  s053_mean_of_session_6_H.period <- numeric(0)
  s053_session_6_H.period <- numeric(0)
  for(i in 1:length(x$H.period)){
    if(subject[i] == "s053"){
      if(session[i] == "6"){
        s053_session_6_H.period <- append(s053_session_6_H.period, H.period[i])
        s053_mean_of_session_6_H.period <- mean(s053_session_6_H.period)
      }
    }
  }
  s053_mean_of_session_6_H.period 
  
  # "s053" "session 7"
  s053_mean_of_session_7_H.period <- numeric(0)
  s053_session_7_H.period <- numeric(0)
  for(i in 1:length(x$H.period)){
    if(subject[i] == "s053"){
      if(session[i] == "7"){
        s053_session_7_H.period <- append(s053_session_7_H.period, H.period[i])
        s053_mean_of_session_7_H.period <- mean(s053_session_7_H.period)
      }
    }
  }
  s053_mean_of_session_7_H.period 
  
  # "s053" "session 8"
  s053_mean_of_session_8_H.period <- numeric(0)
  s053_session_8_H.period <- numeric(0)
  for(i in 1:length(x$H.period)){
    if(subject[i] == "s053"){
      if(session[i] == "8"){
        s053_session_8_H.period <- append(s053_session_8_H.period, H.period[i])
        s053_mean_of_session_8_H.period <- mean(s053_session_8_H.period)
      }
    }
  }
  s053_mean_of_session_8_H.period 
  
  # "s053" "all sessions"
  s053_mean_of_all_sessions_H.period <- numeric(0)
  s053_all_sessions_H.period <- numeric(0)
  for(i in 1:length(x$H.period)){
    if(subject[i] == "s053"){
      s053_all_sessions_H.period <- append(s053_all_sessions_H.period, H.period[i])
      s053_mean_of_all_sessions_H.period <- mean(s053_all_sessions_H.period)
    }
  }
  s053_mean_of_all_sessions_H.period
  
  
  s053_means_combined_H.period<- matrix(data=c(s053_mean_of_session_1_H.period, 
                                               s053_mean_of_session_2_H.period,
                                               s053_mean_of_session_3_H.period,
                                               s053_mean_of_session_4_H.period,
                                               s053_mean_of_session_5_H.period,
                                               s053_mean_of_session_6_H.period, 
                                               s053_mean_of_session_7_H.period, 
                                               s053_mean_of_session_8_H.period))
  
  #plot to compare the different means of different sessions in H.t of s010.
  plot(s053_means_combined_H.period)
  
  
  #standard deviations of the different sessions of H.t withing s010:
  sd(s053_session_1_H.period)
  sd(s053_session_2_H.period)
  sd(s053_session_3_H.period)
  sd(s053_session_4_H.period)
  sd(s053_session_5_H.period)
  sd(s053_session_6_H.period)
  sd(s053_session_7_H.period)
  sd(s053_session_8_H.period)
  
  #converting this into a matrix.
  s053_stdevs_combined_UD.period.t<- matrix(data=c( sd(s053_session_1_H.period),
                                                    sd(s053_session_2_H.period),
                                                    sd(s053_session_3_H.period),
                                                    sd(s053_session_4_H.period),
                                                    sd(s053_session_5_H.period),
                                                    sd(s053_session_6_H.period),
                                                    sd(s053_session_7_H.period),
                                                    sd(s053_session_8_H.period)))
  
  plot(s053_stdevs_combined_H.period)
  
  #plots to compare the standard deviations and
  
  plot(s053_stdevs_combined_H.period, s047_means_combined_H.period.t, type="l")
  
  
  
  
  
  
  
#subject "s053", DD.period.t variable
  # "s053" "session 1"
  s053_mean_of_session_1_DD.period.t <- numeric(0)
  s053_session_1_DD.period.t <- numeric(0)
  for(i in 1:length(x$DD.period.t)){
    if(subject[i] == "s053"){
      if(session[i] == "1"){
        s053_session_1_DD.period.t <- append(s053_session_1_DD.period.t, DD.period.t[i])
        s053_mean_of_session_1_DD.period.t <- mean(s053_session_1_DD.period.t)
      }
    }
  }
  s053_mean_of_session_1_DD.period.t 
  
  # "s053" "session 2"
  s053_mean_of_session_2_DD.period.t <- numeric(0)
  s053_session_2_DD.period.t <- numeric(0)
  for(i in 1:length(x$DD.period.t)){
    if(subject[i] == "s053"){
      if(session[i] == "2"){
        s053_session_2_DD.period.t <- append(s053_session_2_DD.period.t, DD.period.t[i])
        s053_mean_of_session_2_DD.period.t <- mean(s053_session_2_DD.period.t)
      }
    }
  }
  s053_mean_of_session_2_DD.period.t 
  
  # "s053" "session 3"
  s053_mean_of_session_3_DD.period.t <- numeric(0)
  s053_session_3_DD.period.t <- numeric(0)
  for(i in 1:length(x$DD.period.t)){
    if(subject[i] == "s053"){
      if(session[i] == "3"){
        s053_session_3_DD.period.t <- append(s053_session_3_DD.period.t, DD.period.t[i])
        s053_mean_of_session_3_DD.period.t <- mean(s053_session_3_DD.period.t)
      }
    }
  }
  s053_mean_of_session_3_DD.period.t 
  
  # "s053" "session 4"
  s053_mean_of_session_4_DD.period.t <- numeric(0)
  s053_session_4_DD.period.t <- numeric(0)
  for(i in 1:length(x$DD.period.t)){
    if(subject[i] == "s053"){
      if(session[i] == "4"){
        s053_session_4_DD.period.t <- append(s053_session_4_DD.period.t, DD.period.t[i])
        s053_mean_of_session_4_DD.period.t <- mean(s053_session_4_DD.period.t)
      }
    }
  }
  s053_mean_of_session_4_DD.period.t 
  
  # "s053" "session 5"
  s053_mean_of_session_5_DD.period.t <- numeric(0)
  s053_session_5_DD.period.t <- numeric(0)
  for(i in 1:length(x$DD.period.t)){
    if(subject[i] == "s053"){
      if(session[i] == "5"){
        s053_session_5_DD.period.t <- append(s053_session_5_DD.period.t, DD.period.t[i])
        s053_mean_of_session_5_DD.period.t <- mean(s053_session_5_DD.period.t)
      }
    }
  }
  s053_mean_of_session_5_DD.period.t 
  
  # "s053" "session 6"
  s053_mean_of_session_6_DD.period.t <- numeric(0)
  s053_session_6_DD.period.t <- numeric(0)
  for(i in 1:length(x$DD.period.t)){
    if(subject[i] == "s053"){
      if(session[i] == "6"){
        s053_session_6_DD.period.t <- append(s053_session_6_DD.period.t, DD.period.t[i])
        s053_mean_of_session_6_DD.period.t <- mean(s053_session_6_DD.period.t)
      }
    }
  }
  s053_mean_of_session_6_DD.period.t 
  
  # "s053" "session 7"
  s053_mean_of_session_7_DD.period.t <- numeric(0)
  s053_session_7_DD.period.t <- numeric(0)
  for(i in 1:length(x$DD.period.t)){
    if(subject[i] == "s053"){
      if(session[i] == "7"){
        s053_session_7_DD.period.t <- append(s053_session_7_DD.period.t, DD.period.t[i])
        s053_mean_of_session_7_DD.period.t <- mean(s053_session_7_DD.period.t)
      }
    }
  }
  s053_mean_of_session_7_DD.period.t 
  
  # "s053" "session 8"
  s053_mean_of_session_8_DD.period.t <- numeric(0)
  s053_session_8_DD.period.t <- numeric(0)
  for(i in 1:length(x$DD.period.t)){
    if(subject[i] == "s053"){
      if(session[i] == "8"){
        s053_session_8_DD.period.t <- append(s053_session_8_DD.period.t, DD.period.t[i])
        s053_mean_of_session_8_DD.period.t <- mean(s053_session_8_DD.period.t)
      }
    }
  }
  s053_mean_of_session_8_DD.period.t 
  
  # "s053" "all sessions"
  s053_mean_of_all_sessions_DD.period.t <- numeric(0)
  s053_all_sessions_DD.period.t <- numeric(0)
  for(i in 1:length(x$DD.period.t)){
    if(subject[i] == "s053"){
      s053_all_sessions_DD.period.t <- append(s053_all_sessions_DD.period.t, DD.period.t[i])
      s053_mean_of_all_sessions_DD.period.t <- mean(s053_all_sessions_DD.period.t)
    }
  }
  s053_mean_of_all_sessions_DD.period.t
  
  
  
  
  s053_means_combined_DD.period.t<- matrix(data=c(s053_mean_of_session_1_DD.period.t, 
                                               s053_mean_of_session_2_DD.period.t,
                                               s053_mean_of_session_3_DD.period.t,
                                               s053_mean_of_session_4_DD.period.t,
                                               s053_mean_of_session_5_DD.period.t,
                                               s053_mean_of_session_6_DD.period.t, 
                                               s053_mean_of_session_7_DD.period.t, 
                                               s053_mean_of_session_8_DD.period.t))
  
  #plot to compare the different means of different sessions in H.t of s010.
  plot(s053_means_combined_DD.period.t)
  
  
  #standard deviations of the different sessions of H.t withing s010:
  sd(s053_session_1_DD.period.t)
  sd(s053_session_2_DD.period.t)
  sd(s053_session_3_DD.period.t)
  sd(s053_session_4_DD.period.t)
  sd(s053_session_5_DD.period.t)
  sd(s053_session_6_DD.period.t)
  sd(s053_session_7_DD.period.t)
  sd(s053_session_8_DD.period.t)
  
  #converting this into a matrix.
  s053_stdevs_combined_DD.period.t<- matrix(data=c( sd(s053_session_1_DD.period.td),
                                                    sd(s053_session_2_DD.period.t),
                                                    sd(s053_session_3_DD.period.t),
                                                    sd(s053_session_4_DD.period.t),
                                                    sd(s053_session_5_DD.period.t),
                                                    sd(s053_session_6_DD.period.t),
                                                    sd(s053_session_7_DD.period.t),
                                                    sd(s053_session_8_DD.period.t)))
  
  plot(s053_stdevs_combined_DD.period.t)
  
  #plots to compare the standard deviations and
  
  plot(s053_stdevs_combined_DD.period.t, s047_means_combined_DD.period.t, type="l")
  
  
  
#subject "s053", UD.period.t variable
  # "s053" "session 1"
  s053_mean_of_session_1_UD.period.t <- numeric(0)
  s053_session_1_UD.period.t <- numeric(0)
  for(i in 1:length(x$UD.period.t)){
    if(subject[i] == "s053"){
      if(session[i] == "1"){
        s053_session_1_UD.period.t <- append(s053_session_1_UD.period.t, UD.period.t[i])
        s053_mean_of_session_1_UD.period.t <- mean(s053_session_1_UD.period.t)
      }
    }
  }
  s053_mean_of_session_1_UD.period.t
  
  # "s053" "session 2"
  s053_mean_of_session_2_UD.period.t <- numeric(0)
  s053_session_2_UD.period.t <- numeric(0)
  for(i in 1:length(x$UD.period.t)){
    if(subject[i] == "s053"){
      if(session[i] == "2"){
        s053_session_2_UD.period.t <- append(s053_session_2_UD.period.t, UD.period.t[i])
        s053_mean_of_session_2_UD.period.t <- mean(s053_session_2_UD.period.t)
      }
    }
  }
  s053_mean_of_session_2_UD.period.t
  
  # "s053" "session 3"
  s053_mean_of_session_3_UD.period.t <- numeric(0)
  s053_session_3_UD.period.t <- numeric(0)
  for(i in 1:length(x$UD.period.t)){
    if(subject[i] == "s053"){
      if(session[i] == "3"){
        s053_session_3_UD.period.t <- append(s053_session_3_UD.period.t, UD.period.t[i])
        s053_mean_of_session_3_UD.period.t <- mean(s053_session_3_UD.period.t)
      }
    }
  }
  s053_mean_of_session_3_UD.period.t
  
  # "s053" "session 4"
  s053_mean_of_session_4_UD.period.t <- numeric(0)
  s053_session_4_UD.period.t <- numeric(0)
  for(i in 1:length(x$UD.period.t)){
    if(subject[i] == "s053"){
      if(session[i] == "4"){
        s053_session_4_UD.period.t <- append(s053_session_4_UD.period.t, UD.period.t[i])
        s053_mean_of_session_4_UD.period.t <- mean(s053_session_4_UD.period.t)
      }
    }
  }
  s053_mean_of_session_4_UD.period.t
  
  # "s053" "session 5"
  s053_mean_of_session_5_UD.period.t <- numeric(0)
  s053_session_5_UD.period.t <- numeric(0)
  for(i in 1:length(x$UD.period.t)){
    if(subject[i] == "s053"){
      if(session[i] == "5"){
        s053_session_5_UD.period.t <- append(s053_session_5_UD.period.t, UD.period.t[i])
        s053_mean_of_session_5_UD.period.t <- mean(s053_session_5_UD.period.t)
      }
    }
  }
  s053_mean_of_session_5_UD.period.t
  
  # "s053" "session 6"
  s053_mean_of_session_6_UD.period.t <- numeric(0)
  s053_session_6_UD.period.t <- numeric(0)
  for(i in 1:length(x$UD.period.t)){
    if(subject[i] == "s053"){
      if(session[i] == "6"){
        s053_session_6_UD.period.t <- append(s053_session_6_UD.period.t, UD.period.t[i])
        s053_mean_of_session_6_UD.period.t <- mean(s053_session_6_UD.period.t)
      }
    }
  }
  s053_mean_of_session_6_UD.period.t
  
  # "s053" "session 7"
  s053_mean_of_session_7_UD.period.t <- numeric(0)
  s053_session_7_UD.period.t <- numeric(0)
  for(i in 1:length(x$UD.period.t)){
    if(subject[i] == "s053"){
      if(session[i] == "7"){
        s053_session_7_UD.period.t <- append(s053_session_7_UD.period.t, UD.period.t[i])
        s053_mean_of_session_7_UD.period.t <- mean(s053_session_7_UD.period.t)
      }
    }
  }
  s053_mean_of_session_7_UD.period.t
  
  # "s053" "session 8"
  s053_mean_of_session_8_UD.period.t <- numeric(0)
  s053_session_8_UD.period.t <- numeric(0)
  for(i in 1:length(x$UD.period.t)){
    if(subject[i] == "s053"){
      if(session[i] == "8"){
        s053_session_8_UD.period.t <- append(s053_session_8_UD.period.t, UD.period.t[i])
        s053_mean_of_session_8_UD.period.t <- mean(s053_session_8_UD.period.t)
      }
    }
  }
  s053_mean_of_session_8_UD.period.t
  
  # "s053" "all sessions"
  s053_mean_of_all_sessions_UD.period.t <- numeric(0)
  s053_all_sessions_UD.period.t <- numeric(0)
  for(i in 1:length(x$UD.period.t)){
    if(subject[i] == "s053"){
      s053_all_sessions_UD.period.t <- append(s053_all_sessions_UD.period.t, UD.period.t[i])
      s053_mean_of_all_sessions_UD.period.t <- mean(s053_all_sessions_UD.period.t)
    }
  }
  s053_mean_of_all_sessions_UD.period.t
  
  
  
  
  s053_means_combined_UD.period.t<- matrix(data=c(s053_mean_of_session_1_UD.period.t, 
                                                  s053_mean_of_session_2_UD.period.t,
                                                  s053_mean_of_session_3_UD.period.t,
                                                  s053_mean_of_session_4_UD.period.t,
                                                  s053_mean_of_session_5_UD.period.t,
                                                  s053_mean_of_session_6_UD.period.t, 
                                                  s053_mean_of_session_7_UD.period.t, 
                                                  s053_mean_of_session_8_UD.period.t))
  
  #plot to compare the different means of different sessions in H.t of s010.
  plot(s053_means_combined_UD.period.t)
  
  
  #standard deviations of the different sessions of H.t withing s010:
  sd(s053_session_1_UD.period.t)
  sd(s053_session_2_UD.period.t)
  sd(s053_session_3_UD.period.t)
  sd(s053_session_4_UD.period.t)
  sd(s053_session_5_UD.period.t)
  sd(s053_session_6_UD.period.t)
  sd(s053_session_7_UD.period.t)
  sd(s053_session_8_UD.period.t)
  
  #converting this into a matrix.
  s053_stdevs_combined_DD.period.t<- matrix(data=c( sd(s053_session_1_UD.period.td),
                                                    sd(s053_session_2_UD.period.t),
                                                    sd(s053_session_3_UD.period.t),
                                                    sd(s053_session_4_UD.period.t),
                                                    sd(s053_session_5_UD.period.t),
                                                    sd(s053_session_6_UD.period.t),
                                                    sd(s053_session_7_UD.period.t),
                                                    sd(s053_session_8_UD.period.t)))
  
  plot(s053_stdevs_combined_UD.period.t)
  
  #plots to compare the standard deviations and
  
  plot(s053_stdevs_combined_UD.period.t, s047_means_combined_UD.period.t, type="l")
  
  
  
#subject "s053", H.t variable
  # "s053" "session 1"
  s053_mean_of_session_1_H.t <- numeric(0)
  s053_session_1_H.t <- numeric(0)
  for(i in 1:length(x$H.t)){
    if(subject[i] == "s053"){
      if(session[i] == "1"){
        s053_session_1_H.t <- append(s053_session_1_H.t, H.t[i])
        s053_mean_of_session_1_H.t <- mean(s053_session_1_H.t)
      }
    }
  }
  s053_mean_of_session_1_H.t 
  
  # "s053" "session 2"
  s053_mean_of_session_2_H.t <- numeric(0)
  s053_session_2_H.t <- numeric(0)
  for(i in 1:length(x$H.t)){
    if(subject[i] == "s053"){
      if(session[i] == "2"){
        s053_session_2_H.t <- append(s053_session_2_H.t, H.t[i])
        s053_mean_of_session_2_H.t <- mean(s053_session_2_H.t)
      }
    }
  }
  s053_mean_of_session_2_H.t 
  
  # "s053" "session 3"
  s053_mean_of_session_3_H.t <- numeric(0)
  s053_session_3_H.t <- numeric(0)
  for(i in 1:length(x$H.t)){
    if(subject[i] == "s053"){
      if(session[i] == "3"){
        s053_session_3_H.t <- append(s053_session_3_H.t, H.t[i])
        s053_mean_of_session_3_H.t <- mean(s053_session_3_H.t)
      }
    }
  }
  s053_mean_of_session_3_H.t 
  
  # "s053" "session 4"
  s053_mean_of_session_4_H.t <- numeric(0)
  s053_session_4_H.t <- numeric(0)
  for(i in 1:length(x$H.t)){
    if(subject[i] == "s053"){
      if(session[i] == "4"){
        s053_session_4_H.t <- append(s053_session_4_H.t, H.t[i])
        s053_mean_of_session_4_H.t <- mean(s053_session_4_H.t)
      }
    }
  }
  s053_mean_of_session_4_H.t 
  
  # "s053" "session 5"
  s053_mean_of_session_5_H.t <- numeric(0)
  s053_session_5_H.t <- numeric(0)
  for(i in 1:length(x$H.t)){
    if(subject[i] == "s053"){
      if(session[i] == "5"){
        s053_session_5_H.t <- append(s053_session_5_H.t, H.t[i])
        s053_mean_of_session_5_H.t <- mean(s053_session_5_H.t)
      }
    }
  }
  s053_mean_of_session_5_H.t 
  
  # "s053" "session 6"
  s053_mean_of_session_6_H.t <- numeric(0)
  s053_session_6_H.t <- numeric(0)
  for(i in 1:length(x$H.t)){
    if(subject[i] == "s053"){
      if(session[i] == "6"){
        s053_session_6_H.t <- append(s053_session_6_H.t, H.t[i])
        s053_mean_of_session_6_H.t <- mean(s053_session_6_H.t)
      }
    }
  }
  s053_mean_of_session_6_H.t 
  
  # "s053" "session 7"
  s053_mean_of_session_7_H.t <- numeric(0)
  s053_session_7_H.t <- numeric(0)
  for(i in 1:length(x$H.t)){
    if(subject[i] == "s053"){
      if(session[i] == "7"){
        s053_session_7_H.t <- append(s053_session_7_H.t, H.t[i])
        s053_mean_of_session_7_H.t <- mean(s053_session_7_H.t)
      }
    }
  }
  s053_mean_of_session_7_H.t 
  
  # "s053" "session 8"
  s053_mean_of_session_8_H.t <- numeric(0)
  s053_session_8_H.t <- numeric(0)
  for(i in 1:length(x$H.t)){
    if(subject[i] == "s053"){
      if(session[i] == "8"){
        s053_session_8_H.t <- append(s053_session_8_H.t, H.t[i])
        s053_mean_of_session_8_H.t <- mean(s053_session_8_H.t)
      }
    }
  }
  s053_mean_of_session_8_H.t 
  
  # "s053" "all sessions"
  s053_mean_of_all_sessions_H.t <- numeric(0)
  s053_all_sessions_H.t <- numeric(0)
  for(i in 1:length(x$H.t)){
    if(subject[i] == "s053"){
      s053_all_sessions_H.t <- append(s053_all_sessions_H.t, H.t[i])
      s053_mean_of_all_sessions_H.t <- mean(s053_all_sessions_H.t)
    }
  }
  s053_mean_of_all_sessions_H.t
  
 
  
  s053_means_combined_H.t<- matrix(data=c(s053_mean_of_session_1_H.t, 
                                                  s053_mean_of_session_2_H.t,
                                                  s053_mean_of_session_3_H.t,
                                                  s053_mean_of_session_4_H.t,
                                                  s053_mean_of_session_5_H.t,
                                                  s053_mean_of_session_6_H.t, 
                                                  s053_mean_of_session_7_H.t, 
                                                  s053_mean_of_session_8_H.t))
  
  #plot to compare the different means of different sessions in H.t of s010.
  plot(s053_means_combined_H.t)
  
  
  #standard deviations of the different sessions of H.t withing s010:
  sd(s053_session_1_H.t)
  sd(s053_session_2_H.t)
  sd(s053_session_3_H.t)
  sd(s053_session_4_H.t)
  sd(s053_session_5_H.t)
  sd(s053_session_6_H.t)
  sd(s053_session_7_H.t)
  sd(s053_session_8_H.t)
  
  #converting this into a matrix.
  s053_stdevs_combined_DD.period.t<- matrix(data=c( sd(s053_session_1_H.t),
                                                    sd(s053_session_2_H.t),
                                                    sd(s053_session_3_H.t),
                                                    sd(s053_session_4_H.t),
                                                    sd(s053_session_5_H.t),
                                                    sd(s053_session_6_H.t),
                                                    sd(s053_session_7_H.t),
                                                    sd(s053_session_8_H.t)))
  
  plot(s053_stdevs_combined_H.t)
  plot(S053_stdevs_combined_H.t, S054_means_combined_H.t, type="l")
#subject "s053", DD.t.i variable
  # "s053" "session 1"
  s053_mean_of_session_1_DD.t.i <- numeric(0)
  s053_session_1_DD.t.i <- numeric(0)
  for(i in 1:length(x$DD.t.i)){
    if(subject[i] == "s053"){
      if(session[i] == "1"){
        s053_session_1_DD.t.i <- append(s053_session_1_DD.t.i, DD.t.i[i])
        s053_mean_of_session_1_DD.t.i <- mean(s053_session_1_DD.t.i)
      }
    }
  }
  s053_mean_of_session_1_DD.t.i 
  
  # "s053" "session 2"
  s053_mean_of_session_2_DD.t.i <- numeric(0)
  s053_session_2_DD.t.i <- numeric(0)
  for(i in 1:length(x$DD.t.i)){
    if(subject[i] == "s053"){
      if(session[i] == "2"){
        s053_session_2_DD.t.i <- append(s053_session_2_DD.t.i, DD.t.i[i])
        s053_mean_of_session_2_DD.t.i <- mean(s053_session_2_DD.t.i)
      }
    }
  }
  s053_mean_of_session_2_DD.t.i 
  
  # "s053" "session 3"
  s053_mean_of_session_3_DD.t.i <- numeric(0)
  s053_session_3_DD.t.i <- numeric(0)
  for(i in 1:length(x$DD.t.i)){
    if(subject[i] == "s053"){
      if(session[i] == "3"){
        s053_session_3_DD.t.i <- append(s053_session_3_DD.t.i, DD.t.i[i])
        s053_mean_of_session_3_DD.t.i <- mean(s053_session_3_DD.t.i)
      }
    }
  }
  s053_mean_of_session_3_DD.t.i 
  
  # "s053" "session 4"
  s053_mean_of_session_4_DD.t.i <- numeric(0)
  s053_session_4_DD.t.i <- numeric(0)
  for(i in 1:length(x$DD.t.i)){
    if(subject[i] == "s053"){
      if(session[i] == "4"){
        s053_session_4_DD.t.i <- append(s053_session_4_DD.t.i, DD.t.i[i])
        s053_mean_of_session_4_DD.t.i <- mean(s053_session_4_DD.t.i)
      }
    }
  }
  s053_mean_of_session_4_DD.t.i 
  
  # "s053" "session 5"
  s053_mean_of_session_5_DD.t.i <- numeric(0)
  s053_session_5_DD.t.i <- numeric(0)
  for(i in 1:length(x$DD.t.i)){
    if(subject[i] == "s053"){
      if(session[i] == "5"){
        s053_session_5_DD.t.i <- append(s053_session_5_DD.t.i, DD.t.i[i])
        s053_mean_of_session_5_DD.t.i <- mean(s053_session_5_DD.t.i)
      }
    }
  }
  s053_mean_of_session_5_DD.t.i 
  
  # "s053" "session 6"
  s053_mean_of_session_6_DD.t.i <- numeric(0)
  s053_session_6_DD.t.i <- numeric(0)
  for(i in 1:length(x$DD.t.i)){
    if(subject[i] == "s053"){
      if(session[i] == "6"){
        s053_session_6_DD.t.i <- append(s053_session_6_DD.t.i, DD.t.i[i])
        s053_mean_of_session_6_DD.t.i <- mean(s053_session_6_DD.t.i)
      }
    }
  }
  s053_mean_of_session_6_DD.t.i 
  
  # "s053" "session 7"
  s053_mean_of_session_7_DD.t.i <- numeric(0)
  s053_session_7_DD.t.i <- numeric(0)
  for(i in 1:length(x$DD.t.i)){
    if(subject[i] == "s053"){
      if(session[i] == "7"){
        s053_session_7_DD.t.i <- append(s053_session_7_DD.t.i, DD.t.i[i])
        s053_mean_of_session_7_DD.t.i <- mean(s053_session_7_DD.t.i)
      }
    }
  }
  s053_mean_of_session_7_DD.t.i 
  
  # "s053" "session 8"
  s053_mean_of_session_8_DD.t.i <- numeric(0)
  s053_session_8_DD.t.i <- numeric(0)
  for(i in 1:length(x$DD.t.i)){
    if(subject[i] == "s053"){
      if(session[i] == "8"){
        s053_session_8_DD.t.i <- append(s053_session_8_DD.t.i, DD.t.i[i])
        s053_mean_of_session_8_DD.t.i <- mean(s053_session_8_DD.t.i)
      }
    }
  }
  s053_mean_of_session_8_DD.t.i 
  
  # "s053" "all sessions"
  s053_mean_of_all_sessions_DD.t.i <- numeric(0)
  s053_all_sessions_DD.t.i <- numeric(0)
  for(i in 1:length(x$DD.t.i)){
    if(subject[i] == "s053"){
      s053_all_sessions_DD.t.i <- append(s053_all_sessions_DD.t.i, DD.t.i[i])
      s053_mean_of_all_sessions_DD.t.i <- mean(s053_all_sessions_DD.t.i)
    }
  }
  s053_mean_of_all_sessions_DD.t.i

  
  
  
  s053_means_combined_H.t<- matrix(data=c(s053_mean_of_session_1_DD.t.i, 
                                          s053_mean_of_session_2_DD.t.i,
                                          s053_mean_of_session_3_DD.t.i,
                                          s053_mean_of_session_4_DD.t.i,
                                          s053_mean_of_session_5_DD.t.i,
                                          s053_mean_of_session_6_DD.t.i, 
                                          s053_mean_of_session_7_DD.t.i, 
                                          s053_mean_of_session_8_DD.t.i))
  
  #plot to compare the different means of different sessions in H.t of s010.
  plot(s053_means_combined_DD.t.i)
  
  
  #standard deviations of the different sessions of H.t withing s010:
  sd(s053_session_1_DD.t.it)
  sd(s053_session_2_DD.t.i)
  sd(s053_session_3_DD.t.i)
  sd(s053_session_4_DD.t.i)
  sd(s053_session_5_DD.t.i)
  sd(s053_session_6_DD.t.i)
  sd(s053_session_7_DD.t.i)
  sd(s053_session_8_DD.t.i)
  
  #converting this into a matrix.
  s053_stdevs_combined_DD.t.i<- matrix(data=c( sd(s053_session_1_DD.t.i),
                                                    sd(s053_session_2_DD.t.i),
                                                    sd(s053_session_3_DD.t.i),
                                                    sd(s053_session_4_DD.t.i),
                                                    sd(s053_session_5_DD.t.i),
                                                    sd(s053_session_6_DD.t.i),
                                                    sd(s053_session_7_DD.t.i),
                                                    sd(s053_session_8_DD.t.i)))
  
  plot(s053_stdevs_combined_DD.t.i)
  plot(S053_stdevs_combined_DD.t.i, S054_means_combined_DD.t.i, type="l")
  
  
  
#subject "s054", H.period variable
  # "s054" "session 1"
  s054_mean_of_session_1_H.period <- numeric(0)
  s054_session_1_H.period <- numeric(0)
  for(i in 1:length(x$H.period)){
    if(subject[i] == "s054"){
      if(session[i] == "1"){
        s054_session_1_H.period <- append(s054_session_1_H.period, H.period[i])
        s054_mean_of_session_1_H.period <- mean(s054_session_1_H.period)
      }
    }
  }
  s054_mean_of_session_1_H.period 
  
  # "s054" "session 2"
  s054_mean_of_session_2_H.period <- numeric(0)
  s054_session_2_H.period <- numeric(0)
  for(i in 1:length(x$H.period)){
    if(subject[i] == "s054"){
      if(session[i] == "2"){
        s054_session_2_H.period <- append(s054_session_2_H.period, H.period[i])
        s054_mean_of_session_2_H.period <- mean(s054_session_2_H.period)
      }
    }
  }
  s054_mean_of_session_2_H.period 
  
  # "s054" "session 3"
  s054_mean_of_session_3_H.period <- numeric(0)
  s054_session_3_H.period <- numeric(0)
  for(i in 1:length(x$H.period)){
    if(subject[i] == "s054"){
      if(session[i] == "3"){
        s054_session_3_H.period <- append(s054_session_3_H.period, H.period[i])
        s054_mean_of_session_3_H.period <- mean(s054_session_3_H.period)
      }
    }
  }
  s054_mean_of_session_3_H.period 
  
  # "s054" "session 4"
  s054_mean_of_session_4_H.period <- numeric(0)
  s054_session_4_H.period <- numeric(0)
  for(i in 1:length(x$H.period)){
    if(subject[i] == "s054"){
      if(session[i] == "4"){
        s054_session_4_H.period <- append(s054_session_4_H.period, H.period[i])
        s054_mean_of_session_4_H.period <- mean(s054_session_4_H.period)
      }
    }
  }
  s054_mean_of_session_4_H.period 
  
  # "s054" "session 5"
  s054_mean_of_session_5_H.period <- numeric(0)
  s054_session_5_H.period <- numeric(0)
  for(i in 1:length(x$H.period)){
    if(subject[i] == "s054"){
      if(session[i] == "5"){
        s054_session_5_H.period <- append(s054_session_5_H.period, H.period[i])
        s054_mean_of_session_5_H.period <- mean(s054_session_5_H.period)
      }
    }
  }
  s054_mean_of_session_5_H.period 
  
  # "s054" "session 6"
  s054_mean_of_session_6_H.period <- numeric(0)
  s054_session_6_H.period <- numeric(0)
  for(i in 1:length(x$H.period)){
    if(subject[i] == "s054"){
      if(session[i] == "6"){
        s054_session_6_H.period <- append(s054_session_6_H.period, H.period[i])
        s054_mean_of_session_6_H.period <- mean(s054_session_6_H.period)
      }
    }
  }
  s054_mean_of_session_6_H.period 
  
  # "s054" "session 7"
  s054_mean_of_session_7_H.period <- numeric(0)
  s054_session_7_H.period <- numeric(0)
  for(i in 1:length(x$H.period)){
    if(subject[i] == "s054"){
      if(session[i] == "7"){
        s054_session_7_H.period <- append(s054_session_7_H.period, H.period[i])
        s054_mean_of_session_7_H.period <- mean(s054_session_7_H.period)
      }
    }
  }
  s054_mean_of_session_7_H.period 
  
  # "s054" "session 8"
  s054_mean_of_session_8_H.period <- numeric(0)
  s054_session_8_H.period <- numeric(0)
  for(i in 1:length(x$H.period)){
    if(subject[i] == "s054"){
      if(session[i] == "8"){
        s054_session_8_H.period <- append(s054_session_8_H.period, H.period[i])
        s054_mean_of_session_8_H.period <- mean(s054_session_8_H.period)
      }
    }
  }
  s054_mean_of_session_8_H.period 
  
  # "s054" "all sessions"
  s054_mean_of_all_sessions_H.period <- numeric(0)
  s054_all_sessions_H.period <- numeric(0)
  for(i in 1:length(x$H.period)){
    if(subject[i] == "s054"){
      s054_all_sessions_H.period <- append(s054_all_sessions_H.period, H.period[i])
      s054_mean_of_all_sessions_H.period <- mean(s054_all_sessions_H.period)
    }
  }
  s054_mean_of_all_sessions_H.period
  
  
  
  s054_means_combined_H.t<- matrix(data=c(s054_mean_of_session_1_H.period, 
                                          s054_mean_of_session_2_H.period,
                                          s054_mean_of_session_3_H.period,
                                          s054_mean_of_session_4_H.period,
                                          s054_mean_of_session_5_H.period,
                                          s054_mean_of_session_6_H.period, 
                                          s054_mean_of_session_7_H.period, 
                                          s054_mean_of_session_8_H.period))
  
  #plot to compare the different means of different sessions in H.t of s010.
  plot(s054_means_combined_H.period)
  
  
  #standard deviations of the different sessions of H.t withing s010:
  sd(s054_session_1_H.period)
  sd(s054_session_2_H.period)
  sd(s054_session_3_H.period)
  sd(s054_session_4_H.period)
  sd(s054_session_5_H.period)
  sd(s054_session_6_H.period)
  sd(s054_session_7_H.period)
  sd(s054_session_8_H.period)
  
  #converting this into a matrix.
  s054_stdevs_combined_H.period<- matrix(data=c( sd(s054_session_1_H.period),
                                               sd(s054_session_2_H.period),
                                               sd(s054_session_3_H.period),
                                               sd(s054_session_4_H.period),
                                               sd(s054_session_5_H.period),
                                               sd(s054_session_6_H.period),
                                               sd(s054_session_7_H.period),
                                               sd(s054_session_8_H.period)))
  
  plot(s054_stdevs_combined_H.period)
  plot(S054_stdevs_combined_H.period, S054_means_combined_H_period, type="l")
  
#subject "s054", DD.period.t variable
  # "s054" "session 1"
  s054_mean_of_session_1_DD.period.t <- numeric(0)
  s054_session_1_DD.period.t <- numeric(0)
  for(i in 1:length(x$DD.period.t)){
    if(subject[i] == "s054"){
      if(session[i] == "1"){
        s054_session_1_DD.period.t <- append(s054_session_1_DD.period.t, DD.period.t[i])
        s054_mean_of_session_1_DD.period.t <- mean(s054_session_1_DD.period.t)
      }
    }
  }
  s054_mean_of_session_1_DD.period.t 
  
  # "s054" "session 2"
  s054_mean_of_session_2_DD.period.t <- numeric(0)
  s054_session_2_DD.period.t <- numeric(0)
  for(i in 1:length(x$DD.period.t)){
    if(subject[i] == "s054"){
      if(session[i] == "2"){
        s054_session_2_DD.period.t <- append(s054_session_2_DD.period.t, DD.period.t[i])
        s054_mean_of_session_2_DD.period.t <- mean(s054_session_2_DD.period.t)
      }
    }
  }
  s054_mean_of_session_2_DD.period.t 
  
  # "s054" "session 3"
  s054_mean_of_session_3_DD.period.t <- numeric(0)
  s054_session_3_DD.period.t <- numeric(0)
  for(i in 1:length(x$DD.period.t)){
    if(subject[i] == "s054"){
      if(session[i] == "3"){
        s054_session_3_DD.period.t <- append(s054_session_3_DD.period.t, DD.period.t[i])
        s054_mean_of_session_3_DD.period.t <- mean(s054_session_3_DD.period.t)
      }
    }
  }
  s054_mean_of_session_3_DD.period.t 
  
  # "s054" "session 4"
  s054_mean_of_session_4_DD.period.t <- numeric(0)
  s054_session_4_DD.period.t <- numeric(0)
  for(i in 1:length(x$DD.period.t)){
    if(subject[i] == "s054"){
      if(session[i] == "4"){
        s054_session_4_DD.period.t <- append(s054_session_4_DD.period.t, DD.period.t[i])
        s054_mean_of_session_4_DD.period.t <- mean(s054_session_4_DD.period.t)
      }
    }
  }
  s054_mean_of_session_4_DD.period.t 
  
  # "s054" "session 5"
  s054_mean_of_session_5_DD.period.t <- numeric(0)
  s054_session_5_DD.period.t <- numeric(0)
  for(i in 1:length(x$DD.period.t)){
    if(subject[i] == "s054"){
      if(session[i] == "5"){
        s054_session_5_DD.period.t <- append(s054_session_5_DD.period.t, DD.period.t[i])
        s054_mean_of_session_5_DD.period.t <- mean(s054_session_5_DD.period.t)
      }
    }
  }
  s054_mean_of_session_5_DD.period.t 
  
  # "s054" "session 6"
  s054_mean_of_session_6_DD.period.t <- numeric(0)
  s054_session_6_DD.period.t <- numeric(0)
  for(i in 1:length(x$DD.period.t)){
    if(subject[i] == "s054"){
      if(session[i] == "6"){
        s054_session_6_DD.period.t <- append(s054_session_6_DD.period.t, DD.period.t[i])
        s054_mean_of_session_6_DD.period.t <- mean(s054_session_6_DD.period.t)
      }
    }
  }
  s054_mean_of_session_6_DD.period.t 
  
  # "s054" "session 7"
  s054_mean_of_session_7_DD.period.t <- numeric(0)
  s054_session_7_DD.period.t <- numeric(0)
  for(i in 1:length(x$DD.period.t)){
    if(subject[i] == "s054"){
      if(session[i] == "7"){
        s054_session_7_DD.period.t <- append(s054_session_7_DD.period.t, DD.period.t[i])
        s054_mean_of_session_7_DD.period.t <- mean(s054_session_7_DD.period.t)
      }
    }
  }
  s054_mean_of_session_7_DD.period.t 
  
  # "s054" "session 8"
  s054_mean_of_session_8_DD.period.t <- numeric(0)
  s054_session_8_DD.period.t <- numeric(0)
  for(i in 1:length(x$DD.period.t)){
    if(subject[i] == "s054"){
      if(session[i] == "8"){
        s054_session_8_DD.period.t <- append(s054_session_8_DD.period.t, DD.period.t[i])
        s054_mean_of_session_8_DD.period.t <- mean(s054_session_8_DD.period.t)
      }
    }
  }
  s054_mean_of_session_8_DD.period.t 
  
  # "s054" "all sessions"
  s054_mean_of_all_sessions_DD.period.t <- numeric(0)
  s054_all_sessions_DD.period.t <- numeric(0)
  for(i in 1:length(x$DD.period.t)){
    if(subject[i] == "s054"){
      s054_all_sessions_DD.period.t <- append(s054_all_sessions_DD.period.t, DD.period.t[i])
      s054_mean_of_all_sessions_DD.period.t <- mean(s054_all_sessions_DD.period.t)
    }
  }
  s054_mean_of_all_sessions_DD.period.t
  
  
  
  
  s054_means_combined_DD.period.t<- matrix(data=c(s054_mean_of_session_1_DD.period.t, 
                                          s054_mean_of_session_2_DD.period.td,
                                          s054_mean_of_session_3_DD.period.t,
                                          s054_mean_of_session_4_DD.period.t,
                                          s054_mean_of_session_5_DD.period.t,
                                          s054_mean_of_session_6_DD.period.t, 
                                          s054_mean_of_session_7_DD.period.t, 
                                          s054_mean_of_session_8_DD.period.t))
  
  #plot to compare the different means of different sessions in H.t of s010.
  plot(s054_means_combined_DD.period.t)
  
  
  #standard deviations of the different sessions of H.t withing s010:
  sd(s054_session_1_DD.period.t)
  sd(s054_session_2_DD.period.t)
  sd(s054_session_3_DD.period.t)
  sd(s054_session_4_DD.period.t)
  sd(s054_session_5_DD.period.t)
  sd(s054_session_6_DD.period.t)
  sd(s054_session_7_DD.period.t)
  sd(s054_session_8_DD.period.t)
  
  #converting this into a matrix.
  s054_stdevs_combined_DD.period.t<- matrix(data=c( sd(s054_session_1_DD.period.t),
                                                 sd(s054_session_2_DD.period.t),
                                                 sd(s054_session_3_DD.period.t),
                                                 sd(s054_session_4_DD.period.t),
                                                 sd(s054_session_5_DD.period.t),
                                                 sd(s054_session_6_DD.period.t),
                                                 sd(s054_session_7_DD.period.t),
                                                 sd(s054_session_8_DD.period.t)))
  
  plot(s054_stdevs_combined_DD.period.t)
  plot(S054_stdevs_combined_DD.period.t, S054_means_combined_DD.period.t, type="l")
#subject "s054", UD.period.t variable
  # "s054" "session 1"
  s054_mean_of_session_1_UD.period.t <- numeric(0)
  s054_session_1_UD.period.t <- numeric(0)
  for(i in 1:length(x$UD.period.t)){
    if(subject[i] == "s054"){
      if(session[i] == "1"){
        s054_session_1_UD.period.t <- append(s054_session_1_UD.period.t, UD.period.t[i])
        s054_mean_of_session_1_UD.period.t <- mean(s054_session_1_UD.period.t)
      }
    }
  }
  s054_mean_of_session_1_UD.period.t
  
  # "s054" "session 2"
  s054_mean_of_session_2_UD.period.t <- numeric(0)
  s054_session_2_UD.period.t <- numeric(0)
  for(i in 1:length(x$UD.period.t)){
    if(subject[i] == "s054"){
      if(session[i] == "2"){
        s054_session_2_UD.period.t <- append(s054_session_2_UD.period.t, UD.period.t[i])
        s054_mean_of_session_2_UD.period.t <- mean(s054_session_2_UD.period.t)
      }
    }
  }
  s054_mean_of_session_2_UD.period.t
  
  # "s054" "session 3"
  s054_mean_of_session_3_UD.period.t <- numeric(0)
  s054_session_3_UD.period.t <- numeric(0)
  for(i in 1:length(x$UD.period.t)){
    if(subject[i] == "s054"){
      if(session[i] == "3"){
        s054_session_3_UD.period.t <- append(s054_session_3_UD.period.t, UD.period.t[i])
        s054_mean_of_session_3_UD.period.t <- mean(s054_session_3_UD.period.t)
      }
    }
  }
  s054_mean_of_session_3_UD.period.t
  
  # "s054" "session 4"
  s054_mean_of_session_4_UD.period.t <- numeric(0)
  s054_session_4_UD.period.t <- numeric(0)
  for(i in 1:length(x$UD.period.t)){
    if(subject[i] == "s054"){
      if(session[i] == "4"){
        s054_session_4_UD.period.t <- append(s054_session_4_UD.period.t, UD.period.t[i])
        s054_mean_of_session_4_UD.period.t <- mean(s054_session_4_UD.period.t)
      }
    }
  }
  s054_mean_of_session_4_UD.period.t
  
  # "s054" "session 5"
  s054_mean_of_session_5_UD.period.t <- numeric(0)
  s054_session_5_UD.period.t <- numeric(0)
  for(i in 1:length(x$UD.period.t)){
    if(subject[i] == "s054"){
      if(session[i] == "5"){
        s054_session_5_UD.period.t <- append(s054_session_5_UD.period.t, UD.period.t[i])
        s054_mean_of_session_5_UD.period.t <- mean(s054_session_5_UD.period.t)
      }
    }
  }
  s054_mean_of_session_5_UD.period.t
  
  # "s054" "session 6"
  s054_mean_of_session_6_UD.period.t <- numeric(0)
  s054_session_6_UD.period.t <- numeric(0)
  for(i in 1:length(x$UD.period.t)){
    if(subject[i] == "s054"){
      if(session[i] == "6"){
        s054_session_6_UD.period.t <- append(s054_session_6_UD.period.t, UD.period.t[i])
        s054_mean_of_session_6_UD.period.t <- mean(s054_session_6_UD.period.t)
      }
    }
  }
  s054_mean_of_session_6_UD.period.t
  
  # "s054" "session 7"
  s054_mean_of_session_7_UD.period.t <- numeric(0)
  s054_session_7_UD.period.t <- numeric(0)
  for(i in 1:length(x$UD.period.t)){
    if(subject[i] == "s054"){
      if(session[i] == "7"){
        s054_session_7_UD.period.t <- append(s054_session_7_UD.period.t, UD.period.t[i])
        s054_mean_of_session_7_UD.period.t <- mean(s054_session_7_UD.period.t)
      }
    }
  }
  s054_mean_of_session_7_UD.period.t
  
  # "s054" "session 8"
  s054_mean_of_session_8_UD.period.t <- numeric(0)
  s054_session_8_UD.period.t <- numeric(0)
  for(i in 1:length(x$UD.period.t)){
    if(subject[i] == "s054"){
      if(session[i] == "8"){
        s054_session_8_UD.period.t <- append(s054_session_8_UD.period.t, UD.period.t[i])
        s054_mean_of_session_8_UD.period.t <- mean(s054_session_8_UD.period.t)
      }
    }
  }
  s054_mean_of_session_8_UD.period.t
  
  # "s054" "all sessions"
  s054_mean_of_all_sessions_UD.period.t <- numeric(0)
  s054_all_sessions_UD.period.t <- numeric(0)
  for(i in 1:length(x$UD.period.t)){
    if(subject[i] == "s054"){
      s054_all_sessions_UD.period.t <- append(s054_all_sessions_UD.period.t, UD.period.t[i])
      s054_mean_of_all_sessions_UD.period.t <- mean(s054_all_sessions_UD.period.t)
    }
  }
  s054_mean_of_all_sessions_UD.period.t
  
  
  s054_means_combined_UD.period.t<- matrix(data=c(s054_mean_of_session_1_UD.period.t, 
                                                  s054_mean_of_session_2_UD.period.t,
                                                  s054_mean_of_session_3_UD.period.t,
                                                  s054_mean_of_session_4_UD.period.t,
                                                  s054_mean_of_session_5_UD.period.t,
                                                  s054_mean_of_session_6_UD.period.t, 
                                                  s054_mean_of_session_7_UD.period.t, 
                                                  s054_mean_of_session_8_UD.period.t))
  
  #plot to compare the different means of different sessions in H.t of s010.
  plot(s054_means_combined_UD.period.t)
  
  
  #standard deviations of the different sessions of H.t withing s010:
  sd(s054_session_1_UD.period.t)
  sd(s054_session_2_UD.period.t)
  sd(s054_session_3_UD.period.t)
  sd(s054_session_4_UD.period.t)
  sd(s054_session_5_UD.period.t)
  sd(s054_session_6_UD.period.t)
  sd(s054_session_7_UD.period.t)
  sd(s054_session_8_UD.period.t)
  
  #converting this into a matrix.
  s054_stdevs_combined_UD.period.t<- matrix(data=c( sd(s054_session_1_UD.period.t),
                                                    sd(s054_session_2_UD.period.t),
                                                    sd(s054_session_3_UD.period.t),
                                                    sd(s054_session_4_UD.period.t),
                                                    sd(s054_session_5_UD.period.t),
                                                    sd(s054_session_6_UD.period.t),
                                                    sd(s054_session_7_UD.period.t),
                                                    sd(s054_session_8_UD.period.t)))
  
  plot(s054_stdevs_combined_UD.period.t)
  plot(S054_stdevs_combined_UD.period.t, S054_means_combined_UD.period.t, type="l")
  
#subject "s054", H.t variable
  # "s054" "session 1"
  s054_mean_of_session_1_H.t <- numeric(0)
  s054_session_1_H.t <- numeric(0)
  for(i in 1:length(x$H.t)){
    if(subject[i] == "s054"){
      if(session[i] == "1"){
        s054_session_1_H.t <- append(s054_session_1_H.t, H.t[i])
        s054_mean_of_session_1_H.t <- mean(s054_session_1_H.t)
      }
    }
  }
  s054_mean_of_session_1_H.t 
  
  # "s054" "session 2"
  s054_mean_of_session_2_H.t <- numeric(0)
  s054_session_2_H.t <- numeric(0)
  for(i in 1:length(x$H.t)){
    if(subject[i] == "s054"){
      if(session[i] == "2"){
        s054_session_2_H.t <- append(s054_session_2_H.t, H.t[i])
        s054_mean_of_session_2_H.t <- mean(s054_session_2_H.t)
      }
    }
  }
  s054_mean_of_session_2_H.t 
  
  # "s054" "session 3"
  s054_mean_of_session_3_H.t <- numeric(0)
  s054_session_3_H.t <- numeric(0)
  for(i in 1:length(x$H.t)){
    if(subject[i] == "s054"){
      if(session[i] == "3"){
        s054_session_3_H.t <- append(s054_session_3_H.t, H.t[i])
        s054_mean_of_session_3_H.t <- mean(s054_session_3_H.t)
      }
    }
  }
  s054_mean_of_session_3_H.t 
  
  # "s054" "session 4"
  s054_mean_of_session_4_H.t <- numeric(0)
  s054_session_4_H.t <- numeric(0)
  for(i in 1:length(x$H.t)){
    if(subject[i] == "s054"){
      if(session[i] == "4"){
        s054_session_4_H.t <- append(s054_session_4_H.t, H.t[i])
        s054_mean_of_session_4_H.t <- mean(s054_session_4_H.t)
      }
    }
  }
  s054_mean_of_session_4_H.t 
  
  # "s054" "session 5"
  s054_mean_of_session_5_H.t <- numeric(0)
  s054_session_5_H.t <- numeric(0)
  for(i in 1:length(x$H.t)){
    if(subject[i] == "s054"){
      if(session[i] == "5"){
        s054_session_5_H.t <- append(s054_session_5_H.t, H.t[i])
        s054_mean_of_session_5_H.t <- mean(s054_session_5_H.t)
      }
    }
  }
  s054_mean_of_session_5_H.t 
  
  # "s054" "session 6"
  s054_mean_of_session_6_H.t <- numeric(0)
  s054_session_6_H.t <- numeric(0)
  for(i in 1:length(x$H.t)){
    if(subject[i] == "s054"){
      if(session[i] == "6"){
        s054_session_6_H.t <- append(s054_session_6_H.t, H.t[i])
        s054_mean_of_session_6_H.t <- mean(s054_session_6_H.t)
      }
    }
  }
  s054_mean_of_session_6_H.t 
  
  # "s054" "session 7"
  s054_mean_of_session_7_H.t <- numeric(0)
  s054_session_7_H.t <- numeric(0)
  for(i in 1:length(x$H.t)){
    if(subject[i] == "s054"){
      if(session[i] == "7"){
        s054_session_7_H.t <- append(s054_session_7_H.t, H.t[i])
        s054_mean_of_session_7_H.t <- mean(s054_session_7_H.t)
      }
    }
  }
  s054_mean_of_session_7_H.t 
  
  # "s054" "session 8"
  s054_mean_of_session_8_H.t <- numeric(0)
  s054_session_8_H.t <- numeric(0)
  for(i in 1:length(x$H.t)){
    if(subject[i] == "s054"){
      if(session[i] == "8"){
        s054_session_8_H.t <- append(s054_session_8_H.t, H.t[i])
        s054_mean_of_session_8_H.t <- mean(s054_session_8_H.t)
      }
    }
  }
  s054_mean_of_session_8_H.t 
  
  # "s054" "all sessions"
  s054_mean_of_all_sessions_H.t <- numeric(0)
  s054_all_sessions_H.t <- numeric(0)
  for(i in 1:length(x$H.t)){
    if(subject[i] == "s054"){
      s054_all_sessions_H.t <- append(s054_all_sessions_H.t, H.t[i])
      s054_mean_of_all_sessions_H.t <- mean(s054_all_sessions_H.t)
    }
  }
  s054_mean_of_all_sessions_H.t
  
  
  s054_means_combined_H.t<- matrix(data=c(s054_mean_of_session_1_H.t, 
                                                  s054_mean_of_session_2_H.t,
                                                  s054_mean_of_session_3_H.t,
                                                  s054_mean_of_session_4_H.t,
                                                  s054_mean_of_session_5_H.t,
                                                  s054_mean_of_session_6_H.t, 
                                                  s054_mean_of_session_7_H.t, 
                                                  s054_mean_of_session_8_H.t))
  
  #plot to compare the different means of different sessions in H.t of s010.
  plot(s054_means_combined_H.t)
  
  
  #standard deviations of the different sessions of H.t withing s010:
  sd(s054_session_1_H.t)
  sd(s054_session_2_H.t)
  sd(s054_session_3_H.t)
  sd(s054_session_4_H.t)
  sd(s054_session_5_H.t)
  sd(s054_session_6_H.t)
  sd(s054_session_7_H.t)
  sd(s054_session_8_H.t)
  
  #converting this into a matrix.
  s054_stdevs_combined_H.t<- matrix(data=c( sd(s054_session_1_H.t),
                                                    sd(s054_session_2_H.t),
                                                    sd(s054_session_3_H.t),
                                                    sd(s054_session_4_H.t),
                                                    sd(s054_session_5_H.t),
                                                    sd(s054_session_6_H.t),
                                                    sd(s054_session_7_H.t),
                                                    sd(s054_session_8_H.t)))
  
  plot(s054_stdevs_combined_H.t)
  plot(S054_stdevs_combined_H.t, S054_means_combined_H.t, type="l")
#subject "s054", DD.t.i variable
  # "s054" "session 1"
  s054_mean_of_session_1_DD.t.i <- numeric(0)
  s054_session_1_DD.t.i <- numeric(0)
  for(i in 1:length(x$DD.t.i)){
    if(subject[i] == "s054"){
      if(session[i] == "1"){
        s054_session_1_DD.t.i <- append(s054_session_1_DD.t.i, DD.t.i[i])
        s054_mean_of_session_1_DD.t.i <- mean(s054_session_1_DD.t.i)
      }
    }
  }
  s054_mean_of_session_1_DD.t.i 
  
  # "s054" "session 2"
  s054_mean_of_session_2_DD.t.i <- numeric(0)
  s054_session_2_DD.t.i <- numeric(0)
  for(i in 1:length(x$DD.t.i)){
    if(subject[i] == "s054"){
      if(session[i] == "2"){
        s054_session_2_DD.t.i <- append(s054_session_2_DD.t.i, DD.t.i[i])
        s054_mean_of_session_2_DD.t.i <- mean(s054_session_2_DD.t.i)
      }
    }
  }
  s054_mean_of_session_2_DD.t.i 
  
  # "s054" "session 3"
  s054_mean_of_session_3_DD.t.i <- numeric(0)
  s054_session_3_DD.t.i <- numeric(0)
  for(i in 1:length(x$DD.t.i)){
    if(subject[i] == "s054"){
      if(session[i] == "3"){
        s054_session_3_DD.t.i <- append(s054_session_3_DD.t.i, DD.t.i[i])
        s054_mean_of_session_3_DD.t.i <- mean(s054_session_3_DD.t.i)
      }
    }
  }
  s054_mean_of_session_3_DD.t.i 
  
  # "s054" "session 4"
  s054_mean_of_session_4_DD.t.i <- numeric(0)
  s054_session_4_DD.t.i <- numeric(0)
  for(i in 1:length(x$DD.t.i)){
    if(subject[i] == "s054"){
      if(session[i] == "4"){
        s054_session_4_DD.t.i <- append(s054_session_4_DD.t.i, DD.t.i[i])
        s054_mean_of_session_4_DD.t.i <- mean(s054_session_4_DD.t.i)
      }
    }
  }
  s054_mean_of_session_4_DD.t.i 
  
  # "s054" "session 5"
  s054_mean_of_session_5_DD.t.i <- numeric(0)
  s054_session_5_DD.t.i <- numeric(0)
  for(i in 1:length(x$DD.t.i)){
    if(subject[i] == "s054"){
      if(session[i] == "5"){
        s054_session_5_DD.t.i <- append(s054_session_5_DD.t.i, DD.t.i[i])
        s054_mean_of_session_5_DD.t.i <- mean(s054_session_5_DD.t.i)
      }
    }
  }
  s054_mean_of_session_5_DD.t.i 
  
  # "s054" "session 6"
  s054_mean_of_session_6_DD.t.i <- numeric(0)
  s054_session_6_DD.t.i <- numeric(0)
  for(i in 1:length(x$DD.t.i)){
    if(subject[i] == "s054"){
      if(session[i] == "6"){
        s054_session_6_DD.t.i <- append(s054_session_6_DD.t.i, DD.t.i[i])
        s054_mean_of_session_6_DD.t.i <- mean(s054_session_6_DD.t.i)
      }
    }
  }
  s054_mean_of_session_6_DD.t.i 
  
  # "s054" "session 7"
  s054_mean_of_session_7_DD.t.i <- numeric(0)
  s054_session_7_DD.t.i <- numeric(0)
  for(i in 1:length(x$DD.t.i)){
    if(subject[i] == "s054"){
      if(session[i] == "7"){
        s054_session_7_DD.t.i <- append(s054_session_7_DD.t.i, DD.t.i[i])
        s054_mean_of_session_7_DD.t.i <- mean(s054_session_7_DD.t.i)
      }
    }
  }
  s054_mean_of_session_7_DD.t.i 
  
  # "s054" "session 8"
  s054_mean_of_session_8_DD.t.i <- numeric(0)
  s054_session_8_DD.t.i <- numeric(0)
  for(i in 1:length(x$DD.t.i)){
    if(subject[i] == "s054"){
      if(session[i] == "8"){
        s054_session_8_DD.t.i <- append(s054_session_8_DD.t.i, DD.t.i[i])
        s054_mean_of_session_8_DD.t.i <- mean(s054_session_8_DD.t.i)
      }
    }
  }
  s054_mean_of_session_8_DD.t.i 
  
  # "s054" "all sessions"
  s054_mean_of_all_sessions_DD.t.i <- numeric(0)
  s054_all_sessions_DD.t.i <- numeric(0)
  for(i in 1:length(x$DD.t.i)){
     if(subject[i] == "s054"){
        s054_all_sessions_DD.t.i <- append(s054_all_sessions_DD.t.i, DD.t.i[i])
        s054_mean_of_all_sessions_DD.t.i <- mean(s054_all_sessions_DD.t.i)
     }
  }
  s054_mean_of_all_sessions_DD.t.i
  
  s054_means_combined_DD.t.i <- matrix(data=c(s054_mean_of_session_DD.t.i, 
                                          s054_mean_of_session_2_DD.t.i,
                                          s054_mean_of_session_3_DD.t.i,
                                          s054_mean_of_session_4_DD.t.i,
                                          s054_mean_of_session_5_DD.t.i,
                                          s054_mean_of_session_6_DD.t.i, 
                                          s054_mean_of_session_7_DD.t.i, 
                                          s054_mean_of_session_8_DD.t.i))
  
  #plot to compare the different means of different sessions in H.t of s010.
  plot(s054_means_combined_DD.t.i )
  
  
  #standard deviations of the different sessions of H.t withing s010:
  sd(s054_session_1_DD.t.i)
  sd(s054_session_2_DD.t.i)
  sd(s054_session_3_DD.t.i)
  sd(s054_session_4_DD.t.i)
  sd(s054_session_5_DD.t.i)
  sd(s054_session_6_DD.t.i)
  sd(s054_session_7_DD.t.i)
  sd(s054_session_8_DD.t.i)
  
  #converting this into a matrix.
  s054_stdevs_combined_H.t<- matrix(data=c( sd(s054_session_1_DD.t.i ),
                                            sd(s054_session_2_DD.t.i ),
                                            sd(s054_session_3_DD.t.i ),
                                            sd(s054_session_4_DD.t.i ),
                                            sd(s054_session_5_DD.t.i ),
                                            sd(s054_session_6_DD.t.i ),
                                            sd(s054_session_7_DD.t.i ),
                                            sd(s054_session_8_DD.t.i )))
  
  plot(s054_stdevs_combined_DD.t.i )
  
  plot(S054_stdevs_combined_DD.t.i, S054_means_combined_DD.t.i, type="l")
  
  
  
  

#standard deviation of all sessions combined
  # s010
  sd(all_sessions_H.period)
  sd(all_sessions_DD.period.t)
  sd(all_sessions_UD.period.t)
  sd(all_sessions_H.t)
  sd(all_sessions_DD.t.i)
  
  # s024
  sd(s024_all_sessions_H.period)
  sd(s024_all_sessions_DD.period.t)
  sd(s024_all_sessions_UD.period.t)
  sd(s024_all_sessions_H.t)
  sd(s024_all_sessions_DD.t.i)
  
  # s047
  sd(s047_all_sessions_H.period)
  sd(s047_all_sessions_DD.period.t)
  sd(s047_all_sessions_UD.period.t)
  sd(s047_all_sessions_H.t)
  sd(s047_all_sessions_DD.t.i)
  
  # s053
  sd(s053_all_sessions_H.period)
  sd(s053_all_sessions_DD.period.t)
  sd(s053_all_sessions_UD.period.t)
  sd(s053_all_sessions_H.t)
  sd(s053_all_sessions_DD.t.i)
  
  # s054
  sd(s054_all_sessions_H.period)
  sd(s054_all_sessions_DD.period.t)
  sd(s054_all_sessions_UD.period.t)
  sd(s054_all_sessions_H.t)
  sd(s054_all_sessions_DD.t.i)

  
  
#standard deviation of each individual session
  # s010
    # s010 H.period
    sd(session_1_H.period)
    sd(session_2_H.period)
    sd(session_3_H.period)
    sd(session_4_H.period)
    sd(session_5_H.period)
    sd(session_6_H.period)
    sd(session_7_H.period)
    sd(session_8_H.period)
    
    # s010 DD.period.t
    sd(session_1_DD.period.t)
    sd(session_2_DD.period.t)
    sd(session_3_DD.period.t)
    sd(session_4_DD.period.t)
    sd(session_5_DD.period.t)
    sd(session_6_DD.period.t)
    sd(session_7_DD.period.t)
    sd(session_8_DD.period.t)
    
    # s010 UD.period.t 
    sd(session_1_UD.period.t)
    sd(session_2_UD.period.t)
    sd(session_3_UD.period.t)
    sd(session_4_UD.period.t)
    sd(session_5_UD.period.t)
    sd(session_6_UD.period.t)
    sd(session_7_UD.period.t)
    sd(session_8_UD.period.t)
    
    # s010 H.t
    sd(session_1_H.t)
    sd(session_2_H.t)
    sd(session_3_H.t)
    sd(session_4_H.t)
    sd(session_5_H.t)
    sd(session_6_H.t)
    sd(session_7_H.t)
    sd(session_8_H.t)
    
    # s010 DD.t.i
    sd(session_1_DD.t.i)
    sd(session_2_DD.t.i)
    sd(session_3_DD.t.i)
    sd(session_4_DD.t.i)
    sd(session_5_DD.t.i)
    sd(session_6_DD.t.i)
    sd(session_7_DD.t.i)
    sd(session_8_DD.t.i)
    
  # s024
    # s024 H.period
    sd(s024_session_1_H.period)
    sd(s024_session_2_H.period)
    sd(s024_session_3_H.period)
    sd(s024_session_4_H.period)
    sd(s024_session_5_H.period)
    sd(s024_session_6_H.period)
    sd(s024_session_7_H.period)
    sd(s024_session_8_H.period)
    
    # s024 DD.period.t
    sd(s024_session_1_DD.period.t)
    sd(s024_session_2_DD.period.t)
    sd(s024_session_3_DD.period.t)
    sd(s024_session_4_DD.period.t)
    sd(s024_session_5_DD.period.t)
    sd(s024_session_6_DD.period.t)
    sd(s024_session_7_DD.period.t)
    sd(s024_session_8_DD.period.t)
    
    # s024 UD.period.t
    sd(s024_session_1_UD.period.t)
    sd(s024_session_2_UD.period.t)
    sd(s024_session_3_UD.period.t)
    sd(s024_session_4_UD.period.t)
    sd(s024_session_5_UD.period.t)
    sd(s024_session_6_UD.period.t)
    sd(s024_session_7_UD.period.t)
    sd(s024_session_8_UD.period.t)
    
    # s024 H.t
    sd(s024_session_1_H.t)
    sd(s024_session_2_H.t)
    sd(s024_session_3_H.t)
    sd(s024_session_4_H.t)
    sd(s024_session_5_H.t)
    sd(s024_session_6_H.t)
    sd(s024_session_7_H.t)
    sd(s024_session_8_H.t)
    
    # s024 DD.t.i
    sd(s024_session_1_DD.t.i)
    sd(s024_session_2_DD.t.i)
    sd(s024_session_3_DD.t.i)
    sd(s024_session_4_DD.t.i)
    sd(s024_session_5_DD.t.i)
    sd(s024_session_6_DD.t.i)
    sd(s024_session_7_DD.t.i)
    sd(s024_session_8_DD.t.i)
    
  # s047
    # s047 H.period
    sd(s047_session_1_H.period)
    sd(s047_session_2_H.period)
    sd(s047_session_3_H.period)
    sd(s047_session_4_H.period)
    sd(s047_session_5_H.period)
    sd(s047_session_6_H.period)
    sd(s047_session_7_H.period)
    sd(s047_session_8_H.period)
    
    # s047 DD.period.t
    sd(s047_session_1_DD.period.t)
    sd(s047_session_2_DD.period.t)
    sd(s047_session_3_DD.period.t)
    sd(s047_session_4_DD.period.t)
    sd(s047_session_5_DD.period.t)
    sd(s047_session_6_DD.period.t)
    sd(s047_session_7_DD.period.t)
    sd(s047_session_8_DD.period.t)
    
    # s047 UD.period.t
    sd(s047_session_1_UD.period.t)
    sd(s047_session_2_UD.period.t)
    sd(s047_session_3_UD.period.t)
    sd(s047_session_4_UD.period.t)
    sd(s047_session_5_UD.period.t)
    sd(s047_session_6_UD.period.t)
    sd(s047_session_7_UD.period.t)
    sd(s047_session_8_UD.period.t)
    
    # s047 H.t
    sd(s047_session_1_H.t)
    sd(s047_session_2_H.t)
    sd(s047_session_3_H.t)
    sd(s047_session_4_H.t)
    sd(s047_session_5_H.t)
    sd(s047_session_6_H.t)
    sd(s047_session_7_H.t)
    sd(s047_session_8_H.t)
    
    # s047 DD.t.i
    sd(s047_session_1_DD.t.i)
    sd(s047_session_2_DD.t.i)
    sd(s047_session_3_DD.t.i)
    sd(s047_session_4_DD.t.i)
    sd(s047_session_5_DD.t.i)
    sd(s047_session_6_DD.t.i)
    sd(s047_session_7_DD.t.i)
    sd(s047_session_8_DD.t.i)
    
  # s053
    # s053 H.period
    sd(s053_session_1_H.period)
    sd(s053_session_2_H.period)
    sd(s053_session_3_H.period)
    sd(s053_session_4_H.period)
    sd(s053_session_5_H.period)
    sd(s053_session_6_H.period)
    sd(s053_session_7_H.period)
    sd(s053_session_8_H.period)
    
    # s053 DD.period.t
    sd(s053_session_1_DD.period.t)
    sd(s053_session_2_DD.period.t)
    sd(s053_session_3_DD.period.t)
    sd(s053_session_4_DD.period.t)
    sd(s053_session_5_DD.period.t)
    sd(s053_session_6_DD.period.t)
    sd(s053_session_7_DD.period.t)
    sd(s053_session_8_DD.period.t)
    
    # S053 UD.period.t
    sd(s053_session_1_UD.period.t)
    sd(s053_session_2_UD.period.t)
    sd(s053_session_3_UD.period.t)
    sd(s053_session_4_UD.period.t)
    sd(s053_session_5_UD.period.t)
    sd(s053_session_6_UD.period.t)
    sd(s053_session_7_UD.period.t)
    sd(s053_session_8_UD.period.t)
    
    # s053 H.t
    sd(s053_session_1_H.t)
    sd(s053_session_2_H.t)
    sd(s053_session_3_H.t)
    sd(s053_session_4_H.t)
    sd(s053_session_5_H.t)
    sd(s053_session_6_H.t)
    sd(s053_session_7_H.t)
    sd(s053_session_8_H.t)
    
    # s053 DD.t.i
    sd(s053_session_1_DD.t.i)
    sd(s053_session_2_DD.t.i)
    sd(s053_session_3_DD.t.i)
    sd(s053_session_4_DD.t.i)
    sd(s053_session_5_DD.t.i)
    sd(s053_session_6_DD.t.i)
    sd(s053_session_7_DD.t.i)
    sd(s053_session_8_DD.t.i)

  # s054
    # s054 H.period
    sd(s054_session_1_H.period)
    sd(s054_session_2_H.period)
    sd(s054_session_3_H.period)
    sd(s054_session_4_H.period)
    sd(s054_session_5_H.period)
    sd(s054_session_6_H.period)
    sd(s054_session_7_H.period)
    sd(s054_session_8_H.period)
    
    # s054 DD.period.t
    sd(s054_session_1_DD.period.t)
    sd(s054_session_2_DD.period.t)
    sd(s054_session_3_DD.period.t)
    sd(s054_session_4_DD.period.t)
    sd(s054_session_5_DD.period.t)
    sd(s054_session_6_DD.period.t)
    sd(s054_session_7_DD.period.t)
    sd(s054_session_8_DD.period.t)
    
    # s054 UD.period.t
    sd(s054_session_1_UD.period.t)
    sd(s054_session_2_UD.period.t)
    sd(s054_session_3_UD.period.t)
    sd(s054_session_4_UD.period.t)
    sd(s054_session_5_UD.period.t)
    sd(s054_session_6_UD.period.t)
    sd(s054_session_7_UD.period.t)
    sd(s054_session_8_UD.period.t)
    
    # s054 H.t
    sd(s054_session_1_H.t)
    sd(s054_session_2_H.t)
    sd(s054_session_3_H.t)
    sd(s054_session_4_H.t)
    sd(s054_session_5_H.t)
    sd(s054_session_6_H.t)
    sd(s054_session_7_H.t)
    sd(s054_session_8_H.t)
    
    # s054 DD.t.i
    sd(s054_session_1_DD.t.i)
    sd(s054_session_2_DD.t.i)
    sd(s054_session_3_DD.t.i)
    sd(s054_session_4_DD.t.i)
    sd(s054_session_5_DD.t.i)
    sd(s054_session_6_DD.t.i)
    sd(s054_session_7_DD.t.i)
    sd(s054_session_8_DD.t.i)
    
    
    
   
    
    
    
    
    
#graphs
    #mean of all sessions 
    s054_mean_of_all_sessions_DD.t.i
    s054_mean_of_all_sessions_H.period
    s054_mean_of_all_sessions_H.t
    s054_mean_of_all_sessions_UD.period.t
    s054_mean_of_all_sessions_DD.period.t
    
table(s054_mean_of_all_sessions_DD.t.i,
      s054_mean_of_all_sessions_H.period,
      s054_mean_of_all_sessions_H.t,
      s054_mean_of_all_sessions_UD.period.t,
      s054_mean_of_all_sessions_DD.period.t)

    
    
library("MASS")

means <- matrix(c(s054_mean_of_all_sessions_DD.t.i,
                     s054_mean_of_all_sessions_H.period,
                     s054_mean_of_all_sessions_H.t,
                     s054_mean_of_all_sessions_UD.period.t,
                     s054_mean_of_all_sessions_DD.period.t), nrow = 5)



prop.table(means)
#tables of proportions for the means 


#plots of the means:
plot(means)
hist(means)




#for the next part where we have to approximate it with a normal distribution,
# we chose to use the H.period variable.


mu<- mean(H.period)

stdev<-sd(H.period)

#the probability of it being less than 0.05
probab_less_0.05<- pnorm(0.05, mean=mu, sd=stdev) *100


plot(ecdf(H.period))
plot(density(H.period))

#Using the same keystroke variable, divide it into subjects. 
#Approximate this variable per subject with
#a Normal distribution. You may plot this if you want 
#(using appropriate titles, etc. This will count
                                                      
#towards your five plots if you choose.). Calculate the probability of this random variable being less
#than 0.05 for each subject (see me if this number is outside of the variable range.) Compare to the
#previous question results and discuss.



#checking to see if it has a normal distribution:
#If it has a bell shaped density plot and an s shaped ecdf graph, then it is a normal distribution.

for(i in 1:length(x$H.period)){
   if(subject[i] == "s010"){
      mu_s010<- mean(H.period)
      stdev_s010<- sd(H.period)
      plot(ecdf(H.period))
      plot(density(H.period))
   }
}
#probability of the value being less than 0.05:
probab_less_0.05<- pnorm(0.05, mean=mu_s010, sd=stdev_s010) *100


for (i in 1:length(H.period)) {
   if(subject[i] == "s024"){
      mu_s010<- mean(H.period)
      stdev_s010<- sd(H.period)
      plot(ecdf(H.period))
      plot(density(H.period))
   }
}
probab_less_0.05<- pnorm(0.05, mean=mu_s024, sd=stdev_s024) *100




for (i in 1:length(H.period)) {
   if(subject[i] == "s053"){
      mu_s053<- mean(H.period)
      stdev_s053<- sd(H.period)
      plot(ecdf(H.period))
      plot(density(H.period))
   }
}
probab_less_0.05<- pnorm(0.05, mean=mu_s053, sd=stdev_s053) *100





for (i in 1:length(H.period)) {
   if(subject[i] == "s054"){
      mu_s054<- mean(H.period)
      stdev_s054<- sd(H.period)
      plot(ecdf(H.period))
      plot(density(H.period))
   }
}

probab_less_0.05<- pnorm(0.05, mean=mu_s054, sd=stdev_s054) *100




