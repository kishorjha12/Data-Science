
processT <- function(df){
  
  data <- df
  data <- data[,-1]
  #As per features description some of attributes are categorical
  data$land <- as.factor(data$land)
  data$logged_in <- as.factor(data$logged_in)
  data$root_shell <- as.factor(data$root_shell)
  data$su_attempted <- as.factor(data$su_attempted)
  data$is_guest_login <- as.factor(data$is_guest_login)
  
  #is_hot_login attribute has only one factor so it is unusefull.
  data$is_hot_login <- NULL
  
  #Replace missing value through kNN imputation
  library(VIM)
  data1 <- kNN(data, variable=c("duration","urgent", "hot", "num_failed_logins", "logged_in", 
                                "num_compromised", "root_shell", "su_attempted", "num_root", 
                                "num_file_creations", "num_shells", "num_access_files", 
                                "is_guest_login", "rerror_rate", "srv_rerror_rate", "diff_srv_rate",
                                "srv_diff_host_rate", "dst_host_count", "dst_host_srv_diff_host_rate",
                                "dst_host_rerror_rate", "dst_host_srv_rerror_rate", "serror_rate"))
  data <- subset(data1, select= duration:serror_rate )
  
  #return the dataset
  data
  
  
}





