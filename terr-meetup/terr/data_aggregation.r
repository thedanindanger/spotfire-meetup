url <- c("https://www.quandl.com/api/v1/datasets/DOE/MCRFPUT1.csv?trim_end=2008-12-30"
         ,"https://www.quandl.com/api/v1/datasets/DOE/MCRFPOK1.csv?trim_end=2008-12-30"
         ,"https://www.quandl.com/api/v1/datasets/DOE/MCRFPCA1.csv?trim_end=2008-12-30")

data_master <- data.frame(Date=character(),
                          Value=numeric(),
                          File=character(),
                          Date_Order=numeric()
)
for (i in 1: length(url)){
  
  data <- read.csv(url[i])
  #Force date format
  data$Date <- as.character(data$Date)
  
  #find the dataset name in the url
  
  expr <- regexpr("\\/[a-zA-Z0-9_]+\\.csv", 
                  url[i], perl=TRUE
  )
  
  file_source <- regmatches(url[i], expr)
  file <- substr(file_source[[1]],2,nchar(file_source)-4)
  
  #adds file name to dattaframe
  data$File <- rep(file, nrow(data))
  
  #normalize data from max output date
  #executing in the loop to prevent complex windowing functions
  data <- data[1:which.max(data$Value),]
  
  #Data is ordered descending from source
  #Rank our data for side by side comparision
  data$Date_Order <- rank(data$Date)
  data_master <- rbind(data_master, data)
}

dataMaster <- data_master

