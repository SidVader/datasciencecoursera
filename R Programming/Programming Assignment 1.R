pollutantmean <- function(directory, pollutant, id = 1:332){
  setwd(directory)
  files_list <- list.files(full.names = TRUE)
  dat <- data.frame()
  for (i in id) {
    dat <- rbind(dat, read.csv(files_list[i]))
  }
  mean(dat[,pollutant], na.rm = TRUE)
}

complete <- function(directory, id = 1:332){
  setwd(directory)
  files_list <- list.files(full.names = TRUE)
  nobs <- vector()
  for (i in id){
    dat <- read.csv(files_list[i])
    nobs <- c(nobs, sum(!is.na(dat$sulfate) & !is.na(dat$nitrate)))
  }
  data.frame(id = id, nobs = nobs)
}

corr <- function(directory, threshold = 0){
  setwd(directory)
  files_list <- list.files(full.names = TRUE)
  cor_vector <- vector()
  for (i in 1:332){
    data <- read.csv(files_list[i])
    log_test <- !is.na(data[,"sulfate"]) & !is.na(data[,"nitrate"])
    comp_test <- sum(log_test)
    x <- data[,"sulfate"]
    y <- data[,"nitrate"]
    if(comp_test >=threshold){
      cor_vector <- c(cor_vector, cor(x[log_test],y[log_test]))
    }
  }
  cor_vector
}
