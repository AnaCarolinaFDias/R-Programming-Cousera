pollutantmean <- function(directory, pollutant, id = 1:332) {

  files<- list.files(directory, full.names = TRUE)
  
  dat <- data.frame()
  
  for (i in id) {
      dat <- rbind(dat, read.csv(files[i]))
  }
  if (pollutant == 'sulfate') {
    mean(dat$sulfate, na.rm = TRUE)  #identifies the median sulfate 
                                     #while stripping out the NAs
    
  } 
    else if (pollutant == 'nitrate') {
    mean(dat$nitrate, na.rm = TRUE)
  }
}
pollutantmean("specdata", "sulfate", 1:10)
## [1] 4.064
pollutantmean("specdata", "nitrate", 70:72)
## [1] 1.706
pollutantmean("specdata", "nitrate", 23)
## [1] 1.281
