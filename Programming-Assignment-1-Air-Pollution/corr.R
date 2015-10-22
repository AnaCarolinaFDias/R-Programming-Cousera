corr <- function(directory, threshold = 0) {
          
	  files<- list.files(directory, full.names = TRUE) 
	  cr <- c()
for (id in 1:332) {

dat <- read.csv(files[id],header=TRUE)
	  completedata<-  na.omit(dat)

l<- nrow(completedata)  
	if (l >= threshold) {
	sulfate <- completedata[["sulfate"]]
      nitrate <- completedata[["nitrate"]]
	cr <- c(cr, cor(sulfate, nitrate))
				  }
			}
	return(cr)
}
cr <- corr("specdata", 150)
head(cr)
## [1] -0.01896 -0.14051 -0.04390 -0.06816 -0.12351 -0.07589

summary(cr)
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
## -0.2110 -0.0500  0.0946  0.1250  0.2680  0.7630

cr <- corr("specdata", 400)
head(cr)
## [1] -0.01896 -0.04390 -0.06816 -0.07589  0.76313 -0.15783

summary(cr)
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
## -0.1760 -0.0311  0.1000  0.1400  0.2680  0.7630
