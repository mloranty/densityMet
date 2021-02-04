#######################################
# data cleaning and QA/QC 
# for understory micrometeorology data
# across a forest density gradient 
# near Cherksiy, Russia
#
# MML 02/01/21
#######################################

library(lubridate)
library(dplyr)

setwd("L:/data_repo/field_data/densityMet/")

# get unique site names
sn <- unique(sapply(strsplit(list.files(path = "csvRaw", pattern = ".csv"),"_"),"[[",1))

#list all filenames
f <- list.files(path = "csvRaw", full.names = T)

#------------------------------------------------------------------#
# get list of start/end timestamps for all files
t <- read.csv(f[1], na.strings = "#N/A",skip = 3, header = F)
ts <- c(f[1],t[1,1],t[nrow(t),1])

for(i in 2:length(f))
{
  t <- read.csv(f[i], na.strings = "#N/A",skip = 3, header = F)
  ts <- rbind(ts,c(f[i],t[1,1],t[nrow(t),1]))
}

ts[,1] <- sub("csvRaw/","" ,ts[,1])
ts[,1] <- sub(".csv","" ,ts[,1])

colnames(ts) <- c("filename", "Start", "End")
write.csv(ts,file = "logger_file_timestamps.csv",row.names = F)
#------------------------------------------------------------------#


# process data from mdf1
f <- list.files(path = "csvRaw", pattern = "mdf1", full.names = T)

h <- read.csv(f[1], nrows = 3,header = F)
d <- read.csv(f[1], na.strings = "#N/A",skip = 3, header = F)

d[,1] <- strptime(d[,1], format = "%m/%d/%Y %R %p")

par <- data.frame()

for(i in 1:length(f))
{
  h <- read.csv(f[i], nrows = 3,header = F)
  d <- read.csv(f[i], na.strings = "#N/A",skip = 3, header = F)
  
  d[,i] <- strptime(d[,i], format = "%m/%d/%Y %R %p")
}




