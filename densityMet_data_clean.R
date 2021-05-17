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

# READ ME
#------------------------------------------------------------------#
# Raw files downloaded from Decagon EM50 Data loggers in xls format
# Prior to cleaning files are saved to CSV format
# the Timestamp column is converted to MM/DD/YY H:M 'Date' format in excel
# where H is hours (0-23) and M is minutes as decimal numbers
#------------------------------------------------------------------#


# get unique site names
sn <- unique(sapply(strsplit(list.files(path = "csvRaw", pattern = ".csv"),"_"),"[[",1))

#list all filenames
f <- list.files(path = "csvRaw", full.names = T)


# get list of start/end timestamps for all files
#------------------------------------------------------------------#
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

# get logger and sensor metadata for each file
t <- read.csv(f[1], na.strings = "#N/A",nrows = 3, header = F)
si <- cbind(f[1],t[1,1],t(t[,-1]))

for(i in 2:length(f))
{
  t <- read.csv(f[i], na.strings = "#N/A",nrows = 3, header = F)
  si <- rbind(si,cbind(f[i],t[1,1],t(t[,-1])))
}

# PROCESS DATA FROM SITE MDF1
#------------------------------------------------------------------#
f <- list.files(path = "csvRaw", pattern = "mdf1", full.names = T)

# read header info and data separately
h <- read.csv(f[1], nrows = 3,header = F)
d <- read.csv(f[1], na.strings = "#N/A",skip = 3, header = F)

# format time stamp info
ts <- strptime(d[,1], format = "%m/%d/%y %R")

# create create a long data frame
dat <- data.frame(ts,h[1,1],h[1,2],h[2,2],NA,h[3,2],d[,2])
names(dat) <- c("timestamp","logger", "port", "sensor", "sensorZ","unit", "measurement")

# loop through each column and add to the data frame
for(i in 3:ncol(d))
{
  td <- data.frame(ts,h[1,1],h[1,i],h[2,i],NA,h[3,i],d[,i])
  names(td) <- names(dat)
  dat <- rbind(dat,td)
}

# now do the same for each file
for(i in 2:length(f))
{
  # read header info and data separately
  h <- read.csv(f[i], nrows = 3,header = F)
  d <- read.csv(f[i], na.strings = "#N/A",skip = 3, header = F)
  
  # format time stamp info
  ts <- strptime(d[,1], format = "%m/%d/%y %R") 
  
  for(j in 2:ncol(d))
  {
    td <- data.frame(ts,h[1,1],h[1,i],h[2,i],NA,h[3,i],d[,i])
    names(td) <- names(dat)
    dat <- rbind(dat,td) 
  }
}

# set sensorZ values (positive/negative for above/below ground)
# Ports 1 & 2 are PAR and Airtemp/RH at 1m above ground
# Port 3 is soil sensor at Organic/Mineral interface -6cm
# Port 4 is soil sensor initially placed -11cm below surface but then moved to -16cm on date specified

dat$sensorZ <- ifelse(dat$port == "Port 1"|dat$port == "Port 2", 1, 
                      ifelse(dat$port == "Port 3", -6, 
                             ifelse(dat$port == "Port 4" & dat$timestamp < as.POSIXct("2015-09-03 16:30"), -11, -16))) 
#------------------------------------------------------------------#

# MDF2 ifelse(dat$port == "Port 4" & dat$timestamp < as.POSIXct("2015-09-03 13:00"), -14, -19)
# LDF2 ifelse(dat$port == "Port 4" & dat$timestamp < as.POSIXct("2015-09-03 19:30"), -13, -18)

# LBR ifelse(dat$port == "Port 4" & dat$timestamp < as.POSIXct("2015-09-03 17:00"), -23, -28) ## 
#note here that ports 3 & 4 may were switched - the installation was poor as well,
# so until this date Port 4 was the O/M sensor, but contact was poor and data were bad

# HDF1 ifelse(dat$port == "Port 4" & dat$timestamp < as.POSIXct("2015-09-03 19:00"), -14, -19)
# DAV ifelse(dat$port == "Port 4" & dat$timestamp < as.POSIXct("2015-09-04 17:00"), -15, -20)
# 
