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

# list raw data files
f <- list.files(path = "csvRaw", full.names = T)

# file with timestamp offsets for timezone issues
to <- read.csv("time_offset.csv", header = T)

# date format for data
df <- "%m/%d/%y %R"

# timezone for data
z <- "Asia/Srednekolymsk"


# get unique site names
sn <- unique(sapply(strsplit(list.files(path = "csvRaw", pattern = ".csv"),"_"),"[[",1))

#------------------------------------------------------------------#
# FIRST Process Data from csvRaw/dav_usmet_201705181655.csv
# this file has two different time offsets due to timezone shifts
# between the data logger and field computer
#------------------------------------------------------------------#
i <- which(f =="csvRaw/dav_usmet_201705181655.csv")

# read header info and data separately
h <- read.csv(f[i], nrows = 3,header = F)
d <- read.csv(f[i], na.strings = "#N/A",skip = 3, header = F)

# format time stamp info
ts <- strptime(d[,1], format = df, tz = z)

#index before and after time stamp error shift
r <- which(ts > as.POSIXct("2016-07-02 19:30"))
s <- which(ts < as.POSIXct("2016-07-02 20:00"))

# correct timestamp based on index (15 hrs before, 9 hrs after)
ts[s] <- ts[s]+(15*60*60)
ts[r] <- ts[r]+(-9*60*60)

# create a long data frame to hold the data
dat <- data.frame(ts,h[1,1],h[1,2],h[2,2],NA,h[3,2],d[,2])
names(dat) <- c("timestamp","logger", "port", "sensor", "sensorZ","unit", "measurement")

# loop through each column and add to the data frame
for(i in 3:ncol(d))
{
  td <- data.frame(ts,h[1,1],h[1,i],h[2,i],NA,h[3,i],d[,i])
  names(td) <- names(dat)
  dat <- rbind(dat,td)
}

rm(i,r,s)
#------------------------------------------------------------------#
# now add in the rest of the data
#------------------------------------------------------------------#
# create a sequence without the file processed above
r <- which(f !="csvRaw/dav_usmet_201705181655.csv")

for(i in r)
{
  # read header info and data separately
  h <- read.csv(f[i], nrows = 3,header = F)
  d <- read.csv(f[i], na.strings = "#N/A",skip = 3, header = F)
  
  # format time stamp info
  ts <- strptime(d[,1], format = df, tz = z) 
  
  # correct timestamp based on offset info
  # offset is in hours multiply by 60 min x 60 sec and add to ts
  ts <- ts+to$offset[match(f[i],to$filename)]*60*60

  # add each column to the data frame
  for(j in 2:ncol(d))
  {
    td <- data.frame(ts,h[1,1],h[1,j],h[2,j],NA,h[3,j],d[,j])
    names(td) <- names(dat)
    dat <- rbind(dat,td) 
  }
}

#------------------------------------------------------------------#
# set sensorZ values
#------------------------------------------------------------------#
# set sensorZ values (positive/negative for above/below ground)

# Port 3 is soil sensor at Organic/Mineral interface 
# Port 4 is soil sensor initially placed 5cm below O/M interface but then moved down 5cm on date specified

# Ports 1 & 2 are PAR and Airtemp/RH at 1m above ground
dat$sensorZ <- ifelse(dat$port == "Port 1"|dat$port == "Port 2", 1, dat$sensorZ) 

# Port 3 is soil sensor at Organic/Mineral interface 
dat$sensorZ <- ifelse(dat$logger == "DavUSmet" & dat$port == "Port 3", -10, dat$sensorZ)
dat$sensorZ <- ifelse(dat$logger == "HDF1met" & dat$port == "Port 3", -9, dat$sensorZ)
#dat$sensorZ <- ifelse(dat$logger == "LBRmet" & dat$port == "Port 3", -18, dat$sensorZ)
dat$sensorZ <- ifelse(dat$logger == "LDF2met" & dat$port == "Port 3", -8, dat$sensorZ)
dat$sensorZ <- ifelse(dat$logger == "MDF1met" & dat$port == "Port 3", -6, dat$sensorZ)
dat$sensorZ <- ifelse(dat$logger == "MDF2met" & dat$port == "Port 3", -9, dat$sensorZ)

# MDF1
r <- which(dat$logger =="MDF1met" & dat$port == "Port 4" & dat$timestamp < as.POSIXct("2015-09-03 16:30"))
dat$sensorZ[r] <- -11
r <- which(dat$logger =="MDF1met" & dat$port == "Port 4" & dat$timestamp > as.POSIXct("2015-09-03 16:00"))
dat$sensorZ[r] <- -16

# MDF2
r <- which(dat$logger =="MDF2met" & dat$port == "Port 4" & dat$timestamp < as.POSIXct("2015-09-03 13:00"))
dat$sensorZ[r] <- -14
r <- which(dat$logger =="MDF2met" & dat$port == "Port 4" & dat$timestamp > as.POSIXct("2015-09-03 12:30"))
dat$sensorZ[r] <- -19

# LDF2
r <- which(dat$logger =="LDF2met" & dat$port == "Port 4" & dat$timestamp < as.POSIXct("2015-09-03 19:30"))
dat$sensorZ[r] <- -13
r <- which(dat$logger =="LDF2met" & dat$port == "Port 4" & dat$timestamp > as.POSIXct("2015-09-03 19:00"))
dat$sensorZ[r] <- -18

# HDF1
r <- which(dat$logger =="HDF1met" & dat$port == "Port 4" & dat$timestamp < as.POSIXct("2015-09-04 19:00"))
dat$sensorZ[r] <- -14
r <- which(dat$logger =="HDF1met" & dat$port == "Port 4" & dat$timestamp > as.POSIXct("2015-09-04 18:30"))
dat$sensorZ[r] <- -19

# DAV
r <- which(dat$logger =="DavUSmet" & dat$port == "Port 4" & dat$timestamp < as.POSIXct("2015-09-04 17:00"))
dat$sensorZ[r] <- -15
r <- which(dat$logger =="DavUSmet" & dat$port == "Port 4" & dat$timestamp > as.POSIXct("2015-09-04 16:30"))
dat$sensorZ[r] <- -20


# LBR  
#note here that ports 3 & 4 were reversed - the installation was poor as well,
# so until this date Port 4 was the O/M sensor, but contact was poor and data were bad
r <- which(dat$logger =="LBRmet" & dat$port == "Port 4" & dat$timestamp < as.POSIXct("2015-09-03 17:00"))
dat$sensorZ[r] <- -18
r <- which(dat$logger =="LBRmet" & dat$port == "Port 4" & dat$timestamp > as.POSIXct("2015-09-03 16:30"))
dat$sensorZ[r] <- -28
r <- which(dat$logger =="LBRmet" & dat$port == "Port 3" & dat$timestamp < as.POSIXct("2015-09-03 17:00"))
dat$sensorZ[r] <- -23
r <- which(dat$logger =="LBRmet" & dat$port == "Port 3" & dat$timestamp > as.POSIXct("2015-09-03 16:30"))
dat$sensorZ[r] <- -18

# LBR 18cm data are no good before 2015-08-20 13:00


#------------------------------------------------------------------#
# now clean data and write files for archiving
#------------------------------------------------------------------#

# also, for the 2016/17 file there was a double timestamp issue before 7/2/16 20:00 add 15 hrs to the timestamp
# after that date/time subtract 9
