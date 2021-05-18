#######################################
# metadata prep for  
# understory micrometeorology data
# across a forest density gradient 
# near Cherksiy, Russia
#
# MML 02/01/21
#######################################

# READ ME
#------------------------------------------------------------------#
# Note over the years we have had some timezone/timestamp issues
# due to computer clocks not updating, etc...
# here we strip the file names and start/end times in order to 
# have an easier way to keep track of the issue
#------------------------------------------------------------------#

setwd("L:/data_repo/field_data/densityMet/")


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
# t <- read.csv(f[1], na.strings = "#N/A",nrows = 3, header = F)
# si <- cbind(f[1],t[1,1],t(t[,-1]))
# 
# for(i in 2:length(f))
# {
#   t <- read.csv(f[i], na.strings = "#N/A",nrows = 3, header = F)
#   si <- rbind(si,cbind(f[i],t[1,1],t(t[,-1])))
# }