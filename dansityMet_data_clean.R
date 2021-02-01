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


# process data from mdf1
f <- list.files(path = "csvRaw", pattern = "mdf1", full.names = T)

for(i in 1:length(f))
{
  d <- read.csv(f[i], na.strings = "#N/A")
}




