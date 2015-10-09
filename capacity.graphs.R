 
# LOAD NEEDED LIBS+PACKAGAGES
# install.packages("ggplot2") #remember to uncomment at first run - this needs to automated. R (base?) can do that.
library(ggplot2)
library(scales)
library(plyr)
library(reshape2)
library(data.table)

effort.data <-
  read.delim(
    "~/r/rdata/effort_data-BAL15.txt", header = TRUE, fill = TRUE, stringsAsFactors = FALSE, dec = "."
  ) # get capacity data from STECF dataset

gear.list <-
  read.delim(
    "~/r/ices/gear.list", header = TRUE, sep = ",", stringsAsFactors = FALSE, fileEncoding = "UTF-8"
  )



effort.data$gears <- effort.data$Reg_gear
effort.data$area  <- as.character(effort.data$Reg_area) 
effort.data$capacity <- effort.data$Fishing_capacity_.GT.
effort.data$effort_kwd <- effort.data$Effort_.kW.days.

# correcting gears types;
# translate gear data codes to gear type names 
gname.apply <- c(gear.list$gear)
names(gname.apply) <- c(gear.list$data.code)
effort.data$gears.t <- gname.apply[effort.data$gears]

t.cap.data <- data.table(effort.data)
setkey(t.cap.data, Year)
s.year <- t.cap.data[,sum(capacity), by=Year]

