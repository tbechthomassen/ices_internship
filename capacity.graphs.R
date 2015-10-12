 
# LOAD NEEDED LIBS+PACKAGAGES
# install.packages("ggplot2") #remember to uncomment at first run - this needs to automated. R (base?) can do that.
library(ggplot2)
library(scales)
library(plyr)
library(reshape2)
# library(data.table)

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
effort.data$capacityGT <- effort.data$Fishing_capacity_.GT.
effort.data$effortkwd <- effort.data$Effort_.kW.days.
effort.data$years <- as.factor(effort.data$Year)
# correcting gears types;
# translate gear data codes to gear type names 
gname.apply <- c(gear.list$gear)
names(gname.apply) <- c(gear.list$data.code)
effort.data$gears.t <- gname.apply[effort.data$gears]


s.capacity.effort <-
  ddply(
    effort.data, c("area", "gears.t", "years", "Vessel_length", "Country"), summarise,
    capacity.sum = sum(capacityGT, na.rm = TRUE),
    effort.sum = sum(effortkwd, na.rm = TRUE)
  )

m.capacity.effort <-
  melt(
    s.capacity.effort, na.rm = TRUE, id.vars = c("area", "years"), measure.vars = c("capacity.sum", "effort.sum")
  )

#### barplot; landings per guild, summed subregions, no division:
p.effort.capacity <-
  ggplot(data = s.capacity.effort, aes(x = years, y = capacity.sum)) #adding data to plot
p.effort.capacity <-
  p.effort.capacity + geom_bar(
    stat = "identity", aes(fill = gears.t), colour =
      "black", position = "dodge"
  ) # adding bar-shape and colours + outlines
p.effort.capacity <- p.effort.capacity + facet_wrap(~area)
p.effort.capacity <-
  p.effort.capacity + xlab("Years") + ylab("Fleet capacity (GT)") #Add X- & Y-axis labels
p.effort.capacity <-
  p.effort.capacity + ggtitle("Fleet capacity and Effort per gear type and subregion, all years") # setting the title
p.effort.capacity <-
  p.effort.capacity + theme(axis.text.x = element_text(angle = 90))
p.effort.capacity #show the g'damn plot!

#### barplot; landings per guild, summed subregions, no division:
pm.effort.capacity <-
  ggplot(data = m.capacity.effort, aes(x = years, y = value)) #adding data to plot
pm.effort.capacity <-
  pm.effort.capacity + geom_bar(
    stat = "identity", aes(fill = area), colour =
      "black", position = "dodge"
  ) # adding bar-shape and colours + outlines
pm.effort.capacity <- pm.effort.capacity + facet_wrap(~variable)
pm.effort.capacity <-
  pm.effort.capacity + xlab("Years") + ylab("Fleet capacity (GT)") #Add X- & Y-axis labels
pm.effort.capacity <-
  pm.effort.capacity + ggtitle("Fleet capacity and Effort per gear type and subregion, all years") # setting the title
pm.effort.capacity <-
  pm.effort.capacity + theme(axis.text.x = element_text(angle = 90))
pm.effort.capacity #show the g'damn plot!











