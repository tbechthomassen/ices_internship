#------------> Header <----------
# Purpose: producing graphs for ICES fisheries advice outline Celtic Sea 2015
# Author: Thomas Bech-Thomassen
# Modded by:
# Date created: 28-10-15
# Date modded: 28-10-15
#------------> 

#------------> LOAD NEEDED LIBS+PACKAGAGES 
install.packages("ggplot2") #remember to uncomment at first run - this needs to be automated. R (base?) can do that.
library(ggplot2)
library(scales)
library(plyr)
library(reshape2)
library(foreach)
# library(data.table)
#------------|  

#------------> Build functions <----------

#------------|

#------------> Load data <----------
econ.data.all <-
  read.delim(
    "~/r/rdata/stecf-economic-fleet.data", header = TRUE, fill = TRUE, stringsAsFactors = FALSE, dec = "."
  ) # get economic data from STECF dataset
as.numeric(econ.data.all$value)
gear.list <-
  read.delim(
    "~/r/ices/gear.list", header = TRUE, sep = ",", stringsAsFactors = FALSE, fileEncoding = "UTF-8"
  )

reg.list <-
  read.delim(
    "~/r/ices/keys-lists/ices.regions.list", header = TRUE, sep = ",", stringsAsFactors = FALSE, fileEncoding = "UTF-8"
  )

guild.list <-
  read.delim(
    "~/r/ices/keys-lists/guild.list", header = TRUE, sep = ",", stringsAsFactors = FALSE, fileEncoding = "UTF-8"
  )

species.list <-
  read.csv(
    "~/r/ices/keys-lists/species.list", header = TRUE, stringsAsFactors = FALSE
  )

#------------> 


#------------> Execute functions <----------

# create nice functions that iterate over at list of the ecoregions, and create datasets for the graphs, for each ecoregion

# then create nice functions that iterate over the ecoregion-list, and builds the graphs for each ecoregion.

#------------> 


#------------> Build graphs <----------

#------------> 
