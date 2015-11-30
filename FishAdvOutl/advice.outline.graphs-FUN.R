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
#------------||  

#------------> Build functions <----------

#------------||

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

#------------||

#------------> Subset data <----------

# 1- add ecoregion designations, or maybe just subset to 27_7. This should work fine.

# 2- subset data to CEL
# 2a- BE AWARE of ICES ds aggregation rules. For CEL, select only data in region "27_7", to avoid duplication of data.

# 3- remove general dataset

#-------> Fig 2: STECF, effort (kWd)/country

# 1- 

#----|

#-------> Fig 3: ICES, top 3 guilds + rest as one group

# 1- add guild data

# 2- subset data to include guilds and landingsdata

# 3- reorganise to top3 + rest colnames: "species", 

## code idea:
# top3 <- GuildSummary[, [1:3]]
# others <- GuildSummary[, [4:length(GuildSummary)]]

#----|

#-------> Fig 4: top 3-5 species per guild (facet'd by guild)

#----|

#-------> Fig 5:

#----|

#------------||

#------------> Build graphs <----------
#
#
# 
#------------|| 


#------------> Execute functions <----------

# create nice functions that iterate over a list of the ecoregions, seperate to ecoregions
## 1- get list of ecoregions
## 2- subset whole dataset to subregions (modify code above)

# then create datasets for the graphs, for each ecoregion
## 1- get the relevant data for each graph
## 2- iterate over list for each ecoregion
##(use code above)

# then create nice functions that iterate over the ecoregion-list, and builds the graphs for each ecoregion.
## 1- design graphs, based on datasets
## 2- do this for each ecoregion-dataset
##

#------------||
