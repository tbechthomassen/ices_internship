rm(list = ls())
###############
# Script Info #
###############
# PURPOSE: STECF data download & subsetting
# AUTHOR: Scott Large 2015
# REVIEWED/EXTENDED BY: Thomas Bech-Thomassen
#
############
# PACKAGES #
############
#
needList <- c("openxlsx")
new.packages <-
  needList[!(needList %in% installed.packages()[,"Package"])]
if (length(new.packages)) {
  install.packages(new.packages)
}
#
library(openxlsx)
#
###############
# Define dirs #
###############

cust.tmpd <-
  "~/r/rdata/download" # define where the zipped data files are placed

# check if dir for temp files exists, if not create it:
if (!dir.exists(cust.tmpd)) {
  dir.create(cust.tmpd, recursive = TRUE)
}


##################
# Load functions #
##################
#
#
#

#############
# Load data #
#############
#
effort.url <-
  "http://stecf.jrc.ec.europa.eu/documents/43805/870977/2014_STECF+14-20+-+Fishing+Effort+Regimes+data+tables.zip"
effort.f <- "~/r/rdata/download/effort.zip"
effort.df.path <- "~/r/rdata/stecf-landings.by.ICES.rectangle"

if (!file.exists(effort.df.path)) {
  if (!file.exists(effort.f)) {
    download.file(effort.url, destfile = effort.f, mode = "wb")
  }
  
  unzip(effort.f, list = T)
  effort.data <-
    read.xlsx(
      unzip(
        effort.f, "Electronic_appendices/Landings_by_ICES_rectangle.xlsx"
      ),
      sheet = 1
    )
  head(effort.data)
  write.table(
    effort.data, effort.df.path, sep = "\t", quote = FALSE, row.names = FALSE
  )
}


econ.url <-
  "http://stecf.jrc.ec.europa.eu/documents/43805/1034590/2015_STECF+15-07+-+EU+Fleet+Economic+data+tables.zip"
econ.f <- "~/r/rdata/download/econ.data.zip"
econ.df.path <- "~/r/rdata/stecf-economic-fleet.data"

if (!file.exists(econ.df.path)) {
  if (!file.exists(econ.f)) {
    download.file(econ.url, destfile = econ.f, mode = "wb")
  }
  
  unzip(econ.f, list = T)
  econ.data <-
    read.xlsx(
      unzip(
        econ.f, "STECF 15-07_2015_EU Fleet Economic and Transversal data_fleet segment level.xlsx"
      ),
      sheet = 2
    )
  
  head(econ.data)
  
  write.table(
    econ.data, econ.df.path, sep = "\t", quote = FALSE, row.names = FALSE
  )
}



