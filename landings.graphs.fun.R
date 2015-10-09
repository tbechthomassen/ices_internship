
# LOAD NEEDED LIBS+PACKAGES
# install.packages("ggplot2") # remember to uncomment at first run - this needs to automated. R (base?) can do that.
library(ggplot2)
library(scales)
library(plyr)
library(reshape2)

mainDir <- "~/landings.graphs"
subDir <- "outputDirectory"

if (file.exists(paste(mainDir, subDir, "/", sep = "/", collapse = "/"))) {
  cat("subDir exists in mainDir and is a directory")
} else if (file.exists(paste(mainDir, subDir, sep = "/", collapse = "/"))) {
  cat("subDir exists in mainDir but is a file")
  # you will probably want to handle this separately
} else {
  cat("subDir does not exist in mainDir - creating")
  dir.create(file.path(mainDir, subDir), recursive = TRUE)
}

if (file.exists(paste(mainDir, subDir, "/", sep = "/", collapse = "/"))) {
  # By this point, the directory either existed or has been successfully created
  setwd(file.path(mainDir, subDir))
} else {
  cat("subDir does not exist")
  # Handle this error as appropriate
}

land.data <-
  read.delim(
    "./rdata/landings_data-CEL.txt", header = TRUE, fill = TRUE, stringsAsFactors = FALSE, dec = ","
  ) # get land data from 

effort.data <-
  read.delim(
    "./rdata/effort_data-NORTHSEA.txt", header = TRUE, fill = TRUE, stringsAsFactors = FALSE, dec = "."
  )

species.name <-
  read.delim(
    "./rdata/3letterspecies_names.txt", header = TRUE, stringsAsFactors = FALSE
  ) # henter tabel over artsnavne og forkortelser af disse.
# en liste over de forskellige arters guild/gruppe

guildlist <-
  read.delim(
    "./rdata/guilds.txt", header = TRUE, sep = ",", stringsAsFactors = FALSE, fileEncoding = "UTF-8"
  )


gear.list <-
  read.delim(
    "./rdata/gear.list", header = TRUE, sep = ",", stringsAsFactors = FALSE, fileEncoding = "UTF-8"
  )


land.data$area <-
  as.character(land.data$reg_area_cod) # removing horrble area column names
land.data$gears <-
  as.character(land.data$reg_gear_cod) # ...and horrible gear column names

# tilføj lægnavn til land.data
cname.apply <- c(species.name$common_name)
names(cname.apply) <- c(species.name$species)
land.data$cnames <- cname.apply[land.data$species]

# tilføj videnskabeligt navn til land.data:
sname.apply <- c(species.name$scientific_name)
names(sname.apply) <- c(species.name$species)
land.data$snames <- sname.apply[land.data$species]

# tilføj guild til land.data
guild.apply <- c(guildlist$feeding.guild)
names(guild.apply) <- c(guildlist$scientific.name)
land.data$guild <- guild.apply[land.data$snames]

# correcting gears types;
# translate gear data codes to gear type names 
gname.apply <- c(gear.list$gear)
names(gname.apply) <- c(gear.list$data.code)
land.data$gears.t <- gname.apply[land.data$gears]


###################################################
############# Preparation for plots ###############
###################################################



# calculating percentages of effort:
# first, get annual data (should be automated later)

# summarise landings species / area

# sum2003 = sum(X2003, na.rm = TRUE),
# sum2004 = sum(X2004, na.rm = TRUE),
# sum2005 = sum(X2005, na.rm = TRUE),
# sum2006 = sum(X2006, na.rm = TRUE),
# sum2007 = sum(X2007, na.rm = TRUE),
# sum2008 = sum(X2008, na.rm = TRUE),
# sum2009 = sum(X2009, na.rm = TRUE),
# sum2010 = sum(X2010, na.rm = TRUE),
# sum2011 = sum(X2011, na.rm = TRUE),
# sum2012 = sum(X2012, na.rm = TRUE),
# sum2013 = sum(X2013, na.rm = TRUE)

dat.names <- c("snames", "gears.t", "area", "guild")

land.data.s <-
  ddply(
    land.data, dat.names, summarise,
    sum2003 = sum(X2003, na.rm = TRUE),
    sum2004 = sum(X2004, na.rm = TRUE),
    sum2005 = sum(X2005, na.rm = TRUE),
    sum2006 = sum(X2006, na.rm = TRUE),
    sum2007 = sum(X2007, na.rm = TRUE),
    sum2008 = sum(X2008, na.rm = TRUE),
    sum2009 = sum(X2009, na.rm = TRUE),
    sum2010 = sum(X2010, na.rm = TRUE),
    sum2011 = sum(X2011, na.rm = TRUE),
    sum2012 = sum(X2012, na.rm = TRUE),
    sum2013 = sum(X2013, na.rm = TRUE)
  )

land.data.m <-
  melt(land.data.s, id.vars = dat.names, measure.vars = c(grep("20", land.data.s)))

missing.guild <- subset(land.data.m, is.na(guild), select=c(snames,variable))

s.missing.guild <- ddply(missing.guild, "snames", summarise, "snames")


###################################################################################
######## graph'd below is annual comparison of landings per guild and gears #######
###################################################################################

#--- barplot; landings per guild, summed subregions, no division:
guild.sub.plot <-
  ggplot(data = land.data.m, aes(x = variable, y = value)) #adding data to plot
# guild.sub.plot <- + facet_wrap(~variable)
guild.sub.plot <-
  guild.sub.plot + geom_bar(
    stat = "identity", aes(fill = guild), colour =
      "black", position = "dodge"
  ) # adding bar-shape and colours + outlines
guild.sub.plot <-
  guild.sub.plot + xlab("Guilds") + ylab("% of landings") #Add X- & Y-axis labels
guild.sub.plot <-
  guild.sub.plot + ggtitle("landings per guild, Celtic Sea, all subregions, 2003-2013") # setting the title
guild.sub.plot <-
  guild.sub.plot + theme(axis.text.x = element_text(angle = 90))
guild.sub.plot #show the g'damn plot!


#--- barplot; landings per guild, all subregions, divided per subregion:
guild.sub.plotf <-
  ggplot(data = guild.area.melt, aes(x = variable, y = value)) #adding data to plot
# guild2003_2013sub.plot <- guild2003_2013sub.plot +
guild.sub.plotf <-
  guild.sub.plotf + geom_bar(
    stat = "identity", aes(fill = guild), colour =
      "black", position = "dodge"
  )# + scale_y_continuous(formatter='log10') # adding bar-shape and colours + outlines
guild.sub.plotf <-
  guild.sub.plotf + facet_grid(area ~ .)
guild.sub.plotf <-
  guild.sub.plotf + xlab("Year") + ylab("% of landings") #Add X- & Y-axis labels
guild.sub.plotf <-
  guild.sub.plotf + ggtitle("landings per guild, Celtic Sea, per subregion, 2003-2013") # setting the title
guild.sub.plotf <-
  guild.sub.plotf + theme(axis.text.x = element_text(angle = 90))
guild.sub.plotf #show the g'damn plot!

#----------- graph'd below is annual comparison of landings per gear #############

#--- barplot; landings per gear, summed subregions, no division:
gear.sub.p <-
  ggplot(data = gear.area.m, aes(x = variable, y = value)) #adding data to plot
gear.sub.p <-
  gear.sub.p + geom_bar(
    stat = "identity", aes(fill = gears.t), colour =
      "black", position = "dodge"
  )# + scale_y_continuous(formatter='log10') # adding bar-shape and colours + outlines
gear.sub.p <-
  gear.sub.p + xlab("Year") + ylab("% of landings") #Add X- & Y-axis labels
gear.sub.p <-
  gear.sub.p + ggtitle("landings per gear, Celtic Sea, per subregion, 2003-2013") # setting the title
gear.sub.p <-
  gear.sub.p + theme(axis.text.x = element_text(angle = 90))
gear.sub.p #show the g'damn plot!

#--- barplot; landings per gear, all subregions, divided per subregion:
gear.sub.pf <-
  ggplot(data = gear.area.m, aes(x = variable, y = value)) #adding data to plot
gear.sub.p <-
  gear.sub.p + geom_bar(
    stat = "identity", aes(fill = gears.t), colour =
      "black", position = "dodge"
  )
gear.sub.p <-
  gear.sub.p + facet_grid(area ~ .)
gear.sub.p <-
  gear.sub.p + xlab("Year") + ylab("% of landings") #Add X- & Y-axis labels
gear.sub.p <-
  gear.sub.p + ggtitle("landings per gear, Celtic Sea, per subregion, 2003-2013") # setting the title
gear.sub.p <-
  gear.sub.p + theme(axis.text.x = element_text(angle = 90))
gear.sub.p #show the g'damn plot!




###########################################################################################
###########################################################################################
###########################################################################################
############# WARNING! EXPERIMENTAL PLOTS AHEAD - MAY NOT MAKE *ANY* SENSE! ###############
###########################################################################################
##################################### HERE BE DRAGONS #####################################
###########################################################################################

# #### barplot; landings per species, summed subregions, no division:
# species.sub.p <-
#   ggplot(data = species.area.m, aes(x = variable, y = value)) #adding data to plot
# species.sub.p <-
#   species.sub.p + geom_bar(
#     stat = "identity", aes(fill = snames), colour =
#       "black", position = "dodge"
#   )# + scale_y_continuous(formatter='log10') # adding bar-shape and colours + outlines
# species.sub.p <-
#   species.sub.p + xlab("Year") + ylab("% of landings") #Add X- & Y-axis labels
# species.sub.p <-
#   species.sub.p + ggtitle("landings per species, North Sea, all subregions, 2003-2013") # setting the title
# species.sub.p <-
#   species.sub.p + theme(axis.text.x = element_text(angle = 90))
# species.sub.p #show the g'damn plot!
# 
# #### barplot; landings per species, all subregions, divided per subregion:
# species.sub.pf <-
#   ggplot(data = species.area.m, aes(x = variable, y = value)) #adding data to plot
# species.sub.pf <-
#   species.sub.pf + geom_bar(
#     stat = "identity", aes(fill = snames), colour =
#       "black", position = "dodge"
#   )# + scale_y_continuous(formatter='log10') # adding bar-shape and colours + outlines
# species.sub.pf <-
#   species.sub.pf + facet_grid(area ~ .)
# species.sub.pf <-
#   species.sub.pf + xlab("Year") + ylab("% of landings") #Add X- & Y-axis labels
# species.sub.pf <-
#   species.sub.pf + ggtitle("landings per species, North Sea, per subregion, 2003-2013") # setting the title
# species.sub.pf <-
#   species.sub.pf + theme(axis.text.x = element_text(angle = 90))
# species.sub.pf #show the g'damn plot!

# lolplot <-
#   ggplot(data = guild.area.melt, aes(x = variable, y = value)) #adding data to plot
# #lolplot <- lolplot + 
# lolplot <-
#   lolplot + geom_bar(
#     stat = "identity", aes(fill = area), colour =
#       "black", position = "dodge")# + scale_y_continuous(formatter='log10') # adding bar-shape and colours + outlines
# lolplot <-
#   lolplot +facet_grid(guild ~ .)
# lolplot <-
#   lolplot + xlab("Year") + ylab("% of landings") #Add X- & Y-axis labels
# lolplot <-
#   lolplot + ggtitle("landings per guild, BALTIC, all subregions, 2003-2013") # setting the title
# lolplot <-
#   lolplot + theme(axis.text.x = element_text(angle = 90))
# #  sum2003plot <- sum2003plot + scale_y_continuous(labels = percent)
# lolplot #show the g'damn plot!
