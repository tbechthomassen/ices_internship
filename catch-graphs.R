
#LOAD NEEDED LIBS+PACKAGAGES
#install.packages("ggplot2") #remember to uncomment at first run - this needs to automated. R (base?) can do that.
library(ggplot2)
library(scales)
library(plyr)
library(reshape2)

catch.data <-
  read.delim(
    "./rdata/landings_data-CEL.txt", header = TRUE, fill = TRUE, stringsAsFactors = FALSE, dec = ","
  ) # get catch data from 

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
    "./rdata/guilds3.txt", header = TRUE, sep = ",", stringsAsFactors = FALSE, fileEncoding = "UTF-8"
  )


gear.list <-
  read.delim(
    "./rdata/gear.list", header = TRUE, sep = ",", stringsAsFactors = FALSE, fileEncoding = "UTF-8"
  )


catch.data$area <-
  as.character(catch.data$reg_area_cod) #removing horrble area column names
catch.data$gears <-
  as.character(catch.data$reg_gear_cod) #...and horrible gear column names

#tilføj lægnavn til catch.data
cname.apply <- c(species.name$common_name)
names(cname.apply) <- c(species.name$species)
catch.data$cnames <- cname.apply[catch.data$species]

#tilføj videnskabeligt navn til catch.data:
sname.apply <- c(species.name$scientific_name)
names(sname.apply) <- c(species.name$species)
catch.data$snames <- sname.apply[catch.data$species]

#tilføj guild til catch.data
guild.apply <- c(guildlist$feeding.guild)
names(guild.apply) <- c(guildlist$scientific.name)
catch.data$guild <- guild.apply[catch.data$snames]

#correcting gears types;
#translate gear data codes to gear type names 
gname.apply <- c(gear.list$gear)
names(gname.apply) <- c(gear.list$data.code)
catch.data$gears.t <- gname.apply[catch.data$gears]


###################################################
############# Preparation for plots ###############
###################################################



#calculating percentages of effort:
#first, get annual data (should be automated later)

#summarise catches species / area
species_gear.summary <-
  ddply(
    catch.data, c("snames", "gears.t"), summarise,
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
#summarise catches species / area
species.area.s <-
  ddply(
    catch.data, c("snames", "cnames", "area"), summarise,
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


#summarise catches per ICES area
area.summary <-
  ddply(
    catch.data, c("area"), summarise,
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

#summarise catches per gear

gear.area.s <-
  ddply(
    catch.data,
    c("gears.t", "area"), summarise,
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

catch_guild.summary <-
  ddply(
    catch.data,
    c("guild"), summarise,
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

guild.area.summary <-
  ddply(
    catch.data,
    c("guild", "area"), summarise,
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




#calculate percentages per annual catches

gear.area.s$"2003" <-
  (gear.area.s$"sum2003" / sum(gear.area.s$"sum2003") * 100)
gear.area.s$"2004" <-
  (gear.area.s$"sum2004" / sum(gear.area.s$"sum2004") * 100)
gear.area.s$"2005" <-
  (gear.area.s$"sum2005" / sum(gear.area.s$"sum2005") * 100)
gear.area.s$"2006" <-
  (gear.area.s$"sum2006" / sum(gear.area.s$"sum2006") * 100)
gear.area.s$"2007" <-
  (gear.area.s$"sum2007" / sum(gear.area.s$"sum2007") * 100)
gear.area.s$"2008" <-
  (gear.area.s$"sum2008" / sum(gear.area.s$"sum2008") * 100)
gear.area.s$"2009" <-
  (gear.area.s$"sum2009" / sum(gear.area.s$"sum2009") * 100)
gear.area.s$"2010" <-
  (gear.area.s$"sum2010" / sum(gear.area.s$"sum2010") * 100)
gear.area.s$"2011" <-
  (gear.area.s$"sum2011" / sum(gear.area.s$"sum2011") * 100)
gear.area.s$"2012" <-
  (gear.area.s$"sum2012" / sum(gear.area.s$"sum2012") * 100)
gear.area.s$"2013" <-
  (gear.area.s$"sum2013" / sum(gear.area.s$"sum2013") * 100)

catch_guild.summary$"2003" <-
  (catch_guild.summary$"sum2003" / sum(catch_guild.summary$"sum2003") * 100)
catch_guild.summary$"2004" <-
  (catch_guild.summary$"sum2004" / sum(catch_guild.summary$"sum2004") * 100)
catch_guild.summary$"2005" <-
  (catch_guild.summary$"sum2005" / sum(catch_guild.summary$"sum2005") * 100)
catch_guild.summary$"2006" <-
  (catch_guild.summary$"sum2006" / sum(catch_guild.summary$"sum2006") * 100)
catch_guild.summary$"2007" <-
  (catch_guild.summary$"sum2007" / sum(catch_guild.summary$"sum2007") * 100)
catch_guild.summary$"2008" <-
  (catch_guild.summary$"sum2008" / sum(catch_guild.summary$"sum2008") * 100)
catch_guild.summary$"2009" <-
  (catch_guild.summary$"sum2009" / sum(catch_guild.summary$"sum2009") * 100)
catch_guild.summary$"2010" <-
  (catch_guild.summary$"sum2010" / sum(catch_guild.summary$"sum2010") * 100)
catch_guild.summary$"2011" <-
  (catch_guild.summary$"sum2011" / sum(catch_guild.summary$"sum2011") * 100)
catch_guild.summary$"2012" <-
  (catch_guild.summary$"sum2012" / sum(catch_guild.summary$"sum2012") * 100)
catch_guild.summary$"2013" <-
  (catch_guild.summary$"sum2013" / sum(catch_guild.summary$"sum2013") * 100)

guild.area.summary$"2003" <-
  (guild.area.summary$"sum2003" / sum(guild.area.summary$"sum2003") * 100)
guild.area.summary$"2004" <-
  (guild.area.summary$"sum2004" / sum(guild.area.summary$"sum2004") * 100)
guild.area.summary$"2005" <-
  (guild.area.summary$"sum2005" / sum(guild.area.summary$"sum2005") * 100)
guild.area.summary$"2006" <-
  (guild.area.summary$"sum2006" / sum(guild.area.summary$"sum2006") * 100)
guild.area.summary$"2007" <-
  (guild.area.summary$"sum2007" / sum(guild.area.summary$"sum2007") * 100)
guild.area.summary$"2008" <-
  (guild.area.summary$"sum2008" / sum(guild.area.summary$"sum2008") * 100)
guild.area.summary$"2009" <-
  (guild.area.summary$"sum2009" / sum(guild.area.summary$"sum2009") * 100)
guild.area.summary$"2010" <-
  (guild.area.summary$"sum2010" / sum(guild.area.summary$"sum2010") * 100)
guild.area.summary$"2011" <-
  (guild.area.summary$"sum2011" / sum(guild.area.summary$"sum2011") * 100)
guild.area.summary$"2012" <-
  (guild.area.summary$"sum2012" / sum(guild.area.summary$"sum2012") * 100)
guild.area.summary$"2013" <-
  (guild.area.summary$"sum2013" / sum(guild.area.summary$"sum2013") * 100)

species.area.s$"2003" <-
  (species.area.s$"sum2003" / sum(species.area.s$"sum2003") * 100)
species.area.s$"2004" <-
  (species.area.s$"sum2004" / sum(species.area.s$"sum2004") * 100)
species.area.s$"2005" <-
  (species.area.s$"sum2005" / sum(species.area.s$"sum2005") * 100)
species.area.s$"2006" <-
  (species.area.s$"sum2006" / sum(species.area.s$"sum2006") * 100)
species.area.s$"2007" <-
  (species.area.s$"sum2007" / sum(species.area.s$"sum2007") * 100)
species.area.s$"2008" <-
  (species.area.s$"sum2008" / sum(species.area.s$"sum2008") * 100)
species.area.s$"2009" <-
  (species.area.s$"sum2009" / sum(species.area.s$"sum2009") * 100)
species.area.s$"2010" <-
  (species.area.s$"sum2010" / sum(species.area.s$"sum2010") * 100)
species.area.s$"2011" <-
  (species.area.s$"sum2011" / sum(species.area.s$"sum2011") * 100)
species.area.s$"2012" <-
  (species.area.s$"sum2012" / sum(species.area.s$"sum2012") * 100)
species.area.s$"2013" <-
  (species.area.s$"sum2013" / sum(species.area.s$"sum2013") * 100)


### cathes per gear, annual development

#start by melting the dataset:


guild_melt <-
  melt(
    catch_guild.summary, id.vars = "guild", measure.vars = c(
      "2003","2004","2005","2006","2007","2008","2009","2010","2011","2012","2013"
    )
  )

guild.area.melt <-
  melt(
    guild.area.summary, id.vars = c("guild","area"), measure.vars = c(
      "2003","2004","2005","2006","2007","2008","2009","2010","2011","2012","2013"
    )
  )

gear.area.m <-
  melt(
    gear.area.s, id.vars = c("gears.t", "area"), measure.vars = c(
      "2003","2004","2005","2006","2007","2008","2009","2010","2011","2012","2013"
    )
  )

species.area.m <-
  melt(
    species.area.s, id.vars = c("snames","cnames", "area"), measure.vars = c(
      "2003","2004","2005","2006","2007","2008","2009","2010","2011","2012","2013"
    )
  )


###################################################################################
######## graph'd below is annual comparison of catches per guild and gears ########
###################################################################################

#### barplot; catches per guild, summed subregions, no division:
guild.sub.plot <-
  ggplot(data = guild_melt, aes(x = variable, y = value)) #adding data to plot
# guild.sub.plot <- + facet_wrap(~variable)
guild.sub.plot <-
  guild.sub.plot + geom_bar(
    stat = "identity", aes(fill = guild), colour =
      "black", position = "dodge"
  ) # adding bar-shape and colours + outlines
guild.sub.plot <-
  guild.sub.plot + xlab("Guilds") + ylab("% of catches") #Add X- & Y-axis labels
guild.sub.plot <-
  guild.sub.plot + ggtitle("Catches per guild, Celtic Sea, all subregions, 2003-2013") # setting the title
guild.sub.plot <-
  guild.sub.plot + theme(axis.text.x = element_text(angle = 90))
guild.sub.plot #show the g'damn plot!


#### barplot; catches per guild, all subregions, divided per subregion:
guild.sub.plotf <-
  ggplot(data = guild.area.melt, aes(x = variable, y = value)) #adding data to plot
#guild2003_2013sub.plot <- guild2003_2013sub.plot +
guild.sub.plotf <-
  guild.sub.plotf + geom_bar(
    stat = "identity", aes(fill = guild), colour =
      "black", position = "dodge"
  )# + scale_y_continuous(formatter='log10') # adding bar-shape and colours + outlines
guild.sub.plotf <-
  guild.sub.plotf + facet_grid(area ~ .)
guild.sub.plotf <-
  guild.sub.plotf + xlab("Year") + ylab("% of catches") #Add X- & Y-axis labels
guild.sub.plotf <-
  guild.sub.plotf + ggtitle("Catches per guild, Celtic Sea, per subregion, 2003-2013") # setting the title
guild.sub.plotf <-
  guild.sub.plotf + theme(axis.text.x = element_text(angle = 90))
guild.sub.plotf #show the g'damn plot!

############# graph'd below is annual comparison of catches per gear #############

#### barplot; catches per gear, summed subregions, no division:
gear.sub.p <-
  ggplot(data = gear.area.m, aes(x = variable, y = value)) #adding data to plot
gear.sub.p <-
  gear.sub.p + geom_bar(
    stat = "identity", aes(fill = gears.t), colour =
      "black", position = "dodge"
  )# + scale_y_continuous(formatter='log10') # adding bar-shape and colours + outlines
gear.sub.p <-
  gear.sub.p + xlab("Year") + ylab("% of catches") #Add X- & Y-axis labels
gear.sub.p <-
  gear.sub.p + ggtitle("Catches per gear, Celtic Sea, per subregion, 2003-2013") # setting the title
gear.sub.p <-
  gear.sub.p + theme(axis.text.x = element_text(angle = 90))
gear.sub.p #show the g'damn plot!

#### barplot; catches per gear, all subregions, divided per subregion:
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
  gear.sub.p + xlab("Year") + ylab("% of catches") #Add X- & Y-axis labels
gear.sub.p <-
  gear.sub.p + ggtitle("Catches per gear, Celtic Sea, per subregion, 2003-2013") # setting the title
gear.sub.p <-
  gear.sub.p + theme(axis.text.x = element_text(angle = 90))
gear.sub.p #show the g'damn plot!












###########################################################################################
###########################################################################################
###########################################################################################
############# WARNING! EXPERIMENTAL PLOTS AHEAD - MAY NOT MAKE *ANY* SENSE! ###############
###########################################################################################
################################ HERE BE DRAGONS ##########################################
###########################################################################################

# #### barplot; catches per species, summed subregions, no division:
# species.sub.p <-
#   ggplot(data = species.area.m, aes(x = variable, y = value)) #adding data to plot
# species.sub.p <-
#   species.sub.p + geom_bar(
#     stat = "identity", aes(fill = snames), colour =
#       "black", position = "dodge"
#   )# + scale_y_continuous(formatter='log10') # adding bar-shape and colours + outlines
# species.sub.p <-
#   species.sub.p + xlab("Year") + ylab("% of catches") #Add X- & Y-axis labels
# species.sub.p <-
#   species.sub.p + ggtitle("Catches per species, North Sea, all subregions, 2003-2013") # setting the title
# species.sub.p <-
#   species.sub.p + theme(axis.text.x = element_text(angle = 90))
# species.sub.p #show the g'damn plot!
# 
# #### barplot; catches per species, all subregions, divided per subregion:
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
#   species.sub.pf + xlab("Year") + ylab("% of catches") #Add X- & Y-axis labels
# species.sub.pf <-
#   species.sub.pf + ggtitle("Catches per species, North Sea, per subregion, 2003-2013") # setting the title
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
#   lolplot + xlab("Year") + ylab("% of catches") #Add X- & Y-axis labels
# lolplot <-
#   lolplot + ggtitle("Catches per guild, BALTIC, all subregions, 2003-2013") # setting the title
# lolplot <-
#   lolplot + theme(axis.text.x = element_text(angle = 90))
# #  sum2003plot <- sum2003plot + scale_y_continuous(labels = percent)
# lolplot #show the g'damn plot!