 
# LOAD NEEDED LIBS+PACKAGAGES
install.packages("ggplot2") #remember to uncomment at first run - this needs to be automated. R (base?) can do that.
library(ggplot2)
library(scales)
library(plyr)
library(reshape2)
library(foreach)
# library(data.table)

econ.data.all <-
  read.delim(
    "~/r/rdata/stecf-economic-fleet.data", header = TRUE, fill = TRUE, stringsAsFactors = FALSE, dec = "."
  ) # get economic data from STECF dataset
as.numeric(econ.data.all$value)
gear.list <-
  read.delim(
    "~/r/ices/gear.list", header = TRUE, sep = ",", stringsAsFactors = FALSE, fileEncoding = "UTF-8"
  )
mel
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

# # select relevant data, i.e. FAO27/ICES-area:
# fao27.df <- subset.data.frame(
#   econ.data.all, c(supra_reg == "AREA27"), select = c(
#     "sub_reg", "country_name", "year", "variable_name", "value", "unit", "species_name", "species_code", "gear_type"
#   )
# )

# further subsetting, including only catches in ICES


ices27.df <- subset.data.frame(
  econ.data.all, grepl("27.", econ.data.all$sub_reg), select = c(
    "sub_reg", "country_name", "year", "variable_name", "value", "unit", "species_name", "species_code", "gear_type"
  )
)

ices27.df$value <- as.numeric(ices27.df$value)

rm(econ.data.all)

# report on the number of sub regions included in the subset:
ices.area.dim <- as.data.frame(table(ices27.df$sub_reg))
dim(ices.area.dim)

# append subregion key:

reg.apply <- c(reg.list$eco.reg.shrt)
names(reg.apply) <- c(reg.list$ices.code)
ices27.df$ecoregion <- reg.apply[ices27.df$sub_reg]

sciname.apply <- c(species.list$scientific_name)
names(sciname.apply) <- c(species.list$species)
ices27.df$sci.name <- sciname.apply[ices27.df$species_code]

guild.apply <- c(guild.list$feeding.guild)
names(guild.apply) <- c(guild.list$scientific.name)
ices27.df$feeding.guild <- guild.apply[ices27.df$sci.name]

#-------- find species missing guild assignments --------

# get number of unique missing species
missing.guild <-
  as.data.frame(unique(subset.data.frame(ices27.df, c(is.na(feeding.guild) & variable_name == "Landings weight"), select = c("sci.name", "species_code"))))

# get number of missing values pr. missing species
missing.guild.summary <-
  as.data.frame(subset.data.frame(ices27.df, subset = c(is.na(feeding.guild) & variable_name == "Landings weight"), select = c("sci.name", "species_code")))
# calculate frequency of missing species in dataset
missing.count <- count(missing.guild.summary, vars = c("sci.name"))

missingtop100 <- subset(missing.count, freq > 100, select = c("sci.name", "freq"))
missingtop500 <- subset(missing.count, freq > 500, select = c("sci.name", "freq"))

#-------- subset entire dataset to ecoregions ---------------#

bal.df <- subset.data.frame(
  ices27.df, ecoregion == "BAL", select = c(
    "sub_reg","ecoregion", "country_name", "year", "variable_name", "value", "unit","gear_type", "species_name", "species_code", "sci.name", "feeding.guild"
  )
)

ns.df <- subset.data.frame(
  ices27.df, ecoregion == "NS", select = c(
    "sub_reg","ecoregion", "country_name", "year", "variable_name", "value", "unit","gear_type", "species_name", "species_code", "sci.name", "feeding.guild"
  )
)

cel.df <- subset.data.frame(
  ices27.df, ecoregion == "CEL", select = c(
    "sub_reg","ecoregion", "country_name", "year", "variable_name", "value", "unit","gear_type", "species_name", "species_code", "sci.name", "feeding.guild"
  )
)

# put a rm() snip here, when the whole data set is not needed anymore

#--get effort, fishing hours

bhc <-
  subset.data.frame(
    bal.df, subset = c(variable_name == "Actual hours at Sea"), select = c("country_name", "year","variable_name","value"))

c.bhc <-
  dcast(bhc, country_name + variable_name~year, sum, na.rm=TRUE)

m.bhc <- 
  melt(c.bhc, id.vars = c("country_name", "variable_name"), measure.vars = c("2008", "2009", "2010", "2011", "2012", "2013", "2014"))

#--get effort, kWdays

bec <-
  subset.data.frame(
    bal.df, subset = c(variable_name == "kWfishing days"), select = c("country_name", "year","variable_name","value"))

c.bec <-
  dcast(bec, country_name + variable_name~year, sum, na.rm=TRUE)

m.bec <- 
  melt(c.bec, id.vars = c("country_name", "variable_name"), measure.vars = c("2008", "2009", "2010", "2011", "2012", "2013", "2014"))


#--landing value & weight

blv <-
  subset.data.frame(
    bal.df, subset = c(variable_name == "Landings value" | variable_name == "Landings weight"), select = c("variable_name","value", "feeding.guild", "year"))

c.blv <-
  dcast(blv, year + feeding.guild ~ variable_name, sum, na.rm = TRUE)

blvc <-
  subset.data.frame(
    bal.df, subset = c(variable_name == "Landings value" | variable_name == "Landings weight"), select = c("feeding.guild","variable_name","value", "country_name", "year"))

c.byc <-
  dcast(blvc, feeding.guild + variable_name + country_name ~ year, sum, na.rm = TRUE)

m.byc <-
  melt(c.byc, id.vars = c("feeding.guild", "variable_name", "country_name"), measure.vars = c("2008", "2009", "2010", "2011", "2012", "2013", "2014"))

c.bc <-
  dcast(blvc, country_name ~ year, sum, na.rm = TRUE)

m.bc <-
  melt(c.bc, id.vars = c("country_name"), measure.vars = c("2008", "2009", "2010", "2011", "2012", "2013", "2014"))



#### barplot; landings per guild, summed subregions, no division:


ggplot(na.omit(m.bc), aes(x = value, y = factor(variable) )) +
  geom_point() +
  xlab("Total Landings, metric tonnes") + ylab("Country") +
  facet_wrap("country_name")
  # theme(title = expression("New Hampshire Primary 2012")
# theme_bw() + theme(axis.title.x = theme_text(size = 12, vjust = .25))+







