#--------------------------------------------------------------
#Ben Neely
#01/26/2026
#Organize fish data so we we can see catch rates by genus
#--------------------------------------------------------------

## Clear R
cat("\014")  
rm(list=ls())

## Install and load packages
## Checks if package is installed, installs if not, activates for current session
if("FSA" %in% rownames(installed.packages()) == FALSE) {install.packages("FSA")}
library(FSA)

if("patchwork" %in% rownames(installed.packages()) == FALSE) {install.packages("patchwork")}
library(patchwork)

if("tidyverse" %in% rownames(installed.packages()) == FALSE) {install.packages("tidyverse")}
library(tidyverse)

## Read in data with import
fish=read_csv("data cleanup/raw creel data/fish.csv")

################################################################################
## Manipulate data a little bit so we can do some modeling

################################################################################
## Fish data
## The goal here is to get a catch and targeted catch rate for each angling party
## Broke things down by genus to avoid crappie/percid/ictalurid/etc. confusion
fish1=fish%>%
  mutate(group_count=case_when(species=="***No Fish***" ~ 1,
                               TRUE ~ group_count))%>%
  uncount(group_count,
          .remove=F)%>%
  mutate(date=as.Date(sample_date,tryFormats=c("%m/%d/%Y","%Y-%m-%d")),
         month=month(date),
         year=year(date),
         anghrs=number_of_anglers*hours_fishing,
         genus=case_when(species=="Bigmouth Buffalo"|species=="SMALLMOUTH BUFFALO"|species=="Black Buffalo"|
                           species=="ICTIOBUS" ~ "Ictiobus",
                         species=="Black Crappie"|species=="White Crappie"|species=="Pomoxis" ~ "Pomoxis",
                         species=="Bluegill"|species=="LONGEAR SUNFISH"|species=="Redear Sunfish"|
                           species=="ORANGESPOTTED SUNFISH"|species=="Sunfish"|species=="Green Sunfish" ~ "Lepomis",
                         species=="White Bass"|species=="Palmetto Bass"|species=="Striped Bass" ~ "Morone",
                         species=="Channel Catfish"|species=="Blue Catfish" ~ "Ictalurus",
                         species=="Flathead Catfish" ~ "Pylodictis",
                         species=="Common Carp"|species=="CARPOIDES" ~ "Cyprinus",
                         species=="Freshwater Drum" ~ "Aplodinotus",
                         species=="Gizzard Shad" ~ "Dorosoma",
                         species=="Largemouth Bass"|species=="Smallmouth Bass"|species=="Spotted Bass" ~ "Micropterus",
                         species=="LONGNOSE GAR"|species=="SHORTNOSE GAR" ~ "Lepisosteus",
                         species=="Saugeye"|species=="Walleye"|species=="Sauger" ~ "Sander",
                         species=="Bighead Carp"|species=="Silver Carp" ~ "Hypopthalmichthys",
                         species=="BROWN TROUT"|species=="Rainbow Trout" ~ "Salmonidae",
                         species=="Black Bullhead" ~ "Ameiurus",
                         species=="PADDLEFISH" ~ "Polyodon",
                         species=="SPOTTED SUCKER" ~ "Minytrema",
                         species=="Blue Sucker" ~ "Cycleptus",
                         species=="***No Fish***" ~ "Nothing",
                         TRUE ~ "Other"),
         pref_genus=case_when(preferred_species=="Bigmouth Buffalo"|preferred_species=="SMALLMOUTH BUFFALO"|preferred_species=="Black Buffalo"|
                                preferred_species=="ICTIOBUS" ~ "Ictiobus",
                              preferred_species=="Black Crappie"|preferred_species=="White Crappie"|preferred_species=="Pomoxis" ~ "Pomoxis",
                              preferred_species=="Bluegill"|preferred_species=="LONGEAR SUNFISH"|preferred_species=="Redear Sunfish"|
                                preferred_species=="ORANGESPOTTED SUNFISH"|preferred_species=="Sunfish"|preferred_species=="Green Sunfish" ~ "Lepomis",
                              preferred_species=="White Bass"|preferred_species=="Palmetto Bass"|preferred_species=="Striped Bass" ~ "Morone",
                              preferred_species=="Channel Catfish"|preferred_species=="Blue Catfish" ~ "Ictalurus",
                              preferred_species=="Flathead Catfish" ~ "Pylodictis",
                              preferred_species=="Common Carp"|preferred_species=="CARPOIDES" ~ "Cyprinus",
                              preferred_species=="Freshwater Drum" ~ "Aplodinotus",
                              preferred_species=="Gizzard Shad" ~ "Dorosoma",
                              preferred_species=="Largemouth Bass"|preferred_species=="Smallmouth Bass"|preferred_species=="Spotted Bass" ~ "Micropterus",
                              preferred_species=="LONGNOSE GAR"|preferred_species=="SHORTNOSE GAR" ~ "Lepisosteus",
                              preferred_species=="Saugeye"|preferred_species=="Walleye"|preferred_species=="Sauger" ~ "Sander",
                              preferred_species=="Bighead Carp"|preferred_species=="Silver Carp" ~ "Hypopthalmichthys",
                              preferred_species=="BROWN TROUT"|preferred_species=="Rainbow Trout" ~ "Salmonidae",
                              preferred_species=="Black Bullhead" ~ "Ameiurus",
                              preferred_species=="PADDLEFISH" ~ "Polyodon",
                              preferred_species=="SPOTTED SUCKER" ~ "Minytrema",
                              preferred_species=="Blue Sucker" ~ "Cycleptus",
                              TRUE ~ "Anything"))%>%
  
  ## Still in the pipe from above...
  ## tallying multiple fish entries into a single count
  ## Adding zero catch dat afor fish that weren't caught
  ## Calculate catch rate and identify if it was a target genera
  ## Ensure catch and catch rate of 0 for anglers that didn't catch anything
  select(event_id,interview_id,impd=impoundment_code,date,month,year,anghrs,pref_genus,genus)%>%
  group_by(event_id,interview_id,impd,date,month,year,anghrs,pref_genus,genus)%>%
  tally(name="count")%>%
  ungroup()%>%
  complete(nesting(event_id,interview_id,impd,date,month,year,anghrs,pref_genus),
           genus=unique(genus), 
           fill=list(count=0))%>%
  mutate(catch_rate=count/anghrs,
         target=case_when(pref_genus==genus ~ 1,
                          pref_genus=="Anything" ~ 1,
                          TRUE ~ 0),
         count=case_when(genus=="Nothing" ~ 0L,
                         TRUE ~ count),
         catch_rate=case_when(genus=="Nothing" ~ 0,
                              TRUE ~ catch_rate))

## Export
write_csv(fish1,"data cleanup/fish_genus_clean.csv")