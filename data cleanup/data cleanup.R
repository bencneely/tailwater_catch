#--------------------------------------------------------------
#Ben Neely
#08/21/2025
#Clean up discharge and creel data for modeling
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
dd=read_csv("data cleanup/discharge retrieval/daily_discharge.csv")
counts=read_csv("data cleanup/raw creel data/counts.csv")
fish=read_csv("data cleanup/raw creel data/fish.csv")

################################################################################
## Manipulate data a little bit so we can do some modeling

################################################################################
## Discharge data
dd1=dd%>%
  mutate(date=as.Date(date,tryFormats=c("%m/%d/%Y","%Y-%m-%d")),
         month=month(date),
         cfs=as.integer(round(cfs,0)))%>%
  select(impd,date,month,year,cfs)

## Export
write_csv(dd1,"data cleanup/discharge_clean.csv")

################################################################################
## Creel counts
## Anglers during each sampling period is mean boat anglers (1st and 2nd count)
## plus mean shore anglers. This represents effort during the sampling period
counts1=counts%>%
  mutate(date=as.Date(sample_date,tryFormats=c("%m/%d/%Y","%Y-%m-%d")),
         month=month(date),
         year=year(date),
         dow=wday(date,label=T,abbr=F),
         daytype=case_when(dow=="Saturday"|dow=="Sunday" ~ "weekend",
                           TRUE ~ "weekday"))%>%
  mutate(boatangs=(anglers_boat_first_count+anglers_boat_second_count)/2,
         shoreangs=(anglers_shore_first_count+anglers_shore_second_count)/2,
         totangs=boatangs+shoreangs)%>%
  select(event_id,impd=impoundment_code,date,daytype,month,year,totangs)

## Export
write_csv(counts1,"data cleanup/counts_clean.csv")

################################################################################
## Fish data
## The goal here is to get a catch and targeted catch rate for each angling party
## Broke things down by genus to avoid crappie/percid/ictalurid/etc. confusion
fish1=fish%>%
  uncount(group_count,
          .remove=F)%>%
  mutate(date=as.Date(sample_date,tryFormats=c("%m/%d/%Y","%Y-%m-%d")),
         month=month(date),
         year=year(date),
         anghrs=number_of_anglers*hours_fishing,
         genus=case_when(species=="Bigmouth Buffalo"|species=="Smallmouth Buffalo"|species=="Black Buffalo" ~ "Ictiobus",
                         species=="Black Crappie"|species=="White Crappie"|species=="Pomoxis" ~ "Pomoxis",
                         species=="Bluegill"|species=="Longear Sunfish"|species=="Redear Sunfish"|
                           species=="Orangespotted Sunfish"|species=="Sunfish"|species=="Green Sunfish" ~ "Lepomis",
                         species=="White Bass"|species=="Palmetto Bass"|species=="Striped Bass" ~ "Morone",
                         species=="Channel Catfish"|species=="Blue Catfish" ~ "Ictalurus",
                         species=="Flathead Catfish" ~ "Pylodictis",
                         species=="Common Carp" ~ "Cyprinus",
                         species=="Freshwater Drum" ~ "Aplodinotus",
                         species=="Gizzard Shad" ~ "Dorosoma",
                         species=="Largemouth Bass"|species=="Smallmouth Bass"|species=="Spotted Bass" ~ "Micropterus",
                         species=="Longnose Gar"|species=="Shortnose Gar" ~ "Lepisosteus",
                         species=="Saugeye"|species=="Walleye"|species=="Sauger" ~ "Sander",
                         species=="Bighead Carp"|species=="Silver Carp" ~ "Hypopthalmichthys",
                         species=="Brown Trout"|species=="Rainbow Trout" ~ "Salmonidae",
                         species=="Black Bullhead" ~ "Ameiurus",
                         species=="Paddlefish" ~ "Polyodon",
                         species=="Spotted Sucker" ~ "Minytrema",
                         species=="Blue Sucker" ~ "Cycleptus",
                         species=="***No Fish***" ~ "Nothing",
                         TRUE ~ "Other"),
         pref_genus=case_when(preferred_species=="Bigmouth Buffalo"|preferred_species=="Smallmouth Buffalo"|preferred_species=="Black Buffalo" ~ "Ictiobus",
                              preferred_species=="Black Crappie"|preferred_species=="White Crappie"|preferred_species=="Pomoxis" ~ "Pomoxis",
                              preferred_species=="Bluegill"|preferred_species=="Longear Sunfish"|preferred_species=="Redear Sunfish"|
                                preferred_species=="Orangespotted Sunfish"|preferred_species=="Sunfish"|preferred_species=="Green Sunfish" ~ "Lepomis",
                              preferred_species=="White Bass"|preferred_species=="Palmetto Bass"|preferred_species=="Striped Bass" ~ "Morone",
                              preferred_species=="Channel Catfish"|preferred_species=="Blue Catfish" ~ "Ictalurus",
                              preferred_species=="Flathead Catfish" ~ "Pylodictis",
                              preferred_species=="Common Carp" ~ "Cyprinus",
                              preferred_species=="Freshwater Drum" ~ "Aplodinotus",
                              preferred_species=="Gizzard Shad" ~ "Dorosoma",
                              preferred_species=="Largemouth Bass"|preferred_species=="Smallmouth Bass"|preferred_species=="Spotted Bass" ~ "Micropterus",
                              preferred_species=="Longnose Gar"|preferred_species=="Shortnose Gar" ~ "Lepisosteus",
                              preferred_species=="Saugeye"|preferred_species=="Walleye"|preferred_species=="Sauger" ~ "Sander",
                              preferred_species=="Bighead Carp"|preferred_species=="Silver Carp" ~ "Hypopthalmichthys",
                              preferred_species=="Brown Trout"|preferred_species=="Rainbow Trout" ~ "Salmonidae",
                              preferred_species=="Black Bullhead" ~ "Ameiurus",
                              preferred_species=="Paddlefish" ~ "Polyodon",
                              preferred_species=="Spotted Sucker" ~ "Minytrema",
                              preferred_species=="Blue Sucker" ~ "Cycleptus",
                              TRUE ~ "Anything"),
         target=case_when(pref_genus=="Anything" ~ 1,
                          pref_genus==genus ~ 1,
                          TRUE ~ 0))%>%
  select(event_id,interview_id,impd=impoundment_code,date,month,year,anghrs,pref_genus,genus,target)%>%
  mutate(count=case_when(genus=="Nothing" ~ 0,
                         TRUE ~ 1))%>%
  group_by(event_id,interview_id,impd,date,month,year,anghrs,pref_genus)%>%
  summarise(total_fish=sum(count),
            targeted_fish=sum(count[target==1],na.rm=TRUE),
            .groups="drop")%>%
  mutate(total=if_else(anghrs>0,total_fish/anghrs,NA_real_),
         target=if_else(anghrs>0,targeted_fish/anghrs,NA_real_))%>%
  pivot_longer(cols=c(total,target),
               names_to="catch_type",
               values_to="catch_rate")%>%
  drop_na(anghrs)%>%
  select(event_id,interview_id,impd,date,month,year,anghrs,pref_genus,catch_type,catch_rate)

## Export
write_csv(fish1,"data cleanup/fish_clean.csv")