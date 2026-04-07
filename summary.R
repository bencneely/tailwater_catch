#--------------------------------------------------------------
#Ben Neely
#09/04/2025
#Summary data for reporting
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
counts=read_csv("data cleanup/counts_clean.csv")
fish=read_csv("data cleanup/fish_clean.csv")
discharge=read_csv("data cleanup/discharge_clean.csv")

## Get rid of data between 2020 and 2022 and non-standard months
counts1=counts%>%
  filter(year<2020 | year>2022,
         month %in% 3:10)

fish1=fish%>%
  filter(year<2020 | year>2022,
         month %in% 3:10,
         catch_type=="total")

discharge1=discharge%>%
  filter(year<2020 | year>2022,
         month %in% 3:10,
         cfs>=0)

################################################################################
## Creel results paragraph summary data
################################################################################

## Creel survey locations and years
unique(counts1$impd)
unique(counts1$year)

tmp1=counts1%>%
  mutate(locyr=paste(impd,year,sep=""))
print(unique(tmp1$locyr,n=Inf))

## Number of creels per location-year
tmp2=counts1%>%
  group_by(impd,year)%>%
  summarize(n=n(),
            .groups="drop")

min(tmp2$n)
max(tmp2$n)
mean(tmp2$n)
sd(tmp2$n)

## Look at grand sums of counts and interviews
sum(counts1$totangs)
nrow(fish1)

## Angler count summary
tmp3=counts1%>%
  group_by(impd,year)%>%
  summarize(sumangs=sum(totangs),
            .groups="drop")

min(tmp3$sumangs)
max(tmp3$sumangs)
mean(tmp3$sumangs)
sd(tmp3$sumangs)

## Angler count summary
tmp4=fish1%>%
  group_by(impd,year)%>%
  summarize(n=n(),
            .groups="drop")

min(tmp4$n)
max(tmp4$n)
mean(tmp4$n)
sd(tmp4$n)

################################################################################
## Generate data for table 1
################################################################################

## Surveys and angler counts per month
out1=counts1%>%
  group_by(impd,month,year)%>%
  summarize(surveys=n(),
            allangs=sum(totangs),
            .groups="drop")%>%
  group_by(month)%>%
  summarize(mean_survs=mean(surveys,na.rm=T),
            sd_survs=sd(surveys,na.rm=T),
            mean_angs=mean(allangs,na.rm=T),
            sd_angs=sd(allangs,na.rm=T),
            .groups="drop")
print(out1)

## Interviews and catch rate per month
out2=fish1%>%
  group_by(impd,month,year)%>%
  summarize(ints=n(),
            catch_rate=mean(catch_rate,na.rm=T),
            .groups="drop")%>%
  group_by(month)%>%
  summarize(mean_ints=mean(ints,na.rm=T),
            sd_ints=sd(ints,na.rm=T),
            mean_cr=mean(catch_rate,na.rm=T),
            sd_cr=sd(catch_rate,na.rm=T),
            .groups="drop")
print(out2)

## Daily cfs per month
out3=discharge1%>%
  group_by(month)%>%
  summarize(mean_cfs=mean(cfs),
            sd_cfs=sd(cfs))
print(out3)

################################################################################
## All of below is copied from the total catch script so we can summarize discharge indices
################################################################################

## First, let's summarize discharge at a monthly scale to match creel data
## Median cfs - central measure of discharge
discharge_med=discharge1%>%
  group_by(impd,month,year)%>%
  summarize(med_cfs=median(cfs),
            .groups="drop")%>%
  select(impd,month,year,med_cfs)

## Richards-Baker flashiness index - measure of discharge variability
discharge_rbfi=discharge1%>%
  group_by(impd,month,year)%>%
  summarize(rbfi_num=sum(abs(diff(cfs))),
            rbfi_denom=sum(cfs),
            rbfi=rbfi_num/rbfi_denom,
            .groups="drop")%>%
  select(impd,month,year,rbfi)

## Combine to get monthly summarized discharge
discharge2=discharge_med%>%
  inner_join(discharge_rbfi,by=c("impd","month","year"))
################################################################################

## Median cfs per month
out4=discharge2%>%
  group_by(month)%>%
  summarize(mean_med=mean(med_cfs),
            sd_med=sd(med_cfs),
            mean_rbfi=mean(rbfi),
            sd_rbfi=sd(rbfi),
            .groups="drop")
print(out4)

################################################################################
## Combine all output, format, and output
out=out1%>%
  inner_join(out2,by="month")%>%
  inner_join(out3,by="month")%>%
  inner_join(out4,by="month")%>%
  pivot_longer(cols=-month,
               names_to=c(".value","variable"),
               names_sep="_")%>%
  mutate(formatted_value=case_when(variable %in% c("survs","angs","ints") ~ sprintf("%.1f (%.1f)",mean,sd),
                                   variable=="cr" ~ sprintf("%.2f (%.2f)",mean,sd),
                                   variable %in% c("cfs","med") ~ sprintf("%.0f (%.0f)",mean,sd),
                                   variable %in% c("rbfi") ~ sprintf("%.3f (%.3f)",mean,sd),
                                   TRUE ~ as.character(mean)))%>%
  pivot_wider(id_cols=month,
              names_from=variable,
              values_from=formatted_value)

#write_csv(out,"t1dat.csv")

################################################################################
## Discharge results paragraph summary data
################################################################################

## Number of cfs < 0 we removed
subset(discharge,cfs<0)

## Mean daily discharge summary
min(discharge1$cfs)
max(discharge1$cfs)
mean(discharge1$cfs)
sd(discharge1$cfs)

## Break down the three continuous discharge variables
min(discharge2$med_cfs)
max(discharge2$med_cfs)
mean(discharge2$med_cfs)
sd(discharge2$med_cfs)

min(discharge2$rbfi)
max(discharge2$rbfi)
mean(discharge2$rbfi)
sd(discharge2$rbfi)