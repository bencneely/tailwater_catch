#--------------------------------------------------------------
#Ben Neely
#03/26/2025
#Retrieve, consolidate, and organize water discharge data from tailwaters
#--------------------------------------------------------------

## Clear R
cat("\014")  
rm(list=ls())

## Install and load packages
## Checks if package is installed, installs if not, activates for current session
if("FSA" %in% rownames(installed.packages()) == FALSE) {install.packages("FSA")}
library(FSA)

if("rio" %in% rownames(installed.packages()) == FALSE) {install.packages("rio")}
library(rio)

if("lubridate" %in% rownames(installed.packages()) == FALSE) {install.packages("lubridate")}
library(lubridate)

if("tidyverse" %in% rownames(installed.packages()) == FALSE) {install.packages("tidyverse")}
library(tidyverse)

if("httr2" %in% rownames(installed.packages()) == FALSE) {install.packages("httr2",type="binary")}
library(httr2)

if("dataRetrieval" %in% rownames(installed.packages()) == FALSE) {install.packages("dataRetrieval")}
library(dataRetrieval)

################################################################################
## Set up named parameters for easy calling into function
## First set up USGS waterdata gauge stations for each tailwater
clsb_id="06891500"
cgsb_id="07179500"
gelo_id="06875900"
hisb_id="06915000"
mesb_id="06911000"
misb_id="06857100"
mrsb_id="07179795"
pesb_id="06890900"
poms_id="06912500"
rffa_id="06887000"

## Now set parameterCd - this tells the function we want discharge in CFS
dd="00060"

################################################################################
## Use readNWISdv() function in dataRetrieval package to bring in flow data for each creel period
## 2003 CLSB
clsb03=readNWISdv(clsb_id,dd,startDate="2003-03-01",endDate="2003-10-31")%>%
  mutate(impd="CLSB",year=2003)

## 2006 CLSB
clsb06=readNWISdv(clsb_id,dd,startDate="2006-03-01",endDate="2006-10-31")%>%
  mutate(impd="CLSB",year=2006)

## 2012 CLSB
clsb12=readNWISdv(clsb_id,dd,startDate="2012-03-01",endDate="2012-10-31")%>%
  mutate(impd="CLSB",year=2012)

## 2013 CLSB
clsb13=readNWISdv(clsb_id,dd,startDate="2013-03-01",endDate="2013-10-31")%>%
  mutate(impd="CLSB",year=2013)

## 2015 CLSB
clsb15=readNWISdv(clsb_id,dd,startDate="2015-03-01",endDate="2015-10-31")%>%
  mutate(impd="CLSB",year=2015)

## 2017 CLSB
clsb17=readNWISdv(clsb_id,dd,startDate="2017-03-01",endDate="2017-10-31")%>%
  mutate(impd="CLSB",year=2017)

## 1999 CGSB
cgsb99=readNWISdv(cgsb_id,dd,startDate="1999-03-01",endDate="1999-10-31")%>%
  mutate(impd="CGSB",year=1999)

## 2017 CGSB
cgsb17=readNWISdv(cgsb_id,dd,startDate="2017-03-01",endDate="2017-10-31")%>%
  mutate(impd="CGSB",year=2017)

## 2014 GELO
gelo14=readNWISdv(gelo_id,dd,startDate="2014-03-01",endDate="2014-10-31")%>%
  mutate(impd="GELO",year=2014)

## 2019 GELO
gelo19=readNWISdv(gelo_id,dd,startDate="2019-03-01",endDate="2019-10-31")%>%
  mutate(impd="GELO",year=2019)

## 2020 GELO
gelo20=readNWISdv(gelo_id,dd,startDate="2020-03-01",endDate="2020-10-31")%>%
  mutate(impd="GELO",year=2020)

## 2021 GELO
gelo21=readNWISdv(gelo_id,dd,startDate="2021-03-01",endDate="2021-10-31")%>%
  mutate(impd="GELO",year=2021)

## 2022 GELO
gelo22=readNWISdv(gelo_id,dd,startDate="2022-03-01",endDate="2022-10-31")%>%
  mutate(impd="GELO",year=2022)

## 1999 HISB
hisb99=readNWISdv(hisb_id,dd,startDate="1999-03-01",endDate="1999-10-31")%>%
  mutate(impd="HISB",year=1999)

## 2002 HISB
hisb02=readNWISdv(hisb_id,dd,startDate="2002-03-01",endDate="2002-10-31")%>%
  mutate(impd="HISB",year=2002)

## 2004 HISB
hisb04=readNWISdv(hisb_id,dd,startDate="2004-03-01",endDate="2004-10-31")%>%
  mutate(impd="HISB",year=2004)

## 2007 HISB
hisb07=readNWISdv(hisb_id,dd,startDate="2007-03-01",endDate="2007-10-31")%>%
  mutate(impd="HISB",year=2007)

## 2024 HISB
hisb24=readNWISdv(hisb_id,dd,startDate="2024-03-01",endDate="2024-10-31")%>%
  mutate(impd="HISB",year=2024)

## 2016 MESB
mesb16=readNWISdv(mesb_id,dd,startDate="2016-03-01",endDate="2016-10-31")%>%
  mutate(impd="MESB",year=2016)

## 2000 MISB
misb00=readNWISdv(misb_id,dd,startDate="2000-03-01",endDate="2000-10-31")%>%
  mutate(impd="MISB",year=2000)

## 2001 MRSB
mrsb01=readNWISdv(mrsb_id,dd,startDate="2001-03-01",endDate="2001-10-31")%>%
  mutate(impd="MRSB",year=2001)

## 2008 MRSB
mrsb08=readNWISdv(mrsb_id,dd,startDate="2008-03-01",endDate="2008-10-31")%>%
  mutate(impd="MRSB",year=2008)

## 2001 PESB
pesb01=readNWISdv(pesb_id,dd,startDate="2001-03-01",endDate="2001-10-31")%>%
  mutate(impd="PESB",year=2001)

## 2004 PESB
pesb04=readNWISdv(pesb_id,dd,startDate="2004-03-01",endDate="2004-10-31")%>%
  mutate(impd="PESB",year=2004)

## 2011 PESB
pesb11=readNWISdv(pesb_id,dd,startDate="2011-03-01",endDate="2011-10-31")%>%
  mutate(impd="PESB",year=2011)

## 2016 PESB
pesb16=readNWISdv(pesb_id,dd,startDate="2016-03-01",endDate="2016-10-31")%>%
  mutate(impd="PESB",year=2016)

## 2018 POMS
poms18=readNWISdv(poms_id,dd,startDate="2018-03-01",endDate="2018-10-31")%>%
  mutate(impd="POMS",year=2018)

## 2005 RFFA
rffa05=readNWISdv(rffa_id,dd,startDate="2005-03-01",endDate="2005-10-31")%>%
  mutate(impd="RFFA",year=2005)

## 2013 RFFA
rffa13=readNWISdv(rffa_id,dd,startDate="2013-03-01",endDate="2013-10-31")%>%
  mutate(impd="RFFA",year=2013)

## 2023 RFFA
rffa23=readNWISdv(rffa_id,dd,startDate="2023-03-01",endDate="2023-10-31")%>%
  mutate(impd="RFFA",year=2023)

## 2024 RFFA
rffa24=readNWISdv(rffa_id,dd,startDate="2024-03-01",endDate="2024-10-31")%>%
  mutate(impd="RFFA",year=2024)

################################################################################
## Consolidate discharge data and organize
ddat=bind_rows(clsb03,clsb06,clsb12,clsb13,clsb15,clsb17,
               cgsb99,cgsb17,gelo14,gelo19,gelo20,gelo21,
               gelo22,hisb99,hisb02,hisb04,hisb07,hisb24,
               mesb16,misb00,mrsb01,mrsb08,pesb01,pesb04,
               pesb11,pesb16,poms18,rffa05,rffa13,rffa23,
               rffa24)%>%
  mutate(id=paste(impd,year,sep=""))%>%
  select(agency=agency_cd,
         site=site_no,
         date=Date,
         year,
         impd,
         cfs=X_00060_00003,
         id)

## Export discharge data
export(ddat,"data cleanup/discharge retrieval/daily_discharge.csv")
