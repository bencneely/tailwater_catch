#--------------------------------------------------------------
#Ben Neely
#01/30/2026
#Model angling effort as a function of discharge
#This is based on daily count and discharge data
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

if("corrplot" %in% rownames(installed.packages()) == FALSE) {install.packages("corrplot")}
library(corrplot)

if("performance" %in% rownames(installed.packages()) == FALSE) {install.packages("performance")}
library(performance)

if("ggeffects" %in% rownames(installed.packages()) == FALSE) {install.packages("ggeffects")}
library(ggeffects)

if("emmeans" %in% rownames(installed.packages()) == FALSE) {install.packages("emmeans")}
library(emmeans)

if("glmmTMB" %in% rownames(installed.packages()) == FALSE) {install.packages("glmmTMB")}
library(glmmTMB)

if("tidyverse" %in% rownames(installed.packages()) == FALSE) {install.packages("tidyverse")}
library(tidyverse)

## Set ggplot theme
## Set ggplot theme
pubtheme=theme_classic()+
  theme(panel.grid=element_blank(), 
        panel.background=element_blank(),
        plot.background=element_blank(),
        panel.border=element_rect(fill="transparent"),
        axis.title=element_text(size=22,color="black",face="bold"),
        axis.text=element_text(size=18,color="black"),
        legend.position="none",
        strip.text=element_text(size=14,face="bold",margin=margin(b=10,t=10)),
        strip.background=element_rect(fill="gray95"))
options(scipen=999)

## Set seed for randomization
set.seed(904)

## Read in data with import
discharge=read_csv("data cleanup/discharge_clean.csv")
counts=read_csv("data cleanup/counts_clean.csv")

################################################################################
## First, let's combine discharge and counts since we have daily data
## We'll transform daily CFS by taking the natural log of CFS+1
out=counts%>%
  filter(year<2020 | year>2022,
         month %in% 3:10)%>%
  left_join(discharge,by=c("impd","date","month","year"))%>%
  filter(cfs>=0)%>%
  mutate(ln_cfs=log1p(cfs),
         month=factor(month,levels=as.character(3:10)),
         daytype=factor(daytype))%>%
  droplevels()%>%
  drop_na()

################################################################################
## Set up a generalized linear mixed model
## Random effects are impd and year
## Fixed effects are daytype (weekday or weekend), month, discharge and month:dischage
## Data are going to be overdispersed (lots of zero and low counts)
## Specify a Tweedie distribution since data have a lot of zeroes and are overdispersed
## Use effects coding so parameters are relative to the grand mean instead of a reference value
mod_eff=glmmTMB(totangs~ln_cfs*month+
                  daytype+
                  (1|impd)+(1|year),
                data=out,
                family=tweedie(link="log"),
                contrasts=list(daytype=contr.sum,
                               month=contr.sum))

## Model diagnostics
summary(mod_eff)
r2_nakagawa(mod_eff)

################################################################################
## Set up data for plotting
## Calculate mean ln_cfs so we test monthly effects at average flow across all levels
mean_flow=mean(out$ln_cfs,na.rm=TRUE)

## Look at angling effort intercept in each month at mean flow
## P-value from Wald/Z-test to test if month intercept differs from mean
month_eff=contrast(emmeans(mod_eff, ~month,at=list(ln_cfs=mean_flow)),
                    method="eff")%>%
  summary(infer=T,adjust="none")%>%
  as.data.frame()%>%
  mutate(metric="Monthly effort at mean discharge",
         term=c("March","April","May","June","July","August","September","October"),
         term=factor(term,
                     levels=c("March","April","May","June","July","August","September","October")))

## Look at angling effort intercept in each daytype at mean flow
## P-value from Wald/Z-test to test if daytype intercept differs from mean
## Note that since there are only two levels, this is essentially one t-test
day_eff=contrast(emmeans(mod_eff, ~daytype,at=list(ln_cfs=mean_flow)),
                 method="eff")%>%
  summary(infer=T,adjust="none")%>%
  as.data.frame()%>%
  mutate(metric="Day of week effect",
         term=c("Weekday","Weekend"),
         term=factor(term,levels=c("Weekday","Weekend")))

## Look at slope of angler effort to flow by month
## P-value from Wald/Z-test to test if monthly response to cfs differs from mean
slope_eff=contrast(emtrends(mod_eff, ~month,var="ln_cfs"),
                   method="eff")%>%
  summary(infer=T,adjust="none")%>%
  as.data.frame()%>%
  mutate(metric="Monthly effect of discharge on effort",
         term=c("March","April","May","June","July","August","September","October"),
         term=factor(term,
                     levels=c("March","April","May","June","July","August","September","October")))

## Combine into single data frame for plotting and exponentiate
## This will give us values that are relative to the grand mean (1)
## A value of 0.8 represents 20% decrease whereas a value of 1.2 represents a 20% increase
plot_dat=rbind(month_eff,day_eff,slope_eff)%>%
  mutate(across(c(estimate,asymp.LCL,asymp.UCL),exp))%>%
  mutate(across(where(is.numeric), ~round(.,4)))%>%
  mutate(metric=factor(metric,levels=c("Monthly effort at mean discharge",
                                       "Day of week effect",
                                       "Monthly effect of discharge on effort")),
         sig=case_when(p.value<0.05 ~ "sig",
                       TRUE ~ "nosig"))

################################################################################
## Create plots to see relative effect sizes
month_eff_plot=ggplot(plot_dat)+
  geom_vline(xintercept=1,linetype="dashed",color="gray50")+
  geom_pointrange(aes(x=estimate,y=fct_rev(term),xmin=asymp.LCL,xmax=asymp.UCL,color=sig),
                  size=2,linewidth=1.2)+
  scale_color_manual(values=c("gray","black"),
                     guide="none")+
  scale_y_discrete(name="")+
  scale_x_continuous(breaks=seq(0.6,1.6,0.2),
                     name="Relative effect ratio")+
  facet_wrap(~metric,ncol=1,scales="free_y")+
  pubtheme

ggsave(plot=month_eff_plot,"effort_month.png",height=10,width=7,bg="white")

################################################################################
## Extract model coefficients for appendix
## 1. Base model terms and global slope
# Extract the intercept and the main effect of discharge (ln_cfs)
part1_base_eff=tidy(mod_eff)%>%
  filter(term %in% c("(Intercept)","ln_cfs"))%>%
  select(Parameter=term,Estimate=estimate,SE=std.error,P_value=p.value)%>%
  mutate(Parameter=case_when(Parameter=="(Intercept)" ~ "Intercept",
                             Parameter=="ln_cfs" ~ "Discharge (cfs)",
                             TRUE ~ Parameter))

## 2. Categorical main effects (intercept shifts)
# Use emmeans to recover the deviation from grand mean for all levels (including last ones)
part2_main_month_eff=contrast(emmeans(mod_eff, ~month),method="eff")%>%
  summary(infer=c(T,T))%>%
  as_tibble()%>%
  mutate(Parameter=paste("Intercept Shift:",contrast))

part2_main_day_eff=contrast(emmeans(mod_eff, ~daytype),method="eff")%>%
  summary(infer=c(T,T))%>%
  as_tibble()%>%
  mutate(Parameter=paste("Intercept Shift:",contrast))

## 3. Interaction slopes (discharge effect by month)
# Use emtrends to get the specific slope for every month
part3_slopes_eff=emtrends(mod_eff, ~month,var="ln_cfs")%>%
  summary(infer=c(T,T))%>%
  as_tibble()%>%
  mutate(Parameter=paste("Slope: Discharge -",month))%>%
  rename(Estimate=ln_cfs.trend,P_value=p.value)

## 4. Random effects and dispersion
# Random intercept SDs
tmp=tidy(mod_eff,effects="ran_pars")%>%
  mutate(Parameter=case_when(group=="impd" ~ "Location random effect SD",
                             group=="year" ~ "Year random effect SD",
                             TRUE ~ group))%>%
  select(Parameter,Estimate=estimate)

# Tweedie dispersion
tmp1=tibble(Parameter="Tweedie Dispersion (phi)",
            Estimate=sigma(mod_eff))

# Combine
part4_random_eff=bind_rows(tmp,tmp1)%>%
  mutate(SE=NA_real_,
         P_value=NA_real_)

## 5. Combine for appendix
# Big step in here to replace numbers with month names
eff_app=bind_rows(part1_base_eff,
                  part2_main_month_eff%>%
                    select(Parameter,Estimate=estimate,SE,P_value=p.value),
                  part2_main_day_eff%>%
                    select(Parameter,Estimate=estimate,SE,P_value=p.value),
                  part3_slopes_eff%>%
                    select(Parameter,Estimate,SE,P_value=P_value),
                  part4_random_eff%>%
                    select(Parameter,Estimate,SE,P_value))%>%
  mutate(Parameter=Parameter%>%
           str_replace("month3","March")%>%
           str_replace("month4","April")%>%
           str_replace("month5","May")%>%
           str_replace("month6","June")%>%
           str_replace("month7","July")%>%
           str_replace("month8","August")%>%
           str_replace("month9","September")%>%
           str_replace("month10","October")%>%
           str_replace("- 3$","- March")%>%
           str_replace("- 4$","- April")%>%
           str_replace("- 5$","- May")%>%
           str_replace("- 6$","- June")%>%
           str_replace("- 7$","- July")%>%
           str_replace("- 8$","- August")%>%
           str_replace("- 9$","- September")%>%
           str_replace("- 10$","- October"))%>%
  mutate(P=case_when(is.na(P_value) ~ "--",
                     P_value < 0.001 ~ "< 0.001",
                     TRUE ~ sprintf("%.3f",P_value)))%>%
  select(Parameter,Estimate,SE,P)

## Export
export(eff_app,"effort model summary.xlsx")