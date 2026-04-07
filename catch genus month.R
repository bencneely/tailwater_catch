#--------------------------------------------------------------
#Ben Neely
#01/26/2026
#Model catch rate as a function of discharge
#This is based on summarized monthly data since daily catch data aren't available
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

if("ggeffects" %in% rownames(installed.packages()) == FALSE) {install.packages("ggeffects")}
library(ggeffects)

if("rio" %in% rownames(installed.packages()) == FALSE) {install.packages("rio")}
library(rio)

if("emmeans" %in% rownames(installed.packages()) == FALSE) {install.packages("emmeans")}
library(emmeans)

if("broom.mixed" %in% rownames(installed.packages()) == FALSE) {install.packages("broom.mixed")}
library(broom.mixed)

if("performance" %in% rownames(installed.packages()) == FALSE) {install.packages("performance")}
library(performance)

if("glmmTMB" %in% rownames(installed.packages()) == FALSE) {install.packages("glmmTMB")}
library(glmmTMB)

if("tidyverse" %in% rownames(installed.packages()) == FALSE) {install.packages("tidyverse")}
library(tidyverse)

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
fish=read_csv("data cleanup/fish_genus_clean.csv")

################################################################################
## First, let's summarize discharge at a monthly scale to match creel data
## Median cfs - central measure of discharge
discharge_med=discharge%>%
  filter(cfs>=0)%>%
  group_by(impd,month,year)%>%
  summarize(med_cfs=median(cfs),
            .groups="drop")%>%
  select(impd,month,year,med_cfs)

## Richards-Baker flashiness index - measure of discharge variability
discharge_rbfi=discharge%>%
  filter(cfs>=0)%>%
  group_by(impd,month,year)%>%
  summarize(rbfi_num=sum(abs(diff(cfs))),
            rbfi_denom=sum(cfs),
            rbfi=rbfi_num/rbfi_denom,
            .groups="drop")%>%
  select(impd,month,year,rbfi)

## Combine to get monthly summarized discharge
discharge1=discharge_med%>%
  inner_join(discharge_rbfi,by=c("impd","month","year"))

################################################################################
## Clean up fish data just a bit to get rid of outliers and focus on total catch data
## We have a few missing catch rates so we'll get rid of those records
## Data are here to look at targeted catch and preferred taxa but are filtered out
fish1=fish%>%
  filter(anghrs>=1,
         catch_rate<=30,
         year<2020 | year>2022,
         month %in% 3:10)%>%
  select(intid=interview_id,impd,month,year,genus,count,catch_rate,target)

################################################################################
## Now combine monthly fish data and discharge data
## Ensure month is a factor
out=fish1%>%
  inner_join(discharge1,
             by=c("impd","month","year"))%>%
  mutate(month=factor(month,
                      levels=3:10,
                      labels=c("March","April","May","June","July",
                               "August","September","October")))

## We're going to use a Tweedie distribution so zeroes in catch_rate are fine
## We do need to scale the two discharge variables though for modeling
## First step is to save means and sd from all values for later back-calculation
scaling_key=out%>%
  summarise(across(c(med_cfs,rbfi), 
                   list(mean=~mean(.x,na.rm=T), 
                        sd=~sd(.x,na.rm=T))))

## Now scale the discharge variables and make them numeric for a flat dataframe
moddat=out%>%
  mutate(across(c(med_cfs,rbfi),
                ~as.numeric(scale(.x)),
                .names="{.col}_s"))

## Check correlations
cr_cor=cor(moddat%>%
             select(med_cfs_s,rbfi_s),
           use="pairwise.complete.obs",
           method="pearson")
cr_cor
## Looks reasonable, correlation is 0.18
## Predictor variables are good to go

################################################################################
## Clean up the variables a bit. We don't need 20 genera
## Now we'll get rid of most of the genera
## Look at cumulative percentage of each
xtabs(count~genus,moddat)%>%
  as_tibble()%>%
  arrange(desc(n))%>%
  mutate(cum_pct=cumsum(n)/sum(n)*100)
## Pomoxis, Morone, Ictalurus, Aplodinotus, and Sander make up 86% of all caught fish
keeps=c("Pomoxis","Morone","Ictalurus","Aplodinotus","Sander")

## Filter out the less captured genera
moddat1=moddat%>%
  filter(genus %in% keeps)%>%
  mutate(genus=factor(genus))

## Sanity checking to make sure the folks that caught zero fish were retained
moddat1%>%
  group_by(intid)%>%
  filter(sum(count)==0)%>%
  ungroup()

################################################################################
## Set up the model using month and reduced genera
## Run generalized linear mixed model with Tweedie distribution
mod_red=glmmTMB(catch_rate~target+
                  month*(med_cfs_s+
                           rbfi_s)+
                  genus*(med_cfs_s+
                           rbfi_s)+
                  (1|impd)+(1|year),
                data=moddat1,
                family=tweedie(link="log"),
                contrasts=list(month=contr.sum,
                               genus=contr.sum),
                control=glmmTMBControl(optCtrl=list(iter.max=1000,eval.max=1000)))

summary(mod_red)
r2_nakagawa(mod_red)

################################################################################
## Let's extract slopes so we can visualize month and discharge
slps_month_cfs=emtrends(mod_red, 
                        ~month,var="med_cfs_s",
                        at=list(target=1))%>%
  summary(infer=c(T,T))%>%
  as_tibble()%>%
  rename(coef=med_cfs_s.trend)%>%
  mutate(parm="Median cfs")

slps_month_rbfi=emtrends(mod_red,
                         ~month,var="rbfi_s",
                         at=list(target=1))%>%
  summary(infer=c(T,T))%>%
  as_tibble()%>%
  rename(coef=rbfi_s.trend)%>%
  mutate(parm="Richards-Baker flashiness index")

## Combine monthly slope estimates for plotting
slps_month=bind_rows(slps_month_cfs,slps_month_rbfi)%>%
  mutate(parm=factor(parm,levels=c("Median cfs","Richards-Baker flashiness index")),
         sig=case_when(p.value<0.05 ~ "sig",
                       TRUE ~ "nosig"))

## Plot monthly effect slope estimates
month_plot=ggplot(slps_month,aes(x=coef,y=fct_rev(month)))+
  geom_vline(xintercept=0,linetype="dashed",color="gray50")+
  geom_pointrange(aes(xmin=asymp.LCL,xmax=asymp.UCL,color=sig),size=2,linewidth=1.2)+
  scale_color_manual(values=c("gray","black"))+
  scale_y_discrete(name="")+
  scale_x_continuous(breaks=seq(-0.6,1.4,0.2),
                     labels=function(x) sprintf("%.1f",x),
                     name="Standardized coefficient estimate")+
  coord_cartesian(xlim=c(-0.53,1.22),
                  ylim=c(0.5,8.5),
                  expand=F)+
  facet_wrap(~parm,ncol=1,scales="fixed")+
  pubtheme

################################################################################
## Now we'll visualize genus effects on catch rate
## Extract slopes for each genus (averaged across months)
slps_genus_cfs=emtrends(mod_red, 
                        ~genus, var="med_cfs_s",
                        at=list(target=1))%>%
  summary(infer=c(T,T))%>%
  as_tibble()%>%
  rename(coef=med_cfs_s.trend)%>%
  mutate(parm="Median cfs")

slps_genus_rbfi=emtrends(mod_red, 
                         ~genus, var="rbfi_s",
                         at=list(target=1))%>%
  summary(infer=c(T,T))%>%
  as_tibble()%>%
  rename(coef=rbfi_s.trend)%>%
  mutate(parm="Richards-Baker flashiness index")

## Combine general slopes for plotting
slps_genus=bind_rows(slps_genus_cfs,slps_genus_rbfi)%>%
  mutate(parm=factor(parm,levels=c("Median cfs","Richards-Baker flashiness index")),
         sig=case_when(p.value<0.05 ~ "sig",
                       TRUE ~ "nosig"))

## Plot genera effect slope estimates
genus_plot=ggplot(slps_genus,aes(x=coef,y=fct_rev(genus)))+
  geom_vline(xintercept=0,linetype="dashed",color="gray50")+
  geom_pointrange(aes(xmin=asymp.LCL,xmax=asymp.UCL,color=sig),size=2,linewidth=1.2)+
  scale_color_manual(values=c("gray","black"))+
  scale_y_discrete(name="")+
  scale_x_continuous(breaks=seq(-0.3,0.6,0.1),
                     labels=function(x) sprintf("%.1f",x),
                     name="Standardized coefficient estimate")+
  coord_cartesian(xlim=c(-0.23,0.42),
                  ylim=c(0.5,5.5),
                  expand=F)+
  facet_wrap(~parm,ncol=1,scales="fixed")+
  pubtheme

################################################################################
## Combine plots and export
out=month_plot|genus_plot
ggsave(plot=out,"genus_month.png",height=7,width=14,bg="white")

################################################################################
## Extract model coefficients for appendix
## 1. Base model terms (intercept and target)
part1_base=tidy(mod_red)%>%
  filter(term %in% c("(Intercept)","target","med_cfs_s","rbfi_s"))%>%
  select(Parameter=term,Estimate=estimate,SE=std.error,P_value=p.value)%>%
  mutate(Parameter=case_when(Parameter=="(Intercept)" ~ "Intercept",
                             Parameter=="target" ~ "Target genus",
                             Parameter=="med_cfs_s" ~ "Median cfs",
                             Parameter=="rbfi_s" ~ "Richards-Baker flashiness index"))

## 2. Categorical main effects (intercept shifts)
# Uses emmeans to get deviation from grand mean for all levels (recovering the missing one)
part2_main_month=contrast(emmeans(mod_red, ~month),method="eff")%>%
  summary(infer=c(T,T))%>%
  as_tibble()%>%
  mutate(Parameter=paste("Intercept Shift:",contrast))

part2_main_genus=contrast(emmeans(mod_red, ~genus),method="eff")%>%
  summary(infer=c(T,T))%>%
  as_tibble()%>%
  mutate(Parameter=paste("Intercept Shift:",contrast))

## 3. Interaction slopes (The detailed discharge relationships for month and genus)
# Uses the slope objects created for plots
part3_slopes=bind_rows(slps_month,slps_genus)%>%
  mutate(Group=coalesce(as.character(month),as.character(genus)),
         Parameter=paste("Slope:",parm,"-",Group))%>%
  rename(Estimate=coef,P_value=p.value)

## 4. Extract random effects
# Get random intercept SDs
tmp=tidy(mod_red,effects="ran_pars")%>%
  mutate(Parameter=case_when(group=="impd" ~ "Location random effect SD",
                             group=="year" ~ "Year random effect SD",
                             TRUE ~ group))%>%
  select(Parameter,Estimate=estimate)

# Manually extract Tweedie dispersion
tmp1=tibble(Parameter="Tweedie dispersion (phi)",
            Estimate=sigma(mod_red))

# Combine them to show random effect intercept SDs and Tweedie dispersion
part4_random=bind_rows(tmp,tmp1)%>%
  mutate(SE=NA_real_,
         P_value=NA_real_)%>%
  select(Parameter,Estimate,SE,P_value)

## 4. Combine for appendix
catch_app=bind_rows(part1_base,
                    part2_main_month%>%
                      select(Parameter,Estimate=estimate,SE=SE,P_value=p.value),
                    part2_main_genus%>%
                      select(Parameter,Estimate=estimate,SE=SE,P_value=p.value),
                    part3_slopes%>%
                      select(Parameter,Estimate,SE,P_value=P_value),
                    part4_random%>%
                      select(Parameter,Estimate,SE,P_value))%>%
  mutate(P=case_when(P_value<0.001 ~ "< 0.001",
                     TRUE ~ sprintf("%.3f",P_value)))%>%
  select(-P_value)

## Export
export(catch_app,"catch rate model summary.xlsx")