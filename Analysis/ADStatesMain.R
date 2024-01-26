#==================================================================================================
# Title:                 Anaerobic Digester Calculations - States
# Date:                  26 January 2024
#==================================================================================================
rm(list=ls())
library(FinCal)
load("ADData.RData")
#--------------------------------------------------------------------------------------------------
# Parameters
#--------------------------------------------------------------------------------------------------
gwpch4                   = 28
gwpn2o                   = 265
energycontentbiogas      = 6.5     # kWh/m3       AD Screening Tool V2.2
energycontentmethane     = 10      # kWh/m3       AD Screening Tool V2.2
pm                       = data.frame(gwpch4,gwpn2o,energycontentbiogas,energycontentmethane)
pm$sharecs               = 0.0714  # Corn stover share
pm$sharews               = 0.0256  # Wheat straw share
pm$pricedigestate        = 35      # Biomass price
pm$pricecodigestate      = 20      # Digestate price
pm$co2mtu                = 0.084
pm$co2rec                = 0.091
pm$co2cng                = 0.051
pm$co2upi                = 0.042
#--------------------------------------------------------------------------------------------------
# Scenario Data Frame
#--------------------------------------------------------------------------------------------------
carbonprice              = data.frame(pcarbon=seq(0,100,25))
lifetime                 = data.frame(lifetime=c(15,20,30))
sdf                      = merge(sdf,carbonprice) 
sdf                      = merge(sdf,lifetime)
#--------------------------------------------------------------------------------------------------
sdf$pgasgj               = sdf$pgas/1.055056
sdf$pelcgj               = sdf$pelc/1.055056
sdf$pringj               = sdf$prin/1.055056
rm(gwpch4,gwpn2o,energycontentbiogas,energycontentmethane)
#--------------------------------------------------------------------------------------------------
# Carbon Farm (cfarm) and Manure Pathways
# - MMS: (1) ANL, (2) SLU, (3) PIT, (4) PT1
# - AD: (1) Covered Lagoon, (2) Complete Mix, (3) Plug Flow
# - USE: (1) MTU, (2) REC, (3) CNG, (4) UPI
#--------------------------------------------------------------------------------------------------
use                      = c("MTU","REC","CNG","UPI")
farmtype                 = c("BREEDING","MARKET")
head                     = seq(1000,8000,500)
cfarm                    = expand.grid(use,farmtype,head,stringsAsFactors=FALSE)
colnames(cfarm)          = c("use","farmtype","head")
sdf                      = merge(sdf,cfarm)
rm(use,carbonprice,farmtype,head,cfarm)
#--------------------------------------------------------------------------------------------------
# Calculating GHG Emissions from Current MMS and AD System
# - funch4mms returns Metric Tons of CO2-e
# - funch4ad returns Metric Tons of CO2-e
#--------------------------------------------------------------------------------------------------
sdf$ch4mms               = NA
sdf$ch4ad                = NA
#--------------------------------------------------------------------------------------------------
for(i in 1:nrow(sdf)){
     sdf$ch4mms[i]= funch4mms(sdf$head[i],sdf$farmtype[i],sdf$mms[i],sdf$temperature[i])
     sdf$ch4ad[i] = funch4ad(sdf$head[i],sdf$farmtype[i],sdf$ad[i])}
#--------------------------------------------------------------------------------------------------
# Calculating Co-Digestate Required as well as Biogas and Digestate Production
# - Co-digestate: Corn stover, wheat straw, Switchgrass (not yet included)
#--------------------------------------------------------------------------------------------------
sdf$qcodigestate         = NA
sdf$m3biogasperyr        = NA
sdf$digestateperyr       = NA
#--------------------------------------------------------------------------------------------------
for(i in 1:nrow(sdf)){
     temp                     = funadbiogas(sdf$head[i],sdf$farmtype[i],sdf$codigestate[i],
                                            sdf$temperature[i])
     sdf$qcodigestate[i]    = temp[1]
     sdf$m3biogasperyr[i]   = temp[2]
     sdf$digestateperyr[i]  = temp[3]}
#--------------------------------------------------------------------------------------------------
# Biogas Use
#--------------------------------------------------------------------------------------------------
usedata                  = subset(biogasuse,use=="MTU")
bhat                     = lm(costcapital~capacitym3yr,data=usedata)
pm$mtu_capital_b1        = bhat$coefficients[1]
pm$mtu_capital_b2        = bhat$coefficients[2]
bhat                     = lm(costom~0+capacitym3yr,data=usedata)
pm$mtu_om                = bhat$coefficients[1]
bhat                     = lm(gjperyr~0+capacitym3yr,data=usedata)
pm$mtu_output            = bhat$coefficients[1]
#--------------------------------------------------------------------------------------------------
usedata                  = subset(biogasuse,use=="REC")
bhat                     = lm(costcapital~capacitym3yr,data=usedata)
pm$rec_capital_b1        = bhat$coefficients[1]
pm$rec_capital_b2        = bhat$coefficients[2]
bhat                     = lm(costom~0+capacitym3yr,data=usedata)
pm$rec_om                = bhat$coefficients[1]
bhat                     = lm(gjperyr~0+capacitym3yr,data=usedata)
pm$rec_output            = bhat$coefficients[1]
#--------------------------------------------------------------------------------------------------
usedata                  = subset(biogasuse,use=="CNG")
bhat                     = lm(costcapital~capacitym3yr,data=usedata)
pm$cng_capital_b1        = bhat$coefficients[1]
pm$cng_capital_b2        = bhat$coefficients[2]
bhat                     = lm(costom~0+capacitym3yr,data=usedata)
pm$cng_om                = bhat$coefficients[1]
bhat                     = lm(gjperyr~0+capacitym3yr,data=usedata)
pm$cng_output            = bhat$coefficients[1]
#--------------------------------------------------------------------------------------------------
usedata                  = subset(biogasuse,use=="UPI")
bhat                     = lm(costcapital~capacitym3yr,data=usedata)
pm$upi_capital_b1        = bhat$coefficients[1]
pm$upi_capital_b2        = bhat$coefficients[2]
bhat                     = lm(costom~0+capacitym3yr,data=usedata)
pm$upi_om                = bhat$coefficients[1]
bhat                     = lm(gjperyr~0+capacitym3yr,data=usedata)
pm$upi_output            = bhat$coefficients[1]
#--------------------------------------------------------------------------------------------------
rm(biogasuse,bhat,usedata,i,feedstock,wastechar,temp)
#--------------------------------------------------------------------------------------------------
sdf$product                             = NA
sdf$product[which(sdf$use=="MTU")]      = "Electricity"
sdf$product[which(sdf$use=="REC")]      = "Electricity"
sdf$product[which(sdf$use=="CNG")]      = "Natural Gas"
sdf$product[which(sdf$use=="UPI")]      = "Natural Gas"
sdf$irr                                 = NA
#--------------------------------------------------------------------------------------------------
for(i in 1:nrow(sdf)){
     tempcfarm                = sdf[i,]
     tempcfarm$capitalad      = funadcapitalcost(tempcfarm$m3biogasperyr)
     tempcfarm$revdigestate   = tempcfarm$digestateperyr*pm$pricedigestate
     tempcfarm$omcodigestate  = tempcfarm$qcodigestate*pm$pricecodigestate
     tempcfarm$revcarbon      = (tempcfarm$ch4mms-tempcfarm$ch4ad)*tempcfarm$pcarbon
     tempcfarm$capitalbguse   = NA
     tempcfarm$ombguse        = NA
     tempcfarm$revenergy      = NA
     tempcfarm$co2use         = NA
     bguse                    = funcostuse(tempcfarm$m3biogasperyr,tempcfarm$use,tempcfarm)
     tempcfarm$capitalbguse   = bguse[1]
     tempcfarm$ombguse        = bguse[2]
     tempcfarm$revenergy      = bguse[3]
     tempcfarm$co2use         = bguse[4]
     tempcfarm$profit         = tempcfarm$revcarbon+tempcfarm$revdigestate+tempcfarm$revenergy-
                                   tempcfarm$ombguse-tempcfarm$omcodigestate-
                                   tempcfarm$co2use*tempcfarm$pcarbon-
                                   0.05*tempcfarm$capitalad
     tempcfarm$investment     = tempcfarm$capitalad+tempcfarm$capitalbguse
     if(tempcfarm$profit>2000){
          tempcfarm$irr = irr(c(-tempcfarm$investment,rep(tempcfarm$profit,tempcfarm$lifetime)))}
     sdf$irr[i]               = tempcfarm$irr
     print(i)}
#--------------------------------------------------------------------------------------------------
uses                     = data.frame(use=c("CNG","MTU","REC","UPI"),
                                      usename=c("CNG","Microturbine","Reciprocating Engine","RNG"))
sdf                      = merge(sdf,uses,by=c("use"))
#--------------------------------------------------------------------------------------------------
sdf                      = sdf[c("state","usename","level","pcarbon","lifetime","farmtype",
                                 "head","irr")]
rm(list=setdiff(ls(),"sdf"))
#--------------------------------------------------------------------------------------------------
save.image("ADStatesResultsData.RData")
#--------------------------------------------------------------------------------------------------
# Optimal End-Use by State
#--------------------------------------------------------------------------------------------------
enduses             = unique(sdf[c("state","head","pcarbon","farmtype","lifetime","level")])
enduses$use         = NA
#--------------------------------------------------------------------------------------------------
for(i in 1:nrow(enduses)){
     df   = subset(sdf,state==enduses$state[i] & head==enduses$head[i] & 
                        pcarbon==enduses$pcarbon[i] & farmtype==enduses$farmtype[i] & 
                        level==enduses$level[i] & lifetime==enduses$lifetime[i])
     if(length(df$use[which.max(df$irr)])>0){
          enduses$use[i] = df$use[which.max(df$irr)]}}
#--------------------------------------------------------------------------------------------------
enduses             = na.omit(enduses)
#==================================================================================================
# End of File
#==================================================================================================