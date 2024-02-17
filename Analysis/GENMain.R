#==================================================================================================
# Title:                 Anaerobic Digester Calculations: Generic
# Date:                  17 February 2024
#==================================================================================================
rm(list=ls())
library(FinCal)
library(openxlsx)
load("ADData.RData")
#--------------------------------------------------------------------------------------------------
# Parameters
#--------------------------------------------------------------------------------------------------
gwpch4                   = 28
gwpn2o                   = 265
energycontentbiogas      = 6.5     # kWh/m3       AD Screening Tool V2.2
energycontentmethane     = 10      # kWh/m3       AD Screening Tool V2.2
hpyr                     = 7446    # Operating hours per year (85% of 8760 total hours per year)
pm                       = data.frame(hpyr,gwpch4,gwpn2o,energycontentbiogas,energycontentmethane)
pm$sharecs               = 0.0714  # Corn stover share
pm$sharews               = 0.0256  # Wheat straw share
pm$pricedigestate        = 35      # Biomass price
pm$pricecodigestate      = 20      # Digestate price
pm$co2mtu                = 0.084
pm$co2rec                = 0.091
pm$co2cng                = 0.051
pm$co2upi                = 0.042
#--------------------------------------------------------------------------------------------------
# Energy prices
#    Converting all parameters in GJ
#         - 1 million BTU = 1.0550559 GJ
#         - 1 kWh = 0.0036 GJ
#         - 1 GEE = 0.080532413 GJ (76330 and 84530 BTU per Gallon as the LHV and HHV, respectiv.)
#         For GEE, see https://afdc.energy.gov/fuels/properties
# SCENARIO DATA FRAME
#--------------------------------------------------------------------------------------------------
pgas                     = c(3,4,5)               # Price of natural gas in $ per million BTU
pelc                     = c(0.10,0.14,0.18)      # Price of electricity in $ per kWh
prin                     = 3                      # RIN value in $ per gal. of ethanol equiv. (GEE)
carbonprice              = seq(0,100,25)
lifetime                 = c(30)
sdf                      = expand.grid(pgas,pelc,prin,carbonprice,lifetime) 
colnames(sdf)            = c("pgas","pelc","prin","pcarbon","lifetime") 
#--------------------------------------------------------------------------------------------------
sdf$pgasgj               = sdf$pgas/1.055056
sdf$pelcgj               = sdf$pelc/0.0036
sdf$pringj               = sdf$prin/0.080532413
rm(pgas,pelc,prin,gwpch4,gwpn2o,energycontentbiogas,energycontentmethane,hpyr)
#--------------------------------------------------------------------------------------------------
# Carbon Farm (cfarm) and Manure Pathways
# - MMS: (1) ANL, (2) SLU, (3) PIT, (4) PT1
# - AD: (1) Covered Lagoon, (2) Complete Mix, (3) Plug Flow
# - USE: (1) MTU, (2) REC, (3) CNG, (4) UPI
#--------------------------------------------------------------------------------------------------
mms                      = c("ANL","SLU","PIT","PT1")
ad                       = c("COVERED LAGOON","COMPLETE MIX","PLUG FLOW")
use                      = c("MTU","REC","CNG","UPI")
farmtype                 = c("BREEDING","MARKET")
head                     = seq(1000,8000,500)
temperature              = c(9,18)
cfarm                    = expand.grid(mms,ad,use,farmtype,head,temperature,stringsAsFactors=FALSE)
colnames(cfarm)     = c("mms","ad","use","farmtype","head","temperature")
rm(mms,ad,use,carbonprice,farmtype,head,temperature)
#--------------------------------------------------------------------------------------------------
# Calculating GHG Emissions from Current MMS and AD System
# - funch4mms returns Metric Tons of CO2-e
# - funch4ad returns Metric Tons of CO2-e
#--------------------------------------------------------------------------------------------------
cfarm$ch4mms             = NA
cfarm$ch4ad              = NA
#--------------------------------------------------------------------------------------------------
for(i in 1:nrow(cfarm)){
     cfarm$ch4mms[i]= funch4mms(cfarm$head[i],cfarm$farmtype[i],cfarm$mms[i],cfarm$temperature[i])
     cfarm$ch4ad[i] = funch4ad(cfarm$head[i],cfarm$farmtype[i],cfarm$ad[i])}
#--------------------------------------------------------------------------------------------------
codigestion              = data.frame(codigestate=c("CORN STOVER","WHEAT STRAW"))
cfarm                    = merge(cfarm,codigestion)
rm(codigestion)
#--------------------------------------------------------------------------------------------------
# Calculating Co-Digestate Required as well as Biogas and Digestate Production
# - Co-digestate: Corn stover, wheat straw, Switchgrass (not yet included)
#--------------------------------------------------------------------------------------------------
cfarm$qcodigestate       = NA
cfarm$m3biogasperyr      = NA
cfarm$digestateperyr     = NA
#--------------------------------------------------------------------------------------------------
for(i in 1:nrow(cfarm)){
     temp                     = funadbiogas(cfarm$head[i],cfarm$farmtype[i],cfarm$codigestate[i],
                                            cfarm$temperature[i])
     cfarm$qcodigestate[i]    = temp[1]
     cfarm$m3biogasperyr[i]   = temp[2]
     cfarm$digestateperyr[i]  = temp[3]}
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
cfarm$product                           = NA
cfarm$product[which(cfarm$use=="MTU")]  = "Electricity"
cfarm$product[which(cfarm$use=="REC")]  = "Electricity"
cfarm$product[which(cfarm$use=="CNG")]  = "Natural Gas"
cfarm$product[which(cfarm$use=="UPI")]  = "Natural Gas"
#--------------------------------------------------------------------------------------------------
output                   = cfarm[c("mms","ad","use","farmtype","head","temperature","codigestate")]
output$pgas              = NA
output$pelc              = NA
output$prin              = NA
output$pcarbon           = NA
output$irr               = NA
output$lifetime          = NA
output$return            = NA
output                   = output[0,]
myoutput                 = list()
#--------------------------------------------------------------------------------------------------
for(i in 1:nrow(sdf)){
     tempsdf                  = sdf[i,]
     tempcfarm                = cfarm
     tempcfarm$capitalad      = funadcapitalcost(tempcfarm$m3biogasperyr)
     tempcfarm$revdigestate   = tempcfarm$digestateperyr*pm$pricedigestate
     tempcfarm$omcodigestate  = tempcfarm$qcodigestate*pm$pricecodigestate
     tempcfarm$revcarbon      = (tempcfarm$ch4mms-tempcfarm$ch4ad)*tempsdf$pcarbon
     tempcfarm$capitalbguse   = NA
     tempcfarm$ombguse        = NA
     tempcfarm$revenergy      = NA
     tempcfarm$co2use         = NA
     for(k in 1:nrow(tempcfarm)){
          bguse               = funcostuse(tempcfarm$m3biogasperyr[k],tempcfarm$use[k],tempsdf)
          tempcfarm$capitalbguse[k]     = bguse[1]
          tempcfarm$ombguse[k]          = bguse[2]
          tempcfarm$revenergy[k]        = bguse[3]
          tempcfarm$co2use[k]           = bguse[4]}
     tempcfarm$profit         = tempcfarm$revcarbon+tempcfarm$revdigestate+tempcfarm$revenergy-
                                   tempcfarm$ombguse-tempcfarm$omcodigestate-
                                   tempcfarm$co2use*tempsdf$pcarbon-
                                   0.05*tempcfarm$capitalad
     tempcfarm$investment     = tempcfarm$capitalad+tempcfarm$capitalbguse
     tempcfarm$return         = tempcfarm$profit/tempcfarm$investment
     tempcfarm$irr            = NA
     for(k in 1:nrow(tempcfarm)){
          if(tempcfarm$profit[k]>2000){
               tempcfarm$irr[k]    = irr(c(-tempcfarm$investment[k],
                                           rep(tempcfarm$profit[k],tempsdf$lifetime)))}}
     tempcfarm                = tempcfarm[c("mms","ad","use","farmtype","head","temperature","irr",
                                            "codigestate","return")]
     tempcfarm$pgas           = tempsdf$pgas
     tempcfarm$pelc           = tempsdf$pelc
     tempcfarm$prin           = tempsdf$prin
     tempcfarm$pcarbon        = tempsdf$pcarbon 
     tempcfarm$lifetime       = tempsdf$lifetime
     myoutput[[i]]            = rbind(output,tempcfarm)
     print(i)}
output     = do.call("rbind",myoutput)
#--------------------------------------------------------------------------------------------------
uses                     = data.frame(use=c("CNG","MTU","REC","UPI"),
                                      usename=c("CNG","Microturbine","Reciprocating Engine","RNG"))
output                   = merge(output,uses,by=c("use"))
#--------------------------------------------------------------------------------------------------
rm(list=setdiff(ls(),"output"))
#--------------------------------------------------------------------------------------------------
save.image("GENResultsData.RData")
#==================================================================================================
# End of File
#==================================================================================================