#==================================================================================================
# Title:                 Anaerobic Digester Calculation Tool
# Date:                  3 February 2024
#==================================================================================================
rm(list=ls())
library(ggplot2)
load("GENResultsData.RData")
#--------------------------------------------------------------------------------------------------
output$egratio                          = (output$pelc/0.0036)/(output$pgas/1.055056)
output$situation                        = NA
output$situation[which(output$mms=="ANL" & output$temperature==18)] = "Anaerobic Lagoon 18C"
output$situation[which(output$mms=="PIT" & output$temperature==9)]  = "Deep Pit 9C"
output$irr[which(is.na(output$irr))]    = -1
#--------------------------------------------------------------------------------------------------
# Internal Rate of Return for a Complete Mix AD with Corn Stover with the following base values.
# - Electricity Price:   0.14
# - Natural Gas Price:   4
# - RIN Value:           3
#--------------------------------------------------------------------------------------------------
output                   = subset(output,situation %in% c("Anaerobic Lagoon 18C","Deep Pit 9C"))
output                   = subset(output,codigestate=="CORN STOVER" & ad=="COMPLETE MIX")
#--------------------------------------------------------------------------------------------------
dfbase                   = subset(output,pelc==0.14 & pgas==4 & prin==3)
dfbase                   = dfbase[!(names(dfbase) %in% c("pelc","pgas","prin","codigestate","ad"))]
dfl                      = subset(output,pelc==0.10 & pgas==5 & prin==3)
dfl                      = dfl[!(names(dfl) %in% c("pelc","pgas","prin","codigestate","ad"))]
dfh                      = subset(output,pelc==0.18 & pgas==3 & prin==3)
dfh                      = dfh[!(names(dfh) %in% c("pelc","pgas","prin","codigestate","ad"))]
#--------------------------------------------------------------------------------------------------
breakscale               = c(-Inf,seq(0,0.18,0.02))
customcolors             = c("#FFDDDD","#DFF0D8","#C1E2B3","#A3D38F","#85C46A","#67B546","#489621",
                             "#307D0C","#1E5F00","#0D4100")
customlabels             = c("Not Profitable","0%-2%","2%-4%","4%-6%","6%-8%","8%-10%","10%-12%",
                             "12%-14%","14%-16%","16%-18%")
#--------------------------------------------------------------------------------------------------
df                       = subset(dfbase,situation=="Anaerobic Lagoon 18C")
ggplot(df,aes(x=head,y=pcarbon,z=irr))+
     geom_contour_filled(breaks=breakscale)+
     scale_fill_manual(values=customcolors,labels=customlabels)+
     facet_grid(vars(usename),vars(farmtype))+theme_bw()+
     ylab("Carbon Price (in USD per Metric Ton of CO2-e)")+
     xlab("Farm Size (Head)")+theme(legend.position="bottom",legend.title=element_blank())
#ggsave("figANL.pdf",height=8,width=6.5)
#--------------------------------------------------------------------------------------------------
df                       = subset(dfbase,situation=="Deep Pit 9C" & lifetime==30)
ggplot(df,aes(x=head,y=pcarbon,z=irr))+
     facet_grid(vars(usename),vars(farmtype))+theme_bw()+
     geom_contour_filled(breaks=breakscale)+
     scale_fill_manual(values=customcolors,labels=customlabels)+
     ylab("Carbon Price (in USD per Metric Ton of CO2-e)")+
     xlab("Farm Size (Head)")+theme(legend.position="bottom",legend.title=element_blank())
#ggsave("figPIT.pdf",height=8,width=6.5)
#--------------------------------------------------------------------------------------------------
dfbase$item              = "Base Ratio"
dfl$item                 = "Low Elec., High NG"
dfh$item                 = "High Elec., Low NG"
df                       = rbind(dfbase,dfl,dfh)
df                       = subset(df,pcarbon %in% c(0,50,100))
df$irr                   = df$irr*100
df$pcarbon               = paste("USD",df$pcarbon)
rm(dfbase,dfl,dfh)
#--------------------------------------------------------------------------------------------------
# Carbon Price
#--------------------------------------------------------------------------------------------------
temp                     = df[!(names(df) %in% c("use","mms","egratio","temperature"))]
tempbase                 = subset(temp,item=="Base Ratio")
tempmin                  = aggregate(temp$irr,FUN=min,
                                     by=list(temp$farmtype,temp$head,temp$pcarbon,temp$situation,
                                             temp$lifetime))
colnames(tempmin)        = c("farmtype","head","pcarbon","situation","lifetime","min")
tempmax                  = aggregate(temp$irr,FUN=max,
                                     by=list(temp$farmtype,temp$head,temp$pcarbon,temp$situation,
                                             temp$lifetime))
colnames(tempmax)        = c("farmtype","head","pcarbon","situation","lifetime","max")
temprange                = merge(tempmin,tempmax,by=c("farmtype","head","pcarbon","situation",
                                                      "lifetime"))
rm(temp,tempmin,tempmax)
#--------------------------------------------------------------------------------------------------
tempbase                 = subset(tempbase,lifetime==30)
tempbase$irr[which(tempbase$irr<0)] = 0
temprange                = subset(temprange,lifetime==30)
temprange$min[which(temprange$min<0)] = 0
temprange$max[which(temprange$max<0)] = 0
#--------------------------------------------------------------------------------------------------
ggplot(mapping=aes(x=head))+
     geom_ribbon(aes(ymin=min,ymax=max,fill=pcarbon),temprange,alpha=0.25)+     
     geom_line(aes(y=irr,color=pcarbon,linetype=usename),tempbase)+
     facet_grid(vars(farmtype),vars(situation))+
     theme_bw()+ylab("Internal Rate of Return (in %)")+ylim(0,18)+xlab("Farm Size (Head)")+
     theme(legend.title=element_blank(),legend.position="right")
#ggsave("carbonpriceuse.pdf",width=8,height=5)
#==================================================================================================
# End of File
#==================================================================================================