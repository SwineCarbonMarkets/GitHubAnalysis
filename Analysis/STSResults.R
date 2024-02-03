#==================================================================================================
# Title:                 Anaerobic Digester Calculation Tool
# Date:                  3 February 2024
#==================================================================================================
rm(list=ls())
library(ggplot2)
load("STSResultsData.RData")
#--------------------------------------------------------------------------------------------------
sdf                      = subset(sdf,head==6000 & pcarbon==50 & lifetime==30,
                                  select=c("state","usename","farmtype","irr","level"))
#--------------------------------------------------------------------------------------------------
ggplot(sdf,aes(x=state,y=irr*100,fill=usename))+
     geom_bar(stat="identity",position=position_dodge())+
     facet_grid(vars(level),vars(farmtype))+theme_bw()+ylab("Internal Rate of Return (%)")+
     theme(legend.position="bottom",legend.title=element_blank(),axis.title.x=element_blank(),
           axis.text.x=element_text(angle=45,hjust=1))
#ggsave("figstates.pdf",height=8,width=6.5)
#==================================================================================================
# End of File
#==================================================================================================