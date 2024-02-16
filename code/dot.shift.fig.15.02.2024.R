library(AlleleShift)
library(tidyverse)
library(Cairo)
library(tidyr)
library(plotrix)


traits<-read.csv("data/Traits.GPS.Comp.15.02.2024.csv")

traits$above.below.dry.ratio<-as.numeric(paste0(traits$total.below.dry.mass.g))/(traits$total.above.dry.mass.g+as.numeric(paste0(traits$total.below.dry.mass.g)))
traits$cover.area<-pi*(traits$Width.90.cm.y/2)*(traits$Width.long.cm.y/2)
traits$above.volume<-pi*(traits$Width.90.cm.y/2)*(traits$Width.long.cm.y/2)*traits$Height.cm.y*(4/3)
traits$below.volume<-pi*(as.numeric(paste0(traits$Root.lateral.cm))/2)*(as.numeric(paste0(traits$Root.lateral.cm))/2)*as.numeric(paste0(traits$Root.depth.cm))*(4/3)
traits$Root.depth.cm<-as.numeric(paste0(traits$Root.depth.cm))
traits$Root.lateral.cm<-as.numeric(paste0(traits$Root.lateral.cm))


cols<-c("Year","Species","Leaf.dry.mass.g","Leaf.thickness.mm","Leaf.area.mm2","TOTAL_Reproduction",
        "Seed_mass_g.x","Height.cm.y","stem.dry.density.g.mm3","root.dry.density.g.mm3", 
        "SLA","Neighbor.Density","Shannon.index","Diversity","above.below.dry.ratio","cover.area",
        "above.volume","below.volume","LDMC.dry.wet","Root.depth.cm","Root.lateral.cm")


for (j in cols){
  if (j=="Species") {next}
  traits[,j]<-as.numeric(paste0(traits[,j]))
}
traits.sub<-traits[,which(names(traits)%in%cols)]

traits.sub.agg<-with(traits.sub,aggregate(cbind(Leaf.dry.mass.g,Leaf.thickness.mm,Leaf.area.mm2,TOTAL_Reproduction,
                                                Seed_mass_g.x,Height.cm.y,stem.dry.density.g.mm3,root.dry.density.g.mm3, 
                                                SLA,Neighbor.Density,Shannon.index,above.below.dry.ratio,cover.area, above.volume,               
                                                below.volume,LDMC.dry.wet,Diversity,Root.depth.cm,Root.lateral.cm),by=list(Year,Species),mean,na.rm=T))

traits.sub.agg.sd<-with(traits.sub,aggregate(cbind(Leaf.dry.mass.g,Leaf.thickness.mm,Leaf.area.mm2,TOTAL_Reproduction,
                                                   Seed_mass_g.x,Height.cm.y,stem.dry.density.g.mm3,root.dry.density.g.mm3, 
                                                   SLA,Neighbor.Density,Shannon.index,above.below.dry.ratio,cover.area, above.volume,               
                                                   below.volume,LDMC.dry.wet,Diversity,Root.depth.cm,Root.lateral.cm),by=list(Year,Species),sd,na.rm=T))

names(traits.sub.agg)[1:2]<-c("Year","Species")
traits.agg<-traits.sub.agg[-which(traits.sub.agg$Species=="ERCI"),]
for (i in 1:length(names(traits.agg))){
  if (any(is.nan(traits.agg[,i]))) {
    traits.agg[,i][which(is.nan(traits.agg[,i]))]<-NA
  } else {next}
}



names(traits.sub.agg.sd)[1:2]<-c("Year","Species")
traits.agg.sd<-traits.sub.agg.sd[-which(traits.sub.agg.sd$Species=="ERCI"),]
traits.agg.sd$Seed_mass_g.x[which(is.nan(traits.agg.sd$Seed_mass_g.x))]<-NA




traits.long<-gather(traits.agg,"Trait.name","mean",Leaf.dry.mass.g,Leaf.thickness.mm,Leaf.area.mm2,TOTAL_Reproduction,
                    Seed_mass_g.x,Height.cm.y,stem.dry.density.g.mm3,root.dry.density.g.mm3, 
                    SLA,Neighbor.Density,Shannon.index,above.below.dry.ratio,cover.area, above.volume,               
                    below.volume,LDMC.dry.wet,Diversity,Root.depth.cm,Root.lateral.cm)


traits.long.sd<-gather(traits.agg.sd,"Trait.name","sd",Leaf.dry.mass.g,Leaf.thickness.mm,Leaf.area.mm2,TOTAL_Reproduction,
                       Seed_mass_g.x,Height.cm.y,stem.dry.density.g.mm3,root.dry.density.g.mm3, 
                       SLA,Neighbor.Density,Shannon.index,above.below.dry.ratio,cover.area, above.volume,               
                       below.volume,LDMC.dry.wet,Diversity,Root.depth.cm,Root.lateral.cm)

traits.long.m<-merge(traits.long,traits.long.sd)
traits.long.m$LCL<-traits.long.m$mean-1*traits.long.m$sd
traits.long.m$UCL<-traits.long.m$mean+1*traits.long.m$sd
traits.long.m$C.Var<-traits.long.m$sd/traits.long.m$mean

traits.long.m$increasing<-"UNKNOWN"
for ( i in unique(traits.long.m$Species)){
  for (k in unique(traits.long.m$Trait.name)){
    if (any(is.na(traits.long.m$mean[which(traits.long.m$Species==i&traits.long.m$Trait.name==k)]))) {
      traits.long.m$increasing[which(traits.long.m$Species==i&traits.long.m$Trait.name==k)]<-"NA"
    } else if(traits.long.m$mean[which(traits.long.m$Species==i&traits.long.m$Trait.name==k&traits.long.m$Year==2020)]>
              traits.long.m$mean[which(traits.long.m$Species==i&traits.long.m$Trait.name==k&traits.long.m$Year==2021)]) {
      traits.long.m$increasing[which(traits.long.m$Species==i&traits.long.m$Trait.name==k)]<-"FALSE"
    } else if(traits.long.m$mean[which(traits.long.m$Species==i&traits.long.m$Trait.name==k&traits.long.m$Year==2020)]<
              traits.long.m$mean[which(traits.long.m$Species==i&traits.long.m$Trait.name==k&traits.long.m$Year==2021)]) {
      traits.long.m$increasing[which(traits.long.m$Species==i&traits.long.m$Trait.name==k)]<-"TRUE"
    } else {traits.long.m$increasing[which(traits.long.m$Species==i&traits.long.m$Trait.name==k)]<-"NA"}
  }
}

traits.long.m$increasing.CV<-"UNKNOWN"
for ( i in unique(traits.long.m$Species)){
  for (k in unique(traits.long.m$Trait.name)){
    if (any(is.na(traits.long.m$C.Var[which(traits.long.m$Species==i&traits.long.m$Trait.name==k)]))) {
      traits.long.m$increasing.CV[which(traits.long.m$Species==i&traits.long.m$Trait.name==k)]<-"NA"
    } else if(traits.long.m$C.Var[which(traits.long.m$Species==i&traits.long.m$Trait.name==k&traits.long.m$Year==2020)]>
              traits.long.m$C.Var[which(traits.long.m$Species==i&traits.long.m$Trait.name==k&traits.long.m$Year==2021)]) {
      traits.long.m$increasing.CV[which(traits.long.m$Species==i&traits.long.m$Trait.name==k)]<-"FALSE"
    } else if(traits.long.m$C.Var[which(traits.long.m$Species==i&traits.long.m$Trait.name==k&traits.long.m$Year==2020)]<
              traits.long.m$C.Var[which(traits.long.m$Species==i&traits.long.m$Trait.name==k&traits.long.m$Year==2021)]) {
      traits.long.m$increasing.CV[which(traits.long.m$Species==i&traits.long.m$Trait.name==k)]<-"TRUE"
    } else {traits.long.m$increasing.CV[which(traits.long.m$Species==i&traits.long.m$Trait.name==k)]<-"NA"}
  }
}

traits.long.m$increasing.sd<-"UNKNOWN"
for ( i in unique(traits.long.m$Species)){
  for (k in unique(traits.long.m$Trait.name)){
    if (any(is.na(traits.long.m$sd[which(traits.long.m$Species==i&traits.long.m$Trait.name==k)]))) {
      traits.long.m$increasing.sd[which(traits.long.m$Species==i&traits.long.m$Trait.name==k)]<-"NA"
    } else if(traits.long.m$sd[which(traits.long.m$Species==i&traits.long.m$Trait.name==k&traits.long.m$Year==2020)]>
              traits.long.m$sd[which(traits.long.m$Species==i&traits.long.m$Trait.name==k&traits.long.m$Year==2021)]) {
      traits.long.m$increasing.sd[which(traits.long.m$Species==i&traits.long.m$Trait.name==k)]<-"FALSE"
    } else if(traits.long.m$sd[which(traits.long.m$Species==i&traits.long.m$Trait.name==k&traits.long.m$Year==2020)]<
              traits.long.m$sd[which(traits.long.m$Species==i&traits.long.m$Trait.name==k&traits.long.m$Year==2021)]) {
      traits.long.m$increasing.sd[which(traits.long.m$Species==i&traits.long.m$Trait.name==k)]<-"TRUE"
    } else {traits.long.m$increasing.sd[which(traits.long.m$Species==i&traits.long.m$Trait.name==k)]<-"NA"}
  }
}


traits.long.m.2020<-traits.long.m[which(traits.long.m$Year==2020),]
traits.long.m.2021<-traits.long.m[which(traits.long.m$Year==2021),]

traits.long.m.2020$mean.2021<-traits.long.m.2021$mean
traits.long.m.2020$sd.2021<-traits.long.m.2021$sd
traits.long.m.2020$LCL.2021<-traits.long.m.2021$LCL
traits.long.m.2020$UCL.2021<-traits.long.m.2021$UCL
traits.long.m.2020$C.Var.2021<-traits.long.m.2021$C.Var


baseline.colour = "black"; future.colour = "darkorchid1"; 
manual.colour.values = c("firebrick", "chartreuse2","black")

traits.long.m.2020$increasing <- factor(traits.long.m.2020$increasing, 
                                        levels = c("TRUE", "FALSE","NA"))
traits.long.m.2020$increasing.CV <- factor(traits.long.m.2020$increasing.CV, 
                                           levels = c("TRUE", "FALSE","NA"))

traits.long.m.2020$Trait.name2 <- factor(traits.long.m.2020$Trait.name, 
                                         levels = c("Leaf.area.mm2", "Leaf.dry.mass.g",
                                                    "Leaf.thickness.mm","SLA","LDMC.dry.wet",
                                                    "stem.dry.density.g.mm3","root.dry.density.g.mm3",
                                                    "Root.depth.cm","Root.lateral.cm",
                                                    "above.volume","below.volume","above.below.dry.ratio",
                                                    "cover.area","Height.cm.y" ,"TOTAL_Reproduction",
                                                    "Seed_mass_g.x", 
                                                    "Neighbor.Density","Diversity","Shannon.index" ),
                                         labels=c("Leaf \n area (mm2)", "Leaf dry mass (g)",
                                                  "Leaf \n thickness (mm)","SLA (mm2/g)","LDMC (g/g)",
                                                  "Stem-specific \n density (g/mm3)","Root-specifc \n density (g/mm3)",
                                                  "Root depth (cm)","Root lateral \n radius (cm)",
                                                  "Aboveground \n volume (cm3)","Belowground \n volume (cm3)",
                                                  "Root-mass \n fraction",
                                                  "Cover (cm2)","Height (cm)" ,"Total \n reproduction",
                                                  "Seed mass (g)", 
                                                  "Neighbor \n Density","Neighbor \n Diversity","Shannon Index"))

traits.long.m.2020$Species2 <- factor(traits.long.m.2020$Species, 
                                      levels = c("FAME","MATA","KAGR","BOCO","AMCO",
                                                 "HETERO","DICA","BOCU","CURLY",
                                                 "SIDA","SAIB",
                                                 "ERLE"),
                                      labels=c("PHAU","MATA","KAGR","BOCO","AMCO",
                                               "HECO","DICA","BOCU","BOCH",
                                               "SIAB","SATR",
                                               "ERLE"))


graph.data1<-traits.long.m.2020[-which(traits.long.m.2020$Trait.name2 %in% 
                                         c("Leaf dry mass (g)","Leaf dry mass (g)",
                                           "Seed mass (g)","Neighbor \n Density",
                                           "Neighbor \n Diversity",
                                           "Cover (cm2)",
                                           "Shannon Index",
                                           "Height (cm)",
                                           "Root depth (cm)",
                                           "Root lateral \n radius (cm)")),]

#### mean
CairoPNG("figures/Mean_trait_changes.png",width=1200,height=1000)
ggplot(data = graph.data1 ) + 

  
  geom_segment(aes(x = Species2,y = log(mean),
                   xend = Species2,yend = log(mean.2021),
                   colour = increasing),linewidth = 3) + 
  annotate("rect", ymin = -Inf, ymax = Inf, xmin = 9.5, xmax = 12.5, 
           alpha = .2,fill="chocolate1")+
  annotate("rect", ymin = -Inf, ymax = Inf, xmin =11.5, xmax = 12.5, 
           alpha = .2,fill="grey20")+
  annotate("rect", ymin = -Inf, ymax = Inf, xmin =5.5, xmax = 9.5, 
           alpha = .2,fill="grey20")+
  geom_point(aes(x = Species2,y = log(mean)), 
             colour = baseline.colour, size = 1, alpha = 0.9) + 
  geom_point(aes(x = Species2, y = log(mean.2021)), 
             colour = future.colour, size = 1, alpha = 0.7)  +
  
  coord_flip() +
  xlab(element_blank()) + 
  ylab("Trait values (log)") + 
  theme(panel.grid.minor = element_blank()) + 
  ggtitle("Mean Trait Change")+
  labs(colour = "Change in mean trait value") + 
  scale_colour_manual(values = manual.colour.values, 
                      breaks = c("FALSE", "TRUE"), labels = c("Decreased","Increased")) + 
  theme_minimal(base_size = 15)+
  theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, size = 10),
        legend.position = "bottom") + 
  facet_grid(~Trait.name2, scales = "free")
dev.off()


### standard deviation
graph2<-traits.long.m.2020[-which(traits.long.m.2020$Trait.name2 %in% 
                                    c("Leaf dry mass (g)","Leaf dry mass (g)",
                                      "Seed mass (g)","Neighbor \n Density",
                                      "Neighbor \n Diversity",
                                      "Cover (cm2)",
                                      "Shannon Index",
                                      "Height (cm)",
                                      "Root depth (cm)",
                                      "Root lateral \n radius (cm)")),]

CairoPNG("figures/Standard_deviation_trait_changes.png",width=1200,height=1000)
ggplot(data = graph2) + 
  # geom_errorbar(ggplot2::aes(x = Species,ymin = LCL,
  #                                    ymax = UCL), colour = "grey40", 
  #                       width = 0.1, show.legend = FALSE) +
  # geom_errorbar(ggplot2::aes(x = Species,ymin = LCL.2021,
  #                                     ymax = UCL.2021), colour = "purple", 
  #                        width = 0.1, show.legend = FALSE)+
  
  geom_segment(aes(x = Species2,y = log(sd),
                   xend = Species2,yend = log(sd.2021),
                   colour = increasing.sd),linewidth = 3) + 
  annotate("rect", ymin = -Inf, ymax = Inf, xmin = 9.5, xmax = 12.5, 
           alpha = .2,fill="chocolate1")+
  annotate("rect", ymin = -Inf, ymax = Inf, xmin =11.5, xmax = 12.5, 
           alpha = .2,fill="grey20")+
  annotate("rect", ymin = -Inf, ymax = Inf, xmin =5.5, xmax = 9.5, 
           alpha = .2,fill="grey20")+
  geom_point(aes(x = Species2,y = log(sd)), 
             colour = baseline.colour, size = 1, alpha = 0.9) + 
  geom_point(aes(x = Species2, y = log(sd.2021)), 
             colour = future.colour, size = 1, alpha = 0.7)  +
  
  coord_flip() +
  xlab(element_blank()) + 
  ggtitle("Standard Deviation (log)")+
  ylab("Trait values (log)") + 
  theme(panel.grid.minor = element_blank()) + 
  labs(colour = "Change in standard error") + 
  scale_colour_manual(values = manual.colour.values, 
                      breaks = c("FALSE", "TRUE"), labels = c("Decreased","Increased")) + 
  theme_minimal(base_size = 15)+
  theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, size = 10),legend.position = "bottom") + 
  facet_grid(~Trait.name2, scales = "free")
dev.off()

