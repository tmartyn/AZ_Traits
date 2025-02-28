
# read in traits data
traits<-read.csv("output/Traits.GPS.Comp.26.04.2023c.csv")

# calculate some traits
traits$above.below.dry.ratio<-as.numeric(paste0(traits$total.below.dry.mass.g))/(traits$total.above.dry.mass.g+as.numeric(paste0(traits$total.below.dry.mass.g)))
traits$cover.area<-pi*(traits$Width.90.cm.y/2)*(traits$Width.long.cm.y/2)
traits$above.volume<-pi*(traits$Width.90.cm.y/2)*(traits$Width.long.cm.y/2)*traits$Height.cm.y*(4/3)
traits$below.volume<-pi*(as.numeric(paste0(traits$Root.lateral.cm))/2)*(as.numeric(paste0(traits$Root.lateral.cm))/2)*as.numeric(paste0(traits$Root.depth.cm))*(4/3)
traits$Root.depth.cm<-as.numeric(paste0(traits$Root.depth.cm))
traits$Root.lateral.cm<-as.numeric(paste0(traits$Root.lateral.cm))


#### clean data #####
# identify cols to keep
cols<-c("Year","Species","Leaf.dry.mass.g","Leaf.thickness.mm","Leaf.area.mm2","TOTAL_Reproduction",
        "Seed_mass_g.x","Height.cm.y","stem.dry.density.g.mm3","root.dry.density.g.mm3", 
        "SLA","Neighbor.Density","Shannon.index","Diversity","above.below.dry.ratio","cover.area",
        "above.volume","below.volume","LDMC.dry.wet","Root.depth.cm","Root.lateral.cm")

# get a subset of cols into a dataframe
for (j in cols){
  if (j=="Species") {next}
  traits[,j]<-as.numeric(paste0(traits[,j]))
}
traits.sub<-traits[,which(names(traits)%in%cols)]
traits.sub2<-traits.sub[3:21]


## do the standardize traits at the species level
traits.list<-list()
for (k in unique(traits.sub$Species)){
  traits.std<-standard(traits.sub2[which(traits.sub$Species==k),])
  traits.list[[paste0(k)]]<-traits.std
}

#bind the standardized traits back to the true trait value
traits.std.species<-do.call("rbind",traits.list)
to.split<-rownames(traits.std.species)
row.names.species<-matrix(unlist(strsplit(to.split,".",fixed=T)),ncol=2,byrow=T)
traits.std.species$species<-row.names.species[,1]
traits.std.species$row.num<-row.names.species[,2]

names(traits.std.species)<-paste0(names(traits.std.species),"_STD")
names(traits.std.species)[20:21]<-c("Species","row.num")
traits.std.species<-traits.std.species[,-20]
head(traits.std.species)

traits.sub$row.num<-rownames(traits.sub)
traits.sub3<-merge(traits.sub,traits.std.species,by="row.num")

# aggregate traits by year and species (calculate mean and stdev)
traits.sub.agg<-with(traits.sub3,aggregate(cbind(Leaf.dry.mass.g_STD,Leaf.thickness.mm_STD,Leaf.area.mm2_STD,TOTAL_Reproduction_STD,
                                                 Seed_mass_g.x_STD,Height.cm.y_STD,stem.dry.density.g.mm3_STD,root.dry.density.g.mm3_STD, 
                                                 SLA_STD,Neighbor.Density_STD,Shannon.index_STD,above.below.dry.ratio_STD,cover.area_STD, above.volume_STD,               
                                                 below.volume_STD,LDMC.dry.wet_STD,Diversity_STD,Root.depth.cm_STD,Root.lateral.cm_STD),by=list(Year,Species),mean,na.rm=T))

traits.sub.agg.sd<-with(traits.sub3,aggregate(cbind(Leaf.dry.mass.g_STD,Leaf.thickness.mm_STD,Leaf.area.mm2_STD,TOTAL_Reproduction_STD,
                                                    Seed_mass_g.x_STD,Height.cm.y_STD,stem.dry.density.g.mm3_STD,root.dry.density.g.mm3_STD, 
                                                    SLA_STD,Neighbor.Density_STD,Shannon.index_STD,above.below.dry.ratio_STD,cover.area_STD, above.volume_STD,               
                                                    below.volume_STD,LDMC.dry.wet_STD,Diversity_STD,Root.depth.cm_STD,Root.lateral.cm_STD),by=list(Year,Species),sd,na.rm=T))

names(traits.sub.agg)[1:2]<-c("Year","Species")

#remove species not used in analysis
traits.agg<-traits.sub.agg[-which(traits.sub.agg$Species=="ERCI"),]

# correct and NAN to NA
for (i in 1:length(names(traits.agg))){
  if (any(is.nan(traits.agg[,i]))) {
    traits.agg[,i][which(is.nan(traits.agg[,i]))]<-NA
  } else {next}
}



names(traits.sub.agg.sd)[1:2]<-c("Year","Species")

#remove species not used in analysis
traits.agg.sd<-traits.sub.agg.sd[-which(traits.sub.agg.sd$Species=="ERCI"),]

# correct and NAN to NA
traits.agg.sd$Seed_mass_g.x_STD[which(is.nan(traits.agg.sd$Seed_mass_g.x_STD))]<-NA



#### gather data for figures #####
traits.long<-gather(traits.agg,"Trait.name","mean",Leaf.dry.mass.g_STD,Leaf.thickness.mm_STD,Leaf.area.mm2_STD,TOTAL_Reproduction_STD,
                    Seed_mass_g.x_STD,Height.cm.y_STD,stem.dry.density.g.mm3_STD,root.dry.density.g.mm3_STD, 
                    SLA_STD,Neighbor.Density_STD,Shannon.index_STD,above.below.dry.ratio_STD,cover.area_STD, above.volume_STD,               
                    below.volume_STD,LDMC.dry.wet_STD,Diversity_STD,Root.depth.cm_STD,Root.lateral.cm_STD)


traits.long.sd<-gather(traits.agg.sd,"Trait.name","sd",Leaf.dry.mass.g_STD,Leaf.thickness.mm_STD,Leaf.area.mm2_STD,TOTAL_Reproduction_STD,
                       Seed_mass_g.x_STD,Height.cm.y_STD,stem.dry.density.g.mm3_STD,root.dry.density.g.mm3_STD, 
                       SLA_STD,Neighbor.Density_STD,Shannon.index_STD,above.below.dry.ratio_STD,cover.area_STD, above.volume_STD,               
                       below.volume_STD,LDMC.dry.wet_STD,Diversity_STD,Root.depth.cm_STD,Root.lateral.cm_STD)

traits.long.m<-merge(traits.long,traits.long.sd)
traits.long.m$LCL<-traits.long.m$mean-1*traits.long.m$sd
traits.long.m$UCL<-traits.long.m$mean+1*traits.long.m$sd
traits.long.m$C.Var<-traits.long.m$sd/traits.long.m$mean


#### determine decreasing or increaseing change between years #####
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

#### set colors and rename columns #####
baseline.colour = "white"; future.colour = "black"; 
manual.colour.values = c("darkorange2", "darkorchid1","black")

traits.long.m.2020$increasing <- factor(traits.long.m.2020$increasing, 
                                        levels = c("TRUE", "FALSE","NA"))
traits.long.m.2020$increasing.CV <- factor(traits.long.m.2020$increasing.CV, 
                                           levels = c("TRUE", "FALSE","NA"))

traits.long.m.2020$Trait.name2 <- factor(traits.long.m.2020$Trait.name, 
                                         levels = c("Leaf.area.mm2_STD", "Leaf.dry.mass.g_STD",
                                                    "Leaf.thickness.mm_STD","SLA_STD","LDMC.dry.wet_STD",
                                                    "stem.dry.density.g.mm3_STD","root.dry.density.g.mm3_STD",
                                                    "Root.depth.cm_STD","Root.lateral.cm_STD",
                                                    "above.volume_STD","below.volume_STD","above.below.dry.ratio_STD",
                                                    "cover.area_STD","Height.cm.y_STD" ,"TOTAL_Reproduction_STD",
                                                    "Seed_mass_g.x_STD", 
                                                    "Neighbor.Density_STD","Diversity_STD","Shannon.index_STD" ),
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

### below used a modification of the original dot shift function
# code from the AlleleShift package (
# https://cran.r-project.org/web/packages/AlleleShift/index.html)

#### create and print figures #####
CairoPNG("figures/Mean_trait_changes_2025.png",width=1050,height=800)
ggplot(data = graph.data1 ) + 
  geom_segment(aes(x = Species2,y = mean,
                   xend = Species2,yend = mean.2021,
                   colour = increasing),linewidth = 3,alpha=0.7) + 
  annotate("rect", ymin = -Inf, ymax = Inf, xmin = 9.5, xmax = 12.5, 
           alpha = .2,fill="chocolate1")+
  annotate("rect", ymin = -Inf, ymax = Inf, xmin =11.5, xmax = 12.5, 
           alpha = .2,fill="grey20")+
  annotate("rect", ymin = -Inf, ymax = Inf, xmin =5.5, xmax = 9.5, 
           alpha = .2,fill="grey20")+
  geom_point(aes(x = Species2,y = mean), 
             fill = baseline.colour, color="black", size = 3.5, alpha = 1.0,shape=21) + 
  geom_point(aes(x = Species2, y = mean.2021), 
             fill = future.colour, color="black", size = 3.5, alpha = 1.0,shape=17)  +
  
  coord_flip() +
  xlab(element_blank()) + 
  ylab("Stadardized Trait values") + 
  theme(panel.grid.minor = element_blank()) + 
  ggtitle("Mean Trait Change")+
  labs(colour = "Change in mean trait value") + 
  scale_colour_manual(values = manual.colour.values, 
                      breaks = c("FALSE", "TRUE"), labels = c("Decreased","Increased")) + 
  theme_minimal(base_size = 15)+
  theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, size = 15),
        legend.position = "bottom") + 
  facet_grid(~Trait.name2, scales = "free")
dev.off()

CairoPNG("figures/Coefficient_Variation_trait_changes_2025.png",width=1050,height=800)
ggplot(data = traits.long.m.2020[-which(traits.long.m.2020$Trait.name2 %in% 
                                          c("Leaf dry mass (g)","Leaf dry mass (g)",
                                            "Seed mass (g)","Neighbor \n Density",
                                            "Neighbor \n Diversity",
                                            "Cover (cm2)",
                                            "Shannon Index",
                                            "Height (cm)",
                                            "Root depth (cm)",
                                            "Root lateral \n radius (cm)")),]) + 
  geom_segment(aes(x = Species2,y = C.Var,
                   xend = Species2,yend = C.Var.2021,
                   colour = increasing.CV),linewidth = 3) + 
  annotate("rect", ymin = -Inf, ymax = Inf, xmin = 9.5, xmax = 12.5, 
           alpha = .2,fill="chocolate1")+
  annotate("rect", ymin = -Inf, ymax = Inf, xmin =11.5, xmax = 12.5, 
           alpha = .2,fill="grey20")+
  annotate("rect", ymin = -Inf, ymax = Inf, xmin =5.5, xmax = 9.5, 
           alpha = .2,fill="grey20")+
  geom_point(aes(x = Species2,y = C.Var), 
             fill = baseline.colour, color="black", size = 3.5, alpha = 1.0,shape=21) + 
  geom_point(aes(x = Species2, y = C.Var.2021), 
             fill = future.colour, color="black", size = 3.5, alpha = 1.0,shape=17)  +
  
  coord_flip() +
  xlab(element_blank()) + 
  ggtitle("Coefficient Variation")+
  ylab("Standardized Trait values") + 
  theme(panel.grid.minor = element_blank()) + 
  labs(colour = "Change in coefficient of variation") + 
  scale_colour_manual(values = manual.colour.values, 
                      breaks = c("FALSE", "TRUE"), labels = c("decreasing","increasing")) + 
  theme_minimal(base_size = 15)+
  theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, size = 15),legend.position = "bottom") + 
  facet_grid(~Trait.name2, scales = "free")
dev.off()


rm(traits)
