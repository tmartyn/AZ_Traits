rm(list=ls())
library(ggsci)
library(AlleleShift)
source("code/population.shift.function.R")
traits<-read.csv("data/Traits.GPS.Comp.15.02.2024.csv")

traits$above.below.dry.ratio<-as.numeric(paste0(traits$total.below.dry.mass.g))/(traits$total.above.dry.mass.g+as.numeric(paste0(traits$total.below.dry.mass.g)))
traits$cover.area<-pi*(traits$Width.90.cm.y/2)*(traits$Width.long.cm.y/2)
traits$above.volume<-pi*(traits$Width.90.cm.y/2)*(traits$Width.long.cm.y/2)*traits$Height.cm.y*(4/3)
traits$below.volume<-pi*(as.numeric(paste0(traits$Root.lateral.cm))/2)*(as.numeric(paste0(traits$Root.lateral.cm))/2)*as.numeric(paste0(traits$Root.depth.cm))*(4/3)


traits.2021<-traits[which(traits$Year==2021),]
traits.2020<-traits[which(traits$Year==2020),]

cor.test(traits$Height.cm.y, traits$above.volume, na.action=na.omit)
cor.test(traits$above.volume,traits$above.below.dry.ratio)
cor.test(as.numeric(paste0(traits$Root.depth.cm)), traits$below.volume, na.action=na.omit)

cols<-c("Year","Species","Leaf.dry.mass.g","Leaf.thickness.mm","Leaf.area.mm2","TOTAL_Reproduction",
        "stem.dry.density.g.mm3","root.dry.density.g.mm3", 
        "SLA","Neighbor.Density","Diversity","above.below.dry.ratio",
        "above.volume" ,"below.volume" ,"LDMC.dry.wet")

for (j in cols){
  if (j=="Species") {next}
  traits.2021[,j]<-as.numeric(paste0(traits.2021[,j]))
  traits.2020[,j]<-as.numeric(paste0(traits.2020[,j]))
}
traits.2020.sub<-traits.2020[,which(names(traits.2020)%in%cols)]
traits.2021.sub<-traits.2021[,which(names(traits.2021)%in%cols)]

traits.2020.sub.agg<-with(traits.2020.sub,aggregate(cbind(Leaf.thickness.mm,TOTAL_Reproduction,
                                                stem.dry.density.g.mm3,root.dry.density.g.mm3, 
                                                above.below.dry.ratio, above.volume,SLA, 
                                                below.volume,LDMC.dry.wet),by=list(Year,Species),mean,na.rm=T))

traits.2021.sub.agg<-with(traits.2021.sub,aggregate(cbind(Leaf.thickness.mm,TOTAL_Reproduction,
                                                          stem.dry.density.g.mm3,root.dry.density.g.mm3, 
                                                          above.below.dry.ratio,above.volume,SLA,
                                                          below.volume,LDMC.dry.wet),by=list(Year,Species),mean,na.rm=T))

names(traits.2020.sub.agg)[1:2]<-c("Year","Species")
traits.2020.agg<-traits.2020.sub.agg#[-which(traits.2020.sub.agg$Species=="ERCI"),]

rownames(traits.2020.agg)<-traits.2020.agg$Species
traits.2020.agg2<-traits.2020.agg[,-c(1:2)]
rownames(traits.2020.agg2)<-c("AMCO","BOCO","BOCU","BOCH","DICA","ERLE","PHAU","HECO","KAGR","MATA","SATR","SIAB")
names(traits.2020.agg2)<-c("LThick","TRepo","SSD","RSD","RMF","AVol","SLA","BVol","LDMC")

names(traits.2021.sub.agg)[1:2]<-c("Year","Species")
traits.2021.agg<-traits.2021.sub.agg[-which(traits.2021.sub.agg$Species=="ERCI"),]
rownames(traits.2021.agg)<-traits.2021.agg$Species
traits.2021.agg2<-traits.2021.agg[,-c(1:2)]
rownames(traits.2021.agg2)<-c("AMCO","BOCO","BOCU","BOCH","DICA","ERLE","PHAU","HECO","KAGR","MATA","SATR","SIAB")
names(traits.2021.agg2)<-c("LThick","TRepo","SSD","RSD","RMF","AVol","SLA","BVol","LDMC")



environmental.novel(traits.2020.agg2, traits.2021.agg2)

library(Cairo)
plotA <- population.shift.TEM(traits.2020.agg2,
                          traits.2021.agg2,
                          option="PCA")
CairoPNG("figures/Ordination.traits.change.png",width=1000,height=1000)
plotA
dev.off()
