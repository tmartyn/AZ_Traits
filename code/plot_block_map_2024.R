rm(list=ls())
plants<-read.csv("data/Traits.GPS.Comp.15.02.2024.csv")
library(spatstat)
library(sp)
library(rgdal)
library(ads)
library(maptools)


plants.sub<-plants#plants[-which(is.na(plants$Lat)),]
plants.sub$lon<-plants.sub$lon * -1 # change longitude boordinate to change map to appear how we drive up to the site

cols.keep<-c("lat","lon","Species","block","Unique.Plant.ID")
plants.sub2<-plants.sub[,which(names(plants.sub)%in%cols.keep)]

plants.sub2<-na.omit(plants.sub2)
plants.sub2<-unique(plants.sub2)

plants.marks<-plants.sub2[,2]
plants.spp <- SpatialPointsDataFrame(coords = plants.sub2[,3:4], proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"), 
                                 data = data.frame(plants.sub2[,2]))

plot(plants.spp)


plants.sub3<-plants.sub2[,c(3,4,2,5)]
plants.sub4<-plants.sub3[-which(plants.sub3$Species=="ERCI"),]

plants.sub4$Species<-factor(plants.sub4$Species,
                            levels=c("AMCO","BOCO","CURLY","BOCU",
                                     "DICA","ERLE","HETERO","KAGR",
                                     "MATA","FAME","SAIB","SIDA"),
                            labels=c("AMCO","BOCO","BOCH","BOCU",
                                     "DICA","ERLE","HECO","KAGR",
                                     "MATA","PHAU","SATR","SIAB"))


plants.ppp<-as.ppp(X=plants.sub4,
                   owin(xrange=c(31.8152958,31.81976698),
                        yrange=c(111.509862,111.512358)))


library(Cairo)
CairoPNG("figures/block.plot.png",height=900,width=1500)

par(mar=c(1.5,.3,.3,.3))
plot.ppp(plants.ppp,which.marks=c("block"),
         cols=c("black","green","blue","gold","grey70","purple",
                "deeppink","deepskyblue","darkgreen","firebrick1",
                "darkred","tan1"),
         legend=T,main="",pch=16,size=1,
         leg.args=list(nsymbols = 9,cex.names=30))
         

dev.off()

CairoPNG("figures/species.plot.png",height=900,width=1500)
plot.ppp(plants.ppp,which.marks=c("Species"),
         cols=c("black","green","blue","gold","grey70","purple",
                "deeppink","deepskyblue","darkgreen","firebrick1",
                "darkred","tan1"),
         leg.side="left",main="",leg.args=list(nsymbols = 13,cex=3),
         pch=16,size=1)
dev.off()
