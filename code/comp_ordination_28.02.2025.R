
# clean data for spreading
cols.rm<-c("Density","Plot","Native.Exotic","native.rel",
           "exotic.rel","Density","Per_Bare","Pre_Litter","Unique.Plant.ID","BLOCK")
comp.pre<-comp.all[,-which(names(comp.all)%in%cols.rm)]
comp.pre<-na.omit(comp.pre)
comp.pre$Species_Code<-trimws(comp.pre$Species_Code)
comp.test<-spread(comp.pre,key=Species_Code,value=Per_Cover)

#remove log, rocks and poop
cols.rm<-c("LOG","ROCKS/STONE","HORSE POOP")
comp.test1<-comp.test[,-which(names(comp.test)%in%cols.rm)]

# turn all NAs to 0's
comp.test1[is.na(comp.test1)]<-0

# make sure everything is numeric
for (i in 3:122) {
  comp.test1[,i]<-as.numeric(paste0(comp.test1[,i]))
}
str(comp.test1)
rownames(comp.test1)<-comp.test1$Unique.ID

#determine rare species (total cover across two years less than 15%)
rare.merge<-sort(colSums(comp.test1[,3:122],na.rm=T))
rares.rm<-names(rare.merge[which(rare.merge<50)])

#bunch rare species into rare columns
comp.test2<-comp.test1[,-which(names(comp.test1)%in%rares.rm)]
comp.test2$RARE<-rowSums(comp.test1[,which(names(comp.test1)%in%rares.rm)])

#add block on to data
comp.merg<-merge(comp.test2,comp.all,by="Unique.ID")
cols.rm2<-c("Density","Plot","Native.Exotic","native.rel","Year.y","Per_Cover",
            "exotic.rel","Density","Per_Bare","Pre_Litter","Unique.Plant.ID",
            "Species_Code")
comp.merg2<-unique(comp.merg[,-which(names(comp.merg)%in%cols.rm2)])
names(comp.merg2)[2]<-"Year"

end.sp<-dim(comp.test2)[2]
# aggregate data by block and year
comp.agg2<-aggregate(cbind(comp.merg2[,3:end.sp]),by=list(comp.merg2$Year,comp.merg2$BLOCK),mean,na.rm=T)
names(comp.agg2)[1:2]<-c("Year","Block")
names(comp.agg2)[10]<-"UNK.GRASS" #buffel-like
names(comp.agg2)[14]<-"CYFU" #cholla
names(comp.agg2)[15]<-"BOCH" #curly sprucetop grama
names(comp.agg2)[21]<-"HECO" 
names(comp.agg2)[22]<-"IPPU" #ipomea purpurea 
names(comp.agg2)[29]<-"SATR" #Salsola tragus
names(comp.agg2)[30]<-"SIAB" #Sida abutifolia


# split data by year
comp.agg.2020<-comp.agg2[which(comp.agg2$Year==2020),]
comp.agg.2020b<-comp.agg.2020[3:end.sp]
rownames(comp.agg.2020b)<-1:9
comp.agg.2021<-comp.agg2[which(comp.agg2$Year==2021),]
comp.agg.2021b<-comp.agg.2021[3:end.sp]
rownames(comp.agg.2021b)<-1:9
#comp.test.2021b<-comp.test.2021b[-34,]

# Ordination
environmental.novel(comp.agg.2020b,comp.agg.2021b)

plotA <- population.shift.TEM(comp.agg.2020b,
                              comp.agg.2021b,
                              option="PCA",
                              vector.multiply = 3)
CairoPNG("figures/Ordination.composition.change_2025.png",width=1000,height=1000)
plotA
dev.off()

