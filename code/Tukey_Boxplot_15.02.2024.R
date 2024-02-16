library(lme4)
library(lmerTest)
library(MuMIn)
library(partR2)
library(emmeans)
library(tidyverse)

traits<-read.csv("data/Traits.GPS.Comp.15.02.2024.csv")

traits$above.below.dry.ratio<-as.numeric(paste0(traits$total.below.dry.mass.g))/(traits$total.above.dry.mass.g+as.numeric(paste0(traits$total.below.dry.mass.g)))
traits$cover.area<-pi*(traits$Width.90.cm.y/2)*(traits$Width.long.cm.y/2)
traits$above.volume<-pi*(traits$Width.90.cm.y/2)*(traits$Width.long.cm.y/2)*traits$Height.cm.y*(4/3)
traits$below.volume<-pi*(as.numeric(paste0(traits$Root.lateral.cm))/2)*(as.numeric(paste0(traits$Root.lateral.cm))/2)*as.numeric(paste0(traits$Root.depth.cm))*(4/3)
traits$Root.depth.cm<-as.numeric(paste0(traits$Root.depth.cm))
traits$Root.lateral.cm<-as.numeric(paste0(traits$Root.lateral.cm))
traits$SLA<-as.numeric(paste0(traits$SLA))
traits$Leaf.thickness.mm<-as.numeric(paste0(traits$Leaf.thickness.mm))
traits$Leaf.area.mm2<-as.numeric(paste0(traits$Leaf.area.mm2))
traits$TOTAL_Reproduction<-as.numeric(paste0(traits$TOTAL_Reproduction))


lower_bound <- quantile(traits$SLA, 0.01,na.rm=T)
upper_bound <- quantile(traits$SLA, 0.99,na.rm=T)

traits$Year<-as.factor(traits$Year)

outlier_ind <- which(traits$SLA < lower_bound | traits$SLA > upper_bound)
traits.sub<-traits[-which(is.na(traits$SLA)),]


fit.SLA<-lmer(SLA~(1|block)+Year*Native.Exotic*Life.form,traits.sub)
summary(fit.SLA)
r.squaredGLMM(fit.SLA)
em.SLA<-emmeans(fit.SLA, list(pairwise ~ Year*Native.Exotic*Life.form),adjust="tukey")
tri.SLA<-pwpm(em.SLA,means=F,diffs=F)
SLA.df<-data.frame(matrix(tri.SLA,nrow=dim(tri.SLA)[1]))
rownames(SLA.df)<-rownames(tri.SLA)
colnames(SLA.df)<-colnames(tri.SLA)
write.csv(SLA.df,"output/SLA.Tukey.csv")


fit.LDMC<-lmer(log(LDMC.dry.wet)~(1|block)+Year*Native.Exotic*Life.form,traits)
summary(fit.LDMC)
r.squaredGLMM(fit.LDMC)
em.LDMC<-emmeans(fit.LDMC, list(pairwise ~ Year*Native.Exotic*Life.form),adjust="tukey")
tri.LDMC<-pwpm(em.LDMC,means=F,diffs=F)
LDMC.df<-data.frame(matrix(tri.LDMC,nrow=dim(tri.LDMC)[1]))
rownames(LDMC.df)<-rownames(tri.LDMC)
colnames(LDMC.df)<-colnames(tri.LDMC)
write.csv(LDMC.df,"output/LDMC.Tukey.csv")


fit.Leaf.thickness<-lmer(Leaf.thickness.mm~(1|block)+Year*Native.Exotic*Life.form,traits)
summary(fit.Leaf.thickness)
r.squaredGLMM(fit.Leaf.thickness)
em.thick<-emmeans(fit.Leaf.thickness, list(pairwise ~ Year*Native.Exotic*Life.form),adjust="tukey")
tri.thick<-pwpm(em.thick,means=F,diffs=F)
thick.df<-data.frame(matrix(tri.thick,nrow=dim(tri.thick)[1]))
rownames(thick.df)<-rownames(tri.thick)
colnames(thick.df)<-colnames(tri.thick)
write.csv(thick.df,"output/thick.Tukey.csv")

fit.Leaf.area<-lmer(log(Leaf.area.mm2)~(1|block)+Year*Native.Exotic*Life.form,traits)
summary(fit.Leaf.area)
r.squaredGLMM(fit.Leaf.area)
em.LA<-emmeans(fit.Leaf.area, list(pairwise ~ Year*Native.Exotic*Life.form),adjust="tukey")
tri.LA<-pwpm(em.LA,means=F,diffs=F)
LA.df<-data.frame(matrix(tri.LA,nrow=dim(tri.LA)[1]))
rownames(LA.df)<-rownames(tri.LA)
colnames(LA.df)<-colnames(tri.LA)
write.csv(LA.df,"output/LA.Tukey.csv")


fit.stem.density.g.mm3<-lmer(stem.dry.density.g.mm3~(1|block)+Year*Native.Exotic*Life.form,traits)
summary(fit.stem.density.g.mm3)
r.squaredGLMM(fit.stem.density.g.mm3)
em.stem<-emmeans(fit.stem.density.g.mm3, list(pairwise ~ Year*Native.Exotic*Life.form),adjust="tukey")
tri.stem<-pwpm(em.stem,means=F,diffs=F)
stem.df<-data.frame(matrix(tri.stem,nrow=dim(tri.stem)[1]))
rownames(stem.df)<-rownames(tri.stem)
colnames(stem.df)<-colnames(tri.stem)
write.csv(stem.df,"output/stem.Tukey.csv")


fit.root.density.g.mm3<-lmer(root.dry.density.g.mm3~(1|block)+Year*Native.Exotic*Life.form,traits)
summary(fit.root.density.g.mm3)
r.squaredGLMM(fit.root.density.g.mm3)
em.root<-emmeans(fit.root.density.g.mm3, list(pairwise ~ Year*Native.Exotic*Life.form),adjust="tukey")
tri.root<-pwpm(em.root,means=F,diffs=F)
root.df<-data.frame(matrix(tri.root,nrow=dim(tri.root)[1]))
rownames(root.df)<-rownames(tri.root)
colnames(root.df)<-colnames(tri.root)
write.csv(root.df,"output/root.Tukey.csv")


fit.above.volume<-lmer(log(above.volume)~(1|block)+Year*Native.Exotic*Life.form,traits)
summary(fit.above.volume)
r.squaredGLMM(fit.above.volume)
em.AVol<-emmeans(fit.above.volume, list(pairwise ~ Year*Native.Exotic*Life.form),adjust="tukey")
tri.AVol<-pwpm(em.AVol,means=F,diffs=F)
AVol.df<-data.frame(matrix(tri.AVol,nrow=dim(tri.AVol)[1]))
rownames(AVol.df)<-rownames(tri.AVol)
colnames(AVol.df)<-colnames(tri.AVol)
write.csv(AVol.df,"output/AVol.Tukey.csv")


fit.below.volume<-lmer(log(below.volume)~(1|block)+Year*Native.Exotic*Life.form,traits)
summary(fit.below.volume)
r.squaredGLMM(fit.below.volume)
em.BVol<-emmeans(fit.below.volume, list(pairwise ~ Year*Native.Exotic*Life.form),adjust="tukey")
tri.BVol<-pwpm(em.BVol,means=F,diffs=F)
BVol.df<-data.frame(matrix(tri.BVol,nrow=dim(tri.BVol)[1]))
rownames(BVol.df)<-rownames(tri.BVol)
colnames(BVol.df)<-colnames(tri.BVol)
write.csv(BVol.df,"output/BVol.Tukey.csv")


fit.RMF<-lmer(above.below.dry.ratio~(1|block)+Year*Native.Exotic*Life.form,traits)
summary(fit.RMF)
r.squaredGLMM(fit.RMF)
em.RMF<-emmeans(fit.RMF, list(pairwise ~ Year*Native.Exotic*Life.form),adjust="tukey")
tri.RMF<-pwpm(em.RMF,means=F,diffs=F)
RMF.df<-data.frame(matrix(tri.RMF,nrow=dim(tri.RMF)[1]))
rownames(RMF.df)<-rownames(tri.RMF)
colnames(RMF.df)<-colnames(tri.RMF)
write.csv(RMF.df,"output/RMF.Tukey.csv")

fit.TOTAL<-lmer(log(TOTAL_Reproduction+1)~(1|block)+Year*Native.Exotic*Life.form,traits)
summary(fit.TOTAL)
r.squaredGLMM(fit.TOTAL)
em.TOTAL<-emmeans(fit.TOTAL, list(pairwise ~ Year*Native.Exotic*Life.form),adjust="tukey")
tri.TOTAL<-pwpm(em.TOTAL,means=F,diffs=F)
TOTAL.df<-data.frame(matrix(tri.TOTAL,nrow=dim(tri.TOTAL)[1]))
rownames(TOTAL.df)<-rownames(tri.TOTAL)
colnames(TOTAL.df)<-colnames(tri.TOTAL)
write.csv(TOTAL.df,"output/TOTAL.Tukey.csv")

###### predict to show outcomes boxplots
new.data<-data.frame(Year=c(rep(c(2020,2020,2021,2021,2020,2020,2021,2021),9)),
                     Native.Exotic=c(rep(c("Native","Exotic","Native","Exotic","Native","Exotic","Native","Exotic"),9)),
                     Life.form=c(rep(c("Grass","Grass","Grass","Grass","Forb","Forb","Forb","Forb"),9)),
                     block=c(rep(1:9,each=8)))
new.data$type<-paste0(new.data$Native.Exotic,"_",new.data$Life.form)
traits$type<-paste0(traits$Native.Exotic,"_",traits$Life.form)
new.data$Year<-as.factor(new.data$Year)


new.data$SLA.pred<-predict(fit.SLA,new.data,type="response")
new.data$LDMC.pred<-exp(predict(fit.LDMC,new.data,type="response"))
new.data$Leaf.thickness.pred<-predict(fit.Leaf.thickness,new.data,type="response")
new.data$Leaf.area.pred<-exp(predict(fit.Leaf.area,new.data,type="response"))
new.data$stem.density.g.mm3.pred<-predict(fit.stem.density.g.mm3,new.data,type="response")
new.data$root.density.g.mm3.pred<-predict(fit.root.density.g.mm3,new.data,type="response")
new.data$above.volume.pred<-exp(predict(fit.above.volume,new.data,type="response"))
new.data$below.volume.pred<-exp(predict(fit.below.volume,new.data,type="response"))
new.data$RMF.pred<-predict(fit.RMF,new.data,type="response")
new.data$TOTAL.pred<-exp(predict(fit.TOTAL,new.data,type="response"))

ggplot(new.data,aes(x=Year,y=SLA.pred,fill=type))+geom_boxplot()
ggplot(new.data,aes(x=Year,y=LDMC.pred,fill=type))+geom_boxplot()
ggplot(new.data,aes(x=Year,y=Leaf.thickness.pred,fill=type))+geom_boxplot()
ggplot(new.data,aes(x=Year,y=Leaf.area.pred,fill=type))+geom_boxplot()
ggplot(new.data,aes(x=Year,y=stem.density.g.mm3.pred,fill=type))+geom_boxplot()
ggplot(new.data,aes(x=Year,y=root.density.g.mm3.pred,fill=type))+geom_boxplot()
ggplot(new.data,aes(x=Year,y=above.volume.pred,fill=type))+geom_boxplot()
ggplot(new.data,aes(x=Year,y=below.volume.pred,fill=type))+geom_boxplot()
ggplot(new.data,aes(x=Year,y=RMF.pred,fill=type))+geom_boxplot()
ggplot(new.data,aes(x=Year,y=TOTAL.pred,fill=type))+geom_boxplot()

new.data$SLA.pred<-new.data$SLA.pred*0.01
new.data$root.density.g.mm3.pred<-new.data$root.density.g.mm3.pred/0.001
new.data$stem.density.g.mm3.pred<-new.data$stem.density.g.mm3.pred/0.001
#new.data$TOTAL.pred<-new.data$TOTAL.pred/1000
#new.data$above.volume.pred<-log(new.data$above.volume.pred)
#new.data$below.volume.pred<-log(new.data$below.volume.pred)

new.data.long<-gather(new.data,"trait","value",-c(Year,Native.Exotic,Life.form,block,type))
new.data.long$trait<-factor(new.data.long$trait,
                               levels=c("Leaf.area.pred","Leaf.thickness.pred","SLA.pred","LDMC.pred",
                                        "stem.density.g.mm3.pred","root.density.g.mm3.pred","RMF.pred","TOTAL.pred",
                                        "above.volume.pred","below.volume.pred"),
                               labels=c("Leaf \narea (mm2)","Leaf \nthickness (mm)","SLA (cm2/g)","LDMC (g/g)",
                                        "Stem-specific \ndensity (g/cm3)","Root-specific \ndensity (g/cm3)","Root-mass fraction", "Total reproduction \n(seeds)",
                                        "Aboveground \nvolume (cm3)","Belowground \nvolume (cm3)"
                                        ))
traits.agg<-with(traits,aggregate(cbind(above.volume,below.volume,
                                        above.below.dry.ratio,root.dry.density.g.mm3,
                                        stem.dry.density.g.mm3,TOTAL_Reproduction,
                                        LDMC.dry.wet , SLA,
                                        Leaf.thickness.mm,Leaf.area.mm2),by=list(type,Year,Native.Exotic,Life.form,block),mean,na.rm=T))
names(traits.agg)[1:5]<-c("type","Year","Native.Exotic","Life.form","block")
traits.agg[sapply(traits.agg,is.nan)]<-NA
traits.agg$SLA<-traits.agg$SLA*0.01
traits.agg$root.dry.density.g.mm3<-traits.agg$root.dry.density.g.mm3/0.001
traits.agg$stem.dry.density.g.mm3<-traits.agg$stem.dry.density.g.mm3/0.001
#traits.agg$TOTAL_Reproduction<-traits.agg$TOTAL_Reproduction
#traits.agg$above.volume<-log(traits.agg$above.volume)
#traits.agg$below.volume<-log(traits.agg$below.volume)

trait.agg.long<-gather(traits.agg,"trait","value",-c(Year,Native.Exotic,Life.form,block,type))
trait.agg.long$trait<-factor(trait.agg.long$trait,
                            levels=c("Leaf.area.mm2","Leaf.thickness.mm","SLA",
                                     "LDMC.dry.wet","stem.dry.density.g.mm3","root.dry.density.g.mm3",
                                     "above.below.dry.ratio","TOTAL_Reproduction",
                                     "above.volume","below.volume"),
                            labels=c("Leaf \narea (mm2)","Leaf \nthickness (mm)","SLA (cm2/g)","LDMC (g/g)",
                                     "Stem-specific \ndensity (g/cm3)","Root-specific \ndensity (g/cm3)","Root-mass fraction", "Total reproduction \n(seeds)",
                                     "Aboveground \nvolume (cm3)","Belowground \nvolume (cm3)"
                            ))


library(Cairo)
ggplot(new.data.long,aes(x=Year,y=sign(value)+log(abs(value)),fill=type))+geom_boxplot()+
  facet_grid(.~trait)+theme_minimal(base_size = 22)+
  ylab("Log trait value")+
  #geom_jitter(data=traits(aes(x=Year,sign(value)+log(abs(value)))))
  scale_fill_manual(values=c("tan1","darkorange3","plum2","purple2"))

CairoPNG("figures/Traits1.png",width=1000,height=800)
ggplot(new.data.long[which(new.data.long$trait%in%c("Aboveground \nvolume (cm3)","Belowground \nvolume (cm3)","Total reproduction \n(seeds)")),],
       aes(x=Year,y=sign(value)+log(abs(value)),fill=type))+geom_boxplot()+
  facet_grid(.~trait)+theme_bw(base_size = 22)+
  ylab("Log trait value")+
  scale_fill_manual(values=c("tan1","darkorange3","plum2","purple2"))+
  theme(legend.position = "bottom",legend.title=element_blank(),panel.spacing=unit(2,"lines"))
dev.off()

CairoPNG("figures/Traits1_points.png",width=1000,height=800)
ggplot(new.data.long[which(new.data.long$trait%in%c("Aboveground \nvolume (cm3)","Belowground \nvolume (cm3)","Total reproduction \n(seeds)")),],
       aes(x=Year,y=sign(value)+log(abs(value)),fill=type))+geom_boxplot()+
  ylab("Log trait value")+
  geom_jitter(data=trait.agg.long[which(trait.agg.long$trait%in%c("Aboveground \nvolume (cm3)","Belowground \nvolume (cm3)","Total reproduction \n(seeds)")),],
              aes(x=Year,sign(value)+log(abs(value)),col=type),alpha=0.5,width = 0.25)+
  facet_grid(.~trait)+theme_bw(base_size = 22)+
scale_fill_manual(values=c("tan1","darkorange3","plum2","purple2"))+
  scale_color_manual(values=c("tan1","darkorange3","plum2","purple2"))+
  theme(legend.position = "bottom",legend.title=element_blank(),panel.spacing=unit(2,"lines"))
dev.off()

CairoPNG("figures/Traits2.png",width=1100,height=800)
ggplot(new.data.long[which(new.data.long$trait%in%c("Leaf \narea (mm2)","Leaf \nthickness (mm)","SLA (cm2/g)","LDMC (g/g)")),],
       aes(x=Year,y=sign(value)+log(abs(value)),fill=type))+geom_boxplot()+
  facet_grid(.~trait)+theme_bw(base_size = 22)+
  ylab("Log trait value")+
  scale_fill_manual(values=c("tan1","darkorange3","plum2","purple2"))+
  theme(legend.position = "bottom",legend.title=element_blank(),panel.spacing=unit(2,"lines"))
dev.off()

CairoPNG("figures/Traits2_points.png",width=1100,height=800)
ggplot(new.data.long[which(new.data.long$trait%in%c("Leaf \narea (mm2)","Leaf \nthickness (mm)","SLA (cm2/g)","LDMC (g/g)")),],
       aes(x=Year,y=sign(value)+log(abs(value)),fill=type))+geom_boxplot()+
  ylab("Log trait value")+
  geom_jitter(data=trait.agg.long[which(trait.agg.long$trait%in%c("Leaf \narea (mm2)","Leaf \nthickness (mm)","SLA (cm2/g)","LDMC (g/g)")),],
              aes(x=Year,sign(value)+log(abs(value)),col=type),alpha=0.5,width = 0.25)+
  facet_grid(.~trait)+theme_bw(base_size = 22)+
  scale_fill_manual(values=c("tan1","darkorange3","plum2","purple2"))+
  scale_color_manual(values=c("tan1","darkorange3","plum2","purple2"))+
  theme(legend.position = "bottom",legend.title=element_blank(),panel.spacing=unit(2,"lines"))
dev.off()

CairoPNG("figures/Traits3.png",width=1000,height=800)
ggplot(new.data.long[which(new.data.long$trait%in%c("Stem-specific \ndensity (g/cm3)","Root-specific \ndensity (g/cm3)","Root-mass fraction")),],
       aes(x=Year,y=value,fill=type))+geom_boxplot()+
  facet_grid(.~trait)+theme_bw(base_size = 22)+
  ylab("Trait value")+
  scale_fill_manual(values=c("tan1","darkorange3","plum2","purple2"))+
  theme(legend.position = "bottom",legend.title=element_blank(),panel.spacing=unit(2,"lines"))
dev.off()

CairoPNG("figures/Traits3_points.png",width=1000,height=800)
ggplot(new.data.long[which(new.data.long$trait%in%c("Stem-specific \ndensity (g/cm3)","Root-specific \ndensity (g/cm3)","Root-mass fraction")),],
       aes(x=Year,y=value,fill=type))+
  geom_boxplot()+
  facet_grid(.~trait)+theme_bw(base_size = 22)+
  ylab("Trait value")+
  coord_cartesian(ylim=c(0,1.5))+
  geom_jitter(data=trait.agg.long[which(trait.agg.long$trait%in%c("Stem-specific \ndensity (g/cm3)","Root-specific \ndensity (g/cm3)","Root-mass fraction")),],
              aes(x=Year,value,col=type),alpha=0.5,width = 0.25)+
  scale_fill_manual(values=c("tan1","darkorange3","plum2","purple2"))+
  scale_color_manual(values=c("tan1","darkorange3","plum2","purple2"))+
  theme(legend.position = "bottom",legend.title=element_blank(),panel.spacing=unit(2,"lines"))
dev.off()