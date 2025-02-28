
#identify growing season months
growing.season<-c(6,7,8,9)

weath$DATE2<-as.Date(chron(format(as.Date(weath$DATE, "%m/%d/%y"), "%m/%d/%y")))

# clean dates
dates<-matrix(unlist(str_split(weath$DATE2,"-")),ncol=3,byrow=T)
weath$Year<-as.numeric(paste0(dates[,1]))
weath$Month<-as.numeric(paste0(dates[,2]))
weath$Day<-as.numeric(paste0(dates[,3]))

#clean month
test.month<-as.character(weath$Month)
test.month<-ifelse(nchar(test.month)==1,paste0("0",test.month),test.month)
year.month<-paste0(weath$Year,"-",test.month)
weath$year.month<-year.month

##### some ggplots #########
#average max temp by month and station
test.max<-with(weath, aggregate(TMAX, by=list(NAME,year.month),mean,na.rm=T))
test.max$Date<-as.Date(paste0(test.max$Group.2,"-01"))
ggplot(test.max,aes(x=Date,y=x))+geom_line()+facet_grid(Group.1~.)+ylab("Max Temp (F)")

#average min temperature by month and station
test.min<-with(weath, aggregate(TMIN, by=list(NAME,year.month),mean,na.rm=T))
test.min$Date<-as.Date(paste0(test.min$Group.2,"-01"))
ggplot(test.min,aes(x=Date,y=x))+geom_line()+facet_grid(Group.1~.)+ylab("Min Temp (F)")

####### print values ########

### Precip ####
#sum precipitation by year and station
test<-with(weath, aggregate(PRCP, by=list(NAME,Year),sum,na.rm=T))
#average precipitation across stations
test.2<-with(test, aggregate(x, by=list(Group.2),mean,na.rm=T))

# mean precip in mm
precip.30<-25.4*mean(test.2$x[-which(test.2$Group.1%in%c(2020,2021,2022))])

#growing season
#sum precipitation by year and station
test<-with(weath[which(weath$Month%in%growing.season),], aggregate(PRCP, by=list(NAME,Year),sum,na.rm=T))
#average precipitation across stations
test.2<-with(test, aggregate(x, by=list(Group.2),mean,na.rm=T))


# mean precip in mm
growing.precip.30<-25.4*mean(test.2$x[-which(test.2$Group.1%in%c(2020,2021,2022))])


names(test.2)<-c("Year","Growing.Precip")

test.2$Growing.Precip.30.Diff<-(test.2$Growing.Precip*25.4)-growing.precip.30

precip<-test.2

precip_plot<-ggplot(precip[-which(precip$Year==2022),],aes(x=Year,y=Growing.Precip.30.Diff))+
  geom_line(lwd=1.5)+
  geom_hline(yintercept=0)+
  annotate("segment",y = 180, yend = 100, x = 2020, xend = 2020,
           linejoin="mitre",size=6,
           arrow = arrow(type = "closed", length = unit(0.01, "npc")), col="darkgoldenrod") +
  annotate("text", x=2020,y=180, label = "2020", color = "black", 
           angle = 90, hjust = 1.5, size = 5, fontface = "bold") +
  annotate("segment",y = 180, yend = 100, x = 2021, xend = 2021,
           linejoin="mitre",size=6,
           arrow = arrow(type = "closed", length = unit(0.01, "npc")), col="dodgerblue3") +
  annotate("text", x=2021,y=180, label = "2021", color = "black", 
           angle = 90, hjust = 1.5, size = 5, fontface = "bold") +
  ylab("Precipitation (mm)")+
  xlab("")+
  theme_minimal(base_size = 22)+
  theme(plot.margin = margin(0.5,0.5,0.5,1.0, "cm"))


################

### Max temp ####
# mean max temp by year and station
test<-with(weath, aggregate(TMAX, by=list(NAME,Year),mean,na.rm=T))
test.2<-with(test, aggregate(x, by=list(Group.2),mean,na.rm=T))
# mean max tmep in oC
mean.max.temp.30<-(mean(test.2$x[-which(test.2$Group.1%in%c(2020,2021,2022))]) - 32) * (5/9) 

## growing season
# mean max temp by year and station
test<-with(weath[which(weath$Month%in%growing.season),], aggregate(TMAX, by=list(NAME,Year),mean,na.rm=T))
test.2<-with(test, aggregate(x, by=list(Group.2),mean,na.rm=T))

# mean max tmep in oC
growing.max.temp.30<-(mean(test.2$x[-which(test.2$Group.1%in%c(2020,2021,2022))]) - 32) * (5/9) 

names(test.2)<-c("Year","Growing.Max.Temp")

test.2$Growing.Max.Temp.30.Diff<-((test.2$Growing.Max.Temp-32)*(5/9))-growing.max.temp.30

max.temp<-test.2

max_plot<-ggplot(max.temp[-which(max.temp$Year==2022),],aes(x=Year,y=Growing.Max.Temp.30.Diff))+
  geom_line(lwd=1.5)+
  geom_hline(yintercept=0)+
  annotate("segment",y = 5, yend = 3.5, x = 2020, xend = 2020,
           linejoin="mitre",size=6,
           arrow = arrow(type = "closed", length = unit(0.01, "npc")), col="darkgoldenrod") +
  annotate("text", x=2020,y=5, label = "2020", color = "black", 
           angle = 90, hjust = 1.5, size = 5, fontface = "bold") +
  annotate("segment",y = 5, yend = 3.5, x = 2021, xend = 2021,
           linejoin="mitre",size=6,
           arrow = arrow(type = "closed", length = unit(0.01, "npc")), col="dodgerblue3") +
  annotate("text", x=2021,y=5, label = "2021", color = "black", 
           angle = 90, hjust = 1.5, size = 5, fontface = "bold") +
  ylab("Maximum Temperature (C)")+
  xlab("")+
  theme_minimal(base_size = 22)+
  theme(plot.margin = margin(0.5,0.5,0.5,1.0, "cm"))

#################

### Min temp ####
# mean min temp by year and station
test<-with(weath, aggregate(TMIN, by=list(NAME,Year),mean,na.rm=T))
test.2<-with(test, aggregate(x, by=list(Group.2),mean,na.rm=T))

# mean min temp in oC
mean.min.temp.30<-(mean(test.2$x[-which(test.2$Group.1%in%c(2020,2021,2022))]) - 32) * (5/9) 

#growing season
test<-with(weath[which(weath$Month%in%growing.season),], aggregate(TMIN, by=list(NAME,Year),mean,na.rm=T))
test.2<-with(test, aggregate(x, by=list(Group.2),mean,na.rm=T))

growing.min.temp.30<-(mean(test.2$x[-which(test.2$Group.1%in%c(2020,2021,2022))]) - 32) * (5/9) 

names(test.2)<-c("Year","Growing.Min.Temp")

test.2$Growing.Min.Temp.30.Diff<-((test.2$Growing.Min.Temp-32)*(5/9))-growing.min.temp.30

min.temp<-test.2

min_plot<-ggplot(min.temp[-which(min.temp$Year==2022),],aes(x=Year,y=Growing.Min.Temp.30.Diff))+
  geom_line(lwd=1.5)+
  geom_hline(yintercept=0)+
  annotate("segment",y = 3, yend = 1.7, x = 2020, xend = 2020,
           linejoin="mitre",size=6,
           arrow = arrow(type = "closed", length = unit(0.01, "npc")), col="darkgoldenrod") +
  annotate("text", x=2020,y=3, label = "2020", color = "black", 
           angle = 90, hjust = 1.5, size = 5, fontface = "bold") +
  annotate("segment",y = 3, yend = 1.7, x = 2021, xend = 2021,
           linejoin="mitre",size=6,
           arrow = arrow(type = "closed", length = unit(0.01, "npc")), col="dodgerblue3") +
  annotate("text", x=2021,y=3, label = "2021", color = "black", 
           angle = 90, hjust = 1.5, size = 5, fontface = "bold") +
  ylab("Minimum Temperature (C)")+
  theme_minimal(base_size = 22)+
  theme(plot.margin = margin(0.5,0.5,0.5,1.0, "cm"))

##############

##### combined climate plot #########

CairoPNG("figures/Weather_plot.png",height=1200,width=1000)
figure<-ggarrange(precip_plot,max_plot,min_plot,
          labels=c("A","B","C"),
          hjust=-0.8,
          vjust=1.5,
          ncol=1)

annotate_figure(figure, left = textGrob("Growing Season Difference from 30yr Mean", 
                                        rot = 90, vjust = 0.8, gp = gpar(cex = 1.5)))
# bottom = textGrob("Common x-axis", gp = gpar(cex = 1.3)))

dev.off()
##############


#### 2020 ####
y.2020.min<-with(weath[which(weath$Month%in%growing.season),], aggregate(TMIN, by=list(NAME,Year),mean,na.rm=T))
test.2020.min<-with(y.2020.min[which(y.2020.min$Group.2==2020),], aggregate(x, by=list(Group.2),mean,na.rm=T))

(test.2020.min$x-32)*(5/9)

y.2020.max<-with(weath[which(weath$Month%in%growing.season),], aggregate(TMAX, by=list(NAME,Year),mean,na.rm=T))
test.2020.max<-with(y.2020.max[which(y.2020.max$Group.2==2020),], aggregate(x, by=list(Group.2),mean,na.rm=T))

(test.2020.max$x-32)*(5/9)


y.2020.prcp<-with(weath[which(weath$Month%in%growing.season),], aggregate(PRCP, by=list(NAME,Year),sum,na.rm=T))
test.2020.prcp<-with(y.2020.prcp[which(y.2020.prcp$Group.2==2020),], aggregate(x, by=list(Group.2),mean,na.rm=T))

(test.2020.prcp$x*2.54*10)


#### 2021 ####
y.2021.min<-with(weath[which(weath$Month%in%growing.season),], aggregate(TMIN, by=list(NAME,Year),mean,na.rm=T))
test.2021.min<-with(y.2021.min[which(y.2021.min$Group.2==2021),], aggregate(x, by=list(Group.2),mean,na.rm=T))

(test.2021.min$x-32)*(5/9)

y.2021.max<-with(weath[which(weath$Month%in%growing.season),], aggregate(TMAX, by=list(NAME,Year),mean,na.rm=T))
test.2021.max<-with(y.2021.max[which(y.2021.max$Group.2==2021),], aggregate(x, by=list(Group.2),mean,na.rm=T))

(test.2021.max$x-32)*(5/9)


y.2021.prcp<-with(weath[which(weath$Month%in%growing.season),], aggregate(PRCP, by=list(NAME,Year),sum,na.rm=T))
test.2021.prcp<-with(y.2021.prcp[which(y.2021.prcp$Group.2==2021),], aggregate(x, by=list(Group.2),mean,na.rm=T))

(test.2021.prcp$x*2.54*10)

