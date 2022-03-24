library(maptools)
library(spdep)
library(INLA)
library(sp)
library(RColorBrewer)
library(lattice)
library(gstat)
library(raster)
require(splancs)
library(ggplot2)
library(dplyr)
library(tidyr)
library(brinla)
library(sarima)
library(extrafont) 
library(ggsn)


### 
setwd("D:/Kuliah/Semester 5/Spasial Statistik/UAS")
# Download map
#Sumsel at Kabupaten levels
Indo<-getData('GADM', country='IDN', level=2)  ##Get the Province Shapefile for Indo
Kalsel<-Indo[Indo$NAME_1 == "Kalimantan Selatan",]
Kalsel1<-Kalsel
Kabupaten<-Kalsel$NAME_2 
row.names(Kalsel)<-Kabupaten
COOR<-coordinates(Kalsel)

plot(Kalsel, axes=T, col="gray90")
text(COOR[,1],COOR[,2], row.names(Kalsel), col="black", cex=0.8, pos=1.5)
points(COOR[,1],COOR[,2], pch=19, cex=0.7,col="blue")

DataKalsel<-read.csv("DataKalsel.csv")

DataKalsel$x<-COOR[,1]
DataKalsel$y<-COOR[,2]


#Time series plot
ggplot(data=DataKalsel, aes(x=Time, y=Penduduk.Miskin)) +geom_line()+ facet_wrap(.~Kabupaten,ncol=3, scale="free") 

#(5) Menyiapkan matrix bobot

Kalsel1<-Kalsel
W <- poly2nb(Kalsel1, queen=TRUE) #Get W matrix 
WB <- nb2mat(W, style='B', zero.policy = TRUE)
Wls<-nb2listw(W, zero.policy = TRUE)
Ws <- as(as_dgRMatrix_listw(Wls), "CsparseMatrix")

plot(Kalsel, axes=T, col="yellow")
plot(Wls, COOR, add=T, pch = 19, col="blue", lwd=1)

#TW1
DataKalsel_1<-DataKalsel[DataKalsel$Time==1, ]$Penduduk.Miskin
DataKalsel_2<-DataKalsel[DataKalsel$Time==2, ]$Penduduk.Miskin
DataKalsel_3<-DataKalsel[DataKalsel$Time==3, ]$Penduduk.Miskin
DataKalsel_4<-DataKalsel[DataKalsel$Time==4, ]$Penduduk.Miskin
DataKalsel_5<-DataKalsel[DataKalsel$Time==5, ]$Penduduk.Miskin
DataKalsel_6<-DataKalsel[DataKalsel$Time==6, ]$Penduduk.Miskin
DataKalsel_7<-DataKalsel[DataKalsel$Time==7, ]$Penduduk.Miskin
DataKalsel_8<-DataKalsel[DataKalsel$Time==8, ]$Penduduk.Miskin
DataKalsel_9<-DataKalsel[DataKalsel$Time==9, ]$Penduduk.Miskin

Moran_1<-moran.test(DataKalsel_1,Wls)
Moran_2<-moran.test(DataKalsel_2,Wls)
Moran_3<-moran.test(DataKalsel_3,Wls)
Moran_4<-moran.test(DataKalsel_4,Wls)
Moran_5<-moran.test(DataKalsel_5,Wls)
Moran_6<-moran.test(DataKalsel_6,Wls)
Moran_7<-moran.test(DataKalsel_7,Wls)
Moran_8<-moran.test(DataKalsel_8,Wls)
Moran_9<-moran.test(DataKalsel_9,Wls)

TDataKalsel_1<-ts(DataKalsel[DataKalsel$id==1, ]$Penduduk.Miskin)
TDataKalsel_2<-ts(DataKalsel[DataKalsel$id==2, ]$Penduduk.Miskin)
TDataKalsel_3<-ts(DataKalsel[DataKalsel$id==3, ]$Penduduk.Miskin)
TDataKalsel_4<-ts(DataKalsel[DataKalsel$id==4, ]$Penduduk.Miskin)
TDataKalsel_5<-ts(DataKalsel[DataKalsel$id==5, ]$Penduduk.Miskin)
TDataKalsel_6<-ts(DataKalsel[DataKalsel$id==6, ]$Penduduk.Miskin)
TDataKalsel_7<-ts(DataKalsel[DataKalsel$id==7, ]$Penduduk.Miskin)
TDataKalsel_8<-ts(DataKalsel[DataKalsel$id==8, ]$Penduduk.Miskin)
TDataKalsel_9<-ts(DataKalsel[DataKalsel$id==9, ]$Penduduk.Miskin)

par(mfrow=c(3,3))
acf(TDataKalsel_1)
acf(TDataKalsel_2)
acf(TDataKalsel_3)
acf(TDataKalsel_4)
acf(TDataKalsel_5)
acf(TDataKalsel_6)
acf(TDataKalsel_7)
acf(TDataKalsel_8)
acf(TDataKalsel_9)

##Besag
#### Setup INLA Ouput

ModelSpatialBesag<-Penduduk.Miskin~ f(id,model="besag", graph=Ws, constr=T)+IPM+Pengangguran
RModelSpatialBesag <- inla(ModelSpatialBesag, data = DataKalsel, control.predictor = list(compute = TRUE),
                           control.compute = list(dic=TRUE,mlik=TRUE,cpo=TRUE,waic=TRUE))
summary(RModelSpatialBesag)
plot(RModelSpatialBesag)

Y = DataKalsel$Penduduk.Miskin
Y1=RModelSpatialBesag$summary.fitted.values$mean
R.sqr1=sum((Y1-mean(Y))^2)/sum((Y-mean(Y))^2)
R.sqr1
##BYM

ModelSpatialBYM<-Penduduk.Miskin~ f(id,model="bym", graph=Ws, constr=T)+IPM+Pengangguran
RModelSpatialBYM<- inla(ModelSpatialBYM, data=DataKalsel, control.predictor = list(compute = TRUE)
                        ,control.compute = list(dic=TRUE,mlik=TRUE,cpo=TRUE,waic=TRUE)) 

summary(RModelSpatialBYM)
plot(RModelSpatialBYM)

Y2=RModelSpatialBYM$summary.fitted.values$mean
R.sqr2=sum((Y2-mean(Y))^2)/sum((Y-mean(Y))^2)
R.sqr2


DataForecastSpatial<-data.frame(Penduduk.Miskin=DataKalsel$Penduduk.Miskin,Besag=m1$summary.fitted.values$mean,
                                BYM=RModelSpatialBYM$summary.fitted.values$mean,
                                Time=DataKalsel$Time, 
                                LBesag=m1$summary.fitted.values[,3],
                                UBesag=m1$summary.fitted.values[,5],
                                LBYM=RModelSpatialBYM$summary.fitted.values[,3],
                                UBYM=RModelSpatialBYM$summary.fitted.values[,5], Kabupaten=DataKalsel$Kabupaten)

ggplot(DataForecastSpatial)+
  geom_line(aes(x=Time, y=Penduduk.Miskin, color="Penduduk.Miskin"))+
  geom_line(aes(x=Time, y=Besag, color="Besag"))+
  geom_line(aes(x=Time, y=BYM, color="BYM"))+
  geom_ribbon(aes(ymin=LBesag,ymax=UBesag, x=Time, fill="95% CI Besag"), alpha=0.30)+
  geom_ribbon(aes(ymin=LBYM,ymax=UBYM, x=Time, fill="95% CI BYM"), alpha=0.30)+
  facet_wrap(~Kabupaten, ncol=2, scale="free")+labs(color="Model")+labs(fill="95% CI")

#AR1
ModelTemporalAR1<-Penduduk.Miskin~ f(Time,model="ar1", constr=T)+IPM+Pengangguran
RModelTemporalAR1<- inla(ModelTemporalAR1,data=DataKalsel, 
                         control.predictor = list(compute = TRUE)
                         ,control.compute = list(dic=TRUE,mlik=TRUE,cpo=TRUE,waic=TRUE))
summary(RModelTemporalAR1)
plot(RModelTemporalAR1)

Y3=RModelTemporalAR1$summary.fitted.values$mean
R.sqr3=sum((Y3-mean(Y))^2)/sum((Y-mean(Y))^2)
R.sqr3

#BesagAR
ModelSpatioTemporalAR1Besag<-Penduduk.Miskin~ f(Time,model="ar1", constr=T)+IPM+Pengangguran+
  f(id,model="besag", graph=Ws, constr=T) 


RModelSpatioTemporalAR1Besag<- inla(ModelSpatioTemporalAR1Besag,data=DataKalsel, 
                                    control.predictor = list(compute = TRUE)
                                    ,control.compute = list(dic=TRUE,mlik=TRUE,cpo=TRUE,waic=TRUE))
summary(RModelSpatioTemporalAR1Besag)
plot(RModelSpatioTemporalAR1Besag)

Y4 = RModelSpatioTemporalAR1Besag$summary.fitted.values$mean
R.sqr4=sum((Y4-mean(Y))^2)/sum((Y-mean(Y))^2)
R.sqr4


RModelSpatioTemporalAR1Besag$summary.fitted.values

DataForecastSpatioTemporal<-data.frame(Penduduk.Miskin=DataKalsel$Penduduk.Miskin, ST=RModelSpatioTemporalAR1Besag$summary.fitted.values$mean, 
                                       Time=DataKalsel$Time, 
                                       LST=RModelSpatioTemporalAR1Besag$summary.fitted.values[,3],
                                       UST=RModelSpatioTemporalAR1Besag$summary.fitted.values[,5], Kabupaten=DataKalsel$Kabupaten)
head(DataForecastSpatioTemporal)
summary(DataForecastSpatioTemporal)

ggplot(DataForecastSpatioTemporal)+
  geom_line(aes(x=Time, y=Penduduk.Miskin, color="Penduduk.Miskin"))+
  geom_line(aes(x=Time, y=ST, color="ST"))+ 
  geom_ribbon(aes(ymin=LST,ymax=UST, x=Time, fill="95% CI ST"), alpha=0.30)+
  facet_wrap(~Kabupaten, ncol=2, scale="free")+labs(color="Model")+labs(fill="95% CI")

#SPPlot
ST13 = RModelSpatioTemporalAR1Besag$summary.fitted.values$mean[1:13]
ST14 = RModelSpatioTemporalAR1Besag$summary.fitted.values$mean[14:26]
ST15 = RModelSpatioTemporalAR1Besag$summary.fitted.values$mean[27:39]
ST16 = RModelSpatioTemporalAR1Besag$summary.fitted.values$mean[40:52]
ST17 = RModelSpatioTemporalAR1Besag$summary.fitted.values$mean[53:65]
ST18 = RModelSpatioTemporalAR1Besag$summary.fitted.values$mean[66:78]
ST19 = RModelSpatioTemporalAR1Besag$summary.fitted.values$mean[79:91]
ST20 = RModelSpatioTemporalAR1Besag$summary.fitted.values$mean[92:104]
ST21 = RModelSpatioTemporalAR1Besag$summary.fitted.values$mean[105:117]

library(RColorBrewer)
library(classInt)
spplot(KalselMap1_13)

ST=c(ST13,ST14,ST15,ST16,ST17,ST18,ST19,ST20,ST21)
ST1=data.frame(ST13,ST14,ST15,ST16,ST17,ST18,ST19,ST20,ST21)
data.combined <- cbind(Kalsel, ST1)

northarrow<-list("SpatialPolygonsRescale",layout.north.arrow(),
                 offset<-c(106,108),scale=0.1,which=2)
scalebar<-list("SpatialPolygonRescale",layout.scale.bar(),
               offset<-c(-7,-6),scale=0.1,
               fill<-c("transparent","black"),which=2)
text1<-list("sp.text",c(106,108),"0",which=2)
text2<-list("sp.text",c(-7,-6),"0.1 KM",which=2)
plotvar=ST
nlcr=5
plotclr=brewer.pal(nlcr,"Paired")
class=classIntervals(plotvar,nlcr,style="quantile")
colcode=findColours(class,plotclr)
plot<- spplot(data.combined,c("ST17","ST18","ST19"),names.attr=c("ST17","ST18","ST19"),colorkey=list(space="right"),
              main="Taksiran Risiko Relatif Spatio-Temporal Model",col.regions=plotclr,at=round(class$brks,digits=5),
              sp.layout=list(northarrow),as.table=TRUE)
label<-layer(sp.text(coordinates(data.combined),txt=c(1:27),
                     txt.cex=0.1, cex=0.7, pos=0.8))
plot+label

spplot(data.combined,c("ST13","ST14","ST15",
                       "ST16","ST17","ST18","ST19","ST20","ST21"),names.attr=c("ST13","ST14","ST15",
                                            "ST16","ST17","ST18","ST19","ST20","ST21"),colorkey=list(space="right"),
       main="Taksiran Kemiskinan Spatio-Temporal 
Model",col.regions=plotclr,at=round(class$brks,
                                    digits=10),sp.layout=list(northarrow),
       as.table=TRUE)
label<-layer(sp.text(coordinates(data.combined),txt=c(1:13),
                     txt.cex=0.1, cex=0.7, pos=0.8))
